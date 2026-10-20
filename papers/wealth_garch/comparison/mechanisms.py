import hashlib
import math
import statistics
from functools import partial
from multiprocessing import Pool

import numpy as np
from numba import njit

from sample import ANNUAL, RATES, R_REF, samples
from stage1_barriers import u0
from stage2 import RHO, load_panel, steady
from window import FAR, THIN, load_years, observed, quarters, ratio, show

SEED, PATHS, DT, STEPS_Q, YEARS = 20261020, 200, 1 / 360, 90, 10
START, TOP, H = "20041231", 2.0, 1e-5
JUMPS = [(j, lam) for j in (0.02, 0.04, 0.06) for lam in (1 / 25, 1 / 10)]
CENTRAL_JUMP = (0.04, 1 / 25)
GROWTH = (0.5, 1.0, 1.5)
RAISE = (0.01, 0.02, 0.03)


@njit(cache=True)
def _second(x, f, g, a, s, sig2, rho, lam, fj):
    return 2.0 / sig2 * ((rho + lam) * f - (a + s * x) * g - lam * fj)


@njit(cache=True)
def _lag(fs, x, j, h):
    y = x - j
    if y <= 0.0:
        return 0.0
    k = int(y / h)
    t = y / h - k
    return fs[k] * (1.0 - t) + fs[k + 1] * t


@njit(cache=True)
def inflection(a, s, sigma, rho, lam, j, h, xmax):
    n = int(xmax / h)
    fs = np.zeros(n + 2)
    sig2 = sigma * sigma
    f, g = 0.0, 1.0
    prev = _second(0.0, f, g, a, s, sig2, rho, lam, 0.0)
    for i in range(n):
        x = i * h
        fs[i] = f
        k1f, k1g = g, _second(x, f, g, a, s, sig2, rho, lam, _lag(fs, x, j, h))
        xm = x + h / 2
        fm = _lag(fs, xm, j, h)
        k2f, k2g = g + h / 2 * k1g, _second(xm, f + h / 2 * k1f, g + h / 2 * k1g, a, s, sig2, rho, lam, fm)
        k3f, k3g = g + h / 2 * k2g, _second(xm, f + h / 2 * k2f, g + h / 2 * k2g, a, s, sig2, rho, lam, fm)
        x1 = x + h
        k4f, k4g = g + h * k3g, _second(x1, f + h * k3f, g + h * k3g, a, s, sig2, rho, lam, _lag(fs, x1, j, h))
        f += h / 6 * (k1f + 2 * k2f + 2 * k3f + k4f)
        g += h / 6 * (k1g + 2 * k2g + 2 * k3g + k4g)
        fs[i + 1] = f
        now = _second(x1, f, g, a, s, sig2, rho, lam, _lag(fs, x1, j, h))
        if prev < 0.0 and now >= 0.0:
            return x + h * (-prev) / (now - prev)
        prev = now
    return -1.0


def growth_rate(cert, annual):
    s = annual.get(cert, {})
    gs = [s[y]["ASSET"] / s[y - 1]["ASSET"] - 1 for y in range(2005, 2015)
          if y in s and y - 1 in s and s[y]["ASSET"] and s[y - 1]["ASSET"] and steady(s, y)]
    return statistics.fmean(gs) if gs else None


def simulate(b, minimum, a, s, lam, j, top):
    rng = np.random.default_rng([SEED, int(b["cert"]), int(round(minimum * 1e4)), int(round(a * 1e7)) % 2 ** 31,
                                 int(round(lam * 1e4)), int(round(j * 1e4))])
    sig = b["sigma"]
    x = np.full(PATHS, b["x0_train"] - minimum)
    alive = x > 0
    x = np.minimum(x, top)
    f1 = f2 = n1 = n2 = 0
    pre_b = x + minimum - R_REF
    sq = math.sqrt(DT)
    for _ in range(4 * YEARS):
        divq = np.zeros(PATHS, dtype=bool)
        for _ in range(STEPS_Q):
            x = x + (a + s * x) * DT + sig * sq * rng.standard_normal(PATHS)
            if lam > 0:
                x = x - j * (rng.random(PATHS) < lam * DT)
            alive &= ~(alive & (x <= 0))
            held = alive & (x >= top)
            divq |= held
            x = np.where(held, top, x)
        now_b = x + minimum - R_REF
        f2 += int(np.sum(alive & (now_b > FAR * sig)))
        n2 += int(np.sum(alive))
        d = alive & divq
        f1 += int(np.sum(d & (pre_b < THIN * sig)))
        n1 += int(np.sum(d))
        pre_b = now_b
    return f1, n1, f2, n2, bool(b["x0_train"] - minimum <= 0)


def one(spec, b):
    kind, minimum, p = spec
    mu, sig = b["mu"], b["sigma"]
    if kind == "jump":
        j, lam = p
        a, s, rho, lam_ = mu + lam * j, 0.0, RHO, lam
    elif kind == "growth":
        if b["g"] is None:
            return None
        g = p * b["g"]
        if g >= RHO:
            return None
        a, s, rho, lam_, j = mu - g * minimum, -g, RHO - g, 0.0, 0.0
    else:
        a, s, rho, lam_, j = mu, 0.0, RHO, 0.0, 0.0
    top = inflection(a, s, sig, rho, lam_, j, H, TOP)
    if top < 0:
        return None
    return simulate(b, minimum, a, s, lam_, j, top) + (top,)


def report(label, out):
    none = sum(o is None for o in out)
    out = [o for o in out if o is not None]
    tops = sorted(o[5] for o in out)
    print(f"{label}: {none} banks with no barrier below {TOP}; {sum(o[4] for o in out)} start failed; "
          f"median barrier {statistics.median(tops):.4f}")
    show({"F1": (sum(o[0] for o in out), sum(o[1] for o in out)), "F2": (sum(o[2] for o in out), sum(o[3] for o in out))})


def main():
    for p in [ANNUAL, RATES]:
        print(f"{p.split('/')[-1]} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    samp, _ = samples()
    annual = load_panel(ANNUAL)
    worst = max(abs(inflection(b["mu"], 0.0, b["sigma"], RHO, 0.0, 0.0, H, TOP) - u0(b["mu"], b["sigma"], RHO))
                for b in samp["A"])
    print(f"validation: largest |numerical barrier - closed-form u0| over sample A = {worst:.2e} "
          f"-> {'passes' if worst <= 1e-6 else 'fails'}")
    if worst > 1e-6:
        return
    q = load_years(range(2004, 2015))
    qs = quarters(2005, 2014)
    for name in ("B", "A"):
        banks = []
        for b in samp[name]:
            x0 = ratio(q, b["cert"], START)
            if x0 is not None:
                banks.append(dict(b, x0_train=x0, g=growth_rate(b["cert"], annual)))
        gs = [b["g"] for b in banks if b["g"] is not None]
        print(f"\n===== sample {name}: {len(banks)} banks with a 2004Q4 buffer; median g_i {statistics.median(gs):.4f}, "
              f"{sum(g >= RHO for g in gs)} with g_i >= rho")
        print("observed, 2005-2014")
        show(observed(banks, q, qs, R_REF))
        with Pool() as pool:
            for minimum, mlabel in ((0.08, "minimum 8%"), (R_REF, "minimum at R")):
                for jl in JUMPS:
                    tag = " (central)" if jl == CENTRAL_JUMP else ""
                    report(f"1. jumps J = {jl[0]}, lambda = {jl[1]:.3f}{tag}, {mlabel}",
                           pool.map(partial(one, ("jump", minimum, jl)), banks))
                for k in GROWTH:
                    tag = " (central)" if k == 1.0 else ""
                    report(f"3. growth {k} g_i{tag}, {mlabel}", pool.map(partial(one, ("growth", minimum, k)), banks))
            for d in RAISE:
                report(f"4. minimum at R + {d}", pool.map(partial(one, ("plain", R_REF + d, None)), banks))


if __name__ == "__main__":
    main()
