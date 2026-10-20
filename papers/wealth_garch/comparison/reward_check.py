import hashlib
import math
import statistics
from functools import partial
from multiprocessing import Pool

import numpy as np
from scipy.integrate import solve_ivp

from sample import ANNUAL, RATES, R_REF, samples
from stage1_barriers import u0
from stage2 import RHO, load_panel
from window import FAR, THIN, load_years, observed, quarters, ratio, show

C_INS = 1.264e-4 * 100
GRID = (0.0, C_INS, 2 * C_INS, 3 * C_INS)
SEED, PATHS, DT, STEPS_Q, YEARS = 20261020, 200, 1 / 360, 90, 10
START, TOP = "20041231", 2.0


def mean_capital(cert, annual):
    s = annual.get(cert, {})
    xs = [(s[y]["RBCT1J"] + s[y]["RBCT2"]) / s[y]["RWAJT"] for y in range(2005, 2015)
          if y in s and s[y]["RWAJT"] and s[y]["RBCT1J"] is not None and s[y]["RBCT2"] is not None]
    return statistics.fmean(xs)


def barrier(mu, sigma, c, xbar):
    def rhs(x, y):
        f, g = y
        return [g, 2 / sigma ** 2 * (RHO * f - (mu + c * (x - xbar)) * g)]

    def inflection(x, y):
        return rhs(x, y)[1]
    inflection.direction = 1
    sol = solve_ivp(rhs, (0.0, TOP), [0.0, 1.0], events=inflection, rtol=1e-11, atol=1e-13, method="DOP853")
    hits = sol.t_events[0]
    return float(hits[0]) if len(hits) else None


def paths(b, minimum, c, top):
    rng = np.random.default_rng([SEED, int(b["cert"]), int(round(c * 1e6)), int(round(minimum * 1e4))])
    mu, sig, xbar = b["mu"], b["sigma"], b["mean_x"] - minimum
    x = np.full(PATHS, b["x0_train"] - minimum)
    alive = x > 0
    x = np.minimum(x, top)
    f1 = f2 = n1 = n2 = 0
    pre_b = x + minimum - R_REF
    sq = math.sqrt(DT)
    for _ in range(4 * YEARS):
        divq = np.zeros(PATHS, dtype=bool)
        for _ in range(STEPS_Q):
            x = x + (mu + c * (x - xbar)) * DT + sig * sq * rng.standard_normal(PATHS)
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


def one(minimum, c, b):
    top = barrier(b["mu"], b["sigma"], c, b["mean_x"] - minimum)
    if top is None:
        return None
    return paths(b, minimum, c, top) + (top,)


def main():
    for p in [ANNUAL, RATES]:
        print(f"{p.split('/')[-1]} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    print(f"c_ins = {C_INS:.5f}; grid {', '.join(f'{c:.5f}' for c in GRID)}; rho = {RHO}")
    samp, _ = samples()
    annual = load_panel(ANNUAL)
    q = load_years(range(2004, 2015))
    qs = quarters(2005, 2014)
    worst = max(abs(barrier(b["mu"], b["sigma"], 0.0, 0.0) - u0(b["mu"], b["sigma"], RHO)) for b in samp["A"])
    print(f"validation: largest |barrier at c = 0 - closed-form u0| over sample A = {worst:.2e} "
          f"-> {'passes' if worst <= 1e-6 else 'fails'}")
    if worst > 1e-6:
        return
    for name in ("B", "A"):
        banks = []
        for b in samp[name]:
            x0 = ratio(q, b["cert"], START)
            if x0 is not None:
                banks.append(dict(b, x0_train=x0, mean_x=mean_capital(b["cert"], annual)))
        print(f"\n===== sample {name}: {len(banks)} banks with a 2004Q4 buffer")
        print("observed, 2005-2014")
        show(observed(banks, q, qs, R_REF))
        with Pool() as pool:
            for minimum, label in ((0.08, "minimum 8% (reward alone)"), (R_REF, "minimum at R (reward and penalty proxy)")):
                for c in GRID:
                    out = pool.map(partial(one, minimum, c), banks)
                    none = sum(o is None for o in out)
                    out = [o for o in out if o is not None]
                    tops = sorted(o[5] for o in out)
                    res = {"F1": (sum(o[0] for o in out), sum(o[1] for o in out)),
                           "F2": (sum(o[2] for o in out), sum(o[3] for o in out))}
                    print(f"{label}, c = {c:.5f}: {none} banks with no barrier below {TOP}; "
                          f"{sum(o[4] for o in out)} start failed; median barrier {statistics.median(tops):.4f}")
                    show(res)


if __name__ == "__main__":
    main()
