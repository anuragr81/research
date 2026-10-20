import hashlib
import math
from functools import partial
from multiprocessing import Pool

import numpy as np

from sample import ANNUAL, RATES, R_REF, samples
from stage1_barriers import u0
from window import FAR, THIN, load_years, observed, quarters, ratio, show

SEED, PATHS, DT, STEPS_Q, YEARS = 20261020, 200, 1 / 360, 90, 10
START = "20041231"
SCALES = (1.0, 1.5, 2.0, 3.0, 4.0, 6.0)
RHOS = (0.04, 0.02, 0.01, 0.005)


def paths(b, minimum, k, rho):
    rng = np.random.default_rng([SEED, int(b["cert"]), int(round(minimum * 1e4)), int(round(k * 100)), int(round(rho * 1e4))])
    mu, sig = b["mu"], b["sigma"]
    top = u0(mu, k * sig, rho)
    x = np.full(PATHS, b["x0_train"] - minimum)
    alive = x > 0
    x = np.minimum(x, top)
    f1 = f2 = n1 = n2 = 0
    pre_b = x + minimum - R_REF
    sq = math.sqrt(DT)
    for _ in range(4 * YEARS):
        divq = np.zeros(PATHS, dtype=bool)
        for _ in range(STEPS_Q):
            x = x + mu * DT + k * sig * sq * rng.standard_normal(PATHS)
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
    return f1, n1, f2, n2, bool(b["x0_train"] - minimum <= 0), top / sig


def report(label, out):
    rel = sorted(o[5] for o in out)
    print(f"{label}: {sum(o[4] for o in out)} start failed; median barrier {rel[len(rel) // 2]:.2f} sigma_i above the minimum")
    show({"F1": (sum(o[0] for o in out), sum(o[1] for o in out)), "F2": (sum(o[2] for o in out), sum(o[3] for o in out))})


def main():
    for p in [ANNUAL, RATES]:
        print(f"{p.split('/')[-1]} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    samp, _ = samples()
    q = load_years(range(2004, 2015))
    qs = quarters(2005, 2014)
    for name in ("B", "A"):
        banks = [dict(b, x0_train=ratio(q, b["cert"], START)) for b in samp[name] if ratio(q, b["cert"], START) is not None]
        print(f"\n===== sample {name}: {len(banks)} banks with a 2004Q4 buffer")
        print("observed, 2005-2014")
        show(observed(banks, q, qs, R_REF))
        with Pool() as pool:
            for minimum, mlabel in ((0.08, "minimum 8%"), (R_REF, "minimum at R")):
                for k in SCALES:
                    report(f"risk scale k = {k}, rho = 0.04, {mlabel}", pool.map(partial(paths, minimum=minimum, k=k, rho=0.04), banks))
                for rho in RHOS[1:]:
                    report(f"risk scale k = 1, rho = {rho}, {mlabel}", pool.map(partial(paths, minimum=minimum, k=1.0, rho=rho), banks))


if __name__ == "__main__":
    main()
