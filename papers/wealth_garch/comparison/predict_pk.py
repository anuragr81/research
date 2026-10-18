import math
from multiprocessing import Pool

import numpy as np

from sample import R_REF, samples
from stage2 import DELTA, solve

THIN, FAR = 1.0, 4.0
DT, PATHS, SEED, YEARS = 1 / 360, 200, 20261018, 10
STEPS_Q = 90


def simulate(b):
    rng = np.random.default_rng([SEED, int(b["cert"])])
    mu, sig = b["mu"], b["sigma"]
    issuer = b["u2"] is not None
    top = b["u2"] if issuer else b["u0"]
    min_issue = 0.005 * b["asset_rwa"]
    x = np.full(PATHS, b["x0"] - 0.08)
    alive = x > 0
    x = np.minimum(x, top)
    wait = np.full(PATHS, -1.0)
    f1 = f2 = n1 = n2 = 0
    eps = []
    pre_b = x + 0.08 - R_REF
    sq = math.sqrt(DT)
    for qn in range(4 * YEARS):
        divq = np.zeros(PATHS, dtype=bool)
        arrived = np.zeros(PATHS, dtype=bool)
        for _ in range(STEPS_Q):
            x = x + mu * DT + sig * sq * rng.standard_normal(PATHS)
            dead = alive & (x <= 0)
            alive &= ~dead
            held = alive & (x >= top)
            divq |= held
            x = np.where(held, top, x)
            if issuer:
                start = alive & (wait < 0) & (x <= b["u1"])
                wait = np.where(start, DELTA, wait)
                ticking = alive & (wait >= 0)
                wait = np.where(ticking, wait - DT, wait)
                land = alive & ticking & (wait <= 1e-12)
                big = land & (b["u2"] - x >= min_issue)
                arrived |= big
                x = np.where(land, b["u2"], x)
                wait = np.where(land, -1.0, wait)
        now_b = x + 0.08 - R_REF
        live = alive
        f2 += int(np.sum(live & (now_b > FAR * sig)))
        n2 += int(np.sum(live))
        d = live & divq
        f1 += int(np.sum(d & (pre_b < THIN * sig)))
        n1 += int(np.sum(d))
        for i in np.where(live & arrived)[0]:
            eps.append((bool(pre_b[i] < THIN * sig), bool(THIN * sig <= now_b[i] <= FAR * sig)))
        pre_b = now_b
    return f1, n1, f2, n2, eps, int(np.sum(b["x0"] - 0.08 <= 0))


def main():
    samp, _ = samples()
    for name in ("A", "B"):
        banks = samp[name]
        with Pool() as pool:
            solved = pool.map(solve, banks)
            out = pool.map(simulate, solved)
        issuers = sum(b["u2"] is not None for b in solved)
        f1 = sum(o[0] for o in out); n1 = sum(o[1] for o in out)
        f2 = sum(o[2] for o in out); n2 = sum(o[3] for o in out)
        eps = [e for o in out for e in o[4]]
        failed_start = sum(o[5] > 0 for o in out)
        print(f"\nsample {name}: {len(banks)} banks, {issuers} issuer, {failed_start} starting at or below 8%; "
              f"{PATHS} paths each")
        for k, hit, n in (("F1", f1, n1), ("F2", f2, n2), ("F3", sum(e[0] for e in eps), len(eps)),
                          ("F4", sum(e[1] for e in eps), len(eps))):
            print(f"  {k}: {hit}/{n} = {100 * hit / n:.1f}%" if n else f"  {k}: 0/0 = n/a (no episodes)")


if __name__ == "__main__":
    main()
