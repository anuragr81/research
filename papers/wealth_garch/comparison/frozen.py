import hashlib
import math
from functools import partial
from multiprocessing import Pool

import numpy as np

import facts
from book_vol import pk_buffers, with_d
from judged_risk import DT, PATHS, RHO, SEED, STEPS_Q, YEARS
from sample import ANNUAL, QUARTERLY, R_REF, RATES, samples
from stage1_barriers import u0
from stage2 import solve
from window import FAR, THIN, load_years, observed, quarters, ratio

START_TRAIN = "20041231"
SWITCH_TAG = 2010
TRAIN_FLOORS = [0.10] * 20 + [0.105] * 20
TEST_FLOORS = [0.105] * 40


def frozen_paths(b, floors):
    s = b["sD"]
    rng = np.random.default_rng([SEED, int(b["cert"]), SWITCH_TAG, int(round(s * 1e7))])
    mu, sig = b["mu"], b["sigma"]
    top = u0(mu, s, RHO)
    x = np.full(PATHS, b["x0_train"] - floors[0])
    alive = x > 0
    x = np.minimum(x, top)
    f1 = f2 = n1 = n2 = 0
    pre_b = x + floors[0] - R_REF
    sq = math.sqrt(DT)
    for qn in range(4 * YEARS):
        f = floors[qn]
        if qn > 0 and f != floors[qn - 1]:
            x = x - (f - floors[qn - 1])
            alive &= ~(alive & (x <= 0))
        divq = np.zeros(PATHS, dtype=bool)
        for _ in range(STEPS_Q):
            x = x + mu * DT + s * sq * rng.standard_normal(PATHS)
            alive &= ~(alive & (x <= 0))
            held = alive & (x >= top)
            divq |= held
            x = np.where(held, top, x)
        now_b = x + f - R_REF
        f2 += int(np.sum(alive & (now_b > FAR * sig)))
        n2 += int(np.sum(alive))
        d = alive & divq
        f1 += int(np.sum(d & (pre_b < THIN * sig)))
        n1 += int(np.sum(d))
        pre_b = now_b
    return f1, n1, f2, n2, bool(b["x0_train"] - floors[0] <= 0)


def show(label, res):
    print(label)
    for k, (hit, n) in res.items():
        print(f"  {k}: {hit}/{n} = {100 * hit / n:.1f}%" if n else f"  {k}: 0/0 = n/a")


def pct(res):
    return {k: 100 * h / n for k, (h, n) in res.items() if n}


def miss(label, pred, obs):
    d1, d2 = abs(pred["F1"] - obs["F1"]), abs(pred["F2"] - obs["F2"])
    print(f"  {label}: |dF1| {d1:.1f} + |dF2| {d2:.1f} = {d1 + d2:.1f} points; F1 within 10: {d1 <= 10}; F2 within 10: {d2 <= 10}")
    return d1 + d2


def main():
    for p in [ANNUAL, RATES] + QUARTERLY:
        print(f"{p.split('/')[-1]} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    samp, q = samples()
    train = load_years(range(2004, 2015))
    qs = quarters(2005, 2014)
    with Pool() as pool:
        for wname, floors, x0key in (("training 2005-2014, floor 10% to 2009Q4 and 10.5% from 2010Q1", TRAIN_FLOORS, "x0_train"),
                                     ("test 2015-2024, floor 10.5%", TEST_FLOORS, "x0")):
            print(f"\n===== {wname}")
            for name in ("B", "A"):
                banks, unc = with_d(samp[name], train)
                if x0key == "x0_train":
                    banks = [dict(b, x0_train=ratio(train, b["cert"], START_TRAIN)) for b in banks]
                    banks = [b for b in banks if b["x0_train"] is not None]
                    obs_raw = observed(banks, train, qs, R_REF)
                else:
                    banks = [dict(b, x0_train=b["x0"]) for b in banks]
                    obs_raw = facts.observed(banks, q)
                print(f"\n--- sample {name}: {len(banks)} banks; {unc} without sigma_D")
                show("observed", {k: obs_raw[k] for k in ("F1", "F2")})
                obs = pct(obs_raw)
                out = pool.map(partial(frozen_paths, floors=floors), banks)
                res = {"F1": (sum(o[0] for o in out), sum(o[1] for o in out)), "F2": (sum(o[2] for o in out), sum(o[3] for o in out))}
                show(f"(P) frozen model: {sum(o[4] for o in out)} start failed", res)
                base_out = pool.map(partial(pk_buffers, x0key=x0key), pool.map(solve, banks))
                base = {"F1": (sum(o[1] for o in base_out), sum(o[2] for o in base_out)),
                        "F2": (sum(o[3] for o in base_out), sum(o[4] for o in base_out))}
                show("baseline (Peura and Keppo), minimum 8%", base)
                mp = miss("(P)", pct(res), obs)
                mb = miss("baseline", pct(base), obs)
                print(f"  (P) {'beats' if mp < mb else 'does not beat'} the baseline ({mp:.1f} against {mb:.1f} points); reported, decides nothing")


if __name__ == "__main__":
    main()
