import hashlib
import os
from functools import partial
from multiprocessing import Pool

import facts
import predict_pk
from judged_risk import FIRMS, PRICES, declines, equity_sigma, holders, listed, mean_ratio, paths, sha
from sample import ANNUAL, QUARTERLY, R_REF, RATES, samples
from stage2 import solve
from window import load_years


def pct(res):
    return {k: (100 * h / n if n else None) for k, (h, n) in res.items()}


def show(label, res):
    print(label)
    for k, (hit, n) in res.items():
        print(f"  {k}: {hit}/{n} = {100 * hit / n:.1f}%" if n else f"  {k}: 0/0 = n/a")


def miss(label, pred, obs):
    d1, d2 = abs(pred["F1"] - obs["F1"]), abs(pred["F2"] - obs["F2"])
    print(f"  {label}: |dF1| {d1:.1f} + |dF2| {d2:.1f} = {d1 + d2:.1f} points; F1 within 10: {d1 <= 10}; F2 within 10: {d2 <= 10}")
    return d1 + d2


def baseline(banks):
    with Pool() as pool:
        out = pool.map(predict_pk.simulate, pool.map(solve, banks))
    eps = [e for o in out for e in o[4]]
    return {"F1": (sum(o[0] for o in out), sum(o[1] for o in out)), "F2": (sum(o[2] for o in out), sum(o[3] for o in out)),
            "F3": (sum(e[0] for e in eps), len(eps)), "F4": (sum(e[1] for e in eps), len(eps))}


def enhanced(banks, minimum):
    with Pool() as pool:
        out = pool.map(partial(paths, minimum=minimum, key="sB"), banks)
    return {"F1": (sum(o[0] for o in out), sum(o[1] for o in out)), "F2": (sum(o[2] for o in out), sum(o[3] for o in out))}, \
        sum(o[4] for o in out)


def main():
    for p in [ANNUAL, RATES] + QUARTERLY:
        print(f"{p.split('/')[-1]} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    names = listed()
    dec = declines()
    sE = {}
    for rssd, fed, tick, fname, digest, lname in FIRMS:
        path = os.path.join(PRICES, fname)
        assert sha(path) == digest, fname
        assert names[tick] == lname, tick
        assert fed in dec, fed
        sE[rssd] = (tick, equity_sigma(path)[0])
    samp, q = samples()
    train = load_years(range(2004, 2015))
    for name in ("B", "A"):
        hc = holders({b["cert"] for b in samp[name]})
        banks = []
        for b in samp[name]:
            f = sE.get(hc.get(b["cert"], ""))
            if f is None:
                continue
            X = mean_ratio(train, b["cert"])
            banks.append(dict(b, x0_train=b["x0"], X=X, tick=f[0], sB=f[1] * X))
        print(f"\n===== sample {name}: {len(banks)} banks whose top holding company is one of the 11 firms; start 2014Q4, 2015-2024")
        for b in sorted(banks, key=lambda b: -b["size"]):
            print(f"  {b['cert']:>6s} {b['tick']:5s} x0 {b['x0']:.4f} sigma_i {b['sigma']:.5f} sigma_B {b['sB']:.5f}")
        obs_raw = facts.observed(banks, q)
        show("observed, 2015-2024", obs_raw)
        base_raw = baseline(banks)
        show("baseline (Peura and Keppo), minimum 8%", base_raw)
        res8, fail8 = enhanced(banks, 0.08)
        show(f"(B) equity volatility, minimum 8%: {fail8} start failed", res8)
        resR, failR = enhanced(banks, R_REF)
        show(f"(B) equity volatility, minimum at R: {failR} start failed", resR)
        obs, base = pct(obs_raw), pct(base_raw)
        print("misses against observed F1 and F2 (points)")
        mb = miss("baseline, minimum 8%", base, obs)
        me = miss("(B), minimum 8%", pct(res8), obs)
        miss("(B), minimum at R", pct(resR), obs)
        if name == "B":
            print(f"  rule (sample B, 17 banks, minimum 8%): (B) {'beats' if me < mb else 'does not beat'} the baseline ({me:.1f} against {mb:.1f} points)")


if __name__ == "__main__":
    main()
