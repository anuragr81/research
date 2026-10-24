import hashlib
import math
import statistics
from functools import partial
from multiprocessing import Pool

import facts
import predict_pk
from judged_risk import FIRMS, holders, paths
from sample import ANNUAL, QUARTERLY, R_REF, RATES, qflow, samples, shift
from stage2 import solve
from test_extend import NEW
from window import load_years, observed, quarters, ratio

ZS = (1.645, 2.326, 3.090)
Z_CENTRAL = 2.326
HORIZON = 2.25
WINDOW = 9
START_TRAIN = "20041231"
TRAIN_F2 = 48.0


def worst_loss(s, qs):
    flows = {}
    for d in qs:
        ni, dv = qflow(s, d, "NETINC"), qflow(s, d, "EQCDIV")
        flows[d] = None if ni is None or dv is None else ni - dv
    worst, counted = 0.0, 0
    for i in range(len(qs) - WINDOW + 1):
        win = qs[i:i + WINDOW]
        before = s.get(shift(win[0], -1))
        last = s.get(win[-1])
        if any(flows[d] is None for d in win) or before is None or last is None:
            continue
        if not before["RWAJT"] or before["RWAJT"] <= 0 or not before["ASSET"] or not last["ASSET"]:
            continue
        if not 0.5 <= last["ASSET"] / before["ASSET"] <= 2:
            continue
        counted += 1
        worst = max(worst, -sum(flows[d] for d in win) / before["RWAJT"])
    return (worst, counted) if counted else (None, 0)


def show(label, res):
    print(label)
    for k, (hit, n) in res.items():
        print(f"  {k}: {hit}/{n} = {100 * hit / n:.1f}%" if n else f"  {k}: 0/0 = n/a")


def pct(res):
    return {k: 100 * h / n for k, (h, n) in res.items() if n}


def miss(label, pred, obs):
    d1, d2 = abs(pred["F1"] - obs["F1"]), abs(pred["F2"] - obs["F2"])
    print(f"  {label}: |dF1| {d1:.1f} + |dF2| {d2:.1f} = {d1 + d2:.1f} points; F1 within 10: {d1 <= 10}; F2 within 10: {d2 <= 10}")
    return d1 + d2, d2


def run(pool, banks, minimum, key):
    out = pool.map(partial(paths, minimum=minimum, key=key), banks)
    return {"F1": (sum(o[0] for o in out), sum(o[1] for o in out)), "F2": (sum(o[2] for o in out), sum(o[3] for o in out))}, \
        sum(o[4] for o in out)


def baseline(pool, banks):
    out = pool.map(predict_pk.simulate, pool.map(solve, banks))
    return {"F1": (sum(o[0] for o in out), sum(o[1] for o in out)), "F2": (sum(o[2] for o in out), sum(o[3] for o in out))}


def quartiles(xs):
    xs = sorted(xs)
    return xs[0], xs[len(xs) // 4], statistics.median(xs), xs[3 * len(xs) // 4], xs[-1]


def judged(samp_banks, train, qs):
    banks, uncovered = [], []
    for b in samp_banks:
        d, counted = worst_loss(train.get(b["cert"], {}), qs)
        if d is None:
            uncovered.append(b["cert"])
            continue
        sc = {z: max(d / (z * math.sqrt(HORIZON)), b["sigma"]) for z in ZS}
        floor = {z: d / (z * math.sqrt(HORIZON)) < b["sigma"] for z in ZS}
        banks.append(dict(b, D=d, windows=counted, floor=floor, **{f"sC{z}": v for z, v in sc.items()}))
    return banks, uncovered


def main():
    for p in [ANNUAL, RATES] + QUARTERLY:
        print(f"{p.split('/')[-1]} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    samp, q = samples()
    train = load_years(range(2004, 2015))
    qs = quarters(2005, 2014)
    scored_parents = {f[0] for f in FIRMS} | {n[0] for n in NEW}
    passed = False
    with Pool() as pool:
        print("\n===== stage 1: training window 2005-2014, from the 2004Q4 buffer")
        for name in ("B", "A"):
            banks, unc = judged(samp[name], train, qs)
            banks = [dict(b, x0_train=ratio(train, b["cert"], START_TRAIN)) for b in banks]
            banks = [b for b in banks if b["x0_train"] is not None]
            print(f"\n--- sample {name}: {len(banks)} banks with a D_i and a 2004Q4 buffer; {len(unc)} with no counted window")
            print(f"  D_i in points of risk-weighted assets: min, lower quartile, median, upper quartile, max = "
                  + ", ".join(f"{100 * v:.2f}" for v in quartiles([b["D"] for b in banks])))
            print(f"  banks with D_i = 0: {sum(b['D'] == 0 for b in banks)}")
            meds = [statistics.median([ratio(train, b["cert"], d) - R_REF for d in qs if ratio(train, b["cert"], d) is not None]) for b in banks]
            print(f"  median 2005-2014 buffer over R, median over banks: {100 * statistics.median(meds):.2f} points; "
                  f"banks whose median buffer exceeds D_i: {sum(m > b['D'] for m, b in zip(meds, banks))} of {len(banks)}")
            for z in ZS:
                r = quartiles([b[f"sC{z}"] / b["sigma"] for b in banks])
                print(f"  z = {z}: sigma_C over sigma_i, min, lq, median, uq, max = " + ", ".join(f"{v:.2f}" for v in r)
                      + f"; {sum(b['floor'][z] for b in banks)} banks at the floor sigma_i")
            obs = pct(observed(banks, train, qs, R_REF))
            show("observed, 2005-2014", observed(banks, train, qs, R_REF))
            for minimum, mlabel in ((0.08, "minimum 8%"), (R_REF, "minimum at R")):
                for z in ZS:
                    res, fail = run(pool, banks, minimum, f"sC{z}")
                    tag = " (central)" if z == Z_CENTRAL else ""
                    show(f"(C) own worst loss, z = {z}{tag}, {mlabel}: {fail} start failed", res)
                    total, d2 = miss(f"(C) z = {z}, {mlabel}", pct(res), obs)
                    if name == "B" and z == Z_CENTRAL and minimum == 0.08:
                        passed = d2 <= 10
                        print(f"  stage 1 rule (sample B, central z, minimum 8%): F2 {pct(res)['F2']:.1f}% against {obs['F2']:.1f}%; (C) {'goes forward' if passed else 'does not go forward'}")
        if not passed:
            print("\nstage 2 not run: (C) did not pass stage 1")
            return
        print("\n===== stage 2: test window 2015-2024, from the 2014Q4 buffer")
        for name in ("B", "A"):
            banks, unc = judged(samp[name], train, qs)
            banks = [dict(b, x0_train=b["x0"]) for b in banks]
            hc = holders({b["cert"] for b in banks})
            groups = [("unscored", [b for b in banks if hc.get(b["cert"], "") not in scored_parents], name == "B"),
                      ("already scored with (B)", [b for b in banks if hc.get(b["cert"], "") in scored_parents], False),
                      ("all", banks, False)]
            print(f"\n--- sample {name}: {len(banks)} banks with a D_i; {len(unc)} with no counted window")
            for label, grp, decide in groups:
                print(f"\n  {label}: {len(grp)} banks")
                obs_raw = facts.observed(grp, q)
                obs = pct(obs_raw)
                show("observed, 2015-2024", {k: obs_raw[k] for k in ("F1", "F2")})
                base = baseline(pool, grp)
                show("baseline (Peura and Keppo), minimum 8%", base)
                res, fail = run(pool, grp, 0.08, f"sC{Z_CENTRAL}")
                show(f"(C) central z, minimum 8%: {fail} start failed", res)
                resR, failR = run(pool, grp, R_REF, f"sC{Z_CENTRAL}")
                show(f"(C) central z, minimum at R: {failR} start failed", resR)
                mb, _ = miss("baseline, minimum 8%", pct(base), obs)
                mc, _ = miss("(C), minimum 8%", pct(res), obs)
                miss("(C), minimum at R", pct(resR), obs)
                if decide:
                    print(f"  stage 2 rule (the {len(grp)} unscored banks of sample B, minimum 8%): (C) {'beats' if mc < mb else 'does not beat'} the baseline ({mc:.1f} against {mb:.1f} points)")


if __name__ == "__main__":
    main()
