import hashlib
import statistics
from functools import partial
from multiprocessing import Pool

import numpy as np
import statsmodels.api as sm
from scipy.stats import spearmanr

import facts
import predict_pk
from book_vol import level, pk_buffers, sigma_d
from diag_f2 import sim as barrier_sim
from judged_risk import FIRMS, holders, paths
from sample import ANNUAL, QUARTERLY, QUARTERS, R_REF, RATES, samples
from stage2 import solve
from test_extend import NEW
from window import load_years, observed, quarters, ratio

FLOOR = 0.10
MINIMA = ((FLOOR, "minimum 10%"), (0.08, "minimum 8%"), (R_REF, "minimum at R"))
START_TRAIN = "20041231"
MIN_CHANGES = 8


def sigma_n(panel, cert):
    s = panel.get(cert, {})
    changes = []
    for y in range(2005, 2015):
        a, b = s.get(f"{y - 1}1231"), s.get(f"{y}1231")
        ra = ratio(panel, cert, f"{y - 1}1231")
        if a is None or b is None or ra is None:
            continue
        need = (a["RBCT1J"], a["RBCT2"], a["EQCDIV"], b["NETINC"], b["RWAJT"], a["ASSET"], b["ASSET"])
        if any(v is None for v in need) or b["RWAJT"] <= 0 or not a["ASSET"]:
            continue
        if not 0.5 <= b["ASSET"] / a["ASSET"] <= 2:
            continue
        dnk = (a["RBCT1J"] + a["RBCT2"] + b["NETINC"] - a["EQCDIV"]) / b["RWAJT"]
        changes.append(dnk - ra)
    return statistics.stdev(changes) if len(changes) >= MIN_CHANGES else None


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


def covered(samp_banks, train):
    out, unc = [], 0
    for b in samp_banks:
        sn, sd = sigma_n(train, b["cert"]), sigma_d(train, b["cert"])
        if sn is None or sd is None:
            unc += 1
            continue
        out.append(dict(b, sN=sn, sD=sd))
    return out, unc


def place(label, obs, pred):
    pairs = [(o, p) for o, p in zip(obs, pred) if o is not None and p is not None]
    o = np.array([x[0] for x in pairs])
    p = np.array([x[1] for x in pairs])
    rho, pv = spearmanr(o, p)
    fit = sm.OLS(o, sm.add_constant(p)).fit(cov_type="HC1")
    print(f"  {label}: {len(pairs)} banks; Spearman {rho:+.3f} (p {pv:.2g}); observed = {100 * fit.params[0]:+.2f} points "
          f"+ {fit.params[1]:.3f} (se {fit.bse[1]:.3f}) x predicted; R2 {fit.rsquared:.3f}")
    return rho > 0 and pv < 0.05


def main():
    for p in [ANNUAL, RATES] + QUARTERLY:
        print(f"{p.split('/')[-1]} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    samp, q = samples()
    train = load_years(range(2004, 2015))
    qs = quarters(2005, 2014)
    scored_parents = {f[0] for f in FIRMS} | {n[0] for n in NEW}
    with Pool() as pool:
        print("\n===== 1. training gate: sample B, 2005-2014, from the 2004Q4 buffer")
        for name in ("B", "A"):
            banks, unc = covered(samp[name], train)
            banks = [dict(b, x0_train=ratio(train, b["cert"], START_TRAIN)) for b in banks]
            banks = [b for b in banks if b["x0_train"] is not None]
            r = sorted(b["sN"] / b["sigma"] for b in banks)
            rd = sorted(b["sN"] / b["sD"] for b in banks)
            print(f"\n--- sample {name}: {len(banks)} banks with sigma_N, sigma_D and a 2004Q4 buffer; {unc} not covered")
            print(f"  sigma_N in points: median {100 * statistics.median(b['sN'] for b in banks):.2f}; over sigma_i: lq {r[len(r) // 4]:.2f}, "
                  f"median {statistics.median(r):.2f}, uq {r[3 * len(r) // 4]:.2f}; over sigma_D: median {statistics.median(rd):.2f}")
            obs_raw = observed(banks, train, qs, R_REF)
            show("observed, 2005-2014", {k: obs_raw[k] for k in ("F1", "F2")})
            obs = pct(obs_raw)
            for minimum, mlabel in MINIMA:
                res, fail = run(pool, banks, minimum, "sN")
                show(f"(N) do-nothing volatility, {mlabel}: {fail} start failed", res)
                _, d2 = miss(f"(N), {mlabel}", pct(res), obs)
                if name == "B" and minimum == FLOOR:
                    ok = d2 <= 10
                    print(f"  gate (sample B, minimum 10%): F2 {pct(res)['F2']:.1f}% against {obs['F2']:.1f}%; "
                          f"(N) {'passes and replaces (D)' if ok else 'does not pass; (D) stays'}")

        print("\n===== 2. test window, reported: sample B, 2015-2024, from the 2014Q4 buffer")
        banks, unc = covered(samp["B"], train)
        banks = [dict(b, x0_train=b["x0"]) for b in banks]
        hc = holders({b["cert"] for b in banks})
        for label, grp in (("the 26 of PREREG_comparison_17.md", [b for b in banks if hc.get(b["cert"], "") not in scored_parents]),
                           ("all", banks)):
            print(f"\n  {label}: {len(grp)} banks")
            obs_raw = facts.observed(grp, q)
            obs = pct(obs_raw)
            show("observed, 2015-2024", {k: obs_raw[k] for k in ("F1", "F2")})
            base = baseline(pool, grp)
            show("baseline (Peura and Keppo), minimum 8%", base)
            miss("baseline", pct(base), obs)
            for key, mlabel in (("sN", "(N)"), ("sD", "(D)")):
                res, fail = run(pool, grp, FLOOR, key)
                show(f"{mlabel}, minimum 10%: {fail} start failed", res)
                miss(f"{mlabel}, minimum 10%", pct(res), obs)

        print("\n===== 3. bank by bank: observed median buffer over R against each model's")
        verdicts = {}
        for wname, x0key, panel, wqs in (("training 2005-2014", "x0_train", train, qs), ("test 2015-2024", "x0", q, QUARTERS)):
            for name in ("B", "A"):
                banks, _ = covered(samp[name], train)
                if x0key == "x0_train":
                    banks = [dict(b, x0_train=ratio(train, b["cert"], START_TRAIN)) for b in banks]
                    banks = [b for b in banks if b["x0_train"] is not None]
                else:
                    banks = [dict(b, x0_train=b["x0"]) for b in banks]
                print(f"\n--- {wname}, sample {name}: {len(banks)} banks")
                obs_lv = [level(panel, b["cert"], wqs) for b in banks]
                solved = pool.map(solve, banks)
                preds = {"baseline": [o[0] for o in pool.map(partial(pk_buffers, x0key=x0key), solved)],
                         "(D) minimum 10%": [o[0] for o in pool.map(partial(barrier_sim, minimum=FLOOR, key="sD"), banks)],
                         "(N) minimum 10%": [o[0] for o in pool.map(partial(barrier_sim, minimum=FLOOR, key="sN"), banks)]}
                for m, pr in preds.items():
                    verdicts.setdefault((name, m), []).append(place(m, obs_lv, pr))
        print("\n  places buffers (Spearman positive, p < 0.05, in both windows):")
        for (name, m), v in verdicts.items():
            print(f"    sample {name}, {m}: {'yes' if all(v) else 'no'} ({', '.join('yes' if x else 'no' for x in v)})")


if __name__ == "__main__":
    main()
