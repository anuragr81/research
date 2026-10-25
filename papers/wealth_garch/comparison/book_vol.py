import hashlib
import math
import statistics
from functools import partial
from multiprocessing import Pool

import numpy as np
from scipy.stats import spearmanr

import facts
import predict_pk
from diag_f2 import sim as barrier_sim
from judged_risk import FIRMS, holders, paths
from sample import ANNUAL, QUARTERLY, QUARTERS, R_REF, RATES, samples
from stage2 import DELTA, solve
from test_extend import NEW
from window import FAR, THIN, load_years, observed, quarters, ratio

FLOOR = 0.10
MINIMA = ((FLOOR, "minimum 10%"), (0.08, "minimum 8%"), (R_REF, "minimum at R"))
START_TRAIN = "20041231"
MIN_CHANGES = 8


def sigma_d(panel, cert):
    s = panel.get(cert, {})
    changes = []
    for y in range(2005, 2015):
        a, b = f"{y - 1}1231", f"{y}1231"
        ra, rb = ratio(panel, cert, a), ratio(panel, cert, b)
        if ra is None or rb is None:
            continue
        if not 0.5 <= s[b]["ASSET"] / s[a]["ASSET"] <= 2:
            continue
        changes.append(rb - ra)
    return statistics.stdev(changes) if len(changes) >= MIN_CHANGES else None


def pk_buffers(b, x0key):
    rng = np.random.default_rng([predict_pk.SEED, int(b["cert"])])
    mu, sig = b["mu"], b["sigma"]
    issuer = b["u2"] is not None
    top = b["u2"] if issuer else b["u0"]
    min_issue = 0.005 * b["asset_rwa"]
    x = np.full(predict_pk.PATHS, b[x0key] - 0.08)
    alive = x > 0
    x = np.minimum(x, top)
    wait = np.full(predict_pk.PATHS, -1.0)
    f1 = f2 = n1 = n2 = 0
    bufs = []
    pre_b = x + 0.08 - R_REF
    sq = math.sqrt(predict_pk.DT)
    for _ in range(4 * predict_pk.YEARS):
        divq = np.zeros(predict_pk.PATHS, dtype=bool)
        for _ in range(predict_pk.STEPS_Q):
            x = x + mu * predict_pk.DT + sig * sq * rng.standard_normal(predict_pk.PATHS)
            dead = alive & (x <= 0)
            alive &= ~dead
            held = alive & (x >= top)
            divq |= held
            x = np.where(held, top, x)
            if issuer:
                start = alive & (wait < 0) & (x <= b["u1"])
                wait = np.where(start, DELTA, wait)
                ticking = alive & (wait >= 0)
                wait = np.where(ticking, wait - predict_pk.DT, wait)
                land = alive & ticking & (wait <= 1e-12)
                x = np.where(land, b["u2"], x)
                wait = np.where(land, -1.0, wait)
        now_b = x + 0.08 - R_REF
        f2 += int(np.sum(alive & (now_b > FAR * sig)))
        n2 += int(np.sum(alive))
        d = alive & divq
        f1 += int(np.sum(d & (pre_b < THIN * sig)))
        n1 += int(np.sum(d))
        bufs.extend(now_b[alive].tolist())
        pre_b = now_b
    return (statistics.median(bufs) if bufs else None), f1, n1, f2, n2


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


def run(pool, banks, minimum):
    out = pool.map(partial(paths, minimum=minimum, key="sD"), banks)
    return {"F1": (sum(o[0] for o in out), sum(o[1] for o in out)), "F2": (sum(o[2] for o in out), sum(o[3] for o in out))}, \
        sum(o[4] for o in out)


def baseline(pool, banks):
    out = pool.map(predict_pk.simulate, pool.map(solve, banks))
    return {"F1": (sum(o[0] for o in out), sum(o[1] for o in out)), "F2": (sum(o[2] for o in out), sum(o[3] for o in out))}


def with_d(banks, panel):
    out, unc = [], 0
    for b in banks:
        sd = sigma_d(panel, b["cert"])
        if sd is None:
            unc += 1
            continue
        out.append(dict(b, sD=sd))
    return out, unc


def level(panel, cert, qs):
    vals = [ratio(panel, cert, d) - R_REF for d in qs if ratio(panel, cert, d) is not None]
    return statistics.median(vals) if vals else None


def corr_line(label, levels, banks):
    pairs = [(lv, b) for lv, b in zip(levels, banks) if lv is not None]
    lv = [p[0] for p in pairs]
    size = [math.log(p[1]["size"]) for p in pairs]
    risk = [p[1]["sigma"] for p in pairs]
    rs, ps = spearmanr(lv, size)
    rr, pr = spearmanr(lv, risk)
    print(f"  {label}: {len(pairs)} banks; with size rho {rs:+.3f} (p {ps:.2g}); with risk rho {rr:+.3f} (p {pr:.2g})")
    return (rs, ps), (rr, pr)


def terciles(label, levels, banks, key):
    pairs = sorted(((lv, b) for lv, b in zip(levels, banks) if lv is not None), key=lambda p: key(p[1]))
    n = len(pairs)
    cuts = [pairs[: n // 3], pairs[n // 3: 2 * (n // 3)], pairs[2 * (n // 3):]]
    print(f"  {label}: median level by tercile (low, middle, high): "
          + ", ".join(f"{100 * statistics.median(p[0] for p in c):.2f}" for c in cuts) + " points")


def verdict(name, model, data):
    for (what, sign), (rm, pm), (rd, pd) in zip((("size", -1), ("risk", +1)), model, data):
        holds = (rd * sign > 0) and pd < 0.05
        rep = (rm * rd > 0) and pm < 0.05
        print(f"  {name}, {what}: data direction {'holds' if holds else 'does not hold'} (predicted {'negative' if sign < 0 else 'positive'}); model {'reproduces' if rep else 'does not reproduce'} the data's sign")


def main():
    for p in [ANNUAL, RATES] + QUARTERLY:
        print(f"{p.split('/')[-1]} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    samp, q = samples()
    train = load_years(range(2004, 2015))
    qs = quarters(2005, 2014)
    scored_parents = {f[0] for f in FIRMS} | {n[0] for n in NEW}
    with Pool() as pool:
        solvedB = pool.map(solve, samp["B"])
        copy = pool.map(partial(pk_buffers, x0key="x0"), solvedB)
        orig = pool.map(predict_pk.simulate, solvedB)
        same = all(c[1:] == o[:4] for c, o in zip(copy, orig))
        print(f"\nvalidation: the buffer-recording copy reproduces predict_pk.py's per-bank F1 and F2 counts on sample B: {same}")
        if not same:
            return

        print("\n===== part 1, stage 1: training window 2005-2014, from the 2004Q4 buffer")
        passed = False
        for name in ("B", "A"):
            banks, unc = with_d(samp[name], train)
            banks = [dict(b, x0_train=ratio(train, b["cert"], START_TRAIN)) for b in banks]
            banks = [b for b in banks if b["x0_train"] is not None]
            r = sorted(b["sD"] / b["sigma"] for b in banks)
            print(f"\n--- sample {name}: {len(banks)} banks with sigma_D and a 2004Q4 buffer; {unc} with fewer than {MIN_CHANGES} changes")
            print(f"  sigma_D in points: median {100 * statistics.median(b['sD'] for b in banks):.2f}; sigma_D over sigma_i: lq {r[len(r) // 4]:.2f}, median {statistics.median(r):.2f}, uq {r[3 * len(r) // 4]:.2f}")
            obs_raw = observed(banks, train, qs, R_REF)
            show("observed, 2005-2014", {k: obs_raw[k] for k in ("F1", "F2")})
            obs = pct(obs_raw)
            for minimum, mlabel in MINIMA:
                res, fail = run(pool, banks, minimum)
                show(f"(D) book-capital volatility, {mlabel}: {fail} start failed", res)
                _, d2 = miss(f"(D), {mlabel}", pct(res), obs)
                if name == "B" and minimum == FLOOR:
                    passed = d2 <= 10
                    print(f"  stage 1 rule (sample B, minimum 10%): F2 {pct(res)['F2']:.1f}% against {obs['F2']:.1f}%; (D) {'goes forward' if passed else 'does not go forward'}")

        if passed:
            print("\n===== part 1, stage 2: test window 2015-2024, from the 2014Q4 buffer")
            for name in ("B", "A"):
                banks, unc = with_d(samp[name], train)
                banks = [dict(b, x0_train=b["x0"]) for b in banks]
                hc = holders({b["cert"] for b in banks})
                groups = [("unscored", [b for b in banks if hc.get(b["cert"], "") not in scored_parents], name == "B"),
                          ("already scored with (B)", [b for b in banks if hc.get(b["cert"], "") in scored_parents], False),
                          ("all", banks, False)]
                print(f"\n--- sample {name}: {len(banks)} banks with sigma_D; {unc} with fewer than {MIN_CHANGES} changes")
                for label, grp, decide in groups:
                    print(f"\n  {label}: {len(grp)} banks")
                    obs_raw = facts.observed(grp, q)
                    obs = pct(obs_raw)
                    show("observed, 2015-2024", {k: obs_raw[k] for k in ("F1", "F2")})
                    base = baseline(pool, grp)
                    show("baseline (Peura and Keppo), minimum 8%", base)
                    mb, _ = miss("baseline, minimum 8%", pct(base), obs)
                    for minimum, mlabel in MINIMA:
                        res, fail = run(pool, grp, minimum)
                        show(f"(D), {mlabel}: {fail} start failed", res)
                        md, _ = miss(f"(D), {mlabel}", pct(res), obs)
                        if decide and minimum == FLOOR:
                            print(f"  stage 2 rule (the {len(grp)} unscored banks of sample B): (D) {'beats' if md < mb else 'does not beat'} the baseline ({md:.1f} against {mb:.1f} points)")
        else:
            print("\nstage 2 not run: (D) did not pass stage 1")

        print("\n===== part 2: directions of BDF-Q21 (level falls with size, rises with risk)")
        for wname, x0key, panel, wqs in (("training 2005-2014", "x0_train", train, qs), ("test 2015-2024", "x0", q, QUARTERS)):
            for name in ("A", "B"):
                banks, _ = with_d(samp[name], train)
                if x0key == "x0_train":
                    banks = [dict(b, x0_train=ratio(train, b["cert"], START_TRAIN)) for b in banks]
                    banks = [b for b in banks if b["x0_train"] is not None]
                else:
                    banks = [dict(b, x0_train=b["x0"]) for b in banks]
                print(f"\n--- {wname}, sample {name}: {len(banks)} banks")
                obs_lv = [level(panel, b["cert"], wqs) for b in banks]
                data = corr_line("observed level", obs_lv, banks)
                terciles("observed, by size", obs_lv, banks, lambda b: b["size"])
                terciles("observed, by risk", obs_lv, banks, lambda b: b["sigma"])
                solved = pool.map(solve, banks)
                base_lv = [o[0] for o in pool.map(partial(pk_buffers, x0key=x0key), solved)]
                print(f"  baseline: {sum(v is None for v in base_lv)} banks with no surviving path")
                base = corr_line("baseline level", base_lv, banks)
                d_lv = [o[0] for o in pool.map(partial(barrier_sim, minimum=FLOOR, key="sD"), banks)]
                print(f"  (D), minimum 10%: {sum(v is None for v in d_lv)} banks with no surviving path")
                dd = corr_line("(D) level, minimum 10%", d_lv, banks)
                verdict("baseline", base, data)
                verdict("(D)", dd, data)


if __name__ == "__main__":
    main()
