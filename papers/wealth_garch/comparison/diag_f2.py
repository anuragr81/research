import hashlib
import math
import os
import statistics
from functools import partial
from multiprocessing import Pool

import numpy as np

from judged_risk import DT, FIRMS, PATHS, PRICES, RHO, SEED, STEPS_Q, YEARS, holders, listed, mean_ratio, sha
from sample import ANNUAL, QUARTERLY, QUARTERS, R_REF, RATES, samples
from stage1_barriers import u0
from test_extend import NEW, NEW_PRICES, sigma_e
from window import FAR, load_years, quarters, ratio

PTS = 100


def sim(b, minimum=0.08, key="sB"):
    s = b[key]
    rng = np.random.default_rng([SEED, int(b["cert"]), int(round(minimum * 1e4)), int(round(s * 1e7))])
    mu, sig = b["mu"], b["sigma"]
    top = u0(mu, s, RHO)
    x = np.full(PATHS, b["x0_train"] - minimum)
    alive = x > 0
    x = np.minimum(x, top)
    bufs = []
    f2 = n2 = 0
    sq = math.sqrt(DT)
    for _ in range(4 * YEARS):
        for _ in range(STEPS_Q):
            x = x + mu * DT + s * sq * rng.standard_normal(PATHS)
            alive &= ~(alive & (x <= 0))
            held = alive & (x >= top)
            x = np.where(held, top, x)
        now_b = x + minimum - R_REF
        bufs.extend(now_b[alive].tolist())
        f2 += int(np.sum(alive & (now_b > FAR * sig)))
        n2 += int(np.sum(alive))
    return (statistics.median(bufs) if bufs else None), f2, n2


def observed_bank(q, cert, qs, sig):
    bufs = {d: ratio(q, cert, d) - R_REF for d in qs if ratio(q, cert, d) is not None}
    if not bufs:
        return None
    vals = list(bufs.values())
    q4 = [bufs[d] for d in qs if d.endswith("1231") and d in bufs]
    diffs = [b - a for a, b in zip(q4, q4[1:])]
    sd = statistics.stdev(diffs) if len(diffs) >= 3 else None
    hits = sum(v > FAR * sig for v in vals)
    return {"median": statistics.median(vals), "sd": sd, "hits": hits, "n": len(vals)}


def implied_k(mu, sig, target):
    lo, hi = 0.1, 50.0
    if target <= 0 or u0(mu, lo * sig, RHO) >= target:
        return None
    if u0(mu, hi * sig, RHO) <= target:
        return None
    for _ in range(60):
        mid = (lo + hi) / 2
        if u0(mu, mid * sig, RHO) < target:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2


def fmt(v, d=1):
    return "-" if v is None else f"{v:.{d}f}"


def main():
    for p in [ANNUAL, RATES] + QUARTERLY:
        print(f"{p.split('/')[-1]} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    names = listed()
    firms = {}
    for rssd, tick, fname, digest, lname in NEW:
        path = os.path.join(NEW_PRICES, fname)
        assert sha(path) == digest and names[tick] == lname, tick
        firms[rssd] = (tick, sigma_e(path)[0])
    for rssd, fed, tick, fname, digest, lname in FIRMS:
        path = os.path.join(PRICES, fname)
        assert sha(path) == digest and names[tick] == lname, tick
        firms[rssd] = (tick, sigma_e(path)[0])
    samp, q = samples()
    train = load_years(range(2004, 2015))
    qs_train = quarters(2005, 2014)
    hc = holders({b["cert"] for b in samp["B"]})
    banks = []
    for b in samp["B"]:
        f = firms.get(hc.get(b["cert"], ""))
        if f is None:
            continue
        X = mean_ratio(train, b["cert"])
        banks.append(dict(b, x0_train=b["x0"], X=X, tick=f[0], sB=f[1] * X))
    print(f"\n===== {len(banks)} covered banks of sample B; buffers in points of risk-weighted assets over R = 10.5%")
    with Pool() as pool:
        sims = pool.map(partial(sim, minimum=0.08, key="sB"), banks)
    rows = []
    for b, (m_med, f2, n2) in zip(banks, sims):
        ot = observed_bank(q, b["cert"], QUARTERS, b["sigma"])
        otr = observed_bank(train, b["cert"], qs_train, b["sigma"])
        top = u0(b["mu"], b["sB"], RHO)
        k = implied_k(b["mu"], b["sigma"], otr["median"]) if otr else None
        rows.append(dict(b, m_med=m_med, f2=f2, n2=n2, ot=ot, otr=otr, top=top, k=k))
    rows.sort(key=lambda r: r["sigma"])
    print("\n1. per bank, sorted by sigma_i")
    print("  cert   tick  sigma_i  sigma_B  sB/si   u0    | test: obs median  obs yr sd  obs F2 | model median  model F2 | train: obs median  obs yr sd  obs F2 | k_i  (sB/si)/k_i")
    for r in rows:
        ot, otr = r["ot"], r["otr"]
        print(f"  {r['cert']:>6s} {r['tick']:5s} {PTS * r['sigma']:6.2f} {PTS * r['sB']:7.2f} {r['sB'] / r['sigma']:6.2f} {PTS * r['top']:6.2f} | "
              f"{fmt(PTS * ot['median'], 2):>8s} {fmt(None if ot['sd'] is None else PTS * ot['sd'], 2):>8s} {100 * ot['hits'] / ot['n']:6.1f}% | "
              f"{fmt(None if r['m_med'] is None else PTS * r['m_med'], 2):>8s} {fmt(None if r['n2'] == 0 else 100 * r['f2'] / r['n2']):>6s}% | "
              f"{fmt(None if otr is None else PTS * otr['median'], 2):>8s} {fmt(None if otr is None or otr['sd'] is None else PTS * otr['sd'], 2):>8s} "
              f"{fmt(None if otr is None else 100 * otr['hits'] / otr['n']):>6s}% | {fmt(r['k'], 2):>5s} {fmt(None if r['k'] is None else r['sB'] / r['sigma'] / r['k'], 2):>5s}")
    print("\n2. terciles of sigma_i (test window)")
    n = len(rows)
    cuts = [rows[: n // 3], rows[n // 3: 2 * (n // 3)], rows[2 * (n // 3):]]
    obs_all = (sum(r["ot"]["hits"] for r in rows), sum(r["ot"]["n"] for r in rows))
    mod_all = (sum(r["f2"] for r in rows), sum(r["n2"] for r in rows))
    gap = 100 * (mod_all[0] / mod_all[1] - obs_all[0] / obs_all[1])
    total = 0.0
    for name, grp in zip(("low", "middle", "high"), cuts):
        oh, on = sum(r["ot"]["hits"] for r in grp), sum(r["ot"]["n"] for r in grp)
        mh, mn = sum(r["f2"] for r in grp), sum(r["n2"] for r in grp)
        w = on / obs_all[1]
        c = 100 * w * (mh / mn - oh / on)
        total += c
        print(f"  {name:6s} {len(grp):2d} banks, sigma_i {PTS * grp[0]['sigma']:.2f} to {PTS * grp[-1]['sigma']:.2f} points: observed F2 {100 * oh / on:5.1f}%, model F2 {100 * mh / mn:5.1f}%, "
              f"weight {100 * w:.0f}% of observed bank-quarters, contribution {c:+.1f} points")
    print(f"  overall gap, model less observed: {gap:+.1f} points (contributions sum to {total:+.1f})")
    print("\n3. over banks (medians)")
    rt = [r["top"] / r["ot"]["median"] for r in rows if r["ot"]["median"] > 0]
    rtr = [r["top"] / r["otr"]["median"] for r in rows if r["otr"] and r["otr"]["median"] > 0]
    print(f"  u0 over observed median buffer: test {statistics.median(rt):.2f} ({len(rt)} banks with a positive median), training {statistics.median(rtr):.2f} ({len(rtr)})")
    sd_t = [r["ot"]["sd"] for r in rows if r["ot"]["sd"] is not None]
    print(f"  observed yearly sd of the buffer, test window: median {PTS * statistics.median(sd_t):.2f} points; over sigma_i {statistics.median([r['ot']['sd'] / r['sigma'] for r in rows if r['ot']['sd'] is not None]):.2f}; "
          f"over sigma_B {statistics.median([r['ot']['sd'] / r['sB'] for r in rows if r['ot']['sd'] is not None]):.2f}")
    sd_tr = [r["otr"]["sd"] for r in rows if r["otr"] and r["otr"]["sd"] is not None]
    print(f"  observed yearly sd of the buffer, training window: median {PTS * statistics.median(sd_tr):.2f} points; over sigma_i {statistics.median([r['otr']['sd'] / r['sigma'] for r in rows if r['otr'] and r['otr']['sd'] is not None]):.2f}; "
          f"over sigma_B {statistics.median([r['otr']['sd'] / r['sB'] for r in rows if r['otr'] and r['otr']['sd'] is not None]):.2f}")
    print("\n4. implied scale k_i (training median buffer = u0(mu_i, k_i sigma_i, rho))")
    ks = sorted(r["k"] for r in rows if r["k"] is not None)
    print(f"  {len(ks)} banks with a k_i in [0.1, 50]: min {ks[0]:.2f}, lower quartile {ks[len(ks) // 4]:.2f}, median {statistics.median(ks):.2f}, upper quartile {ks[3 * len(ks) // 4]:.2f}, max {ks[-1]:.2f}")
    over = sorted(r["sB"] / r["sigma"] / r["k"] for r in rows if r["k"] is not None)
    print(f"  sigma_B over k_i sigma_i: min {over[0]:.2f}, median {statistics.median(over):.2f}, max {over[-1]:.2f}")
    print(f"  banks with no k_i: {[r['tick'] for r in rows if r['k'] is None]}")


if __name__ == "__main__":
    main()
