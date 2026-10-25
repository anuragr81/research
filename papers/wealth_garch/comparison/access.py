import csv
import gzip
import hashlib
import math
import os
import statistics
from collections import Counter

import numpy as np
import statsmodels.api as sm
from scipy.stats import rankdata, spearmanr

from match_tickers import listed_common, nic, norm
from sample import ANNUAL, QUARTERLY, QUARTERS, R_REF, RATES, ROOT, samples
from window import load_years, quarters, ratio


def holders_at(d0):
    hc = {}
    with gzip.open(os.path.join(ROOT, "common_data", f"fdic_q_{d0[:4]}.csv.gz"), "rt", newline="") as fh:
        for r in csv.DictReader(fh):
            if r["REPDTE"] == d0:
                hc[r["CERT"]] = r["RSSDHCR"].strip()
    counts = Counter(h for h in hc.values() if h not in ("", "0"))
    return hc, counts


def classify(cert, hc, counts, recs, by_cert, common):
    h = hc.get(cert, "")
    standalone = h in ("", "0") or counts[h] < 2
    parent = h if h not in ("", "0") else (sorted(by_cert[cert])[0] if len(by_cert.get(cert, ())) == 1 else None)
    rs = recs.get(parent, []) if parent else []
    if not rs:
        return standalone, None
    act = [r for r in rs if r[0]]
    country = (act[0] if act else max(rs, key=lambda r: r[1]))[3]
    if country != "UNITED STATES":
        return standalone, "foreign"
    cands = set()
    for n in {r[2] for r in rs}:
        cands |= common.get(norm(n), set())
    return standalone, "listed" if len(cands) == 1 else "unlisted"


def level(panel, cert, qs):
    vals = [ratio(panel, cert, d) - R_REF for d in qs if ratio(panel, cert, d) is not None]
    return statistics.median(vals) if vals else None


def ols(y, cols, names):
    X = sm.add_constant(np.column_stack(cols))
    fit = sm.OLS(y, X).fit(cov_type="HC1")
    for name, b, se, p in zip(["const"] + names, fit.params, fit.bse, fit.pvalues):
        print(f"    {name:10s} {b:+.4f} (se {se:.4f}, p {p:.2g})")
    print(f"    n {int(fit.nobs)}, R2 {fit.rsquared:.3f}")
    return dict(zip(["const"] + names, zip(fit.params, fit.bse, fit.pvalues)))


def main():
    for p in [ANNUAL, RATES] + QUARTERLY:
        print(f"{p.split('/')[-1]} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    recs, by_cert = nic()
    common = listed_common()
    samp, q = samples()
    train = load_years(range(2004, 2015))
    windows = (("training 2005-2014", "20041231", train, quarters(2005, 2014), False),
               ("test 2015-2024", "20141231", q, QUARTERS, True))
    for wname, d0, panel, qs, decide_listing in windows:
        hc, counts = holders_at(d0)
        banks = [b for b in samp["A"] if ratio(panel, b["cert"], d0) is not None]
        rows, no_record, no_level = [], 0, 0
        for b in banks:
            sa, lst = classify(b["cert"], hc, counts, recs, by_cert, common)
            lv = level(panel, b["cert"], qs)
            if lst is None:
                no_record += 1
                continue
            if lv is None:
                no_level += 1
                continue
            rows.append(dict(cert=b["cert"], lv=lv, size=math.log(b["size"]), sigma=b["sigma"], sa=int(sa),
                             unl=int(lst == "unlisted"), frn=int(lst == "foreign"), grp=lst))
        print(f"\n===== {wname}: {len(rows)} banks of sample A with a buffer at {d0}; {no_record} whose parent has no NIC record, {no_level} with no level")
        c = Counter((r["sa"], r["grp"]) for r in rows)
        print("  counts (stand-alone, listing): " + ", ".join(f"{'stand-alone' if k[0] else 'affiliated'}/{k[1]} {v}" for k, v in sorted(c.items())))
        raw = np.array([r["lv"] for r in rows])
        lo, hi = np.percentile(raw, [1, 99])
        y = np.clip(raw, lo, hi)
        print(f"  level winsorised at {100 * lo:.2f} and {100 * hi:.2f} points; median {100 * statistics.median(raw):.2f}")
        size = np.array([r["size"] for r in rows])
        sig = np.array([r["sigma"] for r in rows])
        sa = np.array([r["sa"] for r in rows])
        unl = np.array([r["unl"] for r in rows])
        frn = np.array([r["frn"] for r in rows])
        print("  1. level on log size, sigma_i, stand-alone, unlisted, foreign (HC1)")
        full = ols(y, [size, sig, sa, unl, frn], ["logsize", "sigma", "standalone", "unlisted", "foreign"])
        print("  2. level on log size and sigma_i only (HC1)")
        base = ols(y, [size, sig], ["logsize", "sigma"])
        b_with, b_without = full["logsize"][0], base["logsize"][0]
        print(f"  share of the size coefficient carried by access: 1 - {b_with:+.4f} / {b_without:+.4f} = {1 - b_with / b_without:.2f}")
        b3, b4 = full["standalone"], full["unlisted"]
        sa_ok = b3[0] > 0 and b3[2] < 0.05
        un_ok = b4[0] > 0 and b4[2] < 0.05
        print(f"  stand-alone direction (b3 > 0, p < 0.05): {'holds' if sa_ok else 'does not hold'} in this window")
        print(f"  unlisted direction (b4 > 0, p < 0.05): {'holds' if un_ok else 'does not hold'} in this window"
              + (" (decides)" if decide_listing else " (reported)"))
        print("  3. the same on ranks (HC1)")
        ols(rankdata(y), [rankdata(size), rankdata(sig), sa, unl, frn], ["logsize", "sigma", "standalone", "unlisted", "foreign"])
        for label, mask in (("stand-alone", sa == 1), ("affiliated", sa == 0)):
            r, p = spearmanr(raw[mask], size[mask])
            idx = np.argsort(size[mask])
            parts = np.array_split(raw[mask][idx], 3)
            print(f"  {label}: {mask.sum()} banks; Spearman of level with log size {r:+.3f} (p {p:.2g}); "
                  f"median level by size tercile " + ", ".join(f"{100 * np.median(x):.2f}" for x in parts) + " points")
        for g in ("listed", "unlisted", "foreign"):
            m = np.array([r["grp"] == g for r in rows])
            if m.sum():
                print(f"  {g}: {m.sum()} banks; median level {100 * np.median(raw[m]):.2f} points; median log size {np.median(size[m]):.2f}")


if __name__ == "__main__":
    main()
