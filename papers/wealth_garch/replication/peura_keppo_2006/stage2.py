import csv
import gzip
import hashlib
import math
import statistics
import sys
from collections import defaultdict
from multiprocessing import Pool

import numpy as np
from scipy.optimize import fsolve
from scipy.stats import pearsonr, t as tdist

from stage1_barriers import M, f1, u0

DELTA, K, RHO = 0.5, 0.0025, 0.04
U1_GRID = (0.05, 0.15, 0.3, 0.45, 0.6)
U2_GRID = (0.5, 0.7, 0.85, 0.95, 0.99)
YEARS = range(1984, 2003)

THEIR_T1 = {
    "all": [(2.14, 3.69, 4.70, 5.01, 5.95, 10.43, 1.67), (.31, .73, .91, .92, 1.11, 1.88, .32),
            (.22, .48, .60, .76, 1.05, 1.89, .40), (.15, .15, -.37)],
    "high": [(2.91, 3.64, 4.09, 4.61, 5.27, 8.11, 1.42), (.32, .63, .81, .81, .97, 1.35, .29),
             (.41, .65, .78, .95, 1.19, 1.89, .42), (-.21, .58, -.33)],
    "low": [(2.14, 3.85, 5.31, 5.37, 6.36, 10.43, 1.82), (.31, .85, .97, 1.02, 1.18, 1.88, .31),
            (.22, .43, .49, .58, .60, 1.33, .27), (.26, .04, -.17)],
}
THEIR_T3 = {
    "all": {"avg": (5.01, .80, 2.40, 2.72), "sd": (1.67, .78, 1.63, 2.09),
            "corr": {"mu": (.15, -.47, -.48, -.47), "sigma": (.15, .98, .99, .99), "growth": (-.03, .07, .07, .06),
                     "size": (-.39, .11, .10, .10), "llp": (-.34, .49, .48, .47)}},
    "high": {"avg": (4.61, 1.20, 3.24, 3.78), "sd": (1.42, .81, 1.69, 2.24),
             "corr": {"mu": (-.21, -.38, -.44, -.42), "sigma": (.58, .98, .99, .99), "growth": (.19, -.02, -.01, -.01),
                      "size": (-.43, -.11, -.12, -.11), "llp": (-.16, .20, .20, .20)}},
}


def num(v):
    return None if v in (None, "", "None") else float(v)


def load_panel(path):
    panel = defaultdict(dict)
    with gzip.open(path, "rt", newline="") as fh:
        for r in csv.DictReader(fh):
            panel[r["CERT"]][int(r["REPDTE"][:4])] = {k: (r[k] if k == "BKCLASS" else num(r[k])) for k in r
                                                      if k not in ("CERT", "REPDTE")}
    return panel


def load_rates(path):
    by = defaultdict(list)
    with open(path) as fh:
        for r in csv.DictReader(fh):
            by[int(r["observation_date"][:4])].append(float(r["FEDFUNDS"]))
    return {y: statistics.fmean(v) / 100 for y, v in by.items() if len(v) == 12}


def bank_record(cert, s, rates, screen=False):
    if any(rec["BKCLASS"] != "N" for rec in s.values()):
        return None
    shares = [s[y]["RWAJT"] / s[y]["ASSET"] for y in range(1993, 2003)
              if y in s and s[y]["RWAJT"] and s[y]["ASSET"]]
    if not shares:
        return None
    share = statistics.fmean(shares)
    R, X = {}, {}
    for y, rec in s.items():
        if y >= 1993:
            if rec["RWAJT"] and rec["RBCT1J"] is not None and rec["RBCT2"] is not None:
                R[y] = rec["RWAJT"]
                X[y] = (rec["RBCT1J"] + rec["RBCT2"]) / R[y]
        elif rec["ASSET"] and rec["EQ"] is not None:
            R[y] = rec["ASSET"] * share
            X[y] = rec["EQ"] / R[y]
    roa = [s[y]["NETINC"] / R[y - 1] - rates[y] * X[y - 1] for y in range(1985, 2003)
           if y in s and y - 1 in R and s[y]["NETINC"] is not None and (not screen or steady(s, y))]
    buf = [s[y]["RBCRWAJ"] / 100 - 0.08 for y in range(1993, 2003) if y in s and s[y]["RBCRWAJ"]]
    llp = [s[y]["ELNATR"] / s[y]["ASSET"] for y in range(1985, 2003)
           if y in s and s[y]["ELNATR"] is not None and s[y]["ASSET"]]
    assets = [s[y]["ASSET"] for y in range(1985, 2003) if y in s and s[y]["ASSET"]]
    growth = [s[y]["ASSET"] / s[y - 1]["ASSET"] - 1 for y in range(1985, 2003)
              if y in s and y - 1 in s and s[y]["ASSET"] and s[y - 1]["ASSET"]]
    if len(roa) < 15 or len(buf) < 5 or not llp:
        return None
    mu, sigma = statistics.fmean(roa), statistics.stdev(roa)
    if mu <= 0:
        return None
    return {"cert": cert, "mu": mu, "sigma": sigma, "buf": statistics.fmean(buf), "bufmax": max(buf),
            "llp": statistics.fmean(llp), "size": statistics.fmean(assets), "growth": statistics.fmean(growth),
            "n": len(roa)}


def steady(s, y):
    a, b = s[y]["ASSET"], s[y - 1]["ASSET"]
    return bool(a and b) and 0.5 <= a / b <= 2


def barriers_scaled(mu, sigma):
    top = u0(mu, sigma, RHO)

    def eqs(v):
        a, b = v
        if not 0 < a < b < 1.5 * top:
            return [1.0, 1.0]
        g = lambda x: M(x, b, mu, sigma, RHO, DELTA, K)
        h = lambda x: f1(x, b, mu, sigma, RHO)
        d = lambda f, x: (f(x + 1e-7) - f(x - 1e-7)) / 2e-7
        return [g(a) - h(a), d(g, a) - d(h, a)]

    def admissible(a, b):
        if not 0 < a < b < top:
            return False
        return all(M(b * i / 200, b, mu, sigma, RHO, DELTA, K) <= f1(b * i / 200, b, mu, sigma, RHO) + 1e-12
                   for i in range(201))

    found = []
    for g1 in U1_GRID:
        for g2 in U2_GRID:
            sol, info, ok, msg = fsolve(eqs, [g1 * top, g2 * top], full_output=True, xtol=1e-12)
            if ok == 1 and max(abs(e) for e in eqs(sol)) < 1e-10 and admissible(*sol):
                if not any(abs(sol[0] - f[0]) < 1e-7 and abs(sol[1] - f[1]) < 1e-7 for f in found):
                    found.append(sol)
    return (found[0][0], found[0][1]) if len(found) == 1 else None


def solve(bank):
    sol = barriers_scaled(bank["mu"], bank["sigma"])
    return dict(bank, u0=u0(bank["mu"], bank["sigma"], RHO), u1=sol[0] if sol else None, u2=sol[1] if sol else None)


def pct(v):
    return 100 * v


def dist(xs):
    a = np.array(xs)
    return (a.min(), np.percentile(a, 25), np.median(a), a.mean(), np.percentile(a, 75), a.max(), a.std(ddof=1))


def corr(x, y):
    r, p = pearsonr(x, y)
    return r, p


def ols(y, x):
    x, y = np.asarray(x), np.asarray(y)
    X = np.column_stack([np.ones_like(x), x])
    beta, res, _, _ = np.linalg.lstsq(X, y, rcond=None)
    resid = y - X @ beta
    s2 = resid @ resid / (len(y) - 2)
    se = np.sqrt(np.diag(s2 * np.linalg.inv(X.T @ X)))
    r2 = 1 - resid @ resid / ((y - y.mean()) @ (y - y.mean()))
    return beta, se, r2, len(y) - 2


def table1(banks, label, theirs):
    print(f"--- Table 1, {label}: {len(banks)} banks   (theirs in brackets)")
    names = ("min", "p25", "median", "mean", "p75", "max", "SD")
    cols = [dist([pct(b[k]) for b in banks]) for k in ("buf", "mu", "sigma")]
    print(f"  {'':8s} {'buffer':>16s} {'mu':>16s} {'sigma':>16s}")
    for i, n in enumerate(names):
        print(f"  {n:8s} " + " ".join(f"{c[i]:7.2f} [{t[i]:5.2f}]" for c, t in zip(cols, theirs[:3])))
    pairs = (("buf", "mu"), ("buf", "sigma"), ("mu", "sigma"))
    out = {}
    for (a, b), t in zip(pairs, theirs[3]):
        r, p = corr([x[a] for x in banks], [x[b] for x in banks])
        out[(a, b)] = (r, p)
        print(f"  corr({a},{b}) {r:+.2f} (p {p:.3f})   [{t:+.2f}]")
    return out


def table3(banks, label, theirs):
    print(f"--- Table 3, {label}: {len(banks)} banks with u1, u2   (theirs in brackets)")
    cols = ("buf", "u1", "u2", "u0")
    print("  " + " ".join(f"{c:>15s}" for c in cols))
    for key, f in (("avg", lambda v: np.mean(v)), ("sd", lambda v: np.std(v, ddof=1))):
        print(f"  {key:5s}" + " ".join(f"{pct(f([b[c] for b in banks])):7.2f} [{t:5.2f}]"
                                       for c, t in zip(cols, theirs[key])))
    for var in ("mu", "sigma", "growth", "size", "llp"):
        vals = [corr([b[var] for b in banks], [b[c] for b in banks])[0] for c in cols]
        print(f"  {var:6s}" + " ".join(f"{v:+7.2f} [{t:+5.2f}]" for v, t in zip(vals, theirs["corr"][var])))


def fig5(banks, label):
    beta, se, r2, df = ols([b["buf"] for b in banks], [b["u2"] for b in banks])
    r, p = corr([b["buf"] for b in banks], [b["u2"] for b in banks])
    rm, pm = corr([b["bufmax"] for b in banks], [b["u2"] for b in banks])
    slope_below_one = (beta[1] - 1) / se[1] < tdist.ppf(0.025, df)
    intercept_above_zero = beta[0] / se[0] > tdist.ppf(0.975, df)
    print(f"--- Fig. 5, {label}: {len(banks)} banks: corr(buffer, u2) {r:+.2f} (p {p:.3f}); "
          f"OLS buffer = {pct(beta[0]):.2f}% ({pct(se[0]):.2f}) + {beta[1]:.2f} ({se[1]:.2f}) u2, R2 {r2:.2f}; "
          f"corr(max buffer, u2) {rm:+.2f}")
    return r, p, slope_below_one, intercept_above_zero


def run_sample(banks, label):
    print()
    print(f"===== SAMPLE {label}: {len(banks)} banks")
    mean_llp = statistics.fmean(b["llp"] for b in banks)
    high = [b for b in banks if b["llp"] > mean_llp]
    low = [b for b in banks if b["llp"] <= mean_llp]
    print(f"above-average provisions {len(high)} [29], other {len(low)} [32]")
    c_all = table1(banks, "all", THEIR_T1["all"])
    c_high = table1(high, "above-average provisions", THEIR_T1["high"])
    table1(low, "below-average provisions", THEIR_T1["low"])
    solved = [b for b in banks if b["u2"] is not None]
    hsolved = [b for b in high if b["u2"] is not None]
    print(f"barriers: unique admissible solution for {len(solved)} of {len(banks)} banks")
    table3(solved, "all", THEIR_T3["all"])
    table3(hsolved, "above-average provisions", THEIR_T3["high"])
    fig5([b for b in solved if b not in hsolved], "below-average provisions")
    r5, p5, below1, above0 = fig5(hsolved, "above-average provisions")
    ru2s = corr([b["u2"] for b in solved], [b["sigma"] for b in solved])[0]
    mean_buf = statistics.fmean(b["buf"] for b in banks)
    mean_u2 = statistics.fmean(b["u2"] for b in solved)
    mean_u0 = statistics.fmean(b["u0"] for b in banks)
    findings = {
        "F1 mu-sigma correlation negative and significant": c_all[("mu", "sigma")][0] < 0 and c_all[("mu", "sigma")][1] < 0.05,
        "F2 high-provision group: lower mean mu, higher mean sigma":
            statistics.fmean(b["mu"] for b in high) < statistics.fmean(b["mu"] for b in low)
            and statistics.fmean(b["sigma"] for b in high) > statistics.fmean(b["sigma"] for b in low),
        "F3 high-provision group: buffer-sigma correlation positive and significant":
            c_high[("buf", "sigma")][0] > 0 and c_high[("buf", "sigma")][1] < 0.05,
        "F4 u2-sigma correlation at least 0.9": ru2s >= 0.9,
        "F5 high-provision group: buffer-u2 positive significant, slope < 1, intercept > 0":
            r5 > 0 and p5 < 0.05 and below1 and above0,
        "F6 mean u2 and mean u0 below mean buffer": mean_u2 < mean_buf and mean_u0 < mean_buf,
        "F7 no bank with mean buffer below 2%": min(b["buf"] for b in banks) >= 0.02,
    }
    print(f"means: buffer {pct(mean_buf):.2f}% [5.0], u2 {pct(mean_u2):.2f}% [2.4], u0 {pct(mean_u0):.2f}% [2.7]; "
          f"min buffer {pct(min(b['buf'] for b in banks)):.2f}%; corr(u2, sigma) {ru2s:+.2f} [0.99]")
    for k, v in findings.items():
        print(f"  {k}: {'reproduced' if v else 'not reproduced'}")


def main(panel_path, rates_path):
    for p in (panel_path, rates_path):
        print(f"{p} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    panel = load_panel(panel_path)
    rates = load_rates(rates_path)
    banks = [b for b in (bank_record(c, s, rates) for c, s in panel.items()) if b]
    print(f"institutions {len(panel)}; national banks passing the filters (sample A) {len(banks)}")
    with Pool() as pool:
        solved = pool.map(solve, sorted(banks, key=lambda b: int(b["cert"])))
    run_sample(solved, "A (all national banks)")
    run_sample(sorted(solved, key=lambda b: -b["size"])[:61], "B (61 largest)")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
