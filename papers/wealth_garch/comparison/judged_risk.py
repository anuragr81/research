import csv
import gzip
import hashlib
import math
import os
import re
import statistics
from functools import partial
from multiprocessing import Pool

import numpy as np

from sample import ANNUAL, RATES, R_REF, ROOT, samples
from stage1_barriers import u0
from window import FAR, THIN, load_years, observed, quarters, ratio, show

SEED, PATHS, DT, STEPS_Q, YEARS = 20261024, 200, 1 / 360, 90, 10
START = "20041231"
RHO = 0.04
ZS = (1.645, 2.326, 3.090)
Z_CENTRAL = 2.326
HORIZON = 2.25
CACHE = os.path.expanduser("~/.cache/wealth_garch")
PRICES = os.path.join(CACHE, "stooq_20261023")
LISTS = [os.path.join(CACHE, "nasdaqtrader_20261020", f) for f in ("nasdaqlisted.txt", "otherlisted.txt")]
LIST_SHA = {"nasdaqlisted.txt": "df221177204f1c56add0c52adcd8f19c7371bde662eb34c8b32c5bba8bf4f5b0",
            "otherlisted.txt": "cab8d0e5cb077acb38ebe201113a909b7ee3784c32249281d83399c0b0d5732f"}
CLAIMS = {2013: os.path.join(ROOT, "lit", "dfast_2013", "CLAIMS.md"), 2014: os.path.join(ROOT, "lit", "dfast_2014", "CLAIMS.md")}
BNY_STEP = "2007-07-02"

FIRMS = [
    ("1039502", "JPMorgan Chase & Co.", "JPM", "jpm_us_d.csv", "6836318dc26f4abb8df89bed430875940d85b05ecc6fdaceeb1cf5ed1ab10a39", "JP Morgan Chase & Co. Common Stock"),
    ("1073757", "Bank of America Corporation", "BAC", "bac_us_d.csv", "4a3bae9500c923c87c50ee3518c6f578fcc2c0c2a785814015bd54266ee18fca", "Bank of America Corporation Common Stock"),
    ("1951350", "Citigroup Inc.", "C", "c_us_d.csv", "4cda892b1209869180b3aba09f38435db9d7b5b572483963ba78ad3c5506a21c", "Citigroup, Inc. Common Stock"),
    ("1120754", "Wells Fargo & Company", "WFC", "wfc_us_d.csv", "0129de4f0f11108f8bca172bda74f96e5a4ae9d37002e9d4cf352c124d01c5be", "Wells Fargo & Company Common Stock"),
    ("1119794", "U.S. Bancorp", "USB", "usb_us_d.csv", "dd5596935e7a31f249b66dcb284b09c177adcfcc46f17bcd0ab6ecae53346868", "U.S. Bancorp Common Stock"),
    ("1069778", "The PNC Financial Services Group, Inc.", "PNC", "pnc_us_d.csv", "2814e9ad72e6f91cf075cff1018a8da02c320e02147dc6b2aa119835c2afd36b", "PNC Financial Services Group, Inc. (The) Common Stock"),
    ("2277860", "Capital One Financial Corporation", "COF", "cof_us_d.csv", "717d908fd3829063fb7236da8a0ca6f780448154fd2675ab5944dbee6786c7ab", "Capital One Financial Corporation Common Stock"),
    ("1068025", "KeyCorp", "KEY", "key_us_d.csv", "4cfcb32565b4b7c030862d52ce64de5c7cb213a84ce1af9e60ab5252464c5872", "KeyCorp Common Stock"),
    ("1068191", "Huntington Bancshares Incorporated", "HBAN", "hban_us_d.csv", "a5b1f3ad1136d7b42062bd40d585d55e7832578a1449b778ad20e0344389d9a5", "Huntington Bancshares Incorporated - Common Stock"),
    ("1027004", "Zions Bancorporation", "ZION", "zion_us_d.csv", "5f56697173bfa0ea69066ca39f608062eb1851719967c829dbaa05aa52262a83", "Zions Bancorporation N.A. - Common Stock"),
    ("3587146", "The Bank of New York Mellon Corporation", "BNY", "bny_us_d.csv", "ab1061bb66d51340e15a4b4439080b6116c1db0398fb9961d13aae4b90423fd6", "The Bank of New York Mellon Corporation Common Stock"),
]


def sha(path):
    return hashlib.sha256(open(path, "rb").read()).hexdigest()


def listed():
    names = {}
    for path in LISTS:
        assert sha(path) == LIST_SHA[os.path.basename(path)], path
        with open(path) as fh:
            for r in csv.reader(fh, delimiter="|"):
                names.setdefault(r[0], r[1])
    return names


def declines():
    rows = {}
    for year, path in CLAIMS.items():
        for line in open(path):
            cells = [c.strip() for c in line.split("|")]
            if len(cells) < 5 or not re.fullmatch(r"DF1[34]-Q\d+", cells[1]):
                continue
            m = re.fullmatch(r"(.*\S)\s+(\d+\.\d)\s+(\d+\.\d)\s+(\d+\.\d)", cells[3])
            if m:
                rows.setdefault(m.group(1), {})[year] = (cells[1], float(m.group(2)) - float(m.group(4)))
    return rows


def equity_sigma(path, drop=None):
    closes = []
    with open(path) as fh:
        for r in csv.DictReader(fh):
            if "2005-01-01" <= r["Date"] <= "2014-12-31":
                closes.append((r["Date"], float(r["Close"])))
    changes = [0.0 if d == drop else math.log(c / closes[i - 1][1]) for i, (d, c) in enumerate(closes) if i > 0]
    return math.sqrt(252) * statistics.stdev(changes), len(changes)


def holders(certs):
    hc = {}
    with gzip.open(os.path.join(ROOT, "common_data", "fdic_q_2014.csv.gz"), "rt", newline="") as fh:
        for r in csv.DictReader(fh):
            if r["CERT"] in certs and r["REPDTE"] == "20141231":
                hc[r["CERT"]] = r["RSSDHCR"].strip()
    return hc


def mean_ratio(q, cert):
    xs = [ratio(q, cert, f"{y}1231") for y in range(2005, 2015)]
    xs = [x for x in xs if x is not None]
    return statistics.fmean(xs)


def paths(b, minimum, key):
    s = b[key]
    rng = np.random.default_rng([SEED, int(b["cert"]), int(round(minimum * 1e4)), int(round(s * 1e7))])
    mu, sig = b["mu"], b["sigma"]
    top = u0(mu, s, RHO)
    x = np.full(PATHS, b["x0_train"] - minimum)
    alive = x > 0
    x = np.minimum(x, top)
    f1 = f2 = n1 = n2 = 0
    pre_b = x + minimum - R_REF
    sq = math.sqrt(DT)
    for _ in range(4 * YEARS):
        divq = np.zeros(PATHS, dtype=bool)
        for _ in range(STEPS_Q):
            x = x + mu * DT + s * sq * rng.standard_normal(PATHS)
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
    res = {"F1": (sum(o[0] for o in out), sum(o[1] for o in out)), "F2": (sum(o[2] for o in out), sum(o[3] for o in out))}
    show(res)
    return {k: 100 * h / n for k, (h, n) in res.items()}


def score(label, pred, obs):
    d1, d2 = abs(pred["F1"] - obs["F1"]), abs(pred["F2"] - obs["F2"])
    print(f"  {label}: |dF1| {d1:.1f} + |dF2| {d2:.1f} = {d1 + d2:.1f} points; F1 within 10: {d1 <= 10}; F2 within 10: {d2 <= 10}")
    return d1 + d2


def main():
    for p in [ANNUAL, RATES]:
        print(f"{p.split('/')[-1]} sha256 {sha(p)}")
    names = listed()
    dec = declines()
    firms = {}
    print("\nfirms: RSSD, ticker, listed name, stress-test declines (claim, points), D, sigma_E, changes")
    for rssd, fed, tick, fname, digest, lname in FIRMS:
        path = os.path.join(PRICES, fname)
        assert sha(path) == digest, fname
        assert names[tick] == lname, tick
        years = dec[fed]
        d = statistics.fmean(v[1] for v in years.values())
        se, n = equity_sigma(path)
        firms[rssd] = {"D": d / 100, "sE": se, "tick": tick}
        rows = ", ".join(f"{years[y][0]} {years[y][1]:.1f}" for y in sorted(years))
        print(f"  {rssd} {tick:5s} {lname} | {rows} | D {d:.2f} | sigma_E {se:.4f} | {n}")
    se_bny, n_bny = equity_sigma(os.path.join(PRICES, "bny_us_d.csv"), drop=BNY_STEP)
    print(f"  BNY with the {BNY_STEP} change set to zero: sigma_E {se_bny:.4f} | {n_bny}")
    samp, _ = samples()
    q = load_years(range(2004, 2015))
    qs = quarters(2005, 2014)
    for name in ("B", "A"):
        hc = holders({b["cert"] for b in samp[name]})
        banks = []
        for b in samp[name]:
            x0 = ratio(q, b["cert"], START)
            f = firms.get(hc.get(b["cert"], ""))
            if x0 is None or f is None:
                continue
            X = mean_ratio(q, b["cert"])
            sA = {z: f["D"] / (z * math.sqrt(HORIZON)) for z in ZS}
            bny = se_bny if f["tick"] == "BNY" else f["sE"]
            banks.append(dict(b, x0_train=x0, X=X, tick=f["tick"], sB=f["sE"] * X, sB_bny=bny * X,
                              **{f"sA{z}": v for z, v in sA.items()}))
        print(f"\n===== sample {name}: {len(banks)} banks with a 2004Q4 buffer whose top holding company both measures cover")
        if not banks:
            continue
        print("banks: cert, ticker, X_i, sigma_i, sigma_A at z = 2.326, sigma_B, ratios to sigma_i")
        for b in sorted(banks, key=lambda b: -b["size"]):
            sa = b[f"sA{Z_CENTRAL}"]
            print(f"  {b['cert']:>6s} {b['tick']:5s} X {b['X']:.4f} sigma_i {b['sigma']:.5f} sigma_A {sa:.5f} sigma_B {b['sB']:.5f} | {sa / b['sigma']:.2f} {b['sB'] / b['sigma']:.2f}")
        print("observed, 2005-2014")
        obs_raw = observed(banks, q, qs, R_REF)
        show(obs_raw)
        obs = {k: 100 * h / n for k, (h, n) in obs_raw.items() if k in ("F1", "F2")}
        res = {}
        with Pool() as pool:
            for minimum, mlabel in ((0.08, "minimum 8%"), (R_REF, "minimum at R")):
                for z in ZS:
                    tag = " (central)" if z == Z_CENTRAL else ""
                    res[("A", z, minimum)] = report(f"(A) stress tests, z = {z}{tag}, {mlabel}", pool.map(partial(paths, minimum=minimum, key=f"sA{z}"), banks))
                res[("B", minimum)] = report(f"(B) equity volatility, {mlabel}", pool.map(partial(paths, minimum=minimum, key="sB"), banks))
                res[("Bbny", minimum)] = report(f"(B) with BNY's {BNY_STEP} change set to zero, {mlabel}", pool.map(partial(paths, minimum=minimum, key="sB_bny"), banks))
        print("scores against observed F1 and F2 (points)")
        for minimum, mlabel in ((0.08, "minimum 8%"), (R_REF, "minimum at R")):
            a = score(f"(A) central, {mlabel}", res[("A", Z_CENTRAL, minimum)], obs)
            bb = score(f"(B), {mlabel}", res[("B", minimum)], obs)
            if name == "B" and minimum == 0.08:
                print(f"  rule (sample B, minimum 8%, central): better measure {'(A) stress tests' if a < bb else '(B) equity volatility' if bb < a else 'tie'}")


if __name__ == "__main__":
    main()
