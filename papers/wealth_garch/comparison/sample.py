import os
import statistics
import sys
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.join(HERE, "..")
PK = os.path.join(ROOT, "replication", "peura_keppo_2006")
sys.path.insert(0, PK)
sys.path.insert(0, os.path.join(ROOT, "06_empirical_starter"))

import csv
import gzip

from distress_targeting import MIN_ASSET, qflow, shift
from stage2 import load_panel, load_rates, num, steady

R_REF = 0.105
ANNUAL = os.path.join(PK, "fdic_dec_2004_2024.csv.gz")
RATES = os.path.join(PK, "fedfunds.csv")
QUARTERLY = [os.path.join(ROOT, "common_data", f"fdic_q_{y}.csv.gz") for y in range(2014, 2025)]
START = "20141231"


TEXT = ("CERT", "REPDTE", "BKCLASS", "RSSDHCR")


def quarterly():
    panel = defaultdict(dict)
    for path in QUARTERLY:
        with gzip.open(path, "rt", newline="") as fh:
            for r in csv.DictReader(fh):
                panel[r["CERT"]][r["REPDTE"]] = {k: num(v) for k, v in r.items() if k not in TEXT}
    return panel


def ratio(q, cert, d):
    rec = q.get(cert, {}).get(d)
    if rec is None or rec["ASSET"] is None or rec["ASSET"] < MIN_ASSET:
        return None
    if rec["RBCT1J"] is None or rec["RBCT2"] is None or not rec["RWAJT"] or rec["RWAJT"] <= 0:
        return None
    return (rec["RBCT1J"] + rec["RBCT2"]) / rec["RWAJT"]


def inputs(cert, s, rates):
    s = {y: r for y, r in s.items() if 2004 <= y <= 2014}
    if not s or any(rec["BKCLASS"] != "N" for rec in s.values()):
        return None
    R, X = {}, {}
    for y, rec in s.items():
        if rec["RWAJT"] and rec["RBCT1J"] is not None and rec["RBCT2"] is not None:
            R[y] = rec["RWAJT"]
            X[y] = (rec["RBCT1J"] + rec["RBCT2"]) / R[y]
    roa = [s[y]["NETINC"] / R[y - 1] - rates[y] * X[y - 1] for y in range(2005, 2015)
           if y in s and y - 1 in R and s[y]["NETINC"] is not None and steady(s, y)]
    assets = [s[y]["ASSET"] for y in range(2005, 2015) if y in s and s[y]["ASSET"]]
    if len(roa) < 8 or not assets:
        return None
    mu, sigma = statistics.fmean(roa), statistics.stdev(roa)
    if mu <= 0:
        return None
    return {"cert": cert, "mu": mu, "sigma": sigma, "size": statistics.fmean(assets)}


def samples():
    annual = load_panel(ANNUAL)
    rates = load_rates(RATES)
    q = quarterly()
    banks = []
    for cert, s in annual.items():
        b = inputs(cert, s, rates)
        if b is None:
            continue
        x0 = ratio(q, cert, START)
        if x0 is None:
            continue
        rec = q[cert][START]
        b.update(x0=x0, asset_rwa=rec["ASSET"] / rec["RWAJT"])
        banks.append(b)
    banks.sort(key=lambda b: int(b["cert"]))
    big = sorted(banks, key=lambda b: -b["size"])[:61]
    return {"A": banks, "B": sorted(big, key=lambda b: int(b["cert"]))}, q


QUARTERS = [f"{y}{md}" for y in range(2015, 2025) for md in ("0331", "0630", "0930", "1231")]
