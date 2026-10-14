import csv
import gzip
import hashlib
import io
import math
import os
import re
import sys
import zipfile
from collections import defaultdict
from datetime import date, timedelta

import numpy as np
import pandas as pd
import statsmodels.api as sm

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from gmm import system_gmm

GH = os.path.join(os.path.dirname(HERE), "gordon_hirtle_2026")
DEC_ITEMS = ["RSSD9001", "RSSD9348", "RSSD9101", "RSSD9346", "BHCK8274", "BHCK3792", "BHCKA223", "BHCK2170", "BHCK4340",
             "BHCK4460", "BHCK4598", "BHCB3187", "BHOD3187", "BHCB6648", "BHOD6648", "BHCK2948", "BHCK1766", "BHDM1480",
             "BHDM1415", "BHCK2122", "BHCKA126", "BHCKA127", "BHCK8723", "BHCK8724", "BHCK8725", "BHCK8726", "BHCK8727",
             "BHCK8728", "BHCK8729", "BHCK8730", "BHCK8731", "BHCK8732", "BHCK4300", "BHCB2389", "BHOD2389"]
TEXT = {"RSSD9101", "RSSD9346"}
NOTIONAL = ["BHCKA126", "BHCKA127"] + [f"BHCK{c}" for c in range(8723, 8733)]
MEASURES = {"CAPLEV": ("BHCK8274", "BHCK2170", 5.0, 4.0), "CAPTIER1": ("BHCK8274", "BHCKA223", 6.0, 4.0),
            "CAPTOTAL": ("BHCK3792", "BHCKA223", 10.0, 8.0)}
THEIRS = {"CAPLEV": (.0846, .6024, .4113, .0864), "CAPTIER1": (.1208, .4864, .2785, .1440),
          "CAPTOTAL": (.1381, .5632, .3604, .1694)}
STEP1_TARGET = {"CAPLEV": .0830, "CAPTIER1": .1302, "CAPTOTAL": .1500}
ZNAMES = ["WELLCAP_600TO800", "WELLCAP_400TO600", "WELLCAP_200TO400", "WELLCAP_0TO200", "ADEQCAP", "UNDERCAP", "SIG_UNDERCAP"]


def sha(path):
    return hashlib.sha256(open(path, "rb").read()).hexdigest()


def num(v):
    v = v.strip()
    return float(v) if re.fullmatch(r"-?\d+(\.\d+)?", v) else None


def read_y9c(path, items):
    reader = csv.reader(gzip.open(path, "rt", encoding="latin-1"))
    hdr = [h.strip().upper() for h in next(reader)]
    col = {h: k for k, h in enumerate(hdr)}
    out = {}
    for r in reader:
        if len(r) < len(hdr) or not r[col["RSSD9001"]].strip().isdigit():
            continue
        rec = {k: (r[col[k]].strip() if k in TEXT else num(r[col[k]])) for k in items if k in col}
        out[int(rec["RSSD9001"])] = rec
    return out


def cpi_annual(path):
    by = defaultdict(list)
    for r in csv.DictReader(gzip.open(path, "rt")):
        by[int(r["observation_date"][:4])].append(float(r["CPIAUCSL"]))
    return {y: sum(v) / len(v) for y, v in by.items() if len(v) == 12}


def links(path):
    out = defaultdict(list)
    for r in csv.DictReader(gzip.open(path, "rt", encoding="latin-1")):
        out[int(r["entity"])].append((r["dt_start"].strip(), r["dt_end"].strip()))
    return out


def parents(rel_zip):
    zf = zipfile.ZipFile(rel_zip)
    name = [n for n in zf.namelist() if n.lower().endswith(".csv")][0]
    reader = csv.DictReader(io.TextIOWrapper(zf.open(name), encoding="latin-1"))
    reader.fieldnames = [f.strip().strip("#").upper() for f in reader.fieldnames]
    out = defaultdict(list)
    for r in reader:
        if r["RELN_LVL"].strip() == "1" and r["CTRL_IND"].strip() == "1":
            out[int(r["ID_RSSD_OFFSPRING"])].append((int(r["ID_RSSD_PARENT"]), r["DT_START"].strip(), r["DT_END"].strip()))
    return out


def ancestors(entity, day, up):
    seen, todo = set(), [entity]
    while todo:
        e = todo.pop()
        for p, s, t in up.get(e, []):
            if s <= day <= t and p not in seen:
                seen.add(p)
                todo.append(p)
    return seen


def acquisitions(trans_zip, up, years, banks=None):
    zf = zipfile.ZipFile(trans_zip)
    reader = csv.DictReader(io.TextIOWrapper(zf.open(zf.namelist()[0]), encoding="latin-1"))
    reader.fieldnames = [f.strip("#") for f in reader.fieldnames]
    count = defaultdict(int)
    for r in reader:
        if r["TRNSFM_CD"].strip() not in ("1", "9"):
            continue
        if banks is not None and int(r["ID_RSSD_PREDECESSOR"]) not in banks:
            continue
        d = r["DT_TRANS"].strip()
        if int(d[:4]) not in years:
            continue
        before = (date(int(d[:4]), int(d[4:6]), int(d[6:])) - timedelta(days=1)).strftime("%Y%m%d")
        succ, pred = int(r["ID_RSSD_SUCCESSOR"]), int(r["ID_RSSD_PREDECESSOR"])
        owners = ancestors(succ, d, up) | {succ}
        prior = ancestors(pred, before, up) | {pred}
        for b in owners - prior:
            count[(b, int(d[:4]))] += 1
    return count


def stdroa(quarters, rid, year):
    roas = []
    for y in range(year - 2, year + 1):
        prev = 0.0
        for q in ("03", "06", "09", "12"):
            rec = quarters.get((y, q), {}).get(rid)
            if rec is None or rec.get("BHCK4300") is None or not rec.get("BHCK2170"):
                return None
            roas.append((rec["BHCK4300"] - prev) / rec["BHCK2170"])
            prev = rec["BHCK4300"]
    return float(np.std(roas, ddof=1))


def z(rec, k):
    return rec.get(k) or 0.0


def position(ratio, well, adequate):
    pct = 100 * ratio
    if pct >= well + 8:
        return None
    for name, lo in (("WELLCAP_600TO800", well + 6), ("WELLCAP_400TO600", well + 4), ("WELLCAP_200TO400", well + 2),
                     ("WELLCAP_0TO200", well)):
        if pct >= lo:
            return name
    if pct >= adequate:
        return "ADEQCAP"
    return "UNDERCAP" if pct >= adequate - 1 else "SIG_UNDERCAP"


def build(dec, quarters, cpi, link, acq, d2=False):
    rows = []
    for t in range(1997, 2007):
        for rid, rec in dec[t].items():
            prev = dec[t - 1].get(rid)
            hh = rec.get("RSSD9348")
            if prev is None or not (hh in (None, 0.0) or int(hh) == rid) or rec.get("RSSD9101") != "Y" \
                    or rec.get("RSSD9346") not in ("BHC", "FHD"):
                continue
            if not any(s <= f"{t}1231" <= e for s, e in link.get(rid, [])):
                continue
            need = ["BHCK8274", "BHCK3792", "BHCKA223", "BHCK2170", "BHCK4340"]
            if any(not rec.get(k) for k in need) or any(not prev.get(k) for k in need + ["BHCK2948", "BHCK2122"]):
                continue
            row = {"id": rid, "year": t}
            div_prev = z(prev, "BHCK4460") + z(prev, "BHCK4598")
            for m, (kk, aa, well, adequate) in MEASURES.items():
                row[m] = rec[kk] / rec[aa]
                row["DNK_" + m] = (prev[kk] + rec["BHCK4340"] - div_prev) / rec[aa]
                row["Z_" + m] = position(prev[kk] / prev[aa], well, adequate)
            sd = stdroa(quarters, rid, t - 1)
            if sd is None:
                continue
            row["STDROA"] = sd
            row["NMERGER"] = acq.get((rid, t), 0)
            row["LNASSETS"] = math.log(prev["BHCK2170"])
            row["D50B"] = 1.0 if prev["BHCK2170"] * cpi[2001] / cpi[t - 1] > 50_000_000 else 0.0
            row["D50B_LNASSETS"] = row["D50B"] * row["LNASSETS"]
            savings = z(prev, "BHCB2389") + z(prev, "BHOD2389") if d2 else 0.0
            row["RETAILDEP"] = (z(prev, "BHCB3187") + z(prev, "BHOD3187") + z(prev, "BHCB6648") + z(prev, "BHOD6648") + savings) / prev["BHCK2948"]
            row["BUSINESSLOAN"] = (z(prev, "BHCK1766") + z(prev, "BHDM1480") + z(prev, "BHDM1415")) / prev["BHCK2170" if d2 else "BHCK2122"]
            row["OFFBSHEET"] = 1.0 if sum(z(prev, k) for k in NOTIONAL) > 10 * prev["BHCK2170"] else 0.0
            rows.append(row)
    df = pd.DataFrame(rows)
    cut = {m: df[m].quantile(0.99) for m in MEASURES}
    df = df[np.logical_and.reduce([df[m] <= cut[m] for m in MEASURES])].reset_index(drop=True)
    for y in range(1998, 2007):
        df[f"yr{y}"] = (df["year"] == y).astype(float)
    return df, cut


def within(ours, theirs, tol):
    return "holds" if abs(ours - theirs) <= tol else "differs"


def run(df, m, lags=(2, 3), collapse=False, level=True):
    xs = ["STDROA", "NMERGER", "LNASSETS", "D50B", "D50B_LNASSETS", "RETAILDEP"] + (["BUSINESSLOAN", "OFFBSHEET"] if m == "CAPLEV" else [])
    yrs = [f"yr{y}" for y in range(1998, 2007)]
    d = df.rename(columns={m: "k", "DNK_" + m: "dnk"})
    mean_k, (t_k, t_l1, t_l2, t_tgt) = d["k"].mean(), THEIRS[m]
    s1 = system_gmm(d, "k", ["dnk"] + xs + yrs, [("dnk",) + tuple(lags)], xs + yrs, level=level, collapse=collapse)
    b = dict(zip(s1["labels"], s1["b2"]))
    lam = 1 - b["dnk"]
    d["target1"] = (b.get("_cons", 0.0) + sum(b[x] * d[x] for x in xs)) / lam
    d["dev"] = d["target1"] - d["dnk"]
    zcols = []
    for zn in ZNAMES:
        d["DEV_" + zn] = d["dev"] * (d["Z_" + m] == zn)
        zcols.append("DEV_" + zn)
    X2 = d[["dev"] + zcols].copy()
    for y in range(1997, 2007):
        X2[f"y{y}"] = (d["year"] == y).astype(float)
    fit2 = sm.OLS(d["k"] - d["dnk"], X2).fit(cov_type="cluster", cov_kwds={"groups": d["id"]})
    d["lam"] = fit2.params["dev"] + sum(fit2.params["DEV_" + zn] * (d["Z_" + m] == zn) for zn in ZNAMES)
    d["y3"] = d["k"] - d["dnk"] * (1 - d["lam"])
    lx = []
    for x in xs:
        d["lam_" + x] = d["lam"] * d[x]
        lx.append("lam_" + x)
    s3 = system_gmm(d, "y3", ["lam"] + lx + yrs, [("dnk",) + tuple(lags)], ["lam"] + lx + yrs, level=level, collapse=collapse)
    b3 = dict(zip(s3["labels"], s3["b2"]))
    se3 = dict(zip(s3["labels"], s3["se2"]))
    d["target3"] = b3["lam"] + sum(b3["lam_" + x] * d[x] for x in xs)
    print(f"\n===== {m}: bank-years {len(d)}, holding companies {d['id'].nunique()}")
    print(f"  T1 mean ratio {mean_k:.4f} [{t_k}] -> {within(mean_k, t_k, 0.010)}")
    print(f"  Step 1: lambda {lam:.4f} [{t_l1}] (DNK coefficient {b['dnk']:.4f}, se {dict(zip(s1['labels'], s1['se2']))['dnk']:.4f}); "
          f"{s1['nobs']} differenced obs, {s1['ninst']} instruments, Hansen {s1['hansen']:.1f} on {s1['df']} df; "
          f"mean target {d['target1'].mean():.4f} [{STEP1_TARGET[m]}] -> T2 {within(lam, t_l1, 0.10)}")
    print(f"  Step 2: mean lambda_it {d['lam'].mean():.4f} [{t_l2}] -> T3 {within(d['lam'].mean(), t_l2, 0.10)}")
    for c in ["dev"] + zcols:
        print(f"    {c:22s} {fit2.params[c]: .4f} (t {fit2.tvalues[c]: .2f}, p {fit2.pvalues[c]:.4f}, n {int((d['Z_' + m] == c[4:]).sum()) if c != 'dev' else len(d)})")
    t4 = all(fit2.params["DEV_" + zn] > 0 and fit2.pvalues["DEV_" + zn] < 0.05 for zn in ZNAMES[1:4])
    print(f"  T4 400TO600, 200TO400, 0TO200 positive at 5% -> {'holds' if t4 else 'differs'}")
    tgt = d["target3"].mean()
    above = "" if m == "CAPLEV" else f"; mean target above mean ratio: {tgt > mean_k}"
    t5 = abs(tgt - t_tgt) <= 0.010 and (m == "CAPLEV" or tgt > mean_k)
    print(f"  Step 3: mean target {tgt:.4f} [{t_tgt}]{above} -> T5 {'holds' if t5 else 'differs'}")
    for x in xs:
        print(f"    lam_{x:14s} {b3['lam_' + x]: .4f} (se {se3['lam_' + x]:.4f})")
    t6 = b3["lam_LNASSETS"] < 0 and (m == "CAPLEV" or b3["lam_STDROA"] > 0)
    print(f"  T6 LNASSETS negative{'' if m == 'CAPLEV' else ', STDROA positive'} -> {'holds' if t6 else 'differs'}")
    return {"lam1": lam, "lam2": d["lam"].mean(), "tgt3": tgt,
            "verdicts": [abs(mean_k - t_k) <= 0.010, abs(lam - t_l1) <= 0.10, abs(d["lam"].mean() - t_l2) <= 0.10, t4, t5, t6]}


def main(paths):
    for p in paths:
        print(f"{os.path.basename(p)} sha256 {sha(p)}")
    arg = {os.path.basename(p): p for p in paths}
    cpi = cpi_annual(arg["cpiaucsl_1989_2024.csv.gz"])
    dec = {y: read_y9c(arg[f"bhcf{y}12.csv.gz"], DEC_ITEMS) for y in range(1996, 2007)}
    quarters = {}
    for y in range(1994, 2006):
        for q in ("03", "06", "09", "12"):
            quarters[(y, q)] = read_y9c(arg[f"bhcf{y}{q}.csv.gz"], ["RSSD9001", "BHCK4300", "BHCK2170"])
    link = links(arg["crsp_20240930v2.csv.gz"])
    up = parents(arg["CSV_RELATIONSHIPS.zip"])
    acq = acquisitions(arg["CSV_TRANSFORMATIONS.zip"], up, set(range(1997, 2007)))
    df, cut = build(dec, quarters, cpi, link, acq)
    print(f"\nsample: bank-years {len(df)}, holding companies {df['id'].nunique()}, 99th-percentile cuts "
          + ", ".join(f"{m} {c:.4f}" for m, c in cut.items()))
    print("by year: " + " ".join(f"{y}:{n}" for y, n in df.groupby("year").size().items()))
    print("means: " + ", ".join(f"{v} {df[v].mean():.4f}" for v in ["STDROA", "NMERGER", "RETAILDEP", "BUSINESSLOAN", "OFFBSHEET", "D50B"])
          + "  [theirs .0034, .066, .605, .281, .009, .054]")
    for m in MEASURES:
        run(df, m)


if __name__ == "__main__":
    main(sys.argv[1:])
