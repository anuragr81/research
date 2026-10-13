import csv
import gzip
import hashlib
import io
import math
import os
import re
import statistics
import sys
import zipfile
from collections import defaultdict

import numpy as np
import statsmodels.api as sm

ITEMS = ["RSSD9001", "RSSD9348", "RSSD9101", "BHCK2170", "BHCK4340", "BHCKB511", "BHCKB508", "BHCK3217",
         "BHCK4356", "BHCK3579", "BHCK3580", "BHCK4782", "BHCK4783", "BHCK4460", "BHCK3210",
         "BHCK8274", "BHCKA223", "BHCA8274", "BHCAA223", "RSSD9364"]
CONSERVATION = {2016: 0.625, 2017: 1.25, 2018: 1.875}


def sha(path):
    return hashlib.sha256(open(path, "rb").read()).hexdigest()


def cpi_annual(path):
    by_year = defaultdict(list)
    for r in csv.DictReader(gzip.open(path, "rt")):
        by_year[int(r["observation_date"][:4])].append(float(r["CPIAUCSL"]))
    return {y: sum(v) / len(v) for y, v in by_year.items() if len(v) == 12}


def rows_of(path):
    if path.endswith(".csv.gz"):
        year = int(re.search(r"bhcf(\d{4})12", path).group(1))
        reader = csv.reader(gzip.open(path, "rt", encoding="latin-1"))
        return year, reader
    zf = zipfile.ZipFile(path)
    name = zf.namelist()[0]
    year = int(re.search(r"(\d{4})1231", os.path.basename(path)).group(1))
    text = io.TextIOWrapper(zf.open(name), encoding="latin-1")
    return year, csv.reader(text, delimiter="^", quoting=csv.QUOTE_NONE)


def num(v):
    v = v.strip()
    return float(v) if re.fullmatch(r"-?\d+(\.\d+)?", v) else None


def load(path):
    year, reader = rows_of(path)
    hdr = [h.strip().upper() for h in next(reader)]
    col = {h: k for k, h in enumerate(hdr)}
    out = []
    for r in reader:
        if len(r) < len(hdr) or not r[col["RSSD9001"]].strip().isdigit():
            continue
        rec = {k: (r[col[k]].strip() if k in ("RSSD9101",) else num(r[col[k]])) for k in ITEMS if k in col}
        out.append(rec)
    return year, out, "RSSD9348" in col


def controlled_offspring(path):
    zf = zipfile.ZipFile(path)
    name = [n for n in zf.namelist() if n.lower().endswith(".csv")][0]
    reader = csv.DictReader(io.TextIOWrapper(zf.open(name), encoding="latin-1"))
    reader.fieldnames = [f.strip().strip("#").upper() for f in reader.fieldnames]
    spans = defaultdict(list)
    for r in reader:
        if all(r[k].strip() == "1" for k in ("RELN_LVL", "CTRL_IND", "REG_IND", "EQUITY_IND")):
            start, end = r["DT_START"].strip()[:8], r["DT_END"].strip()[:8]
            spans[int(r["ID_RSSD_OFFSPRING"])].append((start, end))
    return {year: {i for i, ss in spans.items() if any(s <= f"{year}1231" < e for s, e in ss)}
            for year in range(2015, 2025)}


def top_by_holder(rec):
    hh = rec.get("RSSD9348")
    return hh in (None, 0.0) or int(hh) == int(rec["RSSD9001"])


def requirement(year):
    if year <= 2014:
        return 4.0
    return 6.0 + CONSERVATION.get(year, 2.5 if year >= 2019 else 0.0)


def z(v):
    return 0.0 if v is None else v


def bank_year(rec, year, cpi, top):
    rid = int(rec["RSSD9001"])
    dom = rec.get("RSSD9101") == "Y"
    assets, ni = rec.get("BHCK2170"), rec.get("BHCK4340")
    if assets is None or ni is None:
        return None
    real = assets * cpi[2024] / cpi[year]
    if not (top and dom and real > 3_000_000):
        return None
    oci = z(rec.get("BHCKB511")) if year >= 2001 else 0.0
    prior = (rec.get("BHCKB508") if year >= 2001 else rec.get("BHCK3217"))
    if prior is None:
        return None
    prior += z(rec.get("BHCK4356"))
    conv = z(rec.get("BHCK3580"))
    iss = z(rec.get("BHCK3579")) + z(rec.get("BHCK4782")) + max(conv, 0.0)
    rep = z(rec.get("BHCK4783")) + max(-conv, 0.0)
    div = z(rec.get("BHCK4460"))
    t1, rwa = (rec.get("BHCK8274"), rec.get("BHCKA223")) if year <= 2014 else (rec.get("BHCA8274"), rec.get("BHCAA223"))
    buf = 100 * t1 / rwa - requirement(year) if year >= 1996 and t1 is not None and rwa else None
    earn = ni + oci
    return {"id": rid, "year": year, "assets": assets, "real": real, "prior": prior, "earn": earn, "ni": ni,
            "oci": oci, "iss": iss, "rep": rep, "div": div, "eq": rec.get("BHCK3210"), "buf": buf,
            "negemp": 1.0 if earn - div - rep < 0 else 0.0,
            "alt_top": rec.get("RSSD9364") in (None, 0.0) or int(rec["RSSD9364"]) == rid}


def winsor(values):
    xs = sorted(v for v in values if v is not None)
    lo, hi = xs[int(0.01 * (len(xs) - 1))], xs[int(math.ceil(0.99 * (len(xs) - 1)))]
    return lambda v: None if v is None else min(max(v, lo), hi)


def verdict_share(ours, theirs):
    tol = 0.5 if abs(theirs) < 5 else 0.10 * abs(theirs)
    return "reproduces" if abs(ours - theirs) <= tol else "differs"


def verdict_count(ours, theirs):
    return "reproduces" if abs(ours - theirs) <= 0.10 * theirs else "differs"


def main(cpi_path, rel_path, paths):
    print(f"{os.path.basename(cpi_path)} sha256 {sha(cpi_path)}")
    if rel_path != "-":
        print(f"{os.path.basename(rel_path)} sha256 {sha(rel_path)}")
    for p in paths:
        print(f"{os.path.basename(p)} sha256 {sha(p)}")
    cpi = cpi_annual(cpi_path)
    nic = controlled_offspring(rel_path) if rel_path != "-" else None
    sample, alt, agree, compared = [], 0, 0, 0
    top2020 = set()
    for p in paths:
        year, recs, has_holder = load(p)
        if not has_holder and (nic is None or year not in nic):
            sys.exit(f"{os.path.basename(p)}: no holder field and no NIC relationships for {year}")
        for rec in recs:
            nic_top = None if nic is None or year not in nic else int(rec["RSSD9001"]) not in nic[year]
            top = top_by_holder(rec) if has_holder else nic_top
            b = bank_year(rec, year, cpi, top)
            if has_holder and nic_top is not None and bank_year(rec, year, cpi, True):
                compared += 1
                agree += top == nic_top
            if b:
                sample.append(b)
                alt += has_holder and b["alt_top"]
                if year == 2020:
                    top2020.add(b["id"])
    years = sorted({b["year"] for b in sample})
    print(f"\nbank-years {len(sample)}, banks {len({b['id'] for b in sample})}, years {years[0]}-{years[-1]} ({len(years)})")
    print(f"reported: bank-years from files with holder fields also top-tier under RSSD9364 {alt}")
    if compared:
        share_agree = agree / compared
        print(f"control: RSSD9348 and the NIC rule agree on {agree}/{compared} = {100 * share_agree:.1f}% of 2015-2020 "
              f"bank-years passing the other rules -> {'at least 97%' if share_agree >= 0.97 else 'below 97%'}")
        late = [b for b in sample if b["year"] >= 2021]
        print(f"reported: 2021-2024 bank-years {len(late)}, of which top-tier in 2020 {sum(b['id'] in top2020 for b in late)}")

    print("\nR1 medians over years of aggregate component / aggregate adjusted prior equity (%)")
    for key, theirs in (("earn", 11.6), ("iss", 2.5), ("div", 4.7), ("rep", 4.2)):
        annual = []
        for y in years:
            bs = [b for b in sample if b["year"] == y]
            annual.append(100 * sum(b[key] for b in bs) / sum(b["prior"] for b in bs))
        ours = statistics.median(annual)
        print(f"  {key:5s} ours {ours:6.2f} theirs {theirs:5.1f} -> {verdict_share(ours, theirs)}")

    paid = sum(b["div"] > 0 for b in sample) / len(sample) * 100
    print(f"\nR2 bank-years with positive dividends: ours {paid:.1f}% theirs >85% -> {'reproduces' if paid > 85 else 'differs'}")
    issue = [100 * b["iss"] / b["prior"] for b in sample if b["iss"] > 0 and b["prior"] > 0]
    med = statistics.median(issue)
    print(f"R3 median issuance among issuers (% of prior equity): ours {med:.2f} theirs 0.9 -> {verdict_share(med, 0.9)} (n {len(issue)})")

    w_eqa = winsor([100 * b["eq"] / b["assets"] for b in sample if b["eq"] is not None])
    w_buf = winsor([b["buf"] for b in sample])
    w_lna = winsor([math.log(b["real"]) for b in sample])
    for b in sample:
        b["eqa"] = None if b["eq"] is None else w_eqa(100 * b["eq"] / b["assets"])
        b["bufw"] = w_buf(b["buf"])
        b["lna"] = w_lna(math.log(b["real"]))
    print(f"\nR4 Table 1")
    n = len(sample)
    print(f"  N ours {n} theirs 6448 -> {verdict_count(n, 6448)}")
    for label, vals, theirs in (
            ("equity/assets", [b["eqa"] for b in sample if b["eqa"] is not None], 9.10),
            ("negative EMP (%)", [100 * b["negemp"] for b in sample], 21.0),
            ("dividends/assets", [100 * b["div"] / b["assets"] for b in sample], 0.37),
            ("repurchases/assets", [100 * b["rep"] / b["assets"] for b in sample], 0.22),
            ("issuance/assets", [100 * b["iss"] / b["assets"] for b in sample], 0.20)):
        ours = sum(vals) / len(vals)
        print(f"  {label:20s} ours {ours:6.2f} theirs {theirs:5.2f} -> {verdict_share(ours, theirs)}")
    bufs = [b["bufw"] for b in sample if b["bufw"] is not None]
    print(f"  {'Tier 1 buffer':20s} ours {sum(bufs) / len(bufs):6.2f} theirs  6.74 -> {verdict_share(sum(bufs) / len(bufs), 6.74)}"
          f"; N ours {len(bufs)} theirs 6394 -> {verdict_count(len(bufs), 6394)}")

    by = {(b["id"], b["year"]): b for b in sample}
    reg = []
    for b in sample:
        l1, l2 = by.get((b["id"], b["year"] - 1)), by.get((b["id"], b["year"] - 2))
        if l1 and l2 and l1["eqa"] is not None and l1["bufw"] is not None:
            reg.append({"y": b["negemp"], "l1": l1["negemp"], "l2": l2["negemp"], "lna": l1["lna"],
                        "eqa": l1["eqa"] / 100, "buf": l1["bufw"], "id": b["id"], "year": b["year"]})
    print(f"\nR5 Table 2, coefficient on negative EMP at t-1 (their SE in brackets)")
    theirs = [(0.201, 0.060, 5148), (0.201, 0.060, 5148), (0.208, 0.029, 5148), (0.097, 0.029, 5091)]
    for col in range(4):
        data = reg
        if col == 3:
            counts = defaultdict(int)
            for r in reg:
                counts[r["id"]] += 1
            data = [r for r in reg if counts[r["id"]] > 1]
        X = np.array([[r["l1"], r["l2"], r["lna"], r["eqa"], r["buf"]] for r in data])
        if col >= 2:
            ys = sorted({r["year"] for r in data})[1:]
            X = np.column_stack([X] + [np.array([1.0 if r["year"] == y else 0.0 for r in data]) for y in ys])
        if col == 3:
            ids = sorted({r["id"] for r in data})[1:]
            X = np.column_stack([X] + [np.array([1.0 if r["id"] == i else 0.0 for r in data]) for i in ids])
        X = sm.add_constant(X, has_constant="add")
        yv = np.array([r["y"] for r in data])
        year_g = np.array([r["year"] for r in data])
        bank_g = np.array([r["id"] for r in data])
        groups = year_g if col == 0 else np.column_stack([year_g, bank_g])
        fit = sm.OLS(yv, X).fit(cov_type="cluster", cov_kwds={"groups": groups})
        coef, se, p = fit.params[1], fit.bse[1], fit.pvalues[1]
        tc, tse, tn = theirs[col]
        ok = coef > 0 and p < 0.05 and abs(coef - tc) <= 2 * tse
        print(f"  ({col + 1}) ours {coef:.3f} ({se:.4f}) p {p:.4f} N {len(data)}; theirs {tc:.3f} ({tse:.3f}) N {tn}"
              f" -> {'reproduces' if ok else 'differs'}; N {verdict_count(len(data), tn)}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3:])
