import csv
import gzip
import math
import os
import statistics
import sys
from collections import Counter, defaultdict

import numpy as np
import statsmodels.api as sm

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from stage1 import cpi_annual, requirement, sha, winsor

COMMERCIAL = {"N", "NM", "SM"}


def num(v):
    return float(v) if v not in ("", None) else None


def load_december(paths, cpi):
    out = []
    for p in paths:
        decs = [r for r in csv.DictReader(gzip.open(p, "rt")) if r["REPDTE"].endswith("1231")]
        holders = Counter(r["RSSDHCR"] for r in decs if r["RSSDHCR"] not in ("", "0"))
        for r in decs:
            if r["BKCLASS"] not in COMMERCIAL:
                continue
            year = int(r["REPDTE"][:4])
            assets, ni, prev = num(r["ASSET"]), num(r["NETINC"]), num(r["EQCPREV"])
            if assets is None or ni is None or prev is None or assets <= 0:
                continue
            hc = r["RSSDHCR"]
            group = "no holding company" if hc in ("", "0") else ("only bank of its holding company" if holders[hc] == 1 else "multibank subsidiary")
            stk = num(r["EQCSTKRX"]) or 0.0
            divc = num(r["EQCDIVC"])
            rwa, t1 = num(r["RWAJT"]), num(r["RBCT1J"])
            out.append({"id": int(r["CERT"]), "year": year, "group": group, "assets": assets,
                        "real": assets * cpi[2024] / cpi[year], "ni": ni, "div": divc or 0.0, "div_missing": divc is None,
                        "rep": max(-stk, 0.0), "iss": max(stk, 0.0), "eq": num(r["EQ"]),
                        "prior": prev + (num(r["EQCREST"]) or 0.0) + (num(r["EQCMRG"]) or 0.0),
                        "buf": 100 * t1 / rwa - requirement(year) if rwa and rwa > 0 and t1 is not None else None})
    for b in out:
        b["negemp"] = 1.0 if b["ni"] - b["div"] - b["rep"] < 0 else 0.0
    return out


def prepare(sample):
    by = {(b["id"], b["year"]): b for b in sample}
    for b in sample:
        prev = by.get((b["id"], b["year"] - 1))
        b["dni_raw"] = (b["ni"] - prev["ni"]) / b["assets"] if prev else None
    w = {k: winsor([f(b) for b in sample]) for k, f in (
        ("lna", lambda b: math.log(b["real"])),
        ("eqa", lambda b: None if b["eq"] is None else b["eq"] / b["assets"]),
        ("buf", lambda b: b["buf"]),
        ("dni", lambda b: b["dni_raw"]))}
    for b in sample:
        b["lna_w"] = w["lna"](math.log(b["real"]))
        b["eqa_w"] = None if b["eq"] is None else w["eqa"](b["eq"] / b["assets"])
        b["buf_w"] = w["buf"](b["buf"])
        b["dni_w"] = w["dni"](b["dni_raw"])
    return by


def absorb(M, years, banks, bank_fe):
    M = M.copy()
    yi = np.unique(years, return_inverse=True)[1]
    bi = np.unique(banks, return_inverse=True)[1]
    for _ in range(10000):
        before = M.copy()
        for c in range(M.shape[1]):
            M[:, c] -= (np.bincount(yi, weights=M[:, c]) / np.bincount(yi))[yi]
            if bank_fe:
                M[:, c] -= (np.bincount(bi, weights=M[:, c]) / np.bincount(bi))[bi]
        if not bank_fe or np.abs(M - before).max() < 1e-10:
            return M
    sys.exit("fixed effects did not converge")


def regress(rows, xkeys, bank_fe):
    if bank_fe:
        counts = Counter(r["id"] for r in rows)
        rows = [r for r in rows if counts[r["id"]] > 1]
    years = np.array([r["year"] for r in rows])
    banks = np.array([r["id"] for r in rows])
    M = absorb(np.array([[r["y"]] + [r[k] for k in xkeys] for r in rows], dtype=float), years, banks, bank_fe)
    fit = sm.OLS(M[:, 0], M[:, 1:]).fit(cov_type="cluster", cov_kwds={"groups": np.column_stack([years, banks])})
    return fit, len(rows)


def tests(sample, by, verdicts):
    t2 = []
    for b in sample:
        l1, l2 = by.get((b["id"], b["year"] - 1)), by.get((b["id"], b["year"] - 2))
        if l1 and l2 and l1["eqa_w"] is not None and l1["buf_w"] is not None:
            t2.append({"y": b["negemp"], "l1": l1["negemp"], "l2": l2["negemp"], "lna": l1["lna_w"], "eqa": l1["eqa_w"],
                       "buf": l1["buf_w"], "id": b["id"], "year": b["year"]})
    t3 = []
    for b in sample:
        l1 = by.get((b["id"], b["year"] - 1))
        if l1 and b["dni_w"] is not None and l1["eqa_w"] is not None and l1["buf_w"] is not None:
            t3.append({"y": b["negemp"], "dni": b["dni_w"], "lna": l1["lna_w"], "eqa": l1["eqa_w"], "buf": l1["buf_w"],
                       "id": b["id"], "year": b["year"]})
    n = len(sample)
    paid = 100 * sum(b["div"] > 0 for b in sample) / n
    neg = 100 * sum(b["negemp"] for b in sample) / n
    word = lambda ok: ("holds" if ok else "fails") if verdicts else "reported"
    print(f"  H1 dividends positive: {paid:.1f}% (theirs >85%) -> {word(paid > 85)}")
    print(f"  H2 payouts above earnings: {neg:.1f}% (theirs >20%; 21% mean) -> {word(neg > 20)}")
    res = {}
    for col, fe in ((3, False), (4, True)):
        fit, m = regress(t2, ["l1", "l2", "lna", "eqa", "buf"], fe)
        res[col] = (fit.params[0], fit.pvalues[0], fit.params[3], fit.pvalues[3], m, fit.bse[0], fit.bse[3])
    theirs = {3: (0.208, 0.910), 4: (0.097, 2.331)}
    for col in (3, 4):
        c, p, e, pe, m, se, see = res[col]
        print(f"  Table 2 col ({col}) N {m}: lag {c:.3f} ({se:.3f}) p {p:.4f} [theirs {theirs[col][0]:.3f}]; "
              f"equity/assets {e:.3f} ({see:.3f}) p {pe:.4f} [theirs {theirs[col][1]:.3f}]")
    print(f"  H3 persistence -> {word(all(res[c][0] > 0 and res[c][1] < 0.05 for c in (3, 4)))}")
    print(f"  H4 capital management -> {word(all(res[c][2] > 0 and res[c][3] < 0.05 for c in (3, 4)))}")
    fit, m = regress(t3, ["dni", "lna", "eqa", "buf"], True)
    print(f"  Table 3 col (3) N {m}: change in net income/assets {fit.params[0]:.3f} ({fit.bse[0]:.3f}) p {fit.pvalues[0]:.4f} [theirs -13.620]")
    print(f"  H5 earnings shock -> {word(fit.params[0] < 0 and fit.pvalues[0] < 0.05)}")


def describe(sample):
    years = sorted({b["year"] for b in sample})
    parts = []
    for key, theirs in (("ni", 11.6), ("iss", 2.5), ("div", 4.7), ("rep", 4.2)):
        annual = []
        for y in years:
            bs = [b for b in sample if b["year"] == y]
            annual.append(100 * sum(b[key] for b in bs) / sum(b["prior"] for b in bs))
        parts.append(f"{key} {statistics.median(annual):.2f} [{theirs}]")
    issue = [100 * b["iss"] / b["prior"] for b in sample if b["iss"] > 0 and b["prior"] > 0]
    print("  typical-year components, % of prior equity: " + ", ".join(parts))
    print(f"  median net issue among issuers {statistics.median(issue):.2f}% [0.9] (n {len(issue)}); "
          f"dividends missing {sum(b['div_missing'] for b in sample)}")


def main(cpi_path, paths):
    print(f"{os.path.basename(cpi_path)} sha256 {sha(cpi_path)}")
    for p in paths:
        print(f"{os.path.basename(p)} sha256 {sha(p)}")
    cpi = cpi_annual(cpi_path)
    allb = load_december(paths, cpi)
    for name, base, primary in (("P", [b for b in allb if b["real"] > 3_000_000], True), ("A", [dict(b) for b in allb], False)):
        by = prepare(base)
        groups = Counter(b["group"] for b in base)
        print(f"\n===== sample {name}: bank-years {len(base)}, banks {len({b['id'] for b in base})}; "
              + ", ".join(f"{g} {groups[g]}" for g in sorted(groups)))
        describe(base)
        tests(base, by, primary)
        for g in sorted(groups):
            sub = [b for b in base if b["group"] == g]
            print(f"\n----- {name}, {g}: bank-years {len(sub)}, banks {len({b['id'] for b in sub})}")
            tests(sub, {(b["id"], b["year"]): b for b in sub}, False)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2:])
