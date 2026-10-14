import csv
import gzip
import hashlib
import os
import sys

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
COMMON = ["CERT", "REPDTE", "BKCLASS", "ASSET", "LIAB", "DEP", "LNLSGR", "LNLSNET", "SC", "CHBAL", "FREPO", "FREPP", "INTAN"]
CLASSES = ("small", "large")
TABLE2 = {
    "small": {"w mean": (0.10, 0.13, 0.08), "w AR(1)": (0.21, 0.20, 0.14), "w s.d.": (0.17, 0.20, 0.10),
              "g mean": (0.81, 0.65, 0.90), "g AR(1)": (-0.01, -0.01, -0.01), "g s.d.": (3.71, 3.50, 3.48),
              "deposit rate": (-0.46, -0.34, -0.51), "loan spread": (1.83, 1.72, 1.87),
              "liquid spread": (0.81, 0.42, 0.92), "failure rate": (0.05, 0.17, 0.01)},
    "large": {"w mean": (0.31, 0.37, 0.22), "w AR(1)": (0.72, 0.70, 0.51), "w s.d.": (0.24, 0.28, 0.14),
              "g mean": (1.63, 1.60, 1.64), "g AR(1)": (0.03, 0.03, 0.03), "g s.d.": (5.64, 5.31, 5.41),
              "deposit rate": (-0.55, -0.48, -0.58), "loan spread": (1.84, 1.68, 1.89),
              "liquid spread": (0.96, 0.56, 1.07), "failure rate": (0.08, 0.18, 0.01)}}
TABLE5 = {"large": {"failure rate": 0.084, "loans/assets": 0.665, "deposits/assets": 0.633, "equity/assets": 0.072,
                    "profit/equity": 0.063, "dividends/equity": 0.028, "sd loans/assets": 0.076, "sd deposits/assets": 0.086,
                    "sd equity/assets": 0.013, "sd profit/equity": 0.048, "sd dividends/equity": 0.034},
          "small": {"failure rate": 0.050, "loans/assets": 0.622, "deposits/assets": 0.857, "equity/assets": 0.099,
                    "profit/equity": 0.037, "dividends/equity": 0.013, "sd loans/assets": 0.082, "sd deposits/assets": 0.035,
                    "sd equity/assets": 0.014, "sd profit/equity": 0.024, "sd dividends/equity": 0.016}}
TABLE1 = {"small": {"banks": 6528, "cash": 9, "securities": 21, "loans": 62, "deposits": 85, "transaction deposits": 22,
                    "wholesale funding": 5, "tangible equity": 10},
          "large": {"banks": 137, "cash": 7, "securities": 20, "loans": 61, "deposits": 68, "transaction deposits": 7,
                    "wholesale funding": 22, "tangible equity": 10}}


def sha(path):
    return hashlib.sha256(open(path, "rb").read()).hexdigest()


def quarter(repdte):
    s = str(repdte)
    return int(s[:4]) * 4 + (int(s[4:6]) - 1) // 3


def load(common_paths, supp_paths):
    frames = []
    for p in common_paths:
        df = pd.read_csv(p, usecols=COMMON, dtype={"BKCLASS": str})
        frames.append(df[df["BKCLASS"].isin(["N", "NM", "SM"]) & (df["REPDTE"] >= 19891231) & (df["REPDTE"] <= 20101231)])
    base = pd.concat(frames)
    supp = pd.concat([pd.read_csv(p) for p in supp_paths])
    df = base.merge(supp, on=["CERT", "REPDTE"], how="left")
    df["q"] = df["REPDTE"].map(quarter)
    return df.sort_values(["CERT", "q"]).reset_index(drop=True)


def recession_quarters(path):
    rec = pd.read_csv(path)
    qs = rec["observation_date"].map(lambda d: int(d[:4]) * 4 + (int(d[5:7]) - 1) // 3)
    on = set(qs[rec["USRECQ"] == 1])
    out = set(on)
    for q in on:
        if q - 1 not in on:
            out |= {q - 1, q - 2}
        if q + 1 not in on:
            out |= {q + k for k in range(1, 7)}
    return out


def cpi_quarterly(path):
    cpi = pd.read_csv(path)
    cpi["q"] = cpi["observation_date"].map(lambda d: int(d[:4]) * 4 + (int(d[5:7]) - 1) // 3)
    return cpi.groupby("q")["CPIAUCSL"].mean()


def winsor(s):
    lo, hi = s.quantile(0.01), s.quantile(0.99)
    return s.clip(lo, hi)


def lag(df, col):
    prev = df.groupby("CERT")[col].shift(1)
    same = df.groupby("CERT")["q"].shift(1) == df["q"] - 1
    return prev.where(same)


def ar1(x, q):
    x, q = np.asarray(x, dtype=float), np.asarray(q)
    a, b = x[:-1], x[1:]
    ok = ~np.isnan(a) & ~np.isnan(b) & (q[1:] - q[:-1] == 1)
    if ok.sum() < 3 or np.std(a[ok]) == 0:
        return np.nan
    return float(np.corrcoef(a[ok], b[ok])[0, 1])


def close(ours, theirs, rel, floor, absol):
    return abs(ours - theirs) <= (absol if abs(theirs) < floor else rel * abs(theirs))


def main(args):
    common = [a for a in args if os.path.basename(a).startswith("fdic_q_")]
    supp = [a for a in args if os.path.basename(a).startswith("fdic_mmp_supplement_")]
    named = {os.path.basename(a): a for a in args}
    for a in args:
        print(f"{os.path.basename(a)} sha256 {sha(a)}")
    df = load(common, supp)
    rec = recession_quarters(named["usrecq_1989_2011.csv"])
    cpi = cpi_quarterly(named["cpiaucsl_1989_2024.csv.gz"])
    infl = 100 * np.log(cpi / cpi.shift(1))
    pct = df.groupby("q")["ASSET"].transform(lambda s: s.rank(pct=True))
    df["size"] = np.where(pct > 0.98, "large", np.where(pct >= 0.95, "medium", "small"))
    window = (df["q"] >= 1990 * 4) & (df["q"] <= 2010 * 4 + 3)
    print(f"\ninitial: bank-quarters {int(window.sum())}, banks {df.loc[window, 'CERT'].nunique()} [890,252; 17,226]")
    df["TA"] = df["ASSET"] - df["INTAN"].fillna(0)
    df["TE"] = df["ASSET"] - df["LIAB"] - df["INTAN"].fillna(0)
    df["gL"] = df["LNLSNET"] / lag(df, "LNLSNET") - 1
    df["gA"] = df["TA"] / lag(df, "TA") - 1
    keep = window & df["gL"].notna() & df["gA"].notna() & np.isfinite(df["gL"]) & np.isfinite(df["gA"])
    for g in ("gL", "gA"):
        lo = df.groupby("q")[g].transform(lambda s: s.quantile(0.01))
        hi = df.groupby("q")[g].transform(lambda s: s.quantile(0.99))
        keep &= (df[g] >= lo) & (df[g] <= hi)
    print(f"final: bank-quarters {int(keep.sum())}, banks {df.loc[keep, 'CERT'].nunique()} [799,437; 16,564]")
    df["rec"] = df["q"].isin(rec)
    df["infl"] = df["q"].map(infl)
    df["w"] = 100 * df["DRLNLSQ"] / (lag(df, "LNLSGR") + lag(df, "DRLNLSQ"))
    df["g"] = 100 * (np.log(df["DEP"]) - np.log(lag(df, "DEP"))) - df["infl"]
    dep_rate = 100 * (df["EDEPDOMQ"].fillna(0) + df["EDEPFORQ"].fillna(0)) / lag(df, "DEP")
    loan_ret = 100 * (df["ILNDOMQ"].fillna(0) + df["ILNFORQ"].fillna(0)) / lag(df, "LNLSNET")
    liq_ret = 100 * (df["IFREPOQ"].fillna(0) + df["IGLSECQ"].fillna(0)) / (lag(df, "CHBAL") + lag(df, "FREPO") + lag(df, "SC"))
    df["deposit rate"] = dep_rate - df["infl"]
    df["loan spread"] = loan_ret - dep_rate
    df["liquid spread"] = liq_ret - dep_rate
    df["loans/assets"] = df["LNLSNET"] / df["TA"]
    df["deposits/assets"] = df["DEP"] / df["TA"]
    df["equity/assets"] = df["TE"] / df["TA"]
    df["profit/equity"] = (df["NETINCQ"] + df["ITAXQ"].fillna(0) - df["EXTRAQ"].fillna(0)) / df["TE"]
    df["dividends/equity"] = df["EQCDIVQ"].fillna(0) / df["TE"]

    fails = pd.read_csv(named["fdic_failures_1990_2010.csv.gz"])
    fails["fq"] = fails["FAILDATE"].map(lambda d: int(str(d)[-4:]) * 4 + (int(str(d).split("/")[0]) - 1) // 3)
    lastq = df[window].groupby("CERT")["q"].max()
    sizeq = df.set_index(["CERT", "q"])["size"]
    matched = []
    for _, f in fails.iterrows():
        if f["CERT"] in lastq.index and lastq[f["CERT"]] <= f["fq"]:
            matched.append((f["fq"], sizeq[(f["CERT"], lastq[f["CERT"]])]))
    print(f"failure events {len(fails)} [1,292], matched to a report {len(matched)} [810]")
    banks_q = df[window].groupby(["q", "size"])["CERT"].nunique()
    events = pd.Series([1] * len(matched), index=pd.MultiIndex.from_tuples(matched, names=["q", "size"])).groupby(level=[0, 1]).sum()
    frate = {}
    for c in CLASSES:
        qs = sorted(q for q in range(1990 * 4, 2010 * 4 + 4))
        r = pd.Series({q: 100 * events.get((q, c), 0) / banks_q.get((q - 1, c), np.nan) for q in qs})
        frate[c] = (r.mean(), r[[q for q in qs if q in rec]].mean(), r[[q for q in qs if q not in rec]].mean())

    s = df[keep & (df["TE"] > 0)].copy()
    for col in ["w", "g", "loans/assets", "deposits/assets", "equity/assets", "profit/equity", "dividends/equity"]:
        s[col] = s.groupby(["q", "size"])[col].transform(winsor)

    t2_hold = t2_n = t5_hold = t5_n = 0
    for c in CLASSES:
        sc = s[s["size"] == c]
        counts = sc.groupby("CERT")["rec"].agg(["sum", "count"])
        ok35 = counts.index[(counts["sum"] >= 35) & (counts["count"] - counts["sum"] >= 35)]
        stats = {k: ([], [], []) for k in TABLE2[c]}
        for cert, b in sc[sc["CERT"].isin(ok35)].groupby("CERT"):
            g = b["g"] - b.groupby(b["q"] % 4)["g"].transform("mean") + b["g"].mean()
            for i, part in enumerate((slice(None), b["rec"].values, ~b["rec"].values)):
                bb, gg = b[part] if i else b, g[part] if i else g
                stats["w mean"][i].append(bb["w"].mean())
                stats["w s.d."][i].append(bb["w"].std())
                stats["w AR(1)"][i].append(ar1(bb["w"], bb["q"]))
                stats["g mean"][i].append(gg.mean())
                stats["g s.d."][i].append(gg.std())
                stats["g AR(1)"][i].append(ar1(gg, bb["q"]))
                for k in ("deposit rate", "loan spread", "liquid spread"):
                    stats[k][i].append(bb[k].mean())
        print(f"\n===== {c}: bank-quarters {len(sc)}, banks {sc['CERT'].nunique()}, banks with 35+35 quarters {len(ok35)}")
        print("Table 2 (ours / theirs; uncond, recession, expansion)")
        for k, theirs in TABLE2[c].items():
            ours = frate[c] if k == "failure rate" else tuple(float(winsor(pd.Series(v).dropna()).mean()) for v in stats[k])
            verdicts = [close(o, t, 0.20, 0.25, 0.05) for o, t in zip(ours, theirs)]
            t2_hold += sum(verdicts)
            t2_n += 3
            print(f"  {k:14s} " + "  ".join(f"{o:7.2f}/{t:5.2f} {'holds' if v else 'differs'}" for o, t, v in zip(ours, theirs, verdicts)))
        ok20 = counts.index[(counts["sum"] >= 20) & (counts["count"] - counts["sum"] >= 20)]
        sb = sc[sc["CERT"].isin(ok20)].groupby("CERT")
        print(f"Table 5 data moments (banks with 20+20 quarters: {len(ok20)})")
        for k, theirs in TABLE5[c].items():
            if k == "failure rate":
                ours = frate[c][0]
            elif k.startswith("sd "):
                ours = float(sb[k[3:]].std().mean())
            else:
                ours = float(sb[k].mean().mean())
            v = close(ours, theirs, 0.10, 0.05, 0.005)
            t5_hold += v
            t5_n += 1
            print(f"  {k:20s} {ours:7.3f} [{theirs}] -> {'holds' if v else 'differs'}")

    t1_hold = t1_n = 0
    d = df[df["REPDTE"] == 20101231].copy()
    print("\n===== Table 1, 2010Q4 (mean of bank shares of tangible assets, %; theirs in brackets)")
    for c in CLASSES:
        dc = d[d["size"] == c]
        shares = {"cash": dc["CHBAL"] / dc["TA"], "securities": dc["SC"] / dc["TA"], "loans": dc["LNLSNET"] / dc["TA"],
                  "deposits": dc["DEP"] / dc["TA"], "transaction deposits": dc["TRN"] / dc["TA"],
                  "wholesale funding": (dc["LIAB"] - dc["DEP"]) / dc["TA"],
                  "tangible equity": dc["TE"] / dc["TA"]}
        n_ok = abs(len(dc) - TABLE1[c]["banks"]) <= 0.10 * TABLE1[c]["banks"]
        t1_hold += n_ok
        t1_n += 1
        print(f"  {c}: banks {len(dc)} [{TABLE1[c]['banks']}] -> {'holds' if n_ok else 'differs'}")
        for k, v in shares.items():
            ours = 100 * float(v.mean())
            ok = abs(ours - TABLE1[c][k]) <= 2
            t1_hold += ok
            t1_n += 1
            print(f"    {k:22s} {ours:6.1f} [{TABLE1[c][k]}] -> {'holds' if ok else 'differs'}")
    print(f"\nSUMMARY: Table 1 {t1_hold}/{t1_n}, Table 2 {t2_hold}/{t2_n}, Table 5 {t5_hold}/{t5_n} hold")


if __name__ == "__main__":
    main(sys.argv[1:])
