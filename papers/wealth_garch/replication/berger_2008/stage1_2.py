import csv
import io
import os
import sys
import zipfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import stage1

BANK_TYPES = {"NAT", "SMB", "NMB", "SSB", "SAL", "FSB", "CPB"}
SETS = {"I1": dict(lags=(2, 3)), "I2": dict(lags=(2, 4)), "I3": dict(lags=(2, 99), collapse=True),
        "I4": dict(lags=(2, 3), level=False)}


def bank_ids(paths):
    out = set()
    for p in paths:
        zf = zipfile.ZipFile(p)
        reader = csv.DictReader(io.TextIOWrapper(zf.open(zf.namelist()[0]), encoding="latin-1"))
        reader.fieldnames = [f.strip("#") for f in reader.fieldnames]
        out |= {int(r["ID_RSSD"]) for r in reader if r["ENTITY_TYPE"].strip() in BANK_TYPES}
    return out


def main(paths):
    for p in paths:
        print(f"{os.path.basename(p)} sha256 {stage1.sha(p)}")
    arg = {os.path.basename(p): p for p in paths}
    cpi = stage1.cpi_annual(arg["cpiaucsl_1989_2024.csv.gz"])
    dec = {y: stage1.read_y9c(arg[f"bhcf{y}12.csv.gz"], stage1.DEC_ITEMS) for y in range(1996, 2007)}
    quarters = {(y, q): stage1.read_y9c(arg[f"bhcf{y}{q}.csv.gz"], ["RSSD9001", "BHCK4300", "BHCK2170"])
                for y in range(1994, 2006) for q in ("03", "06", "09", "12")}
    link = stage1.links(arg["crsp_20240930v2.csv.gz"])
    up = stage1.parents(arg["CSV_RELATIONSHIPS.zip"])
    years = set(range(1997, 2007))
    banks = bank_ids([arg["CSV_ATTRIBUTES_ACTIVE.zip"], arg["CSV_ATTRIBUTES_CLOSED.zip"]])
    data = {"D1": stage1.build(dec, quarters, cpi, link, stage1.acquisitions(arg["CSV_TRANSFORMATIONS.zip"], up, years))[0],
            "D2": stage1.build(dec, quarters, cpi, link,
                               stage1.acquisitions(arg["CSV_TRANSFORMATIONS.zip"], up, years, banks), d2=True)[0]}
    for name, df in data.items():
        print(f"\n{name}: bank-years {len(df)}; means STDROA {df['STDROA'].mean():.4f} (annualised x4 {4 * df['STDROA'].mean():.4f}), "
              f"NMERGER {df['NMERGER'].mean():.3f}, RETAILDEP {df['RETAILDEP'].mean():.3f}, BUSINESSLOAN {df['BUSINESSLOAN'].mean():.3f}"
              "  [theirs .0034, .066, .605, .281]")
    summary = {}
    for dname, df in data.items():
        for iname, opts in SETS.items():
            print(f"\n######## {iname} x {dname}")
            summary[(iname, dname)] = {m: stage1.run(df, m, **opts) for m in stage1.MEASURES}
    print("\n######## SUMMARY: Step 1 speed, Step 2 mean speed, Step 3 mean target; targets holding of 6")
    close = 0
    for key, res in summary.items():
        cells = []
        near = 0
        for m, r in res.items():
            cells.append(f"{m} {r['lam1']:.3f}/{r['lam2']:.3f}/{r['tgt3']:.3f} ({sum(r['verdicts'])}/6)")
            near += abs(r["lam1"] - stage1.THEIRS[m][1]) <= 0.10
        close += near >= 2
        print(f"{key[0]} x {key[1]}: " + "; ".join(cells) + f"; Step 1 speed within 0.10 for {near} of 3")
    print(f"runs with the Step 1 speed within 0.10 of theirs for at least two measures: {close} of {len(summary)}")
    print(f"reading: the speed gap is due to {'our choices' if close >= 4 else 'the data'}")


if __name__ == "__main__":
    main(sys.argv[1:])
