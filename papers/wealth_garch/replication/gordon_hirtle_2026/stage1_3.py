import copy
import csv
import gzip
import io
import itertools
import os
import sys
import zipfile
from collections import Counter, defaultdict

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from stage1 import bank_year, controlled_offspring, cpi_annual, load, report, sha, top_by_holder

KEEP = {"BHC", "FHD"}


def gdp_annual(path):
    by_year = defaultdict(list)
    for r in csv.DictReader(gzip.open(path, "rt")):
        by_year[int(r["observation_date"][:4])].append(float(r["GDPDEF"]))
    return {y: sum(v) / len(v) for y, v in by_year.items() if len(v) == 4}


def attributes(paths):
    out = {}
    for p in paths:
        zf = zipfile.ZipFile(p)
        reader = csv.DictReader(io.TextIOWrapper(zf.open(zf.namelist()[0]), encoding="latin-1"))
        reader.fieldnames = [f.strip("#") for f in reader.fieldnames]
        for r in reader:
            out[int(r["ID_RSSD"])] = (r["ENTITY_TYPE"].strip(), r["DT_START"].strip(), r["DT_END"].strip())
    return out


def main(cpi_path, gdp_path, rel_path, attr_paths, prior_path, paths):
    for p in [cpi_path, gdp_path, rel_path] + attr_paths + [prior_path] + paths:
        print(f"{os.path.basename(p)} sha256 {sha(p)}")
    cpi, gdp = cpi_annual(cpi_path), gdp_annual(gdp_path)
    nic = controlled_offspring(rel_path)
    attr = attributes(attr_paths)
    filers = {}
    year, recs, _ = load(prior_path)
    filers[year] = {int(r["RSSD9001"]) for r in recs if r.get("BHCK2170") is not None}
    last_type, base, agree, compared = {}, [], 0, 0
    source = Counter()
    for p in paths:
        year, recs, has_holder = load(p)
        filers[year] = {int(r["RSSD9001"]) for r in recs if r.get("BHCK2170") is not None}
        end = f"{year}1231"
        for rec in recs:
            rid = int(rec["RSSD9001"])
            top = top_by_holder(rec) if has_holder else rid not in nic[year]
            if "RSSD9346" in rec:
                etype, how = rec["RSSD9346"], "RSSD9346"
                last_type[rid] = etype
            elif rid in attr and attr[rid][1] <= end < attr[rid][2]:
                etype, how = attr[rid][0], "attributes"
            elif rid in last_type:
                etype, how = last_type[rid], "carried RSSD9346"
            else:
                etype, how = None, "unknown"
            if year == 2020 and rid in attr and attr[rid][1] <= end < attr[rid][2] and bank_year(rec, year, cpi, top):
                compared += 1
                agree += (attr[rid][0] in KEEP) == (etype in KEEP)
            for deflator, prices in (("cpi", cpi), ("gdp", gdp)):
                b = bank_year(rec, year, prices, top)
                if b:
                    b.update({"etype": etype, "how": how, "deflator": deflator})
                    base.append(b)
    for b in base:
        b["continuous"] = b["id"] in filers.get(b["year"] - 1, set())
        if b["deflator"] == "cpi" and b["year"] >= 2021:
            source[b["how"]] += 1

    cpi_base = [b for b in base if b["deflator"] == "cpi"]
    print(f"\ncontrol: 2020 keep-or-drop by attributes against RSSD9346: {agree}/{compared} agree")
    print(f"2021-2024 entity type read from: {dict(sorted(source.items()))}")
    dropped = Counter((b["etype"], b["year"]) for b in cpi_base if b["etype"] is not None and b["etype"] not in KEEP)
    by_type = Counter()
    for (t, _), n in dropped.items():
        by_type[t] += n
    print(f"C1 drops by entity type: {dict(sorted(by_type.items()))}")
    print("C1 drops by year: " + " ".join(f"{y}:{sum(n for (t, yy), n in dropped.items() if yy == y)}"
                                          for y in sorted({y for _, y in dropped})))

    years = sorted({b["year"] for b in base})
    for combo in [()] + [c for k in (1, 2, 3) for c in itertools.combinations(("C1", "C2", "C3"), k)]:
        sample = [copy.copy(b) for b in base
                  if b["deflator"] == ("gdp" if "C2" in combo else "cpi")
                  and ("C1" not in combo or b["etype"] is None or b["etype"] in KEEP)
                  and ("C3" not in combo or b["continuous"])]
        name = "+".join(combo) if combo else "stage 1 sample"
        counts = Counter(b["year"] for b in sample)
        print(f"\n===== {name}: bank-years {len(sample)}, banks {len({b['id'] for b in sample})}")
        print("by year: " + " ".join(f"{y}:{counts[y]}" for y in years))
        report(sample)


if __name__ == "__main__":
    a = sys.argv[1:]
    main(a[0], a[1], a[2], a[3:5], a[5], a[6:])
