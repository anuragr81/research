import csv
import gzip
import hashlib
import io
import json
import os
import sys
import urllib.parse
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
sys.path.insert(0, os.path.join(ROOT, "lit"))
from litcheck import CACHE, sheet_entries

FIN = "https://api.fdic.gov/banks/financials"
FAIL = "https://api.fdic.gov/banks/failures"
FRED = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=USRECQ&cosd=1989-01-01&coed=2011-10-01"
FIELDS = ["CERT", "REPDTE", "NETINCQ", "ITAXQ", "EXTRAQ", "DRLNLSQ", "ILNDOMQ", "ILNFORQ", "IFREPOQ", "IGLSECQ",
          "EDEPDOMQ", "EDEPFORQ", "EQCDIVQ", "LNRE", "LNCI", "LNCON", "LNAG", "TRN", "NTR"]
FAIL_FIELDS = ["CERT", "FAILDATE", "RESTYPE", "RESTYPE1", "NAME"]
PAGE = 10000


def get_json(url, params):
    with urllib.request.urlopen(f"{url}?{urllib.parse.urlencode(params)}", timeout=300) as resp:
        return json.load(resp)


def pages(url, params):
    rows, offset = [], 0
    while True:
        batch = get_json(url, dict(params, limit=PAGE, offset=offset, format="json")).get("data", [])
        rows.extend(item["data"] for item in batch)
        if len(batch) < PAGE:
            return rows
        offset += PAGE


def save(name, header, rows):
    buf = io.StringIO()
    writer = csv.DictWriter(buf, fieldnames=header, extrasaction="ignore", lineterminator="\n")
    writer.writeheader()
    writer.writerows(rows)
    path = os.path.join(HERE, name)
    with open(path, "wb") as raw:
        with gzip.GzipFile(fileobj=raw, mode="wb", mtime=0, filename="") as gz:
            gz.write(buf.getvalue().encode())
    print(f"{name} rows {len(rows)} sha256 {hashlib.sha256(open(path, 'rb').read()).hexdigest()}", flush=True)


def main():
    defined = sheet_entries(os.path.join(CACHE, "fdic_all_financial_reports.xlsx"), "Reference-Variables&Definitions")
    missing = [f for f in FIELDS if f not in defined]
    if missing:
        sys.exit(f"undefined fields {missing}")
    with urllib.request.urlopen(FRED, timeout=120) as resp:
        fred = resp.read()
    if any(line.startswith(b"2025") for line in fred.splitlines()):
        sys.exit("hold-out breached")
    with open(os.path.join(HERE, "usrecq_1989_2011.csv"), "wb") as out:
        out.write(fred)
    print(f"usrecq_1989_2011.csv sha256 {hashlib.sha256(fred).hexdigest()}", flush=True)
    fails = [r for r in pages(FAIL, {"filters": "FAILYR:[1990 TO 2010]", "fields": ",".join(FAIL_FIELDS)})]
    save("fdic_failures_1990_2010.csv.gz", FAIL_FIELDS, sorted(fails, key=lambda r: (str(r["FAILDATE"]), int(r["CERT"] or 0))))
    for year in range(1989, 2011):
        rows = []
        for mmdd in (("1231",) if year == 1989 else ("0331", "0630", "0930", "1231")):
            got = pages(FIN, {"filters": f"REPDTE:{year}{mmdd}", "fields": ",".join(FIELDS)})
            rows.extend(sorted(got, key=lambda r: int(r["CERT"])))
        save(f"fdic_mmp_supplement_{year}.csv.gz", FIELDS, rows)


if __name__ == "__main__":
    main()
