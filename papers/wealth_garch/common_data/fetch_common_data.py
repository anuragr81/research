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
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "lit"))
from litcheck import CACHE, sheet_entries

API = "https://api.fdic.gov/banks/financials"
FIELDS = ["CERT", "REPDTE", "BKCLASS", "RSSDHCR",
          "ASSET", "LIAB", "EQ", "EQTOT", "DEP", "LNLSGR", "LNLSNET", "SC", "CHBAL", "FREPO", "FREPP", "INTAN",
          "NETINC", "ELNATR", "INTINC", "EINTEXP", "EDEPDOM", "EDEPFOR", "ILNDOM", "NTLNLS", "DRLNLS",
          "RBCT1J", "RBCT2", "RWAJT", "RBCRWAJ", "RBC1AAJ",
          "EQCDIV", "EQCDIVC", "EQCSXQ", "EQCSTKRX", "EQCTRSTX", "EQCBHCTR", "EQCMRG", "EQCPREV", "EQCREST"]
PAGE = 10000
QUARTER_ENDS = ("0331", "0630", "0930", "1231")
LAST_DATE = "20241231"


def undefined_fields():
    defined = sheet_entries(os.path.join(CACHE, "fdic_all_financial_reports.xlsx"), "Reference-Variables&Definitions")
    return [f for f in FIELDS if f not in defined]


def fetch_quarter(repdte):
    rows, offset = [], 0
    while True:
        query = urllib.parse.urlencode({"filters": f"REPDTE:{repdte}", "fields": ",".join(FIELDS),
                                        "limit": PAGE, "offset": offset, "format": "json"})
        with urllib.request.urlopen(f"{API}?{query}", timeout=300) as resp:
            batch = json.load(resp).get("data", [])
        rows.extend(item["data"] for item in batch)
        if len(batch) < PAGE:
            return rows
        offset += PAGE


def write_year(year):
    buf = io.StringIO()
    writer = csv.DictWriter(buf, fieldnames=FIELDS, extrasaction="ignore", lineterminator="\n")
    writer.writeheader()
    count = 0
    for mmdd in QUARTER_ENDS:
        rows = fetch_quarter(f"{year}{mmdd}")
        late = [r["REPDTE"] for r in rows if str(r["REPDTE"]) > LAST_DATE]
        if late:
            sys.exit(f"hold-out breached: report dates {sorted(set(late))[:3]}")
        writer.writerows(sorted(rows, key=lambda r: int(r["CERT"])))
        count += len(rows)
        print(f"{year}{mmdd}: {len(rows)} rows", flush=True)
    path = os.path.join(HERE, f"fdic_q_{year}.csv.gz")
    with open(path, "wb") as raw:
        with gzip.GzipFile(fileobj=raw, mode="wb", mtime=0, filename="") as gz:
            gz.write(buf.getvalue().encode())
    digest = hashlib.sha256(open(path, "rb").read()).hexdigest()
    print(f"fdic_q_{year}.csv.gz rows {count} bytes {os.path.getsize(path)} sha256 {digest}", flush=True)


def main(first_year, last_year):
    missing = undefined_fields()
    if missing:
        sys.exit(f"fields not defined in the pinned FDIC workbook: {missing}")
    if last_year > int(LAST_DATE[:4]):
        sys.exit("hold-out: no year after 2024")
    for year in range(first_year, last_year + 1):
        write_year(year)


if __name__ == "__main__":
    main(int(sys.argv[1]), int(sys.argv[2]))
