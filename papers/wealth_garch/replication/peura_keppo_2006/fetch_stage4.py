import csv
import gzip
import hashlib
import json
import sys
import urllib.parse
import urllib.request

API = "https://api.fdic.gov/banks/financials"
FIELDS = ["CERT", "REPDTE", "RBCT2"]
PAGE = 10000
QUARTER_ENDS = ("0331", "0630", "0930", "1231")


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


def main(out_path, first_year, last_year):
    total = 0
    with gzip.open(out_path, "wt", newline="") as fh:
        writer = csv.DictWriter(fh, fieldnames=FIELDS, extrasaction="ignore")
        writer.writeheader()
        for year in range(first_year, last_year + 1):
            for mmdd in QUARTER_ENDS:
                rows = sorted(fetch_quarter(f"{year}{mmdd}"), key=lambda r: int(r["CERT"]))
                writer.writerows(rows)
                total += len(rows)
    print(f"rows {total}; sha256 {hashlib.sha256(open(out_path, 'rb').read()).hexdigest()}")


if __name__ == "__main__":
    main(sys.argv[1], int(sys.argv[2]), int(sys.argv[3]))
