import csv
import gzip
import hashlib
import json
import sys
import urllib.parse
import urllib.request

API = "https://api.fdic.gov/banks/financials"
FRED = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=FEDFUNDS"
FIELDS = ["CERT", "REPDTE", "BKCLASS", "ASSET", "NETINC", "EQ", "ELNATR", "RWAJT", "RBCT1J", "RBCT2", "RBCRWAJ"]
PAGE = 10000


def fetch_year(repdte):
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


def main(panel_path, fred_path, first_year, last_year):
    total = 0
    with gzip.open(panel_path, "wt", newline="") as fh:
        writer = csv.DictWriter(fh, fieldnames=FIELDS, extrasaction="ignore")
        writer.writeheader()
        for year in range(first_year, last_year + 1):
            rows = sorted(fetch_year(f"{year}1231"), key=lambda r: int(r["CERT"]))
            writer.writerows(rows)
            total += len(rows)
            print(f"{year}1231: {len(rows)} rows", flush=True)
    with urllib.request.urlopen(FRED, timeout=120) as resp, open(fred_path, "wb") as out:
        out.write(resp.read())
    for path in (panel_path, fred_path):
        print(f"{path} sha256 {hashlib.sha256(open(path, 'rb').read()).hexdigest()}")
    print(f"rows {total}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], int(sys.argv[3]), int(sys.argv[4]))
