import csv
import gzip
import hashlib
import io
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
from fetch_common_data import FIELDS, QUARTER_ENDS, fetch_quarter, undefined_fields

YEAR = 2025


def main():
    missing = undefined_fields()
    if missing:
        sys.exit(f"fields not defined in the pinned FDIC workbook: {missing}")
    buf = io.StringIO()
    writer = csv.DictWriter(buf, fieldnames=FIELDS, extrasaction="ignore", lineterminator="\n")
    writer.writeheader()
    count = 0
    for mmdd in QUARTER_ENDS:
        rows = fetch_quarter(f"{YEAR}{mmdd}")
        outside = [r["REPDTE"] for r in rows if not f"{YEAR}0101" <= str(r["REPDTE"]) <= f"{YEAR}1231"]
        if outside:
            sys.exit(f"report dates outside {YEAR}: {sorted(set(outside))[:3]}")
        writer.writerows(sorted(rows, key=lambda r: int(r["CERT"])))
        count += len(rows)
        print(f"{YEAR}{mmdd}: {len(rows)} rows", flush=True)
    path = os.path.join(HERE, f"fdic_q_{YEAR}.csv.gz")
    with open(path, "wb") as raw:
        with gzip.GzipFile(fileobj=raw, mode="wb", mtime=0, filename="") as gz:
            gz.write(buf.getvalue().encode())
    digest = hashlib.sha256(open(path, "rb").read()).hexdigest()
    print(f"fdic_q_{YEAR}.csv.gz rows {count} bytes {os.path.getsize(path)} sha256 {digest}", flush=True)


if __name__ == "__main__":
    main()
