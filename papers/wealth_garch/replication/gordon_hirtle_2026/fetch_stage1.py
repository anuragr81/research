import gzip
import hashlib
import os
import sys
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120 Safari/537.36"
CHICAGO = "https://www.chicagofed.org/~/media/others/banking/financial-institution-reports/bhc-data/bhcf{yy}12.csv"
FRED = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL&cosd=1989-01-01&coed=2024-12-01"


def get(url, headers):
    req = urllib.request.Request(url, headers=headers)
    with urllib.request.urlopen(req, timeout=600) as resp:
        return resp.read()


def save(name, data):
    path = os.path.join(HERE, name)
    with open(path, "wb") as raw:
        with gzip.GzipFile(fileobj=raw, mode="wb", mtime=0, filename="") as gz:
            gz.write(data)
    print(f"{name} bytes {os.path.getsize(path)} sha256 {hashlib.sha256(open(path, 'rb').read()).hexdigest()}", flush=True)


def main(first_year, last_year):
    if last_year > 2020:
        sys.exit("2021-2024 come from the FFIEC by manual download")
    cpi = get(FRED, {})
    if any(line.startswith(b"2025") for line in cpi.splitlines()):
        sys.exit("hold-out breached in CPI")
    save("cpiaucsl_1989_2024.csv.gz", cpi)
    for year in range(first_year, last_year + 1):
        save(f"bhcf{year}12.csv.gz", get(CHICAGO.format(yy=f"{year % 100:02d}"), {"User-Agent": UA}))


if __name__ == "__main__":
    main(int(sys.argv[1]), int(sys.argv[2]))
