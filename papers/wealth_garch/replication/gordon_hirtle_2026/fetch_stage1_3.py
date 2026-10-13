import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from fetch_stage1 import CHICAGO, UA, get, save

GDPDEF = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=GDPDEF&cosd=1989-01-01&coed=2024-10-01"


def main():
    gdp = get(GDPDEF, {})
    if any(line.startswith(b"2025") for line in gdp.splitlines()):
        sys.exit("hold-out breached in GDPDEF")
    save("gdpdef_1989_2024.csv.gz", gdp)
    save("bhcf198912.csv.gz", get(CHICAGO.format(yy="89"), {"User-Agent": UA}))


if __name__ == "__main__":
    main()
