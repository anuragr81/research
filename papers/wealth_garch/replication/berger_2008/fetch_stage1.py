import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "gordon_hirtle_2026"))
import fetch_stage1 as gh

QUARTER = "https://www.chicagofed.org/~/media/others/banking/financial-institution-reports/bhc-data/bhcf{yy}{mm}.csv"
LINK = "https://www.newyorkfed.org/medialibrary/media/research/banking_research/data/crsp_20240930v2.csv"


def save(name, data):
    gh.HERE = HERE
    gh.save(name, data)


def main():
    save("crsp_20240930v2.csv.gz", gh.get(LINK, {"User-Agent": gh.UA}))
    for year in range(1994, 2007):
        for mm in ("03", "06", "09"):
            save(f"bhcf{year}{mm}.csv.gz", gh.get(QUARTER.format(yy=f"{year % 100:02d}", mm=mm), {"User-Agent": gh.UA}))


if __name__ == "__main__":
    main()
