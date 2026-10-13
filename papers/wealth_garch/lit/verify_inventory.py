import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from litcheck import CACHE, normalise, page_text, table_rows

HERE = os.path.dirname(os.path.abspath(__file__))
rows = table_rows(open(os.path.join(HERE, "DATA_INVENTORY.md")).read(), "Quotations")
results = []


def found(src, layout, page, quote):
    return normalise(quote) in page_text(os.path.join(CACHE, src), int(page), layout == "yes")


def check(name, ok):
    results.append(ok)
    print(f"[{'PASS' if ok else 'FAIL'}] {name}")


check("I0 the quotation table parses and is non-empty", bool(rows) and all(len(r) == 5 for r in rows))
for qid, src, layout, page, quote in rows:
    check(f"{qid} found verbatim on PDF page {page} of {src}", found(src, layout, page, quote))
first = rows[0]
check("I-control a quotation with one word changed is not found",
      not found(first[1], first[2], first[3], first[4].replace("Compustat", "Bankscope", 1)))
check("I-control a true quotation on the wrong page is not found",
      not found(first[1], first[2], int(first[3]) + 5, first[4]))
summary = open(os.path.join(HERE, "DATA_INVENTORY.md")).read().split("## Quotations")[0]
cited = {c for c in summary.replace("(", " ").replace(")", " ").replace(",", " ").split() if c.startswith("DI-")}
check("I-refs every DI- id cited in the summary exists", cited <= {r[0] for r in rows})

nf = results.count(False)
print("=" * 72)
print(f"INVENTORY SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
