import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "clark_oswald_1996.pdf"
TXT = CACHE / "clark_oswald_1996.txt"
PDF_SHA = "ff666a87c0b6a799a42af02fcd628259713d81b55443ed3343ab81582dec3814"
FIRST, LAST = 1, 27
CONTROLS = ["control_ratio_form_needs_equal_and_opposite"]

s = Suite("CLARK-OSWALD-1996 (authors' revision, Aug 1995; quotations; lean/Lit/ClarkOswald1996.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("CO-K KEYS lists ClarkOswald1996", "ClarkOswald1996" in (HERE / "KEYS").read_text().split())
s.check("CO-K LEAN names lean/Lit/ClarkOswald1996.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/ClarkOswald1996.lean")
rows = quotation_rows(claims, "CO")
s.check("CO-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 15, f"{len(rows)} rows")
s.check("CO-Q1 every page lies in pp.1-27", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

if not (PDF.exists() and TXT.exists()):
    s.check("CO-S0 the cached PDF and text are available", False,
            f"download jpub.pdf to {PDF} and run pdftotext -layout to {TXT}")
    sys.exit(s.finish())
s.check("CO-S1 the cached PDF matches its pinned sha256", sha256(PDF) == PDF_SHA)
parts = TXT.read_text().split("\f")
blocks = {p: parts[p] for p in range(FIRST, LAST + 1)}
s.check("CO-S2 the title page is Satisfaction and Comparison Income, revised August 1995, by Clark and Oswald",
        "satisfaction and comparison income" in norm(parts[0]) and "revised august 1995" in norm(parts[0])
        and "clark" in norm(parts[0]) and "oswald" in norm(parts[0]))
s.check("CO-S3 pages 1-26 end with their number and page 27 is Table 4",
        all([l.strip() for l in blocks[p].strip().split("\n") if l.strip()][-1] == str(p) for p in range(1, 27))
        and norm(blocks[27]).startswith("table 4"))
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is on its page verbatim", norm(quote) in norm(blocks[int(page)]))

fab = "Comparison income enters positively and significantly."
s.check("CO-C1 control: CO-12 with negatively replaced by positively is absent", norm(fab) not in norm(TXT.read_text()))
q9 = next(q for r, _, q in rows if r == "CO-9")
s.check("CO-C2 control: CO-9 is not on p.11, so the page check can fail", norm(q9) not in norm(blocks[11]))

lean_audit(s, "CO", "lean/Lit/ClarkOswald1996.lean", "Lit.ClarkOswald1996", "ClarkOswald1996", claims, CONTROLS)
sys.exit(s.finish())
