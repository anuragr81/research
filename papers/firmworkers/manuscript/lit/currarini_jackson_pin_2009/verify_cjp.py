import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "currarini_jackson_pin_wp.pdf"
TXT = CACHE / "currarini_jackson_pin_wp.txt"
PDF_SHA = "cc2bb2b47dec2ff9d3a231651475bc0d8c8a2b8a001585de1ac9ec25bff1f562"
FIRST, LAST = 1, 46
CONTROLS = [
    "control_opposite_needs_two_types",
    "control_closure_needs_outgroup",
]

s = Suite("CURRARINI-JACKSON-PIN-2009 (working paper, July 2008; quotations; lean/Lit/CurrariniJacksonPin2009.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("CJP-K KEYS lists CurrariniJacksonPin2009", "CurrariniJacksonPin2009" in (HERE / "KEYS").read_text().split())
s.check("CJP-K LEAN names lean/Lit/CurrariniJacksonPin2009.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/CurrariniJacksonPin2009.lean")
rows = quotation_rows(claims, "CJP")
s.check("CJP-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 20, f"{len(rows)} rows")
s.check("CJP-Q1 every page lies in pp.1-46", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

if not (PDF.exists() and TXT.exists()):
    s.check("CJP-S0 the cached PDF and text are available", False,
            f"download netminority-1.pdf to {PDF} and run pdftotext -layout to {TXT}")
    sys.exit(s.finish())
s.check("CJP-S1 the cached PDF matches its pinned sha256", sha256(PDF) == PDF_SHA)
parts = TXT.read_text().split("\f")
blocks = {1: parts[0]}
blocks.update({p: parts[p] for p in range(2, LAST + 1)})
s.check("CJP-S2 the first page is the working paper revised July 29, 2008 by Currarini, Jackson and Pin",
        "an economic model of friendship" in norm(blocks[1]) and "revised july 29 2008" in norm(blocks[1])
        and "forthcoming econometrica" in norm(blocks[1]))
s.check("CJP-S3 the overflow page and every page 1-46 end with their number",
        [l.strip() for l in parts[1].strip().split("\n") if l.strip()][-1] == "2"
        and all([l.strip() for l in b.strip().split("\n") if l.strip()][-1] == str(p) for p, b in blocks.items()))
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is on its page verbatim", norm(quote) in norm(blocks[int(page)]))

fab = "Simply by balance, if one group is homophilous, then another group must be homophilous."
s.check("CJP-C1 control: CJP-15 with heterophilous replaced by homophilous is absent", norm(fab) not in norm(TXT.read_text()))
q9 = next(q for r, _, q in rows if r == "CJP-9")
s.check("CJP-C2 control: CJP-9 is not on p.6, so the page check can fail", norm(q9) not in norm(blocks[6]))

lean_audit(s, "CJP", "lean/Lit/CurrariniJacksonPin2009.lean", "Lit.CurrariniJacksonPin2009",
           "CurrariniJacksonPin2009", claims, CONTROLS)
sys.exit(s.finish())
