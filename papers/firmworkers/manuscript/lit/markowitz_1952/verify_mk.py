import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "markowitz_1952.pdf"
OCR = CACHE / "markowitz_1952.ocr.txt"
PDF_SHA = "ec2f16ff3965b5b99cdafddaeee3761eb0145a59574d341652e6cdf962eaa3f1"
OCR_SHA = "a78ce6666ff253e68ca8500f05b88080317496dc8e07b22e77a166ee598bdfd9"
FIRST, LAST = 151, 158
CONTROLS = [
    "control_printed_p154_inequality_favours_bets",
    "control_stakes_need_concavity",
    "control_customary_needs_adaptation_rule",
]

s = Suite("MARKOWITZ-1952 (JPE 60(2), Cowles reprint; quotations; lean/Lit/Markowitz1952.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("MK-K KEYS lists Markowitz1952", "Markowitz1952" in (HERE / "KEYS").read_text().split())
s.check("MK-K LEAN names lean/Lit/Markowitz1952.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/Markowitz1952.lean")
rows = quotation_rows(claims, "MK")
s.check("MK-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 20, f"{len(rows)} rows")
s.check("MK-Q1 every page lies in pp.151-158", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

if not (PDF.exists() and OCR.exists()):
    s.check("MK-S0 the cached scan and OCR text are available", False,
            f"download the Drive PDF to {PDF} and run lit/ocr_pages.py columns to {OCR}")
    sys.exit(s.finish())
s.check("MK-S1 the cached scan matches its pinned sha256", sha256(PDF) == PDF_SHA)
s.check("MK-S2 the cached OCR text matches its pinned sha256", sha256(OCR) == OCR_SHA)
parts = OCR.read_text().split("\f")
s.check("MK-S3 one block per PDF page, pp.151-158", len(parts) == 1 + (LAST - FIRST + 1))
blocks = {FIRST - 1 + i: b for i, b in enumerate(parts) if i >= 1}
s.check("MK-S4 the first page is the Cowles reprint of JPE vol. LX no. 2, April 1952, by Markowitz",
        "cowles foundation paper 57" in norm(blocks[FIRST]) and "april 1952" in norm(blocks[FIRST])
        and "markowitz" in norm(blocks[FIRST]))
s.check("MK-S5 even pages open with their number and the author",
        all(norm(blocks[p].strip().split("\n")[0]).startswith(f"{p} harry markowi") for p in (152, 154, 156)))
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is on its page verbatim", norm(quote) in norm(blocks[int(page)]))

fab = "Generally people prefer symmetric bets."
s.check("MK-C1 control: MK-11 with avoid replaced by prefer is absent", norm(fab) not in norm(OCR.read_text()))
q10 = next(q for r, _, q in rows if r == "MK-10")
s.check("MK-C2 control: MK-10 is not on p.155, so the page check can fail", norm(q10) not in norm(blocks[155]))

lean_audit(s, "MK", "lean/Lit/Markowitz1952.lean", "Lit.Markowitz1952", "Markowitz1952", claims, CONTROLS)
sys.exit(s.finish())
