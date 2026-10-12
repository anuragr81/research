import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "kahneman_tversky_1979.pdf"
OCR = CACHE / "kahneman_tversky_1979.ocr.txt"
PDF_SHA = "ac09b4c02f081123fbe4bb0d14cbebfe065cbf57e787b773543fd5c9a72eb4e1"
OCR_SHA = "9ddeaa09d54d310bb9fed0ebf576259509ebafa3230d3d5f520c6c50c478e971"
FIRST, LAST = 263, 291
CONTROLS = [
    "control_loss_aversion_needs_aversion_to_bets",
    "control_equations_differ_under_subcertainty",
    "control_probabilistic_needs_strict_concavity",
    "control_gamble_needs_subcertainty",
]

s = Suite("KAHNEMAN-TVERSKY-1979 (Econometrica 47(2); quotations; lean/Lit/KahnemanTversky1979.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("KT-K KEYS lists KahnemanTversky1979", "KahnemanTversky1979" in (HERE / "KEYS").read_text().split())
s.check("KT-K LEAN names lean/Lit/KahnemanTversky1979.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/KahnemanTversky1979.lean")
rows = quotation_rows(claims, "KT")
s.check("KT-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 15, f"{len(rows)} rows")
s.check("KT-Q1 every page lies in pp.263-291", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

if not (PDF.exists() and OCR.exists()):
    s.check("KT-S0 the cached scan and OCR text are available", False,
            f"download ProspectTheory.pdf to {PDF} and run lit/ocr_pages.py single to {OCR}")
    sys.exit(s.finish())
s.check("KT-S1 the cached scan matches its pinned sha256", sha256(PDF) == PDF_SHA)
s.check("KT-S2 the cached OCR text matches its pinned sha256", sha256(OCR) == OCR_SHA)
blocks = {FIRST - 1 + i: b for i, b in enumerate(OCR.read_text().split("\f")) if i >= 1}
s.check("KT-S3 the first page is Econometrica 47(2), Prospect Theory, by Kahneman and Tversky",
        "volume 47 march 1979 number 2" in norm(blocks[FIRST]) and "kahneman and amos tversky" in norm(blocks[FIRST]))
s.check("KT-S4 one block per page 263-291, each carrying its page number",
        sorted(blocks) == list(range(FIRST, LAST + 1))
        and all(str(p) in b.strip().split("\n")[0] for p, b in blocks.items() if p > FIRST))
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is on its page verbatim", norm(quote) in norm(blocks[int(page)]))

fab = "A salient characteristic of attitudes to changes in welfare is that gains loom larger than losses."
s.check("KT-C1 control: KT-14 with gains and losses swapped is absent", norm(fab) not in norm(OCR.read_text()))
q19 = next(q for r, _, q in rows if r == "KT-19")
s.check("KT-C2 control: KT-19 is not on p.286, so the page check can fail", norm(q19) not in norm(blocks[286]))

lean_audit(s, "KT", "lean/Lit/KahnemanTversky1979.lean", "Lit.KahnemanTversky1979", "KahnemanTversky1979",
           claims, CONTROLS)
sys.exit(s.finish())
