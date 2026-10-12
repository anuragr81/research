import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "friedman_savage_1948.pdf"
OCR = CACHE / "friedman_savage_1948.ocr.txt"
PDF_SHA = "21fe1a556fa20286be51bf6c11992e70c0793224feee241625e423cf1571113c"
OCR_SHA = "a12ccf28f8979557a3008b6b9b85aafd03ac940aae11fb67ee994fa05977c3a0"
FIRST, LAST = 279, 304
HEAD = "the utility analysis of choices involving risk"
CONTROLS = [
    "control_insurance_needs_strict_concavity",
    "control_compound_needs_both_refused",
    "control_two_prizes_can_win",
]

s = Suite("FRIEDMAN-SAVAGE-1948 (JPE 56(4); quotations; lean/Lit/FriedmanSavage1948.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("FS-K KEYS lists FriedmanSavage1948", "FriedmanSavage1948" in (HERE / "KEYS").read_text().split())
s.check("FS-K LEAN names lean/Lit/FriedmanSavage1948.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/FriedmanSavage1948.lean")
rows = quotation_rows(claims, "FS")
s.check("FS-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 20, f"{len(rows)} rows")
s.check("FS-Q1 every page lies in pp.279-304", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

if not (PDF.exists() and OCR.exists()):
    s.check("FS-S0 the cached scan and OCR text are available", False,
            f"download friedmansavutilriskjpe1948.pdf to {PDF} and run lit/ocr_pages.py columns to {OCR}")
    sys.exit(s.finish())
s.check("FS-S1 the cached scan matches its pinned sha256", sha256(PDF) == PDF_SHA)
s.check("FS-S2 the cached OCR text matches its pinned sha256", sha256(OCR) == OCR_SHA)
parts = OCR.read_text().split("\f")
s.check("FS-S3 one block per PDF page: a cover sheet and pp.279-304", len(parts) == 1 + 1 + (LAST - FIRST + 1))
blocks = {FIRST - 2 + i: b for i, b in enumerate(parts) if i >= 2}
s.check("FS-S4 the first page is the JPE August 1948 article by Friedman and Savage",
        "august 1948" in norm(blocks[FIRST]) and "friedman" in norm(blocks[FIRST]) and "savage" in norm(blocks[FIRST]))
first_lines = {p: norm(b.strip().split("\n")[0]) for p, b in blocks.items()}
s.check("FS-S5 even pages open with their number, odd pages with the running head",
        all(first_lines[p] == str(p) for p in blocks if p % 2 == 0)
        and all(first_lines[p] == HEAD for p in blocks if p % 2 == 1 and p > FIRST))
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is on its page verbatim", norm(quote) in norm(blocks[int(page)]))

fab = "Men will and do take great risks to distinguish themselves, even when they do not know what the risks are."
s.check("FS-C1 control: FS-18 with know replaced by do not know is absent", norm(fab) not in norm(OCR.read_text()))
q7 = next(q for r, _, q in rows if r == "FS-7")
s.check("FS-C2 control: FS-7 is not on p.287, so the page check can fail", norm(q7) not in norm(blocks[287]))

lean_audit(s, "FS", "lean/Lit/FriedmanSavage1948.lean", "Lit.FriedmanSavage1948", "FriedmanSavage1948",
           claims, CONTROLS)
sys.exit(s.finish())
