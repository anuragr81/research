import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "lazear_rosen_1981.pdf"
TXT = CACHE / "lazear_rosen_1981.txt"
PDF_SHA = "c45beeca541d99db242a2cd8fb111e992292ec872735a38193727318a5d5dc3d"
FIRST, LAST = 841, 864
CONTROLS = ["control_self_sort_needs_slope_bound", "control_handicap_needs_zero_profit"]
OCR_DIGITS = str.maketrans({"o": "0", "i": "1"})


def squeeze(s):
    return norm(s).replace(" ", "")


def numbered(page, block):
    lines = [l.strip() for l in block.split("\n") if l.strip()]
    if page == FIRST:
        return lines[-1] == str(page)
    head = lines[0].split()
    return str(page) in (head[0].translate(OCR_DIGITS), head[-1].translate(OCR_DIGITS))


s = Suite("LAZEAR-ROSEN-1981 (JPE article; quotations; lean/Lit/LazearRosen1981.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("LR-K KEYS lists LazearRosen1981", "LazearRosen1981" in (HERE / "KEYS").read_text().split())
s.check("LR-K LEAN names lean/Lit/LazearRosen1981.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/LazearRosen1981.lean")
rows = quotation_rows(claims, "LR")
s.check("LR-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 40, f"{len(rows)} rows")
s.check("LR-Q1 every page lies in pp.841-864", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

if not (PDF.exists() and TXT.exists()):
    s.check("LR-S0 the cached PDF and text are available", False,
            f"download 1981-lazear.pdf to {PDF} and run pdftotext -layout to {TXT}")
    sys.exit(s.finish())
s.check("LR-S1 the cached PDF matches its pinned sha256", sha256(PDF) == PDF_SHA)
parts = TXT.read_text().split("\f")
blocks = {p: parts[p - FIRST] for p in range(FIRST, LAST + 1)}
s.check("LR-S2 the first page is Rank-Order Tournaments as Optimum Labor Contracts by Lazear and Rosen, JPE 1981 vol. 89 no. 5",
        "rankordertournamentsasoptimumlaborcontracts" in squeeze(blocks[FIRST])
        and "lazear" in norm(blocks[FIRST]) and "rosen" in norm(blocks[FIRST])
        and "journalofpoliticaleconomy1981vol89no5" in squeeze(blocks[FIRST]))
s.check("LR-S3 every page carries its number (860 and 861 read as 86o and 86i)",
        all(numbered(p, blocks[p]) for p in range(FIRST, LAST + 1)))
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is on its page with spaces removed", squeeze(quote) in squeeze(blocks[int(page)]))

fab = "Individuals will self-select the payment scheme in accordance with their ability."
s.check("LR-C1 control: LR-26 with wealth replaced by ability is absent", squeeze(fab) not in squeeze(TXT.read_text()))
q26 = next(q for r, _, q in rows if r == "LR-26")
s.check("LR-C2 control: LR-26 is not on p.854, so the page check can fail", squeeze(q26) not in squeeze(blocks[854]))

lean_audit(s, "LR", "lean/Lit/LazearRosen1981.lean", "Lit.LazearRosen1981", "LazearRosen1981", claims, CONTROLS)
sys.exit(s.finish())
