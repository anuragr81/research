import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "bmw_2005.pdf"
TXT = CACHE / "bmw_2005.txt"
PDF_SHA = "62df91b717e95d67474a7b4d9caa032f21451cd639be5e637463eccc13cf87f9"
FIRST, LAST = 1, 32
CONTROLS = [
    "control_more_consumption_needs_complementarity",
    "control_kink_needs_rising_price_or_complementarity",
    "control_printed_Fstar_misses_top",
    "control_density_tail_needs_large_delta",
]

s = Suite("BECKER-MURPHY-WERNING-2005 (authors' manuscript, Dec 2003; quotations; lean/Lit/BeckerMurphyWerning2005.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("BMW-K KEYS lists BeckerMurphyWerning2005", "BeckerMurphyWerning2005" in (HERE / "KEYS").read_text().split())
s.check("BMW-K LEAN names lean/Lit/BeckerMurphyWerning2005.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/BeckerMurphyWerning2005.lean")
rows = quotation_rows(claims, "BMW")
s.check("BMW-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 20, f"{len(rows)} rows")
s.check("BMW-Q1 every page lies in pp.1-32", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

if not (PDF.exists() and TXT.exists()):
    s.check("BMW-S0 the cached PDF and text are available", False,
            f"download the Drive PDF to {PDF} and run pdftotext -layout to {TXT}")
    sys.exit(s.finish())
s.check("BMW-S1 the cached PDF matches its pinned sha256", sha256(PDF) == PDF_SHA)
text = TXT.read_text()
blocks = {i + 1: b for i, b in enumerate(text.split("\f")) if b.strip()}
s.check("BMW-S2 the first page is the manuscript by Becker, Murphy and Werning, revised December 2003",
        "the equilibrium distribution of income and the market for status" in norm(blocks[1])
        and "revised december 2003" in norm(blocks[1]) and "werning" in norm(blocks[1]))
s.check("BMW-S3 every block 1-32 ends with its page number",
        sorted(blocks) == list(range(FIRST, LAST + 1))
        and all([l.strip() for l in b.strip().split("\n") if l.strip()][-1] == str(p) for p, b in blocks.items()))
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is on its page verbatim", norm(quote) in norm(blocks[int(page)]))

fab = "If status and consumption were complements, winners would have higher status and higher utility, but lower consumption."
s.check("BMW-C1 control: BMW-10 with substitutes replaced by complements is absent", norm(fab) not in norm(text))
q21 = next(q for r, _, q in rows if r == "BMW-21")
s.check("BMW-C2 control: BMW-21 is not on p.26, so the page check can fail", norm(q21) not in norm(blocks[26]))

lean_audit(s, "BMW", "lean/Lit/BeckerMurphyWerning2005.lean", "Lit.BeckerMurphyWerning2005",
           "BeckerMurphyWerning2005", claims, CONTROLS)
sys.exit(s.finish())
