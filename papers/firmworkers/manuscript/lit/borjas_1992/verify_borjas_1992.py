import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, page_texts, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "borjas_1992.pdf"
TXT = CACHE / "borjas_1992.txt"
PDF_SHA = "5f90cc1cbed44a26cb646e7d83ad5e0207c076c10087fba01e253537be1a1433"
FIRST, LAST = 123, 150
CONTROLS = [
    "control_complementarity_needs_beta2_pos",
    "control_eta_needs_rho_lt_one",
    "control_eta_needs_beta1_lt_one",
    "control_gap_needs_lt_one",
    "control_understates_needs_gamma2_pos",
    "control_sum_needs_pi_lt_one",
]

s = Suite("BORJAS-1992 (QJE 1992; quotations; lean/Lit/Borjas1992.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("BJ-K KEYS lists Borjas1992", "Borjas1992" in (HERE / "KEYS").read_text().split())
s.check("BJ-K LEAN names lean/Lit/Borjas1992.lean", (HERE / "LEAN").read_text().strip() == "lean/Lit/Borjas1992.lean")
rows = quotation_rows(claims, "BJ")
s.check("BJ-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 15, f"{len(rows)} rows")
s.check("BJ-Q1 every page lies in 123-150", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

if not (PDF.exists() and TXT.exists()):
    s.check("BJ-S0 the cached PDF and text are available", False,
            f"download ethnic_capital_1992.pdf from Drive to {PDF} and run pdftotext -layout")
    sys.exit(s.finish())
s.check("BJ-S1 the cached PDF matches its pinned sha256", sha256(PDF) == PDF_SHA)
pages = page_texts(TXT.read_text(), FIRST)
s.check("BJ-S2 the text has one block per page", max(pages) >= LAST)
s.check("BJ-S3 the title page is Borjas, Ethnic Capital and Intergenerational Mobility",
        "ethnic capital and intergenerational mobility" in norm(pages[FIRST]) and "borjas" in norm(pages[FIRST]))
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is on its page verbatim", norm(quote) in norm(pages[int(page)]))

whole = norm("".join(pages.values()))
fabricated = "the relative dispersion that exists in human capital among ethnic groups in the parent's generation will disappear within one generation"
s.check("BJ-C1 control: BJ-8 with its conclusion reversed is not in the text", norm(fabricated) not in whole)
bj1 = next(q for r, _, q in rows if r == "BJ-1")
s.check("BJ-C2 control: BJ-1 is not on p.124, so the page check can fail", norm(bj1) not in norm(pages[124]))

lean_audit(s, "BJ", "lean/Lit/Borjas1992.lean", "Lit.Borjas1992", "Borjas1992", claims, CONTROLS)
sys.exit(s.finish())
