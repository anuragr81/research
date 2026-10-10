import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, page_texts, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "cunha_heckman_2007_wp.pdf"
TXT = CACHE / "cunha_heckman_2007_wp.txt"
PDF_SHA = "b78d465c373fd588ac86dd8a4b98faad32aa4ad1093bb2c76138a43027015d84"
FIRST_BLOCK_PAGE, FIRST, LAST = -1, 1, 24
CONTROLS = [
    "control_timing_needs_half",
    "control_invest_early_needs_budget",
    "control_ratio_needs_phi_lt_one",
    "control_below_needs_sigma_lt_one",
]

s = Suite("CUNHA-HECKMAN-2007 (NBER WP 12840; quotations; lean/Lit/CunhaHeckman2007.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("CH-K KEYS lists CunhaHeckman2007", "CunhaHeckman2007" in (HERE / "KEYS").read_text().split())
s.check("CH-K LEAN names lean/Lit/CunhaHeckman2007.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/CunhaHeckman2007.lean")
rows = quotation_rows(claims, "CH")
s.check("CH-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 18, f"{len(rows)} rows")
s.check("CH-Q1 every page lies in the text, pp.1-24", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

if not (PDF.exists() and TXT.exists()):
    s.check("CH-S0 the cached PDF and text are available", False,
            f"download w12840.pdf from Drive to {PDF} and run pdftotext -layout")
    sys.exit(s.finish())
s.check("CH-S1 the cached PDF matches its pinned sha256", sha256(PDF) == PDF_SHA)
pages = page_texts(TXT.read_text(), FIRST_BLOCK_PAGE)
s.check("CH-S2 the cover is NBER Working Paper 12840, The Technology of Skill Formation",
        "working paper 12840" in norm(pages[-1]) and "the technology of skill formation" in norm(pages[-1]))
s.check("CH-S3 the page blocks line up with the printed page numbers",
        norm(pages[1]).endswith(" 1") and norm(pages[24]).endswith(" 24"))
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is on its page verbatim", norm(quote) in norm(pages[int(page)]))

whole = norm("".join(pages.values()))
fabricated = "It is optimal to invest late if γ > (1 − γ) (1 + r)."
s.check("CH-C1 control: CH-11 with early replaced by late is not in the text", norm(fabricated) not in whole)
ch7 = next(q for r, _, q in rows if r == "CH-7")
s.check("CH-C2 control: CH-7 is not on p.12, so the page check can fail", norm(ch7) not in norm(pages[12]))

lean_audit(s, "CH", "lean/Lit/CunhaHeckman2007.lean", "Lit.CunhaHeckman2007", "CunhaHeckman2007", claims, CONTROLS)
sys.exit(s.finish())
