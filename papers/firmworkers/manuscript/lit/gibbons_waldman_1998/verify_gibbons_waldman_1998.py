import pathlib
import re
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "gibbons_waldman_1998_wp.pdf"
OCR = CACHE / "gibbons_waldman_1998_wp.ocr.txt"
PDF_SHA = "c2532455b6317169b726a2577cd2e8bf7d101e326f1ffe2e6845065e659d0bcd"
OCR_SHA = "c12dfbbb1c174582f13fb69b59e49d963bf3e5363717eef4d55f4c55f35942a8"
FIRST, LAST = 1, 40
CONTROLS = [
    "control_prefer_needs_order",
    "control_wage_mono_needs_positive_slopes",
    "control_posterior_needs_interior_prior",
    "control_ratio_needs_concavity",
]


def page_blocks(text):
    lines = text.split("\n")
    markers, last = [], 0
    for i, line in enumerate(lines):
        s = line.strip()
        if re.fullmatch(r"\d{1,2}", s) and last < int(s) <= last + 3:
            markers.append((i, int(s)))
            last = int(s)
    blocks = {}
    bounds = [(0, 1)] + markers + [(len(lines), LAST + 1)]
    for (start, page), (end, nxt) in zip(bounds, bounds[1:]):
        chunk = "\n".join(lines[start:end])
        for p in range(page, nxt):
            blocks[p] = chunk
    return blocks, [m for _, m in markers]


s = Suite("GIBBONS-WALDMAN-1998 (NBER WP 6454; quotations; lean/Lit/GibbonsWaldman1998.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("GW-K KEYS lists GibbonsWaldman1999", "GibbonsWaldman1999" in (HERE / "KEYS").read_text().split())
s.check("GW-K LEAN names lean/Lit/GibbonsWaldman1998.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/GibbonsWaldman1998.lean")
rows = quotation_rows(claims, "GW")
s.check("GW-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 15, f"{len(rows)} rows")
s.check("GW-Q1 every page lies in pp.1-40", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

if not (PDF.exists() and OCR.exists()):
    s.check("GW-S0 the cached scan and OCR text are available", False,
            f"download w6454.pdf to {PDF} and Drive's text of it to {OCR}")
    sys.exit(s.finish())
s.check("GW-S1 the cached scan matches its pinned sha256", sha256(PDF) == PDF_SHA)
s.check("GW-S2 the cached OCR text matches its pinned sha256", sha256(OCR) == OCR_SHA)
text = OCR.read_text()
s.check("GW-S3 the title page is NBER Working Paper 6454 by Gibbons and Waldman",
        "working paper 6454" in norm(text[:600]) and "gibbons" in norm(text[:600]) and "waldman" in norm(text[:600]))
blocks, found = page_blocks(text)
s.check("GW-S4 at least 30 printed page numbers are recovered in order", len(found) >= 30, f"{len(found)} found")
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is in its page block verbatim", norm(quote) in norm(blocks[int(page)]))

fabricated = "For the same reason, a worker's wage falls every period."
s.check("GW-C1 control: GW-6 with rises replaced by falls is not in the text", norm(fabricated) not in norm(text))
gw16 = next(q for r, _, q in rows if r == "GW-16")
s.check("GW-C2 control: GW-16 is not in the p.10 block, so the page check can fail",
        norm(gw16) not in norm(blocks[10]))

lean_audit(s, "GW", "lean/Lit/GibbonsWaldman1998.lean", "Lit.GibbonsWaldman1998", "GibbonsWaldman1998",
           claims, CONTROLS)
sys.exit(s.finish())
