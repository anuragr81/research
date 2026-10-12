import pathlib
import re
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "robson_1992.pdf"
OCR = CACHE / "robson_1992.ocr.txt"
PDF_SHA = "fc361a3878e81928f1698445034447e411abafe22d30824133249095345cc632"
OCR_SHA = "b51d77285f100ae9f62e0c8ed22b39a28aacf54682752b4eb1be0f48975a239e"
FIRST, LAST = 837, 857
CONTROLS = [
    "control_total_status_needs_half_ties",
    "control_mass_point_needs_jump",
    "control_rejection_needs_concavity",
    "control_convex_region_needs_status",
    "control_fixed_inflection_proportion_falls",
    "control_total_fixed_needs_whole_population",
    "control_status_lottery_needs_strict_convexity",
]


def page_blocks(text):
    lines = text.split("\n")
    marks = [(i, int(l.strip())) for i, l in enumerate(lines)
             if re.fullmatch(r"\d{3}", l.strip()) and FIRST < int(l.strip()) <= LAST]
    blocks, start, page = {}, 0, FIRST
    for i, p in marks + [(len(lines), LAST + 1)]:
        blocks[page] = "\n".join(lines[start:i])
        start, page = i, p
    return blocks, [p for _, p in marks]


s = Suite("ROBSON-1992 (Econometrica 60(4); quotations; lean/Lit/Robson1992.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("ROB-K KEYS lists Robson1992", "Robson1992" in (HERE / "KEYS").read_text().split())
s.check("ROB-K LEAN names lean/Lit/Robson1992.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/Robson1992.lean")
rows = quotation_rows(claims, "ROB")
s.check("ROB-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 20, f"{len(rows)} rows")
s.check("ROB-Q1 every page lies in pp.837-857", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

if not (PDF.exists() and OCR.exists()):
    s.check("ROB-S0 the cached scan and OCR text are available", False,
            f"download status_distribution_wealth.pdf to {PDF} and Drive's text of it to {OCR}")
    sys.exit(s.finish())
s.check("ROB-S1 the cached scan matches its pinned sha256", sha256(PDF) == PDF_SHA)
s.check("ROB-S2 the cached OCR text matches its pinned sha256", sha256(OCR) == OCR_SHA)
text = OCR.read_text()
s.check("ROB-S3 the first page is Econometrica 60(4) 837-857 by Robson",
        "econometrica vol 60 no 4 july 1992 837 857" in norm(text[:400]) and "robson" in norm(text[:400]))
blocks, found = page_blocks(text)
s.check("ROB-S4 every printed page number 838-857 is recovered in order", found == list(range(FIRST + 1, LAST + 1)),
        f"{len(found)} found")
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is on its page verbatim", norm(quote) in norm(blocks[int(page)]))

fab = "This is a distribution of wealth such that all free fair gambles are accepted."
s.check("ROB-C1 control: ROB-6 with rejected replaced by accepted is absent", norm(fab) not in norm(text))
q16 = next(q for r, _, q in rows if r == "ROB-16")
s.check("ROB-C2 control: ROB-16 is not on p.842, so the page check can fail", norm(q16) not in norm(blocks[842]))

lean_audit(s, "ROB", "lean/Lit/Robson1992.lean", "Lit.Robson1992", "Robson1992", claims, CONTROLS)
sys.exit(s.finish())
