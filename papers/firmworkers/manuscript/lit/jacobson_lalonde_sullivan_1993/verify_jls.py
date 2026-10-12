import pathlib
import re
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "jls_1992_wp.pdf"
TXT = CACHE / "jls_1992_wp.txt"
PDF_SHA = "22595eed564fd4b8b53b2826992bb811a1e60bb23a3fc12dac422bba7a15363c"
FIRST, NUMBERED, LAST = 1, 36, 45
CONTROLS = ["control_simple_change_needs_constant_time_effects", "control_average_needs_balance",
            "control_bias_bound_needs_same_sign"]


def squeeze(s):
    return norm(s).replace(" ", "")


def last_number(block):
    nums = [l.strip() for l in block.split("Digitized for FRASER")[0].split("\n") if re.fullmatch(r"\d+", l.strip())]
    return nums[-1] if nums else None


s = Suite("JLS-1993 (Chicago Fed WP-92-28, revised Nov 1992; quotations; lean/Lit/JacobsonLaLondeSullivan1993.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("JLS-K KEYS lists JacobsonLaLondeSullivan1993", "JacobsonLaLondeSullivan1993" in (HERE / "KEYS").read_text().split())
s.check("JLS-K LEAN names lean/Lit/JacobsonLaLondeSullivan1993.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/JacobsonLaLondeSullivan1993.lean")
rows = quotation_rows(claims, "JLS")
foot = quotation_rows(claims, "JLS-F")
s.check("JLS-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 40 and len(foot) >= 2,
        f"{len(rows)} rows, {len(foot)} footnote rows")
s.check("JLS-Q1 every page lies in pp.1-45", all(FIRST <= int(p) <= LAST for _, p, _ in rows + foot))

if not (PDF.exists() and TXT.exists()):
    s.check("JLS-S0 the cached PDF and text are available", False,
            f"download frbchi_workingpaper_1992-28.pdf to {PDF} and run pdftotext -layout to {TXT}")
    sys.exit(s.finish())
s.check("JLS-S1 the cached PDF matches its pinned sha256", sha256(PDF) == PDF_SHA)
parts = TXT.read_text().split("\f")
blocks = {p: parts[p + 1] for p in range(FIRST, LAST + 1)}
s.check("JLS-S2 the cover is Chicago Fed WP-92-28 and the title page is Earnings Losses of Displaced Workers, revised November 1992",
        "wp 92 28" in norm(parts[0]) and "earnings losses of displaced workers" in norm(parts[1])
        and "revised november 1992" in norm(parts[1])
        and all(a in norm(parts[1]) for a in ("jacobson", "lalonde", "sullivan")))
s.check("JLS-S3 pages 1-36 end with their number before the FRASER footer",
        all(last_number(blocks[p]) == str(p) for p in range(FIRST, NUMBERED + 1)))
s.check("JLS-S4 pages 37-45 are Table 1, Table 2 (two pages), Table 3 and Figures 1-5",
        [norm(blocks[p]).split(" ")[:2] for p in range(37, 46)]
        == [["table", "1"], ["table", "2"], ["table", "2"], ["table", "3"]] + [["figure", str(i)] for i in range(1, 6)])
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is on its page verbatim", norm(quote) in norm(blocks[int(page)]))
for rid, page, quote in foot:
    s.check(f"{rid} (p.{page}) is on its page with spaces removed", squeeze(quote) in squeeze(blocks[int(page)]))

fab = "their losses are still equal to 50 percent of their pre-displacement earnings"
s.check("JLS-C1 control: JLS-4 with 25 replaced by 50 is absent", norm(fab) not in norm(TXT.read_text()))
q4 = next(q for r, _, q in rows if r == "JLS-4")
s.check("JLS-C2 control: JLS-4 is not on p.4, so the page check can fail", norm(q4) not in norm(blocks[4]))
fabf = "largely due to an increase in the number of workers without quarterly earnings"
s.check("JLS-C3 control: footnote 30 reversed is absent with spaces removed", squeeze(fabf) not in squeeze(blocks[19]))

lean_audit(s, "JLS", "lean/Lit/JacobsonLaLondeSullivan1993.lean", "Lit.JacobsonLaLondeSullivan1993",
           "JacobsonLaLondeSullivan1993", claims, CONTROLS)
sys.exit(s.finish())
