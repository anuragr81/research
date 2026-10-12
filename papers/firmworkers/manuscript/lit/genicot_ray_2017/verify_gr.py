import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "genicot_ray_2017.pdf"
TXT = CACHE / "genicot_ray_2017.txt"
PDF_SHA = "cbf493e019f1bb91a385d6e32a72e3c2f8d1c0d6bfdeb501c207feae4a9a9bcb"
FIRST, LAST = 489, 519
CONTROLS = [
    "control_ratio_needs_social_monotonicity",
    "control_stay_needs_milestone_jump",
    "control_growth_needs_positive_extra",
]

s = Suite("GENICOT-RAY-2017 (Econometrica 85(2); quotations; lean/Lit/GenicotRay2017.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("GR-K KEYS lists GenicotRay2017", "GenicotRay2017" in (HERE / "KEYS").read_text().split())
s.check("GR-K LEAN names lean/Lit/GenicotRay2017.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/GenicotRay2017.lean")
rows = quotation_rows(claims, "GR")
s.check("GR-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 20, f"{len(rows)} rows")
s.check("GR-Q1 every page lies in pp.489-519", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

if not (PDF.exists() and TXT.exists()):
    s.check("GR-S0 the cached PDF and text are available", False,
            f"download GenicotRayAsp.pdf to {PDF} and run pdftotext -layout to {TXT}")
    sys.exit(s.finish())
s.check("GR-S1 the cached PDF matches its pinned sha256", sha256(PDF) == PDF_SHA)
text = TXT.read_text()
blocks = {FIRST - 1 + i: b for i, b in enumerate(text.split("\f")) if i >= 1 and b.strip()}
s.check("GR-S2 the first page is Econometrica 85(2) 489-519 by Genicot and Ray",
        "econometrica vol 85 no 2 march 2017 489 519" in norm(blocks[FIRST])
        and "genicot and debraj ray" in norm(blocks[FIRST]))
s.check("GR-S3 blocks 489-519 are present and each running head carries its page",
        sorted(blocks) == list(range(FIRST, LAST + 1))
        and all(str(p) in b.strip().split("\n")[0] for p, b in blocks.items()))
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is on its page verbatim", norm(quote) in norm(blocks[int(page)]))

fab = "With no cross-identity interactions, such an extension is difficult as it would simply represent several co-existing copies of our model."
s.check("GR-C1 control: GR-15 with trivial replaced by difficult is absent", norm(fab) not in norm(text))
q14 = next(q for r, _, q in rows if r == "GR-14")
s.check("GR-C2 control: GR-14 is not on p.493, so the page check can fail", norm(q14) not in norm(blocks[493]))

lean_audit(s, "GR", "lean/Lit/GenicotRay2017.lean", "Lit.GenicotRay2017", "GenicotRay2017", claims, CONTROLS)
sys.exit(s.finish())
