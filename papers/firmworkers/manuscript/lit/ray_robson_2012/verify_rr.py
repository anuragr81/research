import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "ray_robson_2012.pdf"
TXT = CACHE / "ray_robson_2012.txt"
PDF_SHA = "755339cfe6f4e1401b704fa8c027c8aaa802716a53afd319b02bbf33aaf5a71e"
FIRST, LAST = 1505, 1531
CONTROLS = [
    "control_total_status_needs_half",
    "control_efficiency_needs_strict_concavity",
    "control_savings_rule_needs_discounting",
]

s = Suite("RAY-ROBSON-2012 (Econometrica 80(4); quotations; lean/Lit/RayRobson2012.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("RR-K KEYS lists RayRobson2012", "RayRobson2012" in (HERE / "KEYS").read_text().split())
s.check("RR-K LEAN names lean/Lit/RayRobson2012.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/RayRobson2012.lean")
rows = quotation_rows(claims, "RR")
s.check("RR-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 20, f"{len(rows)} rows")
s.check("RR-Q1 every page lies in pp.1505-1531", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

if not (PDF.exists() and TXT.exists()):
    s.check("RR-S0 the cached PDF and text are available", False,
            f"download RayRobsonStatus.pdf to {PDF} and run pdftotext -layout to {TXT}")
    sys.exit(s.finish())
s.check("RR-S1 the cached PDF matches its pinned sha256", sha256(PDF) == PDF_SHA)
text = TXT.read_text()
blocks = {FIRST - 1 + i: b for i, b in enumerate(text.split("\f")) if i >= 1 and b.strip()}
s.check("RR-S2 the first page is Econometrica 80(4) 1505-1531 by Ray and Robson",
        "econometrica vol 80 no 4 july 2012 1505 1531" in norm(blocks[FIRST])
        and "ray and arthur robson" in norm(blocks[FIRST]))
s.check("RR-S3 blocks 1505-1531 are present and each running head carries its page",
        sorted(blocks) == list(range(FIRST, LAST + 1))
        and all(str(p) in b.strip().split("\n")[0] for p, b in blocks.items()))
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is on its page verbatim", norm(quote) in norm(blocks[int(page)]))

fab = "Our main result delivers the Friedman–Savage findings with a strong assumption on the curvature of utility in status."
s.check("RR-C1 control: RR-5 with no assumption at all replaced by a strong assumption is absent",
        norm(fab) not in norm(text))
q7 = next(q for r, _, q in rows if r == "RR-7")
s.check("RR-C2 control: RR-7 is not on p.1508, so the page check can fail", norm(q7) not in norm(blocks[1508]))

lean_audit(s, "RR", "lean/Lit/RayRobson2012.lean", "Lit.RayRobson2012", "RayRobson2012", claims, CONTROLS)
sys.exit(s.finish())
