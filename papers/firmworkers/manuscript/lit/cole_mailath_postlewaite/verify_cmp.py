import pathlib
import re
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF92, TXT92 = CACHE / "cmp_1992.pdf", CACHE / "cmp_1992.txt"
PDF95, TXT95 = CACHE / "cmp_1995_wp.pdf", CACHE / "cmp_1995_wp.txt"
SHA92 = "6dd143cf39b4323b9b184a74cf123a7d6e8a3fa1d22387eda1c5f0a7cd08635d"
SHA95 = "4534ad07a47f68fe35bc29bfa1c82819b18b73f2d91c403f8db59e2a584bc93a"
FIRST92, LAST92 = 1092, 1125
FIRST95, LAST95 = 1, 18
CONTROLS = [
    "control_rank_changes_when_removing_below",
    "control_status_rule_needs_exact_match",
    "control_separability_needs_match_independent_of_capital",
    "control_deviation_needs_beta_lt_one",
    "control_L_exceeds_needs_positive_status",
    "control_slope_needs_dispersion_change",
]


def pages92(text):
    return {i + 1091: b for i, b in enumerate(text.split("\f"))}


def pages95(text):
    pages, last = {}, 0
    for block in text.split("\f"):
        lines = [l.strip() for l in block.strip().split("\n") if l.strip()]
        if lines and re.fullmatch(r"\d{1,2}", lines[-1]) and int(lines[-1]) == last + 1:
            last += 1
            pages[last] = block
    return pages


s = Suite("COLE-MAILATH-POSTLEWAITE (JPE 1992; CARESS WP 95-14; lean/Lit/ColeMailathPostlewaite.lean)")
claims = (HERE / "CLAIMS.md").read_text()
keys = (HERE / "KEYS").read_text().split()
s.check("CMP-K KEYS lists both keys", "ColeMailathPostlewaite1992" in keys and "ColeMailathPostlewaite1995" in keys)
s.check("CMP-K LEAN names lean/Lit/ColeMailathPostlewaite.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/ColeMailathPostlewaite.lean")
rows92 = quotation_rows(claims, "CMP92")
rows95 = quotation_rows(claims, "CMP95")
s.check("CMP-Q0 quotation rows for both papers", len(rows92) >= 20 and len(rows95) >= 6, f"{len(rows92)} + {len(rows95)} rows")
s.check("CMP-Q1 every CMP92 page lies in 1092-1125", all(FIRST92 <= int(p) <= LAST92 for _, p, _ in rows92))
s.check("CMP-Q2 every CMP95 page lies in 1-18", all(FIRST95 <= int(p) <= LAST95 for _, p, _ in rows95))

if not all(f.exists() for f in (PDF92, TXT92, PDF95, TXT95)):
    s.check("CMP-S0 the cached PDFs and texts are available", False, f"cache the two Drive PDFs under {CACHE}")
    sys.exit(s.finish())
s.check("CMP-S1 the JPE scan matches its pinned sha256", sha256(PDF92) == SHA92)
s.check("CMP-S2 the CARESS scan matches its pinned sha256", sha256(PDF95) == SHA95)
p92 = pages92(TXT92.read_text())
p95 = pages95(TXT95.read_text())
s.check("CMP-S3 the JPE copy is Social Norms, Savings Behavior, and Growth, vol. 100 no. 6",
        "social norms savings behavior and growth" in norm(p92[1091]) and "vol 100 no 6" in norm(p92[1091]))
s.check("CMP-S4 the JPE blocks line up with the printed pages",
        "1093" in p92[1093][:120] and norm(p92[1125]).startswith("social norms 1125"))
s.check("CMP-S5 the CARESS copy is Working Paper 95-14 by Cole, Mailath and Postlewaite",
        "caress working paper 95 14" in norm(TXT95.read_text()[:400]))
s.check("CMP-S6 CMP95 pages 1-18 recovered in increasing order", sorted(p95) == list(range(1, 19)), f"{len(p95)} pages")
for rid, page, quote in rows92:
    s.check(f"{rid} (p.{page}) is on its page verbatim", norm(quote) in norm(p92[int(page)]))
for rid, page, quote in rows95:
    s.check(f"{rid} (p.{page}) is on its page verbatim", int(page) in p95 and norm(quote) in norm(p95[int(page)]))

fab92 = "An important feature of this equilibrium is that the path of the capital stock differs from that in the involuntary matching model."
s.check("CMP-C1 control: CMP92-19 with identical replaced by differs is absent",
        norm(fab92) not in norm("".join(p92.values())))
fab95 = "it is competition from above that distorts individuals' effort decisions"
s.check("CMP-C2 control: CMP95-5 with below replaced by above is absent",
        norm(fab95) not in norm(TXT95.read_text()))
q15 = next(q for r, _, q in rows92 if r == "CMP92-15")
s.check("CMP-C3 control: CMP92-15 is not on p.1109, so the page check can fail", norm(q15) not in norm(p92[1109]))

lean_audit(s, "CMP", "lean/Lit/ColeMailathPostlewaite.lean", "Lit.ColeMailathPostlewaite",
           "ColeMailathPostlewaite", claims, CONTROLS)
sys.exit(s.finish())
