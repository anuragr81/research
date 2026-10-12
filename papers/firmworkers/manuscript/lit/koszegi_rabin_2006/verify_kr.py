import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "koszegi_rabin_2006.pdf"
TXT = CACHE / "koszegi_rabin_2006.txt"
PDF_SHA = "c02790a4166994ca7e8a8b8eb675067c854227d280c02029413318b75964c581"
FIRST, LAST = 1133, 1165
CONTROLS = [
    "control_A2_needs_loss_aversion",
    "control_effects_need_loss_aversion",
]

s = Suite("KOSZEGI-RABIN-2006 (QJE 121(4); quotations; lean/Lit/KoszegiRabin2006.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("KR-K KEYS lists KoszegiRabin2006", "KoszegiRabin2006" in (HERE / "KEYS").read_text().split())
s.check("KR-K LEAN names lean/Lit/KoszegiRabin2006.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/KoszegiRabin2006.lean")
rows = quotation_rows(claims, "KR")
s.check("KR-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 15, f"{len(rows)} rows")
s.check("KR-Q1 every page lies in pp.1133-1165", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

if not (PDF.exists() and TXT.exists()):
    s.check("KR-S0 the cached PDF and text are available", False,
            f"download KoszegiRabin2006.pdf to {PDF} and run pdftotext -layout to {TXT}")
    sys.exit(s.finish())
s.check("KR-S1 the cached PDF matches its pinned sha256", sha256(PDF) == PDF_SHA)
text = TXT.read_text()
blocks = {FIRST + i: b for i, b in enumerate(text.split("\f")) if b.strip()}
s.check("KR-S2 the first page is QJE 121(4), A Model of Reference-Dependent Preferences",
        "a model of reference dependent preferences" in norm(blocks[FIRST])
        and "november 2006" in norm(blocks[FIRST]) and "rabin" in norm(blocks[FIRST]))
s.check("KR-S3 blocks 1133-1165 are present and each carries its page number",
        sorted(blocks) == list(range(FIRST, LAST + 1))
        and blocks[FIRST].strip().split("\n")[-1].strip() == str(FIRST)
        and all(str(p) in b.strip().split("\n")[0] for p, b in blocks.items() if p > FIRST))
for rid, page, quote in rows:
    s.check(f"{rid} (p.{page}) is on its page verbatim", norm(quote) in norm(blocks[int(page)]))

fab = "We assume that a person’s reference point is her status quo held in the recent past about outcomes"
s.check("KR-C1 control: KR-2 with rational expectations replaced by status quo is absent", norm(fab) not in norm(text))
q9 = next(q for r, _, q in rows if r == "KR-9")
s.check("KR-C2 control: KR-9 is not on p.1140, so the page check can fail", norm(q9) not in norm(blocks[1140]))

lean_audit(s, "KR", "lean/Lit/KoszegiRabin2006.lean", "Lit.KoszegiRabin2006", "KoszegiRabin2006", claims, CONTROLS)
sys.exit(s.finish())
