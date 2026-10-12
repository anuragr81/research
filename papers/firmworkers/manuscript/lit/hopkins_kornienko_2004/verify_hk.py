import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))
from litcheck import CACHE, Suite, lean_audit, norm, quotation_rows, sha256

HERE = pathlib.Path(__file__).resolve().parent
PDF = CACHE / "hopkins_kornienko_2004.pdf"
RENDERINGS = {
    "layout": CACHE / "hopkins_kornienko_2004.txt",
    "reading order": CACHE / "hopkins_kornienko_2004.raw.txt",
    "columns": CACHE / "hopkins_kornienko_2004.cols.txt",
    "OCR": CACHE / "hopkins_kornienko_2004.ocr.txt",
}
PDF_SHA = "84fbe499eb3f53cc8e745788a7efb82e68e3f1f30e8cd7af58c2c97b884660cc"
COLS_SHA = "c7b43f8a8f504feef48118f21ce3c66f5ce65fec5fb4be641f2c5dc8d4dfbbbe"
OCR_SHA = "5b5cd03ea5feec8317631c141308d4a5efc56b3b0a71e4e3c5011442e60b8472"
FIRST, LAST = 1085, 1107
CONTROLS = ["control_tie_needs_gamma_lt_one", "control_example1_tax_needs_a_zero",
            "control_poor_comparison_needs_positive_V"]

s = Suite("HOPKINS-KORNIENKO-2004 (AER article; quotations; lean/Lit/HopkinsKornienko2004.lean)")
claims = (HERE / "CLAIMS.md").read_text()
s.check("HK-K KEYS lists HopkinsKornienko2004", "HopkinsKornienko2004" in (HERE / "KEYS").read_text().split())
s.check("HK-K LEAN names lean/Lit/HopkinsKornienko2004.lean",
        (HERE / "LEAN").read_text().strip() == "lean/Lit/HopkinsKornienko2004.lean")
rows = quotation_rows(claims, "HK")
s.check("HK-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 25, f"{len(rows)} rows")
s.check("HK-Q1 every page lies in pp.1085-1107", all(FIRST <= int(p) <= LAST for _, p, _ in rows))

missing = [str(f) for f in [PDF, *RENDERINGS.values()] if not f.exists()]
if missing:
    s.check("HK-S0 the cached PDF and its renderings are available", False,
            f"missing {missing}; see RECONSTRUCTION.md for how each is made")
    sys.exit(s.finish())
s.check("HK-S1 the cached PDF matches its pinned sha256", sha256(PDF) == PDF_SHA)
s.check("HK-S2 the column and OCR renderings match their pinned sha256",
        sha256(RENDERINGS["columns"]) == COLS_SHA and sha256(RENDERINGS["OCR"]) == OCR_SHA)
pages = {}
for name, path in RENDERINGS.items():
    parts = path.read_text().split("\f")
    if name == "OCR":
        parts = parts[1:]
    pages[name] = {p: norm(parts[p - 1084]) for p in range(FIRST, LAST + 1)}
cover = norm(RENDERINGS["layout"].read_text().split("\f")[0])
s.check("HK-S3 the cover is Running to Keep in the Same Place by Hopkins and Kornienko, AER 94(4), pp.1085-1107",
        "running to keep in the same place" in cover and "hopkins" in cover and "kornienko" in cover
        and "vol 94 no 4" in cover and "pp 1085 1107" in cover)
s.check("HK-S4 every page carries its number in the layout and OCR renderings",
        all(str(p) in pages[n][p] for p in range(FIRST, LAST + 1) for n in ("layout", "OCR")))
for rid, page, quote in rows:
    found = [n for n in RENDERINGS if norm(quote) in pages[n][int(page)]]
    s.check(f"{rid} (p.{page}) is on its page verbatim", found, ", ".join(found))

fab = "In a more equal society, the poor are better off."
s.check("HK-C1 control: HK-2 with worse replaced by better is absent from every rendering",
        all(norm(fab) not in norm(path.read_text()) for path in RENDERINGS.values()))
q21 = next(q for r, _, q in rows if r == "HK-21")
s.check("HK-C2 control: HK-21 is not on p.1095 in any rendering, so the page check can fail",
        all(norm(q21) not in pages[n][1095] for n in RENDERINGS))

lean_audit(s, "HK", "lean/Lit/HopkinsKornienko2004.lean", "Lit.HopkinsKornienko2004", "HopkinsKornienko2004",
           claims, CONTROLS)
sys.exit(s.finish())
