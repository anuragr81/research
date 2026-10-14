"""Suite for Fernandez and Gali, "To Each According To...? Markets, Tournaments, and the Matching
Problem With Borrowing Constraints" (NBER WP 5930, 1997; RES 1999). The PDF holds page images
only. The suite pins the PDF by sha256 and pins the OCR text made by ocr_fernandez_gali.py. It
checks each quotation verbatim on the PDF page named, against that text, ignoring case,
punctuation and spaces. It runs a reversed-quote control and pins the page image behind the row
read from the image. It then measures lean/mathlib/FernandezGali.lean: a build root, it builds,
no sorry, every Lean name cited in CLAIMS.md declared, every theorem within the Mathlib axioms,
and the controls present.
"""

import hashlib
import os
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile

HERE = pathlib.Path(__file__).resolve().parent
CACHE = pathlib.Path.home() / ".cache" / "entry_contest"
PDF = CACHE / "fernandez_gali_1997_w5930.pdf"
TXT = CACHE / "fernandez_gali_1997.txt"
SHA256 = "b8d56c8f50a69f19aa4615909f79646c4e79c1cfba44fab6677c4beb55d97829"
TXT_SHA256 = "9d7d94341acd002fd145ca041019fd399df3a754b97d9cec9a9d98c0dd57fdec"
IMAGE_PAGES = {  # PDF page: sha256 of the page rendered by pdftoppm -r 300 -gray -png
    29: "a15ff58325998f5a22a825e84bea16ac86ef20c1a2235b2e290ae37d111bbabc",
}
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def squash(s):
    s = re.sub(r"-\s*\n\s*", "", s).lower()
    return re.sub(r"[^a-z0-9]+", "", s)


print("=" * 72)
print("FERNANDEZ-GALI 1997 (NBER WP 5930; quotations; lean/mathlib/FernandezGali.lean)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(FG-P?\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("FG-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 20, f"{len(rows)} rows")

if not PDF.exists():
    check("FG-Q1 the cached PDF is available", False,
          f"download https://www.nber.org/system/files/working_papers/w5930/w5930.pdf to {PDF}")
    sys.exit(1)
d = hashlib.sha256(PDF.read_bytes()).hexdigest()
check("FG-Q1 the cached PDF is the one read", d == SHA256, d[:16])
if not TXT.exists():
    check("FG-Q2 the OCR text is available", False, f"run ocr_fernandez_gali.py to make {TXT}")
    sys.exit(1)
t = hashlib.sha256(TXT.read_bytes()).hexdigest()
check("FG-Q2 the OCR text is the one checked", t == TXT_SHA256, t[:16])
pages = TXT.read_text().split("\f")
check("FG-Q3 the text is Fernandez and Gali's working paper",
      "markets" in squash(pages[1]) and "borrowingconstraints" in squash(pages[1])
      and "workingpaper5930" in squash(pages[0]) and len(pages) >= 46, f"{len(pages) - 1} pages")

image_rows = []
for rid, page, quote in rows:
    if rid.startswith("FG-P"):
        image_rows.append((rid, int(page)))
        continue
    idx = int(page) - 1
    check(f"{rid} (p.{page}) is on the page verbatim",
          0 <= idx < len(pages) and squash(quote) in squash(pages[idx]))

flip = squash("with borrowing constraints the allocation achieved by markets always delivers higher "
              "aggregate output than does the tournament system")
check("FG-C1 control: FG-16 with markets and tournaments swapped is not in the text",
      all(flip not in squash(p) for p in pages))
quote16 = [q for r, p, q in rows if r == "FG-16"][0]
check("FG-C2 control: FG-16 is not on p.26", squash(quote16) not in squash(pages[25]))

pdftoppm = shutil.which("pdftoppm")
for rid, page in image_rows:
    if page not in IMAGE_PAGES:
        check(f"{rid} (p.{page}) has a pinned page image", False, f"no hash for PDF page {page}")
        continue
    img = CACHE / "fg_pages" / f"pg-{page:02d}.png"
    if not img.exists() and pdftoppm:
        img.parent.mkdir(parents=True, exist_ok=True)
        tmpdir = tempfile.mkdtemp()
        subprocess.run([pdftoppm, "-r", "300", "-gray", "-png", "-f", str(page), "-l", str(page),
                        str(PDF), os.path.join(tmpdir, "pg")], capture_output=True)
        made = pathlib.Path(tmpdir) / f"pg-{page:02d}.png"
        if made.exists():
            shutil.move(str(made), str(img))
    ok = img.exists() and hashlib.sha256(img.read_bytes()).hexdigest() == IMAGE_PAGES[page]
    check(f"{rid} (p.{page}) is read from the pinned page image {img.name}", ok,
          "formulas are read from the image")

ROOT = HERE.parent.parent
PROJ = ROOT / "lean" / "mathlib"
LSRC = PROJ / "FernandezGali.lean"
LNS = "FernandezGali1997"
ALLOWED = {"propext", "Classical.choice", "Quot.sound"}


def parse_axioms(out, names):
    used = {}
    for n in names:
        m = re.search(rf"'{re.escape(n)}' (does not depend on any axioms|depends on axioms: \[([^\]]*)\])", out)
        if m:
            used[n] = set() if m.group(2) is None else {a.strip() for a in m.group(2).split(",")}
    return used


lsrc = LSRC.read_text()
lakefile = (PROJ / "lakefile.toml").read_text()
check("FG-L2 FernandezGali is a build root", '"FernandezGali"' in lakefile)
check("FG-L3 no sorry in the source", re.search(r"\bsorry\b", lsrc) is None)
declared = re.findall(r"^(?:theorem|lemma)\s+([^\s:(\[{]+)", lsrc, re.M)
cited = sorted(set(re.findall(r"`([a-z][A-Za-z0-9_']*)`", claims)))
missing = [c for c in cited if c not in declared]
check("FG-L4 every Lean name cited in CLAIMS.md is declared", not missing,
      f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))
lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
b = subprocess.run([lake, "build", "FernandezGali"], cwd=PROJ, capture_output=True, text=True)
check("FG-L5 lake build FernandezGali succeeds", b.returncode == 0, b.stderr.strip()[-200:])
names = [f"{LNS}.{d}" for d in declared]
with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=PROJ, delete=False) as fh:
    fh.write("import FernandezGali\n")
    for n in names:
        fh.write(f"#print axioms {n}\n")
    tmp = fh.name
pr = subprocess.run([lake, "env", "lean", tmp], cwd=PROJ, capture_output=True, text=True)
os.unlink(tmp)
used = parse_axioms(pr.stdout + pr.stderr, names)
check("FG-L6 the axiom audit covers every declared theorem", len(used) == len(names), f"{len(used)} of {len(names)}")
bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
check("FG-L7 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")
check("FG-L8 the controls are present",
      all(c in declared for c in ("sorting_needs_strict", "assortative_needs_supermod", "clearing_P0",
                                  "kappa_log_not_const")))

fails = results.count(False)
print("=" * 72)
print(f"FERNANDEZ-GALI 1997 SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
