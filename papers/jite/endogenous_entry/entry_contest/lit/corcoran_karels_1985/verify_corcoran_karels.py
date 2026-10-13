"""Suite for Corcoran and Karels, "Rent-seeking behavior in the long-run" (Public Choice 46,
1985). The PDF holds page images only. The suite pins the PDF by sha256 and pins the OCR text
made by ocr_corcoran_karels.py. It checks each quotation verbatim on the page named, against
that text, ignoring case, punctuation and spaces (the OCR sometimes joins words). It runs a
reversed-quote control and pins the page images behind the rows read from images. It then
measures lean/mathlib/CorcoranKarels.lean: a build root, it builds, no sorry, every Lean name
cited in CLAIMS.md declared, every theorem within the Mathlib axioms, and the controls present.
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
PDF = CACHE / "corcoran_karels_1985.pdf"
TXT = CACHE / "corcoran_karels_1985.txt"
SHA256 = "542c3ff2105477ddf735ba9ceae36646d5fed5bf9e1bd23a4405be11e6f3549c"
TXT_SHA256 = "8d3be0f72bca7c17d6af0b1e09d2d2648da16c97b087ab3df934ee48074f7eb2"
OFFSET = 226  # printed page minus PDF page
IMAGE_PAGES = {  # PDF page: sha256 of the page image extracted by pdfimages -png
    2: "a8481b76ad8ef17b782c022443a41a5c3af8d9a63fa352eeedec00e03daec8c6",
    4: "60e2f9f0dfb418bea608fec7ccde60b71830510b32637876596b2ff5a61c0c87",
    6: "11f829644f054f07a02dc6811e3436ba1cae46d45225209fa8a149cebc769f53",
    8: "065086ea74687ce62c02196a078ecf3e84a768a827e1df2c6093df16ec10508e",
    11: "5dad73031c5518ee04d4ae6731df1803f670dc65947591925ffe1a08ee961bd9",
    15: "8cc3a54469f6ac0495af0b7d8d5d74d05a8db99a97832fb514dbd47284d5d513",
    18: "e717de3f21d1dc37932d502a44a43025a9b2922b21394ca9088776741fc6ec8e",
    19: "8b7131ff83de3ad845bd8673c3e995469bd3309080d5252807488272a1c403f5",
}
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def squash(s):
    s = re.sub(r"-\s*\n\s*", "", s).lower()
    return re.sub(r"[^a-z0-9]+", "", s)


print("=" * 72)
print("CORCORAN-KARELS 1985 (Public Choice; quotations; lean/mathlib/CorcoranKarels.lean)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(CK-[DP]?\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("CK-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 40, f"{len(rows)} rows")

if not PDF.exists():
    check("CK-Q1 the cached PDF is available", False, f"download the paper from Drive to {PDF}")
    sys.exit(1)
d = hashlib.sha256(PDF.read_bytes()).hexdigest()
check("CK-Q1 the cached PDF is the one read", d == SHA256, d[:16])
if not TXT.exists():
    check("CK-Q2 the OCR text is available", False, f"run ocr_corcoran_karels.py to make {TXT}")
    sys.exit(1)
t = hashlib.sha256(TXT.read_bytes()).hexdigest()
check("CK-Q2 the OCR text is the one checked", t == TXT_SHA256, t[:16])
pages = TXT.read_text().split("\f")
check("CK-Q3 the text is Corcoran and Karels's article",
      "rentseekingbehaviorinthelongrun" in squash(pages[0]) and len(pages) >= 20, f"{len(pages) - 1} pages")

image_rows = []
for rid, page, quote in rows:
    if rid.startswith("CK-P"):
        image_rows.append((rid, int(page)))
        continue
    idx = int(page) - OFFSET - 1
    check(f"{rid} (p.{page}) is on the page verbatim",
          0 <= idx < len(pages) and squash(quote) in squash(pages[idx]))

flip = squash("For r-values larger than 4, only two players can be sustained.")
check("CK-C1 control: CK-D5 with one replaced by two is not in the text",
      all(flip not in squash(p) for p in pages))
quote21 = [q for r, p, q in rows if r == "CK-21"][0]
check("CK-C2 control: CK-21 is not on p.228", squash(quote21) not in squash(pages[228 - OFFSET - 1]))

pdfimages = shutil.which("pdfimages")
for rid, page in image_rows:
    n = page - OFFSET
    if n not in IMAGE_PAGES:
        check(f"{rid} (p.{page}) has a pinned page image", False, f"no hash for PDF page {n}")
        continue
    img = CACHE / "ck_pages" / f"img-{2 * (n - 1):03d}.png"
    if not img.exists() and pdfimages:
        img.parent.mkdir(parents=True, exist_ok=True)
        tmpdir = tempfile.mkdtemp()
        subprocess.run([pdfimages, "-png", "-f", str(n), "-l", str(n), str(PDF),
                        os.path.join(tmpdir, "x")], capture_output=True)
        made = pathlib.Path(tmpdir) / "x-000.png"
        if made.exists():
            shutil.move(str(made), str(img))
    ok = img.exists() and hashlib.sha256(img.read_bytes()).hexdigest() == IMAGE_PAGES[n]
    check(f"{rid} (p.{page}) is read from the pinned page image {img.name}", ok,
          "image-only: equations and tables are read from the image")

ROOT = HERE.parent.parent
PROJ = ROOT / "lean" / "mathlib"
LSRC = PROJ / "CorcoranKarels.lean"
LNS = "CorcoranKarels1985"
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
check("CK-L2 CorcoranKarels is a build root", '"CorcoranKarels"' in lakefile)
check("CK-L3 no sorry in the source", re.search(r"\bsorry\b", lsrc) is None)
declared = re.findall(r"^(?:theorem|lemma)\s+([^\s:(\[{]+)", lsrc, re.M)
cited = sorted(set(re.findall(r"`([a-z][A-Za-z0-9_']*)`", claims)))
missing = [c for c in cited if c not in declared]
check("CK-L4 every Lean name cited in CLAIMS.md is declared", not missing,
      f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))
lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
b = subprocess.run([lake, "build", "CorcoranKarels"], cwd=PROJ, capture_output=True, text=True)
check("CK-L5 lake build CorcoranKarels succeeds", b.returncode == 0, b.stderr.strip()[-200:])
names = [f"{LNS}.{d}" for d in declared]
with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=PROJ, delete=False) as fh:
    fh.write("import CorcoranKarels\n")
    for n in names:
        fh.write(f"#print axioms {n}\n")
    tmp = fh.name
pr = subprocess.run([lake, "env", "lean", tmp], cwd=PROJ, capture_output=True, text=True)
os.unlink(tmp)
used = parse_axioms(pr.stdout + pr.stderr, names)
check("CK-L6 the axiom audit covers every declared theorem", len(used) == len(names), f"{len(used)} of {len(names)}")
bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
check("CK-L7 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")
check("CK-L8 the controls are present",
      "ent_le" in declared and "range_nonempty_iff" in declared and "two_sustained_r42" in declared)

fails = results.count(False)
print("=" * 72)
print(f"CORCORAN-KARELS 1985 SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
