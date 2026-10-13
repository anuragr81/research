"""Suite for Fu and Lu (2009), "Contest with pre-contest investment", read as the
authors' manuscript of February 2009. It pins the cached PDF by sha256, extracts its text
with pdftotext -layout, checks each text quotation verbatim (after normalising
case, spacing, punctuation and ligatures) on the page named, runs a
reversed-quote control, pins the page images of pp.3, 5 and 6 for the quotations
whose text layer is garbled, and measures lean/mathlib/FuLu.lean: a build root, it builds, no sorry,
every Lean name cited in CLAIMS.md declared, every theorem within the Mathlib
axioms, and the controls present.
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
PDF = CACHE / "fu_lu_2009.pdf"
TXT = PDF.with_suffix(".txt")
SHA256 = "4a77b64917f9e8d86e3459be5409acde281f5496ddb6d7ad2149da620ce1b220"
OFFSET = 0  # manuscript page equals PDF page
IMAGE_PAGES = {
    3: "0a708874ea9ec4025f897569b762679db9263a070751c0932bd042ccd41c7ee3",
    5: "2f2407a99cf7e6b4f1372b1aa92e3624569dab36703f8590a8012e34ea7a0815",
    6: "51974f9ab3631a2c72846bbec28019ccec21518051b24412cdab1d8036350302",
}
# the manuscript's font renders ligatures as "¤" (ff), "…" (fi) and "¢" (ffi)
LIGATURES = {"ﬀ": "ff", "ﬁ": "fi", "ﬂ": "fl", "ﬃ": "ffi", "ﬄ": "ffl", "¤": "ff", "…": "fi", "¢ ": "ffi", "¢": "ffi"}
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def norm(s):
    for k, v in LIGATURES.items():
        s = s.replace(k, v)
    s = re.sub(r"-\s*\n\s*", "", s).lower()
    s = re.sub(r"[^a-z0-9]+", " ", s)
    return re.sub(r"\s+", " ", s).strip()


print("=" * 72)
print("FU-LU (manuscript 2009; quotations; lean/mathlib/FuLu.lean)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(FU-[DP]?\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("FU-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 20, f"{len(rows)} rows")

pages = None
if not PDF.exists():
    check("FU-Q1 the cached PDF is available", False, f"download the paper from Drive to {PDF}")
else:
    d = hashlib.sha256(PDF.read_bytes()).hexdigest()
    check("FU-Q1 the cached PDF is the one read", d == SHA256, d[:16])
    pt = shutil.which("pdftotext")
    if pt:
        pages = subprocess.run([pt, "-layout", str(PDF), "-"], capture_output=True, text=True).stdout.split("\f")
    elif TXT.exists():
        pages = TXT.read_text().split("\f")
if pages is None:
    check("FU-Q2 the text is available", False, "install pdftotext or provide the cached text")
    sys.exit(1)
check("FU-Q2 the text is Fu and Lu's manuscript",
      "Contest with Pre-Contest Investment" in pages[0] and len(pages) >= 8, f"{len(pages)} pages")

image_rows = []
for rid, page, quote in rows:
    if rid.startswith("FU-P"):
        image_rows.append((rid, int(page)))
        continue
    idx = int(page) - OFFSET - 1
    check(f"{rid} (p.{page}) is on the page verbatim", 0 <= idx < len(pages) and norm(quote) in norm(pages[idx]))

flip = "positive equilibrium investment emerges when the number of contestants is large"
check("FU-C1 control: FU-3 with small replaced by large is not in the text",
      flip not in norm(" ".join(pages)))
quote1 = [q for r, p, q in rows if r == "FU-1"][0]
check("FU-C2 control: FU-1 is not on the last page", norm(quote1) not in norm(pages[7]))

pdftoppm = shutil.which("pdftoppm")
for rid, page in image_rows:
    n = page - OFFSET
    if n not in IMAGE_PAGES:
        check(f"{rid} (p.{page}) has a pinned page image", False, f"no hash for PDF page {n}")
        continue
    img = CACHE / "fulu_pages" / f"p-{n}.png"
    if not img.exists() and pdftoppm:
        img.parent.mkdir(parents=True, exist_ok=True)
        tmpdir = tempfile.mkdtemp()
        subprocess.run([pdftoppm, "-r", "130", "-f", str(n), "-l", str(n), "-png", str(PDF),
                        os.path.join(tmpdir, "p")], capture_output=True)
        made = sorted(pathlib.Path(tmpdir).glob("p-*.png"))
        if made:
            shutil.move(str(made[-1]), str(img))
    ok = img.exists() and hashlib.sha256(img.read_bytes()).hexdigest() == IMAGE_PAGES[n]
    check(f"{rid} (p.{page}) is read from the pinned page image p-{n}.png", ok,
          "image-only: the text layer is garbled")

ROOT = HERE.parent.parent
PROJ = ROOT / "lean" / "mathlib"
LSRC = PROJ / "FuLu.lean"
LNS = "FuLu2009"
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
check("FU-L2 FuLu is a build root", '"FuLu"' in lakefile)
check("FU-L3 no sorry in the source", re.search(r"\bsorry\b", lsrc) is None)
declared = re.findall(r"^(?:theorem|lemma)\s+([^\s:(\[{]+)", lsrc, re.M)
cited = sorted(set(re.findall(r"`([a-z][A-Za-z0-9_']*)`", claims)))
missing = [c for c in cited if c not in declared]
check("FU-L4 every Lean name cited in CLAIMS.md is declared", not missing,
      f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))
lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
b = subprocess.run([lake, "build", "FuLu"], cwd=PROJ, capture_output=True, text=True)
check("FU-L5 lake build FuLu succeeds", b.returncode == 0, b.stderr.strip()[-200:])
names = [f"{LNS}.{d}" for d in declared]
with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=PROJ, delete=False) as fh:
    fh.write("import FuLu\n")
    for n in names:
        fh.write(f"#print axioms {n}\n")
    tmp = fh.name
pr = subprocess.run([lake, "env", "lean", tmp], cwd=PROJ, capture_output=True, text=True)
os.unlink(tmp)
used = parse_axioms(pr.stdout + pr.stderr, names)
check("FU-L6 the axiom audit covers every declared theorem", len(used) == len(names), f"{len(used)} of {len(names)}")
bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
check("FU-L7 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")
check("FU-L8 the controls are present",
      "prop1ii_fails_K7" in declared and "K3_zero_beats_candidate" in declared and "K2_candidate_beats_zero" in declared)

fails = results.count(False)
print("=" * 72)
print(f"FU-LU SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
