"""Suite for Chiarella and Szidarovszky (2002), Computers and Mathematics with
Applications 43, 169-178. It pins the cached PDF by sha256, extracts its text
with pdftotext -layout, checks each text quotation verbatim (after normalising
case, spacing, punctuation and ligatures) on the page named, runs a
reversed-quote control, pins the page images of pp.172, 173, 174 and 177 for
the quotations whose text layer is garbled, and measures
lean/mathlib/ChiarellaSzidarovszky.lean: a build root, it builds, no sorry,
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
PDF = CACHE / "asymptotic_dynamic_rent.pdf"
TXT = PDF.with_suffix(".txt")
SHA256 = "e0c2531d2b82b536ab2d06a31991d9d77268b74f69b806ca277c81e6b3d76f89"
OFFSET = 168  # printed page minus PDF page; the first text chunk is PDF page 1
IMAGE_PAGES = {
    4: "6956afa7509ee9cc57f84911b548e85b14230f9c33c817ab022c3b35feb3bb05",
    5: "1f6051e97fd8e98ccb249b819fc78df73dd1f1a2246e352704e539b4bdf6a87b",
    6: "bd5b339705a16e6063e33808dc8034eafbef13ed525706252cdd3086ca823b62",
    9: "6b474ab53f264f512a0974ce221f3804571a703ac373b13283af753ea559b96f",
}
LIGATURES = {"ﬀ": "ff", "ﬁ": "fi", "ﬂ": "fl", "ﬃ": "ffi", "ﬄ": "ffl"}
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
print("CHIARELLA-SZIDAROVSZKY (2002; quotations; lean/mathlib/ChiarellaSzidarovszky.lean)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(CS-[DP]?\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("CS-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 27, f"{len(rows)} rows")

pages = None
if not PDF.exists():
    check("CS-Q1 the cached PDF is available", False, f"download the paper from Drive to {PDF}")
else:
    d = hashlib.sha256(PDF.read_bytes()).hexdigest()
    check("CS-Q1 the cached PDF is the one read", d == SHA256, d[:16])
    pt = shutil.which("pdftotext")
    if pt:
        pages = subprocess.run([pt, "-layout", str(PDF), "-"], capture_output=True, text=True).stdout.split("\f")
    elif TXT.exists():
        pages = TXT.read_text().split("\f")
if pages is None:
    check("CS-Q2 the text is available", False, "install pdftotext or provide the cached text")
    sys.exit(1)
check("CS-Q2 the text is Chiarella and Szidarovszky's article",
      "Dynamic Rent-Seeking Games" in pages[0] and len(pages) >= 10, f"{len(pages)} pages")

image_rows = []
for rid, page, quote in rows:
    if rid.startswith("CS-P"):
        image_rows.append((rid, int(page)))
        continue
    idx = int(page) - OFFSET - 1
    check(f"{rid} (p.{page}) is on the page verbatim", 0 <= idx < len(pages) and norm(quote) in norm(pages[idx]))

flip = "in the presence of a dominant agent complex eigenvalues are impossible"
check("CS-C1 control: CS-2 with possible replaced by impossible is not in the text",
      flip not in norm(" ".join(pages)))
quote1 = [q for r, p, q in rows if r == "CS-1"][0]
check("CS-C2 control: CS-1 is not on the last page", norm(quote1) not in norm(pages[9]))

pdftoppm = shutil.which("pdftoppm")
for rid, page in image_rows:
    n = page - OFFSET
    if n not in IMAGE_PAGES:
        check(f"{rid} (p.{page}) has a pinned page image", False, f"no hash for PDF page {n}")
        continue
    img = CACHE / "chiarella_pages" / f"p-{n:02d}.png"
    if not img.exists() and pdftoppm:
        img.parent.mkdir(parents=True, exist_ok=True)
        tmpdir = tempfile.mkdtemp()
        subprocess.run([pdftoppm, "-r", "130", "-f", str(n), "-l", str(n), "-png", str(PDF),
                        os.path.join(tmpdir, "p")], capture_output=True)
        made = sorted(pathlib.Path(tmpdir).glob("p-*.png"))
        if made:
            shutil.move(str(made[-1]), str(img))
    ok = img.exists() and hashlib.sha256(img.read_bytes()).hexdigest() == IMAGE_PAGES[n]
    check(f"{rid} (p.{page}) is read from the pinned page image p-{n:02d}.png", ok,
          "image-only: the text layer is garbled")

ROOT = HERE.parent.parent
PROJ = ROOT / "lean" / "mathlib"
LSRC = PROJ / "ChiarellaSzidarovszky.lean"
LNS = "ChiarellaSzidarovszky2002"
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
check("CS-L2 ChiarellaSzidarovszky is a build root", '"ChiarellaSzidarovszky"' in lakefile)
check("CS-L3 no sorry in the source", re.search(r"\bsorry\b", lsrc) is None)
declared = re.findall(r"^(?:theorem|lemma)\s+([^\s:(\[{]+)", lsrc, re.M)
cited = sorted(set(re.findall(r"`([a-z][A-Za-z0-9_']*)`", claims)))
missing = [c for c in cited if c not in declared]
check("CS-L4 every Lean name cited in CLAIMS.md is declared", not missing,
      f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))
lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
b = subprocess.run([lake, "build", "ChiarellaSzidarovszky"], cwd=PROJ, capture_output=True, text=True)
check("CS-L5 lake build ChiarellaSzidarovszky succeeds", b.returncode == 0, b.stderr.strip()[-200:])
names = [f"{LNS}.{d}" for d in declared]
with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=PROJ, delete=False) as fh:
    fh.write("import ChiarellaSzidarovszky\n")
    for n in names:
        fh.write(f"#print axioms {n}\n")
    tmp = fh.name
pr = subprocess.run([lake, "env", "lean", tmp], cwd=PROJ, capture_output=True, text=True)
os.unlink(tmp)
used = parse_axioms(pr.stdout + pr.stderr, names)
check("CS-L6 the axiom audit covers every declared theorem", len(used) == len(names), f"{len(used)} of {len(names)}")
bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
check("CS-L7 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")
check("CS-L8 the controls are present",
      "ex2_not_admissible" in declared and "wA_unstable" in declared and "wB_unstable_pair" in declared)

fails = results.count(False)
print("=" * 72)
print(f"CHIARELLA-SZIDAROVSZKY SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
