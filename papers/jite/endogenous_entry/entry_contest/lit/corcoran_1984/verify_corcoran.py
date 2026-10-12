"""Suite for Corcoran (1984), Public Choice 43(1), 89-94. It pins the cached
PDF by sha256, extracts its text with pdftotext -layout, checks each text
quotation verbatim (after normalising case, spacing, punctuation and
ligatures) on the page named, runs a reversed-quote control, pins the page
image of p.92 for the three quotations whose text layer is garbled, and
measures lean/mathlib/Corcoran.lean: a build root, it builds, no sorry, every
Lean name cited in CLAIMS.md declared, every theorem within the Mathlib axioms,
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
PDF = CACHE / "corcoran_1984.pdf"
TXT = PDF.with_suffix(".txt")
SHA256 = "089d3ab3846cf61311d51b86c8fdf66fc1744abff98fea66c88976795e9b08ed"
FIRST = 88  # printed page of the first chunk (the JSTOR cover is PDF page 1)
OFFSET = 87  # printed page minus PDF page
IMAGE_PAGES = {5: "78962b3c720fd7e99f92475f27bb68621124fee6c72e1e6b1b6c5f3cbcc198db"}
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
print("CORCORAN (1984; quotations; lean/mathlib/Corcoran.lean)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(CO-[DP]?\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("CO-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 16, f"{len(rows)} rows")

pages = None
if not PDF.exists():
    check("CO-Q1 the cached PDF is available", False, f"download the paper from Drive to {PDF}")
else:
    d = hashlib.sha256(PDF.read_bytes()).hexdigest()
    check("CO-Q1 the cached PDF is the one read", d == SHA256, d[:16])
    pt = shutil.which("pdftotext")
    if pt:
        pages = subprocess.run([pt, "-layout", str(PDF), "-"], capture_output=True, text=True).stdout.split("\f")
    elif TXT.exists():
        pages = TXT.read_text().split("\f")
if pages is None:
    check("CO-Q2 the text is available", False, "install pdftotext or provide the cached text")
    sys.exit(1)
check("CO-Q2 the text is Corcoran's article", "Long-Run Equilibrium" in pages[0] and len(pages) >= 6,
      f"{len(pages)} pages")

image_rows = []
for rid, page, quote in rows:
    idx = int(page) - FIRST
    if rid.startswith("CO-P"):
        image_rows.append((rid, int(page)))
        continue
    check(f"{rid} (p.{page}) is on the page verbatim", 0 <= idx < len(pages) and norm(quote) in norm(pages[idx]))

flip = "total expenditures will exceed the payoff regardless of the marginal cost"
check("CO-C1 control: CO-8 with equal replaced by exceed is not in the text", flip not in norm(" ".join(pages)))
quote8 = [q for r, p, q in rows if r == "CO-8"][0]
check("CO-C2 control: CO-8 is not on the cover page", norm(quote8) not in norm(pages[0]))

pdftoppm = shutil.which("pdftoppm")
for rid, page in image_rows:
    n = page - OFFSET
    if n not in IMAGE_PAGES:
        check(f"{rid} (p.{page}) has a pinned page image", False, f"no hash for PDF page {n}")
        continue
    img = CACHE / "corcoran_pages" / f"p-{n}.png"
    if not img.exists() and pdftoppm:
        img.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run([pdftoppm, "-r", "130", "-f", str(n), "-l", str(n), "-png", str(PDF),
                        str(img.parent / "p")], capture_output=True)
        made = sorted(img.parent.glob(f"p-*{n}.png"))
        if made and made[-1] != img:
            made[-1].rename(img)
    ok = img.exists() and hashlib.sha256(img.read_bytes()).hexdigest() == IMAGE_PAGES[n]
    check(f"{rid} (p.{page}) is read from the pinned page image p-{n}.png", ok,
          "image-only: the text layer is garbled")

ROOT = HERE.parent.parent
PROJ = ROOT / "lean" / "mathlib"
LSRC = PROJ / "Corcoran.lean"
LNS = "Corcoran1984"
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
check("CO-L2 Corcoran is a build root", '"Corcoran"' in lakefile)
check("CO-L3 no sorry in the source", re.search(r"\bsorry\b", lsrc) is None)
declared = re.findall(r"^(?:theorem|lemma)\s+([^\s:(\[{]+)", lsrc, re.M)
cited = sorted(set(re.findall(r"`([a-z][A-Za-z0-9_']*)`", claims)))
missing = [c for c in cited if c not in declared]
check("CO-L4 every Lean name cited in CLAIMS.md is declared", not missing,
      f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))
lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
b = subprocess.run([lake, "build", "Corcoran"], cwd=PROJ, capture_output=True, text=True)
check("CO-L5 lake build Corcoran succeeds", b.returncode == 0, b.stderr.strip()[-200:])
names = [f"{LNS}.{d}" for d in declared]
with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=PROJ, delete=False) as fh:
    fh.write("import Corcoran\n")
    for n in names:
        fh.write(f"#print axioms {n}\n")
    tmp = fh.name
pr = subprocess.run([lake, "env", "lean", tmp], cwd=PROJ, capture_output=True, text=True)
os.unlink(tmp)
used = parse_axioms(pr.stdout + pr.stderr, names)
check("CO-L6 the axiom audit covers every declared theorem", len(used) == len(names), f"{len(used)} of {len(names)}")
bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
check("CO-L7 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")
check("CO-L8 the controls are present", "not_nash_of_loss" in declared and "profit0_cmp" in declared)

fails = results.count(False)
print("=" * 72)
print(f"CORCORAN SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
