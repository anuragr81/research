"""Suite for Coate and Loury, "Will Affirmative-Action Policies Eliminate Negative
Stereotypes?" (American Economic Review 83(5), 1993). It pins the cached PDF by sha256,
extracts the two columns of each page separately with pdftotext (the pages are set in two
columns), checks each quotation verbatim (after normalising case, spacing and
punctuation) on the page named, against the columns read in turn or, for the abstract,
the full width of the page, runs a reversed-quote control, pins the page images of the
formula pages (PDF pages 11-16) for the rows read from images, and measures
lean/mathlib/CoateLoury.lean: a build root, it builds, no sorry, every Lean name cited in
CLAIMS.md declared, every theorem within the Mathlib axioms, and the controls present.
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
PDF = CACHE / "coate_loury_1993.pdf"
SHA256 = "a3389b46de62a8cd41284f9a2b150fa8c0e6e691411fb37b91c4f50782259405"
OFFSET = 1218  # printed page minus PDF page
WIDTH, HEIGHT, SPLIT = 516, 752, 258  # page size in points and the column split
IMAGE_PAGES = {
    11: "263c202b209ab30c90bf7efc4500ee2c8b50d96c2591ccc5bfe52687d8fd06cb",
    12: "830c791bad3971095405d6c229311a5063603714111078fc23762dccd9cb31ab",
    13: "3624ab5970f8e677b77fa9f6705505835a3ff72d004d859b09f6b60cf0c92865",
    14: "3c2139d82e50a2707b16ed042bfc56df75e23d25db03767902a427e429c21842",
    15: "5fe2772a2e1789d45d1f6d23750aa95f0c5d3996642570b147f481e1b6ed784a",
    16: "326797d8f82565e0988533688d8343c37a3ed002b2dc15964ee566011a423800",
}
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def norm(s):
    s = re.sub(r"-\s*\n\s*", "", s).lower()
    s = re.sub(r"[^a-z0-9]+", " ", s)
    return re.sub(r"\s+", " ", s).strip()


print("=" * 72)
print("COATE-LOURY 1993 (AER; quotations; lean/mathlib/CoateLoury.lean)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(CO-[DP]?\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("CO-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 40, f"{len(rows)} rows")

pt = shutil.which("pdftotext")
if not PDF.exists():
    check("CO-Q1 the cached PDF is available", False, f"download the paper from Drive to {PDF}")
    sys.exit(1)
d = hashlib.sha256(PDF.read_bytes()).hexdigest()
check("CO-Q1 the cached PDF is the one read", d == SHA256, d[:16])
if not pt:
    check("CO-Q2 pdftotext is available", False)
    sys.exit(1)


def extract(n, x=None):
    args = [pt, "-f", str(n), "-l", str(n)]
    if x is not None:
        args += ["-x", str(x), "-y", "0", "-W", str(SPLIT), "-H", str(HEIGHT)]
    return subprocess.run(args + [str(PDF), "-"], capture_output=True, text=True).stdout


npages = len(subprocess.run([pt, str(PDF), "-"], capture_output=True, text=True).stdout.split("\f")) - 1
cover = extract(1)
check("CO-Q2 the text is Coate and Loury's article",
      "Will Affirmative-Action Policies Eliminate Negative Stereotypes?" in cover and npages >= 22,
      f"{npages} pages")
cols, full = {}, {}


def page_text(n):
    if n not in cols:
        cols[n] = norm(extract(n, 0) + "\n" + extract(n, SPLIT))
        full[n] = norm(extract(n))
    return cols[n], full[n]


image_rows = []
for rid, page, quote in rows:
    if rid.startswith("CO-P"):
        image_rows.append((rid, int(page)))
        continue
    n = int(page) - OFFSET
    if not 2 <= n <= npages:
        check(f"{rid} (p.{page}) is on the page verbatim", False, "page out of range")
        continue
    c, f = page_text(n)
    q = norm(quote)
    check(f"{rid} (p.{page}) is on the page verbatim", q in c or q in f)

flip = "thus the only stable equilibrium is the color blind one"
check("CO-C1 control: CO-22 with patronizing replaced by color-blind is not in the text",
      all(flip not in page_text(n)[0] and flip not in page_text(n)[1] for n in range(2, npages + 1)))
quote3 = [q for r, p, q in rows if r == "CO-3"][0]
check("CO-C2 control: CO-3 is not on p.1232", norm(quote3) not in page_text(1232 - OFFSET)[0])

pdftoppm = shutil.which("pdftoppm")
for rid, page in image_rows:
    n = page - OFFSET
    if n not in IMAGE_PAGES:
        check(f"{rid} (p.{page}) has a pinned page image", False, f"no hash for PDF page {n}")
        continue
    img = CACHE / "coate_loury_pages" / f"p-{n}.png"
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
          "image-only: the text layer garbles the formulas")

ROOT = HERE.parent.parent
PROJ = ROOT / "lean" / "mathlib"
LSRC = PROJ / "CoateLoury.lean"
LNS = "CoateLoury1993"
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
check("CO-L2 CoateLoury is a build root", '"CoateLoury"' in lakefile)
check("CO-L3 no sorry in the source", re.search(r"\bsorry\b", lsrc) is None)
declared = re.findall(r"^(?:theorem|lemma)\s+([^\s:(\[{]+)", lsrc, re.M)
cited = sorted(set(re.findall(r"`([a-z][A-Za-z0-9_']*)`", claims)))
missing = [c for c in cited if c not in declared]
check("CO-L4 every Lean name cited in CLAIMS.md is declared", not missing,
      f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))
lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
b = subprocess.run([lake, "build", "CoateLoury"], cwd=PROJ, capture_output=True, text=True)
check("CO-L5 lake build CoateLoury succeeds", b.returncode == 0, b.stderr.strip()[-200:])
names = [f"{LNS}.{d}" for d in declared]
with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=PROJ, delete=False) as fh:
    fh.write("import CoateLoury\n")
    for n in names:
        fh.write(f"#print axioms {n}\n")
    tmp = fh.name
pr = subprocess.run([lake, "env", "lean", tmp], cwd=PROJ, capture_output=True, text=True)
os.unlink(tmp)
used = parse_axioms(pr.stdout + pr.stderr, names)
check("CO-L6 the axiom audit covers every declared theorem", len(used) == len(names), f"{len(used)} of {len(names)}")
bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
check("CO-L7 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")
check("CO-L8 the controls are present",
      "iter_tendsto" in declared and "eq8_iff" in declared and "rates_fall_with_band" in declared)

fails = results.count(False)
print("=" * 72)
print(f"COATE-LOURY 1993 SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
