"""Suite for Mathews and Namoro (working paper of 29 March 2005). It pins the
cached PDF by sha256, extracts its text with pdftotext -layout, checks each
quotation verbatim (after normalising case, spacing, punctuation and
ligatures) on the page named, runs a reversed-quote control, and measures
lean/mathlib/MathewsNamoro.lean: a build root, it builds, no sorry, every Lean
name cited in CLAIMS.md declared, every theorem within the Mathlib axioms, and
the controls present.
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
PDF = pathlib.Path.home() / ".cache" / "entry_contest" / "mathews_namoro_2008.pdf"
TXT = PDF.with_suffix(".txt")
SHA256 = "7baf2e5ce3fcd39c35a0d833898b43f52179c128ad3c697ed9c6897dc730ce95"
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
print("MATHEWS-NAMORO (working paper 2005; quotations; lean/mathlib/MathewsNamoro.lean)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(MN-D?\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("MN-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 19, f"{len(rows)} rows")

pages = None
if not PDF.exists():
    check("MN-Q1 the cached PDF is available", False, f"download the paper from Drive to {PDF}")
else:
    d = hashlib.sha256(PDF.read_bytes()).hexdigest()
    check("MN-Q1 the cached PDF is the one read", d == SHA256, d[:16])
    pt = shutil.which("pdftotext")
    if pt:
        pages = subprocess.run([pt, "-layout", str(PDF), "-"], capture_output=True, text=True).stdout.split("\f")
    elif TXT.exists():
        pages = TXT.read_text().split("\f")
if pages is None:
    check("MN-Q2 the text is available", False, "install pdftotext or provide the cached text")
    sys.exit(1)
check("MN-Q2 the text is the working paper", "Participation Incentives in Rank Order" in pages[0] and len(pages) >= 31,
      f"{len(pages)} pages")
for rid, page, quote in rows:
    p = int(page)
    check(f"{rid} (p.{page}) is on the page verbatim", 1 <= p <= len(pages) and norm(quote) in norm(pages[p - 1]))
flip = "allocations a b or c may possibly be realized but allocation d will always be realized"
check("MN-C1 control: MN-7 with never replaced by always is not in the text", flip not in norm(" ".join(pages)))
check("MN-C2 control: MN-7 is not on the first page", norm(rows[6][2]) not in norm(pages[0]))

ROOT = HERE.parent.parent
PROJ = ROOT / "lean" / "mathlib"
LSRC = PROJ / "MathewsNamoro.lean"
LNS = "MathewsNamoro2008"
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
check("MN-L2 MathewsNamoro is a build root", '"MathewsNamoro"' in lakefile)
check("MN-L3 no sorry in the source", re.search(r"\bsorry\b", lsrc) is None)
declared = re.findall(r"^(?:theorem|lemma)\s+([^\s:(\[{]+)", lsrc, re.M)
cited = sorted(set(re.findall(r"`([a-z][A-Za-z0-9_']*)`", claims)))
missing = [c for c in cited if c not in declared]
check("MN-L4 every Lean name cited in CLAIMS.md is declared", not missing,
      f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))
lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
b = subprocess.run([lake, "build", "MathewsNamoro"], cwd=PROJ, capture_output=True, text=True)
check("MN-L5 lake build MathewsNamoro succeeds", b.returncode == 0, b.stderr.strip()[-200:])
names = [f"{LNS}.{d}" for d in declared]
with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=PROJ, delete=False) as fh:
    fh.write("import MathewsNamoro\n")
    for n in names:
        fh.write(f"#print axioms {n}\n")
    tmp = fh.name
pr = subprocess.run([lake, "env", "lean", tmp], cwd=PROJ, capture_output=True, text=True)
os.unlink(tmp)
used = parse_axioms(pr.stdout + pr.stderr, names)
check("MN-L6 the axiom audit covers every declared theorem", len(used) == len(names), f"{len(used)} of {len(names)}")
bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
check("MN-L7 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")
check("MN-L8 the controls are present", "cond1_control" in declared and "cond1_printed_wrong" in declared)

fails = results.count(False)
print("=" * 72)
print(f"MATHEWS-NAMORO SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
