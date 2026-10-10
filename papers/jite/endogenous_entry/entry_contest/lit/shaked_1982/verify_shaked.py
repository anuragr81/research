"""Suite for Shaked (1982), read in full from page images (no text layer, no OCR
available). It pins the cached PDF by sha256, checks the quotation rows (each
with a page inside 310-320), and measures lean/mathlib/ShakedDispersive.lean:
a build root, it builds, no sorry, every Lean name cited in CLAIMS.md declared,
every theorem within the Mathlib axioms, and the two controls present.
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
PDF = pathlib.Path.home() / ".cache" / "entry_contest" / "shaked_1982.pdf"
SHA256 = "07d1c43f7b4e7ac857c40d801c273e7cae560a99a57f2ba5841537cccf518fd6"
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


print("=" * 72)
print("SHAKED 1982 (page images; lean/mathlib/ShakedDispersive.lean)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(SH-\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("SH-Q0 every quotation row has an ID, a page and a quotation", len(rows) >= 5, f"{len(rows)} rows")
check("SH-Q1 every page lies in 310-320", all(310 <= int(p) <= 320 for _, p, _ in rows))
if not PDF.exists():
    check("SH-Q2 the cached PDF is available", False, f"download dispersive_ordering_shaked.pdf from Drive to {PDF}")
else:
    d = hashlib.sha256(PDF.read_bytes()).hexdigest()
    check("SH-Q2 the cached PDF is the one read", d == SHA256, d[:16])

ROOT = HERE.parent.parent
PROJ = ROOT / "lean" / "mathlib"
LSRC = PROJ / "ShakedDispersive.lean"
LNS = "Shaked1982"
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
check("SH-L2 ShakedDispersive is a build root", '"ShakedDispersive"' in lakefile)
check("SH-L3 no sorry in the source", re.search(r"\bsorry\b", lsrc) is None)
declared = re.findall(r"^theorem\s+([^\s:(\[{]+)", lsrc, re.M)
cited = sorted(set(re.findall(r"`([a-z][A-Za-z0-9_']*)`", claims)))
missing = [c for c in cited if c not in declared]
check("SH-L4 every Lean name cited in CLAIMS.md is declared", not missing,
      f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))
lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
b = subprocess.run([lake, "build", "ShakedDispersive"], cwd=PROJ, capture_output=True, text=True)
check("SH-L5 lake build ShakedDispersive succeeds", b.returncode == 0, b.stderr.strip()[-200:])
names = [f"{LNS}.{d}" for d in declared]
with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=PROJ, delete=False) as fh:
    fh.write("import ShakedDispersive\n")
    for n in names:
        fh.write(f"#print axioms {n}\n")
    tmp = fh.name
pr = subprocess.run([lake, "env", "lean", tmp], cwd=PROJ, capture_output=True, text=True)
os.unlink(tmp)
used = parse_axioms(pr.stdout + pr.stderr, names)
check("SH-L6 the axiom audit covers every declared theorem", len(used) == len(names), f"{len(used)} of {len(names)}")
bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
check("SH-L7 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")
check("SH-L8 the controls are present",
      "control_not_monotone" in declared and "control_down_crossing" in declared)

fails = results.count(False)
print("=" * 72)
print(f"SHAKED SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
