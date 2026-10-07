"""Suite for Lewis & Thompson (1981). Every claim is checked in Lean with Mathlib, in
lean/mathlib/LewisThompson.lean. This suite measures that file: it is a build root, it
builds, it has no sorry, every Lean name that CLAIMS.md cites is declared, and every declared
theorem depends only on propext, Classical.choice and Quot.sound."""

import os
import re
import shutil
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
PROJ = os.path.join(ROOT, "lean", "mathlib")
SRC = os.path.join(PROJ, "LewisThompson.lean")
NS = "LewisThompson"
ALLOWED = {"propext", "Classical.choice", "Quot.sound"}
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def parse_axioms(out, names):
    used = {}
    for n in names:
        m = re.search(rf"'{re.escape(n)}' (does not depend on any axioms|depends on axioms: \[([^\]]*)\])", out)
        if m:
            used[n] = set() if m.group(2) is None else {a.strip() for a in m.group(2).split(",")}
    return used


print("=" * 72)
print("LEWIS-THOMPSON 1981 (lean/mathlib/LewisThompson.lean)")
print("=" * 72)

src = open(SRC).read()
lakefile = open(os.path.join(PROJ, "lakefile.toml")).read()
check("LT-L1 LewisThompson is a build root", '"LewisThompson"' in lakefile)
check("LT-L2 no sorry in the source", re.search(r"\bsorry\b", src) is None)

declared = re.findall(r"^theorem\s+([^\s:(\[{]+)", src, re.M)
claims = open(os.path.join(HERE, "CLAIMS.md")).read()
cited = sorted(set(re.findall(r"`([a-z][A-Za-z0-9_']*)`", claims)))
missing = [c for c in cited if c not in declared]
check("LT-L3 every Lean name cited in CLAIMS.md is declared", not missing,
      f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))

lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
b = subprocess.run([lake, "build", "LewisThompson"], cwd=PROJ, capture_output=True, text=True)
check("LT-L4 lake build LewisThompson succeeds", b.returncode == 0, b.stderr.strip()[-200:])

names = [f"{NS}.{d}" for d in declared]
with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=PROJ, delete=False) as fh:
    fh.write("import LewisThompson\n")
    for n in names:
        fh.write(f"#print axioms {n}\n")
    tmp = fh.name
p = subprocess.run([lake, "env", "lean", tmp], cwd=PROJ, capture_output=True, text=True)
os.unlink(tmp)
used = parse_axioms(p.stdout + p.stderr, names)
check("LT-L5 the axiom audit covers every declared theorem", len(used) == len(names),
      f"{len(used)} of {len(names)}")
bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
check("LT-L6 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")

sample = ("'X.a' depends on axioms: [propext, sorryAx]\n"
          "'X.b' depends on axioms: [propext, Classical.choice, Quot.sound]\n"
          "'X.c' does not depend on any axioms\n")
u = parse_axioms(sample, ["X.a", "X.b", "X.c", "X.d"])
check("LT-C1 control: sorryAx is flagged", "sorryAx" in u.get("X.a", set()) and bool(u["X.a"] - ALLOWED))
check("LT-C2 control: an unaudited name is detected", "X.d" not in u)

fails = results.count(False)
print("=" * 72)
print(f"LEWIS-THOMPSON SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
