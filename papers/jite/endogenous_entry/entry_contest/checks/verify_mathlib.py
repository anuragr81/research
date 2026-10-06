import glob
import os
import re
import shutil
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROJ = os.path.join(ROOT, "lean", "mathlib")
ALLOWED = {"propext", "Classical.choice", "Quot.sound"}
results = []


def check(name, ok, detail=""):
    results.append((name, bool(ok)))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def declarations(path):
    names, ns = [], []
    for ln in open(path):
        m = re.match(r"\s*namespace\s+([A-Za-z0-9_.']+)", ln)
        if m:
            ns.append(m.group(1))
            continue
        m = re.match(r"\s*end\s+([A-Za-z0-9_.']+)\s*$", ln)
        if m and ns and ns[-1] == m.group(1):
            ns.pop()
            continue
        m = re.match(r"\s*(?:theorem|lemma)\s+([A-Za-z0-9_.']+)", ln)
        if m:
            names.append(".".join(ns + [m.group(1)]))
    return names


def parse_axioms(out, names):
    used = {}
    for n in names:
        m = re.search(rf"'{re.escape(n)}' (does not depend on any axioms|depends on axioms: \[([^\]]*)\])", out)
        if m:
            used[n] = set() if m.group(2) is None else {a.strip() for a in m.group(2).split(",")}
    return used


def disallowed(used):
    return {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}


print("=" * 72)
print("LEAN WITH MATHLIB (lean/mathlib/)")
print("=" * 72)

lake = shutil.which("lake")
files = sorted(glob.glob(os.path.join(PROJ, "*.lean")))
modules = [os.path.splitext(os.path.basename(f))[0] for f in files]
lakefile = open(os.path.join(PROJ, "lakefile.toml")).read()
roots = [r for block in re.findall(r"roots\s*=\s*\[([^\]]*)\]", lakefile)
         for r in re.findall(r'"([A-Za-z0-9_]+)"', block)]
toolchain = open(os.path.join(PROJ, "lean-toolchain")).read().strip()
print(f"      toolchain {toolchain}; modules {modules}; build roots {roots}")

check("ML-1 every .lean file in lean/mathlib is a build root", set(modules) <= set(roots) and modules,
      f"not built: {sorted(set(modules) - set(roots))}" if set(modules) - set(roots) else "")

if lake is None:
    check("ML-2 lake on PATH", False, "install elan; the Mathlib suite did NOT run")
elif not os.path.isdir(os.path.join(PROJ, ".lake", "packages", "mathlib", ".lake", "build")):
    check("ML-2 Mathlib build files present", False,
          "run: cd lean/mathlib && lake exe cache get && lake build")
else:
    p = subprocess.run([lake, "build"], cwd=PROJ, capture_output=True, text=True)
    check("ML-2 lake build succeeds", p.returncode == 0, (p.stdout + p.stderr).strip()[-300:])
    sources = {f: open(f).read() for f in files}
    sorry = [os.path.basename(f) for f, t in sources.items() if re.search(r"\bsorry\b", t)]
    check("ML-3 no sorry in any Mathlib source", not sorry, f"{sorry}" if sorry else "")
    names = [n for f in files for n in declarations(f)]
    with tempfile.TemporaryDirectory() as td:
        audit = os.path.join(td, "audit.lean")
        with open(audit, "w") as fh:
            for mod in modules:
                fh.write(f"import {mod}\n")
            for n in names:
                fh.write(f"#print axioms {n}\n")
        q = subprocess.run([lake, "env", "lean", audit], cwd=PROJ, capture_output=True, text=True)
    out = q.stdout + q.stderr
    used = parse_axioms(out, names)
    bad = disallowed(used)
    free = sum(1 for n in used if not used[n])
    print(f"      theorems: {len(names)}; audited: {len(used)}; axiom-free: {free}; "
          f"within propext/Classical.choice/Quot.sound: {len(used) - len(bad)}")
    for n in names:
        print(f"        {n}: {sorted(used.get(n, {'NOT AUDITED'}))}")
    check("ML-4 axiom audit covers every declared theorem", names and len(used) == len(names))
    check("ML-5 no axiom outside propext, Classical.choice and Quot.sound", not bad,
          f"{bad}" if bad else "")

print("-" * 72)
print("CONTROLS  the audit parser must reject what it is meant to reject")
sample = ("'X.a' depends on axioms: [propext, sorryAx]\n"
          "'X.b' depends on axioms: [propext, Classical.choice, Quot.sound]\n"
          "'X.c' does not depend on any axioms\n")
u = parse_axioms(sample, ["X.a", "X.b", "X.c", "X.d"])
check("ML-C1 sorryAx is flagged as disallowed", "X.a" in disallowed(u))
check("ML-C2 the three standard axioms are allowed", "X.b" not in disallowed(u) and "X.c" not in disallowed(u))
check("ML-C3 an unaudited name is detected as missing", "X.d" not in u)

nf = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"MATHLIB SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
