import glob
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
PROJ = os.path.join(HERE, "lean")
SOURCE = os.path.join(ROOT, "lean", "mathlib")
MANUSCRIPT = os.path.join(ROOT, "MANUSCRIPT.tex")
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
        m = re.match(r"\s*(?:theorem|lemma)\s+([^\s:(\[{]+)", ln)
        if m:
            names.append(".".join(ns + [m.group(1)]))
    return names


def cited(tex):
    names = []
    for ln in tex.splitlines():
        if ln.startswith(r"\mrow{"):
            m = re.search(r"\{([^{}]*)\}\s*$", ln)
            if m:
                names += [n.strip() for n in m.group(1).split(",") if n.strip()]
    names += [n for n in re.findall(r"\\lean\{([A-Za-z0-9_.']+)\}", tex)]
    return names


def drifted(proj, source):
    return [os.path.basename(f) for f in sorted(glob.glob(os.path.join(proj, "*.lean")))
            if open(f, "rb").read() != open(os.path.join(source, os.path.basename(f)), "rb").read()]


print("=" * 72)
print("THEORY PAPER (theory/lean/)")
print("=" * 72)

files = sorted(glob.glob(os.path.join(PROJ, "*.lean")))
declared = {n for f in files for n in declarations(f)}
names = cited(open(MANUSCRIPT).read())
missing = sorted(set(names) - declared)
print(f"      files {[os.path.basename(f) for f in files]}")
print(f"      theorems declared {len(declared)}; Lean names cited by MANUSCRIPT.tex {len(set(names))}")
check("TH-1 every theory file is identical to its source in lean/mathlib",
      files and not drifted(PROJ, SOURCE), f"{drifted(PROJ, SOURCE)}")
check("TH-2 every Lean name cited by MANUSCRIPT.tex is declared in theory/lean",
      names and not missing, f"missing: {missing}" if missing else "")

print("-" * 72)
print("CONTROLS")
check("TH-C1 a fabricated citation is detected as missing",
      "RateBased.no_such_theorem" not in declared)
check("TH-C2 a row's last brace group is read as its Lean names",
      cited(r"\mrow{M0}{a {b} c}{$x^{2}$}{A.one, B.two}") == ["A.one", "B.two"])

print("-" * 72)
p = subprocess.run([sys.executable, os.path.join(ROOT, "checks", "verify_mathlib.py"), PROJ])
check("TH-3 build, sorry and axiom audit of theory/lean (checks/verify_mathlib.py)", p.returncode == 0)

nf = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"THEORY SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
