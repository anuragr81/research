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
print(f"      theorems declared {len(declared)}; Lean names cited by the parent MANUSCRIPT.tex {len(set(names))}")
check("TH-1 every theory file is identical to its source in lean/mathlib",
      files and not drifted(PROJ, SOURCE), f"{drifted(PROJ, SOURCE)}")
check("TH-2 every Lean name cited by the parent MANUSCRIPT.tex is declared in theory/lean",
      names and not missing, f"missing: {missing}" if missing else "")

THEORY_TEX = os.path.join(HERE, "wealth_garch_theory.tex")
LIT = os.path.join(ROOT, "lit")


def lit_lean(lit):
    """Map each bib key in lit/*/KEYS to the Lean files in that record's LEAN."""
    out = {}
    for d in sorted(glob.glob(os.path.join(lit, "*", "KEYS"))):
        rec = os.path.dirname(d)
        lean = os.path.join(rec, "LEAN")
        files = open(lean).read().split() if os.path.exists(lean) else []
        for k in open(d).read().split():
            out[k] = files
    return out


def cited_keys(tex):
    return sorted({k.strip() for g in re.findall(r"\\cite[pt]?\{([^}]*)\}", tex)
                   for k in g.split(",") if k.strip() and not k.strip().startswith("#")}
                  | set(re.findall(r"^\\lrow\{[^{}]*\}\{([^{}]*)\}", tex, re.M)))


keys = cited_keys(open(THEORY_TEX).read())
lmap = lit_lean(LIT)
present = {os.path.basename(f) for f in files}
gaps = [k for k in keys if not lmap.get(k) or any(f not in present for f in lmap[k])]
print(f"      papers cited by theory/wealth_garch_theory.tex {keys}")
check("TH-4 every paper cited by theory/wealth_garch_theory.tex has a lit record whose Lean file is in theory/lean",
      keys and not gaps, f"gaps: {gaps}" if gaps else "")

tnames = [n for n in cited(open(THEORY_TEX).read()) if "#" not in n]
tmissing = sorted(set(tnames) - declared)
print(f"      Lean names cited by theory/wealth_garch_theory.tex {len(set(tnames))}")
check("TH-5 every Lean name cited by theory/wealth_garch_theory.tex is declared in theory/lean",
      tnames and not tmissing, f"missing: {tmissing}" if tmissing else "")

print("-" * 72)
print("CONTROLS")
check("TH-C1 a fabricated citation is detected as missing",
      "RateBased.no_such_theorem" not in declared)
check("TH-C2 a row's last brace group is read as its Lean names",
      cited(r"\mrow{M0}{a {b} c}{$x^{2}$}{A.one, B.two}") == ["A.one", "B.two"])

check("TH-C3 a cited paper without a lit record is a gap",
      not lmap.get("nosuchpaper1999") and cited_keys("\\citet{a1999, b2000}\\citep{c2001}\n\\lrow{L1}{d2002}{x}") == ["a1999", "b2000", "c2001", "d2002"])

print("-" * 72)
p = subprocess.run([sys.executable, os.path.join(ROOT, "checks", "verify_mathlib.py"), PROJ])
check("TH-3 build, sorry and axiom audit of theory/lean (checks/verify_mathlib.py)", p.returncode == 0)

nf = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"THEORY SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
