import os
import re
import shutil
import subprocess
import tempfile

import sympy as sp

results = []


def check(name, ok, detail=""):
    results.append((name, bool(ok)))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


HERE = os.path.dirname(os.path.abspath(__file__))
NS = "Schoemaker1982"
SRC = os.path.join(HERE, "Schoemaker1982.lean")
ALLOWED = {"propext", "Quot.sound"}

print("=" * 72)
print("Schoemaker (1982), JEL 20(2):529-563. Checks of OUR READING.")
print("=" * 72)

print("SCH-1  EU under aU+b equals a*EU+b when probabilities sum to one, p.531")
p1, p2, p3, u1, u2, u3, a, b = sp.symbols("p1 p2 p3 u1 u2 u3 a b", real=True)
eu = p1 * u1 + p2 * u2 + p3 * u3
eu_t = p1 * (a * u1 + b) + p2 * (a * u2 + b) + p3 * (a * u3 + b)
check("SCH-1a identity, symbolic", sp.simplify(eu_t - (a * eu + b * (p1 + p2 + p3))) == 0)
check("SCH-1b with p1+p2+p3=1 the constant is b",
      sp.simplify(eu_t.subs(p3, 1 - p1 - p2) - (a * eu.subs(p3, 1 - p1 - p2) + b)) == 0)

print("SCH-2  Control: a monotone non-affine map reverses an EU ranking")
U = {0: 0, 3: 3, 5: 10}
lotA = [(sp.Rational(1, 2), 0), (sp.Rational(1, 2), 5)]
lotB = [(1, 3)]
eA = sum(w * o for w, o in lotA)
eB = sum(w * o for w, o in lotB)
fA = sum(w * U[o] for w, o in lotA)
fB = sum(w * U[o] for w, o in lotB)
check("SCH-2 ranking A<B under identity, B<A after the stretch", eA < eB and fB < fA,
      f"EU {eA} vs {eB}; after stretch {fA} vs {fB}")

print("SCH-4  Ratios of utility differences invariant under aU+b, p.533")
d = (a * u1 + b - (a * u2 + b)) / (a * u3 + b - (a * u2 + b))
check("SCH-4 ratio of differences unchanged", sp.simplify(d - (u1 - u2) / (u3 - u2)) == 0)

print("SCH-L  Lean: affine invariance of EU rankings and difference order; controls")
lean = shutil.which("lean")
if lean is None:
    check("SCH-L lean on PATH", False, "lean not on PATH")
else:
    text = open(SRC).read()
    p = subprocess.run([lean, SRC], capture_output=True, text=True)
    check("SCH-L Schoemaker1982.lean compiles with no errors", p.returncode == 0,
          (p.stdout + p.stderr).strip()[:200])
    check("SCH-L no sorry in the source", re.search(r"\bsorry\b", text) is None)
    names = re.findall(r"^theorem\s+([A-Za-z0-9_']+)", text, re.M)
    with tempfile.TemporaryDirectory() as td:
        audit = os.path.join(td, "audit.lean")
        with open(audit, "w") as fh:
            fh.write(text)
            fh.write("\n")
            for n in names:
                fh.write(f"#print axioms {NS}.{n}\n")
        q = subprocess.run([lean, audit], capture_output=True, text=True)
    out = q.stdout + q.stderr
    used = {}
    for n in names:
        m = re.search(rf"'{NS}\.{re.escape(n)}' (does not depend on any axioms|depends on axioms: \[([^\]]*)\])", out)
        if m:
            used[n] = set() if m.group(2) is None else {x.strip() for x in m.group(2).split(",")}
    free = sum(1 for n in used if not used[n])
    bad = {n: sorted(x - ALLOWED) for n, x in used.items() if x - ALLOWED}
    print(f"      theorems: {len(names)}; audited: {len(used)}; axiom-free: {free}; "
          f"on propext/Quot.sound only: {len(used) - free - len(bad)}")
    check("SCH-L axiom audit covers every declared theorem", len(names) > 0 and len(used) == len(names))
    check("SCH-L no axiom outside propext and Quot.sound", not bad, str(bad) if bad else "")

nf = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"SCHOEMAKER 1982 SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our reading is arithmetically consistent with the source and its")
print("formal steps are machine-checked. This does not reprove the paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
