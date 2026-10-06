import os
import re
import shutil
import subprocess
import tempfile

import sympy as sp

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


HERE = os.path.dirname(os.path.abspath(__file__))
q = sp.symbols("q", nonnegative=True)
V, Delta, eff = sp.symbols("V Delta eff", positive=True)
M = sp.symbols("M", integer=True, positive=True)
N, alpha = sp.symbols("N alpha", positive=True)

print("=" * 72)
print("Fu, Jiao & Lu (2015), 'Contests with endogenous entry'")
print("Int J Game Theory 44:387-424.   Checks of OUR READING.")
print("eq (2), p.397:  [1 - (1-q)^M] V  >=  Mq (Delta + E(x^alpha))")
print("=" * 72)
print()

print("FJL-1  The rent bracket is a probability: 0 <= 1-(1-q)^M <= 1 on q in [0,1]")
print("-" * 72)
allok = True
rows = []
for Mv in [1, 2, 3, 5, 8]:
    br = 1 - (1 - q) ** Mv
    lo = sp.minimum(br, q, sp.Interval(0, 1))
    hi = sp.maximum(br, q, sp.Interval(0, 1))
    ok = sp.simplify(lo) == 0 and sp.simplify(hi) == 1
    rows.append(f"M={Mv}:[{lo},{hi}]")
    if not ok:
        allok = False
print("      " + "  ".join(rows))
check("FJL-1 bracket lies in [0,1], so [1-(1-q)^M] V <= V", allok,
      "this is the step the count bound needs")

print()
print("FJL-2  eq (2) implies the expected-count bound  Mq * Delta <= V")
print("-" * 72)
print("      Mq(Delta + E) <= [1-(1-q)^M] V <= V,  and E >= 0,")
print("      so Mq*Delta <= Mq(Delta + E) <= V.")
mq = sp.symbols("mq", nonnegative=True)
slack = sp.simplify((mq * (Delta + eff)) - mq * Delta)
print(f"      Mq(Delta+E) - Mq*Delta = {sp.factor(slack)}  (>= 0 since Mq,E >= 0)")
check("FJL-2 the count bound follows from eq (2)",
      bool(sp.simplify(slack - mq * eff) == 0),
      "dropping the effort-cost term is the only step; it is signed")

print()
print("FJL-3  The bound is Definition 2's feasibility set, restated")
print("-" * 72)
print("      Definition 2 (p.398): q-bar = argmax over")
print("        { q in [0,1] : (1-(1-q)^M) V >= Mq*Delta }.")
print("      Every feasible q in that set satisfies Mq*Delta <= V, because the")
print("      bracket is at most 1.  So the survey's 'Mq cannot exceed V/Delta'")
print("      is not an added claim: it is the content of Definition 2.")
print("      Decided on a grid with concrete V, Delta: every FEASIBLE (M,q)")
print("      must satisfy the count bound, and the constraint must actually")
print("      exclude some (M,q) or the check would be empty.")
allok = True
n_feasible = n_excluded = 0
Vv, Dv = sp.Integer(10), sp.Integer(1)
for Mv in [2, 3, 5, 8, 20]:
    for qv in [sp.Rational(k, 8) for k in range(1, 9)]:
        br = sp.simplify(1 - (1 - qv) ** Mv)
        feasible = bool(sp.simplify(br * Vv - Mv * qv * Dv) >= 0)
        bound_holds = bool(sp.simplify(Vv - Mv * qv * Dv) >= 0)
        if feasible:
            n_feasible += 1
            if not bound_holds:
                allok = False  # a feasible point violating the bound refutes the claim
        else:
            n_excluded += 1
print(f"      V=10, Delta=1: {n_feasible} feasible points, "
      f"{n_excluded} excluded by the constraint")
check("FJL-3 feasibility in Definition 2 forces Mq*Delta <= V",
      allok and n_feasible > 0 and n_excluded > 0,
      "no feasible point violates the bound, and the constraint is not vacuous")

print()
print("FJL-4  Shortlist cutoff  M-bar = min{ N : V/N < alpha*Delta/(alpha-1) }")
print("-" * 72)
print("      Quoted in LITERATURE.tex from Lemma 6 / Theorem 9 (p.412).")
lhs = V / N
d_lhs = sp.simplify(sp.diff(lhs, N))
thresh = alpha * Delta / (alpha - 1)
gap = sp.simplify(thresh - Delta)
lim_inf = sp.limit(thresh, alpha, sp.oo)
print(f"      d/dN (V/N) = {d_lhs}  (< 0, so the min is well defined)")
print(f"      alpha*Delta/(alpha-1) - Delta = {sp.simplify(gap)}  (> 0 for alpha > 1)")
print(f"      limit as alpha -> oo: {lim_inf}")
check("FJL-4 cutoff is well defined and exceeds Delta",
      bool(d_lhs == -V / N**2
           and sp.simplify(gap - Delta / (alpha - 1)) == 0
           and lim_inf == Delta),
      "V/N strictly falls in N; threshold exceeds Delta, tending to it as alpha grows")

print()
print("FJL-L  Lean: P7's cap and FJL's count bound as instances of one lemma")
print("-" * 72)
lean = shutil.which("lean")
if lean is None:
    check("FJL-L Accounting.lean compiles", False, "lean not on PATH")
else:
    src = os.path.join(HERE, "Accounting.lean")
    p = subprocess.run([lean, src], capture_output=True, text=True)
    check("FJL-L Accounting.lean compiles with no errors", p.returncode == 0,
          p.stderr.strip()[:200])
    text = open(src).read()
    check("FJL-L no sorry in the source", re.search(r"\bsorry\b", text) is None)
    names = re.findall(r"^theorem\s+([A-Za-z0-9_']+)", text, re.M)
    with tempfile.TemporaryDirectory() as td:
        audit = os.path.join(td, "audit.lean")
        with open(audit, "w") as fh:
            fh.write(text)
            fh.write("\n")
            for n in names:
                fh.write(f"#print axioms FuJiaoLu.{n}\n")
        q = subprocess.run([lean, audit], capture_output=True, text=True)
    out = q.stdout + q.stderr
    used = {}
    for n in names:
        m = re.search(rf"'FuJiaoLu\.{re.escape(n)}' (does not depend on any axioms|depends on axioms: \[([^\]]*)\])", out)
        if m:
            used[n] = set() if m.group(2) is None else {x.strip() for x in m.group(2).split(",")}
    free = sum(1 for n in used if not used[n])
    bad = {n: sorted(x - {"propext", "Quot.sound"}) for n, x in used.items() if x - {"propext", "Quot.sound"}}
    print(f"      theorems: {len(names)}; audited: {len(used)}; axiom-free: {free}; "
          f"on propext/Quot.sound only: {len(used) - free - len(bad)}")
    check("FJL-L axiom audit covers every declared theorem", len(names) > 0 and len(used) == len(names))
    check("FJL-L no axiom outside propext and Quot.sound", not bad, str(bad) if bad else "")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"FU-JIAO-LU SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent.")
print("This does not reprove any result of the paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
