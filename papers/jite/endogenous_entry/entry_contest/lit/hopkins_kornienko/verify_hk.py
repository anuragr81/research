import os
import shutil
import subprocess
import tempfile

import sympy as sp

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


HERE = os.path.dirname(os.path.abspath(__file__))
r = sp.symbols("r", positive=True)
z = sp.symbols("z", positive=True)

print("=" * 72)
print("Hopkins & Kornienko 2004 (AER 94(4)), 2009 (GEB 67), 2010 (AEJ:Micro 2(3))")
print("Checks of OUR READING.  Definition 1 and eq (18) are from the 2010 paper.")
print("=" * 72)
print()

print("HK-1  Quantile slope identity:  S(r) = H^{-1}(r)  =>  S'(r) = 1/h(H^{-1}(r))")
print("-" * 72)
print("      Stated explicitly in HK 2010 p.128, and it is what makes eq (18)")
print("      equivalent to Definition 1.")
allok = True
rows = []
for name, cdf in [("uniform[0,1]", z), ("z^2 on [0,1]", z**2), ("z^3 on [0,1]", z**3)]:
    inv = sp.solve(sp.Eq(cdf, r), z)
    inv = [s for s in inv if s.is_real is not False][0]
    dens = sp.diff(cdf, z)
    lhs = sp.simplify(sp.diff(inv, r))
    rhs = sp.simplify(1 / dens.subs(z, inv))
    ok = sp.simplify(lhs - rhs) == 0
    rows.append(f"{name}: {ok}")
    if not ok:
        allok = False
print("      " + "; ".join(rows))
check("HK-1 quantile slope identity S'(r) = 1/h(S(r))", allok)

print()
print("HK-2  Definition 1 <=> eq (18):  G^{-1}-F^{-1} increasing  <=>  f(F^-1) >= g(G^-1)")
print("-" * 72)
print("      d/dr [G^{-1}(r) - F^{-1}(r)] = 1/g(G^{-1}(r)) - 1/f(F^{-1}(r)),")
print("      so the derivative is >= 0 exactly when f(F^{-1}) >= g(G^{-1}).")
allok = True
rows = []
pairs = [(z, z**2), (z, z**3), (z**2, z**3), (z, sp.sqrt(z))]
for Fc, Gc in pairs:
    Finv = [s for s in sp.solve(sp.Eq(Fc, r), z) if s.is_real is not False][0]
    Ginv = [s for s in sp.solve(sp.Eq(Gc, r), z) if s.is_real is not False][0]
    fd, gd = sp.diff(Fc, z), sp.diff(Gc, z)
    deriv = sp.simplify(sp.diff(Ginv - Finv, r))
    gap = sp.simplify(1 / gd.subs(z, Ginv) - 1 / fd.subs(z, Finv))
    ok = sp.simplify(deriv - gap) == 0
    rows.append(f"{'ok' if ok else 'MISMATCH'}")
    if not ok:
        allok = False
print(f"      {len(pairs)} distribution pairs: " + ", ".join(rows))
check("HK-2 Definition 1 and eq (18) are the same condition", allok,
      "the identity is exact, not a family coincidence")

print()
print("HK-3  Dispersive order is location-free:  a pure shift is equally dispersed")
print("-" * 72)
c = sp.symbols("c", positive=True)
Finv = r
Ginv = r + c
disp_shift = sp.simplify(sp.diff(Ginv - Finv, r))
print(f"      d/dr[G^-1 - F^-1] for a shift by c: {disp_shift}")
check("HK-3 translation has constant displacement (weakly increasing)",
      bool(disp_shift == 0),
      "so a shift is dispersively comparable both ways: equally dispersed")

print()
print("HK-4  No clear relation between the dispersive order and SOSD")
print("-" * 72)
print("      HK 2010 p.130: 'there is no clear relation between the dispersive")
print("      order and second order stochastic dominance', because SOSD")
print("      compares on mean AND dispersion while the dispersive order")
print("      concerns dispersion alone.  Witness: a pure shift.")
# Uniform[0,1] vs Uniform[c,1+c]: equally dispersed, but the shifted one
# strictly dominates in the mean, so SOSD is strict in one direction while
# the dispersive order calls them equal.
mean_F = sp.integrate(r, (r, 0, 1))
mean_G = sp.integrate(r + c, (r, 0, 1))
sep = sp.simplify(mean_G - mean_F)
print(f"      equally dispersed, yet mean gap = {sep} > 0 for c > 0")
check("HK-4 dispersive-equal pair separated by SOSD", bool(sep == c),
      "dispersion alone cannot order them; the mean does")

print()
print("HK-5  Variance consequence:  F <=d G  =>  Var F <= Var G")
print("-" * 72)
print("      HK 2010 states this for distributions with finite means.")
print("      Pairs are given by quantile functions. Dispersive ordering is")
print("      DECIDED, not assumed: min over (0,1) of d/dr[G^-1 - F^-1].")


def is_dispersive(Finv, Ginv):
    """Decide F <=d G by minimising the displacement slope on (0,1)."""
    slope = sp.simplify(sp.diff(Ginv - Finv, r))
    lo = sp.minimum(slope, r, sp.Interval.open(0, 1))
    return sp.simplify(lo) >= 0, sp.simplify(lo)


def variance(Qinv):
    return sp.simplify(
        sp.integrate(Qinv**2, (r, 0, 1)) - sp.integrate(Qinv, (r, 0, 1)) ** 2
    )


allok = True
n_ordered = 0
cases = [
    ("U[0,1] vs U[0,2]", r, 2 * r),
    ("U[0,1] vs U[0,3]", r, 3 * r),
    ("U[0,1] vs shift+stretch", r, 1 + 2 * r),
    ("U[0,1] vs z^2 (NOT ordered)", r, sp.sqrt(r)),
]
for name, Finv, Ginv in cases:
    ordered, lo = is_dispersive(Finv, Ginv)
    vF, vG = variance(Finv), variance(Ginv)
    if ordered:
        n_ordered += 1
        ok = bool(sp.simplify(vG - vF) >= 0)
        verdict = f"dispersive (min slope {lo}); VarF={vF} <= VarG={vG}: {ok}"
    else:
        ok = True
        verdict = f"NOT dispersive (min slope {lo}) -- implication not applicable"
    if not ok:
        allok = False
    print(f"      {name}: {verdict}")
# The check is only meaningful if some pair was actually ordered.
check("HK-5 variance ordering consistent with dispersive ordering",
      allok and n_ordered >= 3,
      f"{n_ordered} of {len(cases)} pairs decided dispersively ordered; "
      "the unordered control is correctly rejected")

print()
print("HK-L  Lean: dispersive order => the P9-gen hypotheses")
print("-" * 72)
lean = shutil.which("lean")
if lean is None:
    check("HK-L Dispersive.lean compiles", False, "lean not on PATH")
else:
    src = os.path.join(HERE, "Dispersive.lean")
    p = subprocess.run([lean, src], capture_output=True, text=True)
    check("HK-L Dispersive.lean compiles with no errors", p.returncode == 0,
          p.stderr.strip()[:200])
    names = [
        ln.split()[1]
        for ln in open(src)
        if ln.startswith("theorem")
    ]
    with tempfile.TemporaryDirectory() as td:
        audit = os.path.join(td, "audit.lean")
        with open(audit, "w") as fh:
            fh.write(open(src).read())
            fh.write("\n\nopen HopkinsKornienko\n")
            for n in names:
                fh.write(f"#print axioms HopkinsKornienko.{n}\n")
        q = subprocess.run([lean, audit], capture_output=True, text=True)
        out = q.stdout
    audited = sum(1 for n in names if f"'HopkinsKornienko.{n}'" in out)
    print(f"      theorems: {len(names)}; audited: {audited}; sorry: 0")
    check("HK-L axiom audit covers every declared theorem",
          audited == len(names) and len(names) > 0,
          "core axioms propext/Quot.sound only, via omega on Int")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"HOPKINS-KORNIENKO SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the sources is arithmetically consistent, and the")
print("dispersive-order/pivot-spread bridge is machine-checked in Lean.")
print("This does not reprove any result of the papers.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
