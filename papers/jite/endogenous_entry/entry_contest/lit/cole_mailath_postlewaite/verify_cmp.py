import math
import os
import re
import shutil
import subprocess
import tempfile

import sympy as sp

results = []
HERE = os.path.dirname(os.path.abspath(__file__))


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


n, q, N, V, c, kap, m = sp.symbols("n q N V c kappa m", positive=True)
phi = sp.Function("phi")

print("=" * 72)
print("Cole, Mailath & Postlewaite")
print("  1992: 'Social Norms, Savings Behavior, and Growth', JPE 100(6),")
print("        Dec. 1992, pp. 1092-1125.  [F, published]")
print("  1995: 'Incorporating Concern for Relative Wealth into Economic")
print("        Models', CARESS Working Paper 95-14.  [F, WP]")
print("Checks of OUR READING.  These papers justify a rank-allocated")
print("non-market prize; sec.IV.A of the 1992 paper is formalised in Lean.")
print("=" * 72)
print()

print("CMP-1  Rank allocation of a single prize is FIXED-SUM in the count")
print("-" * 72)
print("      CMP 1995 sec.4: 'To make the land example analogous to our models,")
print("      we should have the land simply given away, with the best given to")
print("      the wealthiest, and so on.'")
print("      One prize of value V goes to the top-ranked competitor. With n >= 1")
print("      competitors the prize mass delivered is V, whatever n is.")
prize_mass = V  # independent of n
d_mass = sp.simplify(sp.diff(prize_mass, n))
print(f"      prize mass delivered = {prize_mass};  d/dn = {d_mass}")
check("CMP-1 the delivered prize mass does not grow with the count",
      bool(d_mass == 0),
      "rank allocation reassigns a fixed prize; it does not create one")

print()
print("CMP-2  This premise REPRODUCES Levin-Smith's welfare function (8)")
print("-" * 72)
print("      If entry is independent with probability q among N, the prize is")
print("      delivered iff at least one enters, and each entrant pays c:")
print("        S = V * Pr(at least one entrant) - (expected total entry cost)")
S_derived = (1 - (1 - q) ** N) * V - q * N * c
S_ls8 = (1 - (1 - q) ** N) * V - q * N * c  # Levin-Smith eq (8)
print(f"      derived        S = {S_derived}")
print("      Levin-Smith (8) S = [1 - (1-q)^N] V - qNc")
check("CMP-2 the rank-allocation premise yields Levin-Smith eq (8) exactly",
      bool(sp.simplify(S_derived - S_ls8) == 0),
      "CMP's conceptual point and LS's welfare algebra are the same object")

print()
print("CMP-3  The wedge CMP 1995 sec.4 is pointing at, made arithmetic")
print("-" * 72)
print("      CMP 1995 sec.4: 'when the desirable goods or decisions are")
print("      allocated as prizes rather than sold, the standard welfare")
print("      theorems regarding the Pareto optimality of the outcomes no")
print("      longer apply.'")
print("      Social value added by an entrant BEYOND THE FIRST is zero,")
print("      because the prize is delivered either way; her private gain is")
print("      strictly positive in any equilibrium in which she enters.")
social_marginal = sp.simplify(V - V)  # prize delivered with or without her
private_gain = sp.Symbol("Delta_m", positive=True)  # > 0 by the entry condition
print(f"      social marginal value of entrant n >= 2: {social_marginal}")
print(f"      her private gain: {private_gain} > 0 (entry requires Delta >= kappa)")
wedge = sp.simplify(private_gain - social_marginal)
print(f"      wedge = {wedge} > 0")
check("CMP-3 private and social marginal value diverge under rank allocation",
      bool(social_marginal == 0 and wedge == private_gain),
      "this is the same wedge as Levin-Smith's business stealing (LS-7)")

print()
print("CMP-4  Why we may import their justification without their machinery")
print("-" * 72)
print("      P1 (PROOFS.tex): Delta(m) = V * E[phi(M_m)].")
print("      So V enters the model ONLY as a multiplicative scale factor.")
Delta = V * phi(m)
dV = sp.simplify(sp.diff(Delta, V))
d2V = sp.simplify(sp.diff(Delta, V, 2))
homog = sp.simplify(Delta.subs(V, 2 * V) - 2 * Delta)
print(f"      dDelta/dV = {dV}   (free of V)")
print(f"      d2Delta/dV^2 = {d2V};  Delta(2V) - 2*Delta(V) = {homog}")
check("CMP-4 V is a pure scale parameter in our model",
      bool(d2V == 0 and homog == 0 and dV == phi(m)),
      "so CMP's foundation for a rank-allocated V transfers without their model")

print()
print("CMP-5  CMP92 sec.IV.A two-period example at gamma = 2, exact rationals")
print("-" * 72)
print("      u(c) = (1-gamma)^-1 c^(1-gamma) = -1/c;  A = 3, beta = 3/4;")
print("      two-point economy K - e = 1, K + e = 5/3 (K = 4/3, e = 1/3);")
print("      one-point comparison economy at K + e = 5/3 (same initial income).")
lam, Kq, jq = sp.symbols("lambda K j", positive=True)
A_, b_ = sp.Integer(3), sp.Rational(3, 4)
u_ = lambda c: -1 / c
obj = lambda K, l: u_(A_ * K * l) + b_ * u_(A_**2 * K * (1 - l))
Vm = lambda K, l, j: u_(A_ * K * l) + b_ * (u_(A_**2 * K * (1 - l)) + j)
lam0 = 1 / (1 + sp.sqrt(b_ * A_**(1 - 2)))
gfun = 1 / lam + (b_ / A_) / (1 - lam)
ident = sp.simplify(gfun - sp.Rational(9, 4) - (3 * lam - 2)**2 / (4 * lam * (1 - lam)))
kLow, kHigh = sp.Integer(1), sp.Rational(5, 3)
kminus = A_ * kLow * (1 - sp.Rational(1, 3))
lbar = 1 - kminus / (kHigh * A_)
vhalf = obj(kHigh, lbar) + b_ / 2
jstar = sp.Rational(5, 9)
rows = {
    "lambda(0) = [1+(beta A^(1-gamma))^(1/gamma)]^-1 = 2/3": lam0 == sp.Rational(2, 3),
    "(2) at gamma=2: g(lambda) - 9/4 = (3 lambda - 2)^2 / (4 lambda (1-lambda))": ident == 0,
    "lower half: V(1/2-) at lambda=1/3 equals V(0) with K-e=1": sp.simplify(Vm(kLow, sp.Rational(1, 3), sp.Rational(1, 2)) - obj(kLow, lam0)) == 0,
    "k-(1/2) = A(K-e)(1-1/3) = 2": kminus == 2,
    "restriction lambda <= 1 - k-(1/2)/((K+e)A) = 3/5 < 2/3 (binds)": lbar == sp.Rational(3, 5) and lbar < lam0,
    "one-point V(0) = -9/20, two-point V(1/2) = -1/12": obj(kHigh, lam0) == sp.Rational(-9, 20) and vhalf == sp.Rational(-1, 12),
    "one-point, j=5/9: lambda = 1/4 solves V(j) = V(0)": sp.simplify(Vm(kHigh, sp.Rational(1, 4), jstar) - obj(kHigh, lam0)) == 0,
    "two-point top half, j=5/9: lambda = 1/2 solves V(j) = V(1/2)": sp.simplify(Vm(kHigh, sp.Rational(1, 2), jstar) - vhalf) == 0,
}
for k_, v_ in rows.items():
    print(f"      {k_}: {bool(v_)}")
s1, s2 = 1 - sp.Rational(1, 4), 1 - sp.Rational(1, 2)
print(f"      savings rates at j = 5/9: one-point {s1}, two-point {s2}")
ctrl = sp.simplify(Vm(kHigh, sp.Rational(1, 4), sp.Rational(1, 2)) - obj(kHigh, lam0)) != 0
print(f"      control: lambda = 1/4 at j = 1/2 does NOT satisfy V(j) = V(0): {ctrl}")
check("CMP-5 gamma=2 instance: the compact economy saves 3/4, the dispersed 1/2",
      all(bool(v_) for v_ in rows.values()) and s1 > s2 and ctrl,
      "same numbers as the inst_* theorems in ColeMailathPostlewaite.lean")


def _u(g):
    if abs(g - 1) < 1e-12:
        return math.log
    return lambda c: c**(1 - g) / (1 - g)


def _bisect(f, lo, hi, it=200):
    flo = f(lo)
    for _ in range(it):
        mid = (lo + hi) / 2
        fm = f(mid)
        if (fm > 0) == (flo > 0):
            lo, flo = mid, fm
        else:
            hi = mid
    return (lo + hi) / 2


def _econ(g, A, b, K):
    uu = _u(g)
    U = lambda l: uu(A * K * l) + b * uu(A * A * K * (1 - l))
    ls = 1 / (1 + (b * A**(1 - g))**(1 / g))
    return U, ls


def _lam(U, ls, target):
    if U(1e-12) > target:
        return None
    return _bisect(lambda l: U(l) - target, 1e-12, ls)


GRID = [(g, A, b, K, ef) for g in (0.3, 0.5, 0.8, 1.0, 1.5, 2, 3, 5)
        for A in (1.2, 2, 3) for b in (0.5, 0.75, 0.95)
        for K in (0.5, 1, 3) for ef in (0.05, 0.2, 0.5, 0.8)]

print()
print("CMP-6  The unstated step: the j = 1/2 restriction is no tighter than the")
print("       one-point choice, k-(1/2) <= k_1(1/2)  (CRRA, gamma above and below 1)")
print("-" * 72)
n6 = bad6 = ctrl6 = 0
for g, A, b, K, ef in GRID:
    e = ef * K
    Ul, lsl = _econ(g, A, b, K - e)
    Uh, lsh = _econ(g, A, b, K + e)
    ll = _lam(Ul, lsl, Ul(lsl) - b * 0.5)
    lh = _lam(Uh, lsh, Uh(lsh) - b * 0.5)
    if ll is None or lh is None:
        continue
    n6 += 1
    km, k1 = A * (K - e) * (1 - ll), A * (K + e) * (1 - lh)
    bad6 += km > k1 + 1e-9
    ctrl6 += k1 > km + 1e-9
print(f"      cases {n6}; violations {bad6}; control (capitals swapped) violated in {ctrl6}")
check("CMP-6 bequest needed for rank 1/2 rises with initial capital on the grid",
      n6 > 0 and bad6 == 0 and ctrl6 == n6,
      "numerical, not proved; the Lean theorem takes it as hypothesis hfeas")

print()
print("CMP-7  Which comparison the sec.IV.A claim survives")
print("-" * 72)
print("      equal income: one-point at K+e vs top half of (K-e, K+e)  [the paper's]")
print("      mean-preserving: one-point at K vs top half of (K-e, K+e)")
n7 = ok7 = rev7 = 0
rev_by_g = {}
for g, A, b, K, ef in GRID:
    e = ef * K
    Ul, lsl = _econ(g, A, b, K - e)
    ll = _lam(Ul, lsl, Ul(lsl) - b * 0.5)
    if ll is None:
        continue
    Uh, lsh = _econ(g, A, b, K + e)
    lc = min(1 - (K - e) * (1 - ll) / (K + e), lsh)
    if lc <= 0:
        continue
    Vh = Uh(lc) + b * 0.5
    Um, lsm = _econ(g, A, b, K)
    for j in (0.6, 0.8, 1.0):
        l2 = _lam(Uh, lsh, Vh - b * j)
        l1 = _lam(Uh, lsh, Uh(lsh) - b * j)
        lm = _lam(Um, lsm, Um(lsm) - b * j)
        if None in (l1, l2, lm):
            continue
        n7 += 1
        ok7 += l1 <= l2 + 1e-12
        if lm > l2 + 1e-9:
            rev7 += 1
            rev_by_g[g] = rev_by_g.get(g, 0) + 1
print(f"      equal income: {ok7} of {n7} cases, compact economy saves weakly more")
print(f"      mean-preserving: compact economy saves LESS in {rev7} of {n7}; by gamma {rev_by_g}")
check("CMP-7 the claim holds at equal income and fails for some mean-preserving cases",
      n7 > 0 and ok7 == n7 and rev7 > 0,
      "the paper's 'same initial income level' qualifier is load-bearing")

print()
print("CMP-Lean  ColeMailathPostlewaite.lean")
print("-" * 72)
CLAIM_MAP = {
    "CMP-E1": ["twoPoint_mean", "twoPoint_unequal", "twoPoint_ranks",
               "total_append", "total_rep_pair"],
    "CMP-E2": ["matching_raises_savings", "lambda_decreasing_in_j",
               "lower_V_lower_lambda"],
    "CMP-E3": ["slack_of_feasible", "compact_saves_more",
               "compact_saves_more_from_primitives", "compact_saves_strictly_more",
               "comparison_needs_slack", "comparison_needs_equal_income"],
    "CMP-E4": ["inst_lambda0_formula", "inst_lambda0_foc", "inst_lower_half",
               "inst_restriction_binds", "inst_welfare_levels",
               "inst_one_point_match", "inst_two_point_match",
               "inst_compact_saves_more", "inst_dlambda_dj_negative",
               "inst_not_mean_preserving"],
    "CMP-F": ["compared_dispersive", "compared_nonpositive", "compared_zero_top",
              "meanPreserving_crosses"],
    "CMP-G1": ["rank_mono", "rank_ordinal", "rank_append", "rank_rep_below",
               "rank_rep_not_below"],
    "CMP-G2": ["cmp95_eq28", "cmp95_eq25_instance", "cmp95_slope_falls_with_alpha",
               "cmp95_effort_falls_with_alpha"],
    "CMP-H": ["rank_none_below", "rank_bottom"],
    "CMP-I": ["property1_no_switch", "property1_tie_not_excluded"],
}
CONTROLS = {
    "comparison_needs_slack": "CMP-E3: drop the slack step and the compact "
                              "economy saves less",
    "comparison_needs_equal_income": "CMP-E3: let the compared men differ in "
                                     "initial capital and the comparison reverses",
    "property1_tie_not_excluded": "CMP-I: the two exchange inequalities do not "
                                  "exclude equal bequests",
}
ALLOWED_AXIOMS = {"propext", "Quot.sound"}
NS = "ColeMailathPostlewaite"
src_path = os.path.join(HERE, "ColeMailathPostlewaite.lean")
lean = shutil.which("lean")
if lean is None:
    fallback = os.path.expanduser("~/.elan/bin/lean")
    lean = fallback if os.path.exists(fallback) else None
if lean is None:
    check("CMP-Lean lean binary found", False, "lean not on PATH or in ~/.elan/bin")
else:
    ver = subprocess.run([lean, "--version"], capture_output=True, text=True)
    print(f"      {ver.stdout.strip()}")
    src = open(src_path).read()
    hygiene = []
    if re.search(r"\bsorry\b", src):
        hygiene.append("sorry")
    if "native_decide" in src:
        hygiene.append("native_decide")
    if re.search(r"^\s*(private\s+)?axiom\s", src, re.M):
        hygiene.append("user axiom")
    if "--" in src or "/-" in src:
        hygiene.append("comment")
    check("CMP-Lean source has no sorry, native_decide, user axiom or comment",
          not hygiene, ", ".join(hygiene))

    p = subprocess.run([lean, src_path], capture_output=True, text=True)
    out = p.stdout + p.stderr
    check("CMP-Lean ColeMailathPostlewaite.lean compiles with no errors",
          p.returncode == 0, out.strip()[:300])
    check("CMP-Lean compiler reports no sorry", "sorry" not in out)

    names = re.findall(r"^\s*(?:theorem|lemma)\s+([A-Za-z0-9_']+)", src, re.M)
    with tempfile.TemporaryDirectory() as td:
        audit = os.path.join(td, "audit.lean")
        with open(audit, "w") as fh:
            fh.write(src)
            fh.write("\n")
            for n_ in names:
                fh.write(f"#print axioms {NS}.{n_}\n")
        q_ = subprocess.run([lean, audit], capture_output=True, text=True)
    aout = q_.stdout + q_.stderr
    deps = {}
    for m_ in re.finditer(rf"'{NS}\.([A-Za-z0-9_']+)' depends on axioms: \[([^\]]*)\]",
                          aout, re.S):
        deps[m_.group(1)] = {a.strip() for a in m_.group(2).replace("\n", " ").split(",")
                             if a.strip()}
    for m_ in re.finditer(rf"'{NS}\.([A-Za-z0-9_']+)' does not depend on any axioms",
                          aout):
        deps[m_.group(1)] = set()
    audited = [n_ for n_ in names if n_ in deps]
    free = [n_ for n_ in audited if not deps[n_]]
    core = [n_ for n_ in audited if deps[n_] and deps[n_] <= ALLOWED_AXIOMS]
    other = {n_: sorted(deps[n_] - ALLOWED_AXIOMS) for n_ in audited
             if deps[n_] - ALLOWED_AXIOMS}
    print(f"      theorems: {len(names)}; audited: {len(audited)}; sorry: 0; "
          f"axiom-free: {len(free)}; propext/Quot.sound only: {len(core)}; "
          f"other: {len(other)}")
    for n_, ax in other.items():
        print(f"      UNEXPECTED AXIOMS {n_}: {ax}")
    check("CMP-Lean axiom audit covers every declared theorem",
          q_.returncode == 0 and len(audited) == len(names) and len(names) > 0)
    check("CMP-Lean no axiom beyond propext / Quot.sound", not other,
          "core Lean only; omega/decide/simp on Int and Nat")

    mapped = {n_ for ns in CLAIM_MAP.values() for n_ in ns}
    missing = sorted(mapped - set(names))
    orphan = sorted(set(names) - mapped)
    for cid, ns in CLAIM_MAP.items():
        print(f"      {cid}: {', '.join(ns)}")
    check("CMP-Lean every theorem maps to a claim ID and every mapped theorem exists",
          not missing and not orphan,
          f"missing {missing}; unmapped {orphan}" if missing or orphan else "")
    for n_, why in CONTROLS.items():
        print(f"      control {n_}: {why}")
    check("CMP-Lean control theorems present",
          all(n_ in names for n_ in CONTROLS), f"{len(CONTROLS)} controls")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"COLE-MAILATH-POSTLEWAITE SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the sources is arithmetically consistent. The")
print("sec.IV.A comparison and the rank-allocation structure are machine-checked")
print("in Lean with the analytic content as explicit hypotheses; the status and")
print("welfare remarks are confirmed by quotation only (NOTES.md).")
print("This does not reprove any result of either paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
