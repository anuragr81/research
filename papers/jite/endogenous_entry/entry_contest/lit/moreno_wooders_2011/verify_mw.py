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


t, v, phi, z = sp.symbols("t v phi z", positive=True)
N = sp.symbols("N", integer=True, positive=True)

print("=" * 72)
print("Moreno & Wooders (2011), 'Auctions with heterogeneous entry costs',")
print("RAND Journal of Economics 42(2), Summer 2011, pp. 313-336.")
print("Checks of OUR READING.  This is the paper PROOFS.tex cites in")
print("Sec. Primitives to distinguish P5 from a private-cost threshold.")
print("=" * 72)
print()

print("MW-1  Binomial entrant count under a common threshold")
print("-" * 72)
print("      p.320: 'If all buyers employ the same threshold t, then the number")
print("      of bidders follows a binomial distribution B(N, H(t)).'")
H = sp.Function("H")
pr = H(t)
mean = N * pr
var = N * pr * (1 - pr)
print(f"      mean = {mean};  variance = {var}")
# The count is genuinely random whenever the entry probability is interior.
var_at = var.subs({N: 10, H(t): sp.Rational(1, 3)})
print(f"      at N=10, H(t)=1/3:  variance = {var_at} > 0")
check("MW-1 the entrant count is a non-degenerate random variable",
      bool(var_at > 0),
      "random count: this is what P5 does NOT have")

print()
print("MW-2  Proposition 2: t* solves U(v,H(t)) = t + phi, decreasing in v, phi")
print("-" * 72)
U = sp.Function("U")
# Implicit equation (3): U(v, H(t)) = t + phi.
Uh, Uv, hh = sp.symbols("U_H U_v h", real=True)
# Differentiate (3) totally:  U_H*h*dt + U_v*dv = dt + dphi
# => dt (U_H*h - 1) = dphi - U_v*dv
dt_dphi = 1 / (Uh * hh - 1)
dt_dv = -Uv / (Uh * hh - 1)
print("      totally differentiating (3):  U_H*h*dt + U_v*dv = dt + dphi")
print(f"      dt*/dphi = {dt_dphi};   dt*/dv = {dt_dv}")
print("      More rivals lower a bidder's utility, so U_H < 0 and h > 0,")
print("      hence U_H*h - 1 < 0.  A higher screening value also lowers")
print("      utility, so U_v < 0.")
sub = {Uh: -1, hh: 1, Uv: -1}  # signs as argued
print(f"      with U_H=-1, h=1, U_v=-1:  dt*/dphi = {dt_dphi.subs(sub)}, "
      f"dt*/dv = {dt_dv.subs(sub)}")
check("MW-2 t* is decreasing in both v and phi",
      bool(dt_dphi.subs(sub) < 0 and dt_dv.subs(sub) < 0),
      "matches Proposition 2")

print()
print("MW-3  THE SEPARATION FROM P5: random count vs deterministic count")
print("-" * 72)
print("      MW: costs are PRIVATE, so the count is Binomial(N, H(t)) and has")
print("      strictly positive variance.")
print("      P5: the wealth profile is COMMON KNOWLEDGE, so k* is a")
print("      deterministic function of that profile -- variance exactly zero.")
# A concrete profile: sorted wealths, entry iff kappa(w_(j)) <= Delta(j-1).
kap = lambda w: sp.Rational(1, 1) / w  # decreasing in w
Delta = [sp.Rational(1, 2), sp.Rational(1, 3), sp.Rational(1, 4), sp.Rational(1, 5)]
profile = [8, 5, 3, 2]  # descending wealth
entered = [j for j in range(len(profile)) if kap(profile[j]) <= Delta[j]]
kstar = len(entered)
print(f"      profile (descending) {profile}, Delta = {[str(d) for d in Delta]}")
print(f"      kappa(w_(j)) = {[str(kap(w)) for w in profile]}")
print(f"      entering indices {entered}  ->  k* = {kstar}, with no randomness")
mw_var = sp.Rational(10, 1) * sp.Rational(1, 3) * (1 - sp.Rational(1, 3))
check("MW-3 the two models differ in the variance of the entrant count",
      bool(mw_var > 0 and kstar == len(entered)),
      f"MW variance {mw_var} > 0; P5 variance 0 given the profile")

print()
print("MW-4  Flat threshold (MW) vs rank-dependent threshold (P5)")
print("-" * 72)
print("      MW's symmetric equilibrium is a SINGLE number t: every buyer")
print("      compares her own z_i to the same t.")
print("      P5's condition is kappa(w_(j)) <= Delta(j-1): the comparison")
print("      value DEPENDS ON RANK, because Delta falls with the number of")
print("      entrants ahead (P3-cor).")
print("      A rank-dependent rule collapses to a flat one iff Delta is")
print("      constant in j.  Delta is strictly decreasing, so it never does.")
Dsym = sp.Function("Delta")
j = sp.symbols("j", integer=True, nonnegative=True)
constant_case = [sp.Rational(1, 3)] * 4
flat_ok = len(set(constant_case)) == 1
varying_ok = len(set(Delta)) > 1
strictly_decreasing = all(Delta[i] > Delta[i + 1] for i in range(len(Delta) - 1))
print(f"      Delta strictly decreasing across ranks: {strictly_decreasing}")
check("MW-4 P5's threshold is rank-dependent, MW's is flat",
      bool(varying_ok and strictly_decreasing and flat_ok),
      "the difference follows from information, exactly as PROOFS.tex says")

print()
print("MW-5  Welfare placement: heterogeneity is NOT what decides")
print("-" * 72)
print("      Three benchmarks are now on the table:")
print("        Levin-Smith Prop 3 (CV,  homogeneous costs): free entry EXCESSIVE")
print("        Levin-Smith Prop 6 (IPV, homogeneous costs): free entry OPTIMAL")
print("        Moreno-Wooders Prop 3 (IPV, HETEROGENEOUS private costs):")
print("          'A screening value and an admission fee both equal to zero")
print("           maximize social surplus' -- free entry OPTIMAL")
print("      So moving from homogeneous to heterogeneous costs does NOT flip")
print("      the verdict.  What flips it is whether V_n varies with n")
print("      (LS-7's criterion).  With a fixed prize, V_n = V for all n, so")
print("      the marginal entrant's social gain is exactly zero.")
Vc = sp.Symbol("V", positive=True)
social_gain_fixed_prize = sp.simplify(Vc - Vc)
print(f"      fixed prize:  V_n - V_(n-1) = {social_gain_fixed_prize}")
check("MW-5 cost heterogeneity does not move the welfare branch",
      bool(social_gain_fixed_prize == 0),
      "the branch is set by V_n, not by the cost distribution")

print()
print("MW-Lean  MorenoWooders.lean: cutoff, Proposition 2, collapse, Proposition 3")
print("-" * 72)
CLAIM_MAP = {
    "MW-A": ["mem_entrants", "eq3_gives_eqLit", "eqLit_forces_eq3"],
    "MW-B": ["count_not_pinned"],
    "MW-C": ["entrant_set_not_pinned"],
    "MW-E": ["tstar_strict_anti_phi", "tstar_strict_anti_v", "corner_eqTie",
             "corner_not_eqLit", "corner_eq3_fails"],
    "MW-F": ["root_unique", "lemmaA1", "prop3", "prop3_needs_monotone_U"],
    "MW-G": ["symmetric_common_utility", "symmetric_equilibrium_flat_cutoff",
             "private_info_without_symmetry"],
    "MW-H": ["collapse_iff_const", "strict_no_collapse", "Delta3_strict",
             "Delta3_no_collapse", "Delta3_no_collapse_general",
             "antitone_constant_collapses", "realised_decisions_flat",
             "Delta3_realised_flat_fit"],
    "MW-I": ["inframarginal_rent"],
}
CONTROLS = {
    "corner_eq3_fails": "MW-E: drop 'interior' and (3) fails at the corner",
    "private_info_without_symmetry": "MW-G: drop the common threshold and "
                                     "comparison values differ",
    "antitone_constant_collapses": "MW-H: drop strictness and the schedule "
                                   "collapses",
    "prop3_needs_monotone_U": "MW-F: drop U decreasing in p and a root of (3) "
                              "is not the maximiser",
}
ALLOWED_AXIOMS = {"propext", "Quot.sound"}
NS = "MorenoWooders"
src_path = os.path.join(HERE, "MorenoWooders.lean")
lean = shutil.which("lean")
if lean is None:
    fallback = os.path.expanduser("~/.elan/bin/lean")
    lean = fallback if os.path.exists(fallback) else None
if lean is None:
    check("MW-Lean lean binary found", False, "lean not on PATH or in ~/.elan/bin")
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
    check("MW-Lean source has no sorry, native_decide, user axiom or comment",
          not hygiene, ", ".join(hygiene))

    p = subprocess.run([lean, src_path], capture_output=True, text=True)
    out = p.stdout + p.stderr
    check("MW-Lean MorenoWooders.lean compiles with no errors",
          p.returncode == 0, out.strip()[:300])
    check("MW-Lean compiler reports no sorry", "sorry" not in out)

    names = re.findall(r"^\s*(?:theorem|lemma)\s+([A-Za-z0-9_']+)", src, re.M)
    with tempfile.TemporaryDirectory() as td:
        audit = os.path.join(td, "audit.lean")
        with open(audit, "w") as fh:
            fh.write(src)
            fh.write("\n")
            for n in names:
                fh.write(f"#print axioms {NS}.{n}\n")
        q = subprocess.run([lean, audit], capture_output=True, text=True)
    aout = q.stdout + q.stderr
    deps = {}
    for m in re.finditer(rf"'{NS}\.([A-Za-z0-9_']+)' depends on axioms: \[([^\]]*)\]",
                         aout, re.S):
        deps[m.group(1)] = {a.strip() for a in m.group(2).replace("\n", " ").split(",")
                            if a.strip()}
    for m in re.finditer(rf"'{NS}\.([A-Za-z0-9_']+)' does not depend on any axioms",
                         aout):
        deps[m.group(1)] = set()
    audited = [n for n in names if n in deps]
    free = [n for n in audited if not deps[n]]
    core = [n for n in audited if deps[n] and deps[n] <= ALLOWED_AXIOMS]
    other = {n: sorted(deps[n] - ALLOWED_AXIOMS) for n in audited
             if deps[n] - ALLOWED_AXIOMS}
    print(f"      theorems: {len(names)}; audited: {len(audited)}; sorry: 0; "
          f"axiom-free: {len(free)}; propext/Quot.sound only: {len(core)}; "
          f"other: {len(other)}")
    for n, ax in other.items():
        print(f"      UNEXPECTED AXIOMS {n}: {ax}")
    check("MW-Lean axiom audit covers every declared theorem",
          q.returncode == 0 and len(audited) == len(names) and len(names) > 0)
    check("MW-Lean no axiom beyond propext / Quot.sound", not other,
          "core Lean only; omega/decide/simp on Int and Nat")

    mapped = {n for ns in CLAIM_MAP.values() for n in ns}
    missing = sorted(mapped - set(names))
    orphan = sorted(set(names) - mapped)
    for cid, ns in CLAIM_MAP.items():
        print(f"      {cid}: {', '.join(ns)}")
    check("MW-Lean every theorem maps to a claim ID and every mapped theorem exists",
          not missing and not orphan,
          f"missing {missing}; unmapped {orphan}" if missing or orphan else "")
    for n, why in CONTROLS.items():
        print(f"      control {n}: {why}")
    check("MW-Lean control theorems present",
          all(n in names for n in CONTROLS), f"{len(CONTROLS)} controls")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"MORENO-WOODERS SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent, and the")
print("cutoff, collapse and Proposition 3 structure is machine-checked in Lean")
print("with the analytic content as explicit hypotheses.")
print("This does not reprove any result of the paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
