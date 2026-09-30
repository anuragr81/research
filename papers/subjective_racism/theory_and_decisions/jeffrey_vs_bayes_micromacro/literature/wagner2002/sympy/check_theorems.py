"""
Wagner (2002), "Probability Kinematics and Commutativity" -- exact checks.

Every check asserts; the script prints PASS/FAIL per check and exits 0 iff all pass.
The Lean file lean/Literature/Wagner2002.lean proves the general finite-partition
statements; this script checks instances and the numbers the README quotes.

Schema (3.1): route 1 is p -E-> q -F-> r, route 2 is p -F-> q' -E-> r'.  Identities
(3.2) beta_{r',q'}(E_i1:E_i2) = beta_{q,p}(E_i1:E_i2) and (3.3)
beta_{q',p}(F_j1:F_j2) = beta_{r,q}(F_j1:F_j2) (primes read on the rendered p. 4; the
text layer drops them).
"""
import sys

import sympy as sp

R = sp.Rational
FAILURES = []
N = 0


def check(name, ok, detail=""):
    global N
    N += 1
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}" + (f"\n         {detail}" if detail and not ok else ""))
    if not ok:
        FAILURES.append(name)


def zero(e):
    return sp.simplify(sp.together(e)) == 0


# ---------------------------------------------------------------- 2x2 machinery
# keys are (in E?, in F?) with 1 = yes
a, b, c = sp.symbols('alpha beta c', positive=True)
p_sym = {(1, 1): a * b + c, (1, 0): a * (1 - b) - c, (0, 1): (1 - a) * b - c, (0, 0): (1 - a) * (1 - b) + c}


def kin_E(P, e1):
    pE1 = P[(1, 1)] + P[(1, 0)]
    pE0 = P[(0, 1)] + P[(0, 0)]
    return {k: P[k] / (pE1 if k[0] == 1 else pE0) * (e1 if k[0] == 1 else 1 - e1) for k in P}


def kin_F(P, f1):
    pF1 = P[(1, 1)] + P[(0, 1)]
    pF0 = P[(1, 0)] + P[(0, 0)]
    return {k: P[k] / (pF1 if k[1] == 1 else pF0) * (f1 if k[1] == 1 else 1 - f1) for k in P}


def margE(P):
    return P[(1, 1)] + P[(1, 0)], P[(0, 1)] + P[(0, 0)]


def margF(P):
    return P[(1, 1)] + P[(0, 1)], P[(1, 0)] + P[(0, 0)]


def bf(x1, x0, y1, y0):
    """beta_{x,y}(cell1 : cell0) = (x1/x0)/(y1/y0)."""
    return (x1 / x0) / (y1 / y0)


def benchmark(P, e1, g1):
    """Paper B's P^B: P(i,j) * (x_i/P(E_i)) * (y_j/P(F_j)), normalized."""
    pE1, pE0 = margE(P)
    pF1, pF0 = margF(P)
    w = {k: P[k] * ((e1 / pE1) if k[0] == 1 else ((1 - e1) / pE0))
         * ((g1 / pF1) if k[1] == 1 else ((1 - g1) / pF0)) for k in P}
    Z = sum(w.values())
    return {k: w[k] / Z for k in w}


e1, f1, g1, h1 = sp.symbols('e1 f1 g1 h1', positive=True)

print("=" * 78)
print("Theorem 3.1 (sufficiency), symbolic 2x2 prior (alpha, beta, c)")
print("=" * 78)
q = kin_E(p_sym, e1)
qp = kin_F(p_sym, g1)
pE1, pE0 = margE(p_sym)
pF1, pF0 = margF(p_sym)
qF1, qF0 = margF(q)
qpE1, qpE0 = margE(qp)
# (3.2) solved for h1 (route 2's second-step target), (3.3) solved for f1
h1_bf = sp.solve(sp.Eq(bf(h1, 1 - h1, qpE1, qpE0), bf(e1, 1 - e1, pE1, pE0)), h1)
f1_bf = sp.solve(sp.Eq(bf(g1, 1 - g1, pF1, pF0), bf(f1, 1 - f1, qF1, qF0)), f1)
check("(3.2) has a unique solution h1", len(h1_bf) == 1, str(h1_bf))
check("(3.3) has a unique solution f1", len(f1_bf) == 1, str(f1_bf))
r_bf = kin_F(q, f1_bf[0])
rp_bf = kin_E(qp, h1_bf[0])
check("r' = r in all four cells, symbolically", all(zero(r_bf[k] - rp_bf[k]) for k in r_bf))
PB = benchmark(p_sym, e1, g1)
check("the common endpoint is Paper B's P^B (first-position cues e1, g1 against the prior)",
      all(zero(r_bf[k] - PB[k]) for k in r_bf))

print("=" * 78)
print("Theorem 4.1 (necessity), full-support numeric prior")
print("=" * 78)
subs_prior = {a: R(3, 10), b: R(11, 20), c: R(1, 20)}
pn = {k: v.subs(subs_prior) for k, v in p_sym.items()}
check("prior has full support and sums to 1", all(v > 0 for v in pn.values()) and sum(pn.values()) == 1)
e1n = R(2, 5)
qn = kin_E(pn, e1n)
qpn = kin_F(pn, g1)                 # route 2's first-step target g1 left free
rn = kin_F(qn, f1)
rpn = kin_E(qpn, h1)
sol = sp.solve([sp.Eq(rn[k], rpn[k]) for k in [(1, 1), (1, 0)]], [f1, h1], dict=True)
check("r = r' has a solution (f1, h1) for each free g1", len(sol) == 1, str(sol))
f1e, h1e = sol[0][f1], sol[0][h1]
pE1n, pE0n = margE(pn)
pF1n, pF0n = margF(pn)
qF1n, qF0n = margF(qn)
qpE1n, qpE0n = margE(qpn)
res32 = bf(h1e, 1 - h1e, qpE1n, qpE0n) - bf(e1n, 1 - e1n, pE1n, pE0n)
res33 = bf(g1, 1 - g1, pF1n, pF0n) - bf(f1e, 1 - f1e, qF1n, qF0n)
check("(3.2) holds along the whole commuting family (residual 0 in g1)", zero(res32), str(sp.simplify(res32)))
check("(3.3) holds along the whole commuting family (residual 0 in g1)", zero(res33), str(sp.simplify(res33)))
# With route 2's first step also fixed, the commuting completion is unique and equals P^B.
g1n = R(3, 5)
sol2 = sp.solve([sp.Eq(rn[k].subs(g1, g1n), rpn[k].subs(g1, g1n)) for k in [(1, 1), (1, 0)]],
                [f1, h1], dict=True)
check("with both first steps fixed, the commuting completion is unique", len(sol2) == 1, str(sol2))
rfix = {k: v.subs(sol2[0]) for k, v in rn.items()}
PBn = benchmark(pn, e1n, g1n)
check("... and it is P^B (Theorem 4.1 + (3.7), Lean PB_unique)", all(zero(rfix[k] - PBn[k]) for k in rfix))
# The family over g1 has different endpoints: Theorem 4.1 alone does not pick one out.
r_at = [sp.simplify(rn[(1, 1)].subs(f1, f1e).subs(g1, gv)) for gv in (R(1, 5), R(3, 5))]
check("different g1 give different common endpoints (a one-parameter family)", r_at[0] != r_at[1], str(r_at))

print("=" * 78)
print("Theorems 3.1/4.1 do not single out P^B (Lean wagner_does_not_single_out_PB)")
print("=" * 78)
# P-W's p. 4 prior, x = (4/5, 1/5) on E, y = (3/5, 2/5) on F.
p0 = {(1, 1): R(3, 10), (1, 0): R(1, 10), (0, 1): R(2, 10), (0, 0): R(4, 10)}
x1, y1 = R(4, 5), R(3, 5)
PB0 = benchmark(p0, x1, y1)
JAB = kin_F(kin_E(p0, x1), y1)
JBA = kin_E(kin_F(p0, y1), x1)
check("P^B(EF) = 27/40", PB0[(1, 1)] == R(27, 40), str(PB0[(1, 1)]))
check("A-first Jeffrey sequence J_AB(EF) = 27/50", JAB[(1, 1)] == R(27, 50), str(JAB[(1, 1)]))
check("B-first Jeffrey sequence J_BA(EF) = 36/55", JBA[(1, 1)] == R(36, 55), str(JBA[(1, 1)]))
# Completion: route 1 with Bayes factors read off route 2 ends at J_BA.
qpA = kin_F(p0, y1)
qpE1, qpE0 = margE(qpA)
JE1, JE0 = margE(JBA)
pE1_0, pE0_0 = margE(p0)
B = bf(JE1, JE0, qpE1, qpE0)               # beta_{r',q'}(E:Ebar)
e_c = B * pE1_0 / (B * pE1_0 + pE0_0)      # q(E) with beta_{q,p} = B
q_c = kin_E(p0, e_c)
Bf = bf(y1, 1 - y1, *margF(p0))            # beta_{q',p}(F:Fbar)
qF1c, qF0c = margF(q_c)
f_c = Bf * qF1c / (Bf * qF1c + qF0c)
r_c = kin_F(q_c, f_c)
check("route 1 built from route 2's Bayes factors satisfies (3.2)-(3.3) and ends at J_BA",
      all(zero(r_c[k] - JBA[k]) for k in r_c))
check("... so a Wagner-consistent commuting endpoint differs from P^B", JBA[(1, 1)] != PB0[(1, 1)])

print("=" * 78)
print("Section 4 opening example and Remark 4.1 (F = E)")
print("=" * 78)
# Omega = {0,1}, E = F = singletons: kinematics on E just sets the vector.
pv, qv, rv = [R(1, 2), R(1, 2)], [R(1, 4), R(3, 4)], [R(1, 2), R(1, 2)]
qpv, rpv = [R(1, 2), R(1, 2)], [R(1, 2), R(1, 2)]
check("r' = r", rv == rpv)
check("beta_{r',q'}(E0:E1) = 1", bf(rpv[0], rpv[1], qpv[0], qpv[1]) == 1)
check("beta_{q,p}(E0:E1) = 1/3, so (3.2) fails", bf(qv[0], qv[1], pv[0], pv[1]) == R(1, 3))

print("=" * 78)
print("Remark 3.1: Field's geometric-mean form (3.9)-(3.10), 3 cells")
print("=" * 78)
pc = [R(1, 5), R(3, 10), R(1, 2)]
qc = [R(1, 2), R(1, 3), R(1, 6)]
m = len(pc)
G = [sp.prod([(qc[i] / qc[k]) / (pc[i] / pc[k]) for k in range(m)]) ** R(1, m) for i in range(m)]
Zg = sum(G[i] * pc[i] for i in range(m))
check("q(E_i) = G_i p(E_i) / sum_k G_k p(E_k)", all(zero(G[i] * pc[i] / Zg - qc[i]) for i in range(m)))

print("=" * 78)
print("Remark 5.3, (5.5): the r' satisfying (3.2) is q q'/p normalized")
print("=" * 78)
qE = [R(1, 2), R(1, 3), R(1, 6)]
qpE = [R(1, 4), R(1, 4), R(1, 2)]
pE = [R(1, 3), R(1, 3), R(1, 3)]
w = [qE[i] * qpE[i] / pE[i] for i in range(3)]
rE = [wi / sum(w) for wi in w]
check("(5.5) r'(E_i) satisfies beta_{r',q'} = beta_{q,p} for all pairs",
      all(bf(rE[i], rE[k], qpE[i], qpE[k]) == bf(qE[i], qE[k], pE[i], pE[k]) for i in range(3) for k in range(3)))

print("=" * 78)
print("Note 11: a countable example where (5.4) diverges")
print("=" * 78)
i, j = sp.symbols('i j', positive=True, integer=True)
p_atom = R(7, 2) / 8 ** i
check("p sums to 1 over atoms 2i-1, 2i", sp.summation(2 * p_atom, (i, 1, sp.oo)) == 1)
check("q(E_i) = 1/2^i sums to 1", sp.summation(1 / 2 ** i, (i, 1, sp.oo)) == 1)
check("q'(F_j) = 2/3^j sums to 1", sp.summation(2 / 3 ** j, (j, 1, sp.oo)) == 1)
qF = 1 / 2 ** (2 * j - 1) + 1 / 2 ** (2 * j)
pF = 2 * p_atom.subs(i, 2 * j - 1) + 2 * p_atom.subs(i, 2 * j)
term = sp.simplify(qF * (2 / 3 ** j) / pF)
check("term j of (5.4) is (2/21)(16/3)^j", zero(term - R(2, 21) * R(16, 3) ** j), str(term))
check("so the sum in (5.4) diverges", sp.summation(R(2, 21) * R(16, 3) ** j, (j, 1, sp.oo)) == sp.oo)

print()
print(f"-> {N - len(FAILURES)}/{N} checks passed")
if FAILURES:
    print("-> FAILURES: " + ", ".join(FAILURES))
sys.exit(0 if not FAILURES else 1)
