"""
Wagner (2003), "Commuting Probability Revisions: The Uniformity Rule" -- exact checks.

Every check asserts; the script prints PASS/FAIL per check and exits 0 iff all pass.
lean/Literature/Wagner2003.lean proves the general finite statements; this script checks
instances and the numbers the README quotes.  Schema (2.1): p -> Q -> r and p -> q -> R;
(2.2) beta^r_Q(A:B) = beta^q_p(A:B), (2.3) beta^R_q(A:B) = beta^Q_p(A:B), atoms A, B.
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


def reweight(P, w):
    unnorm = {k: P[k] * w[k] for k in P}
    Z = sum(unnorm.values())
    return {k: unnorm[k] / Z for k in unnorm}


def bfa(x, y, a, b):
    return (x[a] / x[b]) / (y[a] / y[b])


print("=" * 78)
print("Theorem 2.1 on a 4-atom algebra with no 2x2 product structure")
print("=" * 78)
atoms = ['w1', 'w2', 'w3', 'w4']
p = dict(zip(atoms, [R(1, 5), R(3, 10), R(1, 4), R(1, 4)]))
check("p is strictly coherent", all(v > 0 for v in p.values()) and sum(p.values()) == 1)
w_q = dict(zip(atoms, sp.symbols('w_q1:5', positive=True)))
w_Q = dict(zip(atoms, sp.symbols('w_Q1:5', positive=True)))
q = reweight(p, w_q)
Qm = reweight(p, w_Q)
r_ = reweight(Qm, w_q)     # r from Q with the atomic Bayes factors of p -> q  (2.2)
R_ = reweight(q, w_Q)      # R from q with the atomic Bayes factors of p -> Q  (2.3)
check("(2.2) holds by construction", all(zero(bfa(r_, Qm, x, y) - bfa(q, p, x, y)) for x in atoms for y in atoms))
check("(2.3) holds by construction", all(zero(bfa(R_, q, x, y) - bfa(Qm, p, x, y)) for x in atoms for y in atoms))
check("r = R, symbolically in all eight weights", all(zero(r_[k] - R_[k]) for k in atoms))

print("=" * 78)
print("Remark 2.2, formula (2.7): r(A) = R(A) = [q(A)Q(A)/p(A)] / sum")
print("=" * 78)
direct = {k: q[k] * Qm[k] / p[k] for k in atoms}
Zd = sum(direct.values())
check("(2.7) reproduces the schema-built r", all(zero(direct[k] / Zd - r_[k]) for k in atoms))

print("=" * 78)
print("Theorem 3.2 (via 3.1 and 2.1): two partitions of a 6-atom space, strictly coherent p")
print("=" * 78)
# atoms (e, f) with e in {0,1,2} (partition E) and f in {0,1} (partition F)
cells = [(e, f) for e in range(3) for f in range(2)]
pv = dict(zip(cells, [R(1, 12), R(1, 6), R(1, 8), R(1, 8), R(1, 4), R(1, 4)]))
check("p is strictly coherent", all(v > 0 for v in pv.values()) and sum(pv.values()) == 1)


def kin(P, lab, tgt):
    m = {}
    for k in P:
        m[lab(k)] = m.get(lab(k), 0) + P[k]
    return {k: tgt[lab(k)] * P[k] / m[lab(k)] for k in P}


def marg(P, lab):
    m = {}
    for k in P:
        m[lab(k)] = m.get(lab(k), 0) + P[k]
    return m


E = lambda k: k[0]
F = lambda k: k[1]
qv = kin(pv, E, {0: R(1, 2), 1: R(1, 3), 2: R(1, 6)})      # p -E-> q
Qv = kin(pv, F, {0: R(2, 5), 1: R(3, 5)})                   # p -F-> Q
# r from Q by kinematics on E with (3.11) beta^r_Q(E) = beta^q_p(E): targets from (5.5)
qE, pE, QE = marg(qv, E), marg(pv, E), marg(Qv, E)
wE = {i: qE[i] * QE[i] / pE[i] for i in qE}
rv = kin(Qv, E, {i: wE[i] / sum(wE.values()) for i in wE})
QF, pF, qF = marg(Qv, F), marg(pv, F), marg(qv, F)
wF = {j: QF[j] * qF[j] / pF[j] for j in QF}
Rv = kin(qv, F, {j: wF[j] / sum(wF.values()) for j in wF})
rE, RF = marg(rv, E), marg(Rv, F)
check("(3.11) beta^r_Q(E_i1:E_i2) = beta^q_p(E_i1:E_i2)",
      all(bfa(rE, QE, i, k) == bfa(qE, pE, i, k) for i in qE for k in qE))
check("(3.12) beta^R_q(F_j1:F_j2) = beta^Q_p(F_j1:F_j2)",
      all(bfa(RF, qF, i, k) == bfa(QF, pF, i, k) for i in QF for k in QF))
check("r = R on every atom", all(rv[k] == Rv[k] for k in cells))
check("second-step targets differ from first-step ones (general schema, not matched)",
      rE != qE)

print("=" * 78)
print("Theorem 4.1: (4.5) iff the three conditional identities (4.6)-(4.8)")
print("=" * 78)
# atoms HE, HEb, HbE, HbEb
at = ['HE', 'HEb', 'HbE', 'HbEb']
p4 = dict(zip(at, [R(1, 10), R(2, 10), R(3, 10), R(4, 10)]))
Q4 = dict(zip(at, [R(1, 5), R(1, 10), R(1, 5), R(1, 2)]))
q4 = dict(zip(at, [R(1, 4), R(1, 4), R(1, 4), R(1, 4)]))
# R from (2.7)-type formula makes (4.5) hold; check the three conditional identities
w4 = {k: q4[k] * Q4[k] / p4[k] for k in at}
R4 = {k: w4[k] / sum(w4.values()) for k in at}
c46 = bfa(R4, q4, 'HE', 'HEb') == bfa(Q4, p4, 'HE', 'HEb')
c47 = bfa(R4, q4, 'HbE', 'HbEb') == bfa(Q4, p4, 'HbE', 'HbEb')
c48 = bfa(R4, q4, 'HE', 'HbE') == bfa(Q4, p4, 'HE', 'HbE')
check("(4.5) gives (4.6), (4.7), (4.8)", c46 and c47 and c48)
# converse: impose only (4.6)-(4.8) and normalization, solve for R
Rs = dict(zip(at, sp.symbols('R1:5', positive=True)))
eqs = [sp.Eq(bfa(Rs, q4, 'HE', 'HEb'), bfa(Q4, p4, 'HE', 'HEb')),
       sp.Eq(bfa(Rs, q4, 'HbE', 'HbEb'), bfa(Q4, p4, 'HbE', 'HbEb')),
       sp.Eq(bfa(Rs, q4, 'HE', 'HbE'), bfa(Q4, p4, 'HE', 'HbE')),
       sp.Eq(sum(Rs.values()), 1)]
solR = sp.solve(eqs, list(Rs.values()), dict=True)
check("(4.6)-(4.8) with normalization determine R uniquely", len(solR) == 1, str(solR))
Rsol = {k: Rs[k].subs(solR[0]) for k in at}
check("that R satisfies (4.5) for all pairs of atoms",
      all(zero(bfa(Rsol, q4, x, y) - bfa(Q4, p4, x, y)) for x in at for y in at))

print("=" * 78)
print("Section 5: three indices d, D, pi; D = pi - 1")
print("=" * 78)
qq, pp = sp.symbols('q p', positive=True)
check("D = pi - 1", zero((qq - pp) / pp - (qq / pp - 1)))
# Criterion I for d and pi: Theorem 2.1 analogues
dq = {k: q[k] - p[k] for k in atoms}
dQ = {k: Qm[k] - p[k] for k in atoms}
r_d = {k: Qm[k] + dq[k] for k in atoms}
R_d = {k: q[k] + dQ[k] for k in atoms}
check("criterion I for d: r = R", all(zero(r_d[k] - R_d[k]) for k in atoms))
r_pi = {k: Qm[k] * q[k] / p[k] for k in atoms}
R_pi = {k: q[k] * Qm[k] / p[k] for k in atoms}
check("criterion I for pi: r = R", all(zero(r_pi[k] - R_pi[k]) for k in atoms))

print("=" * 78)
print("Note 4: the difference index d fails criterion II (same partition)")
print("=" * 78)
# atoms HE, HbE, HEb, HbEb (Wagner's column order); E = first two atoms
n4p = [R(4, 10), R(1, 10), R(1, 10), R(4, 10)]
n4q = [R(64, 100), R(16, 100), R(4, 100), R(16, 100)]
n4pp = [R(2, 10), R(2, 10), R(3, 10), R(3, 10)]
n4qp = [R(44, 100), R(26, 100), R(24, 100), R(6, 100)]
for nm, v in (("p", n4p), ("q", n4q), ("p'", n4pp), ("q'", n4qp)):
    check(f"{nm} sums to 1", sum(v) == 1)


def cond_H(v, E_first):
    i, j = (0, 1) if E_first else (2, 3)
    return v[i] / (v[i] + v[j])


check("q(H|E) = .8 = p(H|E)", cond_H(n4q, True) == R(4, 5) == cond_H(n4p, True))
check("q(H|Ebar) = .2 = p(H|Ebar)", cond_H(n4q, False) == R(1, 5) == cond_H(n4p, False))
cellE = [0, 0, 1, 1]
mq = [n4q[0] + n4q[1], n4q[2] + n4q[3]]
mp = [n4p[0] + n4p[1], n4p[2] + n4p[3]]
check("q comes from p by kinematics on {E, Ebar}: q(A) = q(E_i) p(A)/p(E_i) on every atom",
      all(n4q[k] == mq[cellE[k]] * n4p[k] / mp[cellE[k]] for k in range(4)))
check("q' - p' = q - p on every atom", all(n4qp[k] - n4pp[k] == n4q[k] - n4p[k] for k in range(4)))
check("q'(H|E) = 22/35", cond_H(n4qp, True) == R(22, 35), str(cond_H(n4qp, True)))
check("22/35 = .6286 to four places", round(float(R(22, 35)), 4) == 0.6286)
check("p'(H|E) = .5, so q' is not kinematical from p' on {E, Ebar}", cond_H(n4pp, True) == R(1, 2))

print("=" * 78)
print("Section 5: the pi index 'reaches the point of absurdity' at two atoms (note 5)")
print("=" * 78)
p1, q1, Q1 = sp.symbols('p1 q1 Q1', positive=True)
eq = sp.Eq(Q1 * q1 / p1 + (1 - Q1) * (1 - q1) / (1 - p1), 1)
sol = sp.solve(eq, Q1)
check("for q1 != p1 the only Q1 admitting r is Q1 = p1", sol == [p1], str(sol))
check("when q1 = p1 (no learning) every Q1 admits r = Q",
      zero(eq.lhs.subs(q1, p1) - 1))

print("=" * 78)
print("Section 5: with finitely many atoms every Bayes factor vector is realizable")
print("=" * 78)
beta = dict(zip(atoms, [R(1), R(3), R(1, 2), R(7)]))
Zb = sum(beta[k] * p[k] for k in atoms)
qb = {k: beta[k] * p[k] / Zb for k in atoms}
check("(5.5) q(A_i) = beta_i p(A_i)/sum gives beta^q_p(A_i:A_1) = beta_i",
      all(bfa(qb, p, k, 'w1') == beta[k] for k in atoms))

print()
print(f"-> {N - len(FAILURES)}/{N} checks passed")
if FAILURES:
    print("-> FAILURES: " + ", ".join(FAILURES))
sys.exit(0 if not FAILURES else 1)
