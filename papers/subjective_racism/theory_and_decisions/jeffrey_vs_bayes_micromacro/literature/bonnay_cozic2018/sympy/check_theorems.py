"""
Bonnay & Cozic (2018), "Weighted averaging, Jeffrey conditioning and invariance",
Theory and Decision -- exact checks.

Every check asserts; the script prints PASS/FAIL per check and exits 0 iff all pass.
The Lean file lean/Literature/BonnayCozic.lean proves the general statements; this
script checks instances and the numbers the README quotes.  Section and definition
numbers are the paper's.
"""
import sys
from fractions import Fraction as Fr

import sympy as sp

FAILURES = []
N = 0


def check(name, ok, detail=""):
    global N
    N += 1
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}" + (f"\n         {detail}" if detail and not ok else ""))
    if not ok:
        FAILURES.append(name)


# ------------------------------------------------------------ finite machinery
def cell_mass(p, u, i):
    return sum((p[x] for x in p if u(x) == i), Fr(0))


def jeffrey(p, u, q):
    """kin: J(p)(x) = q(u x) p(x) / p(A_{u x})."""
    return {x: q[u(x)] * p[x] / cell_mass(p, u, u(x)) for x in p}


def mass(p, B):
    return sum((p[x] for x in p if B(x)), Fr(0))


def push(pi, pfine):
    out = {}
    for x, v in pfine.items():
        out[pi(x)] = out.get(pi(x), Fr(0)) + v
    return out


def wa(l, p, p2):
    return {x: (1 - l) * p[x] + l * p2[x] for x in p}


def ewa(l, p, u, xs):
    """Definition 6: Jeffrey step to the averaged cell probabilities."""
    cells = {u(x) for x in p}
    target = {i: (1 - l) * cell_mass(p, u, i) + l * xs[i] for i in cells}
    return jeffrey(p, u, target)


# a 6-atom prior and a 3-cell partition
P = {0: Fr(1, 10), 1: Fr(3, 20), 2: Fr(1, 5), 3: Fr(1, 4), 4: Fr(1, 8), 5: Fr(7, 40)}
assert sum(P.values()) == 1
U = lambda x: x % 3
Q = {0: Fr(1, 2), 1: Fr(1, 3), 2: Fr(1, 6)}

print("Section 2: Jeffrey conditioning")
J = jeffrey(P, U, Q)
for i in range(3):
    check(f"success: new cell {i} probability is q_{i}", cell_mass(J, U, i) == Q[i])
B = lambda x: x in (0, 1, 4)
rhs = sum(mass(P, lambda x, i=i: B(x) and U(x) == i) / cell_mass(P, U, i) * Q[i] for i in range(3))
check("Jeffrey's formula on events J(p)(B) = sum_i p(B|A_i) r_i (pp. 4, 6)", mass(J, B) == rhs)
check("(R') ratios within a cell are kept (atoms 0 and 3, cell 0)", J[0] / J[3] == P[0] / P[3])
check("(R) p(.|A_i) is kept", all(J[x] / cell_mass(J, U, U(x)) == P[x] / cell_mass(P, U, U(x)) for x in P))

print("Theorem 1 (=>): Jeffrey conditioning is invariant under embedding")
# refine each atom x of P into two atoms (x,0),(x,1) with an arbitrary split
split = {0: Fr(1, 3), 1: Fr(1, 2), 2: Fr(2, 5), 3: Fr(1, 7), 4: Fr(3, 4), 5: Fr(1, 9)}
Pfine = {}
for x, v in P.items():
    Pfine[(x, 0)] = v * split[x]
    Pfine[(x, 1)] = v * (1 - split[x])
pi = lambda z: z[0]
check("the coarse prior is the pushforward of the fine one", push(pi, Pfine) == P)
Jfine = jeffrey(Pfine, lambda z: U(pi(z)), Q)
check("push(J_fine) = J_coarse", push(pi, Jfine) == J)

print("Theorem 1 (<=): the paper's construction at p(B)/p(A) = m/k")
# B an atom inside A = cell 0; p(B)/p(A) = m/k
pA = cell_mass(P, U, 0)
pB = P[0]
ratio = pB / pA
m, k = ratio.numerator, ratio.denominator
r = Q[0]
# S'': k equiprobable atoms in A, m of them forming B, plus the complement
Pss = {("A", i): pA / k for i in range(k)}
Pss[("notA",)] = 1 - pA
Jss = jeffrey(Pss, lambda z: z[0] == "A", {True: r, False: 1 - r})
check("equiprobable atoms in A stay equiprobable", len({Jss[("A", i)] for i in range(k)}) == 1)
check("each gets r/k", Jss[("A", 0)] == r / k)
check("the m atoms of B get m r / k = p(B|A) r = J(p)(B)",
      sum(Jss[("A", i)] for i in range(m)) == m * r / k == J[0])

print("Definition 5 / Theorem 2 (i)=>(vii): weighted averaging")
P2 = {0: Fr(1, 6), 1: Fr(1, 6), 2: Fr(1, 12), 3: Fr(1, 3), 4: Fr(1, 12), 5: Fr(1, 6)}
assert sum(P2.values()) == 1
L = Fr(2, 5)
W = wa(L, P, P2)
check("on events: F^W(p,p')(B) = (1-l)p(B) + l p'(B)", mass(W, B) == (1 - L) * mass(P, B) + L * mass(P2, B))
P2fine = {(x, j): P2[x] * (split[x] if j == 0 else 1 - split[x]) for x in P2 for j in (0, 1)}
check("weighted averaging commutes with pushforward", push(pi, wa(L, Pfine, P2fine)) == wa(L, P, P2))

print("Definition 6 / Theorem 3: extended weighted averaging")
X = {0: Fr(3, 5), 1: Fr(1, 5), 2: Fr(1, 5)}
E = ewa(L, P, U, X)
check("on revealed cells EWA is weighted averaging",
      all(cell_mass(E, U, i) == (1 - L) * cell_mass(P, U, i) + L * X[i] for i in range(3)))
check("l = 1 gives Jeffrey conditioning", ewa(Fr(1), P, U, X) == jeffrey(P, U, X))
check("l = 0 keeps the prior", ewa(Fr(0), P, U, X) == P)
check("every atom revealed: EWA = WA", ewa(L, P, lambda x: x, P2) == wa(L, P, P2))
check("EWA commutes with pushforward", push(pi, ewa(L, Pfine, lambda z: U(pi(z)), X)) == E)

print("Section 3: the Joe/John example (p. 8)")
john_then_joe = (1 - Fr(9, 10)) * ((1 - Fr(1, 2)) * Fr(1, 2) + Fr(1, 2) * Fr(8, 10)) + Fr(9, 10) * Fr(1, 2)
joe_then_john = (1 - Fr(1, 2)) * ((1 - Fr(9, 10)) * Fr(1, 2) + Fr(9, 10) * Fr(1, 2)) + Fr(1, 2) * Fr(8, 10)
check("John then Joe = .515", john_then_joe == Fr(515, 1000))
check("Joe then John = .65", joe_then_john == Fr(65, 100))
p0, xa, xb, la, lb = sp.symbols("p x y l_a l_b")
gap = ((1 - lb) * ((1 - la) * p0 + la * xa) + lb * xb) - ((1 - la) * ((1 - lb) * p0 + lb * xb) + la * xa)
check("two-step gap = l_a l_b (y - x)", sp.expand(gap - la * lb * (xb - xa)) == 0)
check("Joe/John instance: .65 - .515 = .9 x .5 x (.8 - .5)",
      joe_then_john - john_then_joe == Fr(9, 10) * Fr(1, 2) * (Fr(8, 10) - Fr(1, 2)))

print("p. 12: weighted averaging does not preserve independence")
l, a, b, a2, b2 = sp.symbols("lambda a b a2 b2")
T = lambda A, Bb: {(0, 0): A * Bb, (0, 1): A * (1 - Bb), (1, 0): (1 - A) * Bb, (1, 1): (1 - A) * (1 - Bb)}
T1, T2 = T(a, b), T(a2, b2)
mix = {kk: (1 - l) * T1[kk] + l * T2[kk] for kk in T1}
assoc = sp.expand(mix[(0, 0)] * mix[(1, 1)] - mix[(0, 1)] * mix[(1, 0)])
check("assoc(mixture) = l(1-l)(a-a')(b-b')", sp.expand(assoc - l * (1 - l) * (a - a2) * (b - b2)) == 0)

print("Consequences used in the discussion (not claims of the paper)")
# 2x2, rows = A (credential), columns = B (letter); prior with marginals alpha, beta and covariance c
al, be, c, q0, r0, w, lam, mu = sp.symbols("alpha beta c q0 r0 omega lambda mu")


def prior(cc):
    return {(0, 0): al * be + cc, (0, 1): al * (1 - be) - cc, (1, 0): (1 - al) * be - cc,
            (1, 1): (1 - al) * (1 - be) + cc}


rowA = lambda z: z[0]
colB = lambda z: z[1]


def ewa_sym(l_, P_, u_, xs):
    cells = {u_(z) for z in P_}
    cm = {i: sum(P_[z] for z in P_ if u_(z) == i) for i in cells}
    t = {i: (1 - l_) * cm[i] + l_ * xs[i] for i in cells}
    return {z: sp.simplify(t[u_(z)] * P_[z] / cm[u_(z)]) for z in P_}


xA = {0: q0, 1: 1 - q0}
yB = {0: r0, 1: 1 - r0}
P0 = prior(0)
AB = ewa_sym(mu, ewa_sym(lam, P0, rowA, xA), colB, yB)
BA = ewa_sym(lam, ewa_sym(mu, P0, colB, yB), rowA, xA)
check("weights that belong to the cue: EWA sequences agree at c = 0, any (lambda, mu)",
      all(sp.simplify(AB[z] - BA[z]) == 0 for z in AB))
# weights that belong to the position: first cue in full, second with omega (the paper's LAD rule)
ABw = ewa_sym(w, ewa_sym(1, P0, rowA, xA), colB, yB)
BAw = ewa_sym(w, ewa_sym(1, P0, colB, yB), rowA, xA)
mA = lambda T_: sp.simplify(T_[(0, 0)] + T_[(0, 1)])
check("position weights: A-marginal gap at c = 0 is (1-omega)(alpha - q0) [LAD(i)]",
      sp.simplify((mA(BAw) - mA(ABw)) - (1 - w) * (al - q0)) == 0)
# odds ratio: every EWA step is a Jeffrey step, which rescales rows (or columns)
Pc = prior(c)
odds = lambda T_: sp.simplify(T_[(0, 0)] * T_[(1, 1)] / (T_[(0, 1)] * T_[(1, 0)]))
step = ewa_sym(lam, Pc, rowA, xA)
check("an EWA step leaves the odds ratio unchanged (so LAD(iii) holds for every weight)",
      sp.simplify(odds(step) - odds(Pc)) == 0)
# with c != 0, cue weights do not remove the sequence effect
num = {al: sp.Rational(1, 2), be: sp.Rational(1, 2), c: sp.Rational(1, 20), q0: sp.Rational(1, 5),
       r0: sp.Rational(7, 10), lam: sp.Rational(1, 2), mu: sp.Rational(1, 2)}
ABc = ewa_sym(mu, ewa_sym(lam, Pc, rowA, xA), colB, yB)
BAc = ewa_sym(lam, ewa_sym(mu, Pc, colB, yB), rowA, xA)
check("cue weights, c = 1/20: the sequences still differ (association channel)",
      any(sp.simplify((ABc[z] - BAc[z]).subs(num)) != 0 for z in ABc))

print(f"\n{N - len(FAILURES)}/{N} checks passed")
sys.exit(1 if FAILURES else 0)
