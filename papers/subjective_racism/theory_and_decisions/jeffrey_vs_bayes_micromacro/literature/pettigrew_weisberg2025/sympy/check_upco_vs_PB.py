"""
Pettigrew & Weisberg (2025), "Jeffrey Pooling" -- exact checks, and the project's question
of how Paper B's P^B relates to upco.

Every check asserts; the script prints PASS/FAIL per check and exits 0 iff all pass.
lean/Literature/PettigrewWeisberg.lean proves the general statements.
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


def upco(x, y):
    """Equation (1)."""
    return x * y / (x * y + (1 - x) * (1 - y))


def lin(x, y):
    return (x + y) / 2


def field(beta, x):
    """Equation (2)."""
    return beta * x / (beta * x + (1 - x))


# ---------------------------------------------------------------- 2x2 Jeffrey pooling
# keys (in E?, in F?) with 1 = yes; cells EF, EFbar, EbarF, EbarFbar
def jpool_E(P, QE, rule):
    mE1 = P[(1, 1)] + P[(1, 0)]
    new = rule(mE1, QE)
    return {k: P[k] / (mE1 if k[0] else 1 - mE1) * (new if k[0] else 1 - new) for k in P}


def jpool_F(P, RF, rule):
    mF1 = P[(1, 1)] + P[(0, 1)]
    new = rule(mF1, RF)
    return {k: P[k] / (mF1 if k[1] else 1 - mF1) * (new if k[1] else 1 - new) for k in P}


def tbl(a, b, c, d):
    return {(1, 1): a, (1, 0): b, (0, 1): c, (0, 0): d}


print("=" * 78)
print("Section 1: the opening example and Equation (1)")
print("=" * 78)
check("linear pooling of .4 and .8 is .6", lin(R(4, 10), R(8, 10)) == R(6, 10))
check("upco(.4, .8) = 8/11", upco(R(4, 10), R(8, 10)) == R(8, 11))
check("8/11 is 'about 73%'", abs(float(R(8, 11)) - 0.73) < 0.005)

print("=" * 78)
print("Worked example, p. 4 (Theorem 1 in action)")
print("=" * 78)
P = tbl(R(3, 10), R(1, 10), R(2, 10), R(4, 10))
QE, RF = R(8, 10), R(6, 10)
P1 = jpool_E(P, QE, upco)
check("P'(E) = 8/11", P1[(1, 1)] + P1[(1, 0)] == R(8, 11))
check("P' = (6/11, 2/11, 1/11, 2/11)", P1 == tbl(R(6, 11), R(2, 11), R(1, 11), R(2, 11)), str(P1))
check("P'(F) = 7/11", P1[(1, 1)] + P1[(0, 1)] == R(7, 11))
check("upco(7/11, 6/10) = 21/29", upco(R(7, 11), RF) == R(21, 29))
P2 = jpool_F(P1, RF, upco)
check("P'' = (18/29, 4/29, 3/29, 4/29)", P2 == tbl(R(18, 29), R(4, 29), R(3, 29), R(4, 29)), str(P2))
P1b = jpool_F(P, RF, upco)
check("other order: P' = (9/25, 2/25, 6/25, 8/25)", P1b == tbl(R(9, 25), R(2, 25), R(6, 25), R(8, 25)), str(P1b))
P2b = jpool_E(P1b, QE, upco)
check("other order: the same P''", P2b == P2, str(P2b))
check("proportions: 3:1:2:4 times 8*6, 8*4, 2*6, 2*4 gives 18:4:3:4",
      [3 * 48, 1 * 32, 2 * 12, 4 * 8] == [x * 8 for x in (18, 4, 3, 4)])
L1 = jpool_F(jpool_E(P, QE, lin), RF, lin)
L2 = jpool_E(jpool_F(P, RF, lin), QE, lin)
check("linear pooling does not commute on the same example (p. 4)", L1 != L2, f"{L1} vs {L2}")

print("=" * 78)
print("Theorem 1 (Field), symbolic: regular 2x2 prior, any Q(E), R(F)")
print("=" * 78)
a, b, c, qE, rF = sp.symbols('a b c q_E r_F', positive=True)
Ps = tbl(a, b, c, 1 - a - b - c)
T1 = jpool_F(jpool_E(Ps, qE, upco), rF, upco)
T2 = jpool_E(jpool_F(Ps, rF, upco), qE, upco)
check("upco-Jeffrey pooling commutes in all four cells", all(zero(T1[k] - T2[k]) for k in T1))
prod = {k: Ps[k] * (qE if k[0] else 1 - qE) * (rF if k[1] else 1 - rF) for k in Ps}
Zp = sum(prod.values())
check("... and equals P(cell) Q R renormalized", all(zero(T1[k] - prod[k] / Zp) for k in T1))

print("=" * 78)
print("Section 3: Field updating, Equations (2)-(3), and P-W's glosses")
print("=" * 78)
x, beta = sp.symbols('x beta', positive=True)
check("Equation (3): field(beta, x) = upco(x, beta/(beta+1))", zero(field(beta, x) - upco(x, beta / (beta + 1))))
check("P-W p. 7 (their gloss, not Field's): field(beta, 1/2) = beta/(beta+1)",
      zero(field(beta, R(1, 2)) - beta / (beta + 1)))
check("upco with a uniform prior returns the pooled opinion: upco(1/2, y) = y", zero(upco(R(1, 2), x) - x))
xp = field(beta, x)
check("p. 9: beta is the Bayes factor of x -> field(beta, x)", zero((xp / (1 - xp)) / (x / (1 - x)) - beta))

print("=" * 78)
print("The project's question: upco(prior, delivered credence) is not the delivered credence")
print("=" * 78)
p, q = sp.symbols('p q', positive=True)
naive = sp.factor(sp.simplify(upco(p, q) - q))
check("upco(p,q) - q = q(1-q)(2p-1)/D, nonzero in general", zero(naive * (p * q + (1 - p) * (1 - q)) - q * (1 - q) * (2 * p - 1)),
      str(naive))
check("upco(p,q) = q only at p = 1/2", sp.solve(sp.Eq(upco(p, q), q), p) == [R(1, 2)])
bfq = (q / p) / ((1 - q) / (1 - p))                       # Bayes factor of p -> q
check("Field updating on the Bayes factor of p -> q returns q", zero(field(bfq, p) - q))
QEn = sp.simplify(bfq / (bfq + 1))
check("the opinion upco needs is beta/(beta+1) = q(1-p)/(p+q-2pq)", zero(QEn - q * (1 - p) / (p + q - 2 * p * q)))
check("upco(p, beta/(beta+1)) = q", zero(upco(p, QEn) - q))
check("beta/(beta+1) = q only at p = 1/2", sp.solve(sp.Eq(QEn, q), p) == [R(1, 2)])

print("=" * 78)
print("P^B is upco-Jeffrey pooling with matched opinions, not with delivered credences")
print("=" * 78)
xs, ys = sp.symbols('x_A y_B', positive=True)
mE1 = Ps[(1, 1)] + Ps[(1, 0)]
mF1 = Ps[(1, 1)] + Ps[(0, 1)]
lA = {1: xs / mE1, 0: (1 - xs) / (1 - mE1)}
lB = {1: ys / mF1, 0: (1 - ys) / (1 - mF1)}
wPB = {k: Ps[k] * lA[k[0]] * lB[k[1]] for k in Ps}
PB = {k: wPB[k] / sum(wPB.values()) for k in Ps}
QA = lA[1] / (lA[1] + lA[0])                               # matched opinion on E
QB = lB[1] / (lB[1] + lB[0])
check("upco(P(E), matched opinion) = delivered credence x_A", zero(upco(mE1, QA) - xs))
M1 = jpool_F(jpool_E(Ps, QA, upco), QB, upco)
M2 = jpool_E(jpool_F(Ps, QB, upco), QA, upco)
check("pooling E then F with matched opinions gives P^B", all(zero(M1[k] - PB[k]) for k in Ps))
check("pooling F then E with matched opinions gives P^B", all(zero(M2[k] - PB[k]) for k in Ps))
# P-W's own numbers read as delivered credences
mE1n, mF1n = R(4, 10), R(5, 10)
PBn = {k: P[k] * ({1: QE / mE1n, 0: (1 - QE) / (1 - mE1n)}[k[0]])
       * ({1: RF / mF1n, 0: (1 - RF) / (1 - mF1n)}[k[1]]) for k in P}
PBn = {k: v / sum(PBn.values()) for k, v in PBn.items()}
check("on the p. 4 numbers, P^B(EF) = 27/40", PBn[(1, 1)] == R(27, 40), str(PBn[(1, 1)]))
check("... while pooling the credences themselves gives 18/29", P2[(1, 1)] == R(18, 29))

print("=" * 78)
print("Theorem 2's hypotheses hold for upco (so the uniqueness claim is not vacuous)")
print("=" * 78)
u = [R(1, 3)] * 3
Qv = [R(1, 2), R(1, 3), R(1, 6)]


def upcoV(Pv, Qv):
    S = sum(pi * qi for pi, qi in zip(Pv, Qv))
    return [pi * qi / S for pi, qi in zip(Pv, Qv)]


check("uniformity preservation: upco(u, u) = u", upcoV(u, u) == u)
check("uniform is neutral (Lemma 7's conclusion): upco(u, Q) = Q", upcoV(u, Qv) == Qv)
check("symmetry: upco(P, Q) = upco(Q, P)", upcoV(Qv, [R(1, 5), R(1, 5), R(3, 5)]) == upcoV([R(1, 5), R(1, 5), R(3, 5)], Qv))
check("regularity preservation (the added hypothesis): regular inputs give a regular output",
      all(v > 0 for v in upcoV(Qv, [R(1, 5), R(1, 5), R(3, 5)])))

print()
print(f"-> {N - len(FAILURES)}/{N} checks passed")
if FAILURES:
    print("-> FAILURES: " + ", ".join(FAILURES))
sys.exit(0 if not FAILURES else 1)
