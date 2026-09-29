"""
Domotor (1980), "Probability Kinematics and Representation of Belief Change",
Phil. Sci. 47, 384-403 -- the paper's formal claims, checked symbolically and
on the instances used in lean/Literature/Domotor.lean.

The paper has no numeric examples of its own; the checks are:
  (1) law (3), p.388: [P +_a Q]_A = P_A +_{a_A} Q_A, a_A = a P(A) : [P+Q](A);
  (2) Bayesian composition (P_A)_B = P_{A cap B}, p.390;
  (3) the Markovian iteration law, p.399 (same partition: later input wins);
  (4) non-commutativity of the Jeffrey machine, p.395/399, on the Lean instance;
  (5) an iterate of two Jeffrey steps is one Jeffrey step on the meet (p.393,
      p.395-396), symbolically;
  (6) Field's conditional, p.397: composition [P_(U,a)]_(V,b) = P_(U^V, a^b)
      and commutativity, symbolically;
  (7) p.397 clause (ii) (P +_a Q)_(U,a) = P_(U,a) +_a Q_(U,a): FALSE on the
      Lean counterexample; the corrected coefficient a Z_P/(a Z_P+(1-a) Z_Q)
      holds symbolically;
  (8) the embedding h_P(U,a) = (U,p), p(A) = e^{a_A} P(A) : a_X, p.397:
      Jeffrey on h_P(U,a) equals Field on (U,a); unit goes to P|_U; the image
      depends on P; any positive input is reached (a_A = log(p(A)/P(A)));
  (9) observation (iii), p.396: an H independent of every cell is unmoved.
Exit status 0 iff every check passes.
"""
import itertools
import sympy as sp

R = sp.Rational
ok = True


def check(name, cond):
    global ok
    print(("PASS  " if cond else "FAIL  ") + name)
    ok = ok and bool(cond)


def zero(e):
    return sp.simplify(sp.together(e)) == 0


# Sample space: 2x2 cells (a, b), a = A true, b = B true.
W = list(itertools.product([1, 0], repeat=2))
P = dict(zip(W, sp.symbols("P11 P10 P01 P00", positive=True)))
Q = dict(zip(W, sp.symbols("Q11 Q10 Q01 Q00", positive=True)))
a = sp.symbols("a", positive=True)


def mass(D, S):
    return sum(D[w] for w in S)


def cond(D, S):
    m = mass(D, S)
    return {w: (D[w] / m if w in S else 0) for w in D}


def mix(c, D1, D2):
    return {w: c * D1[w] + (1 - c) * D2[w] for w in D1}


def jeffrey(D, u, p):
    """u: world -> label; p: label -> new probability."""
    cm = {}
    for w in D:
        cm[u(w)] = cm.get(u(w), 0) + D[w]
    return {w: p[u(w)] * D[w] / cm[u(w)] for w in D}


def field(D, u, alpha):
    wts = {w: sp.exp(alpha[u(w)]) * D[w] for w in D}
    Z = sum(wts.values())
    return {w: wts[w] / Z for w in D}


A = [w for w in W if w[0] == 1]
B = [w for w in W if w[1] == 1]
AB = [w for w in A if w in B]

# (1) law (3)
M = mix(a, P, Q)
aA = a * mass(P, A) / mass(M, A)
lhs, rhs = cond(M, A), mix(aA, cond(P, A), cond(Q, A))
check("(3) p.388: [P+_aQ]_A = P_A +_{a_A} Q_A", all(zero(lhs[w] - rhs[w]) for w in W))

# (2)
l2, r2 = cond(cond(P, A), B), cond(P, AB)
check("p.390: (P_A)_B = P_{A cap B}", all(zero(l2[w] - r2[w]) for w in W))

# (3) Markov law on the partition {A, notA}
x, y = sp.symbols("x y", positive=True)
uA = lambda w: w[0]
uB = lambda w: w[1]
two = lambda t: {1: t, 0: 1 - t}
l3, r3 = jeffrey(jeffrey(P, uA, two(x)), uA, two(y)), jeffrey(P, uA, two(y))
check("p.399 Markovian iteration: (A,x) then (A,y) = (A,y)", all(zero(l3[w] - r3[w]) for w in W))

# (4) instance of non-commutativity
P0 = {(1, 1): R(2, 5), (1, 0): R(1, 10), (0, 1): R(1, 10), (0, 0): R(2, 5)}
s1 = jeffrey(jeffrey(P0, uA, two(R(4, 5))), uB, two(R(1, 5)))
s2 = jeffrey(jeffrey(P0, uB, two(R(1, 5))), uA, two(R(4, 5)))
check("non-commutativity instance: A-then-B = (16/85, 2/5, 1/85, 2/5)",
      [s1[w] for w in W] == [R(16, 85), R(2, 5), R(1, 85), R(2, 5)])
check("non-commutativity instance: B-then-A = (2/5, 2/5, 1/85, 16/85)",
      [s2[w] for w in W] == [R(2, 5), R(2, 5), R(1, 85), R(16, 85)])
check("the two orders differ (p.395: commutativity 'fails in general')", s1 != s2)

# (5) iterate = one Jeffrey step on the meet, with the iterate's own cell masses.
# Use 8 worlds (A, B, C) so that each cell of the meet {A,notA}^{B,notB} holds
# two atoms and the claim is not trivial.
W8 = list(itertools.product([1, 0], repeat=3))
P8 = dict(zip(W8, sp.symbols("R0:8", positive=True)))
it = jeffrey(jeffrey(P8, lambda w: w[0], two(x)), lambda w: w[1], two(y))
meet = lambda w: (w[0], w[1])
own = {}
for w in W8:
    own[meet(w)] = own.get(meet(w), 0) + it[w]
one = jeffrey(P8, meet, own)
check("p.393/395: two sequential Jeffrey steps = one Jeffrey step on U^V",
      all(zero(it[w] - one[w]) for w in W8))

# (6) Field composition and commutativity
al = dict(zip([1, 0], sp.symbols("alpha1 alpha0", real=True)))
be = dict(zip([1, 0], sp.symbols("beta1 beta0", real=True)))
fc = field(field(P, uA, al), uB, be)
fj = field(P, lambda w: w, {w: al[w[0]] + be[w[1]] for w in W})
fr = field(field(P, uB, be), uA, al)
check("p.397: [P_(U,a)]_(V,b) = P_(U^V, a^b)", all(zero(sp.expand(fc[w] - fj[w])) for w in W))
check("p.397: Field machine is commutative", all(zero(fc[w] - fr[w]) for w in W))

# (7) clause (ii)
pt1 = {1: 1, 0: 0}
pt0 = {1: 0, 0: 1}
aa = {1: sp.log(2), 0: 0}
idu = lambda w: w
L = field(mix(R(1, 2), pt1, pt0), idu, aa)
Rt = mix(R(1, 2), field(pt1, idu, aa), field(pt0, idu, aa))
print(f"      counterexample: (P+Q)_(U,a)(first atom) = {sp.nsimplify(L[1])}, "
      f"P_(U,a) +_1/2 Q_(U,a) (first atom) = {Rt[1]}")
check("p.397 clause (ii) FAILS on the counterexample (2/3 vs 1/2)",
      sp.nsimplify(L[1]) == R(2, 3) and Rt[1] == R(1, 2))
ZP = sum(sp.exp(al[w[0]]) * P[w] for w in W)
ZQ = sum(sp.exp(al[w[0]]) * Q[w] for w in W)
ap = a * ZP / (a * ZP + (1 - a) * ZQ)
lm, rm = field(mix(a, P, Q), uA, al), mix(ap, field(P, uA, al), field(Q, uA, al))
check("corrected clause (ii): coefficient a Z_P / (a Z_P + (1-a) Z_Q)",
      all(zero(lm[w] - rm[w]) for w in W))

# (8) the embedding
ZA = ZP
emb = {i: sp.exp(al[i]) * mass(P, [w for w in W if w[0] == i]) / ZA for i in (1, 0)}
je, fe = jeffrey(P, uA, emb), field(P, uA, al)
check("p.397: Jeffrey on h_P(U,a) = Field on (U,a)", all(zero(je[w] - fe[w]) for w in W))
Pn = {w: P[w] / sum(P.values()) for w in W}
emb0 = {i: mass(Pn, [w for w in W if w[0] == i]) / sum(Pn[w] for w in W) for i in (1, 0)}
check("p.397: unit (U,0) goes to (U, P|_U)",
      all(zero(emb0[i] - mass(Pn, [w for w in W if w[0] == i])) for i in (1, 0)))


def embed_pts(Pv, alpha):
    Z = sum(sp.exp(alpha[i]) * Pv[i] for i in Pv)
    return {i: sp.nsimplify(sp.exp(alpha[i]) * Pv[i] / Z) for i in Pv}


e1 = embed_pts({1: R(1, 2), 0: R(1, 2)}, aa)
e2 = embed_pts({1: R(1, 4), 0: R(3, 4)}, aa)
check("h_P depends on the state: (2/3,1/3) at P=(1/2,1/2); (2/5,3/5) at P=(1/4,3/4)",
      e1 == {1: R(2, 3), 0: R(1, 3)} and e2 == {1: R(2, 5), 0: R(3, 5)})
pp = sp.symbols("p1", positive=True)
target = {1: pp, 0: 1 - pp}
mA = {i: mass(Pn, [w for w in W if w[0] == i]) for i in (1, 0)}
alog = {i: sp.log(target[i] / mA[i]) for i in (1, 0)}
Zs = sum(sp.exp(alog[w[0]]) * Pn[w] for w in W)
emb_s = {i: sp.exp(alog[i]) * mA[i] / Zs for i in (1, 0)}
check("every positive Jeffrey input is h_P of a Field input (a_A = log(p(A)/P(A)))",
      all(zero(emb_s[i] - target[i]) for i in (1, 0)))

# (9) observation (iii): H = B, prior with B independent of A
pa, pb, q1 = sp.symbols("pa pb q1", positive=True)
Pind = {(i, j): (pa if i else 1 - pa) * (pb if j else 1 - pb) for (i, j) in W}
post = jeffrey(Pind, uA, two(q1))
check("p.396 (iii): H independent of every cell of U keeps its probability",
      zero(mass(post, B) - mass(Pind, B)))

print("\nAll checks passed." if ok else "\nSOME CHECKS FAILED.")
raise SystemExit(0 if ok else 1)
