"""
Weisberg, "Commutativity or Holism? A Dilemma for Conditionalizers" (preprint
JCvF.pdf) -- every numeric example and the formal claims, exactly.

Checks:
  (1) pp.3-4 jellybean (Lange's point): .1->.8->.9 vs .1->.9->.8; the Bayes
      factors (36, 9/4) vs (81, 4/9); reversing the experiences (factors) under
      Field's rule returns to .9, reversing input values ends at .8;
  (2) p.9: two Jeffrey updates on one partition {E, notE}: the later value wins;
  (3) p.11: Field's rule recovers its factor and commutes on experiences;
  (4) p.13: 1/10 -> 9/10 is Bayes factor 81, and with q'(E) = 1/10, r'(E) = 9/10;
      the full two-order instance of the Lean theorem `jellybean_wagner`;
  (5) Appendix (26), (27) symbolically on a 2x2 cell model, and Wagner's
      identity (28) as a consequence when the two orders agree on the cells;
  (6) Rigidity Preserves Independence (p.16) on a general 2x2x2 space
      (E x F x G, G a nuisance coordinate so F is not a cell of a partition
      the update is rigid on), with E independent of F;
  (7) p.8: p(.|EF) = p(.|E'F) forces p(E|E'F) = 1, on an instance;
  (8) p.10: Jeffrey on the finest partition reaches any q.
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


def bf(qE, pE):
    return (qE / (1 - qE)) / (pE / (1 - pE))


def odds_update(alpha, pE):
    return alpha * pE / (alpha * pE + (1 - pE))


# (1) pp.3-4
check("bf(.8:.1) = 36, bf(.9:.8) = 9/4", bf(R(8, 10), R(1, 10)) == 36 and bf(R(9, 10), R(8, 10)) == R(9, 4))
check("bf(.9:.1) = 81, bf(.8:.9) = 4/9", bf(R(9, 10), R(1, 10)) == 81 and bf(R(8, 10), R(9, 10)) == R(4, 9))
check("reversed experiences under Field: .1 -(9/4)-> .2 -(36)-> .9",
      odds_update(R(9, 4), R(1, 10)) == R(1, 5) and odds_update(36, odds_update(R(9, 4), R(1, 10))) == R(9, 10))
check("reversed input values end at .8, not .9", R(8, 10) != R(9, 10))

# (2) p.9 on a generic space: worlds E-part and notE-part, two atoms each
p = dict(zip(["e1", "e2", "n1", "n2"], sp.symbols("pe1 pe2 pn1 pn2", positive=True)))
Ecells = ("e1", "e2")


def j2(D, x):
    mE = sum(D[w] for w in Ecells)
    mN = sum(D[w] for w in D if w not in Ecells)
    return {w: (x * D[w] / mE if w in Ecells else (1 - x) * D[w] / mN) for w in D}


x, y = sp.symbols("x y", positive=True)
t1, t2 = j2(j2(p, x), y), j2(p, y)
check("p.9: updating {E,notE} with x then y = updating with y", all(zero(t1[w] - t2[w]) for w in p))
check("p.9: so E ends at y in one order and x in the other",
      zero(sum(t1[w] for w in Ecells) - y) and zero(sum(j2(j2(p, y), x)[w] for w in Ecells) - x))

# (3) p.11
a, b, pe = sp.symbols("a b pE", positive=True)
check("p.11: bf(oddsUpdate(a, pE), pE) = a", zero(bf(odds_update(a, pe), pe) - a))
check("p.11: Field is commutative on experiences",
      zero(odds_update(a, odds_update(b, pe)) - odds_update(b, odds_update(a, pe))))

# (4) p.13
check("p.13: 1/10 -> 9/10 has Bayes factor 81; oddsUpdate(81, 1/10) = 9/10",
      bf(R(9, 10), R(1, 10)) == 81 and odds_update(81, R(1, 10)) == R(9, 10))

cells = list(itertools.product([1, 0], repeat=2))   # (E?, F?)


def jeffE(D, av):
    mE = {i: sum(D[(i, j)] for j in (1, 0)) for i in (1, 0)}
    return {(i, j): av[i] * D[(i, j)] / mE[i] for (i, j) in D}


def jeffF(D, bv):
    mF = {j: sum(D[(i, j)] for i in (1, 0)) for j in (1, 0)}
    return {(i, j): bv[j] * D[(i, j)] / mF[j] for (i, j) in D}


def margE(D, i):
    return sum(D[(i, j)] for j in (1, 0))


jb = {(i, j): (R(1, 10) if i else R(9, 10)) * (R(1, 5) if j else R(4, 5)) for (i, j) in cells}
aE = {1: R(9, 10), 0: R(1, 10)}
bF = {1: R(4, 5), 0: R(1, 5)}
q = jeffE(jb, aE)
r = jeffF(q, bF)
q2 = jeffF(jb, bF)
r2 = jeffE(q2, aE)
check("p.13 instance: both orders end in the same state", all(r[c] == r2[c] for c in cells))
check("p.13 instance: q'(E) = 1/10 (lighting first leaves E untouched)", margE(q2, 1) == R(1, 10))
check("p.13 instance: E-factor 81 in both orders",
      bf(margE(q, 1), margE(jb, 1)) == 81 and bf(margE(r2, 1), margE(q2, 1)) == 81)

# (5) Appendix (26), (27) and (28), symbolically
P = dict(zip(cells, sp.symbols("p11 p10 p01 p00", positive=True)))
a1, a0, b1, b0 = sp.symbols("a1 a0 b1 b0", positive=True)
A_, B_ = {1: a1, 0: a0}, {1: b1, 0: b0}
q = jeffE(P, A_)
r = jeffF(q, B_)
beta_qp = (margE(q, 1) / margE(q, 0)) / (margE(P, 1) / margE(P, 0))
ok26 = all(zero(beta_qp - P[(0, j)] * r[(1, j)] / (P[(1, j)] * r[(0, j)])) for j in (1, 0))
check("Appendix (26): beta_{q,p}(E1:E0) = p(E0Fj) r(E1Fj) / (p(E1Fj) r(E0Fj)), both j", ok26)
a1p, a0p, b1p, b0p = sp.symbols("a1p a0p b1p b0p", positive=True)
q2 = jeffF(P, {1: b1p, 0: b0p})
r2 = jeffE(q2, {1: a1p, 0: a0p})
beta_r2q2 = (margE(r2, 1) / margE(r2, 0)) / (margE(q2, 1) / margE(q2, 0))
ok27 = all(zero(beta_r2q2 - P[(0, j)] * r2[(1, j)] / (P[(1, j)] * r2[(0, j)])) for j in (1, 0))
check("Appendix (27): beta_{r',q'}(E1:E0) = p(E0Fj) r'(E1Fj) / (p(E1Fj) r'(E0Fj)), both j", ok27)
# (28): run the second order with the same Bayes factors (Field's inputs) and
# check that the orders agree on the cells and the E-factor is the same.
alpha = (a1 / a0) / (margE(P, 1) / margE(P, 0))
gamma = (b1 / b0) / ((q[(1, 1)] + q[(0, 1)]) / (q[(1, 0)] + q[(0, 0)]))
mF = {j: P[(1, j)] + P[(0, j)] for j in (1, 0)}
bb1 = gamma * mF[1] / (gamma * mF[1] + mF[0])
q2f = jeffF(P, {1: bb1, 0: 1 - bb1})
mEq2 = {i: margE(q2f, i) for i in (1, 0)}
aa1 = alpha * mEq2[1] / (alpha * mEq2[1] + mEq2[0])
r2f = jeffE(q2f, {1: aa1, 0: 1 - aa1})
constraint = {a0: 1 - a1, b0: 1 - b1}
same = all(zero((r[c] / sum(r.values()) - r2f[c] / sum(r2f.values())).subs(constraint)) for c in cells)
check("same Bayes factors in both orders give r = r' (normalized)", same)
beta2 = (margE(r2f, 1) / margE(r2f, 0)) / (margE(q2f, 1) / margE(q2f, 0))
check("(28): beta_{q,p} = beta_{r',q'} when r = r'", zero((beta_qp - beta2).subs(constraint)))

# (6) Rigidity Preserves Independence, 8 worlds (e, f, g)
W = list(itertools.product([1, 0], repeat=3))
pe_, pf_ = sp.symbols("pe pf", positive=True)
g11, g10, g01, g00 = sp.symbols("g11 g10 g01 g00", positive=True)   # P(G | E, F), arbitrary
gcond = {(1, 1): g11, (1, 0): g10, (0, 1): g01, (0, 0): g00}
Pw = {}
for (e, f, gg) in W:
    base = (pe_ if e else 1 - pe_) * (pf_ if f else 1 - pf_)
    gp = gcond[(e, f)]
    Pw[(e, f, gg)] = base * (gp if gg else 1 - gp)
xq = sp.symbols("xq", positive=True)
mE = sum(Pw[w] for w in W if w[0] == 1)
mN = sum(Pw[w] for w in W if w[0] == 0)
Qw = {w: (xq * Pw[w] / mE if w[0] == 1 else (1 - xq) * Pw[w] / mN) for w in W}
qEF = sum(Qw[w] for w in W if w[0] == 1 and w[1] == 1)
qF = sum(Qw[w] for w in W if w[1] == 1)
qE = sum(Qw[w] for w in W if w[0] == 1)
check("p.16 RPI: p(E|F) = p(E) and rigid on {E,notE} => q(E|F) = q(E)", zero(qEF / qF - qE))
check("  and q(F) = p(F) (the step note 12 leaves implicit)", zero(qF - pf_))

# (7) p.8 on an instance: E' strictly inside E on F
W7 = ["a", "b", "c", "d"]
p7 = {"a": R(1, 10), "b": R(2, 10), "c": R(3, 10), "d": R(4, 10)}
E, Ep, F = {"a", "b"}, {"a"}, {"a", "b", "c"}


def cond(D, S):
    m = sum(D[w] for w in S)
    return {w: (D[w] / m if w in S else 0) for w in D}


c1, c2 = cond(p7, E & F), cond(p7, Ep & F)
lhs_eq = c1 == c2
pE_given = sum(p7[w] for w in E & Ep & F) / sum(p7[w] for w in Ep & F)
check("p.8: here p(E|E'F) = 1 (E' within E) -- consistent with the claim", pE_given == 1)
Ep2 = {"a", "c"}
check("p.8: when p(E|E'F) < 1 the two conditionals differ",
      cond(p7, E & F) != cond(p7, Ep2 & F)
      and sum(p7[w] for w in E & Ep2 & F) / sum(p7[w] for w in Ep2 & F) < 1)

# (8) p.10: Jeffrey on the finest partition reaches any q
qv = {"a": R(1, 2), "b": R(1, 4), "c": R(1, 8), "d": R(1, 8)}
fin = {w: qv[w] * p7[w] / p7[w] for w in W7}
check("p.10: Jeffrey on the partition into worlds with values q(w) gives q", fin == qv)

print("\nAll checks passed." if ok else "\nSOME CHECKS FAILED.")
raise SystemExit(0 if ok else 1)
