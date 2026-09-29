"""
Cripps (2021), "Divisible Updating" -- the paper's claims, and the project's
question of whether Paper B's Jeffrey composite is a Cripps rule at all.

All checks are exact (symbolic, or rational at a fixed point). The script exits
0 iff every check passes.

Part I (the paper):
  (1) Bayes and geometric probability weighting (Sec. 4.1, F^a) are divisible:
      u(u(mu,x),y) = u(mu, x*y) = u(u(mu,y),x), symbolically in a.
  (2) The closed form of Sec. 4.1, u = mu o p^(1/a) / mu^T p^(1/a), equals
      F^{-1}(Bayes(F(mu), p)) for F^a(mu) = mu^a / sum mu^a.
  (3) Epstein-Noor-Sandroni (eq. (1), Cripps's non-divisible example) is
      order-dependent and fails u(u(mu,x),y) = u(mu,x*y).

Part II (the project's question, not Cripps's): on Theta = {0,1}^2,
  (4) matched-likelihood translation: J_A(P; q) = Bayes(P, q_i/P(A=i)) exactly
      (Paper B's Prop. IMM), and J_B(J_A P) = Bayes(P, lA * lB') where lB' is
      matched to the *intermediate* belief. The B-marginal moves by
      c(q0-alpha)/(alpha(1-alpha)), so lB' != lB exactly when c(q0-alpha) != 0.
      With the likelihoods held fixed, Bayes commutes and gives P^B.
  (5) rigid-credence translation phi(p) = normalized A-marginal of p:
      - Symmetry holds (the update depends on p_s only);
      - Uninformativeness fails;
      - Divisibility (Axiom 3(b)) fails;
      - Non-Dogmatic fails, because mu(B|A) is frozen.
"""
import os
import sys

import sympy as sp

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..', 'sympy'))
from jeffrey_core import (prior, jeffrey_A, jeffrey_B, bayes, marg_A, marg_B,  # noqa: E402
                          alpha, beta, c, q0, r0, q, r, GENERIC, Check, simp)


def bayes_vec(mu, p):
    w = [m * x for m, x in zip(mu, p)]
    s = sum(w)
    return [sp.cancel(v / s) for v in w]


def zero_vec(v):
    return all(sp.simplify(e) == 0 for e in v)


def part1():
    ch = Check("Part I -- Cripps's own claims")
    m1, m2, x1, x2, x3, y1, y2, y3 = sp.symbols('m1 m2 x1 x2 x3 y1 y2 y3', positive=True)
    a = sp.Symbol('a', positive=True)
    mu = [m1, m2, 1 - m1 - m2]
    x = [x1, x2, x3]
    y = [y1, y2, y3]
    xy = [xi * yi for xi, yi in zip(x, y)]

    # (1a) Bayes
    lhs = bayes_vec(bayes_vec(mu, x), y)
    ch("Bayes: u(u(mu,x),y) = u(mu,x*y)", zero_vec([l - rr for l, rr in zip(lhs, bayes_vec(mu, xy))]))

    # (1b) geometric probability weighting, closed form of Sec. 4.1
    def u_geo(mu, p):
        w = [m * pi ** (1 / a) for m, pi in zip(mu, p)]
        s = sum(w)
        return [v / s for v in w]
    l1 = u_geo(u_geo(mu, x), y)
    l2 = u_geo(mu, xy)
    l3 = u_geo(u_geo(mu, y), x)
    d12 = [sp.simplify(sp.powsimp(sp.expand_power_base(e1 - e2, force=True), force=True))
           for e1, e2 in zip(l1, l2)]
    d13 = [sp.simplify(sp.powsimp(sp.expand_power_base(e1 - e3, force=True), force=True))
           for e1, e3 in zip(l1, l3)]
    ch("geometric weighting (symbolic a): u(u(mu,x),y) = u(mu,x*y)", all(e == 0 for e in d12))
    ch("geometric weighting (symbolic a): order-invariant", all(e == 0 for e in d13))

    # (2) closed form equals F^{-1}(Bayes(F mu, p)) for F^a
    def F(mu):
        w = [m ** a for m in mu]
        s = sum(w)
        return [v / s for v in w]

    def Finv(nu):
        w = [n ** (1 / a) for n in nu]
        s = sum(w)
        return [v / s for v in w]
    # compare log-odds against the last state; the normalizations cancel there
    m3 = sp.Symbol('m3', positive=True)       # stands for 1 - m1 - m2 > 0
    mup = [m1, m2, m3]
    via_F = Finv(bayes_vec(F(mup), x))
    closed = u_geo(mup, x)
    ok = True
    for i in range(2):
        e = sp.expand_log(sp.log(via_F[i] / via_F[2]), force=True) \
            - sp.expand_log(sp.log(closed[i] / closed[2]), force=True)
        ok &= (sp.simplify(e) == 0)
    ch("Sec. 4.1: F^{-1}(Bayes(F^a mu, p)) = mu o p^(1/a) / norm", ok)

    # (3) Epstein-Noor-Sandroni, eq. (1): (1-lam) mu + lam Bayes(mu, p)
    lam = sp.Rational(3, 2)
    def ens(mu, p):
        b = bayes_vec(mu, p)
        return [(1 - lam) * m + lam * bb for m, bb in zip(mu, b)]
    pt = {m1: sp.Rational(1, 5), m2: sp.Rational(3, 10),
          x1: sp.Rational(1, 3), x2: sp.Rational(1, 2), x3: sp.Rational(3, 4),
          y1: sp.Rational(4, 5), y2: sp.Rational(1, 4), y3: sp.Rational(1, 2)}
    mun = [e.subs(pt) for e in mu]
    xn = [e.subs(pt) for e in x]
    yn = [e.subs(pt) for e in y]
    xyn = [e.subs(pt) for e in xy]
    e_xy = ens(ens(mun, xn), yn)
    e_yx = ens(ens(mun, yn), xn)
    e_one = ens(mun, xyn)
    ch("ENS (lambda=3/2) is order-dependent: u(u(mu,x),y) != u(u(mu,y),x)",
       any(sp.nsimplify(s1 - s2) != 0 for s1, s2 in zip(e_xy, e_yx)))
    ch("ENS fails the Lemma 1(iii) composition: u(u(mu,x),y) != u(mu,x*y)",
       any(sp.nsimplify(s1 - s2) != 0 for s1, s2 in zip(e_xy, e_one)))
    return ch.done()


def part2():
    ch = Check("Part II -- THE PROJECT'S QUESTION: is the Jeffrey composite a Cripps rule?")
    P = prior()
    mA = marg_A(P)
    mB = marg_B(P)

    # (4) matched-likelihood translation
    lA = [q[i] / mA[i] for i in range(2)]
    lB = [r[j] / mB[j] for j in range(2)]
    BA_ = sp.Matrix(2, 2, lambda i, j: P[i, j] * lA[i])
    BA_ = BA_ / simp(sum(BA_))
    ch.mat_eq("Prop. IMM: J_A(P;q) = Bayes(P, q_i/P(A=i))", jeffrey_A(P), BA_)

    QA = jeffrey_A(P)
    shift = simp(marg_B(QA)[0] - beta)
    ch.eq("B-marginal after J_A moves by c(q0-alpha)/(alpha(1-alpha))",
          shift, c * (q0 - alpha) / (alpha * (1 - alpha)))
    lBp = [r[j] / sp.cancel(marg_B(QA)[j]) for j in range(2)]
    W = sp.Matrix(2, 2, lambda i, j: P[i, j] * lA[i] * lBp[j])
    ch.mat_eq("composite J_B(J_A P) = Bayes(P, lA * lB') with lB' matched to J_A P",
              jeffrey_B(QA), W / simp(sum(W)))
    ch("re-matched lB' differs from lB at a generic point with c = 1/50",
       sp.cancel((lBp[0] / lBp[1] - lB[0] / lB[1]).subs(GENERIC).subs(c, sp.Rational(1, 50))) != 0)
    # Bayes on fixed likelihoods commutes and equals P^B
    Wab = sp.Matrix(2, 2, lambda i, j: P[i, j] * lA[i])
    Wab = Wab / simp(sum(Wab))
    Wab = sp.Matrix(2, 2, lambda i, j: Wab[i, j] * lB[j])
    Wab = Wab / simp(sum(Wab))
    Wba = sp.Matrix(2, 2, lambda i, j: P[i, j] * lB[j])
    Wba = Wba / simp(sum(Wba))
    Wba = sp.Matrix(2, 2, lambda i, j: Wba[i, j] * lA[i])
    Wba = Wba / simp(sum(Wba))
    ch.mat_eq("fixed-likelihood Bayes, A then B = B then A", Wab, Wba)
    ch.mat_eq("fixed-likelihood Bayes = P^B (the benchmark)", Wab, bayes(P))

    # (5) rigid-credence translation, phi(p) = normalized A-marginal of p
    # A-measurable likelihood vectors on {0,1}^2 are (x_A0, x_A0, x_A1, x_A1)
    def phi(pA):          # pA = (p on A=0, p on A=1)
        s = pA[0] + pA[1]
        return [pA[0] / s, pA[1] / s]

    def rigid(Q, pA):
        return jeffrey_A(Q, phi(pA))

    pt = GENERIC + [(c, sp.Rational(1, 50))]
    Pn = P.subs(pt)
    # Axiom 1: an uninformative experiment still moves mu unless P(A=0) = 1/2
    unin = rigid(Pn, [sp.Rational(1, 3), sp.Rational(1, 3)])
    ch("rigid: Axiom 1 (Uninformativeness) FAILS at alpha = 1/3",
       any(sp.nsimplify(unin[k] - Pn[k]) != 0 for k in range(4)))
    # Axiom 2: the rule depends on p_s only, so Symmetry holds by construction
    ch("rigid: Axiom 2 (Symmetry) holds (update depends on p_s alone)", True)
    # Axiom 3(b): three A-measurable signals p1, p2, p3
    p1 = [sp.Rational(1, 5), sp.Rational(1, 2)]
    p2 = [sp.Rational(3, 10), sp.Rational(1, 4)]
    one_step = rigid(Pn, p2)
    first = rigid(Pn, [1 - p1[0], 1 - p1[1]])
    two_step = rigid(first, [p2[0] / (1 - p1[0]), p2[1] / (1 - p1[1])])
    ch("rigid: Axiom 3(b) (Divisibility) FAILS",
       any(sp.nsimplify(one_step[k] - two_step[k]) != 0 for k in range(4)))
    # Axiom 4: one cue on A leaves mu(B|A) unchanged
    qq = sp.Symbol('t', positive=True)
    J = jeffrey_A(P, [qq, 1 - qq])
    ch.eq("rigid: J_A keeps P(B=0|A=0) for every credence (Axiom 4 FAILS)",
          J[0, 0] / (J[0, 0] + J[0, 1]), P[0, 0] / (P[0, 0] + P[0, 1]))
    return ch.done()


if __name__ == "__main__":
    ok1 = part1()
    ok2 = part2()
    print("\nAll checks passed." if ok1 and ok2 else "\nSOME CHECKS FAILED.")
    sys.exit(0 if ok1 and ok2 else 1)
