"""
Dietrich (2021), "Fully Bayesian Aggregation", JET 194:105255 -- the paper's
belief-pooling claims, and the project's question of whether Paper B's
population mean can be evaluated against Dietrich's criterion.

Exact arithmetic throughout. The script exits 0 iff every check passes.

Part I (the paper):
  (1) Table 1 (p. 5), the duel: the linear-linear and linear-geometric rows,
      the old and new probabilities and expected utilities, all rounded to two
      decimals as printed. After learning E the linear pool is not the
      conditioned pool (the paper's .83/.17 and EU .08). The geometric pool is
      the conditioned pool, exactly.
  (2) Dynamic Rationality of geometric pooling, symbolically, for two
      individuals on three states with a free weight lambda (Thm 2, Part 1).
  (3) External Bayesianity of geometric pooling with a free likelihood (p. 15).
  (4) The two-person linear-pooling defect identity (Lean `linPool_cond_sub`).

Part II (the project's question, not Dietrich's): common prior P, two cues,
members differ only in reading order.
  (5) P^J_AB has full support and differs from P, so it is not P(.|E) for any
      event E. Dynamic Rationality's premise fails.
  (6) The implied likelihoods P^J_AB/P and P^J_BA/P are not proportional, so
      there is no common likelihood. External Bayesianity's premise fails.
  (7) With a common prior and a common event, linear pooling commutes with
      conditioning (every unanimity-preserving rule does).
  (8) Against the benchmark P^B, the linear pool and the geometric pool of
      (P^J_AB, P^J_BA) both miss at first order in c with the SAME coefficient.
      They differ from each other only at O(c^2), so the criterion's
      linear-vs-geometric contrast does not bear on Paper B's first-order
      results.
"""
import os
import sys

import sympy as sp

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..', 'sympy'))
from jeffrey_core import (posteriors, prior, alpha, beta, c, q0, r0,  # noqa: E402
                          GENERIC, Check, simp)

R = sp.Rational


def norm(v):
    s = sum(v)
    return [x / s for x in v]


def cond(v, E):
    return norm([x if k in E else 0 for k, x in enumerate(v)])


def r2(x):
    """Round to two decimals, as Table 1 prints; returned as an exact rational."""
    return sp.floor(sp.N(100 * x, 50) + R(1, 2)) / sp.Integer(100)


def part1():
    ch = Check("Part I -- Dietrich's own claims")

    # (1) Table 1, the duel.  States: 1 stronger, 2 stronger, 2 better weapon.
    p1 = [R(85, 100), R(5, 100), R(10, 100)]
    p2 = [R(15, 100), R(15, 100), R(70, 100)]
    u1 = {'win1': 1, 'win2': -5}
    u2 = {'win1': 0, 'win2': 1}
    ug = {k: (u1[k] + u2[k]) / sp.Integer(2) for k in u1}    # (.5, -2)
    E = {0, 1}

    def eu(p, u):
        return p[0] * u['win1'] + (p[1] + p[2]) * u['win2']

    lin_old = [(a + b) / 2 for a, b in zip(p1, p2)]
    lin_new = [(a + b) / 2 for a, b in zip(cond(p1, E), cond(p2, E))]
    lin_bayes = cond(lin_old, E)
    geo = lambda a, b: norm([sp.sqrt(x * y) for x, y in zip(a, b)])  # noqa: E731
    geo_old = geo(p1, p2)
    geo_new = geo(cond(p1, E), cond(p2, E))

    ch("group utility (.5, -2)", (ug['win1'], ug['win2']) == (R(1, 2), -2))
    ch("linear-linear old probs (.5, .1, .4)", [r2(x) for x in lin_old] == [R(50, 100), R(10, 100), R(40, 100)])
    ch("linear-linear new probs (.72, .28, 0)", [r2(x) for x in lin_new] == [R(72, 100), R(28, 100), 0])
    ch("linear new prob of state 1 is exactly 13/18", lin_new[0] == R(13, 18))
    ch("old EU of duel, linear: -.75", r2(eu(lin_old, ug)) == R(-75, 100))
    ch("new EU of duel, linear: -.19", r2(eu(lin_new, ug)) == R(-19, 100))
    ch("Bayes-conditioned linear pool (.83, .17): exactly 5/6, 1/6",
       lin_bayes[:2] == [R(5, 6), R(1, 6)])
    ch("Bayes-conditioned linear EU .08", r2(eu(lin_bayes, ug)) == R(8, 100))
    ch("linear pool is NOT dynamically rational on the duel", lin_new != lin_bayes)
    # Table 1 prints (.51, .12, .37). The exact values are (0.5042, 0.1223, 0.3736),
    # which round to (.50, .12, .37). The printed .51 is a rounding discrepancy in
    # the paper (it makes the printed row sum to 1.00). The .37 is confirmed in
    # the text of p. 5. We check the exact rounding and record the discrepancy.
    ch("linear-geometric old probs: exact rounding (.50, .12, .37) [Table 1 prints .51 for state 1]",
       [r2(x) for x in geo_old] == [R(50, 100), R(12, 100), R(37, 100)])
    ch("linear-geometric old EU -.74", r2(eu(geo_old, ug)) == R(-74, 100))
    ch("linear-geometric new probs (.80, .20, 0)",
       [r2(x) for x in geo_new] == [R(80, 100), R(20, 100), 0])
    ch("linear-geometric new EU .01", r2(eu(geo_new, ug)) == R(1, 100))
    ch("geometric pool IS the conditioned geometric pool (exact)",
       all(sp.simplify(a - b) == 0 for a, b in zip(geo_new, cond(geo_old, E))))

    # (2) Dynamic Rationality of geometric pooling, symbolic
    lam = sp.Symbol('lamda', positive=True)
    a1, a2, a3, b1, b2, b3 = sp.symbols('a1 a2 a3 b1 b2 b3', positive=True)
    A = [a1, a2, a3]
    B = [b1, b2, b3]

    def gpool(P, Q):
        return norm([x ** lam * y ** (1 - lam) for x, y in zip(P, Q)])

    lhs = gpool(cond(A, E), cond(B, E))
    rhs = cond(gpool(A, B), E)
    ok = True
    for i in range(2):
        # zero cells: lhs and rhs both exactly 0 at state 3 (0**lam = 0 for lam > 0)
        e = sp.expand_log(sp.log(lhs[i] / lhs[1 - i]), force=True) \
            - sp.expand_log(sp.log(rhs[i] / rhs[1 - i]), force=True)
        ok &= sp.simplify(e) == 0
    ok &= sp.simplify(lhs[2]) == 0 and sp.simplify(rhs[2]) == 0
    ch("Thm 2 Part 1: geometric pooling commutes with conditioning (symbolic lambda)", ok)

    # (3) External Bayesianity with a free positive likelihood
    L1, L2, L3 = sp.symbols('L1 L2 L3', positive=True)
    L = [L1, L2, L3]

    def rev(P):
        return norm([x * l for x, l in zip(P, L)])
    lhs = gpool(rev(A), rev(B))
    rhs = rev(gpool(A, B))
    ok = True
    for i in range(2):
        e = sp.expand_log(sp.log(lhs[i] / lhs[2]), force=True) \
            - sp.expand_log(sp.log(rhs[i] / rhs[2]), force=True)
        ok &= sp.simplify(e) == 0
    ch("p. 15: geometric pooling is externally Bayesian (symbolic lambda, L)", ok)

    # (4) two-person linear defect identity
    x1, x2, e1, e2 = sp.symbols('x1 x2 e1 e2', positive=True)
    m = lam * e1 + (1 - lam) * e2
    d = lam * x1 / e1 + (1 - lam) * x2 / e2 - (lam * x1 + (1 - lam) * x2) / m
    ch("linear defect = lam(1-lam)(e2-e1)(x1/e1-x2/e2)/m",
       sp.cancel(d - lam * (1 - lam) * (e2 - e1) * (x1 / e1 - x2 / e2) / m) == 0)
    return ch.done()


def part2():
    ch = Check("Part II -- THE PROJECT'S QUESTION: Paper B's population mean vs Dietrich")
    P = prior()
    AB, BA, PB = posteriors()
    pt = GENERIC + [(c, R(1, 50))]
    ABn, BAn, Pn = AB.subs(pt), BA.subs(pt), P.subs(pt)

    # (5) not a conditionalisation
    ch("P^J_AB has full support at the generic point", all(v > 0 for v in ABn))
    ch("P^J_AB != P at the generic point", any(ABn[k] != Pn[k] for k in range(4)))
    ch("hence P^J_AB is not P(.|E) for any E (full support forces E = Omega)", True)

    # (6) no common likelihood: P^J_AB/P not proportional to P^J_BA/P
    lab = [ABn[k] / Pn[k] for k in range(4)]
    lba = [BAn[k] / Pn[k] for k in range(4)]
    ratios = [sp.nsimplify(lab[k] / lba[k]) for k in range(4)]
    ch("P^J_AB/P and P^J_BA/P are not proportional (no common likelihood)",
       len(set(ratios)) > 1)

    # (7) common prior + common event: linear pool commutes (trivially)
    lam = R(1, 3)
    vec = [Pn[0, 0], Pn[0, 1], Pn[1, 0], Pn[1, 1]]
    E = {0, 3}
    lin_of_cond = [lam * a + (1 - lam) * b for a, b in zip(cond(vec, E), cond(vec, E))]
    cond_of_lin = cond([lam * a + (1 - lam) * b for a, b in zip(vec, vec)], E)
    ch("common prior, common event: linear pool is dynamically rational",
       all(sp.nsimplify(a - b) == 0 for a, b in zip(lin_of_cond, cond_of_lin)))

    # (8) linear vs geometric pool of (P^J_AB, P^J_BA), against P^B
    lamS = R(1, 3)
    ABg, BAg, PBg = AB.subs(GENERIC), BA.subs(GENERIC), PB.subs(GENERIC)
    G = sp.Matrix(2, 2, lambda i, j: ABg[i, j] ** lamS * BAg[i, j] ** (1 - lamS))
    G = G / sum(G)
    Lin = lamS * ABg + (1 - lamS) * BAg

    def coeffs(e, n=3):
        ser = sp.series(e, c, 0, n).removeO()
        return [sp.nsimplify(sp.simplify(ser.coeff(c, k))) for k in range(n)]

    ok_same_first = True
    ok_first_nonzero = False
    ok_lin_geo_second = True
    ok_lin_geo_second_nonzero = False
    for i in range(2):
        for j in range(2):
            g = coeffs(G[i, j] - PBg[i, j])
            li = coeffs(Lin[i, j] - PBg[i, j])
            lg = coeffs(Lin[i, j] - G[i, j])
            ok_same_first &= (g[0] == 0 and li[0] == 0 and g[1] == li[1])
            ok_first_nonzero |= (li[1] != 0)
            ok_lin_geo_second &= (lg[0] == 0 and lg[1] == 0)
            ok_lin_geo_second_nonzero |= (lg[2] != 0)
    ch("linear and geometric pools both miss P^B at order c, with the same c-coefficient",
       ok_same_first and ok_first_nonzero)
    ch("linear pool - geometric pool = O(c^2), with a nonzero c^2 term",
       ok_lin_geo_second and ok_lin_geo_second_nonzero)
    return ch.done()


if __name__ == "__main__":
    ok1 = part1()
    ok2 = part2()
    print("\nAll checks passed." if ok1 and ok2 else "\nSOME CHECKS FAILED.")
    sys.exit(0 if ok1 and ok2 else 1)
