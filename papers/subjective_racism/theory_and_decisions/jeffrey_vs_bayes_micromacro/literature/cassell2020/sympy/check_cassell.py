"""
Cassell, "Commutativity, Normativity, and Holism: Lange Revisited", Canadian
Journal of Philosophy 50(2), 159-173 (online 2019).  Journal pages.

Checks, in exact rationals or symbolically:
  (1) The raven (pp.160-161): .9 then .7 ends at .7; reversed, .9; factors 7/27, 27/7.
  (2) Figure 1 (p.162): Lange's conditions; factors of xi1..xi4 = 4/99, 3/4,
      1/33, 4/3; xi1 != xi4, xi2 != xi3; the quoted directions (p.161).
  (3) Figure 2 (p.163): factors 27/7, 49/9 vs 21, 9/49; the same input R -> .3
      confirms first and disconfirms second.
  (4) Reversing input values reverses the factors only if x = y = p, and
      reversing factors reverses the inputs only if a = b = 1 (symbolic);
      Weisberg's jellybean numbers.
  (5) JC on one partition: the later input wins; non-commutative (random
      exact-rational 3-cell example).
  (6) Bayes-factor updates commute on any two partitions (random 3x4 cell
      model); Field's rule composes multiplicatively (symbolic).
  (7) Fn 5's converse fails on one partition: (1/2, 1/2) from 1/10.
  (8) ECJC: every permutation of four experiences ends at the same credence;
      experience-keyed JC ends at the last input.
  (9) Certainty (p.168): oddsUpdate(a, p) = 1 has no solution for 0 < p < 1.
 (10) Objective likelihoods .8/.2 (p.169) give factor 4 for every prior.
 (11) Garber (p.170): a repeated factor drives the credence to 1; bounded
      product of factors keeps it below oddsUpdate(M, p).
 (12) Joan (pp.167-168): factor vectors 3/14 vs 14/3.
Exit status 0 iff every check passes.
"""
import random
import sys
from itertools import permutations

import sympy as sp

R = sp.Rational
OK = True


def check(name, cond):
    global OK
    print(("  PASS  " if cond else "  FAIL  ") + name)
    if not cond:
        OK = False


def bf(q, p):
    return (q / (1 - q)) / (p / (1 - p))


def odds_update(a, p):
    return a * p / (a * p + (1 - p))


def jc(p, cells, q):
    """p: dict atom -> prob; cells: dict atom -> cell; q: dict cell -> input."""
    mass = {}
    for w, c in cells.items():
        mass[c] = mass.get(c, 0) + p[w]
    return {w: q[cells[w]] * p[w] / mass[cells[w]] for w in p}


def bf_update(p, cells, beta):
    Z = sum(beta[cells[w]] * p[w] for w in p)
    return {w: beta[cells[w]] * p[w] / Z for w in p}


def main():
    random.seed(20260930)
    print("(1) raven")
    check("bf(.7|.9) = 7/27, bf(.9|.7) = 27/7", bf(R(7, 10), R(9, 10)) == R(7, 27) and bf(R(9, 10), R(7, 10)) == R(27, 7))

    print("(2) Figure 1")
    p, q, r, qp, rp = R(99, 100), R(8, 10), R(75, 100), R(75, 100), R(8, 10)
    check("Lange's conditions p!=q', p!=q, q=r', q'=r", p != qp and p != q and q == rp and qp == r)
    xs = [bf(q, p), bf(r, q), bf(qp, p), bf(rp, qp)]
    check(f"factors xi1..xi4 = {xs}", xs == [R(4, 99), R(3, 4), R(1, 33), R(4, 3)])
    check("xi1 != xi4 and xi2 != xi3", xs[0] != xs[3] and xs[1] != xs[2])
    check("quoted directions: .99->.8 disconfirms, .75->.8 confirms", xs[0] < 1 < xs[3])

    print("(3) Figure 2")
    f2 = [bf(R(3, 10), R(1, 10)), bf(R(7, 10), R(3, 10)), bf(R(7, 10), R(1, 10)), bf(R(3, 10), R(7, 10))]
    check(f"factors = {f2}", f2 == [R(27, 7), R(49, 9), 21, R(9, 49)])
    check("R -> .3 confirms first, disconfirms second", f2[0] > 1 > f2[3])

    print("(4) reversed inputs vs reversed factors")
    P, X, Y = sp.symbols("p x y", positive=True)
    e1 = sp.numer(sp.together(bf(Y, P) - bf(Y, X)))
    e2 = sp.numer(sp.together(bf(X, Y) - bf(X, P)))
    check("bf(y|p) = bf(y|x) <=> p = x  (numerator factors through (p - x))",
          sp.simplify(sp.factor(e1)).has(P - X) or sp.simplify(sp.factor(e1)).has(X - P))
    check("bf(x|y) = bf(x|p) <=> y = p", sp.simplify(sp.factor(e2)).has(P - Y) or sp.simplify(sp.factor(e2)).has(Y - P))
    sols = sp.solve([e1, e2], [X, Y], dict=True)
    check(f"joint solutions {sols} are x = y = p", sols == [{X: P, Y: P}])
    Aa, Bb = sp.symbols("a b", positive=True)
    x1 = odds_update(Aa, P)
    xr = odds_update(Bb, P)
    c1 = sp.numer(sp.together(xr - odds_update(Bb, x1)))
    c2 = sp.numer(sp.together(odds_update(Aa, xr) - x1))
    sols2 = sp.solve([c1, c2], [Aa, Bb], dict=True)
    check(f"reversing factors reverses inputs only at a = b = 1 ({sols2})", sols2 == [{Aa: 1, Bb: 1}])
    check("jellybean (Weisberg pp.3-4): (36, 9/4) vs (81, 4/9)",
          [bf(R(8, 10), R(1, 10)), bf(R(9, 10), R(8, 10)), bf(R(9, 10), R(1, 10)), bf(R(8, 10), R(9, 10))]
          == [36, R(9, 4), 81, R(4, 9)])

    print("(5) JC on one partition")
    atoms = list(range(6))
    cells = {w: w % 3 for w in atoms}
    pr = [R(random.randint(1, 9)) for _ in atoms]
    s = sum(pr)
    pmap = {w: pr[w] / s for w in atoms}
    q1 = {0: R(1, 5), 1: R(3, 10), 2: R(1, 2)}
    q2 = {0: R(1, 2), 1: R(1, 4), 2: R(1, 4)}
    a = jc(jc(pmap, cells, q1), cells, q2)
    b = jc(jc(pmap, cells, q2), cells, q1)
    check("jc(jc(p,q1),q2) = jc(p,q2)", a == jc(pmap, cells, q2))
    check("the two orders differ", a != b)

    print("(6) Bayes-factor updates commute")
    atoms = [(i, j) for i in range(3) for j in range(4)]
    pr = {w: R(random.randint(1, 9)) for w in atoms}
    s = sum(pr.values())
    pmap = {w: v / s for w, v in pr.items()}
    E = {w: w[0] for w in atoms}
    F = {w: w[1] for w in atoms}
    beta = {i: R(random.randint(1, 9), random.randint(1, 9)) for i in range(3)}
    gam = {j: R(random.randint(1, 9), random.randint(1, 9)) for j in range(4)}
    ab = bf_update(bf_update(pmap, E, beta), F, gam)
    ba = bf_update(bf_update(pmap, F, gam), E, beta)
    check("E then F = F then E (3x4 cell model)", ab == ba)
    A, B, Pp = sp.symbols("a b p", positive=True)
    check("oddsUpdate(a, oddsUpdate(b, p)) = oddsUpdate(ab, p)",
          sp.simplify(odds_update(A, odds_update(B, Pp)) - odds_update(A * B, Pp)) == 0)
    check("bf(oddsUpdate(a, p) | p) = a", sp.simplify(bf(odds_update(A, Pp), Pp) - A) == 0)

    print("(7) fn 5 converse on one partition")
    check("(1/2,1/2) from 1/10: factors (9, 1) both orders, not reversed",
          bf(R(1, 2), R(1, 10)) == 9 and bf(R(1, 2), R(1, 2)) == 1)

    print("(8) ECJC vs experience-keyed JC")
    fac = {"a": R(3), "b": R(1, 2), "c": R(5, 4), "d": R(2, 7)}
    p0 = R(1, 3)
    finals = set()
    for perm in permutations("abcd"):
        c = p0
        for xi in perm:
            c = odds_update(fac[xi], c)
        finals.add(c)
    check(f"all 24 orders of 4 experiences end at the same credence {finals}", len(finals) == 1)
    inp = {"a": R(9, 10), "b": R(7, 10)}

    def keyed(seq, c=R(1, 2)):
        for xi in seq:
            c = inp[xi]  # JC on one partition: the input value is realized
        return c
    check("keyed JC: [a, b] ends at .7, [b, a] at .9 (no commutation)",
          keyed("ab") == R(7, 10) and keyed("ba") == R(9, 10))

    print("(9) certainty")
    Aa = sp.symbols("alpha", positive=True)
    sol = sp.solve(sp.Eq(odds_update(Aa, R(3, 10)), 1), Aa)
    check(f"oddsUpdate(alpha, .3) = 1 has no positive solution ({sol})", sol == [])

    print("(10) objective likelihoods")
    post = R(8, 10) * Pp / (R(8, 10) * Pp + R(2, 10) * (1 - Pp))
    check("factor 4 for every prior", sp.simplify(bf(post, Pp) - 4) == 0)

    print("(11) Garber / considered experiences")
    c = R(1, 10)
    for _ in range(40):
        c = odds_update(R(2), c)
    check(f"40 repetitions of factor 2 from .1: {float(c):.12f} > 1 - 1e-10", c > 1 - R(1, 10 ** 10))
    c = R(1, 10)
    for k in range(40):
        c = odds_update(1 + R(1, 2 ** (k + 1)), c)
    M = 1
    for k in range(40):
        M *= 1 + R(1, 2 ** (k + 1))
    check("halving increments: product M < 3 and credence <= oddsUpdate(M, .1) < 1",
          M < 3 and c <= odds_update(M, R(1, 10)) < 1)

    print("(12) Joan")
    check("beta(R:C): child 3/14, adult 14/3",
          (R(15, 100) / R(70, 100)) / (R(1, 3) / R(1, 3)) == R(3, 14)
          and (R(70, 100) / R(15, 100)) / (R(1, 3) / R(1, 3)) == R(14, 3))

    print("\nALL PASS" if OK else "\nSOME CHECKS FAILED")
    return 0 if OK else 1


if __name__ == "__main__":
    sys.exit(main())
