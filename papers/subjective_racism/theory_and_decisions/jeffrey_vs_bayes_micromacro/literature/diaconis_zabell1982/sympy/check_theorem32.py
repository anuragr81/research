"""
Diaconis & Zabell (1982), "Updating Subjective Probability" -- checks of the
paper's Section 3 claims on the 2x2 table (Paper B's layout: cell (a, b), a = 1
iff E, b = 1 iff F). Each check prints PASS/FAIL; the script exits 0 iff all
pass.

  (1) Example 3.4 (p. 826): P_EF(E) = 1/2 = p1, P_FE(F) = 371/851 != 7/15 = q1;
  (2) Example 3.2 (p. 825): P_EF = (.56, .24, .14, .06), and the order does not
      matter;
  (3) Theorem 3.2 / Remark p. 826 on the prior (alpha, beta, c): updating E to p
      moves P(F) by (p - alpha) c / (alpha (1 - alpha)), so J-independence holds
      iff (p = alpha or c = 0) and (q = beta or c = 0); at c = 0 the two orders
      agree identically; at c != 0 they do not (Paper B's witness -1032/369935);
  (4) Theorem 3.2 needs every E_i F_j nonempty: with E = F and p = q != P(E) the
      orders agree without J-independence.

Lean: lean/Literature/DiaconisZabell.lean (symlinked in ../lean/).
"""
import sys

import sympy as sp

R = sp.Rational
FAILS = []


def check(name, ok, detail=""):
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}" + (f"\n         {detail}" if detail and not ok else ""))
    if not ok:
        FAILS.append(name)


def jeff1(P, ev, t):
    comp = [x for x in P if x not in ev]
    mE = sum(P[x] for x in ev)
    mC = sum(P[x] for x in comp)
    return {x: (P[x] / mE * t if x in ev else P[x] / mC * (1 - t)) for x in P}


Aset = {(1, 1), (1, 0)}
Bset = {(1, 1), (0, 1)}


def margA(P):
    return P[(1, 1)] + P[(1, 0)]


def margB(P):
    return P[(1, 1)] + P[(0, 1)]


def zero(expr):
    return sp.simplify(sp.together(expr)) == 0


print("Example 3.4 (p. 826)")
P0 = {(1, 1): R(1, 8), (1, 0): R(1, 4), (0, 1): R(3, 8), (0, 0): R(1, 4)}
p1, q1 = R(1, 2), R(7, 15)
EF = jeff1(jeff1(P0, Aset, p1), Bset, q1)
FE = jeff1(jeff1(P0, Bset, q1), Aset, p1)
check("P_EF(E) = 1/2 = p1 (the earlier target survives on this route)", margA(EF) == p1)
check("P_EF(F) = 7/15 = q1 (the last target is held by construction)", margB(EF) == q1)
check("P_FE(E) = 1/2 = p1", margA(FE) == p1)
check("P_FE(F) = 371/851", margB(FE) == R(371, 851), f"got {margB(FE)}")
check("371/851 != 7/15 = q1, so P_EF != P_FE", R(371, 851) != q1 and EF != FE)

print("Example 3.2 (p. 825)")
U = {k: R(1, 4) for k in P0}
ex32 = jeff1(jeff1(U, Aset, R(8, 10)), Bset, R(7, 10))
ex32r = jeff1(jeff1(U, Bset, R(7, 10)), Aset, R(8, 10))
check("P_EF(a, b, c, d) = (.56, .24, .14, .06)",
      [ex32[(1, 1)], ex32[(1, 0)], ex32[(0, 1)], ex32[(0, 0)]] == [R(56, 100), R(24, 100), R(14, 100), R(6, 100)])
check("the order does not matter in Example 3.2", ex32 == ex32r)

print("Theorem 3.2 and the Remark on p. 826, prior (alpha, beta, c)")
a, b, p, q, c = sp.symbols('alpha beta p q c', positive=True)
Pc = {(1, 1): a * b + c, (1, 0): a * (1 - b) - c, (0, 1): (1 - a) * b - c, (0, 0): (1 - a) * (1 - b) + c}
check("updating E to p moves P(F) by (p - alpha) c / (alpha (1 - alpha))",
      zero(margB(jeff1(Pc, Aset, p)) - b - (p - a) * c / (a * (1 - a))))
check("updating F to q moves P(E) by (q - beta) c / (beta (1 - beta))",
      zero(margA(jeff1(Pc, Bset, q)) - a - (q - b) * c / (b * (1 - b))))
Pi = {k: v.subs(c, 0) for k, v in Pc.items()}
r1 = jeff1(jeff1(Pi, Aset, p), Bset, q)
r2 = jeff1(jeff1(Pi, Bset, q), Aset, p)
check("c = 0 (independent prior): the two orders agree identically in p, q",
      all(zero(r1[k] - r2[k]) for k in Pi))
s1 = jeff1(jeff1(Pc, Aset, p), Bset, q)
s2 = jeff1(jeff1(Pc, Bset, q), Aset, p)
gap11 = sp.simplify(sp.together(s1[(1, 1)] - s2[(1, 1)]))
check("c != 0: the gap in cell (1,1) is not identically zero", gap11 != 0)
w = gap11.subs({a: R(3, 10), b: R(11, 20), c: R(1, 20), p: R(2, 5), q: R(3, 5)})
check("witness (Paper B's, not D-Z's): gap = -1032/369935 at (.3, .55, .05, .4, .6)",
      sp.nsimplify(w) == R(-1032, 369935), f"got {w}")
check("c != 0 but trivial targets p = alpha, q = beta: the orders agree",
      all(zero((s1[k] - s2[k]).subs({p: a, q: b})) for k in Pc))

print("Theorem 3.2 needs qualitative independence (E = F)")
P2pt = {0: R(1, 2), 1: R(1, 2)}
E1 = {1}
t = R(4, 5)
seq_EF = jeff1(jeff1(P2pt, E1, t), E1, t)
seq_FE = jeff1(jeff1(P2pt, E1, t), E1, t)
Jind = jeff1(P2pt, E1, t)[1] == P2pt[1]
check("E = F, p = q = (.8, .2), prior (.5, .5): P_EF = P_FE", seq_EF == seq_FE)
check("... but E, F are not Jeffrey independent (P_E(F1) = .8 != .5)", not Jind)

print(f"\n  -> {'all checks passed' if not FAILS else 'FAILURES: ' + ', '.join(FAILS)}")
sys.exit(0 if not FAILS else 1)
