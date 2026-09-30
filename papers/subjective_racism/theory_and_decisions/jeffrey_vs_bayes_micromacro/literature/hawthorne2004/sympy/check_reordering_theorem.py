"""
Hawthorne (2004), J. Phil. Logic 33, 89-123 -- the Update Reordering Theorem
(Section 9, p. 112) and the Commutation Theorem (Appendix, p. 118), checked on a
finite block-diagonal instance.  Pages are the journal pagination.

Companion to lean/Literature/Hawthorne.lean (`commutation_theorem`, `blk_*`,
`criterion_needs_positive_cells`).  All arithmetic is exact.

Setting: d updates the basis {D1, D2}, epsilon updates {E1, ..., E4}.  The prior
has block-diagonal support: D1 co-occurs only with E1, E2 and D2 only with
E3, E4.  So the Q_{alpha d eps}-compatibility classes of D1 and D2 are {E1, E2}
and {E3, E4}.  Both states are amnestic (each delivers the same targets in
either order): d delivers s = (3/4, 1/4) and eps delivers t.

Hawthorne's clause (2): for each Q_{alpha d eps}-possible D_i there is
    r_i = NL[Q_{alpha eps}, d, D_i] / NL[Q_alpha, d, D_i] > 0
such that every E_j compatible with D_i has
    NL[Q_{alpha d}, eps, E_j] = r_i * NL[Q_alpha, eps, E_j].
r_i is fixed by d's factors; it is not free (audit D8(c)).  The previous
version of this script printed t_j / w_j, which is NL[Q_alpha, eps, E_j] and
not r.

  (1) commutation holds iff t1 + t2 = s1 (t3 + t4 = s2), with t1, t3 free;
  (2) Hawthorne's r is 2/5 on {E1, E2} and 14/5 on {E3, E4}, for every split;
  (3) clause (2) holds on each class with that r, for every split (symbolic);
  (4) NL Extended Rigidity (one r for all E_j) fails: 2/5 != 14/5;
  (5) t_j / w_j (the old printed quantity) is not r and is not constant in a class;
  (6) a non-commuting target violates clause (2) (the "only if" direction);
  (7) the p. 97 criterion's "only if" fails here: the updates commute but each
      moves the other's basis marginal (zero joint cells).
Exit 0 iff all checks pass.
"""
import sys

import sympy as sp

R = sp.Rational
ok = True


def check(name, cond, detail=""):
    global ok
    print(f"  [{'PASS' if cond else 'FAIL'}] {name}")
    if detail:
        print(f"         {detail}")
    if not cond:
        ok = False


w = {1: R(1, 10), 2: R(1, 5), 3: R(3, 10), 4: R(2, 5)}
DOF = {1: 1, 2: 1, 3: 2, 4: 2}             # D-label of each E-atom
prior = {j: w[j] for j in w}               # atoms are the E_j (E refines D)
s = {1: R(3, 4), 2: R(1, 4)}


def margD(P):
    return {i: sum(P[j] for j in P if DOF[j] == i) for i in (1, 2)}


def jeffD(P, tgt):
    m = margD(P)
    return {j: P[j] * tgt[DOF[j]] / m[DOF[j]] for j in P}


def jeffE(P, tgt):
    return {j: tgt[j] for j in P}          # E-cells are atoms: Q_eps[E_j] = t_j


def NL_D(P, Pnew, i):
    return margD(Pnew)[i] / margD(P)[i]


def NL_E(P, Pnew, j):
    return Pnew[j] / P[j]


def routes(t):
    return jeffE(jeffD(prior, s), t), jeffD(jeffE(prior, t), s)


def hawthorne_r(t, i):
    Qe = jeffE(prior, t)
    return sp.simplify(NL_D(Qe, jeffD(Qe, s), i) / NL_D(prior, jeffD(prior, s), i))


t1, t3 = sp.symbols('t1 t3', positive=True)
tsym = {1: t1, 2: s[1] - t1, 3: t3, 4: s[2] - t3}

print("(1) commutation <=> block totals match s")
T = sp.symbols('T1:5', positive=True)
tgen = dict(zip((1, 2, 3, 4), T))
a, b = routes(tgen)
sol = sp.solve([sp.Eq(a[j], b[j]) for j in (1, 2, 3)] + [sp.Eq(sum(T), 1)], T, dict=True)
check("solution family: T1 + T2 = 3/4, T3 + T4 = 1/4, T1 and T3 free",
      len(sol) == 1 and sp.simplify(sol[0][T[1]] - (s[1] - T[0])) == 0
      and sp.simplify(sol[0][T[3]] - (s[2] - T[2])) == 0, str(sol))
a, b = routes(tsym)
check("symbolic split (t1, 3/4 - t1, t3, 1/4 - t3): the two orders agree",
      all(sp.simplify(a[j] - b[j]) == 0 for j in w))

print("(2) Hawthorne's r = NL[Q_ae, d, D_i] / NL[Q_a, d, D_i]")
r1, r2 = hawthorne_r(tsym, 1), hawthorne_r(tsym, 2)
check("r_1 = 2/5 = (w1 + w2)/s1, independent of the split", r1 == R(2, 5), str(r1))
check("r_2 = 14/5 = (w3 + w4)/s2, independent of the split", r2 == R(14, 5), str(r2))
check("r_1, r_2 > 0", r1 > 0 and r2 > 0)

print("(3) clause (2) on each compatibility class, every split")
Qd = jeffD(prior, s)
Qde = jeffE(Qd, tsym)
Qe = jeffE(prior, tsym)
for j in w:
    r = r1 if DOF[j] == 1 else r2
    check(f"E{j}: NL[Q_ad, eps, E{j}] = r_{DOF[j]} * NL[Q_a, eps, E{j}]",
          sp.simplify(NL_E(Qd, Qde, j) - r * NL_E(prior, Qe, j)) == 0)

print("(4) NL Extended Rigidity fails (no single r)")
check("r_1 != r_2, and both classes are Q_ade-possible",
      r1 != r2 and all(sp.simplify(margD(Qde)[i]) > 0 for i in (1, 2)))

print("(5) the old printed quantity t_j / w_j is not Hawthorne's r")
for label, (a1, a3) in {"split A": (R(1, 10), R(1, 20)), "split B": (R(1, 2), R(1, 5))}.items():
    tt = {1: a1, 2: s[1] - a1, 3: a3, 4: s[2] - a3}
    ratios = {j: tt[j] / w[j] for j in w}
    check(f"{label}: t1/w1 = {ratios[1]}, t2/w2 = {ratios[2]} differ within class 1, "
          f"while r_1 = {hawthorne_r(tt, 1)}",
          ratios[1] != ratios[2] and hawthorne_r(tt, 1) == R(2, 5))
    a, b = routes(tt)
    check(f"{label}: commutes", all(a[j] == b[j] for j in w))

print("(6) a non-commuting target violates clause (2)")
tbad = {1: R(1, 10), 2: R(1, 5), 3: R(3, 10), 4: R(2, 5)}   # block totals 3/10, 7/10
a, b = routes(tbad)
check("E-target with block totals (3/10, 7/10) does not commute",
      any(a[j] != b[j] for j in w))
Qd = jeffD(prior, s)
Qe = jeffE(prior, tbad)
rb = hawthorne_r(tbad, 1)
check("clause (2) fails for it on class 1",
      NL_E(Qd, jeffE(Qd, tbad), 1) != rb * NL_E(prior, Qe, 1),
      f"r_1 = {rb}, NL[Q_ad,eps,E1] = {NL_E(Qd, jeffE(Qd, tbad), 1)}, "
      f"r_1 NL[Q_a,eps,E1] = {rb * NL_E(prior, Qe, 1)}")

print("(7) p. 97 criterion needs every joint cell positive")
tA = {1: R(1, 10), 2: R(13, 20), 3: R(1, 20), 4: R(1, 5)}
a, b = routes(tA)
QdE1 = jeffD(prior, s)[1]
QeD1 = margD(jeffE(prior, tA))[1]
check("the updates commute", all(a[j] == b[j] for j in w))
check("but d moves Q[E1] from 1/10 to 1/4", QdE1 == R(1, 4) and prior[1] == R(1, 10))
check("and eps moves Q[D1] from 3/10 to 3/4", QeD1 == R(3, 4) and margD(prior)[1] == R(3, 10))
check("the zero joint cells: Q[D1 . E3] = Q[D1 . E4] = Q[D2 . E1] = Q[D2 . E2] = 0",
      all(DOF[j] != i for i, j in [(1, 3), (1, 4), (2, 1), (2, 2)]))

print()
print("ALL PASS" if ok else "SOME CHECKS FAILED")
sys.exit(0 if ok else 1)
