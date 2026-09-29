"""
Good & Mittal (1987), Ann. Statist. 15(2), 694-711 -- amalgamation paradox.

Checks, in exact rational arithmetic (sympy):
  (1) the paper's Note on p. 702: a1 = [3,1;1,9], a2 = [889,203;381,2349] are
      row-uniform with lambda = 0.4, kappa(a1) = kappa(a2) = 27, and kappa(A) =
      26.991 (to 3 dp) < 27 -- the paradox for the odds ratio under a design that
      is row-uniform only (Lean: `note_p702_kappa_paradox`);
  (2) Theorem 4.1 (p. 701) symbolically for two row-uniform tables: pi_R(A) and
      y(A) are the N_i/N-weighted averages (Lean: `piR_weighted_rowUniform`,
      `yule_weighted_rowUniform`);
  (3) Theorem 4.2 (p. 702) symbolically: exp Q_R(A) = sum (b_i/B) exp Q_R(a_i),
      exp W_R(A) = sum (c_i/C) exp W_R(a_i) under row-uniformity;
  (4) Theorem 4.3 (p. 702-703): under row- AND column-uniformity, table 1 is
      rho*table2 + t*[1,-1;-1,1] (symbolic), and kappa(A) lies between kappa(a1),
      kappa(a2) on a grid of rational instances (Lean: `kappa_between_of_uniform`);
  (5) the factorisation in the proof of Theorem 5.1, (5.4) p. 704;
  (6) the p. 696 remark "this can happen even though N_i ∝ p_i": equal-size
      subpopulations, pi_R > 0 in each, pi_R(A) < 0 (Lean: `equalSize_reversal`);
  (7) Yule's case, p. 695: pi_R = 0 and kappa = 1 in each subpopulation, but not in
      the aggregate (Lean: `yule_case_paradox`);
  (8) the general identity pi_R(A) = (row-T average weighted by a_i+b_i) -
      (row-Tbar average weighted by c_i+d_i) (Lean: `piR_amalg_general`).
Exit status 0 iff every check passes.
"""
import sys
import itertools
import sympy as sp
from sympy import Rational as Q

ok = True


def check(name, cond):
    global ok
    print(("PASS " if cond else "FAIL ") + name)
    ok = ok and bool(cond)


def piR(t):
    a, b, c, d = t
    return a / (a + b) - c / (c + d)


def yule(t):
    a, b, c, d = t
    return (a * d - b * c) / (a + b + c + d) ** 2


def kappa(t):
    a, b, c, d = t
    return a * d / (b * c)


def expQR(t):
    a, b, c, d = t
    return d * (a + b) / (b * (c + d))


def expWR(t):
    a, b, c, d = t
    return a * (c + d) / (c * (a + b))


def amalg(*ts):
    return tuple(sum(x) for x in zip(*ts))


def N(t):
    return sum(t)


def paradox(alpha, ts):
    A = amalg(*ts)
    vals = [alpha(t) for t in ts]
    return max(vals) < alpha(A) or alpha(A) < min(vals)


def R(*xs):
    return tuple(sp.Integer(x) for x in xs)


# (1) Note, p. 702
a1, a2 = R(3, 1, 1, 9), R(889, 203, 381, 2349)
A = amalg(a1, a2)
check("Note p.702: row ratio of a1 = 2/5", (a1[0] + a1[1]) / (a1[2] + a1[3]) == Q(2, 5))
check("Note p.702: row ratio of a2 = 2/5", (a2[0] + a2[1]) / (a2[2] + a2[3]) == Q(2, 5))
check("Note p.702: kappa(a1) = 27", kappa(a1) == 27)
check("Note p.702: kappa(a2) = 27", kappa(a2) == 27)
kA = kappa(A)
print(f"     kappa(A) = {kA} = {sp.N(kA, 8)}")
check("Note p.702: kappa(A) = 26.991 to 3 dp", abs(kA - Q(26991, 1000)) < Q(1, 2000))
check("Note p.702: paradox for kappa", paradox(kappa, [a1, a2]))
check("Note p.702: design is NOT column-uniform",
      (a1[0] + a1[2]) / (a1[1] + a1[3]) != (a2[0] + a2[2]) / (a2[1] + a2[3]))
check("Note p.702: no paradox for pi_R, y, expQR, expWR on the same tables (Thms 4.1-4.2)",
      not any(paradox(f, [a1, a2]) for f in (piR, yule, expQR, expWR)))

# (2),(3) Theorems 4.1, 4.2 symbolically for two row-uniform tables
lam = sp.symbols('lam', positive=True)
a1s, c1s, d1s, a2s, c2s, d2s = sp.symbols('a1 c1 d1 a2 c2 d2', positive=True)
t1 = (a1s, lam * (c1s + d1s) - a1s, c1s, d1s)
t2 = (a2s, lam * (c2s + d2s) - a2s, c2s, d2s)
At = amalg(t1, t2)
for name, f in (("pi_R", piR), ("y", yule)):
    r = sp.simplify(f(At) - (N(t1) * f(t1) + N(t2) * f(t2)) / N(At))
    check(f"Thm 4.1: {name}(A) = sum N_i/N {name}(a_i) (symbolic)", r == 0)
r = sp.simplify(expQR(At) - (t1[1] * expQR(t1) + t2[1] * expQR(t2)) / (t1[1] + t2[1]))
check("Thm 4.2: exp Q_R(A) = sum (b_i/B) exp Q_R(a_i) (symbolic)", r == 0)
r = sp.simplify(expWR(At) - (t1[2] * expWR(t1) + t2[2] * expWR(t2)) / (t1[2] + t2[2]))
check("Thm 4.2: exp W_R(A) = sum (c_i/C) exp W_R(a_i) (symbolic)", r == 0)
r = sp.simplify(yule(t1) - piR(t1) * lam / (lam + 1) ** 2)
check("Proof of Thm 4.1: y = pi_R * lam/(lam+1)^2 on a row-uniform table", r == 0)

# (4) Theorem 4.3
b1s, b2s = sp.symbols('b1 b2', positive=True)
T1 = (a1s, b1s, c1s, d1s)
T2 = (a2s, b2s, c2s, d2s)
rho, tt = sp.symbols('rho t')
T1p = (rho * a2s + tt, rho * b2s - tt, rho * c2s - tt, rho * d2s + tt)
rowc = sp.simplify((T1p[0] + T1p[1]) * (c2s + d2s) - (a2s + b2s) * (T1p[2] + T1p[3]))
colc = sp.simplify((T1p[0] + T1p[2]) * (b2s + d2s) - (a2s + c2s) * (T1p[1] + T1p[3]))
check("Thm 4.3: rho*a2 + t*[1,-1;-1,1] is row- and column-uniform with a2", rowc == 0 and colc == 0)
# converse: solve the two uniformity equations for (a1, b1) given c1, d1
sol = sp.solve([(a1s + b1s) * (c2s + d2s) - (a2s + b2s) * (c1s + d1s),
                (a1s + c1s) * (b2s + d2s) - (a2s + c2s) * (b1s + d1s)], [a1s, b1s], dict=True)
conv = True
for s_ in sol:
    tab1 = (s_[a1s], s_[b1s], c1s, d1s)
    rho_ = (sum(tab1)) / (a2s + b2s + c2s + d2s)
    tval = sp.simplify(tab1[0] - rho_ * a2s)
    conv = conv and all(sp.simplify(x) == 0 for x in (
        tab1[1] - (rho_ * b2s - tval), tab1[2] - (rho_ * c2s - tval), tab1[3] - (rho_ * d2s + tval)))
check("Thm 4.3: every row+column-uniform pair has that form", conv and len(sol) == 1)
grid_ok = True
count = 0
for base in [R(889, 203, 381, 2349), R(2, 3, 5, 7), R(10, 1, 1, 10), R(1, 4, 4, 1)]:
    for rho_ in [Q(1, 3), Q(1), Q(5, 2)]:
        for tv in [Q(-1, 2), Q(-1, 7), Q(0), Q(1, 5), Q(3, 4)]:
            tab1 = (rho_ * base[0] + tv, rho_ * base[1] - tv, rho_ * base[2] - tv, rho_ * base[3] + tv)
            if min(tab1) <= 0:
                continue
            count += 1
            kA = kappa(amalg(tab1, base))
            lo, hi = sorted([kappa(tab1), kappa(base)])
            grid_ok = grid_ok and lo <= kA <= hi
check(f"Thm 4.3: kappa(A) between on {count} rational row+col-uniform instances", grid_ok and count > 20)

# (5) Theorem 5.1 factorisation, (5.4) p. 704
k = sp.symbols('k', positive=True)
expr = (b1s * c1s * k / d1s + b2s * c2s * k / d2s) * (d1s + d2s) - k * (b1s + b2s) * (c1s + c2s)
fac = sp.factor(sp.together(expr))
target = k * (b1s * d2s - b2s * d1s) * (c1s * d2s - c2s * d1s) / (d1s * d2s)
check("Thm 5.1: (5.4) reduces to (b1 d2 - b2 d1)(c1 d2 - c2 d1) = 0", sp.simplify(fac - target) == 0)

# (6) p. 696: paradox with equal subpopulation sizes
e1, e2 = R(8, 2, 60, 30), R(20, 70, 1, 9)
EA = amalg(e1, e2)
check("p.696: N1 = N2 = 100", N(e1) == 100 and N(e2) == 100)
check("p.696: pi_R(a1) = 2/15 > 0", piR(e1) == Q(2, 15))
check("p.696: pi_R(a2) = 11/90 > 0", piR(e2) == Q(11, 90))
check("p.696: pi_R(A) = -33/100 < 0 (reversal)", piR(EA) == Q(-33, 100))
check("p.696: the design is not row-uniform (1/9 vs 9)",
      (e1[0] + e1[1]) / (e1[2] + e1[3]) == Q(1, 9) and (e2[0] + e2[1]) / (e2[2] + e2[3]) == 9)
check("p.696: the reversal also holds for kappa",
      kappa(e1) > 1 and kappa(e2) > 1 and kappa(EA) < 1)

# (7) Yule's case, p. 695
y1, y2 = R(9, 9, 9, 9), R(4, 8, 8, 16)
YA = amalg(y1, y2)
check("Yule case: pi_R(a_i) = 0, kappa(a_i) = 1", piR(y1) == 0 and piR(y2) == 0
      and kappa(y1) == 1 and kappa(y2) == 1)
check("Yule case: pi_R(A) = 1/35, kappa(A) = 325/289", piR(YA) == Q(1, 35) and kappa(YA) == Q(325, 289))
check("Yule case: paradox for pi_R and kappa (nothing erased: association appears)",
      paradox(piR, [y1, y2]) and paradox(kappa, [y1, y2]))

# (8) general decomposition of pi_R(A), checked on the equal-size example
wT = [x[0] + x[1] for x in (e1, e2)]
wU = [x[2] + x[3] for x in (e1, e2)]
pT = [x[0] / (x[0] + x[1]) for x in (e1, e2)]
pU = [x[2] / (x[2] + x[3]) for x in (e1, e2)]
dec = sum(w * p for w, p in zip(wT, pT)) / sum(wT) - sum(w * p for w, p in zip(wU, pU)) / sum(wU)
check("pi_R(A) = T-row average (weights a_i+b_i) - Tbar-row average (weights c_i+d_i)", dec == piR(EA))
print(f"     T-row weights {[w / sum(wT) for w in wT]}, Tbar-row weights {[w / sum(wU) for w in wU]}, "
      f"population shares {[N(e1) / N(EA), N(e2) / N(EA)]}")

print("\nAll checks passed." if ok else "\nSOME CHECKS FAILED.")
sys.exit(0 if ok else 1)
