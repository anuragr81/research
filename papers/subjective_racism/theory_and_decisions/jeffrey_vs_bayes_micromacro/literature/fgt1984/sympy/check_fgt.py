"""
Foster, Greer & Thorbecke (1984), Econometrica 52(3), 761-766.

Checks, in exact rational arithmetic (sympy):
  (1) P_0 = H and P_1 = H*I on a worked population, P_1 = population mean of
      max(z-y,0)/z, and (1/q) sum g_i = z*I (Lean: `P0_eq_H`, `P1_eq_H_mul_I`,
      `P1_eq_popMean_normGap`, `I_eq_meanGapPoor_div_z`);
  (2) equation (2), p. 762, P_2 = H[I^2 + (1-I)^2 C_p^2], symbolically for q = 3
      poor households out of n (Lean: `P2_formula`);
  (3) Proposition 2, (4) p. 764: additive decomposability, symbolically for a
      2-group split of 5 households with symbolic incomes below the line, and for
      alpha = 0, 1, 2, 3 on a mixed rational population (Lean: `P_decomp`);
  (4) Proposition 1 examples: P_0 not monotone, P_1 transfer-neutral, P_2 strictly
      transfer-increasing (Lean: `P0_not_monotone`, `P1_transfer_neutral`);
  (5) I is not population-share decomposable (Lean: `I_not_decomposable`);
  (6) footnote 6, p. 763 (Cowell's example): the Sen measure ranks subgroup 1 and
      the whole population in opposite directions for z >= 13, while every P_alpha
      ranks them the same way;
  (7) Table I, p. 764: the published P_2 levels, population sizes and percentage
      contributions are consistent with (4) up to the table's rounding, except
      that row '6-10' prints 12.1% where its own n_j and P2_j give 12.2%, and
      the recomputed total 0.05574 rounds to 0.0557 rather than 0.0558 (both
      reported as findings, asserted as such below); the
      "13 per cent" headcount (p. 765) is consistent with P_0 decomposition; and
      each row satisfies P_2 >= H*I^2 (a consequence of (2)), with
      I = 1 - (average income of poor)/z.
Exit status 0 iff every check passes.
"""
import sys
import sympy as sp
from sympy import Rational as Q

ok = True


def check(name, cond):
    global ok
    print(("PASS " if cond else "FAIL ") + name)
    ok = ok and bool(cond)


def P(alpha, z, ys):
    """(3), p. 763, with the paper's weak inequality y <= z for 'poor'."""
    n = len(ys)
    return sum(((z - y) / z) ** alpha for y in ys if y <= z) / n


def H(z, ys):
    return Q(sum(1 for y in ys if y <= z), len(ys))


def I(z, ys):
    poor = [y for y in ys if y <= z]
    return sum(z - y for y in poor) / (len(poor) * z)


# (1)
z = sp.Integer(10)
ys = [Q(2), Q(5), Q(10), Q(12), Q(30), Q(7, 2)]
check("P_0 = H (household at the line counts)", P(0, z, ys) == H(z, ys) == Q(4, 6))
check("P_1 = H * I", P(1, z, ys) == H(z, ys) * I(z, ys))
check("P_1 = population mean of max(z - y, 0)/z",
      P(1, z, ys) == sum(sp.Max(z - y, 0) / z for y in ys) / len(ys))
poor = [y for y in ys if y <= z]
mean_gap_poor = sum(z - y for y in poor) / len(poor)
check("mean shortfall among the poor = z * I", mean_gap_poor == z * I(z, ys))
check("mean shortfall among the poor (normalised) != P_1 here", mean_gap_poor / z != P(1, z, ys))

# (2) equation (2)
n = sp.symbols('n', positive=True)
zz = sp.symbols('z', positive=True)
p1, p2, p3 = sp.symbols('y1 y2 y3', positive=True)
pp = [p1, p2, p3]
q = 3
Hs = q / n
Is = sum(zz - y for y in pp) / (q * zz)
mu = sum(pp) / q
Cp2 = sum((mu - y) ** 2 for y in pp) / (q * mu ** 2)
P2s = sum((zz - y) ** 2 for y in pp) / (n * zz ** 2)
check("(2): P_2 = H[I^2 + (1-I)^2 C_p^2] (symbolic, q = 3)",
      sp.simplify(P2s - Hs * (Is ** 2 + (1 - Is) ** 2 * Cp2)) == 0)

# (3) decomposability
ws = sp.symbols('w1:6', positive=True)
alpha = sp.symbols('alpha', positive=True)
g = lambda y: ((zz - y) / zz) ** alpha           # all five assumed poor
tot = sum(g(w) for w in ws) / 5
grp = Q(2, 5) * (g(ws[0]) + g(ws[1])) / 2 + Q(3, 5) * (g(ws[2]) + g(ws[3]) + g(ws[4])) / 3
check("Prop 2: P_alpha = sum (n_j/n) P_alpha^(j) (symbolic alpha, all poor)", sp.simplify(tot - grp) == 0)
mixed = [Q(1), Q(4), Q(9), Q(11), Q(3, 2), Q(20), Q(10)]
g1, g2 = mixed[:3], mixed[3:]
dec = all(P(a, z, mixed) == Q(len(g1), len(mixed)) * P(a, z, g1) + Q(len(g2), len(mixed)) * P(a, z, g2)
          for a in (0, 1, 2, 3))
check("Prop 2 on a mixed population, alpha = 0,1,2,3", dec)

# (4) Proposition 1 boundary cases
check("alpha = 0 not monotone: 1/2 -> 0 leaves P_0 = 1", P(0, 1, [Q(1, 2)]) == P(0, 1, [Q(0)]) == 1)
check("alpha > 0 monotone on the same change (alpha = 1/2, 1, 2)",
      all(P(a, 1, [Q(0)]) > P(a, 1, [Q(1, 2)]) for a in (Q(1, 2), 1, 2)))
before, after = [Q(1, 4), Q(1, 2)], [Q(1, 8), Q(5, 8)]
check("alpha = 1 transfer-neutral: P_1 = 5/8 before and after",
      P(1, 1, before) == P(1, 1, after) == Q(5, 8))
check("alpha = 2 transfer-increasing: P_2 13/32 -> 29/64",
      P(2, 1, before) == Q(13, 32) and P(2, 1, after) == Q(29, 64))
check("alpha = 3/2 transfer-increasing", P(Q(3, 2), 1, after) > P(Q(3, 2), 1, before))

# (5) I is not decomposable
ex = [Q(0), Q(1, 2), Q(2)]
Iall = I(1, ex)
Idec = Q(1, 3) * I(1, ex[:1]) + Q(2, 3) * I(1, ex[1:])
check("I overall = 3/4, share-weighted = 2/3 (not decomposable)", Iall == Q(3, 4) and Idec == Q(2, 3))
check("P_1 overall = 1/2 = share-weighted", P(1, 1, ex) == Q(1, 2) ==
      Q(1, 3) * P(1, 1, ex[:1]) + Q(2, 3) * P(1, 1, ex[1:]))


# (6) footnote 6: Sen (1976) measure, 2/((q+1) n z) sum_{i<=q} g_i (q+1-i), y ascending
def sen(z, ys):
    ys = sorted(ys)
    poor = [y for y in ys if y <= z]
    qq = len(poor)
    return Q(2) / ((qq + 1) * len(ys) * z) * sum((z - y) * (qq + 1 - i) for i, y in enumerate(poor, 1))


y_ = [1, 6, 6, 7, 8, 12]
yh = [3, 3, 6, 7, 8, 13]
y1, yh1, y2 = [1, 6, 12], [3, 3, 13], [6, 7, 8]
y_, yh, y1, yh1, y2 = ([sp.Integer(v) for v in L] for L in (y_, yh, y1, yh1, y2))
check("footnote 6: y^(1) and yhat^(1) have the same mean", sum(y1) == sum(yh1))
fn6 = True
for zv in (13, 14, 20, 100):
    zv = sp.Integer(zv)
    s_sub = sen(zv, y1) > sen(zv, yh1)
    s_tot = sen(zv, yh) > sen(zv, y_)
    fn6 = fn6 and s_sub and s_tot
    print(f"     z = {zv}: Sen(y1) - Sen(yh1) = {sen(zv, y1) - sen(zv, yh1)}, "
          f"Sen(yh) - Sen(y) = {sen(zv, yh) - sen(zv, y_)}")
check("footnote 6: Sen ranks subgroup and total oppositely (z = 13, 14, 20, 100)", fn6)
cons = True
for zv in (13, 14, 20, 100):
    for a in (0, 1, 2, 3):
        d_sub = P(a, zv, y1) - P(a, zv, yh1)
        d_tot = P(a, zv, y_) - P(a, zv, yh)
        cons = cons and sp.sign(d_sub) == sp.sign(d_tot)
check("footnote 6: every P_alpha (alpha = 0..3) ranks subgroup and total the same way", cons)

# (7) Table I, p. 764 (published figures)
rows = [  # n_j, P2_j, % contribution, avg income of poor, proportion poor
    (29, "0.4267", "5.6", "93.3", "0.55"),
    (117, "0.1237", "6.5", "221.9", "0.30"),
    (116, "0.1264", "6.6", "140.0", "0.20"),
    (438, "0.0257", "5.1", "295.1", "0.09"),
    (793, "0.0343", "12.1", "273.8", "0.11"),
    (719, "0.0291", "9.4", "286.6", "0.10"),
    (565, "0.0260", "6.6", "329.3", "0.11"),
    (954, "0.0555", "23.8", "198.8", "0.12"),
    (116, "0.1659", "8.7", "203.3", "0.35"),
    (140, "0.2461", "15.5", "93.0", "0.34"),
]
rows = [(sp.Integer(a), Q(b), Q(c), Q(d), Q(e)) for a, b, c, d, e in rows]
ntot, P2tot, pct_tot = 3987, Q("0.0558"), Q("99.9")
e4 = Q(5, 100000)          # half-unit of the 4th decimal
check("Table I: group sizes sum to 3987", sum(r[0] for r in rows) == ntot)
P2dec = sum(r[0] * r[1] for r in rows) / ntot
print(f"     sum (n_j/n) P2_j = {sp.N(P2dec, 6)} (published total 0.0558)")
check("Table I: total P2 = sum (n_j/n) P2_j within rounding", abs(P2dec - P2tot) <= 2 * e4)
check("Table I: published percentages sum to 99.9", sum(r[2] for r in rows) == pct_tot)
pct_ok = True
bad = []
for nj, p2, pct, _, _ in rows:
    lo = 100 * nj * (p2 - e4) / (ntot * (P2tot + e4))
    hi = 100 * nj * (p2 + e4) / (ntot * (P2tot - e4))
    if not (lo - Q(5, 100) <= pct <= hi + Q(5, 100)):
        bad.append((nj, pct, lo, hi))
for nj, pct, lo, hi in bad:
    print(f"     row n_j={nj}: published {pct}%, implied by its n_j and P2_j: "
          f"[{sp.N(lo, 5)}, {sp.N(hi, 5)}]%")
check("Table I: every % contribution except row '6-10' = 100 (n_j/n) P2_j / P2 within rounding",
      [b[0] for b in bad] == [793])
# Documented discrepancy in the published table (not an error in FGT's mathematics):
# row '6-10' (n_j = 793, P2_j = .0343) implies 12.2%, the table prints 12.1%.
nj, pct, lo, hi = bad[0] if bad else (0, 0, 0, 0)
check("Table I row '6-10': printed 12.1% lies outside the rounding interval implied by its n_j, P2_j (lower end > 12.15)",
      bad != [] and pct == Q("12.1") and Q("12.15") <= lo)
check("Table I: sum (n_j/n) P2_j rounds to 0.0557, not the printed 0.0558 (within 1e-4)",
      sp.floor(P2dec * 10000 + Q(1, 2)) == 557)
Hdec = sum(r[0] * r[4] for r in rows) / ntot
print(f"     sum (n_j/n) H_j = {sp.N(Hdec, 6)} (text p. 765: 'Only 13 per cent')")
check("Table I / p. 765: headcount 13 per cent consistent with P_0 decomposition",
      abs(Hdec - Q(13, 100)) <= Q(5, 1000) + Q(5, 1000))
zN = sp.Integer(515)
bound_ok = True
for nj, p2, _, ybar, h in rows:
    Ij = 1 - ybar / zN
    # P2 >= H I^2 with rounding slack on each published figure
    lhs = p2 + e4
    rhs = (h - Q(5, 1000)) * (1 - (ybar + Q(5, 100)) / zN) ** 2
    bound_ok = bound_ok and lhs >= rhs
    print(f"     n={nj}: P2={p2}, H*I^2={sp.N(h * Ij**2, 4)}")
check("Table I: every row satisfies P2 >= H*I^2 (from (2), C_p^2 >= 0)", bound_ok)

print("\nAll checks passed." if ok else "\nSOME CHECKS FAILED.")
sys.exit(0 if ok else 1)
