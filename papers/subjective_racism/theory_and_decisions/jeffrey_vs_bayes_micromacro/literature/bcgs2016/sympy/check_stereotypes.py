"""
Bordalo, Coffman, Gennaioli & Shleifer (2016), "Stereotypes" -- May 2015 working
paper, printed pages (printed = PDF - 1).

Companion to lean/Literature/BCGS.lean.  Exact rational arithmetic throughout.

  (1) Definition 1 / eq. (2): R(t,G) = prior odds x likelihood ratio (symbolic);
  (2) the worked examples: Florida (p.4), Republicans/Democrats (p.45-46),
      Americans/Europeans (p.46), the medical test (p.25);
  (3) the experiment designs' counts, means and representativeness (pp.8-10,
      54-59, 64, Table 2);
  (4) Proposition 2 on a discrete MLR family, and the zero-mass counterexample;
  (5) Proposition 3 on a symmetric U-shaped case;
  (6) Proposition 5's threshold algebra (pp.33, 44-45);
  (7) the Irish examples (p.28) and the Section 4.3 welfare example of the Lean file:
      pooled correlation exaggerated, within-group association removed;
  (8) BIR's eq. (30) (their p.80) is BCGS fn 17's smooth discounting with
      delta(x) = x^theta, and theta = 0 recovers the truth.
Exit 0 iff all checks pass.
"""
import sys
import sympy as sp
from fractions import Fraction as Fr

ok = True


def check(name, cond, detail=""):
    global ok
    print(f"  [{'PASS' if cond else 'FAIL'}] {name}")
    if detail:
        print(f"         {detail}")
    if not cond:
        ok = False


def argmax(d):
    m = max(d.values())
    return {k for k, v in d.items() if v == m}


def stereo(pi, recalled):
    z = sum(pi[t] for t in recalled)
    return {t: (pi[t] / z if t in recalled else Fr(0)) for t in pi}


def top_d(lr, d):
    """Definition 2: the d most representative types, ties at the d-th included."""
    vals = sorted(lr.values(), reverse=True)
    cut = vals[d - 1]
    return {t for t, v in lr.items() if v >= cut}


def mean(pi, x=lambda t: t):
    return sum(p * x(t) for t, p in pi.items()) / sum(pi.values())


def var(pi):
    m = mean(pi)
    return sum(p * (t - m) ** 2 for t, p in pi.items()) / sum(pi.values())


print("(1) Definition 1 and eq. (2)")
w, pG, pN = sp.symbols('w pi_G pi_nG', positive=True)
PrT = w * pG + (1 - w) * pN
R = (w * pG / PrT) / ((1 - w) * pN / PrT)
check("R(t,G) = w/(1-w) * pi_{t,G}/pi_{t,-G}", sp.simplify(R - w / (1 - w) * pG / pN) == 0)

print("(2) worked examples")
FL = {'0-18': Fr(239, 1000), '19-44': Fr(316, 1000), '45-64': Fr(270, 1000), '65+': Fr(173, 1000)}
US = {'0-18': Fr(266, 1000), '19-44': Fr(334, 1000), '45-64': Fr(265, 1000), '65+': Fr(135, 1000)}
check("Florida (p.4): 65+ maximises Pr(t|FL)/Pr(t|US); 19-44 is modal",
      argmax({t: FL[t] / US[t] for t in FL}) == {'65+'} and argmax(FL) == {'19-44'})
check("Florida table columns sum to 99.8% and 100%", sum(FL.values()) == Fr(998, 1000)
      and sum(US.values()) == 1, "the Florida row sums to 99.8% (rounding in the source)")
Rep = {'Cr': Fr(58, 100), 'Ev': Fr(5, 100), 'God': Fr(31, 100)}
Dem = {'Cr': Fr(41, 100), 'Ev': Fr(19, 100), 'God': Fr(32, 100)}
check("Gallup (pp.45-46): Creationism is both modal and most representative for "
      "Republicans", argmax(Rep) == argmax({t: Rep[t] / Dem[t] for t in Rep}) == {'Cr'})
check("... Evolution is most representative but not modal for Democrats; ratio 3.8 "
      "('four times more prevalent')",
      argmax({t: Dem[t] / Rep[t] for t in Dem}) == {'Ev'} and argmax(Dem) == {'Cr'}
      and Dem['Ev'] / Rep['Ev'] == Fr(19, 5))
Am = {'work': Fr(49, 52), 'vac': Fr(3, 52)}
Eu = {'work': Fr(47, 52), 'vac': Fr(5, 52)}
check("Americans/Europeans (p.46): 49/52 ~ 0.94, 47/52 ~ 0.90; stereotypes work / vacation",
      abs(Am['work'] - Fr(94, 100)) < Fr(1, 100) and abs(Eu['work'] - Fr(90, 100)) < Fr(1, 100)
      and argmax({t: Am[t] / Eu[t] for t in Am}) == {'work'}
      and argmax({t: Eu[t] / Am[t] for t in Eu}) == {'vac'})
prev, tpr, fpr = Fr(5, 100), Fr(9, 10), Fr(5, 100)
pos = {'s': prev * tpr, 'h': (1 - prev) * fpr}
neg = {'s': prev * (1 - tpr), 'h': (1 - prev) * (1 - fpr)}
post_pos = pos['s'] / sum(pos.values())
check("medical test (p.25): condition (4) holds; Bayes P(sick|+) = 18/37 ~ 0.49, "
      "while the d = 1 stereotype of '+' is 'sick' with probability 1",
      tpr / fpr > 1 > (1 - tpr) / (1 - fpr) and post_pos == Fr(18, 37)
      and argmax({t: (pos[t] / sum(pos.values())) / (neg[t] / sum(neg.values())) for t in pos}) == {'s'})

print("(3) experiments")
blue = {1: 3, 2: 8, 3: 24, 4: 14, 5: 1}
redC = {1: 3, 2: 9, 3: 23, 4: 14, 5: 1}
redR = {1: 4, 2: 11, 3: 20, 4: 10, 5: 5}
check("Table 2 (p.64): totals 50 and means 3.04, 3.02, 3.02",
      all(sum(g.values()) == 50 for g in (blue, redC, redR))
      and [mean({t: Fr(c) for t, c in g.items()}) for g in (blue, redC, redR)]
      == [Fr(304, 100), Fr(302, 100), Fr(302, 100)])
check("Table 2: height 5 is the most representative type of the Rep. red group",
      argmax({t: Fr(redR[t], blue[t]) for t in blue}) == {5})
check("50-shapes (pp.54-55): counts 50; Control: circles modal+representative for blue, "
      "squares for red; Rep.: red triangle most representative, square modal",
      sum((22, 24, 4)) == sum((26, 20, 4)) == sum((21, 16, 13)) == 50
      and argmax({'sq': Fr(22, 26), 'ci': Fr(24, 20), 'tr': Fr(4, 4)}) == {'ci'}
      and argmax({'sq': Fr(26, 22), 'ci': Fr(20, 24), 'tr': Fr(4, 4)}) == {'sq'}
      and argmax({'sq': Fr(21, 22), 'ci': Fr(16, 24), 'tr': Fr(13, 4)}) == {'tr'})
check("25-shapes (p.56): counts 25; red triangles 9:8 (Control) vs 9:2 (Rep.); "
      "total triangles 17 vs 11",
      sum((6, 10, 9)) == sum((9, 8, 8)) == sum((11, 12, 2)) == 25
      and 9 + 8 == 17 and 9 + 2 == 11)
check("T-shirts (p.8): 13 purple + 12 green = 25; only girls wear green in Rep.",
      13 + 12 == 25)

print("(4) Proposition 2")
N = 6
bino = lambda n, p: {t: sp.Rational(sp.binomial(n - 1, t - 1)) * p ** (t - 1) * (1 - p) ** (n - t)
                     for t in range(1, n + 1)}
piG = {t: Fr(str(v)) for t, v in bino(N, sp.Rational(11, 20)).items()}
piN = {t: Fr(str(v)) for t, v in bino(N, sp.Rational(1, 2)).items()}
lr = {t: piG[t] / piN[t] for t in piG}
check("binomial shift: likelihood ratio strictly increasing (MLRP)",
      all(lr[t] < lr[t + 1] for t in range(1, N)))
for d in (1, 2, 3, 5):
    S = top_d(lr, d)
    st = stereo(piG, S)
    check(f"d = {d}: stereotype = right tail {sorted(S)}; E^st(t|G) > E(t|G) > E(t|-G)",
          S == set(range(N - d + 1, N + 1)) and mean(st) > mean(piG) > mean(piN),
          f"{float(mean(st)):.4f} > {float(mean(piG)):.4f} > {float(mean(piN)):.4f}")
cG, cN = {0: Fr(0), 1: Fr(1)}, {0: Fr(1, 2), 1: Fr(1, 2)}
S = top_d({t: cG[t] / cN[t] for t in cG}, 1)
check("counterexample: pi_G = (0,1), pi_-G = (1/2,1/2): strict MLR, d = 1, "
      "yet E^st(t|G) = E(t|G) (Prop. 2's strict '>' needs mass in the truncated tail)",
      S == {1} and mean(stereo(cG, S)) == mean(cG) == 1)

print("(5) Proposition 3 (symmetric, U-shaped likelihood ratio)")
piG3 = {1: Fr(2, 10), 2: Fr(15, 100), 3: Fr(15, 100), 4: Fr(15, 100), 5: Fr(15, 100), 6: Fr(2, 10)}
piN3 = {1: Fr(1, 10), 2: Fr(15, 100), 3: Fr(25, 100), 4: Fr(25, 100), 5: Fr(15, 100), 6: Fr(1, 10)}
lr3 = {t: piG3[t] / piN3[t] for t in piG3}
S3 = top_d(lr3, 2)
check("U-shaped LR, d = 2: stereotype = both tails {1, 6}; Var^st > Var(G) > Var(-G)",
      S3 == {1, 6} and var(stereo(piG3, S3)) > var(piG3) > var(piN3),
      f"{float(var(stereo(piG3, S3))):.4f} > {float(var(piG3)):.4f} > {float(var(piN3)):.4f}")
S3b = top_d({t: 1 / v for t, v in lr3.items()}, 2)
check("... and for -G (inverse-U): stereotype = middle {3, 4}; Var^st < Var(-G) < Var(G)",
      S3b == {3, 4} and var(stereo(piN3, S3b)) < var(piN3) < var(piG3))

print("(6) Proposition 5 threshold")
a, Ad, A = sp.symbols('a A_d A', positive=True)
lhs = ((a + 1) / (Ad + 1) - a / Ad) - ((a + 1) / (A + 1) - a / A)
check("over-reaction difference = (A-Ad)(A Ad - a(A+Ad+1)) / (A Ad (A+1)(Ad+1))",
      sp.simplify(lhs - (A - Ad) * (A * Ad - a * (A + Ad + 1)) / (A * Ad * (A + 1) * (Ad + 1))) == 0)
check("threshold A_d/(1+A_d+A) < 1/2 whenever A_d < A",
      sp.simplify(sp.Rational(1, 2) - Ad / (1 + Ad + A) - (1 + A - Ad) / (2 * (1 + Ad + A))) == 0)

print("(7) Section 4.3")
ri, re, c, rs, ci, cs = sp.symbols('r_i r_e c r_s c_i c_s', positive=True)
check("Irish vs Europeans: R(r,c) = R(r,o^) = r_i/r_e and R(o,c) = R(o,o^) = (1-r_i)/(1-r_e)",
      sp.simplify(ri * c / (re * c) - ri * (1 - c) / (re * (1 - c))) == 0
      and sp.simplify((1 - ri) * c / ((1 - re) * c) - (1 - ri) / (1 - re)) == 0)
check("r_i/r_e > (1-r_i)/(1-r_e) iff r_i > r_e (for r's in (0,1))",
      sp.factor(ri / re - (1 - ri) / (1 - re)) == sp.factor((ri - re) / (re * (1 - re))))
G = {(0, 0): Fr(3, 5) * Fr(7, 10), (0, 1): Fr(3, 5) * Fr(3, 10),
     (1, 0): Fr(2, 5) * Fr(9, 10), (1, 1): Fr(2, 5) * Fr(1, 10)}
Nn = {(0, 0): Fr(2, 5) * Fr(4, 5), (0, 1): Fr(2, 5) * Fr(1, 5),
      (1, 0): Fr(3, 5) * Fr(19, 20), (1, 1): Fr(3, 5) * Fr(1, 20)}


def corr2(p):
    Ee, Ew = p[(1, 0)] + p[(1, 1)], p[(0, 1)] + p[(1, 1)]
    cv = p[(1, 1)] - Ee * Ew
    return cv, cv * cv / (Ee * (1 - Ee) * Ew * (1 - Ew))


lrG = {k: G[k] / Nn[k] for k in G}
lrN = {k: Nn[k] / G[k] for k in G}
check("welfare example: G conditionally more on welfare at each education level",
      Fr(3, 10) > Fr(1, 5) and Fr(1, 10) > Fr(1, 20))
check("G's exemplar is (e=0,w=1), -G's is (e=1,w=0)",
      top_d(lrG, 1) == {(0, 1)} and top_d(lrN, 1) == {(1, 0)})
T = {k: (G[k] + Nn[k]) / 2 for k in G}
S1 = {k: (stereo(G, top_d(lrG, 1))[k] + stereo(Nn, top_d(lrN, 1))[k]) / 2 for k in G}
S2 = {k: (stereo(G, top_d(lrG, 2))[k] + stereo(Nn, top_d(lrN, 2))[k]) / 2 for k in G}
cT, rT = corr2(T)
c1, r1 = corr2(S1)
c2, r2 = corr2(S2)
check("true pooled law (37/100, 13/100, 93/200, 7/200), corr^2 = 361/5511",
      [T[k] for k in sorted(T)] == [Fr(37, 100), Fr(13, 100), Fr(93, 200), Fr(7, 200)]
      and rT == Fr(361, 5511))
check("d = 1 stereotyped pooled law (0, 1/2, 1/2, 0): corr = -1",
      [S1[k] for k in sorted(S1)] == [0, Fr(1, 2), Fr(1, 2), 0] and c1 < 0 and r1 == 1)
check("d = 2 stereotyped pooled law (16/89, 9/22, 57/178, 1/11): corr^2 > 3 corr_true^2",
      [S2[k] for k in sorted(S2)] == [Fr(16, 89), Fr(9, 22), Fr(57, 178), Fr(1, 11)]
      and c2 < 0 and r2 > 3 * rT, f"{float(r2):.4f} vs {float(rT):.4f}")
check("welfare rates: true Pr(w=1|G) = 11/50, Pr(w=1|-G) = 11/100; d = 1 stereotypes 1 and 0",
      G[(0, 1)] + G[(1, 1)] == Fr(11, 50) and Nn[(0, 1)] + Nn[(1, 1)] == Fr(11, 100))

def det(p):
    return p[(0, 0)] * p[(1, 1)] - p[(0, 1)] * p[(1, 0)]


check("for a 2x2 law the covariance is the determinant p00 p11 - p01 p10 "
      "(Paper B's assoc)", corr2(T)[0] == det(T) and corr2(S2)[0] == det(S2))
sG2, sN2 = stereo(G, top_d(lrG, 2)), stereo(Nn, top_d(lrN, 2))
check("within each group the d = 2 stereotype has ZERO association (true: -6/125, -9/250)",
      det(sG2) == 0 and det(sN2) == 0 and det(G) == Fr(-6, 125) and det(Nn) == Fr(-9, 250))

print("(8) smooth discounting")
th = sp.symbols('theta', nonnegative=True)
pis = sp.symbols('p1:4', positive=True)
qs = sp.symbols('q1:4', positive=True)
Z = sum(p * (p / qq) ** th for p, qq in zip(pis, qs))
st = [p * (p / qq) ** th / Z for p, qq in zip(pis, qs)]
check("BIR eq.(30) = fn 17 with delta(x) = x^theta; sums to 1; theta = 0 gives the truth",
      sp.simplify(sum(st) - 1) == 0
      and all(sp.simplify(sv.subs(th, 0) - p / sum(pis)) == 0 for sv, p in zip(st, pis)))

print()
print("All checks passed." if ok else "SOME CHECKS FAILED.")
sys.exit(0 if ok else 1)
