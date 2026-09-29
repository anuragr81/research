"""
Heckman (1998), "Detecting Discrimination", JEP 12(2) 101-116 -- the Appendix
model of the audit method (pp.112-115), Figures 1-2 (p.114) and Table 1 (p.105).

Companion to lean/Literature/Heckman.lean.

  (1) linear treatment: the pair difference is X2^1 - X2^0 + gamma (p.113);
  (2) the p.108 two-component example;
  (3) threshold hiring with two-point unobservables (the Lean examples);
  (4) Figures 1-2 with normal unobservables: the curves are reproduced by the
      CAPTIONS' parameters (Var(X2^0) = 2.25, Var(X2^1) = 1: WHITES more dispersed),
      not by the TITLES ("Blacks Have More Dispersion"); Figure 2's title also says
      "No Discrimination" although its caption has c1 = 0.25 > c0 = 0;
  (5) Table 1: every count and percentage recomputed.  Two printed entries are
      inconsistent with their own row: Denver pair 4 "White Yes, Black No" is
      printed "(2) 6.7%" (the row then sums to 16 of 15 audits and 2/15 = 13.3%;
      "(1)" reconciles the row and the Denver total of 7); Washington pair 5's
      "Equal Treatment" is printed 77.6% but (7 + 26)/42 = 78.6%.  (Denver pair 1's
      72.1% for 13/18 = 72.2% is a rounding slip.)
Normal-CDF values are evaluated to 30 digits from sympy's erf.
Exit 0 iff all checks pass.
"""
import sys
import sympy as sp

ok = True


def check(name, cond, detail=""):
    global ok
    print(f"  [{'PASS' if cond else 'FAIL'}] {name}")
    if detail:
        print(f"         {detail}")
    if not cond:
        ok = False


R = sp.Rational
g, x1, x21, x20, f = sp.symbols('gamma X1 X21 X20 f', real=True)

print("(1) linear treatment, p.113")
T = lambda P, r: P + g * r
check("T(P1*,1) - T(P0*,0) = X21 - X20 + gamma",
      sp.simplify(T(x1 + x21 + f, 1) - T(x1 + x20 + f, 0) - (x21 - x20 + g)) == 0)

print("(2) two-component example, p.108")
meanB, meanW = (1, 0), (0, 1)
check("equal mean productivity", sum(meanB) == sum(meanW))
check("audit matching component 1 finds -1 with gamma = 0 (spurious discrimination)",
      meanB[1] - meanW[1] + 0 == -1)

print("(3) threshold hiring, two-point unobservables (spreads 1 black, 3/2 white)")
hire = lambda c, P: 1 if P >= c else 0
hp = lambda c, x, d: R(hire(c, x - d) + hire(c, x + d), 2)
dB, dW = R(1), R(3, 2)
check("variance ratio 9/4 = 2.25, as in Figures 1-2", dW ** 2 / dB ** 2 == R(9, 4))
check("no discrimination, x1 = -5/4: black 0 < white 1/2",
      hp(0, R(-5, 4), dB) == 0 and hp(0, R(-5, 4), dW) == R(1, 2))
check("no discrimination, x1 = 5/4: black 1 > white 1/2",
      hp(0, R(5, 4), dB) == 1 and hp(0, R(5, 4), dW) == R(1, 2))
check("no discrimination, x1 = 2: equal", hp(0, 2, dB) == hp(0, 2, dW) == 1)
check("discrimination (c1 = 1/4), x1 = 3: equal -- disguised",
      hp(R(1, 4), 3, dB) == hp(0, 3, dW) == 1)
check("discrimination (c1 = 1/4), x1 = 5/4: black favoured -- reversed",
      hp(R(1, 4), R(5, 4), dB) > hp(0, R(5, 4), dW))

print("(4) Figures 1-2, normal unobservables, f = 0")
Phi = lambda z: (1 + sp.erf(z / sp.sqrt(2))) / 2
sd_caption_B, sd_caption_W = 1, R(3, 2)     # captions: Var(X2^0)=2.25, Var(X2^1)=1
ratio = lambda X, c1, c0, sB, sW: sp.N(Phi((X - c1) / sB) / Phi((X - c0) / sW), 30)
r1_m3 = ratio(-3, 0, 0, sd_caption_B, sd_caption_W)
r1_p1 = ratio(1, 0, 0, sd_caption_B, sd_caption_W)
r1_p3 = ratio(3, 0, 0, sd_caption_B, sd_caption_W)
check("Fig.1 caption parameters: ratio(-3) ~ 0.06, ratio(1) ~ 1.13, ratio(3) ~ 1.03 "
      "(the plotted curve)",
      abs(r1_m3 - R(6, 100)) < R(1, 100) and abs(r1_p1 - R(113, 100)) < R(1, 100)
      and abs(r1_p3 - R(103, 100)) < R(2, 100),
      f"{float(r1_m3):.4f}, {float(r1_p1):.4f}, {float(r1_p3):.4f}")
t1_m3 = ratio(-3, 0, 0, sd_caption_W, sd_caption_B)   # the TITLE: blacks more dispersed
check("Fig.1 TITLE parameters (blacks more dispersed) give ratio(-3) ~ 16.8, "
      "off the plotted axis (max 1.2): title and caption disagree; the caption is right",
      t1_m3 > 16,
      f"{float(t1_m3):.4f}")
check("p.115 text: 'black hire rate falls short of the white rate if X1* < 0' "
      "(true with caption parameters)",
      ratio(R(-1, 100), 0, 0, 1, R(3, 2)) < 1 < ratio(R(1, 100), 0, 0, 1, R(3, 2)))
r2_p2 = ratio(2, R(1, 4), 0, 1, R(3, 2))
r2_m3 = ratio(-3, R(1, 4), 0, 1, R(3, 2))
check("Fig.2 caption parameters (c1 = 0.25, c0 = 0): ratio(2) ~ 1.06 > 1, ratio(-3) ~ 0.02",
      abs(r2_p2 - R(106, 100)) < R(1, 100) and r2_m3 < R(3, 100),
      f"{float(r2_p2):.4f}, {float(r2_m3):.4f}")
print("         note: Fig.2's title says 'No Discrimination' although its caption has "
      "c1 = 0.25 > c0 = 0;\n         the text (p.115) describes Fig.2 as the "
      "discrimination case.")

print("(5) Table 1, p.105")
table = {
    'Chicago': [(35, 1, 5, 14.3, 23, 65.7, 80.0, 5, 14.3, 2, 5.7),
                (40, 2, 5, 12.5, 25, 62.5, 75.0, 4, 10.0, 6, 15.0),
                (44, 3, 3, 6.8, 37, 84.1, 90.9, 3, 6.8, 1, 2.3),
                (36, 4, 6, 16.7, 24, 66.7, 83.4, 6, 16.7, 0, 0.0),
                (42, 5, 3, 7.1, 38, 90.5, 97.6, 1, 2.4, 0, 0.0)],
    'Washington': [(46, 1, 5, 10.9, 26, 56.5, 67.4, 12, 26.1, 3, 6.5),
                   (54, 2, 11, 20.4, 31, 57.4, 77.8, 9, 16.7, 3, 5.6),
                   (62, 3, 11, 17.7, 36, 58.1, 75.8, 11, 17.7, 4, 6.5),
                   (37, 4, 6, 16.2, 22, 59.5, 75.7, 7, 18.9, 2, 5.4),
                   (42, 5, 7, 16.7, 26, 61.9, 77.6, 7, 16.7, 2, 4.8)],
    'Denver': [(18, 1, 2, 11.1, 11, 61.1, 72.1, 5, 27.8, 0, 0.0),
               (53, 2, 2, 3.8, 41, 77.4, 81.2, 0, 0.0, 10, 18.9),
               (33, 3, 7, 21.2, 25, 75.8, 97.0, 1, 3.0, 0, 0.0),
               (15, 4, 9, 60.0, 3, 20.0, 80.0, 2, 6.7, 2, 13.3),
               (26, 9, 3, 11.5, 23, 88.5, 100.0, 0, 0.0, 0, 0.0)],
}
totals = {'Chicago': (197, 22, 147, 19, 9), 'Washington': (241, 40, 141, 46, 14),
          'Denver': (145, 23, 103, 7, 12)}
bad = []
for city, rows in table.items():
    for (n, pair, a, pa, b, pb, ab, wy, pwy, by, pby) in rows:
        if a + b + wy + by != n:
            bad.append((city, pair, 'row count', a + b + wy + by, n))
        for c, pct, nm in [(a, pa, 'both'), (b, pb, 'neither'), (wy, pwy, 'WyBn'),
                           (by, pby, 'WnBy')]:
            if abs(R(100 * c, n) - sp.nsimplify(pct)) > R(1, 10):
                bad.append((city, pair, nm, float(R(100 * c, n)), pct))
        if abs(R(100 * (a + b), n) - sp.nsimplify(ab)) > R(1, 10):
            bad.append((city, pair, 'a/b', float(R(100 * (a + b), n)), ab))
    col = [sum(r[i] for r in rows) for i in (0, 2, 4, 7, 9)]
    if tuple(col) != totals[city]:
        bad.append((city, 'Total', 'column sums', col, totals[city]))
for b_ in bad:
    print("         inconsistent:", b_)
expected = {('Denver', 4, 'row count'), ('Denver', 4, 'WyBn'), ('Washington', 5, 'a/b'),
            ('Denver', 1, 'a/b'), ('Denver', 'Total', 'column sums')}
check("the only inconsistencies (tolerance 0.1 pt) are Denver pair 4 WyBn (count), "
      "Washington pair 5 a/b (1 pt) and Denver pair 1 a/b (0.12 pt)",
      {b_[:3] for b_ in bad} == expected)
fixed = [r for r in table['Denver']]
fixed[3] = (15, 4, 9, 60.0, 3, 20.0, 80.0, 1, 6.7, 2, 13.3)
check("Denver pair 4 with '(1)' in place of '(2)': row sums to 15, 1/15 = 6.7%, and the "
      "Denver WyBn column sums to the printed total 7",
      9 + 3 + 1 + 2 == 15 and abs(R(100, 15) - R(67, 10)) < R(1, 10)
      and sum(r[7] for r in fixed) == 7)
check("p.105 'In Chicago and Denver this happened about 86 percent of the time'",
      abs(R(22 + 147, 197) - R(86, 100)) < R(1, 100)
      and abs(R(23 + 103, 145) - R(87, 100)) < R(1, 100))

print()
print("All checks passed." if ok else "SOME CHECKS FAILED.")
sys.exit(0 if ok else 1)
