"""
Bohren, Imas & Rosenberg (2019), "The Dynamics of Discrimination" -- the
normal-normal model of Section 2 and Appendix A.1, and the arithmetic of the
experimental tables.  Pages are the January 2019 working paper's printed pages
(printed = PDF - 1).

Companion to lean/Literature/BohrenImasRosenberg.lean.  Symbolic checks are exact;
the Proposition 3 instance evaluates closed-form exponentials to 50 digits.

  (1) conjugacy by completing the square (pp.15-16) and eq. (5) (p.16);
  (2) eq. (12) (p.50) simplifies to tau_a/(tau_a+tau_eps): the belief gap
      contracts by a factor free of the evaluation and of tau_eta;
  (3) endogenous contraction < exogenous contraction (fn 10 vs p.2 "speeds up");
  (4) discrimination strictly decreases across periods (the step the proof of
      Proposition 2, p.51, does not complete);
  (5) Proposition 3's derivative condition (p.54) equals the Lean form, and the
      exponent of C1D1/(C2D2) displayed on p.54 matches the normalising constants;
  (6) the monotonicity words on p.54 ("increasing in v1 and decreasing in s2") are
      the reverse of what the constants do; the limits and Proposition 3 are right;
  (7) a full-model reversal: D(v1,s2) < 0 for small p, > 0 for large p;
  (8) Tables 1-6: interaction coefficients equal differences of the separate
      regressions (saturated models), up to rounding; p.28 reputations.
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


ta, te, tn, tq = sp.symbols('tau_a tau_eps tau_eta tau_q', positive=True)
mu, s, q, v, muM, muF, cF = sp.symbols('mu s q v mu_M mu_F c_F', real=True)

tauQ = lambda a: a * te / (a + te)
tEE = te * tn / (te + tn)
post = lambda tq_, mu_, s_: (tq_ * mu_ + tn * s_) / (tq_ + tn)

print("(1) conjugacy and eq. (5)")
lhs = tq * (q - mu) ** 2 + tn * (s - q) ** 2
rhs = (tq + tn) * (q - post(tq, mu, s)) ** 2 + tq * tn / (tq + tn) * (s - mu) ** 2
check("completing the square", sp.simplify(lhs - rhs) == 0)
D1 = (post(tq, muM, s) - 0) - (post(tq, muF, s) - cF)
check("eq.(5): D = tq/(tq+tn)(muM-muF) + cF, signal-free",
      sp.simplify(D1 - (tq / (tq + tn) * (muM - muF) + cF)) == 0)
check("Prop 1: dD/dtau_eta = -tq(muM-muF)/(tq+tn)^2 (< 0 iff muM > muF)",
      sp.simplify(sp.diff(tq / (tq + tn) * (muM - muF) + cF, tn)
                  + tq * (muM - muF) / (tq + tn) ** 2) == 0)
check("Prop 1: limit tau_eta -> oo is cF",
      sp.limit(tq / (tq + tn) * (muM - muF) + cF, tn, sp.oo) == cF)

print("(2) eq. (12) and its simplification")
req = lambda tq_, mu_, v_: (tq_ + tn) / tn * v_ - tq_ / tn * mu_
nxt = lambda a, mu_, v_: (a * mu_ + tEE * req(tauQ(a), mu_, v_)) / (a + tEE)
gap_endo = sp.simplify(nxt(ta, muM, v) - nxt(ta, muF, v))
eq12 = (ta / (ta + tEE) - tEE * tauQ(ta) / ((ta + tEE) * tn)) * (muM - muF)
check("eq.(12) as printed", sp.simplify(gap_endo - eq12) == 0)
check("eq.(12) coefficient = tau_a/(tau_a+tau_eps)",
      sp.simplify(gap_endo - ta / (ta + te) * (muM - muF)) == 0,
      f"gap = {sp.factor(gap_endo)}")
check("reqSignal inverts the evaluation",
      sp.simplify(post(tauQ(ta), mu, req(tauQ(ta), mu, v)) - v) == 0)

print("(3) endogenous vs exogenous contraction")
exo = ta / (ta + tEE)
endo = ta / (ta + te)
diff = sp.factor(sp.simplify(exo - endo))
check("exo - endo = tau_a*tau_eps^2/((ta+te)(ta te + ta tn + te tn)) > 0",
      sp.simplify(diff - ta * te ** 2 / ((ta + te) * (ta * te + ta * tn + te * tn))) == 0,
      f"{diff}")

print("(4) discrimination decreases (proof of Prop 2, p.51)")
a2 = sp.symbols('a2', positive=True)
w = lambda a: tauQ(a) / (tauQ(a) + tn)
d = sp.factor(sp.simplify(w(ta) - w(a2) * ta / (ta + te)))
num, den = sp.fraction(d)
check("D_t - D_{t+1} > 0 for ANY next precision a2 > 0 (numerator, denominator "
      "are sums of positive monomials)",
      all(c > 0 for c in sp.Poly(sp.expand(num), ta, te, tn, a2).coeffs())
      and all(c > 0 for c in sp.Poly(sp.expand(den), ta, te, tn, a2).coeffs()),
      f"difference = {d}")

print("(5) Proposition 3: derivative condition and the p.54 exponent")
A, B, g = sp.symbols('A B g', positive=True)
m2_f1 = ta / (ta + te) * g
m1_m2 = tEE * tauQ(ta) / (tn * (ta + tEE)) * g
lean_side = sp.simplify(A * m1_m2 - B * m2_f1)          # > 0 iff condition
paper_side = te ** 2 / ((te + tn) * (ta + te)) * (1 + A / B) - 1
ratio = sp.simplify(lean_side / paper_side)
check("B(m2-f1) < A(m1-m2)  <=>  1 < te^2/((te+tn)(ta+te)) (1 + A/B): the two "
      "differ by a positive factor",
      sp.simplify(ratio - B * ta * g * (te + tn) / (ta * te + ta * tn + te * tn)) == 0,
      f"ratio = {ratio}")

# normalising constants, p.52-53
mu2F, mu1F, v1, s2 = sp.symbols('mu2F mu1F v1 s2', real=True)
tq2 = (ta + tEE) * te / (ta + tEE + te)
s11 = req(tauQ(ta), mu1F, v1)       # heuristic-type required signal
s21 = req(tauQ(ta), mu2F, v1)       # impartial-type required signal
m1 = (ta * mu2F + tEE * s11) / (ta + tEE)
m2 = (ta * mu2F + tEE * s21) / (ta + tEE)
logC = lambda si, mi: -sp.Rational(1, 2) * (ta * mu2F ** 2 + si ** 2 * tEE - (ta + tEE) * mi ** 2)
mq = lambda mi: (tq2 * mi + tn * s2) / (tq2 + tn)
logD = lambda mi: -sp.Rational(1, 2) * (tq2 * mi ** 2 + s2 ** 2 * tn - (tq2 + tn) * mq(mi) ** 2)
log_ratio = logC(s11, m1) + logD(m1) - logC(s21, m2) - logD(m2)
printed = (-sp.Rational(1, 2) * tEE * (s11 ** 2 - s21 ** 2)
           + sp.Rational(1, 2) * (ta + tEE - tq2) * (m1 ** 2 - m2 ** 2)
           + sp.Rational(1, 2) * (tq2 + tn) * (mq(m1) ** 2 - mq(m2) ** 2))
check("p.54 displayed exponent of C1D1/(C2D2) equals log of the constants",
      sp.simplify(log_ratio - printed) == 0)
# the constants as proper marginal likelihoods (s ~ N(mu, 1/ta + 1/tEE))
V1 = 1 / ta + 1 / tEE
marg = lambda si: -(si - mu2F) ** 2 / (2 * V1)
check("log C1 - log C2 equals the normal marginal-likelihood ratio",
      sp.simplify(logC(s11, m1) - logC(s21, m2) - (marg(s11) - marg(s21))) == 0)

print("(6) monotonicity of C1D1/(C2D2) (p.54 text says: increasing in v1, "
      "decreasing in s2)")
dv = sp.simplify(sp.diff(log_ratio, v1))
ds = sp.simplify(sp.diff(log_ratio, s2))
gap = mu2F - mu1F
check("d/dv1 log(C1D1/C2D2) = -(positive)*(mu2F - mu1F): DECREASING in v1 when "
      "the heuristic type is partial (mu1F < mu2F)",
      sp.simplify(dv / gap).is_negative is True or
      all(c < 0 for c in sp.Poly(sp.numer(sp.together(dv / gap)), ta, te, tn).coeffs()) and
      all(c > 0 for c in sp.Poly(sp.denom(sp.together(dv / gap)), ta, te, tn).coeffs()),
      f"d/dv1 = {sp.factor(dv)}")
check("d/ds2 log(C1D1/C2D2) = +(positive)*(mu2F - mu1F): INCREASING in s2",
      all(c > 0 for c in sp.Poly(sp.numer(sp.together(ds / gap)), ta, te, tn).coeffs()) and
      all(c > 0 for c in sp.Poly(sp.denom(sp.together(ds / gap)), ta, te, tn).coeffs()),
      f"d/ds2 = {sp.factor(ds)}")
print("         => the ratio is large as v1 -> -oo and s2 -> +oo (as the paper's "
      "limits and Proposition 3 say);\n            the words 'increasing in v1 and "
      "decreasing in s2' on p.54 have both directions reversed.")

print("(7) full-model reversal instance (tau's = 1, mu_M = 1, mu1F = 0)")
subs = {ta: 1, te: 1, tn: 1, mu2F: 1, mu1F: 0}
p = sp.symbols('p', positive=True)


def D_of(pv, v1v, s2v):
    sb = {**subs, v1: v1v, s2: s2v}
    CD1 = sp.exp((logC(s11, m1) + logD(m1)).subs(sb))
    CD2 = sp.exp((logC(s21, m2) + logD(m2)).subs(sb))
    gam = (pv * CD1 * m1.subs(sb) + (1 - pv) * CD2 * m2.subs(sb)) / (pv * CD1 + (1 - pv) * CD2)
    f1 = ((ta * mu1F + tEE * s11) / (ta + tEE)).subs(sb)
    return sp.N(m2.subs(sb) - pv * f1 - (1 - pv) * gam, 50), sp.N(gam - m2.subs(sb), 50)


rho = sp.N(sp.exp(log_ratio.subs({**subs, v1: -6, s2: 4})), 30)
Ds, gap_imp = D_of(sp.Rational(1, 20), -6, 4)
Dl, _ = D_of(sp.Rational(9, 10), -6, 4)
check("v1 = -6, s2 = 4: C1D1/C2D2 > 3 (the derivative condition at unit precisions)",
      rho > 3, f"C1D1/C2D2 = {float(rho):.4f}")
check("v1 = -6, s2 = 4: D < 0 at p = 1/20 (reversal) and D > 0 at p = 9/10",
      Ds < 0 and Dl > 0, f"D(1/20) = {float(Ds):+.5f}, D(9/10) = {float(Dl):+.5f}")
check("p.20: at p = 1/20 the impartial type's mixture mean exceeds mu_M(v1)",
      gap_imp > 0, f"gamma - mu_M(v1) = {float(gap_imp):+.5f}")
check("D = 0 at p = 0", abs(D_of(0, -6, 4)[0]) < sp.Float('1e-40'))

print("(8) tables")
R = sp.Rational
tol = R(2, 100)
tabs = [
    ("T1 dRep  Male*Question = Q - A", R(424, 100), R(286, 100) - R(-138, 100)),
    ("T1 Votes Male*Question", R(89, 100), R(58, 100) - R(-31, 100)),
    ("T1 dRep  Question = constQ - constA", R(8, 100), R(468, 100) - R(460, 100)),
    ("T1 Votes Question", R(9, 100), R(88, 100) - R(79, 100)),
    ("T2 dRep  Male*Advanced = Adv - Nov", R(-602, 100), R(-316, 100) - R(286, 100)),
    ("T2 Votes Male*Advanced", R(-120, 100), R(-62, 100) - R(58, 100)),
    ("T2 dRep  Advanced = constAdv - constNov", R(233, 100), R(701, 100) - R(468, 100)),
    ("T2 Votes Advanced", R(49, 100), R(138, 100) - R(88, 100)),
    ("T3 Male*Question", R(77, 100), R(57, 100) - R(-20, 100)),
    ("T4 Male*Advanced", R(-120, 100), R(-64, 100) - R(57, 100)),
    ("T4 Advanced", R(45, 100), R(142, 100) - R(97, 100)),
    ("T5 dRep  Male*Question", R(332, 100), R(217, 100) - R(-115, 100)),
    ("T5 Votes Male*Question", R(72, 100), R(44, 100) - R(-28, 100)),
    ("T6 dRep  Male*Advanced", R(-475, 100), R(-258, 100) - R(217, 100)),
    ("T6 Votes Male*Advanced", R(-95, 100), R(-51, 100) - R(44, 100)),
    ("T6 dRep  Advanced", R(164, 100), R(477, 100) - R(313, 100)),
]
for name, printed_, implied in tabs:
    check(f"{name}: printed {float(printed_):+.2f}, implied {float(implied):+.2f}",
          abs(printed_ - implied) <= tol)
check("p.28: mean advanced reputation 155.23 = mean of 155.89 and 154.57 (70 + 70)",
      (R(15589, 100) + R(15457, 100)) / 2 == R(15523, 100))
check("p.29/Table 2: 280 questions - 7 dropped = 273 = 135 novice + 138 advanced",
      280 - 7 == 273 == 135 + 138)
check("p.31/Table 1: 140 answers - 5 dropped = 135", 140 - 5 == 135)

print()
print("All checks passed." if ok else "SOME CHECKS FAILED.")
sys.exit(0 if ok else 1)
