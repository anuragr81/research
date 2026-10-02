"""
The ladder under partial adoption: which orders in c each belief statistic
carries when the second cue is adopted with weight omega (code name delta),
and which statistics stay protected at second order for every omega.

Symbolic in the prior (alpha, beta), the cues (q0, r0) and the weight delta
unless a row says "generic point".  Taylor coefficients in c are taken cell by
cell and combined by the product rule, as in verify_interior_omega.py.

Rows:
  (A) at c = 0 both routes end at product measures, q (x) t and s (x) r with
      t = (1-delta) beta + delta r and s = (1-delta) alpha + delta q, and
      their gap is (1-delta)(beta-r0) R1 + (1-delta)(q0-alpha) R2, a vector
      of span{R1, R2}.  Marginals therefore differ at zeroth order.
  (B) a statistic protected across an open set of priors, F = F0 + assoc*h
      with h smooth, has a sequence effect that is zero at c = 0 for every
      delta and first order for delta < 1, vanishing at delta = 1 (ORD).
      Tested for h = 1, 1 + P00, (1+P11)/(mA0 mA1), 1/(mA0 mA1) (the
      conditional difference P(B=1|A=1) - P(B=1|A=0), which equals
      assoc/(mA0 mA1) exactly), and the correlation coefficient's normaliser
      1/sqrt(mA0 mA1 mB0 mB1).
  (C) the c^1 coefficient of each route's association is q0 q1 t0 t1 / Z and
      s0 s1 r0 r1 / Z, Z = alpha(1-alpha)beta(1-beta); so for h = g/mprod,
      mprod = mA0 mA1 mB0 mB1, the c^1 sequence effect is
      [g(q (x) t) - g(s (x) r)] / Z, zero for every delta iff g is constant
      across product measures.  Hence assoc/mprod has a second-order sequence
      effect for every delta (and third order at delta = 1), and at every
      product measure grad(assoc/mprod) = grad(log odds ratio): the
      all-omega protected class is the first-order shadow of the odds ratio.
  (D) a non-protected statistic, P(A=1) + assoc, moves at zeroth order.
  (E) the odds ratio of either route equals the prior's for every delta and c.
  (F) factor inputs (the benchmark reading).  Each cue supplies a factor on its
      own attribute, a_i = q_i/P(A=i), b_j = r_j/P(B=j); the belief is the prior
      with cell (i,j) multiplied by the applied factors and renormalised.  With
      both applied in full the two sequences coincide for every c.  When the
      factor read second is adopted in part, replaced by an arbitrary positive
      a' or b' (instantiated as a^delta, b^delta), the sequences end at c = 0
      at product measures whose A-marginals agree iff a' has the ratio of a;
      the association is c K / S^2 exactly (K the product of the applied
      factors, S the normalising sum), zero at c = 0 and first order
      generically; assoc/mprod has first-order factor 1/Z on both routes; and
      the odds ratio is the prior's for every c.
"""
import sympy as sp
from jeffrey_core import *
from check_zero_slope_identification import damped_route_AB, damped_route_BA, delta


def c01(e):
    """(c^0, c^1) coefficients of a rational e about c = 0, via N/D."""
    N, D = sp.fraction(sp.together(e))
    N0, D0 = N.subs(c, 0), D.subs(c, 0)
    N1, D1 = sp.diff(N, c).subs(c, 0), sp.diff(D, c).subs(c, 0)
    return sp.cancel(N0 / D0), sp.cancel((N1 * D0 - N0 * D1) / D0**2)


def assoc(Q):
    return Q[0, 0] * Q[1, 1] - Q[0, 1] * Q[1, 0]


def mprod(Q):
    mA, mB = marg_A(Q), marg_B(Q)
    return mA[0] * mA[1] * mB[0] * mB[1]


def odds(Q):
    return Q[0, 0] * Q[1, 1] / (Q[0, 1] * Q[1, 0])


def main():
    ck = Check("The ladder under partial adoption -- orders in c, symbolic in the prior")
    QAB, QBA = damped_route_AB(delta), damped_route_BA(delta)
    AB0 = sp.Matrix(2, 2, lambda i, j: sp.cancel(QAB[i, j].subs(c, 0)))
    BA0 = sp.Matrix(2, 2, lambda i, j: sp.cancel(QBA[i, j].subs(c, 0)))
    half = sp.Rational(1, 2)
    Z = alpha * (1 - alpha) * beta * (1 - beta)

    # ---- (A) product measures at c = 0 and the gap in span{R1, R2} ------
    bvec, avec = [beta, 1 - beta], [alpha, 1 - alpha]
    t = [(1 - delta) * bvec[j] + delta * r[j] for j in range(2)]
    s = [(1 - delta) * avec[i] + delta * q[i] for i in range(2)]
    ck.mat_eq("(A) route AB at c=0 is q (x) t, t = (1-delta) beta + delta r",
              AB0, sp.Matrix(2, 2, lambda i, j: q[i] * t[j]))
    ck.mat_eq("(A) route BA at c=0 is s (x) r, s = (1-delta) alpha + delta q",
              BA0, sp.Matrix(2, 2, lambda i, j: s[i] * r[j]))
    R1 = sp.Matrix(2, 2, lambda i, j: q[i] * (1 if j == 0 else -1))
    R2 = sp.Matrix(2, 2, lambda i, j: (1 if i == 0 else -1) * r[j])
    gap0 = AB0 - BA0
    ck.mat_eq("(A) gap at c=0 = (1-delta)(beta-r0) R1 + (1-delta)(q0-alpha) R2",
              gap0, (1 - delta) * (beta - r0) * R1 + (1 - delta) * (q0 - alpha) * R2)
    gA = sp.factor(gap0[1, 0] + gap0[1, 1])
    ck.eq("(A) A-marginal gap at c=0 is (1-delta)(alpha-q0)", gA, (1 - delta) * (alpha - q0))
    ck.ne("(A) ...nonzero at delta=1/2", gA.subs(delta, half))

    # ---- (C) first: the c^1 coefficients of assoc on each route ---------
    aAB0, aAB1 = c01(assoc(QAB))
    aBA0, aBA1 = c01(assoc(QBA))
    ck.eq("(C) assoc of route AB is zero at c=0 for every delta", aAB0, 0)
    ck.eq("(C) assoc of route BA is zero at c=0 for every delta", aBA0, 0)
    ck.eq("(C) c^1 of assoc, route AB = q0 q1 t0 t1 / Z", aAB1, q0 * q1 * t[0] * t[1] / Z)
    ck.eq("(C) c^1 of assoc, route BA = s0 s1 r0 r1 / Z", aBA1, s[0] * s[1] * r[0] * r[1] / Z)
    ck.eq("(C) mprod of route AB at c=0 = q0 q1 t0 t1", sp.cancel(mprod(AB0)), q0 * q1 * t[0] * t[1])
    ck.eq("(C) mprod of route BA at c=0 = s0 s1 r0 r1", sp.cancel(mprod(BA0)), s[0] * s[1] * r[0] * r[1])
    ck.mat_eq("(C) at delta=1 both c=0 tables are q (x) r", AB0.subs(delta, 1), BA0.subs(delta, 1))
    ck.eq("(C) at delta=1 the c^1 coefficients of assoc agree (ORD: second order)",
          aAB1.subs(delta, 1), aBA1.subs(delta, 1))

    # ---- (B) protected statistics F = assoc * h --------------------------
    # With a0 = 0 on both routes the c^1 sequence effect of assoc*h is exactly
    # a1_AB h(AB0) - a1_BA h(BA0).
    hs = {
        "h = 1 (the association)": lambda Q: sp.Integer(1),
        "h = 1 + P00": lambda Q: 1 + Q[0, 0],
        "h = (1 + P11)/(mA0 mA1)": lambda Q: (1 + Q[1, 1]) / (marg_A(Q)[0] * marg_A(Q)[1]),
        "h = 1/(mA0 mA1) (conditional difference)": lambda Q: 1 / (marg_A(Q)[0] * marg_A(Q)[1]),
        "h = 1/sqrt(mprod) (correlation coefficient)": lambda Q: 1 / sp.sqrt(mprod(Q)),
    }
    P = prior()
    ck.eq("(B) the conditional difference P(B=1|A=1) - P(B=1|A=0) equals assoc/(mA0 mA1)",
          P[1, 1] / (P[1, 0] + P[1, 1]) - P[0, 1] / (P[0, 0] + P[0, 1]),
          assoc(P) / (marg_A(P)[0] * marg_A(P)[1]))
    for name, h in hs.items():
        e1 = aAB1 * h(AB0) - aBA1 * h(BA0)
        ck("(B) " + name + ": c^1 sequence effect vanishes at delta=1",
           sp.simplify(e1.subs(delta, 1)) == 0, f"value {sp.simplify(e1.subs(delta, 1))}")
        for w, lab in ((half, "delta=1/2"), (0, "delta=0")):
            v = sp.simplify(e1.subs(delta, w).subs(GENERIC))
            ck(f"(B) {name}: c^1 sequence effect nonzero at {lab}, generic point (first order)",
               v != 0, f"value {v}")

    # ---- (C) the all-omega class: h = g/mprod ----------------------------
    gAB, gBA = sp.symbols('g_AB g_BA')
    e1g = sp.cancel(aAB1 * gAB / mprod(AB0) - aBA1 * gBA / mprod(BA0))
    ck.eq("(C) h = g/mprod: c^1 sequence effect = (g(AB0) - g(BA0)) / Z", e1g, (gAB - gBA) / Z)
    ck.ne("(C) AB0 and BA0 differ at delta=1/2 (so g must be constant across product measures)",
          (AB0 - BA0)[0, 0].subs(delta, half))
    ck.eq("(C) assoc/mprod: c^1 sequence effect is identically zero for every delta",
          sp.cancel(aAB1 / mprod(AB0) - aBA1 / mprod(BA0)), 0)
    ck.eq("(C) assoc/mprod: each route's c^1 coefficient is 1/Z", sp.cancel(aAB1 / mprod(AB0)), 1 / Z)
    F = lambda Q: assoc(Q) / mprod(Q)
    for w, lab, n in ((half, "delta=1/2", 2), (0, "delta=0", 2)):
        ser = sp.series(sp.cancel((F(QAB) - F(QBA)).subs(delta, w).subs(GENERIC)), c, 0, n + 1).removeO()
        ck(f"(C) assoc/mprod: sequence effect is second order at {lab}, generic point",
           all(ser.coeff(c, k) == 0 for k in range(n)) and ser.coeff(c, n) != 0, f"{ser}")
    PJ_AB, PJ_BA, _ = posteriors()
    ser = sp.series(sp.cancel((F(PJ_AB) - F(PJ_BA)).subs(GENERIC)), c, 0, 4).removeO()
    ck("(C) assoc/mprod: sequence effect is third order at delta=1, generic point",
       all(ser.coeff(c, k) == 0 for k in range(3)) and ser.coeff(c, 3) != 0, f"{ser}")
    P00, P01, P10, P11 = sp.symbols('P00 P01 P10 P11', positive=True)
    Qs = sp.Matrix([[P00, P01], [P10, P11]])
    x0, y0 = sp.symbols('x0 y0', positive=True)
    indep = {P00: x0 * y0, P01: x0 * (1 - y0), P10: (1 - x0) * y0, P11: (1 - x0) * (1 - y0)}
    logOR = sp.log(odds(Qs))
    Fm = assoc(Qs) / mprod(Qs)
    cells = (P00, P01, P10, P11)
    ck("(C) at every product measure grad(assoc/mprod) = grad(log odds ratio)",
       all(sp.simplify((sp.diff(Fm, v) - sp.diff(logOR, v)).subs(indep)) == 0 for v in cells))
    ck("(C) ...= grad(assoc)/mprod there, so assoc/mprod is PRO-protected at delta=1 as well",
       all(sp.simplify((sp.diff(Fm, v) - sp.diff(assoc(Qs), v) / mprod(Qs)).subs(indep)) == 0 for v in cells))

    # ---- (D) a non-protected statistic moves at zeroth order ------------
    G = lambda Q: marg_A(Q)[1] + assoc(Q)
    g0, _ = c01(G(QAB) - G(QBA))
    ck.eq("(D) P(A=1)+assoc: zeroth-order sequence effect is (1-delta)(alpha-q0)",
          sp.factor(g0), (1 - delta) * (alpha - q0))

    # ---- (E) the odds ratio, every delta and c ---------------------------
    P = prior()
    ck.eq("(E) odds ratio of route AB equals the prior's, symbolic in delta and c",
          sp.cancel(odds(QAB) - odds(P)), 0)
    ck.eq("(E) odds ratio of route BA equals the prior's, symbolic in delta and c",
          sp.cancel(odds(QBA) - odds(P)), 0)

    # ---- (F) factor inputs --------------------------------------------------
    mA = [sp.cancel(x) for x in marg_A(P)]
    mB = [sp.cancel(x) for x in marg_B(P)]
    lA = [sp.cancel(q[i] / mA[i]) for i in range(2)]
    lB = [sp.cancel(r[j] / mB[j]) for j in range(2)]
    ap0, ap1, bp0, bp1 = sp.symbols('ap0 ap1 bp0 bp1', positive=True)

    def factor_route(a, b):
        W = sp.Matrix(2, 2, lambda i, j: P[i, j] * a[i] * b[j])
        S = sum(W)
        return W.applyfunc(lambda e: sp.cancel(e / S)), sp.cancel(S)

    FAB, SAB = factor_route(lA, [bp0, bp1])          # A in full, B damped (abstract)
    FBA, SBA = factor_route([ap0, ap1], lB)          # B in full, A damped (abstract)
    FULL, _ = factor_route(lA, lB)
    FULL2, _ = factor_route(lA, lB)
    ck.mat_eq("(F) both factors in full: the two sequences give one table for every c (commutation)",
              FULL, FULL2)
    _, _, PBm = posteriors()
    ck.mat_eq("(F) ...and that table is the benchmark P^B of the manuscript",
              FULL, PBm.applyfunc(sp.cancel))
    ck.eq("(F) odds ratio of route AB (B damped) equals the prior's, symbolic in c and the damped factor",
          sp.cancel(odds(FAB) - odds(P)), 0)
    ck.eq("(F) odds ratio of route BA (A damped) equals the prior's, symbolic in c and the damped factor",
          sp.cancel(odds(FBA) - odds(P)), 0)
    KAB = lA[0] * lA[1] * bp0 * bp1
    KBA = ap0 * ap1 * lB[0] * lB[1]
    ck.eq("(F) assoc of route AB = c K / S^2 exactly", sp.cancel(assoc(FAB) - c * KAB / SAB**2), 0)
    ck.eq("(F) assoc of route BA = c K / S^2 exactly", sp.cancel(assoc(FBA) - c * KBA / SBA**2), 0)
    FAB0 = FAB.applyfunc(lambda e: sp.cancel(e.subs(c, 0)))
    FBA0 = FBA.applyfunc(lambda e: sp.cancel(e.subs(c, 0)))
    ck.eq("(F) assoc of route AB is zero at c=0 (product measure)", sp.cancel(assoc(FAB0)), 0)
    ck.eq("(F) assoc of route BA is zero at c=0 (product measure)", sp.cancel(assoc(FBA0)), 0)
    mA1_AB = sp.cancel(marg_A(FAB0)[1])
    mA1_BA = sp.cancel(marg_A(FBA0)[1])
    ck.eq("(F) A-marginal of route AB at c=0 is q1 (the full factor sets its own marginal)", mA1_AB, q1)
    ck.eq("(F) A-marginal of route BA at c=0 is (1-alpha) a'_1 / (alpha a'_0 + (1-alpha) a'_1)",
          mA1_BA, (1 - alpha) * ap1 / (alpha * ap0 + (1 - alpha) * ap1))
    # the gap vanishes iff a' has the ratio of a: substitute a' = a (delta = 1)
    ck.eq("(F) ...equal to q1 when a' = a (delta=1)", mA1_BA.subs({ap0: lA[0], ap1: lA[1]}), q1)
    half = sp.Rational(1, 2)
    aw = {ap0: sp.sqrt(lA[0]), ap1: sp.sqrt(lA[1])}
    bw = {bp0: sp.sqrt(lB[0]), bp1: sp.sqrt(lB[1])}
    gapA = sp.simplify((mA1_AB - mA1_BA).subs(aw).subs(GENERIC))
    ck("(F) A-marginal gap at c=0 nonzero for a' = a^(1/2), generic point (order zero)",
       gapA != 0, f"value {gapA}")
    # first-order factor of assoc: K/S0^2 on each route
    kAB = sp.cancel(KAB / SAB.subs(c, 0)**2)
    kBA = sp.cancel(KBA / SBA.subs(c, 0)**2)
    dk = sp.simplify((kAB.subs(bw) - kBA.subs(aw)).subs(GENERIC))
    ck("(F) assoc first-order factors differ between routes at delta=1/2, generic point (first order)",
       dk != 0, f"value {dk}")
    ck.eq("(F) ...and agree at delta=1", sp.cancel(kAB.subs({bp0: lB[0], bp1: lB[1]}) - kBA.subs({ap0: lA[0], ap1: lA[1]})), 0)
    ck.eq("(F) assoc/mprod first-order factor on route AB is 1/Z, symbolic in the damped factor",
          sp.cancel(kAB / mprod(FAB0)), 1 / Z)
    ck.eq("(F) assoc/mprod first-order factor on route BA is 1/Z, symbolic in the damped factor",
          sp.cancel(kBA / mprod(FBA0)), 1 / Z)
    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
