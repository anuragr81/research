"""
The ladder under partial adoption: which orders in c each belief statistic
carries when the second cue is adopted with weight omega (code name delta),
and which statistics stay protected at second order for every omega.

Symbolic in the prior (alpha, beta), the cues (q0, r0) and the weight delta.
Taylor coefficients in c are taken cell by cell and combined by the product
rule, as in verify_interior_omega.py.  Every "nonzero" claim rests on a
factored closed form and a `zero_set` row (check_zero_slope_identification.py)
giving its exact zero set on the domain alpha, beta, q0, r0 in (0,1), delta in
[0,1]; one-point evaluations at GENERIC are kept only as sanity rows.
Notation: q1 = 1-q0, r1 = 1-r0, Z = alpha(1-alpha)beta(1-beta),
t = (1-delta) beta + delta r, s = (1-delta) alpha + delta q (as distributions),
    H = q0 q1 [beta(1-beta) + delta (beta-r0)^2] - r0 r1 [alpha(1-alpha) + delta (alpha-q0)^2]
      = H0 + delta H1   (Lean: Ladder.lean, Hcof), irreducible over Q.

Rows:
  (A) at c = 0 both routes end at product measures, q (x) t and s (x) r, and
      their gap is (1-delta)(beta-r0) R1 + (1-delta)(q0-alpha) R2, a vector
      of span{R1, R2}.  The A-marginal gap (1-delta)(alpha-q0) is zero exactly
      when delta = 1 or q0 = alpha.
  (B) a statistic protected across an open set of priors, F = F0 + assoc*h
      with h smooth, has a sequence effect that is zero at c = 0 for every
      delta; its c^1 coefficient, in closed form, with exact zero sets:
        h = 1 (assoc)            (1-delta) H / Z
                                 zero iff delta = 1 or H = 0
        h = 1 + P00              (1-delta) K00 / Z,
                                 K00 = H (1 + q0 t0) + s0 s1 r0 r1 (beta q0 - alpha r0)
                                 zero iff delta = 1 or K00 = 0
        h = (1+P11)/(mA0 mA1)    (1-delta) K11 / Z,
                                 K11 = (r0-beta)(t0-r1)(1 + q1 t1) + r0 r1 (q1(1-beta) - (1-alpha) r1)
                                 zero iff delta = 1 or K11 = 0
        h = 1/(mA0 mA1)          (1-delta)(r0-beta)(t0-r1) / Z   (the conditional
                                 difference P(B=1|A=1) - P(B=1|A=0) = assoc/(mA0 mA1))
                                 zero iff delta = 1, r0 = beta, or t0 = r1
        h = 1/sqrt(mprod)        (1-delta) H / [Z (sqrt(mprod(AB0)) + sqrt(mprod(BA0)))]
                                 (correlation coefficient) zero iff delta = 1 or H = 0
      H, K00, K11 are irreducible over Q, so each "= 0" is a hypersurface; each
      contains {q0 = alpha, r0 = beta}, where AB0 = BA0.  H is affine in delta,
      so off {H0 = H1 = 0} it vanishes at one weight at most; at GENERIC that
      weight is 31/4483 (see verify_interior_omega.py).
  (C) the c^1 coefficient of each route's association is q0 q1 t0 t1 / Z and
      s0 s1 r0 r1 / Z; so for h = g/mprod, mprod = mA0 mA1 mB0 mB1, the c^1
      sequence effect is [g(q (x) t) - g(s (x) r)] / Z.  AB0 = BA0 exactly on
      {delta = 1} u {q0 = alpha, r0 = beta}, and at delta = 0 the pair
      (q (x) beta, alpha (x) r) ranges over all pairs of product measures: zero
      for every delta iff g is constant across product measures.  For
      assoc/mprod, symbolically in all five parameters:
        c^0, c^1 coefficients of the sequence effect   0 identically
        c^2 coefficient   (1-delta) L / Z^2,  L = (alpha-q0)(r1-r0) - (beta-r0)(q1-q0)
                          zero iff delta = 1 or L = 0 (irreducible, delta-free)
      and at delta = 1 the c^0..c^2 coefficients vanish identically and
        c^3 coefficient   N3 / Z^3,  N3 = (beta-r0) q0 q1 (r1-r0) - (alpha-q0) r0 r1 (q1-q0)
                          (= Z [kappa'(r1-r0) - kappa(q1-q0)]), zero iff N3 = 0 (irreducible).
      L and N3 vanish on {q0 = alpha, r0 = beta}, on the mirror set
      {beta = alpha, r0 = q0} and on {q0 = r0 = 1/2}.  At every product measure
      grad(assoc/mprod) = grad(log odds ratio): the all-omega protected class is
      the first-order shadow of the odds ratio.
  (D) a non-protected statistic, P(A=1) + assoc, moves at zeroth order.
  (E) the odds ratio of either route equals the prior's for every delta and c.
  (F) factor inputs (the benchmark reading).  Each cue supplies a factor on its
      own attribute, a_i = q_i/P(A=i), b_j = r_j/P(B=j); the belief is the prior
      with cell (i,j) multiplied by the applied factors and renormalised.  With
      both applied in full the two sequences coincide for every c.  When the
      factor read second is adopted in part, replaced by an arbitrary positive
      a' or b', the sequences end at c = 0 at product measures with
        A-marginal gap  alpha(1-alpha)(a1 a'0 - a0 a'1) / (alpha a'0 + (1-alpha) a'1),
      zero iff a' has the ratio of a; at a' = a^delta the numerator is
      (a0 a1)^delta (a1^(1-delta) - a0^(1-delta)), zero iff delta = 1 or
      a0 = a1, i.e. q0 = alpha.  The association is c K / S^2 exactly (K the
      product of the applied factors, S the normalising sum), zero at c = 0,
      with first-order factors k_AB, k_BA whose difference is
        a'0 a'1 b'0 b'1 (M_a^2 - M_b^2) / (S_a'^2 S_b'^2),
        M_a^2 = a0 a1 S_a'^2 / (a'0 a'1),  S_a' = alpha a'0 + (1-alpha) a'1  (likewise b);
      at a' = a^delta, M_a = (a0 a1)^((1-delta)/2) S_a' = q0 rho^(-(1-delta)/2) +
      q1 rho^((1-delta)/2), rho = a0/a1.  Zero set: M_a = M_b, which holds on
      all of delta = 1 (M_a = M_b = 1); for each delta in [0,1) it is a proper
      analytic hypersurface (at delta = 0 it is H0 = 0; for every delta < 1,
      dM_a/dq0 at q0 = alpha is (1-delta)(1-2 alpha)/(2 alpha(1-alpha)), so with
      r0 = beta, M_a != M_b arbitrarily near q0 = alpha when alpha != 1/2).
      assoc/mprod has first-order factor 1/Z on both routes; and the odds ratio
      is the prior's for every c.
"""
import sympy as sp
from jeffrey_core import *
from check_zero_slope_identification import (damped_route_AB, damped_route_BA, delta,
                                             zero_set, vanishes_on,
                                             D_FULL, Q_PRIOR, R_PRIOR)


def c01(e):
    """(c^0, c^1) coefficients of a rational e about c = 0, via N/D."""
    N, D = sp.fraction(sp.together(e))
    N0, D0 = N.subs(c, 0), D.subs(c, 0)
    N1, D1 = sp.diff(N, c).subs(c, 0), sp.diff(D, c).subs(c, 0)
    return sp.cancel(N0 / D0), sp.cancel((N1 * D0 - N0 * D1) / D0**2)


# ---- truncated power series in c, coefficients rational in the rest -------
def taylor_c(e, n):
    """[c^0, ..., c^n] coefficients of a rational e about c = 0: write e = N/D
    with N, D polynomial in c and divide the coefficient lists."""
    N, D = sp.fraction(sp.cancel(sp.together(e)))
    Np, Dp = sp.Poly(N, c), sp.Poly(D, c)
    Nc = [Np.coeff_monomial(c**k) for k in range(n + 1)]
    Dc = [Dp.coeff_monomial(c**k) for k in range(n + 1)]
    return series_div(Nc, Dc, n)


def series_mul(a, b, n):
    return [sp.cancel(sp.together(sum(a[i] * b[k - i] for i in range(k + 1)))) for k in range(n + 1)]


def series_div(a, b, n):
    out = []
    for k in range(n + 1):
        out.append(sp.cancel(sp.together((a[k] - sum(out[i] * b[k - i] for i in range(k))) / b[0])))
    return out


def assoc_over_mprod_series(Q, n):
    """[c^0, ..., c^n] coefficients of assoc(Q)/mprod(Q), cell by cell."""
    s = [[taylor_c(Q[i, j], n) for j in range(2)] for i in range(2)]
    add = lambda x, y, sg=1: [sp.cancel(u + sg * v) for u, v in zip(x, y)]
    a = add(series_mul(s[0][0], s[1][1], n), series_mul(s[0][1], s[1][0], n), -1)
    mA0, mA1 = add(s[0][0], s[0][1]), add(s[1][0], s[1][1])
    mB0, mB1 = add(s[0][0], s[1][0]), add(s[0][1], s[1][1])
    return series_div(a, series_mul(series_mul(mA0, mA1, n), series_mul(mB0, mB1, n), n), n)


def assoc(Q):
    return Q[0, 0] * Q[1, 1] - Q[0, 1] * Q[1, 0]


def mprod(Q):
    mA, mB = marg_A(Q), marg_B(Q)
    return mA[0] * mA[1] * mB[0] * mB[1]


def odds(Q):
    return Q[0, 0] * Q[1, 1] / (Q[0, 1] * Q[1, 0])


# The cofactor of the association's first-order sequence effect (Lean: Hcof).
H0 = q0 * q1 * beta * (1 - beta) - r0 * r1 * alpha * (1 - alpha)
H1 = q0 * q1 * (beta - r0)**2 - r0 * r1 * (alpha - q0)**2
H = H0 + delta * H1
H_DESCR = "H = 0 (H = H0 + delta H1 irreducible over Q: a hypersurface)"
TRIVIAL = {q0: alpha, r0: beta}               # both cues deliver the prior marginals
MIRROR = {beta: alpha, r0: q0}                # the two attributes interchangeable


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
    zero_set(ck, "(A) A-marginal gap at c=0", gA, [(1 - delta, D_FULL), (alpha - q0, Q_PRIOR)])
    ck.ne("(A) ...nonzero at delta=1/2, generic point (sanity, one point)", gA.subs(delta, half))

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
    P = prior()
    ck.eq("(B) the conditional difference P(B=1|A=1) - P(B=1|A=0) equals assoc/(mA0 mA1)",
          P[1, 1] / (P[1, 0] + P[1, 1]) - P[0, 1] / (P[0, 0] + P[0, 1]),
          assoc(P) / (marg_A(P)[0] * marg_A(P)[1]))
    e_cd = aAB1 / (q0 * q1) - aBA1 / (s[0] * s[1])
    ck.eq("(B) c^1 sequence effect of the conditional difference = (t0 t1 - r0 r1)/Z",
          e_cd, (t[0] * t[1] - r[0] * r[1]) / Z)
    Dcd = (r0 - beta) * (t[0] - r[1])
    K00 = H * (1 + q0 * t[0]) + s[0] * s[1] * r0 * r1 * (beta * q0 - alpha * r0)
    K11 = Dcd * (1 + q1 * t[1]) + r0 * r1 * (q1 * (1 - beta) - (1 - alpha) * r1)
    hs = [
        ("h = 1 (the association)", lambda Q: sp.Integer(1),
         "(1-delta) H / Z", (1 - delta) * H / Z,
         [(1 - delta, D_FULL), (H, H_DESCR)]),
        ("h = 1 + P00", lambda Q: 1 + Q[0, 0],
         "(1-delta) K00 / Z, K00 = H (1 + q0 t0) + s0 s1 r0 r1 (beta q0 - alpha r0)", (1 - delta) * K00 / Z,
         [(1 - delta, D_FULL), (K00, "K00 = 0 (irreducible over Q: a hypersurface)")]),
        ("h = (1 + P11)/(mA0 mA1)", lambda Q: (1 + Q[1, 1]) / (marg_A(Q)[0] * marg_A(Q)[1]),
         "(1-delta) K11 / Z, K11 = (r0-beta)(t0-r1)(1 + q1 t1) + r0 r1 (q1 (1-beta) - (1-alpha) r1)",
         (1 - delta) * K11 / Z,
         [(1 - delta, D_FULL), (K11, "K11 = 0 (irreducible over Q: a hypersurface)")]),
        ("h = 1/(mA0 mA1) (conditional difference)", lambda Q: 1 / (marg_A(Q)[0] * marg_A(Q)[1]),
         "(1-delta)(r0-beta)(t0-r1) / Z", (1 - delta) * Dcd / Z,
         [(1 - delta, D_FULL), (beta - r0, R_PRIOR),
          (t[0] - r[1], "t0 = r1, i.e. delta (r0 - beta) = 1 - beta - r0 (irreducible: at most one weight)")]),
    ]
    for name, h, cf_txt, cf, vanishing in hs:
        e1 = aAB1 * h(AB0) - aBA1 * h(BA0)
        ck.eq(f"(B) {name}: c^1 sequence effect = {cf_txt}", e1, cf)
        zero_set(ck, f"(B) {name}: c^1 sequence effect", e1, vanishing)
    vanishes_on(ck, "(B) H, K00, K11 and (r0-beta)(t0-r1) all vanish on {q0 = alpha, r0 = beta} (AB0 = BA0 there)",
                H * K00 * K11 * Dcd, [TRIVIAL])
    # the correlation coefficient: h = 1/sqrt(mprod).  Write u = sqrt(mprod(AB0)),
    # v = sqrt(mprod(BA0)) > 0; since a1_AB = u^2/Z and a1_BA = v^2/Z the c^1
    # effect is (u - v)/Z = (u^2 - v^2) / (Z (u + v)).
    mAB, mBA = sp.cancel(mprod(AB0)), sp.cancel(mprod(BA0))
    ck("(B) h = 1/sqrt(mprod) (correlation coefficient): c^1 sequence effect = "
       "[sqrt(mprod(AB0)) - sqrt(mprod(BA0))] / Z  (a1_AB = mprod(AB0)/Z, a1_BA = mprod(BA0)/Z)",
       sp.cancel(aAB1 - mAB / Z) == 0 and sp.cancel(aBA1 - mBA / Z) == 0)
    ck.eq("(B) ...= (1-delta) H / [Z (sqrt(mprod(AB0)) + sqrt(mprod(BA0)))]: mprod(AB0) - mprod(BA0) = (1-delta) H",
          mAB - mBA, (1 - delta) * H)
    zero_set(ck, "(B) h = 1/sqrt(mprod): c^1 sequence effect (positive factor 1/(sqrt + sqrt) aside)",
             (mAB - mBA) / Z, [(1 - delta, D_FULL), (H, H_DESCR)])
    hs_all = [(n, h) for n, h, *_ in hs] + [("h = 1/sqrt(mprod) (correlation coefficient)",
                                             lambda Q: 1 / sp.sqrt(mprod(Q)))]
    for name, h in hs_all:
        e1 = aAB1 * h(AB0) - aBA1 * h(BA0)
        ck("(B) " + name + ": c^1 sequence effect vanishes at delta=1",
           sp.simplify(e1.subs(delta, 1)) == 0, f"value {sp.simplify(e1.subs(delta, 1))}")
        vals = [sp.simplify(e1.subs(delta, w).subs(GENERIC)) for w in (half, 0)]
        ck(f"(B) {name}: c^1 sequence effect nonzero at delta=1/2 and 0, generic point (sanity, one point)",
           all(v != 0 for v in vals), f"values {vals}")

    # ---- (C) the all-omega class: h = g/mprod ----------------------------
    gAB, gBA = sp.symbols('g_AB g_BA')
    e1g = sp.cancel(aAB1 * gAB / mprod(AB0) - aBA1 * gBA / mprod(BA0))
    ck.eq("(C) h = g/mprod: c^1 sequence effect = (g(AB0) - g(BA0)) / Z", e1g, (gAB - gBA) / Z)
    gB = sp.factor(gap0[0, 1] + gap0[1, 1])
    ck.eq("(C) B-marginal gap of AB0 - BA0 is (1-delta)(r0-beta)", gB, (1 - delta) * (r0 - beta))
    zero_set(ck, "(C) B-marginal gap at c=0", gB, [(1 - delta, D_FULL), (beta - r0, R_PRIOR)])
    ck("(C) AB0, BA0 are product measures, equal iff both marginals agree: AB0 = BA0 exactly on "
       "{delta = 1} u {q0 = alpha, r0 = beta} (the 'if' direction here, 'only if' from the two marginal gaps)",
       all(sp.cancel(e) == 0 for e in gap0.subs(delta, 1)) and all(sp.cancel(e) == 0 for e in gap0.subs(TRIVIAL)))
    ck.mat_eq("(C) at delta=0, AB0 = q (x) beta", AB0.subs(delta, 0), sp.Matrix(2, 2, lambda i, j: q[i] * bvec[j]))
    ck.mat_eq("(C) ...and BA0 = alpha (x) r: with (alpha, beta, q0, r0) free the pair ranges over all pairs of "
              "product measures, so a c^1 effect zero for every delta forces g constant on product measures",
              BA0.subs(delta, 0), sp.Matrix(2, 2, lambda i, j: avec[i] * r[j]))
    ck.ne("(C) AB0 and BA0 differ at delta=1/2, generic point (sanity, one point)",
          (AB0 - BA0)[0, 0].subs(delta, half))
    ck.eq("(C) assoc/mprod: c^1 sequence effect is identically zero for every delta",
          sp.cancel(aAB1 / mprod(AB0) - aBA1 / mprod(BA0)), 0)
    ck.eq("(C) assoc/mprod: each route's c^1 coefficient is 1/Z", sp.cancel(aAB1 / mprod(AB0)), 1 / Z)

    # assoc/mprod to second order, symbolic in alpha, beta, q0, r0 AND delta
    fAB, fBA = assoc_over_mprod_series(QAB, 2), assoc_over_mprod_series(QBA, 2)
    dF = [sp.cancel(x - y) for x, y in zip(fAB, fBA)]
    ck.eq("(C) assoc/mprod sequence effect: c^0 coefficient = 0 identically in alpha, beta, q0, r0, delta", dF[0], 0)
    ck.eq("(C) assoc/mprod sequence effect: c^1 coefficient = 0 identically in alpha, beta, q0, r0, delta", dF[1], 0)
    L = (alpha - q0) * (r1 - r0) - (beta - r0) * (q1 - q0)
    ck.eq("(C) assoc/mprod sequence effect: c^2 coefficient = (1-delta) L / Z^2, "
          "L = (alpha-q0)(r1-r0) - (beta-r0)(q1-q0)", dF[2], (1 - delta) * L / Z**2)
    zero_set(ck, "(C) assoc/mprod c^2 coefficient", dF[2],
             [(1 - delta, D_FULL), (L, "L = 0 (irreducible over Q, free of delta: a hypersurface)")])
    sets = [TRIVIAL, MIRROR, {q0: half, r0: half}]
    vanishes_on(ck, "(C) L vanishes on {q0 = alpha, r0 = beta}, {beta = alpha, r0 = q0} and {q0 = r0 = 1/2}", L, sets)
    # at delta = 1 to third order
    PJ_AB, PJ_BA, _ = posteriors()
    gAB3, gBA3 = assoc_over_mprod_series(PJ_AB, 3), assoc_over_mprod_series(PJ_BA, 3)
    dG = [sp.cancel(x - y) for x, y in zip(gAB3, gBA3)]
    ck("(C) assoc/mprod at delta=1: c^0, c^1, c^2 coefficients = 0 identically in alpha, beta, q0, r0",
       all(dG[k] == 0 for k in range(3)), f"{dG[:3]}")
    N3 = (beta - r0) * q0 * q1 * (r1 - r0) - (alpha - q0) * r0 * r1 * (q1 - q0)
    ck.eq("(C) assoc/mprod at delta=1: c^3 coefficient = N3 / Z^3, "
          "N3 = (beta-r0) q0 q1 (r1-r0) - (alpha-q0) r0 r1 (q1-q0)", dG[3], N3 / Z**3)
    ck.eq("(C) ...= [kappa' (r1-r0) - kappa (q1-q0)] / Z^2", dG[3], (kappa_p * (r1 - r0) - kappa * (q1 - q0)) / Z**2)
    zero_set(ck, "(C) assoc/mprod c^3 coefficient at delta=1", dG[3],
             [(N3, "N3 = 0 (irreducible over Q: a hypersurface)")])
    vanishes_on(ck, "(C) N3 vanishes on {q0 = alpha, r0 = beta}, {beta = alpha, r0 = q0} and {q0 = r0 = 1/2}", N3, sets)
    F = lambda Q: assoc(Q) / mprod(Q)
    for w, lab, n in ((half, "delta=1/2", 2), (0, "delta=0", 2)):
        ser = sp.series(sp.cancel((F(QAB) - F(QBA)).subs(delta, w).subs(GENERIC)), c, 0, n + 1).removeO()
        ck(f"(C) assoc/mprod: sequence effect is second order at {lab}, generic point (sanity, one point)",
           all(ser.coeff(c, k) == 0 for k in range(n)) and ser.coeff(c, n) != 0, f"{ser}")
    ser = sp.series(sp.cancel((F(PJ_AB) - F(PJ_BA)).subs(GENERIC)), c, 0, 4).removeO()
    ck("(C) assoc/mprod: sequence effect is third order at delta=1, generic point (sanity, one point)",
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
    ck.eq("(F) ...equal to q1 when a' = a (delta=1)", mA1_BA.subs({ap0: lA[0], ap1: lA[1]}), q1)
    # A-marginal gap at c = 0: closed form for arbitrary positive a', then a' = a^delta
    ck.eq("(F) A-marginal gap at c=0 = alpha(1-alpha)(a1 a'0 - a0 a'1) / (alpha a'0 + (1-alpha) a'1), "
          "a_i = q_i/P(A=i): zero iff a'0/a'1 = a0/a1",
          mA1_AB - mA1_BA, alpha * (1 - alpha) * (lA[1] * ap0 - lA[0] * ap1) / (alpha * ap0 + (1 - alpha) * ap1))
    a0, a1, b0, b1 = sp.symbols('a0 a1 b0 b1', positive=True)   # stand-ins for the factors, any positive values
    ck.eq("(F) at a' = a^delta: a1 a0^delta - a0 a1^delta = (a0 a1)^delta (a1^(1-delta) - a0^(1-delta))",
          sp.expand((a0 * a1)**delta * (a1**(1 - delta) - a0**(1 - delta))), a1 * a0**delta - a0 * a1**delta)
    ck("(F) log(a1 a0^delta) - log(a0 a1^delta) = (1-delta)(log a1 - log a0): the gap is zero iff delta = 1 "
       "or a0 = a1 (log injective; the denominator is a positive combination)",
       sp.expand(sp.expand_log(sp.log(a1 * a0**delta) - sp.log(a0 * a1**delta), force=True)
                 - (1 - delta) * (sp.log(a1) - sp.log(a0))) == 0)
    ck.eq("(F) a0 - a1 = (q0 - alpha) / (alpha (1-alpha)) at the cue's factors",
          lA[0] - lA[1], (q0 - alpha) / (alpha * (1 - alpha)))
    zero_set(ck, "(F) a0 - a1", lA[0] - lA[1], [(q0 - alpha, Q_PRIOR)])
    gapd = (mA1_AB - mA1_BA).subs({ap0: lA[0]**delta, ap1: lA[1]**delta})
    ck("(F) zero set of the A-marginal gap at c=0, a' = a^delta: delta = 1 | q0 = alpha "
       "('if': the gap vanishes identically on both; 'only if': the log and a0 - a1 rows)",
       sp.cancel(sp.together(gapd.subs(delta, 1))) == 0 and sp.cancel(sp.together(gapd.subs(q0, alpha))) == 0)
    aw = {ap0: sp.sqrt(lA[0]), ap1: sp.sqrt(lA[1])}
    bw = {bp0: sp.sqrt(lB[0]), bp1: sp.sqrt(lB[1])}
    gapA = sp.simplify((mA1_AB - mA1_BA).subs(aw).subs(GENERIC))
    ck("(F) A-marginal gap at c=0 nonzero for a' = a^(1/2), generic point (sanity, one point)",
       gapA != 0, f"value {gapA}")
    # first-order factor of assoc: K/S0^2 on each route
    kAB = sp.cancel(KAB / SAB.subs(c, 0)**2)
    kBA = sp.cancel(KBA / SBA.subs(c, 0)**2)
    Sap, Sbp = alpha * ap0 + (1 - alpha) * ap1, beta * bp0 + (1 - beta) * bp1
    ck.eq("(F) first-order factor of assoc, route AB: k_AB = a0 a1 b'0 b'1 / (beta b'0 + (1-beta) b'1)^2",
          kAB, lA[0] * lA[1] * bp0 * bp1 / Sbp**2)
    ck.eq("(F) first-order factor of assoc, route BA: k_BA = b0 b1 a'0 a'1 / (alpha a'0 + (1-alpha) a'1)^2",
          kBA, lB[0] * lB[1] * ap0 * ap1 / Sap**2)
    Ma2 = lA[0] * lA[1] * Sap**2 / (ap0 * ap1)
    Mb2 = lB[0] * lB[1] * Sbp**2 / (bp0 * bp1)
    ck.eq("(F) k_AB - k_BA = a'0 a'1 b'0 b'1 (M_a^2 - M_b^2) / (S_a'^2 S_b'^2), "
          "M_a^2 = a0 a1 S_a'^2/(a'0 a'1): zero iff M_a = M_b",
          kAB - kBA, ap0 * ap1 * bp0 * bp1 * (Ma2 - Mb2) / (Sap**2 * Sbp**2))
    ck.eq("(F) at a' = a (delta=1): M_a^2 = (alpha a0 + (1-alpha) a1)^2 = 1, likewise M_b^2: zero for all priors and cues",
          Ma2.subs({ap0: lA[0], ap1: lA[1]}), 1)
    ck.eq("(F) ...M_b^2 = 1 at b' = b", Mb2.subs({bp0: lB[0], bp1: lB[1]}), 1)
    eps = (1 - delta) / 2
    Ma_pow = (a0 * a1)**eps * (alpha * a0**delta + (1 - alpha) * a1**delta)
    ck("(F) at a' = a^delta: M_a = (a0 a1)^((1-delta)/2)(alpha a0^delta + (1-alpha) a1^delta) "
       "= q0 rho^(-(1-delta)/2) + q1 rho^((1-delta)/2), rho = a0/a1 (q0 = alpha a0, q1 = (1-alpha) a1)",
       sp.cancel(sp.together((Ma_pow**2 - (a0 * a1 * (alpha * a0**delta + (1 - alpha) * a1**delta)**2
                                           / (a0**delta * a1**delta))))) == 0
       and sp.expand(sp.powsimp(sp.expand(Ma_pow - (alpha * a0 * (a0 / a1)**(-eps)
                                                    + (1 - alpha) * a1 * (a0 / a1)**eps)), force=True)) == 0)
    kd = (kAB - kBA).subs({ap0: lA[0]**delta, ap1: lA[1]**delta, bp0: lB[0]**delta, bp1: lB[1]**delta})
    ck("(F) at a' = a^delta, b' = b^delta: k_AB = k_BA on {q0 = alpha, r0 = beta} and on the mirror set "
       "{beta = alpha, r0 = q0}, for every delta",
       sp.cancel(sp.together(kd.subs(TRIVIAL))) == 0 and sp.cancel(sp.together(kd.subs(MIRROR))) == 0)
    ck.eq("(F) at delta = 0: M_a^2 - M_b^2 = a0 a1 - b0 b1 = H0 / Z, H0 = q0 q1 beta(1-beta) - r0 r1 alpha(1-alpha)",
          (Ma2 - Mb2).subs({ap0: 1, ap1: 1, bp0: 1, bp1: 1}), H0 / Z)
    zero_set(ck, "(F) M_a^2 - M_b^2 at delta = 0", H0 / Z,
             [(H0, "H0 = 0 (irreducible over Q: a hypersurface)")])
    Mq = (lA[0] * lA[1])**eps * (alpha * lA[0]**delta + (1 - alpha) * lA[1]**delta)
    dM = sp.cancel(sp.together(sp.diff(Mq, q0).subs(q0, alpha)))
    ck.eq("(F) every delta: dM_a/dq0 at q0 = alpha is (1-delta)(1-2 alpha)/(2 alpha(1-alpha)) "
          "(and M_b = 1 at r0 = beta)", dM, (1 - delta) * (1 - 2 * alpha) / (2 * alpha * (1 - alpha)))
    zero_set(ck, "(F) dM_a/dq0 at q0 = alpha -- so for each delta in [0,1) the hypersurface M_a = M_b is proper",
             dM, [(1 - delta, D_FULL), (1 - 2 * alpha, "alpha = 1/2")])
    ck.eq("(F) ...M_b = 1 at r0 = beta for every delta", (lB[0] * lB[1]).subs(r0, beta), 1)
    dk = sp.simplify((kAB.subs(bw) - kBA.subs(aw)).subs(GENERIC))
    ck("(F) assoc first-order factors differ between routes at delta=1/2, generic point (sanity, one point)",
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
