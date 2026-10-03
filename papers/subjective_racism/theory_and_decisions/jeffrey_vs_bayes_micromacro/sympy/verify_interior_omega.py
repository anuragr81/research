"""
Interior adoption weight (0 < delta < 1): which orders in c survive.

Symbolic in the prior (alpha, beta), the cues (q0, r0), the weight delta and
the mix lambda.  Taylor coefficients in c are taken cell by cell (cheap) and
combined by the product rule, never by factoring a full association polynomial.

Rows:
  (1) between-sequence effect on each marginal at c = 0        (zeroth order)
  (2) each damped step is a column (row) scaling, so the odds ratio of every
      route equals the prior's -- the structural fact Lemma SEP rests on
  (3) between-sequence effect on assoc: c^0 = 0, c^1 = (1-delta) * G, G affine
      in delta; at delta = 1 the effect is second order.  Closed form G = H/Z,
          H = H0 + delta H1,
          H0 = q0 q1 beta(1-beta) - r0 r1 alpha(1-alpha),
          H1 = q0 q1 (beta-r0)^2 - r0 r1 (alpha-q0)^2
      (Lean: Ladder.lean, Hcof).  H is irreducible over Q; the c^1 coefficient
      vanishes on the domain exactly on {delta = 1} u {H = 0}.  {H = 0} is a
      hypersurface: for each (alpha, beta, q0, r0) at most one weight
      delta* = -H0/H1, unless H0 = H1 = 0 (e.g. q0 = alpha and r0 = beta, or the
      mirror set beta = alpha, r0 = q0).  At the suite's own generic point
      delta* = 31/4483 lies in (0,1): there the association's sequence effect is
      second order at that one weight.
  (4) distortion of the mean belief from the benchmark at c = 0 and interior
      lambda: marginals and assoc both nonzero (zeroth order).  The assoc
      distortion -lambda(1-lambda)(1-delta)^2(alpha-q0)(beta-r0) vanishes for
      lambda in (0,1) exactly when delta = 1, q0 = alpha or r0 = beta.
  (5) Bayes-factor reading with the second factor raised to delta: the
      A-marginal gap at c = 0 is, for the cue's factors a_i = q_i/P(A=i),
          alpha(1-alpha)(a1 a0^delta - a0 a1^delta) / (alpha a0^delta + (1-alpha) a1^delta)
        = alpha(1-alpha)(a0 a1)^delta (a1^(1-delta) - a0^(1-delta)) / (...),
      zero exactly when delta = 1 or a0 = a1, i.e. q0 = alpha
      (a0 - a1 = (q0 - alpha)/(alpha(1-alpha))).
  Exact zero sets use `zero_set` from check_zero_slope_identification.py on the
  domain alpha, beta, q0, r0, lambda in (0,1), delta in [0,1]; one-point
  evaluations at GENERIC are kept only as sanity rows.
"""
import sympy as sp
from jeffrey_core import *
from check_zero_slope_identification import (damped_route_AB, damped_route_BA, delta,
                                             zero_set, vanishes_on,
                                             D_FULL, Q_PRIOR, R_PRIOR)

# The cofactor of the association's first-order sequence effect (Lean:
# Ladder.lean, Hcof / ladder_assoc_coeff): affine in delta, H = H0 + delta H1.
H0 = q0 * q1 * beta * (1 - beta) - r0 * r1 * alpha * (1 - alpha)
H1 = q0 * q1 * (beta - r0)**2 - r0 * r1 * (alpha - q0)**2
H = H0 + delta * H1
H_DESCR = ("H = 0, H = H0 + delta H1 irreducible over Q: a hypersurface, met by at most one "
           "weight delta = -H0/H1 per (alpha, beta, q0, r0) unless H0 = H1 = 0")


def c01(e):
    """(c^0, c^1) coefficients of a rational e about c = 0, via N/D."""
    N, D = sp.fraction(sp.together(e))
    N0, D0 = N.subs(c, 0), D.subs(c, 0)
    N1, D1 = sp.diff(N, c).subs(c, 0), sp.diff(D, c).subs(c, 0)
    return sp.cancel(N0 / D0), sp.cancel((N1 * D0 - N0 * D1) / D0**2)


def mat_c01(Q):
    a = [[c01(Q[i, j]) for j in range(2)] for i in range(2)]
    Q0 = sp.Matrix(2, 2, lambda i, j: a[i][j][0])
    Q1 = sp.Matrix(2, 2, lambda i, j: a[i][j][1])
    return Q0, Q1


def assoc_c01(Q0, Q1):
    """assoc = Q00 Q11 - Q01 Q10; product rule for its c^0 and c^1 terms."""
    a0 = Q0[0, 0] * Q0[1, 1] - Q0[0, 1] * Q0[1, 0]
    a1 = (Q1[0, 0] * Q0[1, 1] + Q0[0, 0] * Q1[1, 1]
          - Q1[0, 1] * Q0[1, 0] - Q0[0, 1] * Q1[1, 0])
    return sp.factor(sp.cancel(a0)), sp.factor(sp.cancel(a1))


def main():
    ck = Check("Interior adoption weight -- orders in c, symbolic in the prior")
    QAB, QBA = damped_route_AB(delta), damped_route_BA(delta)
    _, _, PBm = posteriors()
    AB0, AB1 = mat_c01(QAB)
    BA0, BA1 = mat_c01(QBA)
    PB0, PB1 = mat_c01(PBm)

    # ---- (1) marginals between sequences at c = 0 -----------------------
    eA0 = sp.factor(AB0[1, 0] + AB0[1, 1] - BA0[1, 0] - BA0[1, 1])
    eB0 = sp.factor(AB0[0, 1] + AB0[1, 1] - BA0[0, 1] - BA0[1, 1])
    ck.eq("(1) A-marginal sequence effect at c=0 is (1-delta)(alpha-q0)",
          sp.expand(eA0 - (1 - delta) * (alpha - q0)), 0)
    ck.eq("(1) B-marginal sequence effect at c=0 is -(1-delta)(beta-r0)",
          sp.expand(eB0 + (1 - delta) * (beta - r0)), 0)

    # ---- (2) each damped step is a column/row scaling of the table it meets
    P1 = jeffrey_A(prior())
    for j in range(2):
        ck.eq(f"(2) damped B-step: column {j} multiplier is row-independent",
              sp.cancel(QAB[0, j] * P1[1, j] - QAB[1, j] * P1[0, j]), 0)
    P2 = jeffrey_B(prior())
    for i in range(2):
        ck.eq(f"(2) damped A-step: row {i} multiplier is column-independent",
              sp.cancel(QBA[i, 0] * P2[i, 1] - QBA[i, 1] * P2[i, 0]), 0)
    # hence both routes are separable reweightings of the prior and carry its
    # odds ratio (Lemma SEP; Lean isSeparable_applySteps); checked symbolically,
    # with the old one-point evaluation kept as a sanity row
    for name, Q in (("AB", QAB), ("BA", QBA)):
        ck.eq(f"(2) odds ratio of route {name} equals the prior's, symbolic in the prior, the cues, delta and c",
              odds_ratio(Q), odds_ratio(prior()))
        ck.eq(f"(2) ...route {name} at the generic point, delta=1/3, c=1/40 (sanity, one point)",
              at_generic(odds_ratio(Q) - odds_ratio(prior())).subs({delta: sp.Rational(1, 3), c: sp.Rational(1, 40)}), 0)

    # ---- (3) assoc between sequences ------------------------------------
    sAB0, sAB1 = assoc_c01(AB0, AB1)
    sBA0, sBA1 = assoc_c01(BA0, BA1)
    a0 = sp.factor(sp.cancel(sAB0 - sBA0))
    a1 = sp.factor(sp.cancel(sAB1 - sBA1))
    ck.eq("(3) assoc sequence effect is zero at c=0 for every delta", a0, 0)
    G = sp.cancel(a1 / (1 - delta))
    ck("(3) c^1 coefficient of the assoc sequence effect carries the factor (1-delta)",
       sp.cancel(sp.expand(a1) - sp.expand((1 - delta) * G)) == 0 and
       sp.denom(sp.together(G)).has(delta) is False, f"a1 = {a1}")
    ck.eq("(3) at delta=1 the assoc sequence effect is second order (agrees with ORD)",
          sp.cancel(a1.subs(delta, 1)), 0)
    ck("(3) the cofactor G is affine in delta",
       sp.degree(sp.numer(sp.together(G)), delta) <= 1, f"G = {sp.factor(G)}")
    ck.eq("(3) G = H / Z, H = q0 q1 [beta(1-beta) + delta (beta-r0)^2] - r0 r1 [alpha(1-alpha) + delta (alpha-q0)^2]",
          G, H / Z)
    zero_set(ck, "(3) G", G, [(H, H_DESCR)])
    zero_set(ck, "(3) the c^1 coefficient (1-delta) G", a1, [(1 - delta, D_FULL), (H, "H = 0 (as above)")])
    vanishes_on(ck, "(3) H = 0 contains {q0 = alpha, r0 = beta} and the mirror set {beta = alpha, r0 = q0}, for every delta",
                H, [{q0: alpha, r0: beta}, {beta: alpha, r0: q0}])
    dstar = at_generic(-H0 / H1)
    ck(f"(3) at the generic prior and cues H vanishes at the single weight delta* = -H0/H1 = {dstar} in (0,1)",
       0 < dstar < 1 and at_generic(H).subs(delta, dstar) == 0
       and sp.degree(sp.numer(sp.together(at_generic(H))), delta) == 1)
    ck.ne("(3) G nonzero at the generic prior for delta=1/2 (sanity, one point)",
          at_generic(G).subs(delta, sp.Rational(1, 2)))
    ck.ne("(3) ...and for delta=0 (sanity, one point)",
          at_generic(G).subs(delta, 0))

    # ---- (4) distortion of the mean belief at c = 0, interior lambda ----
    M0 = lam * AB0 + (1 - lam) * BA0
    dA0 = sp.factor(sp.cancel(M0[1, 0] + M0[1, 1] - PB0[1, 0] - PB0[1, 1]))
    dS0 = sp.factor(sp.cancel(M0[0, 0] * M0[1, 1] - M0[0, 1] * M0[1, 0]
                              - (PB0[0, 0] * PB0[1, 1] - PB0[0, 1] * PB0[1, 0])))
    ck.eq("(4) A-marginal distortion at c=0 is (1-lambda)(1-delta)(q0-alpha)",
          sp.expand(dA0 - (1 - lam) * (1 - delta) * (q0 - alpha)), 0)
    ck.eq("(4) assoc distortion at c=0 is -lambda(1-lambda)(1-delta)^2(alpha-q0)(beta-r0)",
          sp.expand(dS0 + lam * (1 - lam) * (1 - delta)**2 * (alpha - q0) * (beta - r0)), 0)
    zero_set(ck, "(4) assoc distortion at c=0, lambda in (0,1)", dS0,
             [(1 - delta, D_FULL), (alpha - q0, Q_PRIOR), (beta - r0, R_PRIOR)])
    ck("(4) ...nonzero at lambda=2/5, delta=1/2, generic point (sanity, one point)",
       at_generic(dS0).subs({lam: sp.Rational(2, 5), delta: sp.Rational(1, 2)}) != 0,
       f"value = {at_generic(dS0).subs({lam: sp.Rational(2,5), delta: sp.Rational(1,2)})}")
    ck.eq("(4) ...which vanishes at delta=1 (the setting of Sections 4-5)", dS0.subs(delta, 1), 0)
    # ---- (5) the Bayes-factor reading is not sequence-free under partial
    #      adoption either: scale the SECOND cue's factor by delta ----------
    P0 = prior()
    mA = [sp.cancel(x) for x in marg_A(P0)]; mB = [sp.cancel(x) for x in marg_B(P0)]
    lA = [q[i] / mA[i] for i in range(2)]; lB = [r[j] / mB[j] for j in range(2)]
    def scaled(first_A, aa=lA, bb=lB):
        W = sp.Matrix(2, 2, lambda i, j: P0[i, j] * (aa[i] * bb[j]**delta if first_A else aa[i]**delta * bb[j]))
        return W / sum(W)
    SAB, SBA = scaled(True), scaled(False)
    gap0 = (marg_A(SAB)[1] - marg_A(SBA)[1]).subs(c, 0)
    # closed form for arbitrary positive factors a = (fa0, fa1), b = (fb0, fb1)
    fa0, fa1, fb0, fb1 = sp.symbols('fa0 fa1 fb0 fb1', positive=True)   # any positive factors
    Sa1 = alpha * fa0 + (1 - alpha) * fa1
    Sad = alpha * fa0**delta + (1 - alpha) * fa1**delta
    gapF = alpha * (1 - alpha) * (fa1 * fa0**delta - fa0 * fa1**delta) / (Sa1 * Sad)
    ck.eq("(5) any positive factors: A-marginal gap at c=0 = alpha(1-alpha)(a1 a0^delta - a0 a1^delta)"
          " / [(alpha a0 + (1-alpha) a1)(alpha a0^delta + (1-alpha) a1^delta)]",
          (marg_A(scaled(True, [fa0, fa1], [fb0, fb1]))[1]
           - marg_A(scaled(False, [fa0, fa1], [fb0, fb1]))[1]).subs(c, 0), gapF)
    ck.eq("(5) ...at the cue's factors a_i = q_i/P(A=i): alpha a0 + (1-alpha) a1 = 1",
          Sa1.subs({fa0: lA[0], fa1: lA[1]}), 1)
    ck.eq("(5) ...so the gap is alpha(1-alpha)(a1 a0^delta - a0 a1^delta)/(alpha a0^delta + (1-alpha) a1^delta)",
          gap0, (gapF * Sa1).subs({fa0: lA[0], fa1: lA[1]}))
    ck.eq("(5) a1 a0^delta - a0 a1^delta = (a0 a1)^delta (a1^(1-delta) - a0^(1-delta))",
          sp.expand((fa0 * fa1)**delta * (fa1**(1 - delta) - fa0**(1 - delta))), fa1 * fa0**delta - fa0 * fa1**delta)
    ck("(5) log(a1 a0^delta) - log(a0 a1^delta) = (1-delta)(log a1 - log a0): the two positive terms "
       "agree iff delta = 1 or a0 = a1 (log injective; both denominators are positive combinations)",
       sp.expand(sp.expand_log(sp.log(fa1 * fa0**delta) - sp.log(fa0 * fa1**delta), force=True)
                 - (1 - delta) * (sp.log(fa1) - sp.log(fa0))) == 0)
    ck.eq("(5) a0 - a1 = (q0 - alpha) / (alpha (1-alpha)) at the cue's factors", lA[0] - lA[1],
          (q0 - alpha) / (alpha * (1 - alpha)))
    zero_set(ck, "(5) a0 - a1", lA[0] - lA[1], [(q0 - alpha, Q_PRIOR)])
    ck("(5) zero set of the scaled Bayes-factor A-marginal gap at c=0: delta = 1 | q0 = alpha "
       "('if': the gap vanishes identically on both; 'only if': the log and a0 - a1 rows)",
       sp.cancel(gap0.subs(delta, 1)) == 0 and sp.cancel(gap0.subs(q0, alpha)) == 0)
    ck.ne("(5) scaled Bayes-factor update: A-marginal gap at c=0 nonzero at delta=1/2, generic prior (sanity, one point)",
          gap0.subs(GENERIC).subs(delta, sp.Rational(1, 2)))
    ck.eq("(5) ...and agrees at delta=1 (Wagner: same factor in either position commutes)",
          sp.simplify(gap0.subs(delta, 1)), 0)
    # ---- (6) at delta=0 the two readings coincide: only the first cue is ever
    #      applied, and on one cue Jeffrey and Bayes factor agree (IMM) --------
    ck.mat_eq("(6) no adoption, Bayes-factor reading, sequence AB = single Jeffrey step on A, all c",
              SAB.subs(delta, 0).applyfunc(sp.cancel), jeffrey_A(P0).applyfunc(sp.cancel))
    ck.mat_eq("(6) no adoption, delivered-credence reading, sequence AB = the same table, all c",
              damped_route_AB(0).applyfunc(sp.cancel), jeffrey_A(P0).applyfunc(sp.cancel))
    ck.mat_eq("(6) ...and likewise BA under both readings = single Jeffrey step on B",
              SBA.subs(delta, 0).applyfunc(sp.cancel), damped_route_BA(0).applyfunc(sp.cancel))
    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
