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
      in delta; G != 0 generically; at delta = 1 the effect is second order
  (4) distortion of the mean belief from the benchmark at c = 0 and interior
      lambda: marginals and assoc both nonzero (zeroth order)
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
    # odds ratio (Lemma SEP; Lean isSeparable_applySteps); witnessed numerically
    for name, Q in (("AB", QAB), ("BA", QBA)):
        ck.eq(f"(2) odds ratio of route {name} equals the prior's at the generic point, delta=1/3, c=1/40",
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
    ck.ne("(3) G is nonzero at the generic prior for delta=1/2, so the effect is first order there",
          at_generic(G).subs(delta, sp.Rational(1, 2)))
    ck.ne("(3) ...and for delta=0 (first impression never moved)",
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
    ck("(4) ...nonzero for interior lambda and delta whenever both cues differ from the prior marginals",
       at_generic(dS0).subs({lam: sp.Rational(2, 5), delta: sp.Rational(1, 2)}) != 0,
       f"value = {at_generic(dS0).subs({lam: sp.Rational(2,5), delta: sp.Rational(1,2)})}")
    ck.eq("(4) ...which vanishes at delta=1 (the setting of Sections 4-5)", dS0.subs(delta, 1), 0)
    # ---- (5) the Bayes-factor reading is not sequence-free under partial
    #      adoption either: scale the SECOND cue's factor by delta ----------
    P0 = prior()
    mA = [sp.cancel(x) for x in marg_A(P0)]; mB = [sp.cancel(x) for x in marg_B(P0)]
    lA = [q[i] / mA[i] for i in range(2)]; lB = [r[j] / mB[j] for j in range(2)]
    def scaled(first_A):
        W = sp.Matrix(2, 2, lambda i, j: P0[i, j] * (lA[i] * lB[j]**delta if first_A else lA[i]**delta * lB[j]))
        return W / sum(W)
    SAB, SBA = scaled(True), scaled(False)
    gap0 = sp.simplify((marg_A(SAB)[1] - marg_A(SBA)[1]).subs(c, 0))
    ck.ne("(5) scaled Bayes-factor update: A-marginal differs between sequences at c=0, generic prior, delta=1/2",
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
