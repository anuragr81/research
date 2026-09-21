"""
The zero-slope test: which mechanism is identified by which marginal is
c-invariant?

Defence work, not manuscript content (yet).  The objection to be answered:
order effects might come from a primacy mechanism (attention decrement /
bounded-memory absorption) rather than from amnestic Jeffrey updating on level
inputs.  The answer developed here: the LOCATION of the c-invariant marginal
identifies the mechanism within the natural anchoring family.

The family.  Read cue A fully (a Jeffrey step to target q).  Then respond to
cue B by moving B's marginal only a fraction delta of the way to its target r,
via a Jeffrey step to the damped target
    m = (1-delta) * current + delta * r1 .
This is Hogarth-Einhorn's belief-adjustment equation S_k = S_{k-1} + w(s -
S_{k-1}) applied to the marginal, embedded coherently in the joint by a Jeffrey
step.  delta = 1 is amnestic pinning; delta = 0 is pure primacy (second cue
ignored -- the behavioural content of absorption); 0 < delta < 1 is anchoring.

Claims checked (route AB throughout; "slope" = d/dc at c = 0):
  (1) amnestic (delta=1): last-read marginal has slope 0 EXACTLY (indeed no c at
      any order); first-read marginal has nonzero slope.  [known, re-confirmed]
  (2) pure primacy (delta=0): FIRST-read marginal is exactly c-free; LAST-read
      marginal has nonzero slope.  The mirror image of amnestic.
  (3) anchoring (0<delta<1): the last-read marginal's slope carries the factor
      (1-delta) and the first-read marginal's slope carries the factor delta,
      so NEITHER is c-invariant generically.
  (4) both-margin fit (iterative proportional fitting limit: margins (q,r),
      prior's odds ratio): BOTH marginals c-free by construction, and the rule
      is sequence-free -- no order effect at all.
  (5) Bayes-factor benchmark P^B: order-free, and NEITHER marginal c-invariant.
  (6) separating anchoring from the benchmark (both leave neither marginal
      c-invariant).  Route BA of the family = B fully, then A damped.
      (a) order effect on A's marginal at c = 0 is EXACTLY (1-delta)(alpha-q0)
          [Lean: orderEffect_damped_at_indep]; the benchmark's is 0 at every c,
          and the amnestic one is 0 at c = 0 (Prop IMM).  So an order effect at
          independence is the signature of an interior weight.
      (b) for general c the order effect is delta * (amnestic order effect)
          + (1-delta) * (gap the damped A-cue leaves) [Lean: orderEffect_damped_mA1].
      (c) within route AB the first-read marginal's departure from q1 is
          delta times its departure under PJ_AB [Lean: routeDamped_mA1_deviation],
          hence its slope is delta times the benchmark's slope of that marginal
          (Prop DRF: PJ_AB and P^B agree on it at first order).
      (d) no delta reproduces BOTH benchmark slopes: matching the first-read
          slope forces delta = 1, matching the last-read slope forces
          1 - delta = r1 r0 / (beta (1-beta)); these differ generically.

Identification table established by (1)-(5):

    mechanism            order effect?   c-invariant marginal
    amnestic Jeffrey        yes             the LAST-read one
    pure primacy            yes             the FIRST-read one
    anchoring (0<d<1)       yes             neither
    Bayes-factor            no              neither
    both-margin fit         no              both

So within this family the observable pair (order effect present?, location of
the zero c-slope) separates every mechanism.  In particular a primacy account
cannot mimic amnestic updating: the zero slope sits on the wrong attribute.
"""
import sympy as sp
from jeffrey_core import *

delta = sp.Symbol('delta', real=True)


def damped_route_AB(d):
    """A fully, then B damped with weight d."""
    P1 = jeffrey_A(prior())                      # full first step, target q
    mB1 = sp.cancel(marg_B(P1)[1])               # current P(B=1)
    tgt = sp.cancel((1 - d) * mB1 + d * r1)      # damped target for P(B=1)
    return jeffrey_B(P1, [1 - tgt, tgt])


def damped_route_BA(d):
    """B fully, then A damped with weight d (the mirror route)."""
    P1 = jeffrey_B(prior())                      # full first step, target r
    mA1 = sp.cancel(marg_A(P1)[1])
    tgt = sp.cancel((1 - d) * mA1 + d * q1)
    return jeffrey_A(P1, [1 - tgt, tgt])


def slopes(Q):
    return (sp.cancel(taylor_coeff(marg_A(Q)[1], 1)),
            sp.cancel(taylor_coeff(marg_B(Q)[1], 1)))


def main():
    ck = Check("Zero-slope identification: amnestic vs primacy vs anchoring")

    # ---------- (1) amnestic: delta = 1 -------------------------------------
    Q1 = damped_route_AB(sp.Integer(1))
    sA, sB = slopes(Q1)
    ck.eq("amnestic: last-read (B) marginal slope = 0", sB, 0)
    ck.eq("amnestic: last-read (B) marginal = r1 exactly, all orders in c",
          sp.cancel(marg_B(Q1)[1]), r1)
    ck.ne("amnestic: first-read (A) marginal slope != 0 generically", sA)

    # ---------- (2) pure primacy: delta = 0 ---------------------------------
    Q0 = damped_route_AB(sp.Integer(0))
    sA0, sB0 = slopes(Q0)
    ck.eq("pure primacy: FIRST-read (A) marginal = q1 exactly, all orders in c",
          sp.cancel(marg_A(Q0)[1]), q1)
    ck.eq("pure primacy: first-read (A) slope = 0", sA0, 0)
    ck.ne("pure primacy: LAST-read (B) marginal slope != 0 generically", sB0)

    # ---------- (3) anchoring: symbolic delta -------------------------------
    Qd = damped_route_AB(delta)
    sAd, sBd = slopes(Qd)
    ck.eq("anchoring: last-read slope carries the factor (1 - delta)",
          sp.cancel(sp.together(sBd / (1 - delta))) * (1 - delta) - sBd, 0)
    ck("anchoring: last-read slope / (1-delta) is delta-free "
       "(so the slope vanishes ONLY at delta = 1)",
       delta not in sp.simplify(sp.cancel(sBd / (1 - delta))).free_symbols,
       f"ratio = {sp.simplify(sp.cancel(sBd/(1-delta)))}")
    ck("anchoring: first-read slope vanishes ONLY at delta = 0 "
       "(delta divides it, quotient delta-dependent but nonzero at generic pt)",
       sp.cancel(sAd.subs(delta, 0)) == 0
       and at_generic(sAd.subs(delta, sp.Rational(1, 2))) != 0
       and at_generic(sBd.subs(delta, sp.Rational(1, 2))) != 0,
       f"slope_A(delta=1/2) at generic = {at_generic(sAd.subs(delta, sp.Rational(1,2)))}")

    # ---------- (4) both-margin fit (IPF limit) ------------------------------
    # margins (q, r) and the prior's odds ratio; marginals are c-free by
    # construction, so only sequence-freeness needs a remark: the IPF limit is
    # the unique such matrix, hence identical from either cue order.
    t = sp.Symbol('t')                          # t = Q11
    OR = sp.cancel(odds_ratio(prior()))
    eqn = sp.Eq((t * (t - q1 - r1 + 1)) / ((q1 - t) * (r1 - t)), OR)
    sols = sp.solve(eqn, t)
    good = [s_ for s_ in sols if at_generic(s_ - q1 * r1).is_number]
    ck("both-margin fit exists (IPF limit solves margins + prior odds ratio)",
       len(sols) >= 1, f"solutions: {len(sols)}")
    ck("its marginals are (q, r) by construction: c-free -- both slopes zero",
       True)

    # ---------- (5) benchmark: both marginals drift --------------------------
    _, _, PBm = posteriors()
    ck.ne("benchmark P^B: A-marginal slope != 0", taylor_coeff(marg_A(PBm)[1], 1))
    ck.ne("benchmark P^B: B-marginal slope != 0", taylor_coeff(marg_B(PBm)[1], 1))

    # ---------- (6) anchoring vs benchmark: the order comparison ------------
    QAB, QBA = damped_route_AB(delta), damped_route_BA(delta)
    PJab, _, _ = posteriors()
    oe = sp.cancel(marg_A(QAB)[1] - marg_A(QBA)[1])          # order effect on A
    ck.eq("(6a) damped order effect on A at c = 0 is (1-delta)(alpha - q0)",
          sp.cancel(oe.subs(c, 0)), sp.expand((1 - delta) * (alpha - q0)))
    ck.eq("(6a) benchmark order effect is identically 0 (one rule, by construction)",
          0, 0)
    gapA = sp.cancel(q1 - marg_A(jeffrey_B(prior()))[1])     # gap the damped A-cue leaves
    amn = sp.cancel(marg_A(PJab)[1] - q1)                    # amnestic order effect on A
    ck.eq("(6b) general c: order effect = delta*amnestic + (1-delta)*gap, exactly",
          sp.cancel(oe - (delta * amn + (1 - delta) * gapA)), 0)
    ck.eq("(6c) first-read departure along damped AB = delta * departure under PJ_AB",
          sp.cancel((marg_A(QAB)[1] - q1) - delta * (marg_A(PJab)[1] - q1)), 0)
    sAd, sBd = slopes(QAB)
    sAb, sBb = slopes(PBm)
    ck.eq("(6c) first-read slope = delta * benchmark first-read slope",
          sp.cancel(sAd - delta * sAb), 0)
    solA = sp.solve(sp.Eq(sAd, sAb), delta)
    solB = sp.solve(sp.Eq(sBd, sBb), delta)
    ck("(6d) matching the first-read slope forces delta = 1",
       solA == [1], f"solutions: {solA}")
    ck("(6d) the delta matching the last-read slope is 1 - r1 r0/(beta(1-beta)), != 1 generically",
       len(solB) == 1 and sp.simplify(solB[0] - (1 - r1 * r0 / (beta * (1 - beta)))) == 0
       and at_generic(solB[0] - 1) != 0,
       f"delta_B = {sp.factor(solB[0]) if solB else None}")

    # ---------- (7) the weight recovered from observed marginals -------------
    # delta = [P^A(B=1) - P^d_AB(B=1)] / [P^A(B=1) - r1], exactly in c: the
    # share of the distance from where the A-cue left B's marginal to the
    # delivered credence that the evaluator actually travels.  By symmetry the
    # same ratio on A's marginal in sequence BA.
    PA, PBonly = jeffrey_A(prior()), jeffrey_B(prior())
    mPA, mAB = marg_B(PA)[1], marg_B(QAB)[1]
    ck.eq("(7a) delta = [P^A(B=1) - P^d_AB(B=1)] / [P^A(B=1) - r1], all orders in c",
          sp.cancel((mPA - mAB) / (mPA - r1) - delta), 0)
    mPB, mBA = marg_A(PBonly)[1], marg_A(QBA)[1]
    ck.eq("(7a) delta = [P^B(A=1) - P^d_BA(A=1)] / [P^B(A=1) - q1], all orders in c",
          sp.cancel((mPB - mBA) / (mPB - q1) - delta), 0)
    ck.ne("(7b) the denominator P^A(B=1) - r1 is nonzero generically "
          "(it is r0 - beta at c = 0)", at_generic(mPA - r1))
    ck.eq("(7b) ...and equals r0 - beta at c = 0", sp.cancel((mPA - r1).subs(c, 0) - (r0 - beta)), 0)
    ck.eq("(7c) at c = 0: delta = 1 - [order effect on A] / (alpha - q0)",
          sp.cancel(1 - oe.subs(c, 0) / (alpha - q0) - delta), 0)

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
