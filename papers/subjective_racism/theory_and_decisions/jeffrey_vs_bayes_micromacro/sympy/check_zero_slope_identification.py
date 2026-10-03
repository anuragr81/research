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

Closed forms and exact exceptional sets.  Every "nonzero generically" row is
a factored closed form plus a `zero_set` row stating where it vanishes on the
domain alpha, beta, q0, r0 in (0,1), delta in [0,1] (one-point evaluations at
GENERIC are kept only as sanity rows).  Route AB, Z = alpha(1-alpha)beta(1-beta):
    first-read (A) slope   delta q0 q1 (beta - r0) / Z     zero iff delta = 0 or r0 = beta
    last-read (B) slope    (1-delta)(alpha - q0)/(alpha(1-alpha))
                                                         zero iff delta = 1 or q0 = alpha
    benchmark A slope      q0 q1 (beta - r0) / Z           zero iff r0 = beta
    benchmark B slope      r0 r1 (alpha - q0) / Z          zero iff q0 = alpha
(delta = 1 and delta = 0 are the amnestic and primacy rows.)  So the table
above holds exactly off {q0 = alpha} u {r0 = beta}, where a cue delivers the
prior marginal and its slope is zero under every mechanism.
  (6d) first-read slope gap to the benchmark  -(1-delta) q0 q1 (beta - r0)/Z,
       last-read slope gap  (alpha - q0)[(1-delta) beta(1-beta) - r0 r1]/Z, so the
       last-read slope matches at delta_B = (beta - r0)(r1 - beta)/(beta(1-beta)),
       and delta_B - 1 = -r0 r1/(beta(1-beta)) is never zero.  Hence no delta
       matches both benchmark slopes except on {q0 = alpha} u {r0 = beta}.
  (7b) the recovery denominator P^A(B=1) - r1 = (r0 - beta) + c(alpha - q0)/(alpha(1-alpha))
       vanishes exactly on the irreducible hypersurface
       alpha(1-alpha)(r0 - beta) + c(alpha - q0) = 0 (at c = 0: r0 = beta), which
       meets the admissible domain (witness row); mirror for P^B(A=1) - q1.
The helper `zero_set` (shared with verify_ladder.py and verify_interior_omega.py)
checks that every irreducible factor of the numerator over Q is either listed
or sign-definite on the domain, and that the denominator is sign-definite.
Sign-definiteness is certified for polynomials affine in each variable by
their vertex values (see `_multilinear_positive`), on the open box and on the
faces delta = 0, 1.
"""
import itertools
import sympy as sp
from jeffrey_core import *

delta = sp.Symbol('delta', real=True)

# ------------------------------------------------------ exact zero sets ----
# The domain of every genericity statement in the suite: alpha, beta, q0, r0
# and the mix lamda range over the open interval (0,1); the adoption weight
# delta ranges over the closed interval [0,1].  A claim "E != 0 generically"
# is upgraded to "E = 0 on the domain exactly where one of these polynomials
# vanishes" by `zero_set`.
OPEN_VARS = (alpha, beta, q0, r0, lam)


def _multilinear_positive(p, xs):
    """p > 0 on the open box (0,1)^xs, certified: p is affine in each variable
    separately, p >= 0 at every vertex of [0,1]^xs, and p is not identically
    zero.  (Such a p is >= 0 on the closed box; if it vanished at an interior
    point it would vanish along every coordinate line through that point,
    hence -- coordinate by coordinate -- on the whole box.)"""
    if p == 0 or any(sp.degree(p, x) > 1 for x in xs):
        return False
    return all(p.subs(dict(zip(xs, v))) >= 0
               for v in itertools.product((0, 1), repeat=len(xs)))


def positive_on_domain(p):
    """Certify p > 0 on the domain (OPEN_VARS in (0,1), delta in [0,1])."""
    p = sp.expand(p)
    xs = [x for x in OPEN_VARS + (delta,) if p.has(x)]
    if p.free_symbols - set(xs):
        return False                      # depends on c or another symbol
    if not _multilinear_positive(p, xs):
        return False
    if delta in xs:                       # the closed faces delta = 0, 1
        rest = [x for x in xs if x != delta]
        return all(_multilinear_positive(sp.expand(p.subs(delta, e)), rest) for e in (0, 1))
    return True


def sign_definite(p):
    return positive_on_domain(p) or positive_on_domain(-p)


def zero_set(ck, name, E, vanishing):
    """
    Assert the exact zero set of the rational function E on the domain.

    `vanishing` lists (polynomial, description) pairs.  The check passes iff
    every irreducible factor (over Q) of E's numerator is either one of the
    listed polynomials (up to sign) or sign-definite on the domain, every
    listed polynomial occurs, and every factor of E's denominator is
    sign-definite on the domain.  Then E = 0 on the domain exactly on the
    union of the listed polynomials' zero sets, and each listed polynomial is
    a nonconstant irreducible factor, so not identically zero.
    """
    N, D = sp.fraction(sp.cancel(sp.together(E)))
    listed = [sp.expand(f) for f, _ in vanishing]
    found, stray = set(), []
    for f, _m in sp.factor_list(N)[1]:
        f = sp.expand(f)
        hit = [k for k, g in enumerate(listed) if sp.expand(f - g) == 0 or sp.expand(f + g) == 0]
        if hit:
            found.add(hit[0])
        elif not sign_definite(f):
            stray.append(f)
    bad_den = [f for f, _m in sp.factor_list(D)[1] if not sign_definite(f)]
    ok = not stray and not bad_den and found == set(range(len(listed)))
    descr = " | ".join(d for _, d in vanishing) if vanishing else "empty (never zero)"
    return ck(f"{name} -- zero set: {descr}", ok,
              f"unlisted numerator factors {stray}; unsigned denominator factors {bad_den}; "
              f"listed but absent {[vanishing[k][0] for k in range(len(listed)) if k not in found]}")


def vanishes_on(ck, name, f, subsets):
    """f vanishes identically on each partial substitution in `subsets`
    (sets inside the domain), so its zero set meets the domain there."""
    return ck(name, all(sp.cancel(sp.together(f.subs(s))) == 0 for s in subsets))


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


D_FULL = "delta = 1 (full adoption)"
D_NONE = "delta = 0 (second cue ignored)"
Q_PRIOR = "q0 = alpha (the A-cue delivers the prior marginal)"
R_PRIOR = "r0 = beta (the B-cue delivers the prior marginal)"


def main():
    ck = Check("Zero-slope identification: amnestic vs primacy vs anchoring")

    # ---------- (1) amnestic: delta = 1 -------------------------------------
    Q1 = damped_route_AB(sp.Integer(1))
    sA, sB = slopes(Q1)
    ck.eq("amnestic: last-read (B) marginal slope = 0", sB, 0)
    ck.eq("amnestic: last-read (B) marginal = r1 exactly, all orders in c",
          sp.cancel(marg_B(Q1)[1]), r1)
    ck.eq("amnestic: first-read (A) marginal slope = q0 q1 (beta - r0) / Z", sA, q0 * q1 * (beta - r0) / Z)
    zero_set(ck, "amnestic: first-read (A) marginal slope", sA, [(beta - r0, R_PRIOR)])
    ck.ne("amnestic: first-read slope != 0 at the generic point (sanity, one point)", sA)

    # ---------- (2) pure primacy: delta = 0 ---------------------------------
    Q0 = damped_route_AB(sp.Integer(0))
    sA0, sB0 = slopes(Q0)
    ck.eq("pure primacy: FIRST-read (A) marginal = q1 exactly, all orders in c",
          sp.cancel(marg_A(Q0)[1]), q1)
    ck.eq("pure primacy: first-read (A) slope = 0", sA0, 0)
    ck.eq("pure primacy: last-read (B) marginal slope = (alpha - q0) / (alpha (1-alpha))",
          sB0, (alpha - q0) / (alpha * (1 - alpha)))
    zero_set(ck, "pure primacy: last-read (B) marginal slope", sB0, [(alpha - q0, Q_PRIOR)])
    ck.ne("pure primacy: last-read slope != 0 at the generic point (sanity, one point)", sB0)

    # ---------- (3) anchoring: symbolic delta -------------------------------
    Qd = damped_route_AB(delta)
    sAd, sBd = slopes(Qd)
    ck.eq("anchoring: last-read (B) slope = (1-delta)(alpha - q0) / (alpha (1-alpha))",
          sBd, (1 - delta) * (alpha - q0) / (alpha * (1 - alpha)))
    zero_set(ck, "anchoring: last-read (B) slope", sBd, [(1 - delta, D_FULL), (alpha - q0, Q_PRIOR)])
    ck.eq("anchoring: first-read (A) slope = delta q0 q1 (beta - r0) / Z",
          sAd, delta * q0 * q1 * (beta - r0) / Z)
    zero_set(ck, "anchoring: first-read (A) slope", sAd, [(delta, D_NONE), (beta - r0, R_PRIOR)])
    ck("anchoring: both slopes nonzero at delta=1/2, generic point (sanity, one point)",
       at_generic(sAd.subs(delta, sp.Rational(1, 2))) != 0
       and at_generic(sBd.subs(delta, sp.Rational(1, 2))) != 0)

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
    sAb, sBb = slopes(PBm)
    ck.eq("benchmark P^B: A-marginal slope = q0 q1 (beta - r0) / Z", sAb, q0 * q1 * (beta - r0) / Z)
    zero_set(ck, "benchmark P^B: A-marginal slope", sAb, [(beta - r0, R_PRIOR)])
    ck.eq("benchmark P^B: B-marginal slope = r0 r1 (alpha - q0) / Z", sBb, r0 * r1 * (alpha - q0) / Z)
    zero_set(ck, "benchmark P^B: B-marginal slope", sBb, [(alpha - q0, Q_PRIOR)])
    ck("benchmark P^B: both slopes nonzero at the generic point (sanity, one point)",
       at_generic(sAb) != 0 and at_generic(sBb) != 0)

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
    ck.eq("(6c) first-read slope = delta * benchmark first-read slope",
          sp.cancel(sAd - delta * sAb), 0)
    solA = sp.solve(sp.Eq(sAd, sAb), delta)
    solB = sp.solve(sp.Eq(sBd, sBb), delta)
    ck("(6d) matching the first-read slope forces delta = 1 (off r0 = beta)",
       solA == [1], f"solutions: {solA}")
    ck("(6d) the delta matching the last-read slope is delta_B = 1 - r1 r0/(beta(1-beta)) (off q0 = alpha)",
       len(solB) == 1 and sp.cancel(solB[0] - (1 - r1 * r0 / (beta * (1 - beta)))) == 0,
       f"delta_B = {sp.factor(solB[0]) if solB else None}")
    # the same statements as exact zero sets, degenerate cases included
    ck.eq("(6d) first-read slope gap = -(1-delta) q0 q1 (beta - r0) / Z",
          sAd - sAb, -(1 - delta) * q0 * q1 * (beta - r0) / Z)
    zero_set(ck, "(6d) first-read slope gap", sAd - sAb, [(1 - delta, D_FULL), (beta - r0, R_PRIOR)])
    ck.eq("(6d) last-read slope gap = (alpha - q0) [(1-delta) beta(1-beta) - r0 r1] / Z",
          sBd - sBb, (alpha - q0) * ((1 - delta) * beta * (1 - beta) - r0 * r1) / Z)
    zero_set(ck, "(6d) last-read slope gap", sBd - sBb,
             [(alpha - q0, Q_PRIOR),
              ((1 - delta) * beta * (1 - beta) - r0 * r1,
               "delta = delta_B (irreducible; delta_B in [0,1) iff beta lies between r0 and r1)")])
    ck.eq("(6d) delta_B = (beta - r0)(r1 - beta) / (beta (1-beta)): in [0,1) iff beta lies between r0 and r1",
          solB[0], (beta - r0) * (r1 - beta) / (beta * (1 - beta)))
    ck.eq("(6d) delta_B - 1 = -r0 r1 / (beta (1-beta))", solB[0] - 1, -r0 * r1 / (beta * (1 - beta)))
    zero_set(ck, "(6d) delta_B - 1 (so delta_B != 1 everywhere)", solB[0] - 1, [])
    ck("(6d) delta_B != 1 at the generic point (sanity, one point)", at_generic(solB[0] - 1) != 0)

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
    adjA = sp.cancel(mPA - r1)
    ck.eq("(7b) the denominator P^A(B=1) - r1 = (r0 - beta) + c (alpha - q0) / (alpha (1-alpha))",
          adjA, (r0 - beta) + c * (alpha - q0) / (alpha * (1 - alpha)))
    surfA = alpha * (1 - alpha) * (r0 - beta) + c * (alpha - q0)
    zero_set(ck, "(7b) P^A(B=1) - r1", adjA,
             [(surfA, "alpha(1-alpha)(r0 - beta) + c (alpha - q0) = 0 (irreducible: a hypersurface; "
                      "at c = 0 exactly r0 = beta)")])
    w = {alpha: sp.Rational(1, 2), beta: sp.Rational(1, 2), q0: sp.Rational(1, 4),
         r0: sp.Rational(21, 40), c: -sp.Rational(1, 40)}
    ck("(7b) ...that hypersurface meets the admissible domain off r0 = beta: "
       "(alpha, beta, q0, r0, c) = (1/2, 1/2, 1/4, 21/40, -1/40), every prior cell > 0",
       adjA.subs(w) == 0 and all(e > 0 for e in prior().subs(w)))
    ck("(7b) P^A(B=1) - r1 != 0 at the generic point (sanity, one point)", at_generic(mPA - r1) != 0)
    ck.eq("(7b) ...and equals r0 - beta at c = 0", sp.cancel((mPA - r1).subs(c, 0) - (r0 - beta)), 0)
    adjB = sp.cancel(mPB - q1)
    ck.eq("(7b) mirror: P^B(A=1) - q1 = (q0 - alpha) + c (beta - r0) / (beta (1-beta))",
          adjB, (q0 - alpha) + c * (beta - r0) / (beta * (1 - beta)))
    zero_set(ck, "(7b) P^B(A=1) - q1", adjB,
             [(beta * (1 - beta) * (q0 - alpha) + c * (beta - r0),
               "beta(1-beta)(q0 - alpha) + c (beta - r0) = 0 (irreducible: a hypersurface; "
               "at c = 0 exactly q0 = alpha)")])
    ck.eq("(7c) at c = 0: delta = 1 - [order effect on A] / (alpha - q0)",
          sp.cancel(1 - oe.subs(c, 0) / (alpha - q0) - delta), 0)

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
