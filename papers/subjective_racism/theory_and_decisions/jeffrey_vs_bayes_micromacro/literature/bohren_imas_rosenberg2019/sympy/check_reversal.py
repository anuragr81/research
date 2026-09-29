"""
Does amnestic Jeffrey updating break Bohren-Imas-Rosenberg's Proposition 2?

BIR (AER 2019) Proposition 2: with a single evaluator type holding belief-based
partiality and no preference-based partiality, discrimination decreases across
periods but NEVER reverses.  Their proof establishes that "the posterior mean is
increasing in the prior mean", so posteriors inherit the ordering of priors.
That monotonicity is a property of Bayesian conditioning.  Amnestic Jeffrey
updating violates it exactly: the last-read cue pins its own marginal
independently of the prior.

This script asks whether that violation actually produces a REVERSAL.

Mapping to the 2x2 model of the paper:
  A = ability   (the attribute the evaluation history speaks to)
  B = quality   (the attribute the current signal speaks to, and the one the
                 evaluation is based on: BIR eq. (2), v = E[q | h,s,g] - c_ig)
  c = prior covariance between ability and quality
Discrimination is BIR eq. (3): D = E[q | h,s,M] - E[q | h,s,F], i.e. the gap in
posterior P(B=1) between a male and a female worker with the SAME history and
signal.  D > 0 is discrimination against women; a reversal needs D < 0.

Groups differ ONLY in their (correct) prior: statistical discrimination in BIR's
sense, no animus, no misspecification.
"""
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..', 'sympy'))
import sympy as sp
from jeffrey_core import (prior, jeffrey_A, jeffrey_B, bayes, marg_B, Check)

# --- group priors: correct, differing.  alpha_g = P_g(A=0) = P(low ability) ---
# Women have a lower prior on high ability: alpha_F > alpha_M.
aM, aF = sp.Rational(2, 5), sp.Rational(3, 5)     # P(A=0): M favoured
bM, bF = sp.Rational(2, 5), sp.Rational(3, 5)     # P(B=0): quality tracks ability
c      = sp.Rational(1, 50)                        # prior covariance, small


def posteriors_for(alpha_g, beta_g, q0_g, r0_g, cc=c):
    """(Bayes benchmark, route AB, route BA) for one group."""
    P = sp.Matrix([[alpha_g*beta_g + cc,        alpha_g*(1-beta_g) - cc],
                   [(1-alpha_g)*beta_g - cc,    (1-alpha_g)*(1-beta_g) + cc]])
    qq, rr = [q0_g, 1 - q0_g], [r0_g, 1 - r0_g]
    PB    = bayes(P, qq, rr)
    PJ_AB = jeffrey_B(jeffrey_A(P, qq), rr)   # history first, signal last
    PJ_BA = jeffrey_A(jeffrey_B(P, rr), qq)   # signal first, history last
    return PB, PJ_AB, PJ_BA


def quality(Q):
    """E[q] proxy: posterior P(B=1)."""
    return sp.nsimplify(marg_B(Q)[1])


def D(rule_M, rule_F):
    """BIR eq.(3): discrimination = quality(M) - quality(F).  >0 against women."""
    return sp.nsimplify(quality(rule_M) - quality(rule_F))


def main():
    ck = Check("BIR Proposition 2 under amnestic Jeffrey updating")

    # ---------- Part 1: novice history (single cue) --------------------------
    # With h = empty there is only the current signal.  Proposition IMM: on one
    # cue Jeffrey and the benchmark coincide exactly, so the novice comparison is
    # identical under both rules.
    r0 = sp.Rational(1, 3)          # signal: leans toward good quality
    PB_M = bayes(prior_of(aM, bM), [sp.Rational(1,2), sp.Rational(1,2)], [r0, 1-r0])
    PB_F = bayes(prior_of(aF, bF), [sp.Rational(1,2), sp.Rational(1,2)], [r0, 1-r0])
    D_novice = D(PB_M, PB_F)
    ck("novice (single cue): women face discrimination, D > 0",
       D_novice > 0, f"D_novice = {D_novice} = {float(D_novice):.5f}")

    # ---------- Part 2: advanced history, cues IDENTICAL across groups -------
    # BIR's design holds the evaluation history and current signal fixed across
    # genders, so the delivered credences are the same for M and F.
    q0 = sp.Rational(1, 4)          # history: strong evidence of high ability
    PBm, ABm, BAm = posteriors_for(aM, bM, q0, r0)
    PBf, ABf, BAf = posteriors_for(aF, bF, q0, r0)

    D_bayes = D(PBm, PBf)
    D_AB    = D(ABm, ABf)           # signal read LAST  -> quality pinned
    D_BA    = D(BAm, BAf)           # history read LAST -> ability pinned

    print(f"\n  D_bayes = {D_bayes} = {float(D_bayes):+.6f}")
    print(f"  D_AB    = {D_AB} = {float(D_AB):+.6f}   (quality pinned)")
    print(f"  D_BA    = {D_BA} = {float(D_BA):+.6f}   (ability pinned)\n")

    ck("Bayes benchmark: no reversal, D stays > 0 (their Proposition 2)",
       D_bayes > 0, f"D_bayes = {float(D_bayes):.6f}")
    ck("route AB (quality read last): quality is pinned to the delivered "
       "credence, identically for both groups, so D = 0 EXACTLY",
       D_AB == 0, f"D_AB = {D_AB}")
    ck("route BA (ability read last): D agrees with Bayes to first order, "
       "so no reversal on this route either",
       sp.sign(D_BA) == sp.sign(D_bayes), f"D_BA = {float(D_BA):.6f}")

    # Does ANY mixture of the two routes reverse?
    lam = sp.Symbol('lam')
    D_mix = sp.expand(lam * D_AB + (1 - lam) * D_BA)
    neg = sp.solve([sp.Lt(D_mix, 0), sp.Ge(lam, 0), sp.Le(lam, 1)], lam)
    ck("NO mixture of reading orders reverses discrimination when the delivered "
       "credences are group-independent",
       neg == sp.false or neg == [] or neg is sp.S.false,
       f"lambda with D_mix < 0: {neg}")

    # ---------- Part 3: group-DEPENDENT impressions (BIR's channel (ii)) -----
    # BIR's own second channel: the signal required for a given evaluation is
    # DECREASING in the prior mean (their eq. 6), so the same observed history is
    # a STRONGER signal of ability for the lower-prior group.  Under Jeffrey the
    # delivered credence is an impression, not a posterior, so this channel acts
    # directly on q0: the woman's history reads as more impressive.
    q0_M, q0_F = sp.Rational(1, 4), sp.Rational(1, 6)   # q0 = P(A=0): lower = more impressive
    PBm2, ABm2, BAm2 = posteriors_for(aM, bM, q0_M, r0)
    PBf2, ABf2, BAf2 = posteriors_for(aF, bF, q0_F, r0)

    D_bayes2 = D(PBm2, PBf2)
    D_AB2    = D(ABm2, ABf2)
    D_BA2    = D(BAm2, BAf2)
    print(f"  with channel (ii) impressions (q0_M={q0_M}, q0_F={q0_F}):")
    print(f"  D_bayes = {float(D_bayes2):+.6f}")
    print(f"  D_AB    = {float(D_AB2):+.6f}   (quality pinned)")
    print(f"  D_BA    = {float(D_BA2):+.6f}   (ability pinned)\n")

    ck("channel (ii) alone does NOT reverse the Bayes benchmark here "
       "(their Proposition 2's 'first effect dominates')",
       D_bayes2 > 0, f"D_bayes = {float(D_bayes2):.6f}")
    ck("but under amnestic updating with ABILITY read last, discrimination "
       "REVERSES: D < 0, men face discrimination",
       D_BA2 < 0, f"D_BA = {float(D_BA2):.6f}")

    # The reason, isolated: the pinned ability marginal is the impression itself.
    ck("the mechanism: ability posterior is the delivered credence exactly, "
       "independent of the prior -- so the prior channel is annihilated",
       sp.nsimplify(marg_A_1(BAm2)) == 1 - q0_M
       and sp.nsimplify(marg_A_1(BAf2)) == 1 - q0_F,
       f"P_M(A=1) = {marg_A_1(BAm2)} vs 1-q0_M = {1-q0_M}; "
       f"P_F(A=1) = {marg_A_1(BAf2)} vs 1-q0_F = {1-q0_F}")

    return ck.done()


def prior_of(alpha_g, beta_g, cc=c):
    return sp.Matrix([[alpha_g*beta_g + cc,     alpha_g*(1-beta_g) - cc],
                      [(1-alpha_g)*beta_g - cc, (1-alpha_g)*(1-beta_g) + cc]])


def marg_A_1(Q):
    return sp.nsimplify(Q[1, 0] + Q[1, 1])


if __name__ == "__main__":
    sys.exit(0 if main() else 1)
