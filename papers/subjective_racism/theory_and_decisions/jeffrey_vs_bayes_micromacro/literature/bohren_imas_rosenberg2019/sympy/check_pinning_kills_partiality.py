"""
Does Jeffrey pinning silence belief-based partiality?

Bohren, Imas & Rosenberg (AER 2019) define discrimination (their eq. 3) as
    D(h,s) = v(h,s,M) - v(h,s,F),
the difference in evaluations of a male and female worker with the SAME history
and signal, where (their eq. 2) v = E[q | h,s,g] - c_g.  With no preference
partiality (c_g = 0) discrimination is the gap in posterior expected quality.

Model here (the paper's 2x2, mapped to theirs):
    A = ability, B = quality, correlated through the prior covariance c.
    Group g has prior marginals P_g(A=1) = a_g, P_g(B=1) = b_g.
    "Belief-based partiality" = a_M > a_F and b_M > b_F, held as CORRECT beliefs.
    No animus.

Two updating rules are compared on the SAME evidence:
  * BAYESIAN evaluator with EXOGENOUS likelihoods (what BIR assume): the signal
    has a fixed likelihood ratio, identical across groups.  This is the object
    their Proposition 2 is about -- NOT the paper's benchmark P^B, which matches
    likelihoods to each prior and is therefore a different rule.
  * JEFFREY evaluator: the cue delivers a credence on a partition and the update
    holds conditionals fixed.

Claims checked:
  (1) pinning is exact: a Jeffrey step sets its own partition's marginal to the
      delivered credence identically -- for every prior and every covariance;
  (2) Bayesian with exogenous likelihoods: D != 0, and the ordering of posteriors
      follows the ordering of priors (the monotonicity BIR's Prop. 2 rests on);
  (3) Jeffrey with the QUALITY cue read last: D = 0 EXACTLY, for all priors and
      all c -- belief partiality is silenced without any gain in precision;
  (4) Jeffrey with the ABILITY cue read last: D != 0, so the silencing is
      route-dependent;
  (5) the silencing does not require small c, and does not require the two groups
      to share anything except the cues.
"""
import sympy as sp

# ---------------------------------------------------------------- symbols ---
a, b, c = sp.symbols('a b c', real=True)          # P(A=1), P(B=1), covariance
r0, q0 = sp.symbols('r0 q0', real=True)           # delivered credences: P(B=0), P(A=0)
L1, L0 = sp.symbols('L1 L0', positive=True)       # exogenous likelihoods on B


def joint(aa, bb, cc):
    """2x2 joint with P(A=1)=aa, P(B=1)=bb, covariance cc.
    Rows indexed by A in {0,1}, columns by B in {0,1}."""
    return sp.Matrix([[(1-aa)*(1-bb) + cc, (1-aa)*bb - cc],
                      [aa*(1-bb) - cc,     aa*bb + cc]])


def margB1(Q):
    return sp.cancel(sp.together(Q[0, 1] + Q[1, 1]))


def margA1(Q):
    return sp.cancel(sp.together(Q[1, 0] + Q[1, 1]))


def jeffrey_on_B(Q, rB1):
    """Jeffrey step on B's partition: set P(B=1)=rB1, hold A|B fixed."""
    m0 = Q[0, 0] + Q[1, 0]
    m1 = Q[0, 1] + Q[1, 1]
    return sp.Matrix(2, 2, lambda i, j:
                     sp.cancel(((1-rB1) * Q[i, 0] / m0) if j == 0
                               else (rB1 * Q[i, 1] / m1)))


def jeffrey_on_A(Q, qA1):
    """Jeffrey step on A's partition: set P(A=1)=qA1, hold B|A fixed."""
    n0 = Q[0, 0] + Q[0, 1]
    n1 = Q[1, 0] + Q[1, 1]
    return sp.Matrix(2, 2, lambda i, j:
                     sp.cancel(((1-qA1) * Q[0, j] / n0) if i == 0
                               else (qA1 * Q[1, j] / n1)))


def bayes_on_B_exogenous(Q, l1, l0):
    """Bayesian update on a signal about B with EXOGENOUS likelihoods
    l1 = P(s|B=1), l0 = P(s|B=0), identical across groups."""
    W = sp.Matrix(2, 2, lambda i, j: Q[i, j] * (l1 if j == 1 else l0))
    Z = sum(W)
    return sp.Matrix(2, 2, lambda i, j: sp.cancel(W[i, j] / Z))


ok = True
def check(name, cond, detail=""):
    global ok
    status = "PASS" if cond else "FAIL"
    print(f"  [{status}] {name}")
    if detail and not cond:
        print(f"         {detail}")
    if not cond:
        ok = False


print("=" * 78)
print("Jeffrey pinning and belief-based partiality (BIR eq. 2-3)")
print("=" * 78)

# ---- (1) pinning is exact, symbolically, for arbitrary prior and covariance --
P = joint(a, b, c)
pinnedB = margB1(jeffrey_on_B(P, 1 - r0))
check("(1) Jeffrey step on B sets P(B=1) = 1-r0 EXACTLY, for all (a,b,c)",
      sp.simplify(pinnedB - (1 - r0)) == 0, f"got {pinnedB}")
pinnedA = margA1(jeffrey_on_A(P, 1 - q0))
check("    ... and symmetrically on A, for all (a,b,c)",
      sp.simplify(pinnedA - (1 - q0)) == 0, f"got {pinnedA}")

# ---- group priors: correct, with belief-based partiality favouring men -------
aM, aF = sp.Rational(3, 5), sp.Rational(2, 5)     # P(A=1): men favoured
bM, bF = sp.Rational(3, 5), sp.Rational(2, 5)     # P(B=1): quality tracks ability
cc = sp.Rational(1, 50)
PM, PF = joint(aM, bM, cc), joint(aF, bF, cc)

# ---- (2) Bayesian evaluator with exogenous likelihoods -----------------------
lik = {L1: sp.Rational(3, 4), L0: sp.Rational(1, 4)}   # signal favours high quality
BM = bayes_on_B_exogenous(PM, lik[L1], lik[L0])
BF = bayes_on_B_exogenous(PF, lik[L1], lik[L0])
D_bayes = sp.nsimplify(margB1(BM) - margB1(BF))
check("(2) Bayesian with exogenous likelihoods: D != 0 (partiality transmits)",
      D_bayes != 0, f"D = {D_bayes}")
check("    ... and posteriors inherit the ordering of priors "
      "(the monotonicity BIR's Prop. 2 rests on)",
      D_bayes > 0 and (bM - bF) > 0, f"D = {float(D_bayes):+.6f}")
print(f"         D_bayes = {D_bayes} = {float(D_bayes):+.6f}")

# ---- (3) Jeffrey, quality cue read LAST --------------------------------------
rB = sp.Rational(2, 3)          # delivered credence P(B=1) = 2/3, same for both
qA = sp.Rational(7, 10)         # delivered credence P(A=1) = 7/10, same for both
JM_AB = jeffrey_on_B(jeffrey_on_A(PM, qA), rB)     # ability first, quality last
JF_AB = jeffrey_on_B(jeffrey_on_A(PF, qA), rB)
D_AB = sp.nsimplify(margB1(JM_AB) - margB1(JF_AB))
check("(3) Jeffrey, QUALITY read last: D = 0 EXACTLY -- belief partiality silenced",
      sp.simplify(D_AB) == 0, f"D = {D_AB}")

# ---- (4) Jeffrey, ability cue read LAST --------------------------------------
JM_BA = jeffrey_on_A(jeffrey_on_B(PM, rB), qA)     # quality first, ability last
JF_BA = jeffrey_on_A(jeffrey_on_B(PF, rB), qA)
D_BA = sp.nsimplify(margB1(JM_BA) - margB1(JF_BA))
check("(4) Jeffrey, ABILITY read last: D != 0 -- the silencing is route-dependent",
      sp.simplify(D_BA) != 0, f"D = {D_BA}")
print(f"         D_BA = {D_BA} = {float(D_BA):+.6f}")

# ---- (5) generality: arbitrary priors, arbitrary covariances, arbitrary cues --
aM2, aF2, bM2, bF2 = sp.symbols('aM aF bM bF', real=True)
cM2, cF2 = sp.symbols('cM cF', real=True)          # even different covariances
GM = jeffrey_on_B(jeffrey_on_A(joint(aM2, bM2, cM2), 1 - q0), 1 - r0)
GF = jeffrey_on_B(jeffrey_on_A(joint(aF2, bF2, cF2), 1 - q0), 1 - r0)
D_gen = sp.simplify(margB1(GM) - margB1(GF))
check("(5) D = 0 for ARBITRARY group priors, covariances and delivered credences "
      "(no small-c approximation, nothing shared but the cues)",
      D_gen == 0, f"D = {D_gen}")

print()
print("All checks passed." if ok else "SOME CHECKS FAILED.")
import sys
sys.exit(0 if ok else 1)
