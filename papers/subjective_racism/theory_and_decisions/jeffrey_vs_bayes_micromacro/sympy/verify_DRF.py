"""
Proposition DRF (marginal-probability drift), Section 5.2.2.

    Pbar(A=1) - P^B(A=1) = c (1-lambda) q0(1-q0)(r0-beta)/Z + O(c^2),

so for lambda < 1 its sign is sgn(r0 - beta); at lambda = 1 the A-drift
vanishes at first order and the B-marginal drifts with coefficient
lambda r0(1-r0)(q0-alpha)/Z, of sign sgn(q0 - alpha).  For every lambda in
[0,1], therefore, at least one marginal drifts at first order.

Every displayed line of the proof is checked, including the exact identity
P^J_BA(A=1) = q1 (a Jeffrey step pins its own marginal) and the claim of
Section 4.2 that the marginal of the attribute read FIRST is O(c^2) while the
marginal of the attribute read LAST is Theta(c).
"""
import sympy as sp
from jeffrey_core import *


def main():
    ck = Check("Proposition DRF -- marginal-probability drift")

    PJ_AB, PJ_BA, PB = posteriors()
    Pbar = mean_belief(PJ_AB, PJ_BA)

    def pA1(Q):
        return marg_A(Q)[1]

    def pB1(Q):
        return marg_B(Q)[1]

    K = q0 * (1 - q0) * (r0 - beta) / Z            # proof's K
    KB = r0 * (1 - r0) * (q0 - alpha) / Z          # lambda=1 B-marginal coefficient

    # ---- a Jeffrey step pins its own marginal, exactly in c ---------------
    ck.eq("P^J_BA(A=1) = q1 exactly in c (A read last on route BA)", pA1(PJ_BA), q1)
    ck.eq("P^J_BA(A=0) = q0 exactly in c", marg_A(PJ_BA)[0], q0)
    ck.eq("P^J_AB(B=1) = r1 exactly in c (B read last on route AB)", pB1(PJ_AB), r1)

    # ---- and the earlier step's marginal is pulled off by the covariance ---
    ck.eq("P^J_AB(A=1) = q1 - c*q0(1-q0)(r0-beta)/Z + O(c^2)",
          taylor_coeff(pA1(PJ_AB) - q1, 1), -K)
    ck.eq("P^B(A=1)    = q1 - c*q0(1-q0)(r0-beta)/Z + O(c^2)",
          taylor_coeff(pA1(PB) - q1, 1), -K)
    ck.eq("P^J_AB(A=1) and P^B(A=1) have the same value at c=0",
          taylor_coeff(pA1(PJ_AB) - pA1(PB), 0), 0)

    # ---- the two route-level gaps -----------------------------------------
    ck.eq("P^J_AB(A=1) - P^B(A=1) = O(c^2)  [attribute read FIRST is protected]",
          taylor_coeff(pA1(PJ_AB) - pA1(PB), 1), 0)
    ck.eq("P^J_BA(A=1) - P^B(A=1) = cK + O(c^2)  [attribute read LAST drifts]",
          taylor_coeff(pA1(PJ_BA) - pA1(PB), 1), K)
    ck.eq("K = -kappa' (consistency with Proposition DIV's mirror)", K, -kappa_p)

    # symmetric statement on B
    ck.eq("P^J_BA(B=1) - P^B(B=1) = O(c^2)  [B read first on route BA]",
          taylor_coeff(pB1(PJ_BA) - pB1(PB), 1), 0)
    ck.eq("P^J_AB(B=1) - P^B(B=1) = c*r0(1-r0)(q0-alpha)/Z + O(c^2)",
          taylor_coeff(pB1(PJ_AB) - pB1(PB), 1), KB)
    ck.eq("the B-coefficient equals -kappa", KB, -kappa)

    # ---- the aggregate ----------------------------------------------------
    d = pA1(Pbar) - pA1(PB)
    ck.eq("Pbar(A=1) - P^B(A=1) has no constant term", taylor_coeff(d, 0), 0)
    ck.eq("Pbar(A=1) - P^B(A=1) = c (1-lambda) q0(1-q0)(r0-beta)/Z + O(c^2)",
          taylor_coeff(d, 1), (1 - lam) * K)

    ck.eq("at lambda = 1 the A-marginal drift vanishes at first order",
          taylor_coeff(d, 1).subs(lam, 1), 0)
    dB = pB1(Pbar) - pB1(PB)
    ck.eq("at lambda = 1 the B-marginal drifts with coefficient lambda r0(1-r0)(q0-alpha)/Z",
          taylor_coeff(dB, 1), lam * KB)

    # ---- signs ------------------------------------------------------------
    # On the open cube q0(1-q0) > 0 and Z > 0, so sgn K = sgn(r0 - beta).
    pos = [(alpha, sp.Rational(1, 3)), (beta, sp.Rational(1, 4)), (q0, sp.Rational(2, 5))]
    ck("sgn[Pbar(A=1)-P^B(A=1)] = sgn(r0 - beta) for lambda < 1: positive when r0 > beta",
       sp.sign(sp.cancel(((1 - lam) * K).subs(pos + [(r0, sp.Rational(3, 4)),
                                                     (lam, sp.Rational(1, 2))]))) == 1)
    ck("...and negative when r0 < beta",
       sp.sign(sp.cancel(((1 - lam) * K).subs(pos + [(r0, sp.Rational(1, 8)),
                                                     (lam, sp.Rational(1, 2))]))) == -1)
    ck.eq("the drift vanishes at first order when r0 = beta (impression matches prior)",
          taylor_coeff(d, 1).subs(r0, beta), 0)

    # ---- "for every lambda, at least one marginal drifts at first order" ---
    # The two first-order coefficients are (1-lambda)K and lambda*KB; they cannot
    # both vanish for generic (alpha,beta,q0,r0) since K, KB != 0 there.
    both_zero = sp.solve([sp.Eq((1 - lam) * K, 0), sp.Eq(lam * KB, 0)], [lam], dict=True)
    ck("for generic priors no lambda kills BOTH marginal drifts",
       all(sp.cancel(sp.together(K).subs(GENERIC)) != 0 for _ in [0])
       and sp.cancel(sp.together(KB).subs(GENERIC)) != 0
       and not any(sol.get(lam) is not None and
                   sp.cancel(((1 - lam) * K).subs(GENERIC).subs(sol)) == 0 and
                   sp.cancel((lam * KB).subs(GENERIC).subs(sol)) == 0
                   for sol in (both_zero or [])),
       f"solutions: {both_zero}")

    # genuinely Theta(c)
    k, co = order_in_c_at_generic(d.subs(lam, sp.Rational(1, 3)))
    ck("Pbar(A=1) - P^B(A=1) is exactly Theta(c) generically (lambda=1/3)",
       k == 1, f"leading order = {k}, coefficient = {co}")

    # ---- contrast with the association (Prop. DEC): different orders --------
    ka, _ = order_in_c_at_generic((assoc(Pbar) - assoc(PB)).subs(lam, sp.Rational(1, 3)))
    ck("the marginal drifts at order 1 while the association drifts at order 2 "
       "-- the decoupling the paper turns on", k == 1 and ka == 2,
       f"marginal order {k}, association order {ka}")

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
