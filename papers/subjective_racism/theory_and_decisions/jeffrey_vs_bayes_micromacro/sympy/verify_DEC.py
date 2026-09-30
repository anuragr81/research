"""
Proposition DEC (belief-level decoupling), Section 5.2.1.

    assoc(Pbar) - assoc(P^B) = O(c^2),

together with every intermediate identity of its proof:

  * for any separable (rank-one) reweighting Q'_ij = g_i h_j Q_ij / Z,
        assoc(Q') = (g_0 g_1 h_0 h_1 / Z^2) * assoc(Q),
    so a separable reweighting can only RESCALE an association, never shift it;
  * every route maps assoc(P) = c to K c + O(c^2) with the SAME leading factor
        K = q_0 q_1 r_0 r_1 / (alpha(1-alpha) beta(1-beta));
  * hence the O(c) coefficients cancel for every lambda.

Also checked is the exact remark following the proof: the factor
g_0 g_1 h_0 h_1 / Z^2 cancels in the ODDS RATIO, so P^J_AB, P^J_BA and P^B all
carry the prior's odds ratio identically in c -- the departure is exactly zero
rather than O(c^2).
"""
import sympy as sp
from jeffrey_core import *


def main():
    ck = Check("Proposition DEC -- belief-level decoupling")

    # ---- the separable-reweighting multiplier identity ---------------------
    g0, g1, h0, h1 = sp.symbols('g0 g1 h0 h1', positive=True)
    Qs = sp.Matrix(2, 2, sp.symbols('Q00 Q01 Q10 Q11', positive=True))
    gg, hh = [g0, g1], [h0, h1]
    W = sp.Matrix(2, 2, lambda i, j: gg[i] * hh[j] * Qs[i, j])
    Zs = sum(W)
    Qp = W / Zs
    ck.eq("separable reweighting: assoc(Q') = (g0 g1 h0 h1 / Z^2) assoc(Q)",
          sp.cancel(sp.together(assoc(Qp))),
          sp.cancel(g0 * g1 * h0 * h1 * assoc(Qs) / Zs**2))
    ck.eq("separable reweighting leaves the ODDS RATIO exactly unchanged",
          sp.cancel(odds_ratio(Qp) - odds_ratio(Qs)), 0)
    ck("separable reweighting cannot shift assoc additively: assoc(Q')=0 iff assoc(Q)=0",
       sp.cancel(assoc(Qp).subs(Qs[0, 0], Qs[0, 1] * Qs[1, 0] / Qs[1, 1])) == 0)

    # ---- the Bayes benchmark IS such a reweighting -------------------------
    P = prior()
    PB_direct = sp.Matrix(2, 2, lambda i, j: P[i, j] * (q[i] / [alpha, 1 - alpha][i])
                          * (r[j] / [beta, 1 - beta][j]))
    PB_direct = PB_direct / sp.cancel(sum(PB_direct))
    ck.mat_eq("P^B is the separable reweighting with g_i = q_i/alpha_i, h_j = r_j/beta_j",
              sp.Matrix(2, 2, lambda i, j: sp.cancel(PB_direct[i, j])), posteriors()[2])

    PJ_AB, PJ_BA, PB = posteriors()

    # ---- the common leading factor K --------------------------------------
    K = q0 * q1 * r0 * r1 / (alpha * (1 - alpha) * beta * (1 - beta))
    for nm, Q in [("P^J_AB", PJ_AB), ("P^J_BA", PJ_BA), ("P^B", PB)]:
        ck.eq(f"assoc({nm}) has no constant term (vanishes at c=0)",
              taylor_coeff(assoc(Q), 0), 0)
        ck.eq(f"assoc({nm}) = K c + O(c^2) with K = q0 q1 r0 r1 / (a(1-a)b(1-b))",
              taylor_coeff(assoc(Q), 1), K)

    # ---- the conclusion, for arbitrary lambda ------------------------------
    Pbar = mean_belief(PJ_AB, PJ_BA)
    d = assoc(Pbar) - assoc(PB)
    ck.eq("assoc(Pbar_lambda) - assoc(P^B): no constant term", taylor_coeff(d, 0), 0)
    ck.eq("assoc(Pbar_lambda) - assoc(P^B): no O(c) term, for EVERY lambda",
          taylor_coeff(d, 1), 0)
    ck.eq("assoc(Pbar_lambda) = K c + O(c^2)", taylor_coeff(assoc(Pbar), 1), K)

    # genuinely second order, not higher
    k, co = order_in_c_at_generic(d.subs(lam, sp.Rational(1, 3)))
    ck("assoc(Pbar) - assoc(P^B) is exactly Theta(c^2) generically (lambda=1/3)",
       k == 2, f"leading order = {k}, coefficient = {co}")

    # ---- the exact odds-ratio remark --------------------------------------
    OR_prior = odds_ratio(P)
    for nm, Q in [("P^J_AB", PJ_AB), ("P^J_BA", PJ_BA), ("P^B", PB)]:
        ck.eq(f"odds ratio of {nm} equals the prior odds ratio EXACTLY in c "
              "(not merely to O(c^2))", odds_ratio(Q), OR_prior)
    ck.eq("odds-ratio departure from the benchmark is exactly zero: OR(P^J_AB) = OR(P^B)",
          odds_ratio(PJ_AB), odds_ratio(PB))

    # ---- the other odds-ratio-based dependence measures --------------------
    # Yule's Q is a function of the odds ratio alone, Q = (OR-1)/(OR+1), so the
    # exact odds-ratio invariance carries over to it verbatim.
    def yule(Q):
        return assoc(Q) / (Q[0, 0] * Q[1, 1] + Q[0, 1] * Q[1, 0])

    OR = sp.Symbol('OR', positive=True)
    ck.eq("Yule's Q is a function of the odds ratio alone: Q = (OR-1)/(OR+1)",
          sp.cancel(yule(Qs) - (odds_ratio(Qs) - 1) / (odds_ratio(Qs) + 1)), 0)
    for nm, Q in [("P^J_AB", PJ_AB), ("P^J_BA", PJ_BA)]:
        ck.eq(f"Yule's Q: {nm} and P^B agree exactly in c",
              yule(Q), yule(PB))
    # log odds ratio: exact equality follows from OR equality
    ck.eq("log odds ratio: exp of the difference is 1, so the difference is exactly 0",
          sp.cancel(odds_ratio(PJ_AB) / odds_ratio(PB)), 1)

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
