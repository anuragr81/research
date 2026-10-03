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

Exact order two, symbolic in lambda, with its exceptional set.  assoc is a
quadratic form, so assoc(Pbar) = lambda assoc(P^J_AB) + (1-lambda)
assoc(P^J_BA) - lambda(1-lambda) assoc(P^J_AB - P^J_BA), and
assoc(Delta_seq) = kappa kappa'.  The c^2 coefficient of the aggregate gap,
computed symbolically in (alpha, beta, q0, r0, lambda), is

    [c^2] (assoc(Pbar) - assoc(P^B))
        = lambda a_AB + (1-lambda) a_BA - lambda(1-lambda) kappa kappa'
        = q0 q1 r0 r1 F / Z^2,
    a_AB = kappa q0 q1 (1-2r0)/Z,  a_BA = kappa' r0 r1 (1-2q0)/Z   (Lemma ASC),
    F = lambda (alpha-q0)(1-2r0) + (1-lambda)(beta-r0)(1-2q0)
        - lambda(1-lambda)(alpha-q0)(beta-r0).

F is irreducible over Q and changes sign on (0,1)^5, so the gap is exactly
Theta(c^2) off the hypersurface {F = 0} (codimension one); q0, q1, r0, r1 never
vanish.  {F = 0} contains {q0 = alpha, r0 = beta} for every lambda, reduces to
{q0 = alpha} u {r0 = 1/2} at lambda = 1 and to {r0 = beta} u {q0 = 1/2} at
lambda = 0, and for every fixed lambda in (0,1) F is a nonzero polynomial in
the prior (its alpha*beta coefficient is -lambda(1-lambda)).  The evaluation
at lambda = 1/3 and the rational point GENERIC is kept only as a sanity row.
"""
import itertools
import sympy as sp
from jeffrey_core import *

# ------------------------------------------------ exceptional-set helper ----
# A nonzero value at one rational point shows only that an expression is not
# identically zero.  zero_set() states WHERE it vanishes on the open cube
# (0,1)^n of its parameters: every irreducible factor (over Q) of the
# numerator is listed and its zero set on the cube is certified, and every
# factor of the denominator is checked to have no zero there.
_GRID = (sp.Rational(1, 4), sp.Rational(1, 2), sp.Rational(3, 4))


def _never(f, dom):
    """f = +-v or +-(1-v) for a parameter v: no zero on the open cube."""
    return any(sp.expand(f - s * t) == 0
               for v in dom for t in (v, 1 - v) for s in (1, -1))


def _sign_change(f, dom):
    """Rational grid points of the cube where f > 0 and where f < 0."""
    pos = neg = None
    for p in itertools.product(_GRID, repeat=len(dom)):
        val = f.subs(dict(zip(dom, p)))
        pos = p if pos is None and val > 0 else pos
        neg = p if neg is None and val < 0 else neg
    return pos, neg


def zero_set(ck, name, expr, dom, summary, factors):
    """
    Check that `expr` vanishes on (0,1)^len(dom) exactly on `summary`.
    `factors` lists the non-constant irreducible factors of the numerator as
    (f, how, meaning):
      how = 'never'    f = +-v or +-(1-v): no zero on the cube;
      how = (v, val)   f = const*(v - val): zero exactly on {v = val};
      how = 'hyper'    f is not the zero polynomial and changes sign between
                       two rational points of the cube, so on the cube it
                       vanishes on a hypersurface (codimension one).
    """
    N, D = sp.fraction(sp.cancel(sp.together(expr)))
    found = [f for f, _ in sp.factor_list(N)[1]]
    declared = [f for f, _, _ in factors]

    def same(f, g):
        return sp.expand(f - g) == 0 or sp.expand(f + g) == 0

    ck(f"{name}: numerator = const * product of the {len(declared)} listed factors; "
       f"denominator never 0 on (0,1)^{len(dom)}; zero set = {summary}",
       len(found) == len(declared)
       and all(any(same(f, g) for g in declared) for f in found)
       and all(any(same(f, g) for f in found) for g in declared)
       and all(_never(f, dom) for f, _ in sp.factor_list(D)[1]),
       f"numerator factors {found}, denominator {sp.factor(D)}")
    for f, how, meaning in factors:
        if how == 'never':
            ok, extra = _never(f, dom), ""
        elif how == 'hyper':
            pos, neg = _sign_change(f, dom)
            ok = (sp.Poly(f, *dom).is_zero is False
                  and pos is not None and neg is not None)
            extra = f" [> 0 at {pos}, < 0 at {neg}]"
        else:
            v, val = how
            k = sp.cancel(f / (v - val))
            ok, extra = k.is_number and k != 0, ""
        ck(f"{name}: factor {f} -- {meaning}{extra}", ok)


def c012(e):
    """
    (c^0, c^1, c^2) Taylor coefficients of a rational e about c = 0.  The c01
    trick of verify_ladder.py carried one order further: with e = N/D and
    N = N0 + N1 c + N2 c^2 + ..., D likewise, the series quotient gives
    e0 = N0/D0, e1 = (N1 - e0 D1)/D0, e2 = (N2 - e1 D1 - e0 D2)/D0.
    """
    N, D = sp.fraction(sp.together(e))
    Nk = [sp.diff(N, c, k).subs(c, 0) / sp.factorial(k) for k in range(3)]
    Dk = [sp.diff(D, c, k).subs(c, 0) / sp.factorial(k) for k in range(3)]
    e0 = sp.cancel(Nk[0] / Dk[0])
    e1 = sp.cancel((Nk[1] - e0 * Dk[1]) / Dk[0])
    e2 = sp.cancel((Nk[2] - e1 * Dk[1] - e0 * Dk[2]) / Dk[0])
    return e0, e1, e2


NEVER_Q = [(q0, 'never', "never 0 on (0,1)"), (1 - q0, 'never', "never 0 on (0,1)")]
NEVER_R = [(r0, 'never', "never 0 on (0,1)"), (1 - r0, 'never', "never 0 on (0,1)")]
# the factor of the aggregate c^2 coefficient that carries its zero set
F_AGG = (lam * (alpha - q0) * (1 - 2 * r0) + (1 - lam) * (beta - r0) * (1 - 2 * q0)
         - lam * (1 - lam) * (alpha - q0) * (beta - r0))


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

    # ---- exactly second order: the c^2 coefficient, symbolic in lambda ----
    Xs = sp.Matrix(2, 2, sp.symbols('X00 X01 X10 X11'))
    Ys = sp.Matrix(2, 2, sp.symbols('Y00 Y01 Y10 Y11'))
    ck.eq("assoc is a quadratic form: assoc(lam X + (1-lam) Y) = lam assoc(X) "
          "+ (1-lam) assoc(Y) - lam(1-lam) assoc(X - Y)",
          assoc(lam * Xs + (1 - lam) * Ys),
          lam * assoc(Xs) + (1 - lam) * assoc(Ys) - lam * (1 - lam) * assoc(Xs - Ys))
    ck.eq("assoc(Delta_seq) = assoc(kappa R1 - kappa' R2) = kappa kappa'",
          assoc(kappa * R1 - kappa_p * R2), kappa * kappa_p)
    d0, d1, d2 = c012(d)
    ck("c012 jets: c^0 and c^1 coefficients of the aggregate gap vanish for every lambda",
       d0 == 0 and d1 == 0, f"c^0 = {d0}, c^1 = {d1}")
    a_AB = c012(assoc(PJ_AB) - assoc(PB))[2]
    a_BA = c012(assoc(PJ_BA) - assoc(PB))[2]
    ck.eq("[c^2] (assoc(P^J_AB) - assoc(P^B)) = a_AB = kappa q0 q1 (1-2r0)/Z",
          a_AB, kappa * q0 * q1 * (1 - 2 * r0) / Z)
    ck.eq("[c^2] (assoc(P^J_BA) - assoc(P^B)) = a_BA = kappa' r0 r1 (1-2q0)/Z",
          a_BA, kappa_p * r0 * r1 * (1 - 2 * q0) / Z)
    ck.eq("[c^2] (assoc(Pbar) - assoc(P^B)) = lam a_AB + (1-lam) a_BA - lam(1-lam) kappa kappa'",
          d2, lam * a_AB + (1 - lam) * a_BA - lam * (1 - lam) * kappa * kappa_p)
    ck.eq("[c^2] (assoc(Pbar) - assoc(P^B)) = q0 q1 r0 r1 F / Z^2, F = lam (alpha-q0)(1-2r0) "
          "+ (1-lam)(beta-r0)(1-2q0) - lam(1-lam)(alpha-q0)(beta-r0)",
          d2, q0 * q1 * r0 * r1 * F_AGG / Z**2)
    zero_set(ck, "[c^2] aggregate assoc gap", d2, [alpha, beta, q0, r0, lam],
             "the hypersurface {F = 0}",
             [(F_AGG, 'hyper', "irreducible; zero on a hypersurface of (0,1)^5")]
             + NEVER_Q + NEVER_R)
    ck.eq("F = 0 on {q0 = alpha, r0 = beta} for every lambda",
          F_AGG.subs({q0: alpha, r0: beta}), 0)
    ck.eq("F|_{lam=1} = (alpha-q0)(1-2r0): route AB's exceptional set (Lemma ASC)",
          F_AGG.subs(lam, 1), (alpha - q0) * (1 - 2 * r0))
    ck.eq("F|_{lam=0} = (beta-r0)(1-2q0): route BA's exceptional set (Lemma ASC)",
          F_AGG.subs(lam, 0), (beta - r0) * (1 - 2 * q0))
    ck.eq("for each fixed lambda in (0,1), F is a nonzero polynomial in (alpha,beta,q0,r0): "
          "its alpha*beta coefficient is -lam(1-lam)",
          sp.Poly(F_AGG, alpha, beta, q0, r0).coeff_monomial(alpha * beta), -lam * (1 - lam))
    k, co = order_in_c_at_generic(d.subs(lam, sp.Rational(1, 3)))
    ck("(sanity, one point) aggregate gap has leading order 2 at lambda = 1/3 and GENERIC, "
       "with the coefficient the closed form gives there",
       k == 2 and co == at_generic(d2.subs(lam, sp.Rational(1, 3))),
       f"leading order = {k}, coefficient = {co}")

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
