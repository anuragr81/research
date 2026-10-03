"""
Lemma SCR (first-order score gap), Section 5.1.2.

    s(P^J_sigma) - s(P^B) = c * delta_sigma + O(c^2),
    delta_sigma := <v, d/dc (P^J_sigma - P^B)|_{c=0}>,

with, generically, delta_AB != delta_BA, and delta_sigma = 0 whenever v is
constant (both posteriors being normalised, the score telescopes).

Also checked: the definition of delta_sigma given in Section 4.1 agrees with
the one used in Section 5.1.2, and the score gap is Theta(c) exactly when v
lies outside span{J, grad assoc} -- the "unprotected weight" row of Table 1.

Exceptional sets (symbolic in alpha, beta, q0, r0 and in v; zero sets on the
open cube (0,1)^4 of the prior and cue parameters):

  * delta_AB - delta_BA = <v, Delta_seq>, Delta_seq = kappa R1 - kappa' R2.
    For the weight v = e_00 it equals Delta_seq,00 = q0 r0 L / Z with
    L = (alpha-beta) + r0(1-alpha) - q0(1-beta) irreducible over Q: zero
    exactly on the hypersurface {L = 0}.  As a functional of v it vanishes
    identically exactly on {q0 = alpha, r0 = beta} (codimension two): the
    indicator of {B=0} gives kappa (zero iff q0 = alpha), the indicator of
    {A=0} gives -kappa' (zero iff r0 = beta).
  * Unprotected weight v = e_00 on route AB: the c^0 coefficient of the score
    gap vanishes identically (every v) and the c^1 coefficient is
    q0 kappa = q0 r0 (alpha-q0)(1-r0)/Z, zero exactly on {q0 = alpha}; for a
    general v it is kappa <v, R1>, zero exactly on {q0 = alpha} u
    {<v, R1> = 0}.  The evaluations at the rational point GENERIC are kept
    only as sanity rows.
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


P4 = [alpha, beta, q0, r0]
NEVER_Q = [(q0, 'never', "never 0 on (0,1)"), (1 - q0, 'never', "never 0 on (0,1)")]
NEVER_R = [(r0, 'never', "never 0 on (0,1)"), (1 - r0, 'never', "never 0 on (0,1)")]
L_SEQ = (alpha - beta) + r0 * (1 - alpha) - q0 * (1 - beta)


def main():
    ck = Check("Lemma SCR -- first-order score gap")

    v00, v01, v10, v11 = sp.symbols('v00 v01 v10 v11', real=True)
    v = sp.Matrix([[v00, v01], [v10, v11]])

    PJ_AB, PJ_BA, PB = posteriors()

    def score(Q):
        return frob(v, Q)

    D_AB, D_BA = kappa * R1, kappa_p * R2
    delta_AB = frob(v, D_AB)
    delta_BA = frob(v, D_BA)

    # ---- the expansion ----------------------------------------------------
    ck.eq("score gap on route AB has no constant term",
          taylor_coeff(score(PJ_AB) - score(PB), 0), 0)
    ck.eq("d/dc [s(P^J_AB) - s(P^B)]|_0 = delta_AB = <v, kappa R1>",
          taylor_coeff(score(PJ_AB) - score(PB), 1), delta_AB)
    ck.eq("score gap on route BA has no constant term",
          taylor_coeff(score(PJ_BA) - score(PB), 0), 0)
    ck.eq("d/dc [s(P^J_BA) - s(P^B)]|_0 = delta_BA = <v, kappa' R2>",
          taylor_coeff(score(PJ_BA) - score(PB), 1), delta_BA)

    # ---- constant v kills the score gap -----------------------------------
    const = {v00: 1, v01: 1, v10: 1, v11: 1}
    ck.eq("delta_AB = 0 when v is constant", delta_AB.subs(const), 0)
    ck.eq("delta_BA = 0 when v is constant", delta_BA.subs(const), 0)
    k_ = sp.Symbol('k_')
    ck.eq("delta_AB = 0 for any constant vector v = k*J",
          delta_AB.subs({v00: k_, v01: k_, v10: k_, v11: k_}), 0)

    # ---- where delta_AB = delta_BA: the exceptional sets --------------------
    e00_sub = {v00: 1, v01: 0, v10: 0, v11: 0}
    gap_e00 = (delta_AB - delta_BA).subs(e00_sub)
    ck.eq("v = e_00: delta_AB - delta_BA = q0 r0 [(alpha-beta) + r0(1-alpha) - q0(1-beta)]/Z",
          gap_e00, q0 * r0 * L_SEQ / Z)
    zero_set(ck, "v = e_00: delta_AB - delta_BA", gap_e00, P4,
             "the hypersurface {(alpha-beta) + r0(1-alpha) - q0(1-beta) = 0}",
             [(L_SEQ, 'hyper', "irreducible; zero on a hypersurface")]
             + [NEVER_Q[0], NEVER_R[0]])
    colB0 = {v00: 1, v01: 0, v10: 1, v11: 0}      # indicator of {B = 0}
    rowA0 = {v00: 1, v01: 1, v10: 0, v11: 0}      # indicator of {A = 0}
    ck.eq("v = 1{B=0}: delta_AB - delta_BA = kappa",
          (delta_AB - delta_BA).subs(colB0), kappa)
    ck.eq("v = 1{A=0}: delta_AB - delta_BA = -kappa'",
          (delta_AB - delta_BA).subs(rowA0), -kappa_p)
    zero_set(ck, "kappa", kappa, P4, "{q0 = alpha}",
             [(alpha - q0, (q0, alpha), "zero iff q0 = alpha")] + NEVER_R)
    zero_set(ck, "kappa'", kappa_p, P4, "{r0 = beta}",
             [(beta - r0, (r0, beta), "zero iff r0 = beta")] + NEVER_Q)
    ck.eq("delta_AB = delta_BA for every v on {q0 = alpha, r0 = beta}; with the two "
          "indicator rows: identically in v exactly there (codimension two)",
          (delta_AB - delta_BA).subs({q0: alpha, r0: beta}), 0)
    ck.ne("(sanity, one point) v = e_00: delta_AB - delta_BA != 0 at GENERIC", gap_e00)

    # ---- protected vs unprotected weights (Table 1, "score gap" row) ------
    # v in span{J, grad assoc} -> the score gap is O(c^2) for BOTH routes
    x_, y_ = sp.symbols('x_ y_')
    v_prot = x_ * J + y_ * grad_assoc_indep
    ck.eq("v in span{J, grad assoc}: delta_AB = 0",
          frob(v_prot, D_AB), 0)
    ck.eq("v in span{J, grad assoc}: delta_BA = 0",
          frob(v_prot, D_BA), 0)
    ck.eq("v in span{J, grad assoc}: aggregate first-order score gap = 0",
          frob(v_prot, lam * D_AB + (1 - lam) * D_BA), 0)

    # an explicitly unprotected v: the score gap is Theta(c) off {q0 = alpha}.
    # Its c^0 coefficient vanishes and its c^1 coefficient is delta_AB, for
    # every v (rows above); here delta_AB in closed form and its zero set.
    ck.eq("route AB, every v: delta_AB = kappa <v, R1> = kappa [q0(v00-v01) + q1(v10-v11)]",
          delta_AB, kappa * (q0 * (v00 - v01) + q1 * (v10 - v11)))
    v_unprot = {v00: 1, v01: 0, v10: 0, v11: 0}
    lead = delta_AB.subs(v_unprot)
    ck.eq("unprotected weight v = e_00: c^1 coefficient of the score gap = q0 kappa "
          "= q0 r0 (alpha-q0)(1-r0)/Z", lead, q0 * r0 * (alpha - q0) * (1 - r0) / Z)
    zero_set(ck, "v = e_00: c^1 coefficient of s(P^J_AB) - s(P^B)", lead, P4,
             "{q0 = alpha}",
             [(alpha - q0, (q0, alpha), "zero iff q0 = alpha: the A-cue delivers the prior marginal")]
             + [NEVER_Q[0]] + NEVER_R)
    k, co = order_in_c_at_generic((score(PJ_AB) - score(PB)).subs(v_unprot))
    ck("(sanity, one point) unprotected weight v = e_00: score gap has leading order 1 "
       "at GENERIC, with the coefficient the closed form gives there",
       k == 1 and co == at_generic(lead), f"leading order = {k}, coefficient = {co}")

    # e_00 is indeed outside span{J, grad assoc}
    e00 = sp.Matrix([[1, 0], [0, 0]])
    sol = sp.solve([sp.Eq((x_ * J + y_ * grad_assoc_indep - e00)[i, j], 0)
                    for i in range(2) for j in range(2)], [x_, y_], dict=True)
    ck("e_00 is outside span{J, grad assoc} (no solution)", sol == [], f"sol={sol}")

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
