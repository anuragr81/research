"""
Lemma ASC (individual-level association immunity), Section 4.2.

    assoc(P^J_sigma) - assoc(P^B) = O(c^2)  identically in (alpha,beta,q0,r0),
    for each reading sequence sigma in {AB, BA}.

The manuscript's proof runs through the chain rule plus the two vanishing
inner products <grad assoc(q(x)r), R1> = <grad assoc(q(x)r), R2> = 0.  Both the
conclusion and the two inner products are checked here, and the gradient
formula printed in the proof is checked against the actual gradient.

Exact order two, with its exceptional set.  The c^2 coefficient of the gap is
computed symbolically in (alpha, beta, q0, r0) (N/D jets, c012 below):

    [c^2] (assoc(P^J_AB) - assoc(P^B)) = kappa  q0(1-q0) (1-2 r0) / Z
                                       = q0 q1 r0 r1 (alpha-q0)(1-2 r0) / Z^2,
    [c^2] (assoc(P^J_BA) - assoc(P^B)) = kappa' r0(1-r0) (1-2 q0) / Z
                                       = q0 q1 r0 r1 (beta-r0)(1-2 q0) / Z^2.

On (0,1)^4 the route-AB coefficient vanishes exactly on {q0 = alpha} u
{r0 = 1/2} (the A-cue delivers the prior marginal, or the B-cue is the even
credence r = (1/2,1/2)); the route-BA coefficient exactly on {r0 = beta} u
{q0 = 1/2}.  Off those sets the gap is exactly Theta(c^2).  The evaluation at
the rational point GENERIC is kept only as a sanity row.
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


P4 = [alpha, beta, q0, r0]
NEVER_Q = [(q0, 'never', "never 0 on (0,1)"), (1 - q0, 'never', "never 0 on (0,1)")]
NEVER_R = [(r0, 'never', "never 0 on (0,1)"), (1 - r0, 'never', "never 0 on (0,1)")]


def main():
    ck = Check("Lemma ASC -- individual-level association immunity")

    PJ_AB, PJ_BA, PB = posteriors()

    # ---- the gradient of assoc at the independent posterior q (x) r --------
    X = sp.Matrix(2, 2, sp.symbols('x00 x01 x10 x11'))
    grad = sp.Matrix(2, 2, lambda i, j: sp.diff(assoc(X), X[i, j])).subs(
        list(zip(list(X), list(indep))))
    ck.mat_eq("grad assoc|_{q(x)r} = [[q1 r1, -q1 r0], [-q0 r1, q0 r0]]",
              grad, grad_assoc_indep)

    # ---- the two inner products vanish termwise ---------------------------
    ck.eq("<grad assoc(q(x)r), R1> = 0", frob(grad_assoc_indep, R1), 0)
    ck.eq("<grad assoc(q(x)r), R2> = 0", frob(grad_assoc_indep, R2), 0)

    # ---- hence the first-order term of the association gap vanishes -------
    gaps = {"AB": assoc(PJ_AB) - assoc(PB), "BA": assoc(PJ_BA) - assoc(PB)}
    first = {}
    for nm, d in gaps.items():
        first[nm] = taylor_coeff(d, 1)
        ck.eq(f"assoc(P^J_{nm}) - assoc(P^B) has no constant term", taylor_coeff(d, 0), 0)
        ck.eq(f"assoc(P^J_{nm}) - assoc(P^B) has no O(c) term  ->  = O(c^2)",
              first[nm], 0)

    # ---- and it is exactly second order, off a stated exceptional set ------
    # c^2 coefficient, symbolic in (alpha, beta, q0, r0).  The N/D jets also
    # return c^0 and c^1, which must agree with the taylor_coeff rows above.
    second = {
        "AB": (kappa * q0 * (1 - q0) * (1 - 2 * r0) / Z,
               "q0 q1 r0 r1 (alpha-q0)(1-2r0)/Z^2 = kappa q0(1-q0)(1-2r0)/Z",
               "{q0 = alpha} u {r0 = 1/2}",
               [(alpha - q0, (q0, alpha), "zero iff q0 = alpha: the A-cue delivers the prior marginal"),
                (1 - 2 * r0, (r0, sp.Rational(1, 2)), "zero iff r0 = 1/2: the B-cue is the even credence")]
               + NEVER_Q + NEVER_R),
        "BA": (kappa_p * r0 * (1 - r0) * (1 - 2 * q0) / Z,
               "q0 q1 r0 r1 (beta-r0)(1-2q0)/Z^2 = kappa' r0(1-r0)(1-2q0)/Z",
               "{r0 = beta} u {q0 = 1/2}",
               [(beta - r0, (r0, beta), "zero iff r0 = beta: the B-cue delivers the prior marginal"),
                (1 - 2 * q0, (q0, sp.Rational(1, 2)), "zero iff q0 = 1/2: the A-cue is the even credence")]
               + NEVER_Q + NEVER_R),
    }
    e2s = {}
    for nm, d in gaps.items():
        e0, e1, e2 = c012(d)
        e2s[nm] = e2
        ck(f"c012 jets reproduce the c^0 and c^1 coefficients (route {nm})",
           e0 == 0 and simp(e1 - first[nm]) == 0, f"e0 = {e0}, e1 = {e1}")
        closed, text, summary, factors = second[nm]
        ck.eq(f"[c^2] (assoc(P^J_{nm}) - assoc(P^B)) = {text}", e2, closed)
        zero_set(ck, f"[c^2] assoc gap, route {nm}", e2, P4, summary, factors)
    k, co = order_in_c_at_generic(gaps["AB"])
    ck("(sanity, one point) assoc(P^J_AB) - assoc(P^B) has leading order 2 at GENERIC, "
       "with the coefficient the closed form gives there",
       k == 2 and co == at_generic(e2s["AB"]), f"leading order = {k}, coefficient = {co}")

    # ---- the direct chain-rule identity used in the proof -----------------
    for nm, D in [("AB", kappa * R1), ("BA", kappa_p * R2)]:
        ck.eq(f"chain rule on route {nm}: d/dc[assoc(P^J)-assoc(P^B)]|_0 "
              f"= <grad assoc, D_{nm}>",
              first[nm], frob(grad_assoc_indep, D))

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
