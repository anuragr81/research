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

Every "generically" is checked symbolically, with its exceptional set stated
(on the open cube (0,1)^4 of (alpha, beta, q0, r0), lambda in [0,1]; the
factors q0, 1-q0, r0, 1-r0 and Z have no zero there):

  * A-drift, c^1 coefficient (1-lambda) K, K = q0(1-q0)(r0-beta)/Z:
    zero exactly on {lambda = 1} U {r0 = beta}.
  * B-drift, c^1 coefficient lambda K_B, K_B = r0(1-r0)(q0-alpha)/Z:
    zero exactly on {lambda = 0} U {q0 = alpha}.
  * Some lambda in [0,1] kills both first-order drifts exactly on
    {r0 = beta} U {q0 = alpha}: the identity K K_B = K_B (1-lambda)K + K lambda K_B
    gives "only if", and lambda = 0 resp. lambda = 1 gives "if".
  * Association drift assoc(Pbar) - assoc(P^B): c^0 and c^1 coefficients are 0
    identically in (alpha, beta, q0, r0, lambda); the c^2 coefficient is

        q0 q1 r0 r1 P2 / Z^2,
        P2 = lambda (q0-alpha)(2r0-1) + (1-lambda)(r0-beta)(2q0-1)
             - lambda(1-lambda)(q0-alpha)(r0-beta),

    so it is exactly Theta(c^2) off the hypersurface {P2 = 0}.  P2 is
    irreducible over Q, meets the cube (e.g. at alpha = 2/3, beta = 1/4,
    q0 = 1/3, r0 = 2/5, lambda = 1/3), and no lambda makes it vanish
    identically (its coefficients generate the unit ideal of Q[lambda]).

Association coefficients are computed by exact series arithmetic on the cells
of the composed routes (each cell N/D, N and D polynomials in c), which is
far cheaper than differentiating the assembled association.
"""
import sympy as sp
from jeffrey_core import *


# --------------------------------------------- exceptional-set machinery ----
# A claim "nonzero generically" or "leading order k in c" is checked
# symbolically: the coefficient is computed in all its parameters, asserted
# equal to a factored closed form, and its zero set on the open cube of
# (alpha, beta, q0, r0) is read off its irreducible factors over Q.  A factor
# x or 1-x of a cube coordinate has no zero there; every other factor is named.
CUBE = (alpha, beta, q0, r0)


class Ser:
    """A power series in c truncated after a fixed order, with exact rational
    coefficients; + - * / are the truncated series operations."""

    def __init__(self, a):
        self.a = [sp.cancel(sp.together(x)) for x in a]

    def _lift(self, o):
        return o if isinstance(o, Ser) else Ser([o] + [0] * (len(self.a) - 1))

    def __add__(self, o):
        return Ser([x + y for x, y in zip(self.a, self._lift(o).a)])

    __radd__ = __add__

    def __neg__(self):
        return Ser([-x for x in self.a])

    def __sub__(self, o):
        return self + (-self._lift(o))

    def __rsub__(self, o):
        return self._lift(o) + (-self)

    def __mul__(self, o):
        b = self._lift(o).a
        return Ser([sum(self.a[i] * b[k - i] for i in range(k + 1))
                    for k in range(len(self.a))])

    __rmul__ = __mul__

    def __truediv__(self, o):
        b, out = self._lift(o).a, []
        for k in range(len(self.a)):
            out.append(sp.cancel((self.a[k] - sum(b[j] * out[k - j] for j in range(1, k + 1)))
                                 / b[0]))
        return Ser(out)

    def __rtruediv__(self, o):
        return self._lift(o) / self


def series_cells(Q, n):
    """The cells of the table Q expanded about c = 0 through c^n, exactly: each
    cell is N/D with N, D polynomials in c, and the series of D is divided out."""
    out = {}
    for i in range(2):
        for j in range(2):
            N, D = (sp.Poly(x, c) for x in sp.fraction(sp.together(Q[i, j])))
            out[i, j] = (Ser([N.coeff_monomial(c**k) for k in range(n + 1)])
                         / Ser([D.coeff_monomial(c**k) for k in range(n + 1)]))
    return out


def mix(X, Y, w):
    """The table w X + (1-w) Y, cell by cell."""
    return {k: w * X[k] + (1 - w) * Y[k] for k in X}


def _cube_unit(f):
    return any(sp.expand(f - s) == 0 for x in CUBE for s in (x, -x, 1 - x, x - 1))


def _sign_normal(f):
    f = sp.expand(f)
    return f if sp.Poly(f, *sorted(f.free_symbols, key=str)).LC() > 0 else -f


def vanishing_factors(p):
    """Irreducible factors over Q of the polynomial p, up to sign, other than x, 1-x."""
    return {_sign_normal(f) for f, _ in sp.factor_list(sp.expand(p))[1] if not _cube_unit(f)}


def zero_set(ck, name, expr, num, den=()):
    """The numerator of expr has exactly the irreducible factors `num` besides
    cube units, and its denominator exactly `den`: so on the open cube expr
    vanishes exactly where some factor in `num` does (and is undefined where
    some factor in `den` does)."""
    n, d = sp.fraction(sp.cancel(sp.together(expr)))
    fn, fd = vanishing_factors(n), vanishing_factors(d)
    return ck(name, fn == {_sign_normal(f) for f in num} and fd == {_sign_normal(f) for f in den},
              f"numerator factors {fn}, denominator factors {fd}")


def hypersurface(ck, name, p, var, point):
    """p is irreducible over Q and not the zero polynomial, and its zero set
    meets the open cube at a point where dp/dvar != 0 -- so near that point it
    is a smooth hypersurface (codimension one) of the cube.  p must be linear
    in var; `point` gives the other coordinates, all in (0,1), and the root in
    var must lie in (0,1)."""
    p = sp.expand(p)
    _, fl = sp.factor_list(p)
    a = sp.diff(p, var)
    a0 = a.subs(point)
    root = sp.cancel(-p.subs(var, 0).subs(point) / a0) if a0 != 0 else None
    ok = (len(fl) == 1 and fl[0][1] == 1
          and not sp.Poly(p, *sorted(p.free_symbols, key=str)).is_zero
          and var not in a.free_symbols and root is not None and 0 < root < 1
          and all(0 < v < 1 for v in point.values()))
    where = ", ".join(f"{k}={v}" for k, v in list(point.items()) + [(var, root)])
    return ck(f"{name} [meets the cube at {where}]", ok, f"factors {fl}, root {root}")


def no_weight_kills(p, weights):
    """No value of the weights, real or complex, makes p vanish identically in
    (alpha, beta, q0, r0): its coefficients there generate the unit ideal."""
    coeffs = sp.Poly(sp.expand(p), *CUBE).coeffs()
    return list(sp.groebner(coeffs, *weights).exprs) == [1]


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
    dB = pB1(Pbar) - pB1(PB)
    a1, b1 = taylor_coeff(d, 1), taylor_coeff(dB, 1)
    ck.eq("Pbar(A=1) - P^B(A=1) has no constant term", taylor_coeff(d, 0), 0)
    ck.eq("Pbar(A=1) - P^B(A=1) = c (1-lambda) q0(1-q0)(r0-beta)/Z + O(c^2)",
          a1, (1 - lam) * K)
    ck.eq("at lambda = 1 the A-marginal drift vanishes at first order", a1.subs(lam, 1), 0)
    ck.eq("Pbar(B=1) - P^B(B=1) has no constant term", taylor_coeff(dB, 0), 0)
    ck.eq("at lambda = 1 the B-marginal drifts with coefficient lambda r0(1-r0)(q0-alpha)/Z",
          b1, lam * KB)

    # ---- exact zero sets of the first-order coefficients ---------------------
    zero_set(ck, "K = q0(1-q0)(r0-beta)/Z vanishes on the open cube exactly on {r0 = beta}, "
             "where the B-cue delivers the prior B-marginal", K, [r0 - beta])
    zero_set(ck, "K_B = r0(1-r0)(q0-alpha)/Z vanishes exactly on {q0 = alpha}, where the "
             "A-cue delivers the prior A-marginal", KB, [q0 - alpha])
    ok_A = zero_set(ck, "the A-drift (1-lambda)K is exactly Theta(c) off {lambda = 1} U {r0 = beta}: "
                    "(1-lambda) vanishes only for the pure AB population, which reads A first",
                    a1, [lam - 1, r0 - beta])
    zero_set(ck, "the B-drift lambda K_B is exactly Theta(c) off {lambda = 0} U {q0 = alpha}: "
             "lambda vanishes only for the pure BA population", b1, [lam, q0 - alpha])

    # ---- signs ------------------------------------------------------------
    w = (1 - lam) * q0 * (1 - q0) / Z
    ck.eq("(1-lambda)K = (r0 - beta) w with w = (1-lambda) q0(1-q0)/Z", a1, (r0 - beta) * w)
    zero_set(ck, "w, a product of factors positive on the open cube for lambda < 1, has no zero "
             "there but lambda = 1, so sgn[Pbar(A=1)-P^B(A=1)] = sgn(r0 - beta) at first order",
             w, [lam - 1])
    pos = [(alpha, sp.Rational(1, 3)), (beta, sp.Rational(1, 4)), (q0, sp.Rational(2, 5))]
    ck("sanity: positive at r0 = 3/4 > beta (lambda = 1/2)",
       sp.sign(sp.cancel(((1 - lam) * K).subs(pos + [(r0, sp.Rational(3, 4)),
                                                     (lam, sp.Rational(1, 2))]))) == 1)
    ck("sanity: negative at r0 = 1/8 < beta (lambda = 1/2)",
       sp.sign(sp.cancel(((1 - lam) * K).subs(pos + [(r0, sp.Rational(1, 8)),
                                                     (lam, sp.Rational(1, 2))]))) == -1)

    # ---- "for every lambda, at least one marginal drifts at first order" ---
    # If both first-order coefficients vanish, so does K K_B = K_B a1 + K b1.
    ck.eq("K K_B = K_B (1-lambda)K + K lambda K_B, identically in lambda: a lambda that kills "
          "both first-order drifts forces K K_B = 0", K * KB, KB * a1 + K * b1)
    ok_KK = zero_set(ck, "K K_B vanishes on the open cube exactly on {r0 = beta} U {q0 = alpha}",
                     K * KB, [r0 - beta, q0 - alpha])
    ck("...and that exceptional set is exact: on {r0 = beta} lambda = 0 kills both drifts, "
       "on {q0 = alpha} lambda = 1 does",
       all(simp(e.subs(s)) == 0 for e in (a1, b1)
           for s in ({r0: beta, lam: 0}, {q0: alpha, lam: 1})))
    ck("so for every lambda in [0,1] at least one marginal drifts at first order, for every "
       "prior and cue off {r0 = beta} U {q0 = alpha}", ok_KK)

    # ---- contrast with the association (Prop. DEC): different orders --------
    AB, BA, B = (series_cells(Q, 2) for Q in (PJ_AB, PJ_BA, PB))
    sa = assoc(mix(AB, BA, lam)) - assoc(B)
    P2 = (lam * (q0 - alpha) * (2 * r0 - 1) + (1 - lam) * (r0 - beta) * (2 * q0 - 1)
          - lam * (1 - lam) * (q0 - alpha) * (r0 - beta))
    ck.eq("assoc(Pbar) - assoc(P^B): c^0 coefficient is 0 identically in (alpha,beta,q0,r0,lambda)",
          sa.a[0], 0)
    ck.eq("assoc(Pbar) - assoc(P^B): c^1 coefficient is 0 identically", sa.a[1], 0)
    ck.eq("assoc(Pbar) - assoc(P^B): c^2 coefficient = q0 q1 r0 r1 P2 / Z^2, P2 = "
          "lambda(q0-alpha)(2r0-1) + (1-lambda)(r0-beta)(2q0-1) - lambda(1-lambda)(q0-alpha)(r0-beta)",
          sa.a[2], q0 * q1 * r0 * r1 * P2 / Z**2)
    ok_P2 = zero_set(ck, "...so the association drift is exactly Theta(c^2) off the hypersurface "
                     "{P2 = 0}, its only vanishing factor", sa.a[2], [P2])
    hypersurface(ck, "P2 is irreducible over Q and not the zero polynomial", P2, alpha,
                 {beta: sp.Rational(1, 4), q0: sp.Rational(1, 3), r0: sp.Rational(2, 5),
                  lam: sp.Rational(1, 3)})
    ck("no lambda (indeed no complex lambda) makes P2 vanish identically in (alpha,beta,q0,r0): "
       "its coefficients generate the unit ideal of Q[lambda]", no_weight_kills(P2, [lam]))
    ck("decoupling: off {lambda = 1} U {r0 = beta} U {P2 = 0} the A-marginal drifts at order "
       "exactly 1 and the association at order exactly 2 -- the decoupling the paper turns on",
       ok_A and ok_P2)

    # sanity rows at the old rational point (the claims rest on the rows above)
    k, co = order_in_c_at_generic(d.subs(lam, sp.Rational(1, 3)))
    ka, _ = order_in_c_at_generic((assoc(Pbar) - assoc(PB)).subs(lam, sp.Rational(1, 3)))
    ck("sanity: at the generic point, lambda = 1/3, recomputed from the model: marginal order 1, "
       "association order 2", k == 1 and ka == 2, f"marginal order {k}, association order {ka}")

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
