"""
Proposition ORD (between-order contrast), companion to DRF/SCR/DEC.

DRF and SCR compare a *route* (or the mixed aggregate) to the sequence-free
benchmark P^B.  ORD compares the two *routes to each other*:

    d/dc <v, P^J_AB - P^J_BA> |_{c=0}  =  kappa <v, R1>  -  kappa' <v, R2>,

for ANY linear read-out <v, .>.  Two things distinguish this from DRF:

  (1) It is benchmark-free and lambda-free.  DRF's aggregate drift is
      (1-lambda) K and requires knowing P^B and the order mix lambda; the
      between-order gap is the same -K c whatever the benchmark or lambda,
      because P^J_AB - P^J_BA never mentions either.  An auditor who records
      reading order can compute it; one who has only a pooled aggregate cannot.

  (2) It says what the discriminating design must have.  The contrast exists
      only when reading order varies across evaluators and is recorded; a
      pooled audit that discards order also discards this statistic.

The row table (first-order coefficient of the between-order gap in c):

    statistic        coefficient                     needs benchmark?
    A-marginal       kappa' = -K = (beta-r0)q0(1-q0)/Z      no
    B-marginal       -kappa      = (q0-alpha)r0(1-r0)/Z     no
    association      0 at first order (Theta(c^2))          no
    generic score    Theta(c) unless v in span{J,grad}      no

The four rows are read off the single formula by choosing v, and the formula
itself is checked directly against the composed routes (not merely assumed from
Proposition DIV).

Every "generically" is checked symbolically in all parameters, with its exact
exceptional set on the open cube (0,1)^4 of (alpha, beta, q0, r0) (the factors
q0, 1-q0, r0, 1-r0 and Z have no zero there):

  * A-marginal row kappa' = (beta-r0) q0 q1 / Z: zero exactly on {r0 = beta};
    B-marginal row -kappa = (q0-alpha) r0 r1 / Z: zero exactly on {q0 = alpha};
    both at once only on their intersection, of codimension two.
  * Association row: c^0, c^1 coefficients 0 identically; c^2 coefficient
        q0 q1 r0 r1 H / Z^2,   H = (q0-alpha)(2r0-1) - (r0-beta)(2q0-1),
    exactly Theta(c^2) off the hypersurface {H = 0} (H irreducible over Q; it
    contains the symmetric locus alpha = beta, q0 = r0).
  * Score row, symbolic weight v: c^1 coefficient
        [(alpha-q0) r0 r1 <v,R1> - (beta-r0) q0 q1 <v,R2>] / Z,
    numerator irreducible in (v, alpha, beta, q0, r0).  For the weight
    u = [[3,-1],[2,5]]: Su / Z, Su = (alpha-q0) r0 r1 (7q0-3) - (beta-r0) q0 q1 (7r0-6),
    exactly Theta(c) off the hypersurface {Su = 0} (Su irreducible).
  * (8) For every cue (q0, r0) in the cube -- symbolically, not only at the four
    sample priors -- the between-order coefficient vanishes on an open set of
    priors iff grad F is in span{J, grad assoc}: the linear system has a 2x2
    minor -q0^2 q1 r0 r1, never zero.  The Jacobian
        det d(kappa, kappa')/d(alpha, beta) = q0 q1 r0 r1 Jp / Z^3,
        Jp = alpha(alpha-1)(2beta-1) r0 + beta(beta-1)(2alpha-1) q0
             - alpha beta (3 alpha beta - 2 alpha - 2 beta + 1),
    is nonzero exactly off the hypersurface {Jp = 0} (Jp irreducible; it meets
    the cube, e.g. at alpha = 1/3, beta = 1/4, r0 = 1/32, q0 = 1/18).
  * (9) Association gap between populations with order mixes lam, lam':
    c^0, c^1 coefficients 0 identically in (alpha, beta, q0, r0, lam, lam'); c^2
        (lam - lam') q0 q1 r0 r1 Pm / Z^2,   Pm = H - (1 - lam - lam')(q0-alpha)(r0-beta),
    exactly second order off {lam = lam'} U {Pm = 0} (Pm irreducible, and no
    (lam, lam') makes it vanish identically).

Each irreducible factor is shown to meet the cube at a rational point where
its gradient is nonzero, so its zero set there is a hypersurface (codimension
one), not empty.  Association coefficients come from exact series arithmetic on
the cells of the composed routes.
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
    ck = Check("Proposition ORD -- the between-order contrast")

    PJ_AB, PJ_BA, PB = posteriors()

    # entrywise first-order Taylor coefficient of the between-order gap
    G1 = mat_taylor_coeff(PJ_AB - PJ_BA, 1)          # d/dc (P^J_AB - P^J_BA)|0
    # the claimed closed form
    G1_claim = kappa * R1 - kappa_p * R2

    # ---- (0) the between-order gap vanishes at c = 0 (Proposition IMM) -----
    ck.mat_eq("P^J_AB = P^J_BA at c = 0 (routes agree on independent prior)",
              mat_taylor_coeff(PJ_AB - PJ_BA, 0), sp.zeros(2, 2))

    # ---- (1) the master formula, entrywise, checked against the routes ----
    ck.mat_eq("d/dc (P^J_AB - P^J_BA)|0 = kappa R1 - kappa' R2  (entrywise)",
              G1, G1_claim)

    # linear read-out form, for a fully generic weight v
    v = sp.Matrix([[sp.Symbol('v00'), sp.Symbol('v01')],
                   [sp.Symbol('v10'), sp.Symbol('v11')]])
    lhs = frob(v, G1)
    rhs = kappa * frob(v, R1) - kappa_p * frob(v, R2)
    ck.eq("<v, .> form: coeff = kappa<v,R1> - kappa'<v,R2> for arbitrary v",
          lhs, rhs)

    # ---- (2) benchmark-free / lambda-free: the gap never mentions P^B or lambda
    # The mixed-aggregate-vs-benchmark drift (DRF) carries a (1-lambda) factor
    # and is measured against P^B; the between-order gap has neither.
    K = q0 * (1 - q0) * (r0 - beta) / Z
    ck("the gap expression contains no lambda symbol",
       lam not in (PJ_AB - PJ_BA).free_symbols)
    ck("DRF's aggregate drift DOES carry (1-lambda) -- the contrast is that ORD does not",
       sp.simplify((1 - lam) * K).has(lam))

    # ---- (3) row: A-marginal --------------------------------------------
    vA = sp.Matrix([[0, 0], [1, 1]])                 # picks Q(A=1)
    cA = frob(vA, G1)
    ck.eq("A-marginal between-order gap coeff = kappa' = (beta-r0)q0(1-q0)/Z", cA, kappa_p)
    ck.eq("...and kappa' = -K  (so the gap is -K c, DRF's coefficient with sign flip)",
          kappa_p, -K)
    # cross-check straight off the pinning identities: P^J_AB(A=1)=q1-Kc+..,
    # P^J_BA(A=1)=q1 exactly, so the gap's A-coeff is -K = kappa'.
    ck.eq("direct: P^J_AB(A=1) - P^J_BA(A=1) first-order coeff = kappa' "
          "(P^J_BA(A=1)=q1 exact, P^J_AB(A=1)=q1-Kc)",
          taylor_coeff(marg_A(PJ_AB)[1] - marg_A(PJ_BA)[1], 1), kappa_p)

    # ---- (4) row: B-marginal --------------------------------------------
    vB = sp.Matrix([[0, 1], [0, 1]])                 # picks Q(B=1)
    cB = frob(vB, G1)
    ck.eq("B-marginal between-order gap coeff = -kappa = (q0-alpha)r0(1-r0)/Z",
          cB, -kappa)

    # ---- (5) row: association is second-order ---------------------------
    AB, BA = series_cells(PJ_AB, 2), series_cells(PJ_BA, 2)
    sa = assoc(AB) - assoc(BA)
    H = (q0 - alpha) * (2 * r0 - 1) - (r0 - beta) * (2 * q0 - 1)
    ck.eq("association between-order gap: c^0 coefficient is 0 identically", sa.a[0], 0)
    ck.eq("association between-order gap: c^1 coefficient is 0 identically", sa.a[1], 0)
    ck.eq("association between-order gap: c^2 coefficient = q0 q1 r0 r1 H / Z^2, "
          "H = (q0-alpha)(2r0-1) - (r0-beta)(2q0-1)", sa.a[2], q0 * q1 * r0 * r1 * H / Z**2)
    zero_set(ck, "...so it is exactly Theta(c^2) off the hypersurface {H = 0}, its only "
             "vanishing factor", sa.a[2], [H])
    hypersurface(ck, "H is irreducible over Q and not the zero polynomial", H, alpha,
                 {beta: sp.Rational(1, 4), q0: sp.Rational(1, 3), r0: sp.Rational(2, 3)})
    ck.eq("{H = 0} contains the symmetric locus alpha = beta, q0 = r0",
          H.subs({beta: alpha, r0: q0}), 0)
    ka, _ = order_in_c_at_generic(assoc(PJ_AB) - assoc(PJ_BA))
    ck("sanity: recomputed from the model at the generic point, the leading order is 2",
       ka == 2, f"leading order in c = {ka}")
    # consistent with the linear formula: no single v reproduces assoc, but the
    # first-order piece of any linear proxy for assoc must vanish -- check that
    # the gradient of assoc at the independent point is in the null space.
    ck.eq("grad(assoc) at q(x)r annihilates the first-order gap direction",
          frob(grad_assoc_indep, G1), 0)

    # ---- (6) row: generic score is Theta(c), protected weights are not ---
    # For a fully symbolic weight v the c^1 coefficient (checked in (1)) is
    # Nv / Z with Nv irreducible: the first-order score gap vanishes exactly on
    # one hypersurface of (weight, prior, cue) space.
    Nv = (alpha - q0) * r0 * r1 * frob(v, R1) - (beta - r0) * q0 * q1 * frob(v, R2)
    ck.eq("symbolic weight v: c^1 coefficient = [(alpha-q0) r0 r1 <v,R1> - (beta-r0) q0 q1 <v,R2>]/Z",
          lhs, Nv / Z)
    zero_set(ck, "...whose numerator Nv is irreducible over Q in (v, alpha, beta, q0, r0)",
             lhs, [Nv])
    # A generic desirability weight u gives a first-order score gap.
    u = sp.Matrix([[sp.Rational(3, 1), sp.Rational(-1, 1)],
                   [sp.Rational(2, 1), sp.Rational(5, 1)]])
    Su = (alpha - q0) * r0 * r1 * (7 * q0 - 3) - (beta - r0) * q0 * q1 * (7 * r0 - 6)
    ck.eq("weight u = [[3,-1],[2,5]]: c^0 coefficient of the between-order score gap is 0",
          taylor_coeff(frob(u, PJ_AB - PJ_BA), 0), 0)
    ck.eq("weight u: c^1 coefficient = Su / Z, Su = (alpha-q0) r0 r1 (7q0-3) - (beta-r0) q0 q1 (7r0-6)",
          taylor_coeff(frob(u, PJ_AB - PJ_BA), 1), Su / Z)
    zero_set(ck, "weight u: the score gap is exactly Theta(c) off the hypersurface {Su = 0}, "
             "its only vanishing factor", Su / Z, [Su])
    hypersurface(ck, "Su is irreducible over Q and not the zero polynomial", Su, beta,
                 {alpha: sp.Rational(1, 3), q0: sp.Rational(2, 5), r0: sp.Rational(5, 7)})
    kg, _ = order_in_c_at_generic(frob(u, PJ_AB - PJ_BA))
    ck("sanity: recomputed from the model at the generic point, the leading order is 1",
       kg == 1, f"leading order = {kg}")
    # constant weight J: no score gap (probabilities sum to 1 on both routes)
    ck.eq("constant weight J: no between-order score gap (both routes normalise)",
          frob(J, G1), 0)

    # ---- (7) at least one marginal moves; both cannot be first-order-null --
    # A-coeff = kappa' , B-coeff = -kappa ; both zero only if r0=beta AND q0=alpha
    okA = zero_set(ck, "the A-marginal contrast kappa' = (beta-r0) q0 q1 / Z vanishes exactly on "
                   "{r0 = beta}", cA, [r0 - beta])
    okB = zero_set(ck, "the B-marginal contrast -kappa = (q0-alpha) r0 r1 / Z vanishes exactly on "
                   "{q0 = alpha}", cB, [q0 - alpha])
    ck("so both vanish exactly on {r0 = beta} n {q0 = alpha}, a set of codimension two",
       okA and okB)

    # ---- (8) equivalence with protection: the between-order coefficient
    # kappa<G,R1> - kappa'<G,R2> vanishes over an open set of priors (four
    # independent (alpha, beta) points) exactly when G annihilates span{R1,R2},
    # i.e. G in span{J, grad assoc} -- the same solution space PRO finds for the
    # aggregate coefficient <G, M_lambda>.  No lambda enters.
    G = sp.Matrix(2, 2, sp.symbols('G00 G01 G10 G11'))
    pts = [(sp.Rational(1, 3), sp.Rational(1, 4)), (sp.Rational(2, 7), sp.Rational(3, 8)),
           (sp.Rational(5, 9), sp.Rational(1, 5)), (sp.Rational(3, 5), sp.Rational(5, 8))]
    base = {q0: sp.Rational(2, 5), r0: sp.Rational(5, 7)}
    coeff = kappa * frob(G, R1) - kappa_p * frob(G, R2)
    eqs = []
    for (av, bv) in pts:
        sub = dict(base); sub[alpha] = av; sub[beta] = bv
        eqs.append(sp.Eq(sp.cancel(coeff.subs(sub)), 0))
    solG = sp.solve(eqs, list(G), dict=True)
    Gsol = sp.Matrix(2, 2, lambda i, j: G[i, j].subs(solG[0])) if solG else None
    free = set().union(*[Gsol[k].free_symbols for k in range(4)]) if solG else set()
    ck("(8) vanishing between-order coefficient over 4 priors leaves a 2-parameter "
       "solution space for grad F", bool(solG) and len(free) == 2, f"solG = {solG}")
    if solG:
        GA = sp.Matrix(2, 2, lambda i, j: grad_assoc_indep[i, j].subs(base))
        a_, b_ = sp.symbols('a_ b_')
        fit = sp.solve([sp.Eq(Gsol[k], a_ * J[k] + b_ * GA[k]) for k in range(4)],
                       [a_, b_] + sorted(free, key=str), dict=True)
        ck("(8) every solution is a combination of J and grad assoc, i.e. "
           "annihilates span{R1,R2}: second-order between-order gap <=> protected",
           bool(fit), f"fit = {fit}")
        R1n = sp.Matrix(2, 2, lambda i, j: R1[i, j].subs(base))
        R2n = sp.Matrix(2, 2, lambda i, j: R2[i, j].subs(base))
        ck.eq("(8) ...and the solution annihilates R1", sp.expand(frob(Gsol, R1n)), 0)
        ck.eq("(8) ...and the solution annihilates R2", sp.expand(frob(Gsol, R2n)), 0)
    # The same, symbolically in the cues: Z * coeff is affine in (alpha, beta),
    # so it vanishes on an open set of priors iff its alpha-, beta- and
    # constant coefficients do -- a linear system A g = 0 in g = vec(G).
    NG = sp.Poly(sp.expand(sp.cancel(Z * coeff)), alpha, beta)
    ck("(8) symbolic in (q0, r0): Z * coefficient is affine in (alpha, beta)",
       set(NG.monoms()) <= {(1, 0), (0, 1), (0, 0)}, f"monomials {NG.monoms()}")
    A = sp.Matrix([[sp.diff(NG.coeff_monomial(m), g) for g in G] for m in (alpha, beta, 1)])
    ck.mat_eq("(8) J and grad assoc(q (x) r) solve A g = 0, symbolically in (q0, r0)",
              A * sp.Matrix.hstack(sp.Matrix(list(J)), sp.Matrix(list(grad_assoc_indep))),
              sp.zeros(3, 2))
    zero_set(ck, "(8) A has a 2x2 minor -q0^2 q1 r0 r1 with no zero on the cube: rank A = 2, "
             "so the solution space is 2-dimensional for every cue", A[0:2, 0:2].det(), [])
    ck.eq("(8) ...that minor is -q0^2 q1 r0 r1", A[0:2, 0:2].det(), -q0**2 * q1 * r0 * r1)
    zero_set(ck, "(8) J and grad assoc are independent for every cue (a 2x2 minor is -(1-q0)): "
             "the solution space is exactly span{J, grad assoc} on the whole cube",
             sp.Matrix([[J[0, 0], J[0, 1]], [grad_assoc_indep[0, 0], grad_assoc_indep[0, 1]]]).det(),
             [])
    # kappa and kappa' vary independently across priors: the Jacobian in
    # (alpha, beta) has full rank off one hypersurface.
    Jac = sp.Matrix([[sp.diff(kappa, alpha), sp.diff(kappa, beta)],
                     [sp.diff(kappa_p, alpha), sp.diff(kappa_p, beta)]])
    detJ = sp.cancel(Jac.det())
    Jp = (alpha * (alpha - 1) * (2 * beta - 1) * r0 + beta * (beta - 1) * (2 * alpha - 1) * q0
          - alpha * beta * (3 * alpha * beta - 2 * alpha - 2 * beta + 1))
    ck.eq("(8) det d(kappa,kappa')/d(alpha,beta) = q0 q1 r0 r1 Jp / Z^3, Jp = alpha(alpha-1)(2beta-1) r0 "
          "+ beta(beta-1)(2alpha-1) q0 - alpha beta (3 alpha beta - 2alpha - 2beta + 1)",
          detJ, q0 * q1 * r0 * r1 * Jp / Z**3)
    zero_set(ck, "(8) kappa, kappa' vary independently (full-rank Jacobian) exactly off the "
             "hypersurface {Jp = 0}, its only vanishing factor", detJ, [Jp])
    hypersurface(ck, "(8) Jp is irreducible over Q and not the zero polynomial", Jp, q0,
                 {alpha: sp.Rational(1, 3), beta: sp.Rational(1, 4), r0: sp.Rational(1, 32)})

    # ---- (9) two populations differing only in the sequence mix ------------
    # Pbar_lam - Pbar_lam' = (lam - lam')(P^J_AB - P^J_BA), exactly in c: a
    # group gap produced by sequence alone is the between-order gap scaled by
    # the difference in shares, so it inherits rows (3)-(5).
    lam2 = sp.Symbol('lambda_prime')
    Pbar = lambda l: l * PJ_AB + (1 - l) * PJ_BA
    ck.mat_eq("(9) Pbar_lam - Pbar_lam' = (lam - lam')(P^J_AB - P^J_BA), all orders in c",
              (Pbar(lam) - Pbar(lam2) - (lam - lam2) * (PJ_AB - PJ_BA)).applyfunc(sp.cancel),
              sp.zeros(2, 2))
    ck.eq("(9) first-order group gap on the A-marginal = (lam - lam') kappa'",
          sp.cancel(taylor_coeff(frob(vA, Pbar(lam) - Pbar(lam2)), 1) - (lam - lam2) * kappa_p), 0)
    sg = assoc(mix(AB, BA, lam)) - assoc(mix(AB, BA, lam2))
    Pm = H - (1 - lam - lam2) * (q0 - alpha) * (r0 - beta)
    ck.eq("(9) group gap on the association: c^0 coefficient is 0 identically in "
          "(alpha, beta, q0, r0, lam, lam')", sg.a[0], 0)
    ck.eq("(9) group gap on the association: c^1 coefficient is 0 identically", sg.a[1], 0)
    ck.eq("(9) c^2 coefficient = (lam - lam') q0 q1 r0 r1 Pm / Z^2, "
          "Pm = H - (1 - lam - lam')(q0-alpha)(r0-beta)",
          sg.a[2], (lam - lam2) * q0 * q1 * r0 * r1 * Pm / Z**2)
    zero_set(ck, "(9) ...so the group gap is exactly second order off {lam = lam'} (no sequence "
             "difference) U {Pm = 0}", sg.a[2], [lam - lam2, Pm])
    hypersurface(ck, "(9) Pm is irreducible over Q and not the zero polynomial", Pm, alpha,
                 {beta: sp.Rational(1, 4), q0: sp.Rational(1, 3), r0: sp.Rational(2, 3),
                  lam: sp.Rational(2, 5), lam2: sp.Rational(4, 5)})
    ck("(9) no (lam, lam') makes Pm vanish identically in (alpha, beta, q0, r0): its "
       "coefficients generate the unit ideal", no_weight_kills(Pm, [lam, lam2]))
    kgap, _ = order_in_c_at_generic(assoc(Pbar(sp.Rational(2, 5))) - assoc(Pbar(sp.Rational(3, 4))))
    ck("(9) sanity: recomputed from the model at the generic point, lam = 2/5 vs 3/4, "
       "the leading order is 2", kgap == 2, f"leading order in c = {kgap}")

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
