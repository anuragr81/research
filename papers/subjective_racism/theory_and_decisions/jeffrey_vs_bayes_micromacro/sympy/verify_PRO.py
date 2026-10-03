"""
Proposition PRO (uniqueness of the protected statistic), Section 5.4,
proof in Appendix A.4.  This is the paper's central characterisation.

  (i)  Protection.  If dF at q(x)r, restricted to the simplex tangent space, is
       a scalar multiple of d assoc, then F(Pbar_lambda) - F(P^B) = O(c^2) for
       every prior and every lambda.
  (ii) Uniqueness.  Fix lambda in (0,1).  If F(Pbar_lambda) - F(P^B) = o(c) for
       all (alpha,beta) in some open set, then dF at q(x)r restricted to the
       simplex is a multiple of d assoc.

The four steps of the appendix proof are checked in turn, then the resulting
classification is exercised on the statistics the paper names: the association,
the odds ratio, its logarithm, Yule's Q (all protected), against the marginal
probability, the conditional probability Pbar(B=1|A=1) (unprotected), and the
conditional-probability DIFFERENCE Pbar(B=1|A=1) - Pbar(B=1|A=0) (protected) --
the boundary case discussed at the end of Section 5.4.

Everything below is symbolic in (alpha, beta, q0, r0, lambda); exceptional sets
are stated on the open cube (0,1)^4 of (alpha, beta, q0, r0), lambda in [0,1]
(the factors q0, 1-q0, r0, 1-r0 and Z have no zero there):

  * Step 4: d/dalpha (kappa/kappa') = r0(1-r0) / [(beta-r0) q0(1-q0)] has no
    zero on the cube and is undefined only on {beta = r0}, the only zero of kappa'.
  * Part (ii) for every cue and every lambda, not only at four sample priors:
    Z <G, M_lambda> is affine in (alpha, beta); the linear system its
    coefficients impose on G has the 2x2 minor lambda(1-lambda) q0^2 q1 r0 r1,
    so for every lambda in (0,1) the protected class is exactly
    span{J, grad assoc}, and at lambda in {0,1} (rank 1) it is 3-dimensional.
  * The actual expansion of F(Pbar_lambda) - F(P^B), by exact series arithmetic
    on the cells of the composed routes: for the five protected statistics the
    c^0 and c^1 coefficients vanish identically; for the four unprotected ones
    the c^0 coefficient vanishes and the c^1 coefficient is
        P(A=1):          (1-lambda)(r0-beta) q0 q1 / Z   zero on {lambda = 1} U {r0 = beta}
        P(B=1):          lambda (q0-alpha) r0 r1 / Z     zero on {lambda = 0} U {q0 = alpha}
        P(B=1|A=1):      lambda (q0-alpha) r0 r1 / Z     zero on {lambda = 0} U {q0 = alpha}
        Q11:             q1 r1 C11 / Z,  C11 = lambda r0 (q0-alpha) + (1-lambda) q0 (r0-beta),
                         zero on the hypersurface {C11 = 0} (C11 irreducible, meets
                         the cube, and no lambda makes it vanish identically),
    each equal to the criterion's <grad F, M_lambda>.  The zero sets at
    lambda = 1 and lambda = 0 are the pure populations, where the remark after
    the proof finds extra protected statistics.
"""
import itertools
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

    def log(self):
        """log a0 + log(1 + y), y = self/a0 - 1 = O(c), by the log series."""
        y = self / self.a[0] - 1
        out, p = Ser([sp.log(self.a[0])] + [0] * (len(self.a) - 1)), y
        for k in range(1, len(self.a)):
            out, p = out + p * sp.Rational((-1) ** (k + 1), k), p * y
        return out


def LOG(x):
    """log of a symbol expression or of a series."""
    return x.log() if isinstance(x, Ser) else sp.log(x)


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


X = sp.Matrix(2, 2, sp.symbols('X00 X01 X10 X11', positive=True))
SUBS_INDEP = list(zip(list(X), list(indep)))


def grad_at_indep(F):
    """grad F evaluated at the independent posterior q (x) r."""
    return sp.Matrix(2, 2, lambda i, j: sp.cancel(sp.diff(F(X), X[i, j]).subs(SUBS_INDEP)))


def in_span_J_gradassoc(G):
    """Is G in span{J, grad assoc(q(x)r)}?  Returns (bool, x, y)."""
    x_, y_ = sp.symbols('x_ y_')
    sol = sp.solve([sp.Eq(sp.cancel((x_ * J + y_ * grad_assoc_indep - G)[i, j]), 0)
                    for i in range(2) for j in range(2)], [x_, y_], dict=True)
    if not sol:
        return False, None, None
    s = sol[0]
    return True, sp.cancel(s.get(x_, x_)), sp.cancel(s.get(y_, y_))


def main():
    ck = Check("Proposition PRO -- uniqueness of the protected statistic")

    PJ_AB, PJ_BA, PB = posteriors()
    Pbar = mean_belief(PJ_AB, PJ_BA)
    M_lam = lam * kappa * R1 + (1 - lam) * kappa_p * R2

    # ================= Step 1: the aggregate moves in a plane ==============
    ck.mat_eq("Step 1: d/dc (Pbar_lambda - P^B)|_0 = M_lambda "
              "= lambda kappa R1 + (1-lambda) kappa' R2",
              mat_taylor_coeff(Pbar - PB, 1), M_lam)
    ck.mat_eq("Step 1: (Pbar_lambda - P^B)|_{c=0} = 0",
              sp.Matrix(2, 2, lambda i, j: sp.cancel((Pbar - PB)[i, j].subs(c, 0))),
              sp.zeros(2, 2))

    # ============ Step 2: the annihilator is span{J, grad assoc} ===========
    # R1, R2 independent (the appendix's row-sum argument)
    a_, b_ = sp.symbols('a_ b_')
    ck.eq("Step 2: adding the top-row entries of a R1 + b R2 gives b(r0+r1) = b",
          sp.expand((a_ * R1 + b_ * R2)[0, 0] + (a_ * R1 + b_ * R2)[0, 1]), b_)
    sol = sp.solve([sp.Eq((a_ * R1 + b_ * R2)[i, j], 0) for i in range(2) for j in range(2)],
                   [a_, b_], dict=True)
    ck("Step 2: R1, R2 are linearly independent",
       sol and all(sp.simplify(s.get(a_, 0)) == 0 and sp.simplify(s.get(b_, 0)) == 0
                   for s in sol), f"sol = {sol}")

    # the annihilator has dimension 4 - 2 = 2 (computed as a nullspace)
    def flat(Mx):
        return [Mx[i, j] for i in range(2) for j in range(2)]

    Arows = sp.Matrix([flat(R1), flat(R2)])          # <V, R_k> = 0 as a linear system
    ck("Step 2: the constraint matrix [R1; R2] has rank 2",
       Arows.rank() == 2, f"rank = {Arows.rank()}")
    ck("Step 2: dim{V : <V,R1> = <V,R2> = 0} = 4 - 2 = 2",
       len(Arows.nullspace()) == 2, f"nullity = {len(Arows.nullspace())}")

    ck.eq("Step 2: <J, R1> = 0", frob(J, R1), 0)
    ck.eq("Step 2: <J, R2> = 0", frob(J, R2), 0)
    ck.eq("Step 2: <grad assoc, R1> = 0", frob(grad_assoc_indep, R1), 0)
    ck.eq("Step 2: <grad assoc, R2> = 0", frob(grad_assoc_indep, R2), 0)

    x_, y_ = sp.symbols('x_ y_')
    dep = sp.solve([sp.Eq((x_ * J + y_ * grad_assoc_indep)[i, j], 0)
                    for i in range(2) for j in range(2)], [x_, y_], dict=True)
    ck("Step 2: J and grad assoc are linearly independent, hence span the annihilator",
       dep and all(sp.simplify(s.get(x_, 0)) == 0 and sp.simplify(s.get(y_, 0)) == 0
                   for s in dep), f"sol = {dep}")

    # ==================== Step 3: part (i), protection =====================
    ck.eq("Step 3 / part (i): grad F = xJ + y grad assoc  =>  <grad F, M_lambda> = 0 "
          "identically in (alpha, beta, lambda)",
          frob(x_ * J + y_ * grad_assoc_indep, M_lam), 0)

    # ==================== Step 4: part (ii), uniqueness ====================
    ratio = sp.cancel(kappa / kappa_p)
    ck.eq("Step 4: kappa/kappa' = (alpha-q0) r0(1-r0) / [(beta-r0) q0(1-q0)]",
          ratio, (alpha - q0) * r0 * (1 - r0) / ((beta - r0) * q0 * (1 - q0)))
    ck.eq("Step 4: d/dalpha (kappa/kappa') = r0(1-r0) / [(beta-r0) q0(1-q0)]",
          sp.cancel(sp.diff(ratio, alpha)),
          r0 * (1 - r0) / ((beta - r0) * q0 * (1 - q0)))
    zero_set(ck, "Step 4: d/dalpha (kappa/kappa') has no zero on the open cube (its numerator "
             "r0(1-r0) never vanishes there) and is undefined only on {beta = r0}: so kappa/kappa' "
             "is non-constant on every open set of priors", sp.diff(ratio, alpha), [], [beta - r0])
    ck.eq("Step 4: kappa' vanishes on {beta = r0}", kappa_p.subs(beta, r0), 0)
    zero_set(ck, "Step 4: ...and only there", kappa_p, [beta - r0])

    # Uniqueness, checked constructively: impose vanishing of the aggregate
    # first-order coefficient at an interior lambda over an open set of
    # (alpha, beta) -- realised as several independent prior points -- and solve
    # for grad F.  The solution space must be exactly span{J, grad assoc}.
    G = sp.Matrix(2, 2, sp.symbols('G00 G01 G10 G11'))
    LAM = sp.Rational(2, 5)
    pts = [(sp.Rational(1, 3), sp.Rational(1, 4)), (sp.Rational(2, 7), sp.Rational(3, 8)),
           (sp.Rational(5, 9), sp.Rational(1, 5)), (sp.Rational(3, 5), sp.Rational(5, 8))]
    base = {q0: sp.Rational(2, 5), r0: sp.Rational(5, 7)}
    eqs = []
    for (av, bv) in pts:
        sub = dict(base); sub[alpha] = av; sub[beta] = bv; sub[lam] = LAM
        eqs.append(sp.Eq(sp.cancel(frob(G, M_lam).subs(sub)), 0))
    solG = sp.solve(eqs, list(G), dict=True)
    ck("Step 4 / part (ii): vanishing of the aggregate O(c) coefficient over an open "
       "set of priors (4 independent points, lambda = 2/5) has a 2-parameter "
       "solution space for grad F",
       bool(solG) and len(set().union(*[set(sp.Matrix(2, 2, lambda i, j:
            G[i, j].subs(solG[0]))[k].free_symbols) for k in range(4)])) == 2,
       f"solG = {solG}")
    if solG:
        Gsol = sp.Matrix(2, 2, lambda i, j: G[i, j].subs(solG[0]))
        Jn = J
        GA = sp.Matrix(2, 2, lambda i, j: grad_assoc_indep[i, j].subs(base))
        # every solution is a combination of J and grad assoc
        frees = sorted(set().union(*[set(Gsol[k].free_symbols) for k in range(4)]),
                       key=str)
        ok = True
        for f in frees:
            Gf = sp.Matrix(2, 2, lambda i, j: sp.cancel(sp.diff(Gsol[i, j], f)))
            s2 = sp.solve([sp.Eq(sp.cancel((x_ * Jn + y_ * GA - Gf)[i, j]), 0)
                           for i in range(2) for j in range(2)], [x_, y_], dict=True)
            ok = ok and bool(s2)
        ck("Step 4 / part (ii): that solution space IS span{J, grad assoc} "
           "-- no other statistic is protected", ok)

    # Part (ii) symbolically in (q0, r0, lambda): Z <G, M_lambda> is affine in
    # (alpha, beta), so it vanishes on an open set of priors iff its alpha-,
    # beta- and constant coefficients do -- a linear system A g = 0, g = vec(G).
    NG = sp.Poly(sp.expand(sp.cancel(Z * frob(G, M_lam))), alpha, beta)
    ck("Step 4 / part (ii), symbolic: Z <G, M_lambda> is affine in (alpha, beta)",
       set(NG.monoms()) <= {(1, 0), (0, 1), (0, 0)}, f"monomials {NG.monoms()}")
    A = sp.Matrix([[sp.diff(NG.coeff_monomial(m), g) for g in G] for m in (alpha, beta, 1)])
    ck.mat_eq("Step 4 / part (ii), symbolic: J and grad assoc(q (x) r) solve A g = 0 "
              "identically in (q0, r0, lambda)",
              A * sp.Matrix.hstack(sp.Matrix(list(J)), sp.Matrix(list(grad_assoc_indep))),
              sp.zeros(3, 2))
    ck.eq("Step 4 / part (ii), symbolic: A has the 2x2 minor lambda(1-lambda) q0^2 q1 r0 r1",
          A[0:2, 0:2].det(), lam * (1 - lam) * q0**2 * q1 * r0 * r1)
    zero_set(ck, "Step 4 / part (ii), symbolic: that minor vanishes only at lambda in {0, 1}; so for "
             "EVERY interior lambda and every cue rank A = 2 and, J and grad assoc being independent "
             "(minor -(1-q0)), the protected class is exactly span{J, grad assoc}",
             A[0:2, 0:2].det(), [lam, lam - 1])
    minors = lambda Mx: [Mx.extract(list(rr), list(cc)).det()
                         for rr in itertools.combinations(range(3), 2)
                         for cc in itertools.combinations(range(4), 2)]
    ck("Step 4 / part (ii), symbolic: at lambda = 0 and at lambda = 1 every 2x2 minor of A vanishes "
       "identically -- rank 1, a 3-dimensional protected class (the remark below)",
       all(simp(m) == 0 for w in (0, 1) for m in minors(A.subs(lam, w)))
       and A.subs(lam, 0) != sp.zeros(3, 4) and A.subs(lam, 1) != sp.zeros(3, 4))

    # the interior-lambda hypothesis is essential (remark after the proof)
    n1 = len(sp.Matrix([flat(R1)]).nullspace())
    ck("remark: at lambda = 0 or 1 only one route direction enters, and the "
       "annihilator of a single direction is 3-dimensional, not 2", n1 == 3,
       f"dimension = {n1}")
    # concretely: at lambda = 1 there is a protected statistic outside
    # span{J, d assoc}, namely the A-marginal (Prop. DRF at lambda = 1)
    G_margA = grad_at_indep(lambda Q: Q[1, 0] + Q[1, 1])
    ck("remark: at lambda = 1 the A-marginal is protected although its "
       "differential is NOT in span{J, d assoc}",
       sp.cancel(frob(G_margA, M_lam.subs(lam, 1))) == 0
       and not in_span_J_gradassoc(G_margA)[0])

    # =============== the classification, exercised on real statistics ======
    print("\n  -- the protected class, statistic by statistic --")
    stats = [
        ("assoc",                      lambda Q: assoc(Q),                                   True),
        ("odds ratio",                 lambda Q: (Q[0, 0] * Q[1, 1]) / (Q[0, 1] * Q[1, 0]),  True),
        ("log odds ratio",             lambda Q: LOG((Q[0, 0] * Q[1, 1]) / (Q[0, 1] * Q[1, 0])),
                                                                                             True),
        ("Yule's Q",                   lambda Q: assoc(Q) / (Q[0, 0] * Q[1, 1]
                                                             + Q[0, 1] * Q[1, 0]),           True),
        ("marginal P(A=1)",            lambda Q: Q[1, 0] + Q[1, 1],                          False),
        ("marginal P(B=1)",            lambda Q: Q[0, 1] + Q[1, 1],                          False),
        ("conditional P(B=1|A=1)",     lambda Q: Q[1, 1] / (Q[1, 0] + Q[1, 1]),              False),
        ("conditional difference "
         "P(B=1|A=1) - P(B=1|A=0)",    lambda Q: Q[1, 1] / (Q[1, 0] + Q[1, 1])
                                                 - Q[0, 1] / (Q[0, 0] + Q[0, 1]),            True),
        ("cell probability Q11",       lambda Q: Q[1, 1],                                    False),
    ]
    criterion = {}
    for name, F, expected in stats:
        G = grad_at_indep(F)
        inspan, _, _ = in_span_J_gradassoc(G)
        coeff = sp.cancel(frob(G, M_lam))
        criterion[name] = coeff
        firstorder_zero = (coeff == 0)
        ck(f"{name}: dF in span{{J, d assoc}} = {inspan}, aggregate O(c) coefficient "
           f"{'vanishes' if firstorder_zero else 'is nonzero'} "
           f"-> {'PROTECTED' if inspan else 'UNPROTECTED'} (paper: "
           f"{'protected' if expected else 'unprotected'})",
           inspan == expected and firstorder_zero == expected,
           f"grad = {list(G)}, coefficient = {coeff}")

    # ---- and the criterion agrees with the actual expansion ---------------
    # c^0 and c^1 coefficients of F(Pbar_lambda) - F(P^B), computed by exact
    # series arithmetic on the cells of the composed routes, symbolically in
    # (alpha, beta, q0, r0, lambda) -- independent of the gradient criterion.
    print("\n  -- cross-check against the actual expansion in c, symbolic in lambda --")
    AB, BA, B = (series_cells(Q, 1) for Q in (PJ_AB, PJ_BA, PB))
    Pbar_s = {k: lam * AB[k] + (1 - lam) * BA[k] for k in AB}
    C11 = lam * r0 * (q0 - alpha) + (1 - lam) * q0 * (r0 - beta)
    first_order = {     # c^1 coefficient: closed form, vanishing factors, as text
        "marginal P(A=1)": ((1 - lam) * (r0 - beta) * q0 * q1 / Z, [lam - 1, r0 - beta],
                            "(1-lambda)(r0-beta) q0 q1 / Z", "{lambda = 1} U {r0 = beta}"),
        "marginal P(B=1)": (lam * (q0 - alpha) * r0 * r1 / Z, [lam, q0 - alpha],
                            "lambda (q0-alpha) r0 r1 / Z", "{lambda = 0} U {q0 = alpha}"),
        "conditional P(B=1|A=1)": (lam * (q0 - alpha) * r0 * r1 / Z, [lam, q0 - alpha],
                                   "lambda (q0-alpha) r0 r1 / Z", "{lambda = 0} U {q0 = alpha}"),
        "cell probability Q11": (q1 * r1 * C11 / Z, [C11], "q1 r1 C11 / Z, C11 = lambda r0 (q0-alpha) "
                                 "+ (1-lambda) q0 (r0-beta)", "the hypersurface {C11 = 0}"),
    }
    for name, F, expected in stats:
        s = F(Pbar_s) - F(B)
        if expected:
            ck(f"{name}: c^0 and c^1 coefficients vanish identically in (alpha,beta,q0,r0,lambda) "
               f"-> leading order >= 2, as protected", s.a[0] == 0 and s.a[1] == 0,
               f"coefficients {s.a}")
            continue
        closed, factors, text, where = first_order[name]
        ck.eq(f"{name}: c^0 coefficient vanishes identically", s.a[0], 0)
        ck.eq(f"{name}: c^1 coefficient = {text}", s.a[1], closed)
        ck.eq(f"{name}: ...which is the criterion's <grad F, M_lambda>", criterion[name], closed)
        zero_set(ck, f"{name}: exactly order 1, as unprotected, off {where}", s.a[1], factors)
    hypersurface(ck, "cell probability Q11: C11 = lambda r0 (q0-alpha) + (1-lambda) q0 (r0-beta) "
                 "is irreducible over Q and not the zero polynomial", C11, alpha,
                 {beta: sp.Rational(1, 3), q0: sp.Rational(1, 4), r0: sp.Rational(2, 3),
                  lam: sp.Rational(1, 3)})
    ck("cell probability Q11: no lambda makes C11 vanish identically in (alpha,beta,q0,r0)",
       no_weight_kills(C11, [lam]))

    # sanity row: the old rational-point recomputation (cheap: substitute first)
    pt = GENERIC + [(lam, sp.Rational(2, 5))]
    Pbar_g = Pbar.applyfunc(lambda e: sp.cancel(sp.together(e).subs(pt)))
    PB_g = PB.applyfunc(lambda e: sp.cancel(e.subs(pt)))
    orders = {name: order_in_c_at_generic(F(Pbar_g) - F(PB_g))[0]
              for name, F, expected in stats if 'log' not in name}
    ck("sanity: recomputed from the model at the generic point, lambda = 2/5, the leading orders "
       "are >= 2 for the protected statistics and 1 for the others",
       all((orders[name] is None or orders[name] >= 2) if expected else orders[name] == 1
           for name, F, expected in stats if name in orders), f"orders {orders}")

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
