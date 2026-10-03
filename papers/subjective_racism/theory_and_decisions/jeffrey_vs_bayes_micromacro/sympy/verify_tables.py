"""
Tables 1 and 2 of the manuscript, reproduced end to end.

Table 1 ("Order in the prior covariance c, individual versus aggregate")
and Table 2 ("What four aggregate audits of the same population find") are the
paper's summary of every order claim.  This script recomputes the order of each
listed quantity directly from the model -- individual and aggregate, belief and
decision -- and checks it against the order the table asserts.

`exactly` in the table means the leading coefficient vanishes identically in
(alpha, beta, q0, r0, lambda); `generically` means it vanishes only on a stated
lower-dimensional set.  Every order claim here rests on Taylor coefficients
computed symbolically in all parameters (by exact series arithmetic on the
cells of the composed routes): the lower coefficients are shown to vanish
identically and the leading one is given in factored closed form with its exact
zero set on the open cube (0,1)^4 of (alpha, beta, q0, r0), lambda in [0,1]
(the factors q0, 1-q0, r0, 1-r0 and Z never vanish there):

  belief statistic                     leading coefficient                  zero set
  gap P^J_AB - P^B          (ind.)     c   kappa R1                          {q0 = alpha}
  gap P^J_BA - P^B          (ind.)     c   kappa' R2                         {r0 = beta}
  gap Pbar - P^B            (agg.)     c   lambda kappa R1 + (1-lambda) kappa' R2
                                           {lambda=0, r0=beta} U {lambda=1, q0=alpha}
                                           U {q0=alpha, r0=beta}  (codimension two)
  score, weight Q00         (ind. AB)  c   q0 r0 r1 (alpha-q0) / Z           {q0 = alpha}
  score, weight Q00         (agg.)     c   q0 r0 S00 / Z,
                                           S00 = lambda r1 (alpha-q0) + (1-lambda) q1 (beta-r0)
                                                                             {S00 = 0}
  assoc                     (ind. AB)  c^2 q0q1r0r1 (q0-alpha)(2r0-1) / Z^2  {q0 = alpha} U {r0 = 1/2}
  assoc                     (ind. BA)  c^2 q0q1r0r1 (r0-beta)(2q0-1) / Z^2   {r0 = beta} U {q0 = 1/2}
  assoc                     (agg.)     c^2 q0q1r0r1 P2 / Z^2,                {P2 = 0}
         P2 = lambda (q0-alpha)(2r0-1) + (1-lambda)(r0-beta)(2q0-1) - lambda(1-lambda)(q0-alpha)(r0-beta)
            (the two route coefficients mixed, less the cross term lambda(1-lambda) kappa kappa')
  marginal read FIRST       (ind.)     c^0, c^1 vanish identically: O(c^2)
  B-marginal, route AB      (ind.)     c   r0 r1 (q0-alpha) / Z              {q0 = alpha}
  A-marginal, route BA      (ind.)     c   q0 q1 (r0-beta) / Z               {r0 = beta}
  A-marginal                (agg.)     c   (1-lambda) q0 q1 (r0-beta) / Z    {lambda = 1} U {r0 = beta}
  B-marginal                (agg.)     c   lambda r0 r1 (q0-alpha) / Z       {lambda = 0} U {q0 = alpha}
  some marginal, every lambda (agg.)   fails exactly on {r0 = beta} U {q0 = alpha}

S00 and P2 are irreducible over Q, meet the cube at a rational point with
nonzero gradient (so each zero set is a hypersurface there), and no lambda
makes either vanish identically.  The decision rows are symbolic in the
density height f(0) > 0 near the threshold and in a two-point law of delta with
atoms d1 > 0 and -d2 < 0: share = f(0) E|delta| c and loss = f(0)/2 E[delta^2]
c^2 exactly, with positive coefficients (they vanish only if f(0) = 0 or
delta = 0 almost surely).
"""
import sympy as sp
from jeffrey_core import *
from decision_core import (c as cd, flip_bounds, share_given_delta, loss_given_delta)


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
    ck = Check("Tables 1 and 2 -- the order summary, end to end")

    PJ_AB, PJ_BA, PB = posteriors()
    Pbar = mean_belief(PJ_AB, PJ_BA)
    AB, BA, B = (series_cells(Q, 2) for Q in (PJ_AB, PJ_BA, PB))
    Pb = mix(AB, BA, lam)
    O2 = sp.zeros(2, 2)

    def gap(X, Y, k):
        """c^k coefficient matrix of X - Y."""
        return sp.Matrix(2, 2, lambda i, j: (X[i, j] - Y[i, j]).a[k])

    mA1 = lambda Q: Q[1, 0] + Q[1, 1]
    mB1 = lambda Q: Q[0, 1] + Q[1, 1]

    # ---------------- Table 1: primitives ---------------------------------
    print("\n  -- Table 1: primitives --")
    M1 = gap(Pb, B, 1)
    ok = [ck.mat_eq("gap: c^0 coefficients of P^J_AB - P^B, P^J_BA - P^B, Pbar - P^B are 0",
                    gap(AB, B, 0).row_join(gap(BA, B, 0)).row_join(gap(Pb, B, 0)),
                    O2.row_join(O2).row_join(O2)),
          ck.mat_eq("gap, individual: c^1 coefficient of P^J_AB - P^B is kappa R1", gap(AB, B, 1),
                    kappa * R1),
          ck.mat_eq("gap, individual: c^1 coefficient of P^J_BA - P^B is kappa' R2", gap(BA, B, 1),
                    kappa_p * R2),
          zero_set(ck, "gap, individual: Theta(c) [Prop. DIV] exactly off {q0 = alpha} on route AB "
                   "(kappa R1 = 0 iff kappa = 0, as R1 has the entry q0)", kappa, [q0 - alpha]),
          zero_set(ck, "...and exactly off {r0 = beta} on route BA", kappa_p, [r0 - beta]),
          ck.mat_eq("gap, aggregate: c^1 coefficient is lambda kappa R1 + (1-lambda) kappa' R2", M1,
                    lam * kappa * R1 + (1 - lam) * kappa_p * R2),
          ck.eq("gap, aggregate: its left-column sum is lambda kappa", M1[0, 0] + M1[1, 0],
                lam * kappa),
          ck.eq("gap, aggregate: its top-row sum is (1-lambda) kappa'", M1[0, 0] + M1[0, 1],
                (1 - lam) * kappa_p),
          zero_set(ck, "gap, aggregate: lambda kappa vanishes exactly on {lambda = 0} U {q0 = alpha}",
                   lam * kappa, [lam, q0 - alpha]),
          zero_set(ck, "gap, aggregate: (1-lambda) kappa' vanishes exactly on {lambda = 1} U {r0 = beta}",
                   (1 - lam) * kappa_p, [lam - 1, r0 - beta])]
    ok_gap = ck("gap, aggregate: Theta(c) [proof of Prop. PRO] exactly off {lambda=0, r0=beta} U "
                "{lambda=1, q0=alpha} U {q0=alpha, r0=beta}, where both sums vanish -- codimension two",
                all(ok))

    v00 = lambda Q: Q[0, 0]                     # weight [[1,0],[0,0]], outside span{J, grad assoc}
    s_ind, s_agg = v00(AB) - v00(B), v00(Pb) - v00(B)
    S00 = lam * r1 * (alpha - q0) + (1 - lam) * q1 * (beta - r0)
    ck("score gap on the unprotected weight Q00: c^0 coefficients are 0, individual and aggregate",
       s_ind.a[0] == 0 and s_agg.a[0] == 0)
    ck.eq("score gap, individual (route AB): c^1 coefficient = q0 r0 r1 (alpha-q0) / Z",
          s_ind.a[1], q0 * r0 * r1 * (alpha - q0) / Z)
    zero_set(ck, "score gap, individual: Theta(c) [Lemma SCR] exactly off {q0 = alpha}",
             s_ind.a[1], [q0 - alpha])
    ck.eq("score gap, aggregate: c^1 coefficient = q0 r0 S00 / Z, "
          "S00 = lambda r1 (alpha-q0) + (1-lambda) q1 (beta-r0)", s_agg.a[1], q0 * r0 * S00 / Z)
    zero_set(ck, "score gap, aggregate: Theta(c) [Lemma SCR] exactly off the hypersurface {S00 = 0}",
             s_agg.a[1], [S00])
    hypersurface(ck, "S00 is irreducible over Q and not the zero polynomial", S00, alpha,
                 {beta: sp.Rational(2, 3), q0: sp.Rational(3, 4), r0: sp.Rational(1, 3),
                  lam: sp.Rational(1, 3)})
    ck("no lambda makes S00 vanish identically in (alpha, beta, q0, r0)", no_weight_kills(S00, [lam]))

    x_, y_ = sp.symbols('x_ y_')
    v_pr = x_ * J + y_ * grad_assoc_indep      # protected weight
    ck.eq("score gap on a protected weight has zero first-order coefficient "
          "(the aggregate route plane is annihilated)",
          frob(v_pr, lam * kappa * R1 + (1 - lam) * kappa_p * R2), 0)

    # ---------------- Table 1: belief statistics --------------------------
    print("\n  -- Table 1: belief statistics --")
    ck.eq("association: the first-order coefficient is annihilated on both routes "
          "-- <grad assoc, R1> = <grad assoc, R2> = 0 [Lemma ASC]",
          frob(grad_assoc_indep, lam * kappa * R1 + (1 - lam) * kappa_p * R2), 0)
    a_AB, a_BA, a_agg = assoc(AB) - assoc(B), assoc(BA) - assoc(B), assoc(Pb) - assoc(B)
    ok_a01 = ck("association: c^0 and c^1 coefficients of the departure vanish identically, on "
                "each route and in the aggregate for every lambda",
                all(s.a[k] == 0 for s in (a_AB, a_BA, a_agg) for k in (0, 1)))
    ind_AB = q0 * q1 * r0 * r1 * (q0 - alpha) * (2 * r0 - 1) / Z**2
    ind_BA = q0 * q1 * r0 * r1 * (r0 - beta) * (2 * q0 - 1) / Z**2
    P2 = (lam * (q0 - alpha) * (2 * r0 - 1) + (1 - lam) * (r0 - beta) * (2 * q0 - 1)
          - lam * (1 - lam) * (q0 - alpha) * (r0 - beta))
    ck.eq("association, individual (route AB): c^2 coefficient = q0q1r0r1 (q0-alpha)(2r0-1) / Z^2",
          a_AB.a[2], ind_AB)
    zero_set(ck, "association, individual: exactly order 2 [Lemma ASC] off {q0 = alpha} U {r0 = 1/2} "
             "on route AB", a_AB.a[2], [q0 - alpha, 2 * r0 - 1])
    ck.eq("association, individual (route BA): c^2 coefficient = q0q1r0r1 (r0-beta)(2q0-1) / Z^2",
          a_BA.a[2], ind_BA)
    zero_set(ck, "...and off {r0 = beta} U {q0 = 1/2} on route BA", a_BA.a[2],
             [r0 - beta, 2 * q0 - 1])
    ok = [ck.eq("association, aggregate: c^2 coefficient = q0q1r0r1 P2 / Z^2", a_agg.a[2],
                q0 * q1 * r0 * r1 * P2 / Z**2),
          ck.eq("...= lambda (route AB) + (1-lambda) (route BA) - lambda(1-lambda) kappa kappa'",
                a_agg.a[2], lam * ind_AB + (1 - lam) * ind_BA - lam * (1 - lam) * kappa * kappa_p),
          ck.eq("...the cross term because assoc(kappa R1 - kappa' R2) = kappa kappa'",
                assoc(kappa * R1 - kappa_p * R2), kappa * kappa_p),
          zero_set(ck, "association, aggregate: exactly order 2, not 1 and not higher [Prop. DEC], "
                   "off the hypersurface {P2 = 0}", a_agg.a[2], [P2]),
          hypersurface(ck, "P2 is irreducible over Q and not the zero polynomial", P2, alpha,
                       {beta: sp.Rational(1, 4), q0: sp.Rational(1, 3), r0: sp.Rational(2, 5),
                        lam: sp.Rational(1, 3)}),
          ck("no lambda makes P2 vanish identically in (alpha, beta, q0, r0)",
             no_weight_kills(P2, [lam]))]
    ok_agg = all(ok)

    K = q0 * q1 * (r0 - beta) / Z
    KB = r0 * r1 * (q0 - alpha) / Z
    first = [mA1(AB) - mA1(B), mB1(BA) - mB1(B)]
    ck("marginal: on each route the attribute read FIRST is O(c^2) -- c^0 and c^1 coefficients "
       "vanish identically", all(s.a[k] == 0 for s in first for k in (0, 1)))
    lastB, lastA = mB1(AB) - mB1(B), mA1(BA) - mA1(B)
    ck("marginal: route AB, B read LAST: c^0 = 0, c^1 = r0 r1 (q0-alpha) / Z",
       lastB.a[0] == 0 and simp(lastB.a[1] - KB) == 0)
    zero_set(ck, "...Theta(c) exactly off {q0 = alpha}", lastB.a[1], [q0 - alpha])
    ck("marginal: route BA, A read LAST: c^0 = 0, c^1 = q0 q1 (r0-beta) / Z",
       lastA.a[0] == 0 and simp(lastA.a[1] - K) == 0)
    zero_set(ck, "...Theta(c) exactly off {r0 = beta}", lastA.a[1], [r0 - beta])
    gA, gB = mA1(Pb) - mA1(B), mB1(Pb) - mB1(B)
    ok = [ck("marginal, aggregate: c^0 coefficients vanish, A and B", gA.a[0] == 0 and gB.a[0] == 0),
          ck.eq("marginal, aggregate: A coefficient (1-lambda) K", gA.a[1], (1 - lam) * K),
          ck.eq("marginal, aggregate: B coefficient lambda K_B", gB.a[1], lam * KB),
          ck.eq("K K_B = K_B (1-lambda)K + K lambda K_B: a lambda killing both forces K K_B = 0",
                K * KB, KB * gA.a[1] + K * gB.a[1]),
          zero_set(ck, "K K_B vanishes exactly on {r0 = beta} U {q0 = alpha}", K * KB,
                   [r0 - beta, q0 - alpha]),
          ck("...and there lambda = 0 (resp. 1) does kill both",
             all(simp(e.subs(s)) == 0 for e in (gA.a[1], gB.a[1])
                 for s in ({r0: beta, lam: 0}, {q0: alpha, lam: 1})))]
    ck("marginal: aggregate Theta(c) for at least one attribute, for EVERY lambda, exactly off "
       "{r0 = beta} U {q0 = alpha}", all(ok))
    ok_A = zero_set(ck, "marginal, aggregate A: Theta(c) exactly off {lambda = 1} U {r0 = beta}",
                    gA.a[1], [lam - 1, r0 - beta])

    # sanity row: the old rational-point recomputation from the model
    LAM = sp.Rational(2, 5)
    order_agg = lambda e: order_in_c_at_generic(sp.together(e).subs(lam, LAM))[0]
    order_ind = lambda e: order_in_c_at_generic(e)[0]
    orders = [order_ind((PJ_AB - PB)[0, 0]), order_agg((Pbar - PB)[0, 0]),
              order_ind(assoc(PJ_AB) - assoc(PB)), order_agg(assoc(Pbar) - assoc(PB)),
              order_ind(mB1(PJ_AB) - mB1(PB)), order_ind(mA1(PJ_BA) - mA1(PB)),
              order_agg(mA1(Pbar) - mA1(PB))]
    ck("sanity: recomputed from the model at the generic point, lambda = 2/5, the belief rows "
       "(gap/score Q00 ind. and agg., assoc ind. and agg., last-read marginals, aggregate "
       "A-marginal) have orders 1, 1, 2, 2, 1, 1, 1", orders == [1, 1, 2, 2, 1, 1, 1],
       f"orders {orders}")

    # ---------------- Table 1: decision statistics ------------------------
    print("\n  -- Table 1: decision statistics --")
    # symbolic: density of height f0 > 0 near the threshold, delta = d1 or -d2
    # with equal weights, d1, d2 > 0.
    f0, d1, d2, uu = sp.symbols('f0 d1 d2 u_', positive=True)
    f_ = lambda x: f0
    dvals = [d1, -d2]
    share = sp.cancel(sum(share_given_delta(f_, cd, dv, c_positive=True) for dv in dvals) / 2)
    loss = sp.cancel(sum(loss_given_delta(f_, cd, dv, c_positive=True) for dv in dvals) / 2)
    E1, E2 = (d1 + d2) / 2, (d1**2 + d2**2) / 2          # E|delta|, E[delta^2]

    ck("loss (individual): for u > 0 the flip interval is (-c delta, 0) when delta > 0, never "
       "containing u, and (0, c|delta|) when delta < 0, containing u only once c > u/|delta|: "
       "identically 0 for c < |u|/|delta| -- no leading coefficient at all [Sec. 4.4]",
       flip_bounds(cd, d1, c_positive=True) == (-cd * d1, 0)
       and flip_bounds(cd, -d2, c_positive=True) == (0, cd * d2)
       and sp.solve(sp.Eq(cd * d2, uu), cd) == [uu / d2])
    ok_loss = ck.eq("loss (aggregate): = f(0)/2 E[delta^2] c^2 exactly -- O(c^2), no c^1 term "
                    "[Theorem LOS]", loss, f0 * E2 / 2 * cd**2)
    ok_share = ck.eq("share (aggregate): = f(0) E|delta| c exactly -- Theta(c) [Prop. SHR]",
                     share, f0 * E1 * cd)
    ok_pos = ck("the two leading coefficients f(0)E|delta| and f(0)E[delta^2]/2 are positive: "
                "they vanish only if f(0) = 0 or delta = 0 almost surely",
                bool((f0 * E1).is_positive) and bool((f0 * E2 / 2).is_positive))
    ck("share and loss decouple by exactly one order in c: loss/share = c E[delta^2]/(2 E|delta|)",
       ok_loss and ok_share and ok_pos
       and simp(loss / share - cd * E2 / (2 * E1)) == 0)

    # ---------------- Table 2: the four audit questions -------------------
    print("\n  -- Table 2: what four aggregate audits of the same population find --")
    ck("Q1 'How much stereotype does the average belief carry?' "
       "-> assoc(Pbar) - assoc(PB) = O(c^2), exactly order 2 off {P2 = 0}", ok_agg)
    ck("Q2 'How far does believed prevalence sit from the benchmark?' "
       "-> Pbar(A=1) - PB(A=1) = Theta(c) off {lambda = 1} U {r0 = beta}", ok_A)
    ck("Q3 'What share of decisions did the reading sequence change?' "
       "-> Theta(c), coefficient f(0) E|delta| > 0", ok_share and ok_pos)
    ck("Q4 'How large is the loss through sequence-affected decisions?' "
       "-> O(c^2), no c^1 term", ok_loss)

    # ---------------- the paper's headline claim --------------------------
    print("\n  -- the headline claim --")
    ck("every individual's gap is Theta(c) (off {q0 = alpha} resp. {r0 = beta}) while the "
       "aggregate association and the surplus-weighted loss are O(c^2): an aggregate can look "
       "Bayesian when no member's belief is", ok_gap and ok_a01 and ok_loss)
    ck("...while a FIRST-order share of individuals is genuinely affected, so "
       "the null on the protected statistics is a fact about the instrument",
       ok_share and ok_pos)

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
