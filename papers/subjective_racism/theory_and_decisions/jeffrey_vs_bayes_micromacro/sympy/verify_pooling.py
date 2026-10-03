"""
The pooling rule is immaterial at first order (Section 5 remark).

The population mean P̄_λ = λ P^J_AB + (1-λ) P^J_BA is a linear pool L of the two
posteriors.  A geometric pool G, cell-wise (P^J_AB)^λ (P^J_BA)^(1-λ) renormalised,
differs from it only at second order in c, because the two posteriors coincide
at c = 0 (Proposition IMM) and two tables that differ by O(c) have linear and
geometric means that differ by O(c^2).

Everything is symbolic in (alpha, beta, q0, r0) and λ.  Each cell of P^J_AB and
P^J_BA is a rational function of c; its 2-jet (c^0, c^1, c^2 coefficients) is
computed symbolically (c012).  The c^0..c^2 coefficients of a smooth function of
the cells depend only on the cells' 2-jets, so G's are obtained exactly from
(a) the 2-jet of the one-cell map (x, y) -> x^λ y^(1-λ), derived by sympy for
generic jets x = m + a1 c + a2 c^2, y = m + b1 c + b2 c^2 (m > 0):
        m,   λ a1 + (1-λ) b1,   λ a2 + (1-λ) b2 - λ(1-λ)(a1-b1)^2/(2m),
and (b) the series quotient for the renormalisation.

Rows:
  (1) at c = 0 both posteriors equal m = q (x) r, so both pools do
  (2) the c^0 and c^1 coefficients of L - G vanish in every cell, identically
  (3) the c^2 coefficient in closed form, through the first-order sequence gap
      D = Δ_seq = [c^1](P^J_AB - P^J_BA) = κ R1 - κ' R2 and the product table m:
        [c^2](L - G)_ij = λ(1-λ)/2 [D_ij^2/m_ij - m_ij χ²],
        χ² = Σ_kl D_kl^2/m_kl = κ²/(r0 r1) + κ'²/(q0 q1);
      equivalently, with u = α - q0, v = β - r0, s = (1,-1), ī = 1-i,
        [c^2](L - G)_ij = λ(1-λ) q_i r_j H_ij / (2 Z^2),
        H_ij = s_j r_j̄ (1-2r0) u² - 2 s_i s_j q_ī r_j̄ u v + s_i q_ī (1-2q0) v².
      Each H_ij is irreducible over Q and changes sign on the cube, so each cell's
      c^2 coefficient vanishes on a hypersurface of (0,1)^5.  The table's row-0
      sum is λ(1-λ)κ'²(1-2q0)/(2 q0 q1) and its column-0 sum λ(1-λ)κ²(1-2r0)/(2 r0 r1);
      with the case rows this shows that the whole c^2 table vanishes on (0,1)^5
      exactly on {q0 = α, r0 = β} u {r0 = β = 1/2} u {q0 = α = 1/2}
      (codimension two): off that set L and G differ at exactly second order.
  (4) the associations of the two pools agree at c^0 and c^1 identically; their
      c^2 coefficients differ by [c^2](assoc L - assoc G) = -λ(1-λ) κ κ', zero on
      (0,1)^5 exactly on {q0 = α} u {r0 = β}.
  (5) sanity rows: at the rational point GENERIC and λ in {1/2, 2/5}, the c^2
      coefficients obtained by differentiating the pools directly (no jets)
      match the closed form.
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


P5 = [alpha, beta, q0, r0, lam]
NEVER_LAM = [(lam, 'never', "never 0 on (0,1)"), (1 - lam, 'never', "never 0 on (0,1)")]
CELLS = [(i, j) for i in range(2) for j in range(2)]
s = [1, -1]
u, v = alpha - q0, beta - r0


def H(i, j):
    """The factor of [c^2](L - G)_ij that carries its zero set."""
    return (s[j] * r[1 - j] * (1 - 2 * r0) * u**2
            - 2 * s[i] * s[j] * q[1 - i] * r[1 - j] * u * v
            + s[i] * q[1 - i] * (1 - 2 * q0) * v**2)


def geo_pool(X, Y, w):
    W = sp.Matrix(2, 2, lambda i, j: X[i, j] ** w * Y[i, j] ** (1 - w))
    S = sum(W)
    return W.applyfunc(lambda e: e / S)


def main():
    ck = Check("Pooling rule -- linear and geometric pools of the two posteriors differ at second order")
    PJ_AB, PJ_BA, _ = posteriors()
    m = indep                                             # q (x) r

    # ---- 2-jets of the two posteriors, cell by cell -------------------------
    JX = {ij: c012(PJ_AB[ij]) for ij in CELLS}
    JY = {ij: c012(PJ_BA[ij]) for ij in CELLS}
    ck.mat_eq("(1) P^J_AB at c=0 is q (x) r", sp.Matrix(2, 2, lambda i, j: JX[i, j][0]), m)
    ck.mat_eq("(1) P^J_BA at c=0 is q (x) r", sp.Matrix(2, 2, lambda i, j: JY[i, j][0]), m)
    D = sp.Matrix(2, 2, lambda i, j: sp.cancel(JX[i, j][1] - JY[i, j][1]))
    ck.mat_eq("D := [c^1](P^J_AB - P^J_BA) = Delta_seq = kappa R1 - kappa' R2",
              D, kappa * R1 - kappa_p * R2)

    # ---- the one-cell lemma: 2-jet of x^lam y^(1-lam) -----------------------
    mm = sp.Symbol('m', positive=True)
    a1, a2, b1, b2 = sp.symbols('a1 a2 b1 b2', real=True)
    w = (mm + a1 * c + a2 * c**2) ** lam * (mm + b1 * c + b2 * c**2) ** (1 - lam)
    wj = [sp.cancel(sp.powsimp(sp.expand_power_base(
        sp.diff(w, c, k).subs(c, 0) / sp.factorial(k), force=True), force=True))
        for k in range(3)]
    ck("one cell: 2-jet of x^lam y^(1-lam) = (m, lam a1 + (1-lam) b1, "
       "lam a2 + (1-lam) b2 - lam(1-lam)(a1-b1)^2/(2m))",
       simp(wj[0] - mm) == 0
       and simp(wj[1] - (lam * a1 + (1 - lam) * b1)) == 0
       and simp(wj[2] - (lam * a2 + (1 - lam) * b2
                         - lam * (1 - lam) * (a1 - b1)**2 / (2 * mm))) == 0,
       f"{wj}")

    # ---- the two pools' 2-jets ----------------------------------------------
    Wj = {ij: [sp.cancel(x.subs({a1: JX[ij][1], a2: JX[ij][2], b1: JY[ij][1],
                                 b2: JY[ij][2], mm: JX[ij][0]}, simultaneous=True))
               for x in wj] for ij in CELLS}
    Wpoly = sp.Matrix(2, 2, lambda i, j: sum(Wj[i, j][k] * c**k for k in range(3)))
    S = sp.cancel(sp.together(sum(Wpoly)))
    G = {ij: c012(Wpoly[ij] / S) for ij in CELLS}
    Lj = {ij: [sp.cancel(lam * JX[ij][k] + (1 - lam) * JY[ij][k]) for k in range(3)]
          for ij in CELLS}

    ck("(2) c^0 coefficient of L - G is zero in every cell, identically",
       all(simp(Lj[ij][0] - G[ij][0]) == 0 for ij in CELLS))
    ck("(2) c^1 coefficient of L - G is zero in every cell, identically",
       all(simp(Lj[ij][1] - G[ij][1]) == 0 for ij in CELLS))

    # ---- (3) the c^2 coefficient in closed form -----------------------------
    E = sp.Matrix(2, 2, lambda i, j: sp.cancel(Lj[i, j][2] - G[i, j][2]))
    chi2 = sum(D[ij]**2 / m[ij] for ij in CELLS)
    ck.eq("(3) chi^2 := sum D_kl^2 / m_kl = kappa^2/(r0 r1) + kappa'^2/(q0 q1)",
          chi2, kappa**2 / (r0 * r1) + kappa_p**2 / (q0 * q1))
    for (i, j) in CELLS:
        ck.eq(f"(3) [c^2](L - G)_{i}{j} = lam(1-lam)/2 [D_{i}{j}^2/m_{i}{j} - m_{i}{j} chi^2]",
              E[i, j], lam * (1 - lam) / 2 * (D[i, j]**2 / m[i, j] - m[i, j] * chi2))
        ck.eq(f"(3) ... = lam(1-lam)/2 [s_j kappa^2 q_i (1-2r0)/(r0 r1) - 2 kappa kappa' s_i s_j "
              f"+ s_i kappa'^2 r_j (1-2q0)/(q0 q1)]  (cell {i}{j})",
              E[i, j], lam * (1 - lam) / 2 * (
                  s[j] * kappa**2 * q[i] * (1 - 2 * r0) / (r0 * r1)
                  - 2 * kappa * kappa_p * s[i] * s[j]
                  + s[i] * kappa_p**2 * r[j] * (1 - 2 * q0) / (q0 * q1)))
        ck.eq(f"(3) ... = lam(1-lam) q_i r_j H_{i}{j} / (2 Z^2)  (cell {i}{j})",
              E[i, j], lam * (1 - lam) * m[i, j] * H(i, j) / (2 * Z**2))
        zero_set(ck, f"(3) [c^2](L - G)_{i}{j}", E[i, j], P5,
                 f"the hypersurface {{H_{i}{j} = 0}}",
                 [(H(i, j), 'hyper', "irreducible quartic; zero on a hypersurface"),
                  (q[i], 'never', "never 0 on (0,1)"), (r[j], 'never', "never 0 on (0,1)")]
                 + NEVER_LAM)

    # the whole table: two marginal sums, then the case analysis they force
    ck.eq("(3) row-0 sum of the c^2 table = lam(1-lam) kappa'^2 (1-2q0) / (2 q0 q1)",
          E[0, 0] + E[0, 1], lam * (1 - lam) * kappa_p**2 * (1 - 2 * q0) / (2 * q0 * q1))
    ck.eq("(3) column-0 sum of the c^2 table = lam(1-lam) kappa^2 (1-2r0) / (2 r0 r1)",
          E[0, 0] + E[1, 0], lam * (1 - lam) * kappa**2 * (1 - 2 * r0) / (2 * r0 * r1))
    ck.eq("(3) sum of all four cells of the c^2 table = 0", sum(E), 0)
    zero_set(ck, "(3) kappa", kappa, P5, "{q0 = alpha}",
             [(alpha - q0, (q0, alpha), "zero iff q0 = alpha"),
              (r0, 'never', "never 0 on (0,1)"), (1 - r0, 'never', "never 0 on (0,1)")])
    zero_set(ck, "(3) kappa'", kappa_p, P5, "{r0 = beta}",
             [(beta - r0, (r0, beta), "zero iff r0 = beta"),
              (q0, 'never', "never 0 on (0,1)"), (1 - q0, 'never', "never 0 on (0,1)")])
    # by the two sums a zero of the table has kappa'^2 (1-2q0) = kappa^2 (1-2r0) = 0
    half = sp.Rational(1, 2)
    ck.mat_eq("(3) case kappa = kappa' = 0: the c^2 table vanishes on {q0 = alpha, r0 = beta}",
              E.subs({q0: alpha, r0: beta}), sp.zeros(2, 2))
    ck.mat_eq("(3) case kappa' = 0, r0 = 1/2: the c^2 table vanishes on {r0 = beta = 1/2}",
              E.subs({r0: half, beta: half}), sp.zeros(2, 2))
    ck.mat_eq("(3) case kappa = 0, q0 = 1/2: the c^2 table vanishes on {q0 = alpha = 1/2}",
              E.subs({q0: half, alpha: half}), sp.zeros(2, 2))
    ck.eq("(3) remaining case q0 = r0 = 1/2: [c^2](L - G)_00 = -lam(1-lam) kappa kappa', "
          "nonzero unless kappa kappa' = 0 (cases above); so the table vanishes on (0,1)^5 "
          "exactly on {q0=alpha, r0=beta} u {r0=beta=1/2} u {q0=alpha=1/2}",
          E[0, 0].subs({q0: half, r0: half}),
          (-lam * (1 - lam) * kappa * kappa_p).subs({q0: half, r0: half}))

    # ---- (4) associations of the two pools ----------------------------------
    Lp = sp.Matrix(2, 2, lambda i, j: sum(Lj[i, j][k] * c**k for k in range(3)))
    Gp = sp.Matrix(2, 2, lambda i, j: sum(G[i, j][k] * c**k for k in range(3)))
    A0, A1, A2 = c012(assoc(Lp) - assoc(Gp))
    ck("(4) assoc(L) - assoc(G): c^0 and c^1 coefficients vanish identically "
       "(associations agree to first order)", A0 == 0 and A1 == 0, f"{A0}, {A1}")
    ck.eq("(4) [c^2](assoc(L) - assoc(G)) = <grad assoc(q (x) r), [c^2](L - G)> "
          "= -lam(1-lam) kappa kappa'",
          A2, -lam * (1 - lam) * kappa * kappa_p)
    ck.eq("(4) ... = <grad assoc(q (x) r), [c^2](L - G)>",
          A2, frob(grad_assoc_indep, E))
    zero_set(ck, "(4) [c^2](assoc(L) - assoc(G))", A2, P5, "{q0 = alpha} u {r0 = beta}",
             [(alpha - q0, (q0, alpha), "zero iff q0 = alpha"),
              (beta - r0, (r0, beta), "zero iff r0 = beta"),
              (q0, 'never', "never 0 on (0,1)"), (1 - q0, 'never', "never 0 on (0,1)"),
              (r0, 'never', "never 0 on (0,1)"), (1 - r0, 'never', "never 0 on (0,1)")]
             + NEVER_LAM)

    # ---- (5) sanity: direct differentiation of the pools at one point -------
    AB = PJ_AB.applyfunc(lambda e: sp.cancel(e.subs(GENERIC)))
    BA = PJ_BA.applyfunc(lambda e: sp.cancel(e.subs(GENERIC)))
    for wt in (sp.Rational(1, 2), sp.Rational(2, 5)):
        lin = wt * AB + (1 - wt) * BA
        geo = geo_pool(AB, BA, wt)
        direct = [sp.simplify(sp.diff(lin[ij] - geo[ij], c, 2).subs(c, 0) / 2) for ij in CELLS]
        closed = [at_generic(E[ij].subs(lam, wt)) for ij in CELLS]
        ck(f"(5) (sanity, one point) lambda={wt}: direct c^2 coefficients at GENERIC "
           f"match the closed form", all(sp.simplify(x - y) == 0 for x, y in zip(direct, closed)),
           f"direct {direct}, closed form {closed}")
    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
