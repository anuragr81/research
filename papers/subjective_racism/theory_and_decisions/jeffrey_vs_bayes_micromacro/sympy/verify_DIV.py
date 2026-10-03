"""
Proposition DIV (micro divergence), Section 4.1, with the deferred proof of
Appendix A.1.

  (i)  P^J_AB - P^B = c * kappa * R1 + O(c^2),  R1 = q (x) (1,-1),
                                                kappa  = (alpha-q0) r0(1-r0)/Z
       P^J_BA - P^B = c * kappa'* R2 + O(c^2),  R2 = (1,-1) (x) r,
                                                kappa' = (beta-r0) q0(1-q0)/Z
  (ii) P^J_AB - P^J_BA = c * Delta_seq + O(c^2),  Delta_seq = kappa R1 - kappa' R2.

Appendix A.1 additionally asserts closed forms for D and Delta_seq that are
checked here entry by entry.

Exceptional sets.  Every statement below is symbolic in (alpha, beta, q0, r0)
and gives the exact zero set on the open cube (0,1)^4 (Z never vanishes there):

  * kappa  = (alpha-q0) r0(1-r0)/Z     vanishes exactly on {q0 = alpha}
    (r0, 1-r0 never vanish); kappa' = (beta-r0) q0(1-q0)/Z exactly on
    {r0 = beta}.  Since R1, R2 have no zero entry on the cube, the leading
    gap kappa R1 (route AB) is zero exactly on {q0 = alpha} and kappa' R2
    (route BA) exactly on {r0 = beta}.
  * Delta_seq: its column-0 sum is kappa and its row-0 sum is -kappa', so
    Delta_seq = 0 exactly on {q0 = alpha, r0 = beta} (codimension two).
  * Delta_seq,00 = q0 r0 L/Z with L = (alpha-beta) + r0(1-alpha) - q0(1-beta),
    irreducible over Q: it vanishes exactly on the hypersurface {L = 0}
    (codimension one; it contains the symmetry locus {alpha=beta, q0=r0}).
  * On the symmetry locus, Delta_seq,01 = q0(1-q0)(q0-alpha)/(alpha^2(1-alpha)^2),
    zero on (0,1)^2 exactly on {q0 = alpha}.
  * Leading order 1 of the (0,0) entries: the c^0 coefficients vanish
    identically; the c^1 coefficients are q0 kappa (zero iff q0 = alpha),
    r0 kappa' (zero iff r0 = beta) and Delta_seq,00 (zero iff L = 0), so each
    entry is exactly Theta(c) off that set.  The evaluations at the rational
    point GENERIC are kept only as sanity rows.
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
# the factor of Delta_seq,00 that carries its zero set
L_SEQ = (alpha - beta) + r0 * (1 - alpha) - q0 * (1 - beta)


def main():
    ck = Check("Proposition DIV -- micro divergence (with Appendix A.1)")

    PJ_AB, PJ_BA, PB = posteriors()

    gap_AB = PJ_AB - PB
    gap_BA = PJ_BA - PB
    seq = PJ_AB - PJ_BA

    # ---- Step 1 of the appendix proof: everything vanishes at c = 0 --------
    ck.mat_eq("Step 1: (P^J_AB - P^B)|_{c=0} = 0", gap_AB.subs(c, 0), sp.zeros(2, 2))
    ck.mat_eq("Step 1: (P^J_BA - P^B)|_{c=0} = 0", gap_BA.subs(c, 0), sp.zeros(2, 2))
    ck.mat_eq("Step 1: (P^J_AB - P^J_BA)|_{c=0} = 0", seq.subs(c, 0), sp.zeros(2, 2))

    # ---- Step 2/3: the leading coefficients --------------------------------
    D_AB = mat_taylor_coeff(gap_AB, 1)
    D_BA = mat_taylor_coeff(gap_BA, 1)
    D_seq = mat_taylor_coeff(seq, 1)

    ck.mat_eq("(i) d/dc (P^J_AB - P^B)|_0 = kappa * R1", D_AB, kappa * R1)
    ck.mat_eq("(i) d/dc (P^J_BA - P^B)|_0 = kappa' * R2", D_BA, kappa_p * R2)
    ck.mat_eq("(ii) d/dc (P^J_AB - P^J_BA)|_0 = kappa R1 - kappa' R2",
              D_seq, kappa * R1 - kappa_p * R2)

    # ---- Appendix Step 3: the explicit entries of D ------------------------
    ck.eq("App. Step 3: D_00 = q0 r0 (q0-alpha)(r0-1)/Z",
          D_AB[0, 0], q0 * r0 * (q0 - alpha) * (r0 - 1) / Z)
    ck.eq("App. Step 3: D_01 = -D_00", D_AB[0, 1], -D_AB[0, 0])
    ck.eq("App. Step 3: D_10 = -r0 (q0-alpha)(q0-1)(r0-1)/Z",
          D_AB[1, 0], -r0 * (q0 - alpha) * (q0 - 1) * (r0 - 1) / Z)
    ck.eq("App. Step 3: D_11 = -D_10", D_AB[1, 1], -D_AB[1, 0])
    ck.eq("App. Step 3: D = kappa q (x) (1,-1)  [A-marginal preserved: D_i0 = -D_i1]",
          sum(D_AB[i, j] for i in range(2) for j in range(2)), 0)
    for i in range(2):
        ck.eq(f"A-marginal of D vanishes in row {i}: D_{i}0 + D_{i}1 = 0",
              D_AB[i, 0] + D_AB[i, 1], 0)

    # mirror: on route BA the B-marginal of the leading gap vanishes
    for j in range(2):
        ck.eq(f"B-marginal of D' vanishes in column {j}: D'_0{j} + D'_1{j} = 0",
              D_BA[0, j] + D_BA[1, j], 0)

    # ---- Step 4: kappa vanishes exactly on the stated locus ----------------
    ck.eq("App. Step 4: kappa|_{q0=alpha} = 0", kappa.subs(q0, alpha), 0)
    ck.eq("App. Step 4: kappa|_{r0=0} = 0", kappa.subs(r0, 0), 0)
    ck.eq("App. Step 4: kappa|_{r0=1} = 0", kappa.subs(r0, 1), 0)
    ck.eq("kappa' |_{r0=beta} = 0", kappa_p.subs(r0, beta), 0)
    # kappa factors as (surprise in the A-impression) x (spread of the B-impression)
    ck.eq("kappa = (alpha-q0) * r0(1-r0) / Z exactly",
          kappa, (alpha - q0) * r0 * (1 - r0) / Z)
    ck.eq("kappa' = (beta-r0) * q0(1-q0) / Z exactly",
          kappa_p, (beta - r0) * q0 * (1 - q0) / Z)
    zero_set(ck, "App. Step 4: kappa", kappa, P4, "{q0 = alpha}",
             [(alpha - q0, (q0, alpha), "zero iff q0 = alpha: the A-cue delivers the prior marginal")]
             + NEVER_R)
    zero_set(ck, "kappa'", kappa_p, P4, "{r0 = beta}",
             [(beta - r0, (r0, beta), "zero iff r0 = beta: the B-cue delivers the prior marginal")]
             + NEVER_Q)
    ck.ne("(sanity, one point) kappa != 0 at GENERIC", kappa)
    ck.ne("(sanity, one point) kappa' != 0 at GENERIC", kappa_p)

    # ---- Step 5: the sequence effect --------------------------------------
    ck.eq("App. Step 5: Delta_seq,00 = q0 r0 [(alpha-beta) + r0(1-alpha) - q0(1-beta)]/Z",
          D_seq[0, 0],
          q0 * r0 * ((alpha - beta) + r0 * (1 - alpha) - q0 * (1 - beta)) / Z)
    ck.eq("App. Step 5: sum of Delta_seq entries = 0",
          sum(D_seq[i, j] for i in range(2) for j in range(2)), 0)
    zero_set(ck, "Delta_seq,00", D_seq[0, 0], P4,
             "the hypersurface {(alpha-beta) + r0(1-alpha) - q0(1-beta) = 0}",
             [(L_SEQ, 'hyper', "irreducible; zero on a hypersurface containing "
                               "the symmetry locus {alpha=beta, q0=r0}")]
             + [NEVER_Q[0], NEVER_R[0]])

    # on the symmetry locus {alpha=beta, q0=r0} the diagonal coefficient dies ...
    sym = [(beta, alpha), (r0, q0)]
    ck.eq("App. Step 5: Delta_seq,00 = 0 on {alpha=beta, q0=r0}",
          D_seq[0, 0].subs(sym), 0)
    # ... and the effect survives in the antisymmetric off-diagonal part
    seq01_sym = q0 * (1 - q0) * (q0 - alpha) / (alpha**2 * (1 - alpha)**2)
    ck.eq("App. Step 5: Delta_seq,01 = q0(1-q0)(q0-alpha)/(alpha^2 (1-alpha)^2) on the locus",
          sp.cancel(D_seq[0, 1].subs(sym)), seq01_sym)
    ck.eq("App. Step 5: Delta_seq,01 = -Delta_seq,10 on the locus",
          D_seq[0, 1].subs(sym), -D_seq[1, 0].subs(sym))
    zero_set(ck, "App. Step 5: Delta_seq,01 on the locus", seq01_sym, [alpha, q0],
             "{q0 = alpha}",
             [(q0 - alpha, (q0, alpha), "zero iff q0 = alpha")] + NEVER_Q)
    ck.ne("(sanity, one point) Delta_seq,01 on the locus != 0 at GENERIC",
          D_seq[0, 1].subs(sym))
    ck.eq("App. Step 5: Delta_seq,01 = 0 on the locus when q0 = alpha",
          D_seq[0, 1].subs(sym).subs(q0, alpha), 0)

    # ---- (ii) R1 and R2 are linearly independent ---------------------------
    # a R1 + b R2 = 0  =>  a = b = 0 for q0, r0 in (0,1)
    aa, bb = sp.symbols('a_ b_')
    sol = sp.solve([sp.Eq((aa * R1 + bb * R2)[i, j], 0)
                    for i in range(2) for j in range(2)], [aa, bb], dict=True)
    ck("(ii) R1, R2 linearly independent (only trivial combination vanishes)",
       sol in ([{aa: 0, bb: 0}], [{bb: 0, aa: 0}]), f"solutions: {sol}")

    # hence Delta_seq == 0 iff kappa = kappa' = 0: the two marginal sums of
    # Delta_seq read off kappa and kappa' directly
    ck.eq("(ii) column-0 sum of Delta_seq = kappa", D_seq[0, 0] + D_seq[1, 0], kappa)
    ck.eq("(ii) row-0 sum of Delta_seq = -kappa'", D_seq[0, 0] + D_seq[0, 1], -kappa_p)
    ck.mat_eq("(ii) Delta_seq = 0 on {q0 = alpha, r0 = beta}; with the two sums and the "
              "zero sets of kappa, kappa' above: Delta_seq = 0 on (0,1)^4 exactly there "
              "(codimension two)",
              D_seq.subs({q0: alpha, r0: beta}), sp.zeros(2, 2))
    ck.ne("(sanity, one point) Delta_seq,00 != 0 at GENERIC", D_seq[0, 0])

    # ---- the gap is genuinely Theta(c): leading order exactly 1 -----------
    # order 0 vanishes identically (Step 1); the order-1 coefficient of entry
    # (0,0) is stated in closed form with its exact zero set.
    ck.eq("Theta(c), route AB: c^1 coefficient of entry (0,0) = q0 kappa",
          D_AB[0, 0], q0 * kappa)
    zero_set(ck, "Theta(c), route AB: (P^J_AB - P^B)_00 c^1 coefficient", D_AB[0, 0], P4,
             "{q0 = alpha}",
             [(alpha - q0, (q0, alpha), "zero iff q0 = alpha")] + [NEVER_Q[0]] + NEVER_R)
    ck.eq("Theta(c), route BA: c^1 coefficient of entry (0,0) = r0 kappa' "
          "= q0(1-q0) r0 (beta-r0)/Z", D_BA[0, 0], q0 * (1 - q0) * r0 * (beta - r0) / Z)
    zero_set(ck, "Theta(c), route BA: (P^J_BA - P^B)_00 c^1 coefficient", D_BA[0, 0], P4,
             "{r0 = beta}",
             [(beta - r0, (r0, beta), "zero iff r0 = beta")] + NEVER_Q + [NEVER_R[0]])
    # (the sequence gap's c^1 coefficient Delta_seq,00 and its zero set
    #  {L = 0} are stated under Step 5 above)
    for nm, G in [("P^J_AB - P^B", gap_AB), ("P^J_BA - P^B", gap_BA),
                  ("P^J_AB - P^J_BA", seq)]:
        k, co = order_in_c_at_generic(G[0, 0])
        ck(f"(sanity, one point) {nm}: entry (0,0) has leading order 1 at GENERIC",
           k == 1, f"leading order = {k}, coefficient = {co}")

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
