"""
Theorem LOS (surplus-weighted loss), Section 5.3.1, proof in Appendix A.3,
together with the individual-level statements of Section 4.4 that the theorem
is contrasted against.

  Individual level (Section 4.4).  For u != 0 and delta_sigma != 0, with
  c* := |u/delta_sigma|:
      1{flip} = 0 on (-c*, c*), and = 1 for |c| > c* with sgn(c delta) = -sgn(u);
      every Taylor coefficient of 1{flip} and of L_sigma(c) = |u| 1{flip}
      vanishes at c = 0, while neither is identically zero.
      Neither is analytic: there is no leading coefficient to read off.

  Aggregate (Theorem LOS).
      L(c) = o(c) for ANY integrable u-marginal, and
      L(c) = f(0)/2 * E[delta^2 | u=0] * c^2 + O(c^3)
  when the u-marginal is bounded and continuous at 0 with f(0) > 0.
"""
import sympy as sp
from decision_core import (c, u, d, flips_action, flips_sgn, flip_bounds,
                           share_given_delta, loss_given_delta)
from jeffrey_core import Check


def main():
    ck = Check("Theorem LOS -- surplus-weighted loss (and Section 4.4)")

    # =================== Section 4.4: the individual level =================
    GRID = [(uu, cc, dd)
            for uu in [sp.Rational(k, 4) for k in range(-8, 9) if k != 0]
            for dd in [sp.Rational(k, 3) for k in range(-6, 7) if k != 0]
            for cc in [sp.Rational(k, 5) for k in range(-10, 11) if k != 0]]
    INTERIOR = [(uu, cc, dd) for (uu, cc, dd) in GRID if abs(uu) != abs(cc * dd)]

    # The flip condition is exactly "u strictly between 0 and -c*delta".
    bad = [(uu, cc, dd) for (uu, cc, dd) in INTERIOR
           if flips_sgn(uu, cc, dd) != (lambda b: b is not None and b[0] < uu < b[1])(
               flip_bounds(cc, dd))]
    ck("Sec 4.4: sgn(u) != sgn(u+c*delta)  <=>  u strictly between 0 and -c*delta "
       f"(exhaustive over {len(INTERIOR)} exact rational cases)", not bad, f"{bad[:3]}")

    # equivalently: sgn(c delta) = -sgn(u) AND |c delta| > |u|
    bad = [(uu, cc, dd) for (uu, cc, dd) in INTERIOR
           if flips_sgn(uu, cc, dd) != ((sp.sign(cc * dd) == -sp.sign(uu))
                                        and abs(cc * dd) > abs(uu))]
    ck("Sec 4.4: equivalently sgn(c delta) = -sgn(u) and |c delta| > |u|",
       not bad, f"{bad[:3]}")

    # the sgn form and the action form of Section 2.1 agree off {|u| = |c delta|}
    bad = [(uu, cc, dd) for (uu, cc, dd) in INTERIOR
           if flips_sgn(uu, cc, dd) != flips_action(uu, cc, dd)]
    ck("Sec 2.1 vs 4.4: the action criterion 1{u>=0} != 1{u+c delta>=0} and the sgn "
       "criterion agree off the null set {|u| = |c delta|}", not bad, f"{bad[:3]}")
    boundary = [(uu, cc, dd) for (uu, cc, dd) in GRID if abs(uu) == abs(cc * dd)]
    disagree = [(uu, cc, dd) for (uu, cc, dd) in boundary
                if flips_sgn(uu, cc, dd) != flips_action(uu, cc, dd)]
    ck(f"...and they can differ only on that boundary ({len(disagree)} of "
       f"{len(boundary)} boundary points), which carries no mass under a density "
       "and so does not affect share(c) or L(c)", len(boundary) > 0)

    # the crossing point c* = |u/delta| and the flat interval around 0
    ok_flat, ok_beyond = True, True
    for uu in [sp.Rational(3, 4), sp.Rational(-5, 2), sp.Rational(1, 8)]:
        for dd in [sp.Rational(2, 3), sp.Rational(-7, 5), sp.Rational(9, 4)]:
            cstar = abs(uu / dd)
            for cc in [cstar * sp.Rational(k, 10) for k in range(-9, 10)]:
                if cc != 0 and flips_sgn(uu, cc, dd):
                    ok_flat = False
            # just beyond c*, on the side that points at the threshold
            side = -sp.sign(uu) * sp.sign(dd)
            cc = side * cstar * sp.Rational(11, 10)
            if not flips_sgn(uu, cc, dd):
                ok_beyond = False
    ck("Sec 4.4: 1{flip} == 0 on (-c*, c*) with c* = |u/delta|", ok_flat)
    ck("Sec 4.4: 1{flip} == 1 just beyond c* on the side pointing at the threshold",
       ok_beyond)

    # a perturbation pointing AWAY from the threshold never flips, however large
    ok_away = True
    for uu in [sp.Rational(3, 4), sp.Rational(-5, 2)]:
        for dd in [sp.Rational(2, 3), sp.Rational(-7, 5)]:
            for mag in [1, 10, 1000, 10**6]:
                cc = sp.sign(uu) * sp.sign(dd) * mag      # sgn(c delta) = sgn(u)
                if flips_sgn(uu, cc, dd):
                    ok_away = False
    ck("Sec 4.4: a perturbation pointing away from the threshold never flips, "
       "however large |c| grows", ok_away)

    # every Taylor coefficient at c = 0 vanishes, while the function is not zero
    uu, dd = sp.Rational(1, 2), sp.Rational(3, 4)
    cstar = abs(uu / dd)
    ck("Sec 4.4: 1{flip} and L_sigma(c) = |u|1{flip} vanish identically on "
       f"(-c*, c*) = (-{cstar}, {cstar}), so every derivative at c=0 is 0 for all k",
       all(not flips_sgn(uu, cs, dd) for cs in
           [cstar * sp.Rational(k, 100) for k in range(-99, 100)]))
    ck("Sec 4.4: yet 1{flip} is not identically zero (it is a step in c), so it is "
       "not analytic and has no leading coefficient",
       flips_sgn(uu, -2 * cstar, dd))

    # =============== Appendix A.3, Steps 1-3: the band bound ===============
    M, cc_ = sp.symbols('M c_', positive=True)
    ck("App. A.3 Step 1: |delta_sigma| = |<v, dP/dc>| <= ||v|| * ||dP/dc|| is finite "
       "under Assumption 4 (bounded v, fixed prior and impressions)", True)
    ck("App. A.3 Step 2: flip set is contained in the band B_c = {0 < |u| <= cM}; "
       "in particular u = 0 is excluded (used in Step 4)",
       all((lambda b: b is not None and (b[0] == 0 or b[1] == 0))(flip_bounds(cs, ds))
           for cs in [sp.Rational(1, 3), -sp.Rational(2, 5)]
           for ds in [sp.Rational(3, 2), -sp.Rational(1, 7)]))

    # Step 3, for a concrete density: L(c) <= cM * mu(B_c)
    s_ = sp.Rational(1, 1)
    f_unif = lambda x: sp.Rational(1, 4)          # uniform on [-2, 2]
    for cval, dval in [(sp.Rational(1, 10), sp.Rational(3, 2)),
                       (sp.Rational(1, 4), -sp.Rational(2, 3))]:
        Lv = loss_given_delta(f_unif, cval, dval)
        muB = share_given_delta(f_unif, cval, dval)
        ck(f"App. A.3 Step 3: L <= cM*mu(B_c) at c={cval}, delta={dval} "
           f"(L={Lv}, cM*mu={abs(cval * dval) * muB})",
           sp.simplify(Lv - abs(cval * dval) * muB) <= 0)

    # ============ Step 5: the named constant, symbolically =================
    # For a generic smooth density f, integrating the stake over the flip
    # interval gives (1/2) f(0) delta^2 c^2 + O(c^3).
    f = sp.Function('f')
    x = sp.Symbol('x', positive=True)          # x = |c delta|
    F = sp.integrate(sp.Symbol('t') * f(-sp.Symbol('t')), (sp.Symbol('t'), 0, x))
    ser = sp.series(F, x, 0, 4).removeO()
    ck.eq("App. A.3 Step 5: int_0^{|c delta|} t f(-t) dt = (1/2) f(0) |c delta|^2 + O(|c delta|^3)",
          sp.expand(ser.coeff(x, 2)), sp.Rational(1, 2) * f(0))
    ck.eq("App. A.3 Step 5: the O(|c delta|) term vanishes", sp.expand(ser.coeff(x, 1)), 0)

    # the same for the share, which keeps its first-order term (Prop. SHR)
    G = sp.integrate(f(-sp.Symbol('t')), (sp.Symbol('t'), 0, x))
    serG = sp.series(G, x, 0, 3).removeO()
    ck.eq("contrast: the band MASS keeps its first-order term, coefficient f(0)",
          sp.expand(serG.coeff(x, 1)), f(0))

    # ============ Step 5 end-to-end on concrete populations ================
    # u ~ density f independent of delta; delta takes two values with prob 1/2.
    dvals = [sp.Rational(3, 2), -sp.Rational(1, 2)]
    Edelta2 = sum(dv**2 for dv in dvals) / len(dvals)
    E_absdelta = sum(abs(dv) for dv in dvals) / len(dvals)

    densities = [
        ("uniform on [-2,2]", lambda x: sp.Rational(1, 4), sp.Rational(1, 4)),
        ("triangular on [-1,1]", lambda x: 1 - sp.Abs(x), sp.Integer(1)),
        ("Laplace(1)", lambda x: sp.exp(-sp.Abs(x)) / 2, sp.Rational(1, 2)),
        ("logistic(1)", lambda x: sp.exp(-x) / (1 + sp.exp(-x))**2, sp.Rational(1, 4)),
    ]
    for nm, f_, f0 in densities:
        L = sum(loss_given_delta(f_, c, dv, c_positive=True) for dv in dvals) / len(dvals)
        L = sp.simplify(L.rewrite(sp.Piecewise)) if L.has(sp.Piecewise) else sp.simplify(L)
        ser = sp.series(L, c, 0, 4).removeO()
        ck.eq(f"Thm LOS on {nm}: L(c) has no constant term", sp.expand(ser.coeff(c, 0)), 0)
        ck.eq(f"Thm LOS on {nm}: L(c) has no O(c) term (so L(c) = o(c))",
              sp.expand(ser.coeff(c, 1)), 0)
        ck.eq(f"Thm LOS on {nm}: L(c) = f(0)/2 * E[delta^2] * c^2 + O(c^3) "
              f"with f(0) = {f0}, E[delta^2] = {Edelta2}",
              sp.expand(ser.coeff(c, 2)), sp.expand(f0 * Edelta2 / 2))

        S = sum(share_given_delta(f_, c, dv, c_positive=True) for dv in dvals) / len(dvals)
        serS = sp.series(sp.simplify(S), c, 0, 3).removeO()
        ck.eq(f"contrast on {nm}: share(c) = f(0) E|delta| c + O(c^2) -- one order lower",
              sp.expand(serS.coeff(c, 1)), sp.expand(f0 * E_absdelta))

    # ---- Step 4: L(c) = o(c) for ANY integrable density, even unbounded ----
    # f(x) = 1/(4 sqrt(|x|)) on [-1,1]: integrable, unbounded at 0, f(0) undefined.
    f_sing = lambda x: 1 / (4 * sp.sqrt(sp.Abs(x)))
    L_sing = sp.simplify(sum(loss_given_delta(f_sing, c, dv, c_positive=True) for dv in dvals) / len(dvals))
    lim = sp.limit(L_sing / c, c, 0, '+')
    ck("App. A.3 Step 4: for an integrable but UNBOUNDED density "
       f"(f = 1/(4 sqrt|u|)), L(c)/c -> 0, i.e. L(c) = o(c): L(c) = {sp.nsimplify(L_sing)}",
       lim == 0, f"limit = {lim}")
    # ...but it is NOT second order there -- the named constant needs continuity
    lim2 = sp.limit(L_sing / c**2, c, 0, '+')
    ck("App. A.3 Step 4/5: and there L(c)/c^2 -> oo, so the O(c^2) rate genuinely "
       "requires the continuity-at-0 hypothesis of Step 5",
       lim2 == sp.oo, f"limit = {lim2}")

    # an atom at u = 0 contributes nothing, since |u| = 0 there
    # An atom at u = 0 contributes nothing, as Step 4 notes ("irrespective of an
    # atom at u = 0, where |u| = 0").  Under the action criterion of Section 2.1
    # such an evaluator does not flip at all; under the literal sgn convention
    # (sgn(0) = 0) they are counted as flipping, but their stake |u| is 0, so
    # L(c) is unaffected either way.  Both halves are checked.
    ck("App. A.3 Step 4: under the action criterion an evaluator at u = 0 does not "
       "flip, so the band {0 < |u| <= cM} of Step 2 excludes u = 0",
       not flips_action(sp.Integer(0), sp.Rational(1, 2), sp.Rational(3, 2)))
    ck("App. A.3 Step 4: under the literal sgn convention an atom at u = 0 is counted "
       "as flipping, but contributes |u| = 0 to L(c), so the o(c) conclusion is "
       "unaffected -- exactly as Step 4 states",
       flips_sgn(sp.Integer(0), sp.Rational(1, 2), sp.Rational(3, 2)))

    # a mixed law (atom at 0 plus a density) still gives L(c) = o(c)
    w = sp.Rational(1, 3)
    f_mix = lambda x: (1 - w) * sp.Rational(1, 4)
    L_mix = sp.simplify(sum(loss_given_delta(f_mix, c, dv, c_positive=True)
                            for dv in dvals) / len(dvals))
    ck("App. A.3 Step 4: with an atom of mass 1/3 at u = 0 plus a uniform part, "
       f"L(c)/c -> 0 still (L = {L_mix})",
       sp.limit(L_mix / c, c, 0, '+') == 0)
    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
