"""
Proposition SHR (sequence-affected share -- incidence versus intensity),
Section 5.3.2.

    Under the same density hypothesis as Theorem LOS (u-marginal bounded,
    continuous at 0, f(0) > 0), the 0-1 criterion aggregate -- the share of the
    population whose Jeffrey and benchmark actions differ -- is genuinely
    Theta(c), not merely o(1), since the band mass itself scales as
    f(0) * O(c).

The three steps of the proof are checked in turn:

    Step 1  the affected set lies in S_c = {|u| <= |c delta| + O(c^2)},
            a band of width O(c) about the threshold;
    Step 2  mu(S_c) = f(0) * O(c), first order rather than merely bounded by it;
    Step 3  int_{S_c} 1 dmu = O(c)   (incidence)
            int_{S_c} |u| dmu = O(c^2)   (surplus, Theorem LOS),
            the extra factor of c being the weight |u|, which the band forces
            to first order.

The conclusion the paper draws -- that incidence and the protected aggregates
decouple by exactly one order in c -- is checked as a limit.
"""
import sympy as sp
from decision_core import (c, u, flips_sgn, flip_bounds,
                           share_given_delta, loss_given_delta)
from jeffrey_core import Check


def main():
    ck = Check("Proposition SHR -- sequence-affected share (incidence vs intensity)")

    dvals = [sp.Rational(3, 2), -sp.Rational(1, 2)]
    Edelta2 = sum(dv**2 for dv in dvals) / len(dvals)
    Eabs = sum(abs(dv) for dv in dvals) / len(dvals)

    # ---- Step 1: the affected set is a band of width O(c) -----------------
    ok = True
    for dv in [sp.Rational(3, 2), -sp.Rational(2, 3), sp.Rational(1, 7)]:
        for cv in [sp.Rational(1, k) for k in (5, 20, 100)]:
            lo, hi = flip_bounds(cv, dv)
            width = hi - lo
            if width != abs(cv * dv):
                ok = False
            # every flipping u satisfies |u| <= |c delta|
            for uu in [lo + (hi - lo) * sp.Rational(k, 20) for k in range(1, 20)]:
                if abs(uu) > abs(cv * dv):
                    ok = False
    ck("Step 1: the flip set is an interval of width exactly |c delta|, and every "
       "member satisfies |u| <= |c delta| = O(c)", ok)

    ck("Step 1: a flip REQUIRES |u| <= |c delta| -- evaluators further than "
       "|c delta| from the threshold never flip, whatever the sign of delta",
       all(not flips_sgn(uu, cv, dv)
           for uu in [sp.Rational(3, 2), -sp.Rational(3, 2)]
           for cv in [sp.Rational(1, 10), -sp.Rational(1, 10)]
           for dv in [sp.Rational(2, 3), -sp.Rational(2, 3)]))

    # the condition is necessary but NOT sufficient: the perturbation must also
    # point towards the threshold
    ck("Step 1: the band condition is necessary but not sufficient -- inside the "
       "band, a perturbation pointing away from the threshold still does not flip",
       abs(sp.Rational(1, 10)) <= abs(sp.Rational(1, 2) * sp.Rational(1, 2))
       and not flips_sgn(sp.Rational(1, 10), sp.Rational(1, 2), sp.Rational(1, 2)))

    # ---- Steps 2 and 3, on concrete populations ---------------------------
    densities = [
        ("uniform on [-2,2]", lambda x: sp.Rational(1, 4), sp.Rational(1, 4)),
        ("triangular on [-1,1]", lambda x: 1 - sp.Abs(x), sp.Integer(1)),
        ("Laplace(1)", lambda x: sp.exp(-sp.Abs(x)) / 2, sp.Rational(1, 2)),
        ("logistic(1)", lambda x: sp.exp(-x) / (1 + sp.exp(-x))**2, sp.Rational(1, 4)),
    ]
    for nm, f_, f0 in densities:
        share = sp.simplify(sum(share_given_delta(f_, c, dv, c_positive=True)
                                for dv in dvals) / len(dvals))
        loss = sp.simplify(sum(loss_given_delta(f_, c, dv, c_positive=True)
                               for dv in dvals) / len(dvals))
        sser = sp.series(share, c, 0, 3).removeO()
        lser = sp.series(loss, c, 0, 4).removeO()

        ck.eq(f"Step 2 on {nm}: mu(S_c) has no constant term", sp.expand(sser.coeff(c, 0)), 0)
        ck.eq(f"Step 2 on {nm}: mu(S_c) = f(0) E|delta| c + O(c^2), f(0) = {f0} "
              "-- first order, not merely bounded by it",
              sp.expand(sser.coeff(c, 1)), sp.expand(f0 * Eabs))
        ck(f"Step 2 on {nm}: the leading coefficient is nonzero, so the share is "
           "Theta(c) and not o(c)", sp.expand(sser.coeff(c, 1)) != 0)

        ck.eq(f"Step 3 on {nm}: the surplus aggregate is O(c^2) (Theorem LOS)",
              sp.expand(lser.coeff(c, 1)), 0)
        ck(f"Step 3 on {nm}: incidence O(c) vs surplus O(c^2) -- L(c)/share(c) -> 0, "
           "the two criteria decoupling by exactly one order in c",
           sp.limit(loss / share, c, 0, '+') == 0)
        ck(f"Step 3 on {nm}: L(c) <= |c delta|_max * share(c) pointwise for small c",
           all(sp.simplify((loss - max(abs(d_) for d_ in dvals) * c * share).subs(
               c, cv)) <= 0 for cv in [sp.Rational(1, 10), sp.Rational(1, 50)]))

    # ---- the paper's conclusion: "few are affected" is not available ------
    f_ = lambda x: sp.Rational(1, 4)
    share = sp.simplify(sum(share_given_delta(f_, c, dv, c_positive=True)
                            for dv in dvals) / len(dvals))
    loss = sp.simplify(sum(loss_given_delta(f_, c, dv, c_positive=True)
                           for dv in dvals) / len(dvals))
    ck("conclusion: share(c)/c tends to a strictly positive limit, so a FIRST-ORDER "
       "number of individuals is affected -- the audit result cannot be phrased as "
       "'few are affected'", sp.limit(share / c, c, 0, '+') > 0,
       f"limit = {sp.limit(share / c, c, 0, '+')}")
    ck("conclusion: L(c)/c^2 tends to a finite positive limit while share(c)/c does "
       "too, so incidence and intensity differ by exactly one order",
       sp.limit(loss / c**2, c, 0, '+').is_finite
       and sp.limit(loss / c**2, c, 0, '+') > 0)

    # numerical illustration of the split at a small c
    cv = sp.Rational(1, 1000)
    ck(f"illustration at c = {cv}: share = {sp.nsimplify(share.subs(c, cv))} "
       f"~ {float(share.subs(c, cv)):.3e}, loss = {float(loss.subs(c, cv)):.3e}; "
       "the loss is smaller by a factor of order c",
       float(loss.subs(c, cv)) / float(share.subs(c, cv)) < 10 * float(cv))

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
