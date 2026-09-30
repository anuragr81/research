"""
Shared machinery for the decision-statistics results (Theorem LOS,
Proposition SHR, and the individual-level statements of Section 4.4).

Model (Sections 2.1 and 4.4).  An evaluator has net benchmark surplus
u = s(P^B) - tau and a first-order score coefficient delta_sigma, so their
Jeffrey score sits at u + c*delta_sigma + O(c^2).  Section 2.1 has them engage
iff the score is at least the threshold, so the ACTION differs from the
benchmark action exactly when

    1{u >= 0}  !=  1{u + c*delta_sigma >= 0}                       (FLIP)

which the manuscript writes as sgn(u) != sgn(u + c*delta_sigma).  The two
readings agree off the null set {|u| = |c*delta|}; see verify_LOS.py, which
checks both the agreement and the boundary convention explicitly.

The two aggregate decision statistics are

    share(c) = Pr{flip}                          (0-1 criterion, incidence)
    L(c)     = E[ |u| * 1{flip} ]                (surplus criterion, intensity)
"""
import sympy as sp

c = sp.Symbol('c', positive=True)
u, d, t = sp.symbols('u delta t', real=True)


def flips_action(uu, cc, dd):
    """The flip event as the ACTION criterion of Section 2.1: engage iff score >= tau."""
    return bool(uu >= 0) != bool(uu + cc * dd >= 0)


def flips_sgn(uu, cc, dd):
    """The flip event written with sgn, as in Sections 4.4 and 5.3."""
    return sp.sign(uu) != sp.sign(uu + cc * dd)


def flip_bounds(cc, dd, c_positive=False):
    """
    The set of u that flip, for fixed c and delta: the interval strictly
    between 0 and -c*delta (Section 4.4; Appendix A.3 Step 2).  Returns
    (lo, hi) with lo <= hi, or None when c*delta = 0.

    With `c_positive` the orientation is decided from the sign of delta alone,
    so `cc` may be a positive symbol.
    """
    if c_positive:
        if dd == 0:
            return None
        return (-cc * dd, sp.Integer(0)) if dd > 0 else (sp.Integer(0), -cc * dd)
    x = cc * dd
    if x == 0:
        return None
    return (min(0, -x), max(0, -x))


def share_given_delta(f, cc, dd, c_positive=False, var=u):
    """Pr{flip | delta} for a density f(u): the mass of the flip interval."""
    b = flip_bounds(cc, dd, c_positive)
    if b is None:
        return sp.Integer(0)
    lo, hi = b
    return sp.integrate(f(var), (var, lo, hi))


def loss_given_delta(f, cc, dd, c_positive=False, var=u):
    """E[|u| 1{flip} | delta] for a density f(u)."""
    b = flip_bounds(cc, dd, c_positive)
    if b is None:
        return sp.Integer(0)
    lo, hi = b
    return sp.integrate(sp.Abs(var) * f(var), (var, lo, hi))
