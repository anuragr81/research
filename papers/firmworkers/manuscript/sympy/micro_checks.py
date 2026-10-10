import sys
import sympy as sp

q, a, x0, t = sp.symbols('q a x0 t', positive=True)
i = sp.symbols('i', integer=True, positive=True)
mi, mk = sp.symbols('m_i m_k', positive=True)
x = x0 * (1 - q) ** (-1 / a)


def m1_printed_index_ill_posed():
    base = 1 - i
    return base.subs(i, 1) == 0 and bool(base.subs(i, 2) < 0)


def m4_adjacent_gap_decreasing_in_a():
    g = t ** (-1 / a) * sp.log(t)
    dg = sp.simplify(sp.diff(g, t))
    factored = sp.simplify(dg / t ** (-1 / a - 1))
    target = 1 - sp.log(t) / a
    identity = sp.simplify(factored - target) == 0
    q1, q2 = sp.symbols('q1 q2', positive=True)
    gap = x0 * ((1 - q2) ** (-1 / a) - (1 - q1) ** (-1 / a))
    dgap = sp.diff(gap, a)
    rhs = x0 / a ** 2 * (g.subs(t, 1 - q2) - g.subs(t, 1 - q1))
    gap_identity = sp.simplify(dgap - rhs) == 0
    samples = []
    for av in [sp.Rational(1, 2), 1, 2, 5, 20]:
        for (u, v) in [(0, sp.Rational(1, 10)), (sp.Rational(1, 2), sp.Rational(9, 10)), (sp.Rational(9, 10), sp.Rational(99, 100))]:
            samples.append(sp.N(dgap.subs({x0: 1, a: av, q1: u, q2: v})) < 0)
    return identity and gap_identity and all(samples)


def m5_top_bottom_ratio_falls_in_a():
    ratios = [sp.N(x.subs({x0: 1, a: av, q: sp.Rational(9, 10)})) for av in [sp.Rational(3, 2), 2, 3, 5]]
    return all(r1 > r2 for r1, r2 in zip(ratios, ratios[1:])), [round(float(r), 3) for r in ratios]


def m6_printed_cost_complex_branch():
    val = sp.N(((mk - mi) ** sp.Rational(1, 2)).subs({mi: 2, mk: 1}))
    return sp.im(val) != 0


def m17_variance_sign_conflict():
    lam, c, m, p = sp.symbols('lambda c m p', real=True)
    lhs = (1 - lam) ** 2 * m ** 2
    rhs_at = (p * (1 - lam) ** 2).subs({p: -1, lam: 0})
    return bool(sp.simplify(lhs.subs({lam: 0, m: 1})) > 0) and bool(rhs_at < 0)


CHECKS = {
    'M1': m1_printed_index_ill_posed,
    'M4': m4_adjacent_gap_decreasing_in_a,
    'M5': lambda: m5_top_bottom_ratio_falls_in_a()[0],
    'M6': m6_printed_cost_complex_branch,
    'M17': m17_variance_sign_conflict,
}

if __name__ == '__main__':
    failed = 0
    for key, fn in CHECKS.items():
        ok = bool(fn())
        failed += not ok
        print(f"{key} {'PASS' if ok else 'FAIL'}")
    print('M5 ratios x(0.9)/x0 at a=1.5,2,3,5:', m5_top_bottom_ratio_falls_in_a()[1])
    sys.exit(1 if failed else 0)
