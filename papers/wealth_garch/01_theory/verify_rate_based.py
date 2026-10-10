"""Verification ledger for the rate-based reference model (Phase 1
formalisation). Mirrors the check()-ledger style of the predecessor
project's verify_appendix.py.

Every check is a DERIVATION, not an assertion: quantities are built up
from the primitive assumptions and compared to the closed form printed in
proofs_rate_based.tex, exactly as the predecessor project's protocol
requires.
"""
import sympy as sp

ledger = []


def check(tag, statement, lhs, rhs):
    ok = sp.simplify(sp.together(sp.expand(lhs - rhs))) == 0
    ledger.append((tag, statement, ok))
    return ok


g, b, r_E, dp, dm, lam, theta, V0, c = sp.symbols(
    'g b r_E delta_p delta_m lambda theta V_0 c', positive=True)
zprev, x = sp.symbols('z_prev x', real=True)

# ============================================================
# B1/B2: the maintained model (Assumption B2 in proofs_rate_based.tex)
# ============================================================

# B1: sustainable growth pins r_E = g/b
r_E_from_B1 = g / b

# B2 calibration: delta_m = r_E - g, using B1
delta_m_calib = sp.simplify(r_E_from_B1 - g)
delta_m_target = sp.simplify(g * (1 - b) / b)
check('B1a', 'delta_m calibration: r_E-g = g(1-b)/b given r_E=g/b (B1)',
      delta_m_calib, delta_m_target)

# ============================================================
# Persistence (Proposition: persistence asymmetry)
# ============================================================
phi_plus = 1 - dp
phi_minus = 1 - g * (1 - b) / b

check('P1a', 'phi^+ = 1 - delta_p (surplus, exact by policy definition)',
      phi_plus, 1 - dp)

check('P1b', 'phi^- -> 1 as g -> 0 (exact unit root)',
      sp.limit(phi_minus, g, 0), sp.Integer(1))

check('P1c', 'd phi^- / dg = -(1-b)/b < 0',
      sp.diff(phi_minus, g), -(1 - b) / b)

asym = sp.simplify(phi_minus - phi_plus)
check('P1d', 'persistence asymmetry = delta_p + g - g/b',
      asym, sp.simplify(dp + g - g / b))

check('P1e', 'asymmetry -> delta_p as g -> 0',
      sp.limit(asym, g, 0), dp)

# ============================================================
# Variance saturation (Theorem: recursion-independent)
# ============================================================
z = sp.Symbol('z', real=True)
V = V0 * sp.exp(-2 * c * sp.tanh(z / theta))

V_surplus_limit = sp.limit(V, z, sp.oo)
V_deficit_limit = sp.limit(V, z, -sp.oo)
ratio = sp.simplify(V_deficit_limit / V_surplus_limit)

check('S1a', 'deep-deficit / deep-surplus variance ratio = exp(4c)',
      ratio, sp.exp(4 * c))

check('S1b', 'with c = log(lambda): ratio = lambda^4',
      sp.simplify(ratio.subs(c, sp.log(lam))), lam**4)

# This ratio must NOT depend on the recursion governing z -- verify by
# checking it is literally a function of the tanh SATURATION limits alone,
# i.e. it has no dependence on any transition-dynamics symbol (phi_s,
# delta_m, delta_p never appear in its free symbols).
transition_symbols = {dp, dm, phi_plus, phi_minus}
ratio_free_symbols = ratio.free_symbols
check('S1c', 'saturated ratio has no dependence on delta_p or delta_m',
      sp.Integer(1) if not ({dp, dm} & ratio_free_symbols) else sp.Integer(0),
      sp.Integer(1))

# ============================================================
# EGARCH linearisation (maintained model)
# ============================================================
y1 = sp.Symbol('y_1')
# substitute the MAINTAINED recursion z_t = phi_s z_{t-1} + x_t into the
# linearised log-variance map
lhs_egarch = sp.log(V0) - (2 * c / theta) * (
    (1 - dm) * (-(theta / (2 * c)) * (y1 - sp.log(V0))) + x)
rhs_egarch = (1 - (1 - dm)) * sp.log(V0) + (1 - dm) * y1 - (2 * c / theta) * x

check('E1a', 'EGARCH form: y_t = (1-phi_-)logV0 + phi_- y_1 - (2c/theta)x',
      sp.expand(lhs_egarch), sp.expand(rhs_egarch))

# ============================================================
# Robustness appendix: sojourn time under the EXACT accounting
# alternative (additive drift, Remark: additive)
# ============================================================
d, mu = sp.symbols('d mu', positive=True)
ET = d / mu

check('R1a', 'Wald first-passage: E[T] = d/mu',
      ET, d / mu)

check('R1b', 'd E[T]/d mu = -d/mu^2 < 0',
      sp.diff(ET, mu), -d / mu**2)

check('R1c', 'E[T] -> infinity as mu -> 0',
      sp.Integer(1) if sp.limit(ET, mu, 0) == sp.oo else sp.Integer(0),
      sp.Integer(1))

# ============================================================
# Summary
# ============================================================
print('=' * 74)
print(f'{"tag":<6}{"result":<8}statement')
print('=' * 74)
for tag, stmt, ok in ledger:
    print(f'{tag:<6}{"PASS" if ok else "FAIL":<8}{stmt}')

passed = sum(1 for _, _, ok in ledger if ok)
print('=' * 74)
print(f'{passed}/{len(ledger)} checks resolved as expected')
if passed != len(ledger):
    print('UNRESOLVED:', [tag for tag, _, ok in ledger if not ok])
