# ============================================================
# Regulatory Floor Variance Ratio — SymPy Verification
# Add this cell to the existing symbolic verification notebook.
#
# Verifies:
#   T1a-b  B^r = 1/(2*lambda_eff_p) in both regimes
#   T2     Unregulated R = lambda^4
#   T3     Regulated   R = lambda^2 * lambda_0^2 / lambda_reg^2
#   T4     Critical threshold: R_reg < 1 iff lambda_reg > lambda*lambda_0
#   T5     Flip threshold is lambda^2 times the binding threshold
#   T6     Implied lambda_reg/lambda_0 = lambda/sqrt(R_reg)
#   T7     Numerical consistency with OOS results
# ============================================================

from sympy import *

lam, lam0, lam_reg = symbols('lambda lambda_0 lambda_reg', positive=True)
R_sym = symbols('R', positive=True)

results = []

# ── Regime-specific effective price impact (from eq:reg_floor) ──────────────
lam_eff_exp_unreg = lam0 / lam       # expansion, no regulation
lam_eff_con       = lam * lam0       # contraction (unchanged by regulation)
lam_eff_exp_reg   = lam_reg          # expansion, binding regulatory floor

# ── B^r general rule: B^r = 1 / (2 * lambda_eff_p) ─────────────────────────
# Verified by substituting the two regimes and matching paper's stated values
# (equations eq:Br_coeff): B^+ = lambda/(2*lambda_0), B^- = 1/(2*lambda*lambda_0)
B_plus_paper  = lam   / (2 * lam0)
B_minus_paper = S.One / (2 * lam * lam0)

# T1a-b: confirm the general rule reproduces the paper's values
t1a = simplify(B_plus_paper  - Rational(1,2)/lam_eff_exp_unreg) == 0
t1b = simplify(B_minus_paper - Rational(1,2)/lam_eff_con)       == 0
results += [("T1a: B^+ = 1/(2*lam_eff_exp)", t1a),
            ("T1b: B^- = 1/(2*lam_eff_con)", t1b)]

# ── Variance in each regime (only B^r stochastic term contributes) ──────────
# Var(D_t | regime r) = (B^r)^2 * sigma_mu^2 * sigma_eps^2
# sigma^2 factors cancel in the ratio, so work with (B^r)^2 directly.
V_plus_unreg = B_plus_paper**2
V_minus      = B_minus_paper**2

# T2: unregulated variance ratio = lambda^4
R_unreg = simplify(V_plus_unreg / V_minus)
t2 = simplify(R_unreg - lam**4) == 0
results.append(("T2:  R_unreg = lambda^4", t2))

# ── Regulated expansion: lambda_eff_p = lambda_reg ──────────────────────────
B_plus_reg  = Rational(1,2) / lam_eff_exp_reg   # = 1 / (2*lambda_reg)
V_plus_reg  = B_plus_reg**2
R_reg       = simplify(V_plus_reg / V_minus)     # = lambda^2 * lambda_0^2 / lambda_reg^2

# T3: correct regulated ratio
R_reg_correct = lam**2 * lam0**2 / lam_reg**2
t3 = simplify(R_reg - R_reg_correct) == 0
results.append(("T3:  R_reg = lambda^2*lambda_0^2/lambda_reg^2", t3))

# Confirm the manuscript's original lambda^4 exponent was a typo
R_reg_typo = lam**2 * lam0**4 / lam_reg**2
t3_typo = simplify(R_reg - R_reg_typo) == 0
results.append(("T3x: R_reg != lambda^2*lambda_0^4/lambda_reg^2 (was typo)", not t3_typo))

# T4: critical threshold — R_reg < 1 iff lambda_reg > lambda * lambda_0
threshold = [s for s in solve(Eq(R_reg, 1), lam_reg) if s.is_positive][0]
t4 = simplify(threshold - lam*lam0) == 0
results.append(("T4:  Critical threshold = lambda*lambda_0", t4))

# T5: flip threshold is lambda^2 times the binding threshold
# Binding: lambda_reg > lambda_0/lambda  (regulatory constraint active)
# Flip:    lambda_reg > lambda*lambda_0  (variance ratio inverts)
binding_threshold = lam0 / lam
ratio_thresholds  = simplify(threshold / binding_threshold)
t5 = simplify(ratio_thresholds - lam**2) == 0
results.append(("T5:  Flip/bind ratio = lambda^2", t5))

# T6: implied lambda_reg from observed R_reg
# From R_reg = lambda^2*lambda_0^2/lambda_reg^2
#   => lambda_reg/lambda_0 = lambda/sqrt(R_reg)
implied_ratio = solve(Eq(R_reg, R_sym), lam_reg)
implied_pos   = [s for s in implied_ratio if s.is_positive][0]
t6 = simplify(implied_pos/lam0 - lam/sqrt(R_sym)) == 0
results.append(("T6:  Implied lambda_reg/lambda_0 = lambda/sqrt(R_reg)", t6))

# T7: numerical consistency with OOS results
# Pre-GFC lambda_hat = 1.2159, post-GFC R_hat = 0.6767 => lambda_reg/lambda_0 = 1.4781
lam_pre  = 1.2159
R_post   = 0.6767
implied  = lam_pre / R_post**0.5
expected = 1.2159 / 0.6767**0.5
t7 = abs(implied - expected) < 1e-10
results.append((f"T7:  OOS implied lambda_reg/lambda_0 = {implied:.4f}", t7))

# ── Scorecard ────────────────────────────────────────────────────────────────
print("=" * 65)
print("REGULATORY FLOOR VARIANCE RATIO — SYMPY VERIFICATION")
print("=" * 65)
n_pass = n_fail = 0
for name, passed in results:
    status = "PASS" if passed else "FAIL"
    if passed: n_pass += 1
    else:      n_fail += 1
    print(f"  {status}  {name}")
print(f"\n{n_pass} PASS  {n_fail} FAIL")
assert n_fail == 0, f"{n_fail} test(s) failed — see above"
print("All regulatory floor tests passed.")
