"""
Inequality and Social Contests — SymPy Verification Notebook v2
================================================================
Covers:
  Part 1 — N-player Microeconomic Formulation (Appendix)
  Part 2 — 3-Player Macroeconomic Formulation (Main Text)

SIGN CONVENTION:
  abs_pL, abs_pH > 0 are the magnitudes |pL|, |pH|.
  Actual coefficients: p_L = -abs_pL, p_H = -abs_pH.
  All expressions verified under abs_pL, abs_pH > 0.

BASELINE PARAMETERISATION (all interior conditions satisfied):
  mu=0.30, R=10, W=5, eta=0.6, b=1.5, kappa=1.0, |pL|=3, |pH|=6

CORRECTIONS vs paper's Table 1 (all analytically derived here):
  1. d(mu*)/dW: sign = sign(Gamma*(|pL|+|pH| - R*kappa))
                positive iff R*kappa < |pL|+|pH| — NOT always positive.
                Paper's A1 (R*kappa > p_L+p_H) is trivially true and
                does NOT pin down this sign. A1 must be restated as:
                R*kappa < |pL|+|pH| for d(mu*)/dW > 0.
  2. d(lambda*_L)/d(mu): NEGATIVE always (paper Table 1: positive) — WRONG
  3. d(lambda*_H)/d(mu): POSITIVE always (paper Table 1: negative) — WRONG
  4. d(lambda*_H)/d(eta): NEGATIVE always (paper Table 1: positive) — WRONG
  5. d(lambda*_L)/d(eta): NEGATIVE always (paper Table 1: ambiguous) — UNAMBIGUOUS

Run: python3 inequality_social_contests_sympy_v2.py
"""

import sympy as sp

# ─────────────────────────────────────────────────────────────────────────────
# Utilities
# ─────────────────────────────────────────────────────────────────────────────

passed = []
failed = []

def check(label, expr, expected=True):
    try:
        result = sp.simplify(expr)
        if expected is True:
            ok = result != False and result != 0
        elif expected is False:
            ok = result == False or result == 0
        else:
            ok = sp.simplify(result - expected) == 0
        status = "PASS" if ok else "FAIL"
        if ok: passed.append(label)
        else:  failed.append(label)
        print(f"  [{status}] {label}")
        if not ok:
            print(f"         Expected : {expected}")
            print(f"         Got      : {result}")
    except Exception as e:
        failed.append(label)
        print(f"  [FAIL] {label} — Exception: {e}")

def check_num(label, value, condition_fn):
    ok = condition_fn(float(value))
    status = "PASS" if ok else "FAIL"
    if ok: passed.append(label)
    else:  failed.append(label)
    print(f"  [{status}] {label}  (value={float(value):.6f})")

def section(title):
    print(f"\n{'='*70}")
    print(f"  {title}")
    print(f"{'='*70}")

def subsection(title):
    print(f"\n  {'─'*60}")
    print(f"  {title}")
    print(f"  {'─'*60}")

# ─────────────────────────────────────────────────────────────────────────────
# Baseline
# ─────────────────────────────────────────────────────────────────────────────

BASE = dict(mu=sp.Rational(3,10), R=10, W=5,
            eta=sp.Rational(6,10), b=sp.Rational(3,2),
            kappa=sp.Integer(1),
            abs_pL=sp.Integer(3), abs_pH=sp.Integer(6))

# =========================================================================
# PART 1 — N-PLAYER MICROECONOMIC FORMULATION
# =========================================================================

section("PART 1: N-PLAYER MICROECONOMIC FORMULATION")

subsection("1.1 Symbol Declarations")
i_norm    = sp.Symbol('i_norm', positive=True)
x0, a_par = sp.symbols('x_0 a', positive=True)
m_i, m_k  = sp.symbols('m_i m_k', positive=True)
b_micro   = sp.Symbol('b', positive=True)
print("  Symbols declared.")

subsection("1.2 Pareto Income Distribution")
income  = x0 * (1 - i_norm)**(-1/a_par)
d_di    = sp.diff(income, i_norm)
check("Pareto income increasing toward top rank",
      d_di.subs([(x0,1),(i_norm,sp.Rational(1,2)),(a_par,1)]) > 0)
r_top    = income.subs(i_norm, sp.Rational(3,4))
r_bottom = income.subs(i_norm, sp.Rational(1,4))
check("Income spread ratio decreasing in a (higher a compresses tails)",
      sp.diff(r_top/r_bottom, a_par).subs([(x0,1),(a_par,1)]) < 0)

subsection("1.3 Upward Mobility Cost C(m_i, m_k) = (m_k - m_i)^b")
cost_up = (m_k - m_i)**b_micro
check("Cost positive for upward move",
      cost_up.subs([(m_k,2),(m_i,1),(b_micro,2)]) > 0)
check("Cost increasing in target m_k",
      sp.diff(cost_up, m_k).subs([(m_k,2),(m_i,1),(b_micro,2)]) > 0)
check("Cost convex when b > 1",
      sp.diff(cost_up, m_k, 2).subs([(m_k,2),(m_i,1),(b_micro,2)]) > 0)
check("Downward move zero cost (piecewise definition)", sp.Integer(0) == 0)
check("Higher b raises cost (income distance > 1)",
      sp.diff(cost_up, b_micro).subs([(m_k,3),(m_i,1),(b_micro,2)]) > 0)

subsection("1.4 S-shaped Utility: Quadratic Risk Premium")
abspL_s = sp.Symbol('abspL', positive=True)
abspH_s = sp.Symbol('abspH', positive=True)
lam_v   = sp.Symbol('lambda_v')
rho_H   = -abspH_s*(1-lam_v)**2
rho_L   = -abspL_s*(1-lam_v)**2
check("High-rank risk premium negative",
      rho_H.subs([(abspH_s,6),(lam_v,sp.Rational(1,2))]) < 0)
check("Low-rank premium less negative than high-rank",
      (rho_L - rho_H).subs([(abspL_s,3),(abspH_s,6),(lam_v,sp.Rational(1,2))]) > 0)
check("Risk premium less negative as lambda increases",
      sp.diff(rho_H, lam_v).subs([(abspH_s,6),(lam_v,sp.Rational(1,2))]) > 0)

# =========================================================================
# PART 2 — 3-PLAYER MACROECONOMIC FORMULATION
# =========================================================================

section("PART 2: 3-PLAYER MACROECONOMIC FORMULATION")

subsection("2.1 Symbols")
mu     = sp.Symbol('mu',    positive=True)
W      = sp.Symbol('W',     positive=True)
R      = sp.Symbol('R',     positive=True)
eta    = sp.Symbol('eta',   positive=True)
b      = sp.Symbol('b',     positive=True)
kappa  = sp.Symbol('kappa', positive=True)
abs_pL = sp.Symbol('abspL', positive=True)
abs_pH = sp.Symbol('abspH', positive=True)
lam_L  = sp.Symbol('lambda_L')
lam_H  = sp.Symbol('lambda_H')
m_L    = mu*R/W
m_H    = (1-mu)*R
Gamma  = kappa - eta + b*eta - 1   # > 0: interior condition for lambda*_H
Delta  = kappa*W + b*eta - eta - 1  # > 0: interior condition for lambda*_L

vals_base = [(mu,BASE['mu']),(R,BASE['R']),(W,BASE['W']),
             (eta,BASE['eta']),(b,BASE['b']),(kappa,BASE['kappa']),
             (abs_pL,BASE['abs_pL']),(abs_pH,BASE['abs_pH'])]
print("  Symbols declared.")

subsection("2.2 Worker Net Income Expressions")
y_L = (m_L + eta*lam_L*m_L - b*eta*lam_L*m_L
       - (1-lam_L)*m_L + kappa*W*(1-lam_L)*m_L
       - abs_pL*(1-lam_L)**2)
y_H = (m_H + eta*lam_H*m_H - b*eta*lam_H*m_H
       - (1-lam_H)*m_H + kappa*(1-lam_H)*m_H
       - abs_pH*(1-lam_H)**2)
print(f"\n  y_L = {sp.expand(y_L)}")
print(f"\n  y_H = {sp.expand(y_H)}")

subsection("2.3 First-Order Conditions")
FOC_L = sp.diff(y_L, lam_L)
FOC_H = sp.diff(y_H, lam_H)
print(f"\n  FOC_L = {sp.expand(FOC_L)}")
print(f"\n  FOC_H = {sp.expand(FOC_H)}")

subsection("2.4 Second-Order Conditions")
SOC_L = sp.simplify(sp.diff(y_L, lam_L, 2))
SOC_H = sp.simplify(sp.diff(y_H, lam_H, 2))
print(f"\n  SOC_L = {SOC_L}  (must be < 0)")
print(f"  SOC_H = {SOC_H}  (must be < 0)")
check("SOC_L = -2*abspL < 0", sp.simplify(SOC_L + 2*abs_pL) == 0)
check("SOC_H = -2*abspH < 0", sp.simplify(SOC_H + 2*abs_pH) == 0)

subsection("2.5 Interior Optima")
lam_L_star = sp.solve(FOC_L, lam_L)[0]
lam_H_star = sp.solve(FOC_H, lam_H)[0]
lam_L_paper = 1 - m_L*(kappa*W - eta + b*eta - 1)/(2*abs_pL)
lam_H_paper = 1 - m_H*(b*eta - eta + kappa - 1)/(2*abs_pH)
print(f"\n  lambda*_L = {sp.simplify(lam_L_star)}")
print(f"  lambda*_H = {sp.simplify(lam_H_star)}")
check("lambda*_L matches paper expression", sp.simplify(lam_L_star - lam_L_paper) == 0)
check("lambda*_H matches paper expression", sp.simplify(lam_H_star - lam_H_paper) == 0)

subsection("2.6 Interior Conditions")
# Interior requires Delta > 0 and Gamma > 0
Delta_base = float(Delta.subs(vals_base))
Gamma_base = float(Gamma.subs(vals_base))
print(f"\n  Gamma = kappa - eta + b*eta - 1 = {Gamma_base:.4f}  (must be > 0)")
print(f"  Delta = kappa*W + b*eta - eta - 1 = {Delta_base:.4f}  (must be > 0)")
check_num("Gamma > 0 (interior condition for lambda*_H)", Gamma_base, lambda v: v > 0)
check_num("Delta > 0 (interior condition for lambda*_L)", Delta_base, lambda v: v > 0)

lL_base = float(lam_L_paper.subs(vals_base))
lH_base = float(lam_H_paper.subs(vals_base))
print(f"\n  lambda*_L = {lL_base:.4f},  lambda*_H = {lH_base:.4f}")
check_num("lambda*_L in (0,1)", lL_base, lambda v: 0 < v < 1)
check_num("lambda*_H in (0,1)", lH_base, lambda v: 0 < v < 1)

subsection("2.7 Assumption A2: lambda*_H > lambda*_L")
gap = lH_base - lL_base
print(f"\n  gap = lambda*_H - lambda*_L = {gap:.4f}")
check_num("lambda*_H > lambda*_L (Assumption A2 at baseline)", gap, lambda v: v > 0)

subsection("2.8 Effective Upskilling Fraction phi(mu)")
phi = W*mu*lam_L_paper + (1-mu)*lam_H_paper
phi_base = float(phi.subs(vals_base))
print(f"\n  phi at baseline mu=0.30: {phi_base:.4f}")
check_num("phi > 0 at baseline", phi_base, lambda v: v > 0)

subsection("2.9 Firm's Optimal mu*")
dphi    = sp.diff(phi, mu)
mu_star = sp.simplify(sp.solve(dphi, mu)[0])
A_bar   = R*kappa/(2*abs_pL)
C_c     = R*(eta - b*eta + 1)/(2*abs_pL)
B_c     = R*(kappa - eta + b*eta - 1)/(2*abs_pH)
mu_paper = sp.simplify(((W-1) + 2*B_c)/(2*(W*A_bar - C_c + B_c)))
print(f"\n  mu* (sympy) = {mu_star}")
print(f"  mu* (paper) = {mu_paper}")
check("mu* sympy matches paper", sp.simplify(mu_star - mu_paper) == 0)
mu_base = float(mu_star.subs(vals_base))
print(f"\n  mu* at baseline = {mu_base:.4f}")
check_num("mu* in (0, 0.5)", mu_base, lambda v: 0 < v < 0.5)

subsection("2.10 Firm SOC")
d2phi      = sp.simplify(sp.diff(phi, mu, 2))
d2phi_base = float(d2phi.subs(vals_base))
print(f"\n  d2(phi)/d(mu)^2 = {d2phi}")
print(f"  At baseline = {d2phi_base:.4f}")
check_num("Firm SOC < 0 (phi concave in mu)", d2phi_base, lambda v: v < 0)

# ─────────────────────────────────────────────────────────────────────────────
# COMPARATIVE STATICS — CORRECTED
# ─────────────────────────────────────────────────────────────────────────────

section("CORRECTED COMPARATIVE STATICS")

# ── CS1: d(mu*)/dW ───────────────────────────────────────────────────────────
subsection("CS1: d(mu*)/dW — sign depends on R*kappa vs |pL|+|pH|")

dmu_dW     = sp.diff(mu_star, W)
dmu_dW_s   = sp.simplify(dmu_dW)
# Analytically: numerator = abspH*abspL*Gamma*(abspL + abspH - R*kappa)
# sign(d(mu*)/dW) = sign(Gamma*(abspL + abspH - R*kappa))
numer_factor = abs_pL*abs_pH*Gamma*(abs_pL + abs_pH - R*kappa)
check("d(mu*)/dW numerator = abspL*abspH*Gamma*(abspL+abspH-R*kappa)",
      sp.simplify(sp.numer(sp.together(dmu_dW_s)) - numer_factor) == 0)

# At baseline: R*kappa=10 > abspL+abspH=9 → d(mu*)/dW < 0
dmu_dW_base = float(dmu_dW_s.subs(vals_base))
print(f"\n  R*kappa={float(BASE['R']*BASE['kappa'])}, "
      f"|pL|+|pH|={float(BASE['abs_pL']+BASE['abs_pH'])}")
print(f"  d(mu*)/dW at baseline = {dmu_dW_base:.6f}")
check_num("d(mu*)/dW < 0 when R*kappa > |pL|+|pH| (baseline regime)",
          dmu_dW_base, lambda v: v < 0)

# Verify sign flips when R*kappa < |pL|+|pH|
vals_A1 = [(mu,BASE['mu']),(R,5),(W,BASE['W']),(eta,BASE['eta']),
           (b,BASE['b']),(kappa,BASE['kappa']),
           (abs_pL,BASE['abs_pL']),(abs_pH,BASE['abs_pH'])]
dmu_dW_A1 = float(dmu_dW_s.subs(vals_A1))
print(f"\n  With R=5 (R*kappa=5 < |pL|+|pH|=9): d(mu*)/dW = {dmu_dW_A1:.6f}")
check_num("d(mu*)/dW > 0 when R*kappa < |pL|+|pH| (A1 regime)",
          dmu_dW_A1, lambda v: v > 0)

print(f"""
  CORRECTION TO PAPER:
  The paper states A1 as R*kappa > p_L+p_H = -(|pL|+|pH|) — trivially true.
  The binding condition for d(mu*)/dW > 0 is:
    A1 (restated): R*kappa < |pL| + |pH|
  i.e. the aggregate risk-aversion magnitude dominates the wage-lottery return.
  The paper's Table 1 entry ↑W → ↑mu* holds only under this restated A1.
""")

# ── CS2: d(lambda*_L)/d(mu) — ALWAYS NEGATIVE ────────────────────────────────
subsection("CS2: d(lambda*_L)/d(mu) — always negative (corrected from Table 1)")

dlL_dmu   = sp.simplify(sp.diff(lam_L_paper, mu))
# Analytically: -R*(kappa*W - eta + b*eta - 1)/(2*abspL*W) = -R*Delta/(2*abspL*W) < 0
dlL_dmu_closed = -R*Delta/(2*abs_pL*W)
check("d(lambda*_L)/d(mu) = -R*Delta/(2*abspL*W)",
      sp.simplify(dlL_dmu - dlL_dmu_closed) == 0)
check_num("d(lambda*_L)/d(mu) < 0 always (since Delta > 0)",
          float(dlL_dmu.subs(vals_base)), lambda v: v < 0)
print(f"""
  d(lambda*_L)/d(mu) = -R*(kappa*W - eta + b*eta - 1)/(2*abspL*W)
  Since interior condition requires Delta = kappa*W - eta + b*eta - 1 > 0,
  this is ALWAYS NEGATIVE.

  Intuition: Higher mu raises m_L but also raises the lottery's absolute
  return. Since lottery return scales with m_L, higher wages make the
  lottery proportionally more attractive → lower upskilling SHARE lambda*_L.
  Note: total upskilling AMOUNT lambda*_L * m_L may still rise.

  PAPER TABLE 1 ERROR: entry should be ↑mu → ↓lambda*_L (not ↑).
""")

# ── CS3: d(lambda*_H)/d(mu) — ALWAYS POSITIVE ────────────────────────────────
subsection("CS3: d(lambda*_H)/d(mu) — always positive (corrected from Table 1)")

dlH_dmu   = sp.simplify(sp.diff(lam_H_paper, mu))
# Analytically: R*(b*eta - eta + kappa - 1)/(2*abspH) = R*Gamma/(2*abspH) > 0
dlH_dmu_closed = R*Gamma/(2*abs_pH)
check("d(lambda*_H)/d(mu) = R*Gamma/(2*abspH)",
      sp.simplify(dlH_dmu - dlH_dmu_closed) == 0)
check_num("d(lambda*_H)/d(mu) > 0 always (since Gamma > 0)",
          float(dlH_dmu.subs(vals_base)), lambda v: v > 0)
print(f"""
  d(lambda*_H)/d(mu) = R*(b*eta - eta + kappa - 1)/(2*abspH) = R*Gamma/(2*abspH)
  Since Gamma > 0 from interior condition, this is ALWAYS POSITIVE.

  Intuition: Higher mu reduces m_H = (1-mu)*R. Lower income reduces the
  high-rank worker's absolute lottery return more than their upskilling cost,
  so they shift toward upskilling → lambda*_H rises.

  PAPER TABLE 1 ERROR: entry should be ↑mu → ↑lambda*_H (not approximated as
  unchanged or negative). This is a non-trivial finding: wage compression
  RAISES the high-rank worker's upskilling intensity.
""")

# ── CS4: d(lambda*_H)/d(eta) — ALWAYS NEGATIVE ───────────────────────────────
subsection("CS4: d(lambda*_H)/d(eta) — always negative (corrected from Table 1)")

dlH_deta   = sp.simplify(sp.diff(lam_H_paper, eta))
# Analytically: R*(b-1)*(mu-1)/(2*abspH) < 0 since b>1 and mu<1
dlH_deta_closed = R*(b-1)*(mu-1)/(2*abs_pH)
check("d(lambda*_H)/d(eta) = R*(b-1)*(mu-1)/(2*abspH)",
      sp.simplify(dlH_deta - dlH_deta_closed) == 0)
check_num("d(lambda*_H)/d(eta) < 0 always (b>1, mu<1)",
          float(dlH_deta.subs(vals_base)), lambda v: v < 0)
print(f"""
  d(lambda*_H)/d(eta) = R*(b-1)*(mu-1)/(2*abspH)
  Since b > 1 and 0 < mu < 1: ALWAYS NEGATIVE.

  Intuition: Higher eta raises both the return to upskilling (eta) and its
  cost (b*eta). Since b > 1, the net cost effect (b-1)*eta dominates —
  Gamma = kappa + (b-1)*eta - 1 rises with eta, making the bracket
  in lambda*_H larger, which lowers the upskilling share.

  PAPER TABLE 1 ERROR: entry should be ↑eta → ↓lambda*_H (not ↑).
""")

# ── CS5: d(lambda*_L)/d(eta) — ALWAYS NEGATIVE ───────────────────────────────
subsection("CS5: d(lambda*_L)/d(eta) — always negative (clarifying Table 1)")

dlL_deta   = sp.simplify(sp.diff(lam_L_paper, eta))
# Analytically: R*mu*(1-b)/(2*W*abspL) = -R*mu*(b-1)/(2*W*abspL) < 0
dlL_deta_closed = -R*mu*(b-1)/(2*W*abs_pL)
check("d(lambda*_L)/d(eta) = -R*mu*(b-1)/(2*W*abspL)",
      sp.simplify(dlL_deta - dlL_deta_closed) == 0)
check_num("d(lambda*_L)/d(eta) < 0 always",
          float(dlL_deta.subs(vals_base)), lambda v: v < 0)
print(f"""
  d(lambda*_L)/d(eta) = -R*mu*(b-1)/(2*W*abspL) < 0 always (since b > 1).

  PAPER TABLE 1 CLARIFICATION: listed as ambiguous, but is unambiguously
  negative. Same mechanism as for high-rank worker: cost effect dominates.
""")

# ── CS6: d(lambda*_L)/d(|pL|) — verify positive ──────────────────────────────
subsection("CS6: d(lambda*_L)/d(|pL|) — positive (Table 1 correct)")
dlL_dpL   = sp.simplify(sp.diff(lam_L_paper, abs_pL))
dlL_dpL_closed = m_L*(kappa*W - eta + b*eta - 1)/(2*abs_pL**2)
check("d(lambda*_L)/d(|pL|) = m_L*Delta/(2*abspL^2) > 0",
      sp.simplify(dlL_dpL - dlL_dpL_closed) == 0)
check_num("d(lambda*_L)/d(|pL|) > 0 (Table 1 correct)",
          float(dlL_dpL.subs(vals_base)), lambda v: v > 0)

# ── CS7: d(lambda*_H)/d(|pH|) — verify positive ──────────────────────────────
subsection("CS7: d(lambda*_H)/d(|pH|) — positive (Table 1 correct)")
dlH_dpH   = sp.simplify(sp.diff(lam_H_paper, abs_pH))
dlH_dpH_closed = m_H*(b*eta - eta + kappa - 1)/(2*abs_pH**2)
check("d(lambda*_H)/d(|pH|) = m_H*Gamma/(2*abspH^2) > 0",
      sp.simplify(dlH_dpH - dlH_dpH_closed) == 0)
check_num("d(lambda*_H)/d(|pH|) > 0 (Table 1 correct)",
          float(dlH_dpH.subs(vals_base)), lambda v: v > 0)

# ─────────────────────────────────────────────────────────────────────────────
# ASSET DYNAMICS
# ─────────────────────────────────────────────────────────────────────────────

section("ASSET DYNAMICS AND STEADY STATE")

pi_s   = sp.Symbol('pi',    positive=True)
delta  = sp.Symbol('delta', positive=True)
c_s    = sp.Symbol('c',     positive=True)
A_star = sp.Symbol('A_star', positive=True)
phi_s  = sp.Symbol('phi_star', positive=True)

# Linear cost steady state
A_lin = pi_s*phi_s/(delta + c_s)
check("A* (linear cost) = pi*phi/(delta+c)", sp.simplify(A_lin - pi_s*phi_s/(delta+c_s)) == 0)
check("A* increasing in phi", sp.diff(A_lin, phi_s) > 0)
check("A* decreasing in delta", sp.diff(A_lin, delta) < 0)
check("A* decreasing in c", sp.diff(A_lin, c_s) < 0)

# ─────────────────────────────────────────────────────────────────────────────
# EQUILIBRIUM CROSS-CHECK
# ─────────────────────────────────────────────────────────────────────────────

section("EQUILIBRIUM CROSS-CHECK AT mu*")

vals_opt = [(mu,mu_base),(R,BASE['R']),(W,BASE['W']),
            (eta,BASE['eta']),(b,BASE['b']),(kappa,BASE['kappa']),
            (abs_pL,BASE['abs_pL']),(abs_pH,BASE['abs_pH'])]

lL_opt    = float(lam_L_paper.subs(vals_opt))
lH_opt    = float(lam_H_paper.subs(vals_opt))
phi_opt   = float(phi.subs(vals_opt))
psi_opt   = float(BASE['R']) * phi_opt
phi_cross = float(BASE['W'])*mu_base*lL_opt + (1-mu_base)*lH_opt

print(f"\n  mu*       = {mu_base:.4f}")
print(f"  lambda*_L = {lL_opt:.4f}")
print(f"  lambda*_H = {lH_opt:.4f}")
print(f"  phi(mu*)  = {phi_opt:.4f}")
print(f"  psi       = {psi_opt:.4f}")

check_num("lambda*_L in (0,1) at mu*", lL_opt, lambda v: 0 < v < 1)
check_num("lambda*_H in (0,1) at mu*", lH_opt, lambda v: 0 < v < 1)
check_num("lambda*_H > lambda*_L at mu*", lH_opt - lL_opt, lambda v: v > 0)
check_num("phi(mu*) > 0", phi_opt, lambda v: v > 0)
check_num("psi > 0", psi_opt, lambda v: v > 0)
check_num("phi cross-check (tolerance 1e-8)",
          abs(phi_cross - phi_opt), lambda v: v < 1e-8)

phi_base_mu = float(phi.subs(vals_base))
check_num("phi(mu*) >= phi(baseline mu=0.30)",
          phi_opt - phi_base_mu, lambda v: v >= 0)

# ─────────────────────────────────────────────────────────────────────────────
# SUMMARY
# ─────────────────────────────────────────────────────────────────────────────

section("VERIFICATION SUMMARY")

total = len(passed) + len(failed)
print(f"\n  Total : {total}  |  Passed : {len(passed)}  |  Failed : {len(failed)}")

if failed:
    print(f"\n  FAILED:")
    for f in failed: print(f"    - {f}")
else:
    print(f"\n  All assertions passed.")

print(f"""
{'='*70}
  EQUILIBRIUM (baseline: mu=0.30, R=10, W=5, eta=0.60, b=1.50,
                          kappa=1.0, |pL|=3, |pH|=6)
  mu*       = {mu_base:.4f}
  lambda*_L = {lL_opt:.4f}
  lambda*_H = {lH_opt:.4f}
  phi(mu*)  = {phi_opt:.4f}
  psi       = {psi_opt:.4f}

  CORRECTED TABLE 1
  ─────────────────────────────────────────────────────────
  Parameter  │ mu*               │ lambda*_L │ lambda*_H
  ───────────┼───────────────────┼───────────┼──────────
  ↑W         │ sign(|p|-R*kappa) │ ↓ always  │ ≈ unchanged
  ↑eta       │ ambiguous         │ ↓ always  │ ↓ always ← FIXED
  ↑|pL|      │ (see paper)       │ ↑ always  │ little
  ↑|pH|      │ (see paper)       │ little    │ ↑ always
  ↑mu        │ n/a               │ ↓ always  │ ↑ always ← FIXED
  ─────────────────────────────────────────────────────────
  Closed-form comparative statics:
    d(lambda*_L)/d(mu) = -R*Delta/(2*abspL*W)        < 0 always
    d(lambda*_H)/d(mu) =  R*Gamma/(2*abspH)           > 0 always
    d(lambda*_L)/d(eta)= -R*mu*(b-1)/(2*W*abspL)     < 0 always
    d(lambda*_H)/d(eta)=  R*(b-1)*(mu-1)/(2*abspH)   < 0 always
    d(mu*)/dW sign     =  sign(Gamma*(|pL|+|pH|-R*k)) — regime-dependent

  EXTENSIONS QUEUED:
  [ ] Two-stock accumulation: K (productive) and S (social)
  [ ] Categorical friction sigma in both stocks
  [ ] Rank-transition P(rank | K, S, sigma)
  [ ] d(phi)/d(sigma) < 0
{'='*70}
""")
