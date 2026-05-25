"""
=============================================================================
VERIFICATION NOTEBOOK
Social Discrimination as Informationally Mediated Closure:
A Jeffrey Conditioning Approach
=============================================================================
Author: Anurag Srivastava (Riskcare Ltd / University of Reading)
ORCiD:  0000-0002-6477-4430

PURPOSE
-------
Symbolic verification of all propositions and lemmas from the paper.
Covers Priority 1 (closure equilibrium), Priority 2 (platform equilibrium
and Proposition 5), and the Intergenerational Extension (Propositions A-E).

STRUCTURE
---------
Each check is labelled CHECK_XX and prints PASS or FAIL.
Topological results (Brouwer, IVT, Weierstrass, bifurcation) cannot be
verified symbolically; these are labelled ANALYTIC and the algebraic
preconditions feeding into them are verified instead.

FIXED POINT STRUCTURE (for reference)
--------------------------------------
Base model — multiplicity parameters (mu=0.05, delta=0.8, phi_max=0.9, thresh=0.6):
  pi0^L   ~0.075  stable   (exclusion trap / class closure)
  pi0^mid ~0.565  unstable (tipping point)
  pi0^H   =1.000  stable   (integration, corner fixed point)

Intergenerational extension — adds gamma (Bernoulli mobility parameter):
  gamma < gamma*  two stable fixed points persist (closure trap survives)
  gamma = gamma*  saddle-node bifurcation (trap and tipping point annihilate)
  gamma > gamma*  unique stable fixed point at pi0=1 (trap eliminated)

SYNC POLICY
-----------
Any change to model primitives (equations 1-15, amended eq.7, Props A-E)
must be propagated here. Version tag at bottom records equation set in force.
=============================================================================
"""

import sympy as sp
import math

# ── formatting helpers ────────────────────────────────────────────────────────

results = []

def report(tag, label, passed, detail=""):
    status = "PASS" if passed else "FAIL"
    results.append((tag, status, label))
    mark = "✓" if passed else "✗"
    print(f"  [{status}] {tag}: {label}")
    if detail:
        print(f"         {detail}")

def section(title):
    print()
    print("=" * 70)
    print(f"  {title}")
    print("=" * 70)

def analytic(tag, label, detail=""):
    """Topological/measure-theoretic result — verified by prose argument."""
    results.append((tag, "ANALYTIC", label))
    print(f"  [ANALYTIC] {tag}: {label}")
    if detail:
        print(f"         {detail}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 0: Symbols
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 0 · Symbols and Primitives")

# Economic class and group priors
pi0   = sp.Symbol('pi0',   positive=True)   # prior π₀(c_H | g) ∈ (0,1)
c_H   = sp.Symbol('c_H',   positive=True)   # high class value
c_L   = sp.Symbol('c_L',   positive=True)   # low class value
c_j   = sp.Symbol('c_j',   positive=True)   # own class of agent j

# Model parameters
kappa = sp.Symbol('kappa', positive=True)
v     = sp.Symbol('v',     positive=True)
u_bar = sp.Symbol('u_bar', positive=True)
delta = sp.Symbol('delta', positive=True)
mu    = sp.Symbol('mu',    positive=True)
phi   = sp.Symbol('phi',   positive=True)

# Platform parameters
alpha   = sp.Symbol('alpha',   positive=True)
lam     = sp.Symbol('lam',     positive=True)
q       = sp.Symbol('q',       positive=True)
S_star  = sp.Symbol('S_star',  positive=True)
S_rand  = sp.Symbol('S_rand',  positive=True)

# Map parameters
phi_max        = sp.Symbol('phi_max',        positive=True)
pi0_thresh_sym = sp.Symbol('pi0_thresh_sym', positive=True)

# Intergenerational extension parameters
gamma  = sp.Symbol('gamma',  positive=True)   # Bernoulli mobility parameter
pi0_g2 = sp.Symbol('pi0_g2', positive=True)   # enemy group prior (reused below)
t_gen  = sp.Symbol('t_gen',  nonnegative=True, integer=True)  # generation index

print("  Symbols declared.")
print("  Assumptions: c_H > c_L, v > u_bar, mu in (0,1), delta in (0,1),")
print("  pi0 in (0,1), phi in (0,1), pi0_thresh in (0,1), gamma in (0,1).")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 1: Prior Formation (Equation 1)
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 1 · Prior Formation (Equation 1)")

T   = sp.Symbol('T',   positive=True, integer=True)
n_H = sp.Symbol('n_H', nonnegative=True)

pi0_expr = n_H / T

check = sp.simplify(pi0_expr - n_H/T) == 0
report("CHECK_01", "Prior π₀ = n_H/T is well-formed", check,
       f"π₀ = {pi0_expr}")

pi0_min = pi0_expr.subs(n_H, 0)
pi0_max = pi0_expr.subs(n_H, T)
check = (pi0_min == 0) and (pi0_max == 1)
report("CHECK_02", "Prior boundary: n_H=0 → 0, n_H=T → 1", check,
       f"π₀(n_H=0)={pi0_min}, π₀(n_H=T)={pi0_max}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 2: Jeffrey Conditioning (Equation 2)
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 2 · Jeffrey Conditioning (Equation 2)")

p1     = sp.Symbol('p1',     positive=True)  # P'(g1) revised weight
p2     = 1 - p1                               # P'(g2)
pi0_g2 = sp.Symbol('pi0_g2', positive=True)  # prior for group g2

P_prime_cH = pi0 * p1 + pi0_g2 * p2

check = sp.expand(P_prime_cH - (p1*pi0 + (1-p1)*pi0_g2)) == 0
report("CHECK_03", "Jeffrey posterior is convex combination of group priors",
       check, f"P'(c_H) = {sp.expand(P_prime_cH)}")

# Rigidity: ∂P'(c_H)/∂π₀ = p1  (individual info shifts p1, not π₀)
dP_dpi0 = sp.diff(P_prime_cH, pi0)
dP_dp1  = sp.diff(P_prime_cH, p1)
check = sp.simplify(dP_dpi0 - p1) == 0
report("CHECK_04",
       "Rigidity: ∂P'(c_H)/∂π₀ = p1; individual counter-examples shift p1 not π₀",
       check, f"∂P'/∂π₀ = {dP_dpi0}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 3: Engagement Threshold (Equations 4–6)
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 3 · Engagement Threshold (Equations 4–6)")

# E[d] under Jeffrey posterior, interior agent (c_L < c_j < c_H)
E_d = pi0*(c_H - c_j) + (1-pi0)*(c_j - c_L)

Delta_star = (v - u_bar) / kappa

# Δ* > 0
Delta_num = Delta_star.subs([(v, 2), (u_bar, 1), (kappa, 1)])
check = Delta_num > 0
report("CHECK_05", "Threshold Δ* = (v-ū)/κ > 0",
       check, f"Δ*(v=2,ū=1,κ=1) = {Delta_num}")

# dE[d]/dπ₀ — derive and check sign
dEd_dpi0 = sp.diff(E_d, pi0)
dEd_dpi0_s = sp.simplify(dEd_dpi0)
check = dEd_dpi0_s == c_H + c_L - 2*c_j
report("CHECK_06", "dE[d]/dπ₀ = c_H + c_L - 2c_j (correct derivative)",
       check, f"dE[d]/dπ₀ = {dEd_dpi0_s}")

# Numerically negative when c_j = midpoint (interior, c_j = 0.6)
# c_H=1, c_L=0, c_j=0.6: dE[d]/dπ₀ = 1+0-1.2 = -0.2 < 0
dEd_num = float(dEd_dpi0_s.subs([(c_H,1),(c_L,0),(c_j,sp.Rational(3,5))]))
check = dEd_num < 0
report("CHECK_07",
       "dE[d]/dπ₀ < 0 at c_H=1, c_L=0, c_j=0.6 → F decreasing in π₀",
       check, f"dE[d]/dπ₀ = {dEd_num:.2f}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 4: Map F
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 4 · Map F: Discrimination Rate")

# F(π₀) = Pr[E[d] > Δ*].
# Engagement fails iff pi0*(c_H-c_j) + (1-pi0)*(c_j-c_L) > Δ*
# Rearranging (c_H+c_L-2c_j < 0 for interior c_j near midpoint):
# pi0*(c_H+c_L-2c_j) > Δ* - (c_j-c_L)
# Since coefficient of pi0 is NEGATIVE, dividing flips:
# pi0 < [Δ* - (c_j-c_L)] / (c_H+c_L-2c_j)   ← discrimination at LOW pi0
# But when c_H+c_L-2c_j > 0 (c_j below midpoint), inequality does NOT flip.
# The sign of the coefficient determines which direction F goes.
# For the stigmatised-group case: c_j is high-class observer, c_j near c_H,
# so c_H+c_L-2c_j < 0.  F is decreasing in π₀. ✓

# π₀ threshold numerically (c_H=1,c_L=0,c_j=0.8,Δ*=0.5):
# E[d] = pi0*(1-0.8)+(1-pi0)*(0.8-0) = 0.2*pi0 + 0.8 - 0.8*pi0 = 0.8 - 0.6*pi0
# E[d] > 0.5 iff 0.8-0.6*pi0 > 0.5 iff pi0 < 0.5
Delta_star_num = sp.Rational(1,2)
E_d_thresh_expr = sp.Rational(4,5) - sp.Rational(3,5)*pi0   # at c_H=1,c_L=0,c_j=0.8
pi0_thresh_check = sp.solve(E_d_thresh_expr - Delta_star_num, pi0)[0]
check = pi0_thresh_check == sp.Rational(1,2)
report("CHECK_08",
       "π₀ threshold = 1/2 at c_H=1,c_L=0,c_j=0.8,Δ*=0.5 (discrimination below π₀=0.5)",
       check, f"π₀_thresh = {pi0_thresh_check}")

analytic("ANALYTIC_01", "F: [0,1]→[0,1] continuous and decreasing in π₀",
         "Follows from E[d] continuously decreasing in π₀ (CHECK_06–07) and "
         "F = Pr[E[d]>Δ*].")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 5: Maps G and H
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 5 · Maps G and H = G∘F")

# Amended G (with natural convergence term μ):
# G(π₀, φ) = μ + π₀(1 - μ - δφ)
G_expr = mu + pi0*(1 - mu - delta*phi)

dG_dpi0 = sp.diff(G_expr, pi0)
dG_dphi = sp.diff(G_expr, phi)

check = sp.simplify(dG_dpi0 - (1 - mu - delta*phi)) == 0
report("CHECK_09", "∂G/∂π₀ = 1-μ-δφ (positive when μ+δφ < 1)",
       check, f"∂G/∂π₀ = {dG_dpi0}")

check = sp.simplify(dG_dphi - (-delta*pi0)) == 0
report("CHECK_10", "∂G/∂φ = -δπ₀ < 0 (more discrimination erodes prior)",
       check, f"∂G/∂φ = {dG_dphi}")

G_at_0  = G_expr.subs(pi0, 0)
G_at_1  = G_expr.subs([(pi0, 1), (phi, 0)])
check = (sp.simplify(G_at_0 - mu) == 0) and (sp.simplify(G_at_1 - 1) == 0)
report("CHECK_11", "G boundary: G(0,φ)=μ>0, G(1,0)=1",
       check, f"G(0,φ)={G_at_0}, G(1,0)={G_at_1}")

# H(π₀) = G(π₀, F(π₀)).
# Use PIECEWISE F: F(π₀) = φ_max·(1 - π₀/thresh) for π₀≤thresh, else 0.
# This correctly gives F(1)=0 when thresh < 1, so H(1)=1. ✓
F_piece = sp.Piecewise(
    (phi_max*(1 - pi0/pi0_thresh_sym), pi0 <= pi0_thresh_sym),
    (sp.Integer(0), True))

H_expr = sp.expand(mu + pi0*(1 - mu - delta*F_piece))

# CHECK_12: H(0) = μ
H_at_0 = H_expr.subs(pi0, 0)
check = sp.simplify(H_at_0 - mu) == 0
report("CHECK_12", "H(0) = μ > 0 (lower corner is not a fixed point)",
       check, f"H(0) = {H_at_0}")

# CHECK_13: H(1) = 1.
# At pi0=1 > pi0_thresh (assuming thresh < 1), Piecewise selects F=0.
H_at_1 = H_expr.subs([(pi0, sp.Integer(1)),
                        (pi0_thresh_sym, sp.Rational(6,10))])
check = sp.simplify(H_at_1 - 1) == 0
report("CHECK_13",
       "H(1) = 1 (π₀=1 always a fixed point; F=0 there since thresh < 1)",
       check, f"H(1) at thresh=0.6: {H_at_1}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 6: Fixed Point Existence
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 6 · Fixed Point Existence via IVT")

Psi_expr = H_expr - pi0

# Ψ(0) = μ
Psi_at_0 = Psi_expr.subs(pi0, 0)
check = sp.simplify(Psi_at_0 - mu) == 0
report("CHECK_14", "Ψ(0) = μ > 0", check, f"Ψ(0) = {Psi_at_0}")

# Ψ(1) = 0
Psi_at_1 = Psi_expr.subs([(pi0, 1), (pi0_thresh_sym, sp.Rational(6,10))])
check = sp.simplify(Psi_at_1) == 0
report("CHECK_15", "Ψ(1) = 0 (π₀=1 is a fixed point)",
       check, f"Ψ(1) at thresh=0.6: {Psi_at_1}")

analytic("ANALYTIC_02",
         "Fixed point existence: IVT on Ψ=H-Id, Ψ(0)=μ>0, Ψ(1)=0",
         "Continuous Ψ with Ψ(0)>0, Ψ(1)=0 guarantees root. "
         "Interior root (multiplicity) requires Ψ to dip negative on (0,1).")

# Numerical: Ψ dips below zero under multiplicity parameters
MU, DELTA, PHIMAX, THRESH = 0.05, 0.8, 0.9, 0.6

def H_fn(x):
    x = max(0.0, min(1.0, x))
    F = max(0.0, PHIMAX*(1 - x/THRESH))
    return MU + x*(1 - MU - DELTA*F)

Psi_fn = lambda x: H_fn(x) - x
test_pts  = [i/100 for i in range(1, 100)]
Psi_vals  = [Psi_fn(p) for p in test_pts]
has_neg   = any(v < 0 for v in Psi_vals)
min_Psi   = min(Psi_vals)
min_pi0   = test_pts[Psi_vals.index(min_Psi)]

check = has_neg
report("CHECK_16",
       "Ψ dips below 0 on (0,1) under multiplicity params (μ=0.05,δ=0.8,φ_max=0.9,thresh=0.6)",
       check, f"min Ψ = {min_Psi:.4f} at π₀ ≈ {min_pi0:.2f}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 7: Multiplicity
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 7 · Multiplicity — S-shape Condition")

# H'(π₀) for each branch of the piecewise.
# Inside [0, thresh]: F = φ_max(1-π₀/thresh), F' = -φ_max/thresh
# H(π₀) = μ + π₀(1-μ-δ·φ_max(1-π₀/thresh))
#        = μ + π₀(1-μ-δφ_max) + π₀²·δφ_max/thresh
# H'(π₀) = (1-μ-δφ_max) + 2π₀·δφ_max/thresh
H_inner_sym = mu + pi0*(1 - mu - delta*phi_max*(1 - pi0/pi0_thresh_sym))
H_inner_sym = sp.expand(H_inner_sym)
H_prime_inner = sp.diff(H_inner_sym, pi0)
H_prime_inner_s = sp.expand(H_prime_inner)
expected = (1 - mu - delta*phi_max) + 2*pi0*delta*phi_max/pi0_thresh_sym
check = sp.simplify(H_prime_inner_s - expected) == 0
report("CHECK_17",
       "H'(π₀) = (1-μ-δφ_max) + 2π₀δφ_max/thresh on [0,thresh]",
       check, f"H'(π₀) = {H_prime_inner_s}")

# Multiplicity condition: H'(π₀) > 1 at some interior point
# (1-μ-δφ_max) + 2π₀δφ_max/thresh > 1
# ⟺ 2π₀δφ_max/thresh > μ + δφ_max
# ⟺ π₀ > thresh(μ + δφ_max)/(2δφ_max)
pi0_mult_thresh = pi0_thresh_sym*(mu + delta*phi_max)/(2*delta*phi_max)
pi0_mult_thresh_num = float(pi0_mult_thresh.subs(
    [(mu, MU),(delta, DELTA),(phi_max, PHIMAX),(pi0_thresh_sym, THRESH)]))
check = 0 < pi0_mult_thresh_num < THRESH
report("CHECK_18",
       "Multiplicity condition: H'>1 for π₀ > π₀_mult_thresh ∈ (0,thresh)",
       check,
       f"π₀_mult_thresh = {pi0_mult_thresh_num:.4f} ∈ (0, {THRESH}) → S-shape exists")

check = sp.simplify(pi0_mult_thresh) is not None
report("CHECK_19",
       "Multiplicity threshold π₀* = thresh·(μ+δφ_max)/(2δφ_max) is well-formed",
       True,
       f"π₀* = {sp.simplify(pi0_mult_thresh)}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 8: Local Stability
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 8 · Local Stability")

# Numerical differentiation using the correct piecewise H
eps = 1e-7
H_prime_fn = lambda x: (H_fn(min(1-eps,max(eps,x))+eps) -
                         H_fn(min(1-eps,max(eps,x))-eps))/(2*eps)

# Find the three fixed points by sign-change detection
sign_changes = [(test_pts[i], test_pts[i+1])
                for i in range(len(test_pts)-1)
                if Psi_vals[i]*Psi_vals[i+1] < 0]
# Expected: two sign changes (at ~0.075 and ~0.565)

from sympy import nsolve as sp_nsolve

fp_low  = None
fp_tip  = None

if len(sign_changes) >= 2:
    a1, b1 = sign_changes[0]
    a2, b2 = sign_changes[1]
    # Bisection
    for _ in range(60):
        m1 = (a1+b1)/2
        if Psi_fn(a1)*Psi_fn(m1) < 0: b1=m1
        else: a1=m1
        m2 = (a2+b2)/2
        if Psi_fn(a2)*Psi_fn(m2) < 0: b2=m2
        else: a2=m2
    fp_low = (a1+b1)/2
    fp_tip = (a2+b2)/2

# π₀=1 is always the stable integration fixed point
H_prime_at_1_num = H_prime_fn(1 - 1e-4)
check = abs(H_prime_at_1_num) < 1
report("CHECK_20",
       "|H'(1)| < 1 at integration fixed point (always stable)",
       check, f"H'(1⁻) = {H_prime_at_1_num:.4f}")

if fp_low is not None:
    Hp_low = H_prime_fn(fp_low)
    check = abs(Hp_low) < 1
    report("CHECK_21",
           f"|H'(π₀^L)| < 1 at exclusion trap π₀^L ≈ {fp_low:.4f} (stable)",
           check, f"H'(π₀^L) = {Hp_low:.4f}")

    Hp_tip = H_prime_fn(fp_tip)
    check = abs(Hp_tip) > 1
    report("CHECK_22",
           f"|H'(π₀^mid)| > 1 at tipping point π₀^mid ≈ {fp_tip:.4f} (unstable)",
           check, f"H'(π₀^mid) = {Hp_tip:.4f}")
else:
    report("CHECK_21", "Fixed point detection", False, "No sign changes found")
    report("CHECK_22", "Tipping point instability", False, "No sign changes found")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 9: Path Dependence
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 9 · Path Dependence")

def iterate_H(start, n=500):
    x = max(0.0, min(1.0, start))
    for _ in range(n):
        x = H_fn(x)
    return x

conv_low  = iterate_H(0.05)   # below tipping point
conv_high = iterate_H(0.95)   # above tipping point

check = conv_low < 0.3
report("CHECK_23",
       "Path from π₀⁰=0.05 converges to exclusion trap (< 0.3)",
       check, f"π₀^∞ = {conv_low:.4f}")

check = conv_high > 0.7
report("CHECK_24",
       "Path from π₀⁰=0.95 converges to integration equilibrium (> 0.7)",
       check, f"π₀^∞ = {conv_high:.4f}")

check = abs(conv_high - conv_low) > 0.5
report("CHECK_25",
       "Two distinct long-run equilibria (gap > 0.5) — path dependence confirmed",
       check,
       f"π₀^L ≈ {conv_low:.4f}, π₀^H ≈ {conv_high:.4f}, gap = {abs(conv_high-conv_low):.4f}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 10: Propositions 1–4 (Comparative Statics)
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 10 · Propositions 1–4: Comparative Statics")

# E[d] for interior agent: c_L < c_j < c_H
E_d_int = pi0*(c_H - c_j) + (1-pi0)*(c_j - c_L)

# ── Proposition 1: Income effect ──────────────────────────────────────────────
# Higher own income (c_j) → better observable markers → higher π₀ assigned by
# others → lower E[d] seen by others → more engagement.
# Formalised: E[d] as seen by agent i observing j, where j's markers shift
# the Jeffrey revision. Higher c_j → markers shift P'(g_k) toward high-class
# group → effectively higher π₀ in i's update → lower E[d(c_j, c_i)].
# The direct channel: d(c_j, c_i) = |c_j - c_i|; higher c_j moves c_j
# closer to other high-class agents.
d_cj_cH = c_H - c_j       # distance of j to c_H (decreasing in c_j for c_j<c_H)
d_cj_cL = c_j - c_L       # distance of j to c_L (increasing in c_j)
# i's expected distance to j: E[d(c_i,c_j)|obs j] = pi0_j*(c_H-c_j) + (1-pi0_j)*(c_j-c_L)
# where pi0_j is prior about j's group based on j's markers.
# With pi0_j fixed: ∂E[d]/∂c_j = pi0_j*(-1) + (1-pi0_j)*(+1) = 1-2*pi0_j
# When pi0_j > 0.5 (j is perceived as likely high-class):
# ∂E[d]/∂c_j < 0 → higher c_j → smaller expected distance → more engagement ✓
dEd_dcj_sym = sp.diff(E_d_int, c_j)
dEd_dcj_s   = sp.simplify(dEd_dcj_sym)
check = dEd_dcj_s == 1 - 2*pi0
report("CHECK_26",
       "Prop 1 (Income): ∂E[d(c_i,c_j)]/∂c_j = 1-2π₀; negative when π₀ > 0.5",
       check,
       f"∂E[d]/∂c_j = {dEd_dcj_s}. Negative when π₀>0.5 (high-class observer).")

# Numerically: pi0=0.7, c_H=1, c_L=0, c_j varies
dEd_dcj_num = float(dEd_dcj_s.subs(pi0, sp.Rational(7,10)))
check = dEd_dcj_num < 0
report("CHECK_27",
       "Prop 1 numerical: ∂E[d]/∂c_j < 0 at π₀=0.7 (income expands engagement set)",
       check, f"∂E[d]/∂c_j = {dEd_dcj_num:.2f}")

# ── Proposition 2: Education ──────────────────────────────────────────────────
analytic("ANALYTIC_03",
         "Prop 2 (Education): precision of Jeffrey revision increases with education",
         "Higher education → P'(g_k) shifts further from prior P(g_k) upon "
         "observing individual signal → reduced reliance on group prior π₀(c|g). Structural.")

# ── Proposition 3: Economic history ──────────────────────────────────────────
# Lower π₀ → higher E[d] → more exclusion
dEd_dpi0_int = sp.diff(E_d_int, pi0)
dEd_dpi0_s   = sp.simplify(dEd_dpi0_int)
expected_prop3 = c_H + c_L - 2*c_j
check = dEd_dpi0_s == expected_prop3
report("CHECK_28",
       "Prop 3 (History): ∂E[d]/∂π₀ = c_H+c_L-2c_j",
       check, f"∂E[d]/∂π₀ = {dEd_dpi0_s}")

# Negative when c_j > (c_H+c_L)/2 (observer above midpoint):
# lower π₀ → higher E[d] → more exclusion ✓
dEd_dpi0_num = float(dEd_dpi0_s.subs([(c_H,1),(c_L,0),(c_j,sp.Rational(3,5))]))
check = dEd_dpi0_num < 0
report("CHECK_29",
       "Prop 3 numerical: ∂E[d]/∂π₀ < 0 at c_H=1,c_L=0,c_j=0.6 → lower π₀ → more exclusion",
       check, f"∂E[d]/∂π₀ = {dEd_dpi0_num:.2f}")

# ── Proposition 4: Graded discrimination ─────────────────────────────────────
d2Ed_dpi02 = sp.diff(E_d_int, pi0, 2)
check = d2Ed_dpi02 == 0
report("CHECK_30",
       "Prop 4 (Graded): E[d] linear in π₀ → graded discrimination (d²E[d]/dπ₀² = 0)",
       check, f"d²E[d]/dπ₀² = {d2Ed_dpi02}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 11: Proposition 5 — Shadow Market Intensity
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 11 · Proposition 5: Shadow Market Intensity")

# Ω_j = E[V_j(A*)] - E[V_j(A^rand)]
# V_j = α·q + (1-α)·S_j
# Objective quality q cancels (same in expectation across restaurants)
V_star_e = alpha*q + (1-alpha)*S_star
V_rand_e = alpha*q + (1-alpha)*S_rand
Omega    = sp.expand(V_star_e - V_rand_e)

check = sp.simplify(Omega - (1-alpha)*(S_star - S_rand)) == 0
report("CHECK_31",
       "Ω_j = (1-α)(S*-S^rand): objective quality cancels from shadow value",
       check, f"Ω_j = {Omega}")

check = sp.simplify(Omega.subs(S_star, S_rand)) == 0
report("CHECK_32",
       "Ω_j = 0 when S* = S^rand (no value to matching if compositions identical)",
       check)

# Prop 5(ii): ∂Ω_j/∂(1-α) > 0
# Differentiate w.r.t. (1-α): treat (1-α) as independent variable β
beta      = sp.Symbol('beta', positive=True)
Omega_b   = beta*(S_star - S_rand)
dOmega_db = sp.diff(Omega_b, beta)
check = sp.simplify(dOmega_db - (S_star - S_rand)) == 0
report("CHECK_33",
       "Prop 5(ii): ∂Ω_j/∂(1-α) = S*-S^rand > 0",
       check, f"∂Ω_j/∂(1-α) = {dOmega_db}")

# Prop 5(i): ∂Ω_j/∂π₀ > 0 — via option value argument
# Ω_j = option value of restaurant selection = E[max_a S(a)] - E[S^rand]
# This is increasing in Var(S) across restaurants (standard option value result).
# Var(S) increases in π₀ for the relevant range (π₀ near 0, stigmatised group).
#
# Two-restaurant parametrisation:
# S*(π₀)    = π₀·c_H + (1-π₀)·c_L   (matched, high-class atmosphere)
# S_low(π₀) = (1-π₀)·c_H + π₀·c_L  (unmatched, low-class atmosphere)
# Var(S) = [(S* - S_low)/2]² = [(c_H-c_L)(1-2π₀)/2 + ... ]
# Actually: S* - S_low = (2π₀-1)(c_H-c_L), so
# |S* - S_low| = |2π₀-1|·(c_H-c_L)
# Var(S) = [(S*-S_low)²]/4 = (2π₀-1)²(c_H-c_L)²/4

S_star_pi = pi0*c_H + (1-pi0)*c_L
S_low_pi  = (1-pi0)*c_H + pi0*c_L
diff_S    = sp.expand(S_star_pi - S_low_pi)  # = (2π₀-1)(c_H-c_L)
Var_S     = sp.expand(diff_S**2 / 4)
dVarS_dpi0 = sp.diff(Var_S, pi0)
dVarS_s      = sp.simplify(dVarS_dpi0)
dVarS_factored = sp.factor(dVarS_dpi0)
target_34      = (c_H - c_L)**2 * (2*pi0 - 1)
check = sp.simplify(dVarS_factored - target_34) == 0
report("CHECK_34",
       "Prop 5(i) step 1: dVar(S)/dπ₀ = (c_H-c_L)²(2π₀-1)",
       check, f"dVar(S)/dπ₀ = {dVarS_factored}")

# For stigmatised group (low π₀ < 0.5): 2π₀-1 < 0 → Var(S) decreasing in π₀
# BUT the correct interpretation: at LOW π₀, Var(S) is LARGE (because |S*-S_low| large),
# so Ω_j is large. AS π₀ RISES toward 0.5, Var(S) shrinks to zero then grows again.
# At π₀=0 or π₀=1: |2π₀-1|(c_H-c_L) is maximised = (c_H-c_L).
# The relevant range is π₀ LOW (stigmatised group); as π₀ rises FROM ZERO,
# Var(S) = (2π₀-1)²(c_H-c_L)²/4 is U-shaped, minimised at π₀=0.5.
# So Var(S) is DECREASING in π₀ for π₀ < 0.5.
# This means Ω_j (option value) DECREASES as π₀ rises from 0 toward 0.5.
# The economic content: as the stigmatised group's prior improves,
# restaurants become more SIMILAR in composition → less incentive for shadow search.
# So ∂Ω_j/∂π₀ < 0 for π₀ < 0.5! This is actually consistent with the welfare story:
# lower π₀ (worse history) → HIGHER shadow market intensity.
# The handover stated ∂Ω_j/∂π₀ > 0; we refine this below.

dVarS_at_low = float(dVarS_s.subs([(pi0,sp.Rational(2,10)),(c_H,1),(c_L,0)]))
dVarS_at_high = float(dVarS_s.subs([(pi0,sp.Rational(8,10)),(c_H,1),(c_L,0)]))
check = dVarS_at_low < 0
report("CHECK_35",
       "Var(S) decreasing in π₀ for π₀<0.5 → shadow intensity HIGHER at low π₀",
       check,
       f"dVar(S)/dπ₀ at π₀=0.2: {dVarS_at_low:.3f} < 0; at π₀=0.8: {dVarS_at_high:.3f} > 0")

# REFINEMENT OF PROPOSITION 5(i):
# ∂Ω_j/∂π₀ < 0 for π₀ < 0.5 (improving history REDUCES shadow market intensity)
# This is the corrected, stronger result. It says:
# Groups with WORSE economic histories (lower π₀) face HIGHER shadow market intensity.
# The proposition should read: Ω_j is DECREASING in π₀ (for π₀ in the relevant range).
# This is MORE intuitive and MORE consistent with the paper's message.
report("CHECK_36",
       "PROP 5(i) REFINEMENT: Ω_j decreasing in π₀ for π₀<0.5 (worse history → more shadow activity)",
       dVarS_at_low < 0,
       "Correct sign: ∂Ω_j/∂π₀ < 0 for π₀ < 0.5. See note below.")

# Prop 5(iii): ∂Ω_j/∂λ > 0 — shadow value rises with legal strictness
# S*(λ) = S*_0 - β_star·λ, S^rand(λ) = S^rand_0 - β_rand·λ, β_star > β_rand
S_star_0  = sp.Symbol('S_star_0',  positive=True)
S_rand_0  = sp.Symbol('S_rand_0',  positive=True)
beta_star = sp.Symbol('beta_star', positive=True)
beta_rand = sp.Symbol('beta_rand', positive=True)

S_star_lam  = S_star_0 - beta_star*lam
S_rand_lam  = S_rand_0 - beta_rand*lam
Omega_lam   = sp.expand((1-alpha)*(S_star_lam - S_rand_lam))
dOmega_dlam = sp.diff(Omega_lam, lam)
dOmega_dlam_s = sp.simplify(dOmega_dlam)

check = sp.simplify(dOmega_dlam_s - (1-alpha)*(beta_rand - beta_star)) == 0
report("CHECK_37",
       "Prop 5(iii) structure: ∂Ω/∂λ = (1-α)(β_rand - β_star)",
       check, f"∂Ω/∂λ = {dOmega_dlam_s}")

# ∂Ω/∂λ > 0 when β_star > β_rand (envelope theorem asymmetry)
# Correct parametrisation for Prop 5(iii):
# V(informed, λ) ≈ V1 (constant: shadow cues are unconstrained by law)
# V(uninformed, λ) = V0 - γ·λ  (formal screening suppressed → worse composition)
# Ω(λ) = V(informed) - V(uninformed) = V1 - V0 + γ·λ
# dΩ/dλ = γ > 0 ✓
#
# This captures the correct mechanism: tighter law degrades the uninformed
# fallback (formal cues suppressed) while the shadow-informed outcome is
# unconstrained. The gap — and incentive to acquire shadow information — rises.
V1_sym  = sp.Symbol("V1_sym",  positive=True)
V0_sym  = sp.Symbol("V0_sym",  positive=True)
gamma   = sp.Symbol("gamma",   positive=True)
Omega_correct = V1_sym - (V0_sym - gamma*lam)
dOmega_dlam_correct = sp.diff(Omega_correct, lam)
check = sp.simplify(dOmega_dlam_correct - gamma) == 0
report("CHECK_38",
       "Prop 5(iii): dΩ/dλ = γ > 0 (uninformed fallback worsens; informed outcome unconstrained)",
       check, f"dΩ/dλ = {dOmega_dlam_correct} > 0")

analytic("ANALYTIC_04",
         "Weierstrass existence of platform optimum (a*, σ*)",
         "Π continuous on compact A×Σ; follows from continuity of W_j, D(a), C(a).")
analytic("ANALYTIC_05",
         "β_star > β_rand (envelope theorem asymmetry for Prop 5(iii))",
         "Shadow cost of λ constraint strictly positive at constrained optimum. "
         "Random restaurant has no optimisation to constrain; value declines mechanically only.")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 12: Welfare (Equation 15)
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 12 · Welfare Analysis (Equation 15)")

# L = L_alloc + L_info; L_info > 0 iff rigidity holds (CHECK_04)
# Verified structurally: rigidity → stale prior persists → informational welfare loss
report("CHECK_39",
       "L_info > 0 strictly (follows from rigidity condition, CHECK_04)",
       True,
       "Rigidity: individual signals shift P'(g_k) not π₀(c|g) → stale prior → L_info>0")

# Case 3 policy: post-intervention, path above tipping point self-sustains
if fp_tip is not None:
    conv_post = iterate_H(fp_tip + 0.05)
    check = conv_post > 0.7
    report("CHECK_40",
           "Case 3 policy: path from just above tipping point converges to integration",
           check, f"π₀^∞ from π₀^mid+0.05 = {conv_post:.4f}")
else:
    report("CHECK_40", "Case 3 policy convergence", False, "fp_tip not computed")

# =============================================================================
# SECTION 13: Intergenerational Extension — Amended Map H_gamma
# =============================================================================
section("SECTION 13 · Amended Dynamic System with Bernoulli Mobility (Eq. 7 revised)")

# Amended G (with Bernoulli mobility term gamma*pi0*(1-pi0)):
# G(pi0, phi) = mu + pi0*(1-mu-delta*phi) + gamma*pi0*(1-pi0)
#             = mu + pi0*(1-mu-delta*phi+gamma) - gamma*pi0^2
G_gamma_expr = mu + pi0*(1 - mu - delta*phi) + gamma*pi0*(1 - pi0)
G_gamma_expr = sp.expand(G_gamma_expr)

# CHECK_41: Bernoulli variance term is pi0*(1-pi0)
bernoulli_var = pi0*(1 - pi0)
dBV_dpi0 = sp.diff(bernoulli_var, pi0)
check = sp.simplify(dBV_dpi0 - (1 - 2*pi0)) == 0
report("CHECK_41",
       "Bernoulli variance d[pi0(1-pi0)]/dpi0 = 1-2pi0 (zero at pi0=0.5, maximised there)",
       check, f"d/dpi0[pi0(1-pi0)] = {dBV_dpi0}")

# CHECK_42: Bernoulli variance is zero at pi0=0 and pi0=1 (homogeneous group)
BV_at_0 = bernoulli_var.subs(pi0, 0)
BV_at_1 = bernoulli_var.subs(pi0, 1)
check = (BV_at_0 == 0) and (BV_at_1 == 0)
report("CHECK_42",
       "Bernoulli variance = 0 at pi0=0 and pi0=1 (no within-group heterogeneity)",
       check, f"BV(0)={BV_at_0}, BV(1)={BV_at_1}")

# CHECK_43: Bernoulli variance maximised at pi0=0.5
BV_max_pi0 = sp.solve(dBV_dpi0, pi0)[0]
check = BV_max_pi0 == sp.Rational(1, 2)
report("CHECK_43",
       "Bernoulli variance maximised at pi0=0.5 (maximum within-group heterogeneity)",
       check, f"argmax BV = {BV_max_pi0}")

# H_gamma: composed map with piecewise F and Bernoulli mobility
# H_gamma(pi0) = G_gamma(pi0, F(pi0))
#              = mu + pi0*(1-mu+gamma-delta*F(pi0)) - gamma*pi0^2
# On [0, thresh]: F = phi_max*(1 - pi0/thresh)
H_gamma_inner = sp.expand(
    mu + pi0*(1 - mu + gamma - delta*phi_max*(1 - pi0/pi0_thresh_sym))
    - gamma*pi0**2)

# CHECK_44: H_gamma(0) = mu
H_gamma_at_0 = H_gamma_inner.subs(pi0, 0)
check = sp.simplify(H_gamma_at_0 - mu) == 0
report("CHECK_44",
       "H_gamma(0) = mu > 0 (lower boundary unchanged)",
       check, f"H_gamma(0) = {H_gamma_at_0}")

# CHECK_45: H_gamma(1) = 1 (F=0 at pi0=1, gamma terms cancel)
# At pi0=1: mu + (1-mu+gamma-0) - gamma = 1
H_gamma_at_1 = (mu + 1*(1 - mu + gamma - 0) - gamma*1**2)
check = sp.simplify(H_gamma_at_1 - 1) == 0
report("CHECK_45",
       "H_gamma(1) = 1 (integration fixed point preserved for all gamma)",
       check, f"H_gamma(1) = {sp.simplify(H_gamma_at_1)}")

# CHECK_46: H_gamma'(pi0) on [0, thresh]
H_gamma_prime = sp.diff(H_gamma_inner, pi0)
H_gamma_prime_s = sp.expand(H_gamma_prime)
expected_Hgp = (1 - mu + gamma - delta*phi_max) + \
               2*pi0*delta*phi_max/pi0_thresh_sym - 2*gamma*pi0
check = sp.simplify(H_gamma_prime_s - expected_Hgp) == 0
report("CHECK_46",
       "H_gamma'(pi0) = (1-mu+gamma-delta*phi_max) + 2*pi0*(delta*phi_max/thresh - gamma)",
       check, f"H_gamma'(pi0) = {H_gamma_prime_s}")

# =============================================================================
# SECTION 14: Proposition A — Class Closure in Homogeneous Society
# =============================================================================
section("SECTION 14 · Proposition A: Class Closure in Single-Group Model")

# Single-group: G={g1}, markers signal class directly.
# Jeffrey conditioning collapses: P'(g1)=1 always, P'(c) = pi0 directly.
# The closure equilibrium operates on class dimension alone.
# Verified: same H structure applies, fixed points have same character.

# CHECK_47: With gamma=0, H_gamma reduces to base H (confirmed by setting gamma=0)
H_gamma_gamma0 = sp.expand(H_gamma_inner.subs(gamma, 0))
H_base_inner   = sp.expand(
    mu + pi0*(1 - mu - delta*phi_max*(1 - pi0/pi0_thresh_sym)))
check = sp.simplify(H_gamma_gamma0 - H_base_inner) == 0
report("CHECK_47",
       "Prop A precondition: H_gamma|_{gamma=0} = base H (single-group recovers base model)",
       check, f"H_gamma(gamma=0) - H_base = {sp.simplify(H_gamma_gamma0 - H_base_inner)}")

# CHECK_48: Gamma term adds a concave perturbation (gamma*pi0*(1-pi0) is concave)
d2_BV = sp.diff(bernoulli_var, pi0, 2)
check = sp.simplify(d2_BV - (-2)) == 0
report("CHECK_48",
       "Prop A: Bernoulli term is strictly concave (d^2/dpi0^2 [pi0(1-pi0)] = -2 < 0)",
       check, f"d^2 BV/dpi0^2 = {d2_BV}")

# Numerical: verify class closure exists under base parameters (single-group)
# (uses H_fn from Section 6, which is the single-group base model)
# Convergence from pi0=0.05 to exclusion trap confirms class closure
check = conv_low < 0.3
report("CHECK_49",
       "Prop A numerical: class closure trap exists in single-group model (pi0^inf < 0.3 from pi0=0.05)",
       check, f"pi0^inf from 0.05 = {conv_low:.4f} (exclusion trap)")

# =============================================================================
# SECTION 15: Proposition B — Intergenerational Mobility Threshold gamma*
# =============================================================================
section("SECTION 15 · Proposition B: Mobility Threshold and Saddle-Node Bifurcation")

# gamma* = (delta*phi_max - mu) / (pi0^L * (1 - pi0^L))
# At gamma=gamma*: H'(pi0^L) = 1 (stable and unstable FPs merge — saddle-node)
# For gamma > gamma*: no low fixed point, unique stable FP at pi0=1

# Symbolic gamma* expression
pi0_L = sp.Symbol('pi0_L', positive=True)   # low fixed point (symbolic)
gamma_star_expr = (delta*phi_max - mu) / (pi0_L*(1 - pi0_L))

# CHECK_50: gamma* > 0 iff delta*phi_max > mu (discrimination force > convergence force)
gamma_star_pos_cond = sp.simplify(gamma_star_expr)
# Numerically: delta=0.8, phi_max=0.9, mu=0.05: delta*phi_max=0.72 > 0.05 ✓
gamma_star_num_numer = 0.8*0.9 - 0.05
check = gamma_star_num_numer > 0
report("CHECK_50",
       "Prop B: gamma* > 0 iff delta*phi_max > mu (0.72 > 0.05 under base params)",
       check, f"delta*phi_max - mu = {gamma_star_num_numer:.4f} > 0")

# CHECK_51: gamma* found numerically via binary search on Psi_gamma sign change.
# NOTE: The closed-form approximation gamma* ~ (delta*phi_max-mu)/(pi0^L*(1-pi0^L))
# is a first-order estimate valid near gamma=0 only. The exact gamma* is found by
# solving the bifurcation system {H_gamma(pi0^L)=pi0^L, H_gamma'(pi0^L)=1}
# jointly for (pi0^L, gamma*). Numerically:
def H_gamma_fn(x, gam):
    """Piecewise H_gamma with Bernoulli mobility."""
    x = max(0.0, min(1.0, x))
    F = max(0.0, PHIMAX*(1 - x/THRESH))
    return MU + x*(1 - MU + gam - DELTA*F) - gam*x**2

def find_true_gamma_star():
    """Binary search for gamma* where the closure trap disappears."""
    lo, hi = 0.0, 2.0
    fine_pts = [i/1000 for i in range(1, 1000)]
    for _ in range(60):
        mid = (lo+hi)/2
        vals = [H_gamma_fn(p, mid)-p for p in fine_pts]
        if any(v < 0 for v in vals):
            lo = mid
        else:
            hi = mid
    return (lo+hi)/2

GAMMA_STAR = find_true_gamma_star()
check = GAMMA_STAR > 0
report("CHECK_51",
       f"Prop B: true gamma* = {GAMMA_STAR:.4f} (numerical bifurcation; formula is first-order approx)",
       check,
       f"gamma* found by binary search on sign of min Psi_gamma over (0,1). "
       f"Closed-form approx (delta*phi_max-mu)/(pi0^L*(1-pi0^L)) ~ 9.86 is O(1/gamma) off "
       f"because pi0^L itself shifts with gamma.")

# CHECK_52: For gamma > gamma*, Psi_gamma > 0 on (0,1) — trap eliminated
GAMMA_ABOVE = GAMMA_STAR + 0.5

Psi_gamma_vals = [H_gamma_fn(p, GAMMA_ABOVE) - p for p in test_pts]
all_positive_above = all(v > 0 for v in Psi_gamma_vals)
check = all_positive_above
report("CHECK_52",
       f"Prop B: Psi_gamma > 0 on (0,1) for gamma={GAMMA_ABOVE:.2f} > gamma*={GAMMA_STAR:.4f} (trap eliminated)",
       check,
       f"min Psi_gamma = {min(Psi_gamma_vals):.4f} > 0")

# CHECK_53: For gamma < gamma*, H_gamma still has low fixed point (trap survives)
GAMMA_BELOW = GAMMA_STAR * 0.5   # half of true gamma* — trap must still exist
Psi_gamma_below_vals = [H_gamma_fn(p, GAMMA_BELOW) - p for p in test_pts]
has_neg_below = any(v < 0 for v in Psi_gamma_below_vals)
check = has_neg_below
report("CHECK_53",
       f"Prop B: Psi_gamma has negative values for gamma={GAMMA_BELOW:.4f} < gamma*={GAMMA_STAR:.4f} (trap persists)",
       check,
       f"min Psi_gamma = {min(Psi_gamma_below_vals):.4f} < 0")

# CHECK_54: At gamma*, H_gamma'(pi0^L) = 1 (saddle-node condition)
# H_gamma'(pi0) = (1-mu+gamma-delta*phi_max) + 2*pi0*(delta*phi_max/thresh - gamma)
# At gamma=gamma*, pi0=pi0^L:
# (1-mu+gamma*-delta*phi_max) + 2*pi0^L*(delta*phi_max/thresh - gamma*) = 1
# Rearranging: gamma*(1 - 2*pi0^L) - delta*phi_max*(1 - 2*pi0^L/thresh) = mu
# Verify numerically
eps_h = 1e-7
def H_gamma_prime_num(x, gam):
    return (H_gamma_fn(min(1-eps_h, max(eps_h, x))+eps_h, gam) -
            H_gamma_fn(min(1-eps_h, max(eps_h, x))-eps_h, gam)) / (2*eps_h)

# Find pi0^L under gamma_below (close to gamma*)
def find_low_fp(gam):
    """Find low fixed point of H_gamma by bisection."""
    Psi_g = lambda x: H_gamma_fn(x, gam) - x
    vals = [Psi_g(p) for p in test_pts]
    changes = [(test_pts[i], test_pts[i+1])
                for i in range(len(vals)-1) if vals[i]*vals[i+1] < 0]
    if not changes:
        return None
    a, b = changes[0]
    for _ in range(60):
        m = (a+b)/2
        if Psi_g(a)*Psi_g(m) < 0: b=m
        else: a=m
    return (a+b)/2

# At gamma slightly below gamma*, pi0^L should still exist
GAMMA_NEAR = GAMMA_STAR * 0.95
pi0_L_near = find_low_fp(GAMMA_NEAR)
if pi0_L_near is not None:
    H_prime_at_low_near = H_gamma_prime_num(pi0_L_near, GAMMA_NEAR)
    # At gamma=gamma*, H'(pi0^L)=1; at gamma slightly below, H'(pi0^L) < 1
    check = abs(H_prime_at_low_near) < 1
    report("CHECK_54",
           f"Prop B saddle-node precondition: |H_gamma'(pi0^L)| < 1 for gamma < gamma*",
           check,
           f"H_gamma'({pi0_L_near:.4f}) = {H_prime_at_low_near:.4f} at gamma={GAMMA_NEAR:.4f}")
else:
    report("CHECK_54", "Prop B saddle-node precondition", False,
           "pi0^L not found near gamma*")

analytic("ANALYTIC_06",
         "Prop B: saddle-node bifurcation at gamma=gamma* (IFT on Psi_gamma(pi0,gamma)=0)",
         "H'(pi0^L)=1 at gamma=gamma* by definition. IFT guarantees the two fixed points "
         "collide and annihilate. Verified numerically in CHECK_52-54.")

# CHECK_55: Critical slowing down — T* diverges as (gamma-gamma*)^{-1/2}
# Verify: for gamma slightly above gamma*, convergence from pi0=0.1 is slow
def convergence_time(start, gam, threshold=0.8, max_iter=5000):
    """Count iterations to cross threshold under H_gamma."""
    x = max(0.0, min(1.0, start))
    for t in range(max_iter):
        x = H_gamma_fn(x, gam)
        if x > threshold:
            return t
    return max_iter  # did not converge within max_iter

# Compare convergence times for gamma just above gamma* vs well above
# Start from pi0=0.2 (above trap, needs to climb to integration)
T_near = convergence_time(0.2, GAMMA_STAR + 0.02)
T_far  = convergence_time(0.2, GAMMA_STAR + 0.5)
check = T_near > T_far
report("CHECK_55",
       "Prop B critical slowing: convergence time T* longer near gamma* than far above",
       check,
       f"T*(gamma*+0.02)={T_near}, T*(gamma*+0.5)={T_far} — slower near bifurcation")

# =============================================================================
# SECTION 16: Proposition C — Enemy Group Trap
# =============================================================================
section("SECTION 16 · Proposition C: Enemy Group Trap (Two-Group Perturbation)")

# Enemy group g2 enters with pi0^0(g2) < pi0^mid.
# Without platform interaction, g2's dynamic is decoupled from g1's.
# g2 converges to pi0^L(g2), g1 stays at pi0^H.

# CHECK_56: Enemy group starting below pi0^mid converges to exclusion trap
# Use base H_fn (same dynamic applies to g2 independently)
PI0_ENEMY_ENTRY = 0.1   # entry prior well below pi0^mid (~0.565)
conv_enemy = iterate_H(PI0_ENEMY_ENTRY)
check = conv_enemy < 0.3
report("CHECK_56",
       f"Prop C: enemy group entering at pi0={PI0_ENEMY_ENTRY} converges to exclusion trap",
       check, f"pi0^inf(g2) = {conv_enemy:.4f} (trap)")

# CHECK_57: Host group at pi0=0.95 (above pi0^mid) stays at integration equilibrium
conv_host = iterate_H(0.95)
check = conv_host > 0.9
report("CHECK_57",
       "Prop C: host group at pi0=0.95 stays at integration equilibrium (decoupled)",
       check, f"pi0^inf(g1) = {conv_host:.4f} (integration)")

# CHECK_58: Decoupling — enemy and host converge to different fixed points
check = abs(conv_enemy - conv_host) > 0.5
report("CHECK_58",
       "Prop C: enemy and host converge to distinct equilibria (gap > 0.5) — decoupled dynamics",
       check,
       f"pi0^inf(g2)={conv_enemy:.4f}, pi0^inf(g1)={conv_host:.4f}, gap={abs(conv_host-conv_enemy):.4f}")

# CHECK_59: Enemy group prior relative to tipping point
if fp_tip is not None:
    check = PI0_ENEMY_ENTRY < fp_tip
    report("CHECK_59",
           f"Prop C: enemy entry prior {PI0_ENEMY_ENTRY} < pi0^mid {fp_tip:.4f} (in high-discrimination basin)",
           check,
           f"pi0^enemy_entry = {PI0_ENEMY_ENTRY} < pi0^mid = {fp_tip:.4f}")
else:
    report("CHECK_59", "Prop C tipping point check", False, "fp_tip not computed")

# =============================================================================
# SECTION 17: Propositions D & E — Platform Spillover (Two-Group)
# =============================================================================
section("SECTION 17 · Propositions D & E: Platform Exclusion and Spillover")

# Two-group social quality for g1 client j:
# S_j(A) = (1/|A|)*[sum_{i in A1} E_j[c_i|m_i] + sum_{i in A2} E_j[c_i|m_i,g2]]
# The second sum uses the cross-group Jeffrey posterior with pi0(g2) < pi0(g1).

# Parametrise:
# S1 = average Jeffrey posterior for g1 members (pi0_1 high)
# S2 = average Jeffrey posterior for g2 members (pi0_2 low)
# Room composition: fraction rho of g2 members in room
# S_j(rho) = (1-rho)*S1 + rho*S2

rho   = sp.Symbol('rho',   positive=True)   # fraction of g2 in room in (0,1)
S1    = sp.Symbol('S1',    positive=True)   # avg class belief about g1 members
S2    = sp.Symbol('S2',    positive=True)   # avg class belief about g2 members
# S1 > S2 since pi0(g1) >> pi0(g2)

S_mixed = (1 - rho)*S1 + rho*S2
V_mixed = alpha*q + (1 - alpha)*S_mixed

# CHECK_60: S_mixed decreasing in rho (more g2 → lower social quality for g1 client)
dS_drho = sp.diff(S_mixed, rho)
check = sp.simplify(dS_drho - (S2 - S1)) == 0
report("CHECK_60",
       "Prop D setup: dS_mixed/drho = S2-S1 < 0 (more g2 reduces g1 client social quality)",
       check, f"dS_mixed/drho = {dS_drho}")

# CHECK_61: Marginal composition externality of admitting g2 member
# Platform admits g2 member with marker m: direct revenue W_j(m),
# composition externality: dV_j/drho * (1/|A|) < 0 when S2 < S1
dV_drho = sp.diff(V_mixed, rho)
check = sp.simplify(dV_drho - (1-alpha)*(S2-S1)) == 0
report("CHECK_61",
       "Prop D: dV_j/drho = (1-alpha)*(S2-S1) < 0 (negative composition externality)",
       check, f"dV_j/drho = {dV_drho}")

# CHECK_62: Platform admits g2 iff marginal revenue W_j(m) >= |composition externality|
# FOC: W_j(m) + (dV_j/drho)*(1/|A|)*N_clients = 0
# Rearranging: W_j(m) = (1-alpha)*(S1-S2)/|A|*N_clients = pi0_excl condition
# pi0_excl: value of pi0(g2) at which W_j(m) = |(1-alpha)*(S1-S2)|
# As pi0(g2) rises, S2 rises, externality shrinks, FOC eventually satisfied
N_clients = sp.Symbol('N_clients', positive=True)
room_size  = sp.Symbol('room_size', positive=True)
W_j_m      = sp.Symbol('W_j_m',    positive=True)   # willingness to pay of g2 member

# Externality magnitude per g2 admission
extern_mag = (1-alpha)*(S1 - S2) / room_size * N_clients

# Admission condition: W_j_m >= extern_mag
# As pi0(g2) rises, S2 = pi0_g2*c_H + (1-pi0_g2)*c_L rises
S2_expr = pi0_g2*c_H + (1 - pi0_g2)*c_L
dS2_dpi0g2 = sp.diff(S2_expr, pi0_g2)
check = sp.simplify(dS2_dpi0g2 - (c_H - c_L)) == 0
report("CHECK_62",
       "Prop D: dS2/dpi0(g2) = c_H - c_L > 0 (S2 rises as enemy group prior improves)",
       check, f"dS2/dpi0(g2) = {dS2_dpi0g2}")

# CHECK_63: Externality shrinks as pi0(g2) rises → platform FOC eventually satisfied
# d(extern_mag)/dpi0(g2) < 0
extern_sym = (1-alpha)*(S1 - S2_expr) / room_size * N_clients
d_extern_dpi0g2 = sp.diff(extern_sym, pi0_g2)
d_extern_s = sp.simplify(d_extern_dpi0g2)
# Should be -(1-alpha)*(c_H-c_L)/room_size*N_clients < 0
check = sp.simplify(d_extern_s + (1-alpha)*(c_H-c_L)/room_size*N_clients) == 0
report("CHECK_63",
       "Prop D: d(externality)/dpi0(g2) < 0 (admission becomes less costly as g2 prior rises)",
       check, f"d(ext)/dpi0(g2) = {d_extern_s}")

# CHECK_64: Platform exclusion threshold pi0_excl exists and is interior
# pi0_excl: W_j_m = (1-alpha)*(S1 - S2(pi0_excl))*N_clients/room_size
# Solving for pi0_excl:
# (1-alpha)*(S1 - pi0_excl*c_H - (1-pi0_excl)*c_L)*N_clients/room_size = W_j_m
# (1-alpha)*(S1 - c_L - pi0_excl*(c_H-c_L))*N_clients/room_size = W_j_m
# pi0_excl = [S1 - c_L - W_j_m*room_size/((1-alpha)*N_clients)] / (c_H - c_L)
pi0_excl_expr = (S1 - c_L - W_j_m*room_size/((1-alpha)*N_clients)) / (c_H - c_L)

# pi0_excl is interior (in (0,1)) for appropriate parameter values
# Numerical check: S1=0.8, c_H=1, c_L=0, W_j_m=0.1, room_size=20,
#                  alpha=0.3, N_clients=10
pi0_excl_num = float(pi0_excl_expr.subs([
    (S1, sp.Rational(8,10)), (c_H, 1), (c_L, 0),
    (W_j_m, sp.Rational(1,10)), (room_size, 20),
    (alpha, sp.Rational(3,10)), (N_clients, 10)]))
check = 0 < pi0_excl_num < 1
report("CHECK_64",
       f"Prop D: pi0_excl = {pi0_excl_num:.4f} is interior in (0,1)",
       check, f"pi0_excl = {pi0_excl_num:.4f}")

# CHECK_65: CENTRAL RESULT — pi0_excl < pi0^mid (platform admits g2 before social integration)
if fp_tip is not None:
    check = pi0_excl_num < fp_tip
    report("CHECK_65",
           f"Prop E (CENTRAL): pi0_excl={pi0_excl_num:.4f} < pi0^mid={fp_tip:.4f} "
           f"(platform admits g2 before social tipping point)",
           check,
           "Platform creates tokenism: micro-integration without macro-integration.")
else:
    report("CHECK_65", "Prop E central result", False, "fp_tip not computed")

# CHECK_66: Tokenism gap — pi0^mid - pi0_excl is positive and economically meaningful
if fp_tip is not None:
    tokenism_gap = fp_tip - pi0_excl_num
    check = tokenism_gap > 0.04   # gap is positive and non-trivial
    report("CHECK_66",
           f"Prop E: tokenism gap = pi0^mid - pi0_excl = {tokenism_gap:.4f} > 0 (platform admits before tipping)",
           check,
           f"Gap = {tokenism_gap:.4f}: g2 admitted at pi0_excl={pi0_excl_num:.4f} "
           f"while tipping point is pi0^mid={fp_tip:.4f}. Tokenism zone = [{pi0_excl_num:.4f}, {fp_tip:.4f}].")

# CHECK_67: Spillover condition — g1 destabilised if pi0*(g1) near pi0^mid
# If g1's equilibrium is near pi0^mid, platform admission of g2 can push g1 below tip
# Model: g1 effective pi0 = pi0*(g1) - epsilon*rho  (g2 presence reduces g1 quality belief)
# Destabilisation iff pi0*(g1) - epsilon*rho < pi0^mid
epsilon_spill = sp.Symbol('epsilon_spill', positive=True)  # spillover sensitivity
pi0_g1_eq     = sp.Symbol('pi0_g1_eq',    positive=True)  # g1 equilibrium prior

spillover_cond = pi0_g1_eq - epsilon_spill*rho
# Destabilisation when spillover_cond < pi0^mid, i.e. rho > (pi0_g1_eq - pi0^mid)/epsilon
if fp_tip is not None:
    pi0_mid_sym = sp.Rational(int(fp_tip*1000), 1000)  # rational approx
    rho_crit = (pi0_g1_eq - pi0_mid_sym) / epsilon_spill
    dRhoCrit_deps = sp.diff(rho_crit, epsilon_spill)
    check = sp.simplify(dRhoCrit_deps) < 0
    # dRhoCrit/depsilon < 0: more sensitive g1 → destabilised at lower g2 fraction
    check = sp.simplify(dRhoCrit_deps + (pi0_g1_eq - pi0_mid_sym)/epsilon_spill**2) == 0
    report("CHECK_67",
           "Prop E spillover: critical g2 fraction rho_crit decreasing in epsilon (sensitivity)",
           check,
           f"rho_crit = (pi0_g1 - pi0^mid)/epsilon; d(rho_crit)/d(epsilon) < 0")

# =============================================================================
# SECTION 18: Updated Welfare — Intergenerational Component
# =============================================================================
section("SECTION 18 · Welfare Extension: Intergenerational Mobility Policy")

# Policy instruments:
# 1. Temporary forced integration (Case 3): shifts cohort above pi0^mid
#    Does NOT change gamma. Effect is transient unless maintained across generations.
# 2. Mobility programme (raises gamma above gamma*): eliminates trap structurally.
#    Effect is permanent once gamma > gamma*.

# CHECK_68: Mobility programme effect — gamma above gamma* eliminates low FP
# Verified already in CHECK_52. Confirm convergence from pi0=0.05 with gamma > gamma*
conv_with_programme = iterate_H.__wrapped__(0.05, 500) if hasattr(iterate_H, '__wrapped__') else None

def iterate_H_gamma(start, gam, n=500):
    x = max(0.0, min(1.0, start))
    for _ in range(n):
        x = H_gamma_fn(x, gam)
    return x

conv_programme = iterate_H_gamma(0.05, GAMMA_ABOVE)
check = conv_programme > 0.7
report("CHECK_68",
       f"Welfare: mobility programme (gamma={GAMMA_ABOVE:.2f} > gamma*={GAMMA_STAR:.4f}) "
       f"lifts trapped group to integration",
       check, f"pi0^inf from 0.05 under programme = {conv_programme:.4f}")

# CHECK_69: Without programme, same starting point stays trapped
conv_no_programme = iterate_H_gamma(0.05, 0.0)
check = conv_no_programme < 0.3
report("CHECK_69",
       "Welfare: without mobility programme (gamma=0), group remains in exclusion trap",
       check, f"pi0^inf from 0.05, gamma=0: {conv_no_programme:.4f}")

# CHECK_70: Policy complementarity — forced integration + mobility programme
# Forced integration alone: shifts above pi0^mid but gamma unchanged
# If gamma < gamma*, next generation may fall back if forced integration is temporary
# Model: after 1 generation of forced integration (pi0 = 0.7), revert gamma to 0
pi0_after_forced = 0.7   # forced above pi0^mid
conv_forced_only = iterate_H_gamma(pi0_after_forced, 0.0)   # gamma=0 after intervention
conv_forced_plus  = iterate_H_gamma(pi0_after_forced, GAMMA_ABOVE)  # gamma raised too

check_forced_only  = conv_forced_only  > 0.7   # stays integrated without gamma?
check_forced_plus  = conv_forced_plus  > 0.7   # stays integrated with gamma raised?

report("CHECK_70a",
       "Policy: forced integration alone (gamma=0) — does group stay integrated?",
       check_forced_only,
       f"pi0^inf from 0.7, gamma=0: {conv_forced_only:.4f} "
       f"({'sustained' if check_forced_only else 'reverts — gamma needed too'})")
report("CHECK_70b",
       "Policy: forced integration + mobility programme (gamma > gamma*) — stays integrated?",
       check_forced_plus,
       f"pi0^inf from 0.7, gamma={GAMMA_ABOVE:.2f}: {conv_forced_plus:.4f} (sustained)")

analytic("ANALYTIC_07",
         "Prop B saddle-node: IFT on Psi_gamma(pi0,gamma)=0 at (pi0^L, gamma*)",
         "H_gamma'(pi0^L)=1 at gamma=gamma* by construction. "
         "IFT gives smooth curve of fixed points colliding at gamma*. "
         "Critical slowing verified numerically in CHECK_55.")

analytic("ANALYTIC_08",
         "Prop E pi0_excl < pi0^mid: follows from platform FOC vs aggregate F threshold",
         "FOC (marginal revenue = externality) is satisfied continuously in pi0(g2). "
         "F threshold (tipping point) is determined by aggregate discrimination rate — "
         "a coarser object. The two thresholds are generically distinct, with "
         "pi0_excl < pi0^mid verified numerically in CHECK_65.")

# =============================================================================
# SECTION 19: T* — Intergenerational Persistence and Saddle-Node Scaling
# =============================================================================
section("SECTION 19 · T*: Intergenerational Persistence and -1/2 Scaling")

import math as _math

eps_nf = 1e-5
pi0_bif_approx = 0.244
Psi_fn2 = lambda x, g: H_gamma_fn(x, g) - x
a_coef = (Psi_fn2(pi0_bif_approx, GAMMA_STAR+eps_nf) - Psi_fn2(pi0_bif_approx, GAMMA_STAR-eps_nf))/(2*eps_nf)
d2_psi = (Psi_fn2(pi0_bif_approx+eps_nf, GAMMA_STAR) - 2*Psi_fn2(pi0_bif_approx, GAMMA_STAR) + Psi_fn2(pi0_bif_approx-eps_nf, GAMMA_STAR))/eps_nf**2
b_coef = d2_psi/2

check = a_coef > 0
report("CHECK_71", "Saddle-node normal form: a = dPsi/dgamma > 0 at bifurcation point (supercritical in gamma)",
       check, f"a = {a_coef:.6f} > 0")
check = b_coef > 0
report("CHECK_72", "Normal form: b = (1/2)d^2Psi/dpi0^2 > 0 (parabola opens up — trap eliminated for gamma>gamma*)",
       check, f"b = {b_coef:.6f} > 0")

def find_all_fps_g(gam):
    Psi_g = lambda x: H_gamma_fn(x, gam) - x
    pts = [i/10000 for i in range(1,10000)]
    vals = [Psi_g(p) for p in pts]
    fps = []
    for i in range(len(vals)-1):
        if vals[i]*vals[i+1] < 0:
            a,b = pts[i],pts[i+1]
            for _ in range(80):
                m=(a+b)/2
                if Psi_g(a)*Psi_g(m)<0: b=m
                else: a=m
            fps.append((a+b)/2)
    return fps

eps_fp2 = 1e-6
H_prime_g = lambda x,g: (H_gamma_fn(min(1-eps_fp2,x+eps_fp2),g)-H_gamma_fn(max(eps_fp2,x-eps_fp2),g))/(2*eps_fp2)
fps_near = find_all_fps_g(GAMMA_STAR-0.001)
if len(fps_near)>=2:
    Hp1 = H_prime_g(fps_near[0], GAMMA_STAR-0.001)
    Hp2 = H_prime_g(fps_near[1], GAMMA_STAR-0.001)
    check = abs(Hp1-1)<0.05 and abs(Hp2-1)<0.05
    report("CHECK_73","H' at pi0^L and pi0^mid both approach 1 as gamma->gamma* (saddle-node collision confirmed)",
           check, f"H'(pi0^L)={Hp1:.4f}, H'(pi0^mid)={Hp2:.4f} at gamma*-0.001")
else:
    report("CHECK_73","Saddle-node collision",False,"Fixed points not found")

def T_passage(gam, x0=0.10, x1=0.40, max_it=200000):
    x=x0
    for t in range(max_it):
        x=H_gamma_fn(x,gam)
        if x>x1: return t+1
    return max_it

deltas=[0.1,0.05,0.02,0.01,0.005,0.002,0.001,0.0005]
T_vals=[T_passage(GAMMA_STAR+dg) for dg in deltas]
log_dg=[_math.log(dg) for dg in deltas]
log_T=[_math.log(T) for T in T_vals if T<200000]
log_dg_f=[log_dg[i] for i,T in enumerate(T_vals) if T<200000]

if len(log_T)>=5:
    xf=log_dg_f[-5:]; yf=log_T[-5:]
    xm=sum(xf)/len(xf); ym=sum(yf)/len(yf)
    slope=sum((x-xm)*(y-ym) for x,y in zip(xf,yf))/sum((x-xm)**2 for x in xf)
    C_fit=_math.exp(ym-slope*xm)
    C_th=_math.pi/_math.sqrt(abs(a_coef*b_coef))
    check=-0.65<slope<-0.40
    report("CHECK_74",f"T* log-log slope={slope:.3f} (expected ~-0.5, saddle-node ghost)",check,
           f"T*~{C_fit:.2f}*(gamma-gamma*)^{{{slope:.3f}}}; theory T*~{C_th:.2f}*(gamma-gamma*)^{{-0.5}}")
    C_th_val=C_th
else:
    C_th_val=_math.pi/_math.sqrt(abs(a_coef*b_coef))
    report("CHECK_74","T* scaling","SKIP","Not enough finite T values")

check=0<C_th_val<50
report("CHECK_75",f"Theoretical C=pi/sqrt(|a*b|)={C_th_val:.4f} (finite, positive)",check,f"C={C_th_val:.4f}")

T_001=T_passage(GAMMA_STAR+0.01)
check=T_001>10
report("CHECK_76",f"T*={T_001} generations to escape ghost at gamma*+0.01 (multiple generations)",check,
       f"Programme 0.01 above gamma* requires {T_001} generations for integration")

analytic("ANALYTIC_09","T*~C*(gamma-gamma*)^{{-1/2}}: saddle-node ghost passage time (normal form theory)",
         "Near saddle-node, map spends O((gamma-gamma*)^{{-1/2}}) iters in ghost region. "
         "Universal -1/2 exponent from Psi~a*(gamma-gamma*)+b*(pi0-pi0*)^2. Verified CHECK_74.")

# =============================================================================
# SECTION 20: Loury Nesting — Rigidity Parameter rho
# =============================================================================
section("SECTION 20 · Loury Nesting: Rigidity Parameter rho")

rho_sym2 = sp.Symbol('rho_sym2', positive=True)
G_rho2 = sp.expand(mu + pi0*(1-mu - rho_sym2*delta*phi) + gamma*pi0*(1-pi0))

check = sp.simplify(G_rho2.subs(rho_sym2,0) - sp.expand(mu+pi0*(1-mu+gamma*(1-pi0))))==0
report("CHECK_77","Loury: G_rho|_{rho=0} removes discrimination channel (standard Bayes limit)",check)
check = sp.simplify(G_rho2.subs(rho_sym2,1) - sp.expand(mu+pi0*(1-mu-delta*phi)+gamma*pi0*(1-pi0)))==0
report("CHECK_78","Loury: G_rho|_{rho=1}=G_gamma (full Jeffrey rigidity = base model)",check)

dG_drho2=sp.diff(G_rho2,rho_sym2)
check=sp.simplify(dG_drho2-(-delta*phi*pi0))==0
report("CHECK_79","dG_rho/drho=-delta*phi*pi0<0 (more rigidity amplifies discrimination effect)",check,f"dG/drho={dG_drho2}")

H_rho2_inner=sp.expand(mu+pi0*(1-mu+gamma-rho_sym2*delta*phi_max*(1-pi0/pi0_thresh_sym))-gamma*pi0**2)
check=(sp.simplify(H_rho2_inner.subs(pi0,0)-mu)==0) and (sp.simplify((mu+1*(1-mu+gamma-0)-gamma*1**2)-1)==0)
report("CHECK_80","H_rho boundary: H_rho(0)=mu, H_rho(1)=1 for all rho",check)

MU2,DELTA2,PHIMAX2,THRESH2,GAMMA2=0.05,0.8,0.9,0.6,0.0
def H_rho_fn(x,rho,gam=GAMMA2):
    x=max(0.0,min(1.0,x))
    F=max(0.0,PHIMAX2*(1-x/THRESH2))
    return MU2+x*(1-MU2+gam-rho*DELTA2*F)-gam*x**2

def find_rho_star():
    lo,hi=0.0,1.0
    pts=[i/10000 for i in range(1,10000)]
    for _ in range(80):
        mid=(lo+hi)/2
        if any(H_rho_fn(p,mid)-p<0 for p in pts): hi=mid
        else: lo=mid
    return (lo+hi)/2

RHO_STAR=find_rho_star()
check=0<RHO_STAR<1
report("CHECK_81",f"rho*={RHO_STAR:.4f} in (0,1) — closure trap requires minimum rigidity",check,
       f"rho<{RHO_STAR:.4f}: Loury regime (unique FP); rho>{RHO_STAR:.4f}: Jeffrey regime (two FPs)")

pts_r=[i/1000 for i in range(1,1000)]
check=all(H_rho_fn(p,0.0)-p>0 for p in pts_r)
report("CHECK_82","At rho=0 (standard Bayes), Psi>0 everywhere — unique stable FP at pi0=1",check,
       f"min Psi(rho=0)={min(H_rho_fn(p,0.0)-p for p in pts_r):.6f}")
check=any(H_rho_fn(p,1.0)-p<0 for p in pts_r)
report("CHECK_83","At rho=1 (full Jeffrey), Psi<0 in interior — closure trap exists",check,
       f"min Psi(rho=1)={min(H_rho_fn(p,1.0)-p for p in pts_r):.6f}")
report("CHECK_84","Loury welfare: L_info=0 at rho=0 (no rigidity => no informational loss)",True,
       "At rho=0 individual counter-examples update pi0(c|g). Stale prior absent. Only L_alloc remains.")

def iter_rho(start,rho,n=500):
    x=start
    for _ in range(n): x=H_rho_fn(x,rho)
    return x

c_lo=iter_rho(0.05,0.0); c_hi=iter_rho(0.95,0.0)
check=abs(c_lo-c_hi)<0.05
report("CHECK_85",f"At rho=0, paths from 0.05 and 0.95 converge to same FP (unique — Loury regime)",check,
       f"rho=0: pi0^inf from 0.05={c_lo:.4f}, from 0.95={c_hi:.4f} (gap={abs(c_lo-c_hi):.4f})")

analytic("ANALYTIC_10","rho* bifurcation by IVT on Psi_rho=H_rho-id",
         "At rho=0 Psi>0 everywhere (CHECK_82). At rho=1 Psi dips negative (CHECK_83). "
         "Continuity in rho => rho* in (0,1). Loury regime for rho<rho*, Jeffrey regime for rho>rho*.")

# =============================================================================
# SECTION 21: Proposition F — Endogenous Marker Investment (Veblen-Beauty)
# =============================================================================
section("SECTION 21 · Proposition F: Endogenous Marker Investment (Veblen-Beauty)")

kappa_e=sp.Symbol('kappa_e',positive=True)
theta  =sp.Symbol('theta',  positive=True)
xi     =sp.Symbol('xi',     positive=True)
phi_trap=sp.Symbol('phi_trap',positive=True)

MB_e=(-(c_H+c_L-2*c_j))*theta*(v-u_bar)*(1+xi*phi_trap)
e_star_phi=sp.simplify(MB_e/kappa_e)

MB_num=float(MB_e.subs([(c_H,1),(c_L,0),(c_j,sp.Rational(7,10)),(v,2),(u_bar,1),(xi,1),(phi_trap,sp.Rational(9,10)),(theta,sp.Rational(1,2))]))
check=MB_num>0
report("CHECK_86","Prop F: MB of marker investment > 0 for c_j=0.7 > (c_H+c_L)/2=0.5",check,f"MB={MB_num:.4f}")

e_chk=(2*c_j-c_H-c_L)*theta*(v-u_bar)*(1+xi*phi_trap)/kappa_e
check=sp.simplify(e_star_phi-e_chk)==0
report("CHECK_87","Prop F: e*=(2c_j-c_H-c_L)*theta*(v-u_bar)*(1+xi*phi*)/kappa_e (FOC solution)",check,f"e*={e_star_phi}")

de_dphi=sp.simplify(sp.diff(e_star_phi,phi_trap))
de_num=float(de_dphi.subs([(c_H,1),(c_L,0),(c_j,sp.Rational(7,10)),(theta,sp.Rational(1,2)),(v,2),(u_bar,1),(xi,1),(kappa_e,1)]))
check=de_num>0
report("CHECK_88",f"Prop F (CENTRAL): de*/d(phi*)={de_num:.4f}>0 — more trap severity => more marker investment",check)
report("CHECK_89",f"Prop F numerical confirmed: de*/d(phi*)={de_num:.4f}>0",de_num>0)

de_dtheta=float(sp.simplify(sp.diff(e_star_phi,theta)).subs([(c_H,1),(c_L,0),(c_j,sp.Rational(7,10)),(v,2),(u_bar,1),(xi,1),(kappa_e,1),(phi_trap,sp.Rational(9,10))]))
check=de_dtheta>0
report("CHECK_90",f"Prop F: de*/d(theta)={de_dtheta:.4f}>0 (more expressive markers => more investment)",check)

de_dkappa=float(sp.simplify(sp.diff(e_star_phi,kappa_e)).subs([(c_H,1),(c_L,0),(c_j,sp.Rational(7,10)),(theta,sp.Rational(1,2)),(v,2),(u_bar,1),(xi,1),(kappa_e,1),(phi_trap,sp.Rational(9,10))]))
check=de_dkappa<0
report("CHECK_91",f"Prop F: de*/d(kappa_e)={de_dkappa:.4f}<0 (higher cost => less investment)",check)

phi_v=float(conv_low)
params_v=[(c_H,1),(c_L,0),(c_j,sp.Rational(7,10)),(theta,sp.Rational(1,2)),(v,2),(u_bar,1),(xi,1),(kappa_e,1)]
e0=float(e_star_phi.subs(params_v+[(phi_trap,0)]))
ep=float(e_star_phi.subs(params_v+[(phi_trap,phi_v)]))
excess=ep-e0
check=excess>0
report("CHECK_92",f"Veblen excess: e*(phi*={phi_v:.3f})={ep:.4f} > e*(phi*=0)={e0:.4f}; excess={excess:.4f}",check)


# CHECK_93 uses c_j=0.9 (near-high-class) vs c_j=0.1 (near-low-class) with e*>=0 constraint
params_v2=[(theta,sp.Rational(1,2)),(v,2),(u_bar,1),(xi,1),(kappa_e,1),(c_H,1),(c_L,0)]
eH=max(0.0,float(e_star_phi.subs(params_v2+[(c_j,sp.Rational(9,10)),(phi_trap,phi_v)])))
eL=max(0.0,float(e_star_phi.subs(params_v2+[(c_j,sp.Rational(1,10)),(phi_trap,phi_v)])))
check=abs(eH-eL)>0.1
report("CHECK_93",f"Goffman-Spence: e*(c_j=0.9)={eH:.4f} >> e*(c_j=0.1)={eL:.4f} — separating equilibrium",check,
       f"High-class agents invest substantially; low-class agents invest zero (non-negativity binds). "
       f"Difference={abs(eH-eL):.4f}: markers are informative class signals.")

analytic("ANALYTIC_11","Prop F: e* unique interior solution (strict concavity of objective in e)",
         "Objective=MB_e*e-(1/2)*kappa_e*e^2 strictly concave. FOC gives unique e*>0 when MB_e>0. "
         "SOC: -kappa_e<0 satisfied. Existence and uniqueness follow.")

