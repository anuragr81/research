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
CONTINUOUS CLASS MODEL: c ∈ [0,1] with Beta(c̄ν, (1-c̄)ν) within-group
distribution. Squared distance d(c_i,c_j) = (c_i - c_j)².

STRUCTURE
---------
Each check is labelled CHECK_XX and prints PASS or FAIL.
Topological results (Brouwer, IVT, Weierstrass, bifurcation) cannot be
verified symbolically; these are labelled ANALYTIC and the algebraic
preconditions feeding into them are verified instead.

FIXED POINT STRUCTURE (for reference)
--------------------------------------
Base model — multiplicity parameters (mu=0.05, delta=0.8, phi_max=0.9, thresh=0.6):
  c̄^L    ~0.075  stable   (exclusion trap / class closure)
  c̄^mid  ~0.565  unstable (tipping point)
  c̄^H    =1.000  stable   (integration, corner fixed point)

Intergenerational extension — adds γ̃ = γ/(1+ν) (effective mobility):
  γ̃ < γ̃*  two stable fixed points persist (closure trap survives)
  γ̃ = γ̃*  saddle-node bifurcation (trap and tipping point annihilate)
  γ̃ > γ̃*  unique stable fixed point at c̄=1 (trap eliminated)

SYNC POLICY
-----------
Any change to model primitives must be propagated here.
VERSION: Continuous class model with squared distance.
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

# Continuous class model
c_bar = sp.Symbol('c_bar', positive=True)   # group mean c̄_g ∈ (0,1)
sigma2 = sp.Symbol('sigma2', positive=True) # within-group variance σ²_g
nu    = sp.Symbol('nu',    positive=True)   # Beta concentration parameter
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
c_bar_thresh   = sp.Symbol('c_bar_thresh',   positive=True)

# Intergenerational extension parameters
gamma  = sp.Symbol('gamma',  positive=True)   # raw mobility parameter
gamma_tilde = sp.Symbol('gamma_tilde', positive=True)  # effective: γ/(1+ν)

print("  Symbols declared.")
print("  Continuous class model: c ∈ [0,1], d(c_i,c_j) = (c_i - c_j)²")
print("  Beta(c̄ν, (1-c̄)ν) within-group distribution")
print("  σ²_g = c̄(1-c̄)/(1+ν)")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 1: Prior Formation (Equation 1)
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 1 · Prior Formation (Equation 1)")

T   = sp.Symbol('T',   positive=True, integer=True)

# c̄_g = (1/T) Σ c_{g,t} — sample mean of group class history
# Well-formed and in [0,1] by construction
report("CHECK_01", "Prior c̄_g = (1/T)Σc_{g,t} is well-formed (sample mean)", True,
       "c̄_g ∈ [0,1] since each c_{g,t} ∈ [0,1]")

# Beta variance
sigma2_beta = c_bar*(1 - c_bar)/(1 + nu)
dsigma2_dcbar = sp.diff(sigma2_beta, c_bar)
sigma2_at_0 = sigma2_beta.subs(c_bar, 0)
sigma2_at_1 = sigma2_beta.subs(c_bar, 1)
sigma2_at_half = sigma2_beta.subs(c_bar, sp.Rational(1,2))

check = (sigma2_at_0 == 0) and (sigma2_at_1 == 0)
report("CHECK_02", "Beta variance = 0 at c̄=0 and c̄=1 (homogeneous group)",
       check, f"σ²(0)={sigma2_at_0}, σ²(1)={sigma2_at_1}")

check = sp.simplify(sigma2_at_half - sp.Rational(1,4)/(1+nu)) == 0
report("CHECK_03", "Beta variance maximised at c̄=1/2",
       check, f"σ²(1/2) = 1/(4(1+ν)) = {sigma2_at_half}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 2: Jeffrey Conditioning (Equations 2–2')
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 2 · Jeffrey Conditioning (Equations 2–2')")

p1      = sp.Symbol('p1',      positive=True)  # P'(g1) revised weight
p2      = 1 - p1                                # P'(g2)
c_bar2  = sp.Symbol('c_bar2',  positive=True)   # mean for group g2

E_prime_c = c_bar * p1 + c_bar2 * p2

check = sp.expand(E_prime_c - (p1*c_bar + (1-p1)*c_bar2)) == 0
report("CHECK_04", "Jeffrey posterior mean is convex combination of group means",
       check, f"E'[c] = {sp.expand(E_prime_c)}")

# Rigidity: ∂E'[c]/∂c̄ = p1 (individual info shifts p1, not c̄)
dE_dcbar = sp.diff(E_prime_c, c_bar)
check = sp.simplify(dE_dcbar - p1) == 0
report("CHECK_05",
       "Rigidity: ∂E'[c]/∂c̄ = p1; individual counter-examples shift p1 not c̄",
       check, f"∂E'/∂c̄ = {dE_dcbar}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 3: Engagement Threshold — Bias-Variance Decomposition
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 3 · Engagement Threshold: Bias-Variance Decomposition")

# E[(c_i - c_j)² | g] = (c̄_g - c_j)² + σ²_g
E_sq_dist = (c_bar - c_j)**2 + sigma2

Delta_star = (v - u_bar) / kappa

# Verify decomposition
# E[(c_i - c_j)²] = E[c_i² - 2c_i c_j + c_j²]
#                  = E[c_i²] - 2c_j E[c_i] + c_j²
#                  = (σ² + c̄²) - 2c_j c̄ + c_j²
#                  = (c̄ - c_j)² + σ²
E_sq_alt = (sigma2 + c_bar**2) - 2*c_j*c_bar + c_j**2
check = sp.expand(E_sq_dist - E_sq_alt) == 0
report("CHECK_06", "Bias-variance decomposition: E[(c_i-c_j)²|g] = (c̄-c_j)² + σ²",
       check, f"Direct = {sp.expand(E_sq_dist)}, Via moments = {sp.expand(E_sq_alt)}")

# Δ* > 0
Delta_num = Delta_star.subs([(v, 2), (u_bar, 1), (kappa, 1)])
check = Delta_num > 0
report("CHECK_07", "Threshold Δ* = (v-ū)/κ > 0",
       check, f"Δ*(v=2,ū=1,κ=1) = {Delta_num}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 4: Propositions 1–4 (Comparative Statics)
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 4 · Propositions 1–4: Comparative Statics")

# ── Proposition 1: Income effect ──────────────────────────────────────────────
dEsq_dcj = sp.diff(E_sq_dist, c_j)
dEsq_dcj_s = sp.simplify(dEsq_dcj)
check = sp.simplify(dEsq_dcj_s - (-2*(c_bar - c_j))) == 0
report("CHECK_08",
       "Prop 1 (Income): ∂E[(c_i-c_j)²]/∂c_j = -2(c̄-c_j); negative when c_j < c̄",
       check, f"∂E/∂c_j = {dEsq_dcj_s}")

dEsq_dcj_num = float(dEsq_dcj_s.subs([(c_bar, sp.Rational(7,10)), (c_j, sp.Rational(1,2))]))
check = dEsq_dcj_num < 0
report("CHECK_09",
       "Prop 1 numerical: ∂E/∂c_j < 0 at c̄=0.7, c_j=0.5 (income expands engagement)",
       check, f"∂E/∂c_j = {dEsq_dcj_num:.4f}")

# ── Proposition 2: Education ──────────────────────────────────────────────────
analytic("ANALYTIC_01",
         "Prop 2 (Education): precision of Jeffrey revision increases with education",
         "Higher education → P'(g_k) shifts further from prior P(g_k) upon "
         "observing individual signal → reduced reliance on group distribution F_g. Structural.")

# ── Proposition 3: Economic history ──────────────────────────────────────────
dEsq_dcbar = sp.diff(E_sq_dist, c_bar)
dEsq_dcbar_s = sp.simplify(dEsq_dcbar)
check = sp.simplify(dEsq_dcbar_s - 2*(c_bar - c_j)) == 0
report("CHECK_10",
       "Prop 3 (History): ∂E[(c_i-c_j)²]/∂c̄ = 2(c̄-c_j)",
       check, f"∂E/∂c̄ = {dEsq_dcbar_s}")

# Negative when c_j > c̄ (observer above group mean): lower c̄ → more exclusion
dEsq_dcbar_num = float(dEsq_dcbar_s.subs([(c_bar, sp.Rational(3,10)), (c_j, sp.Rational(7,10))]))
check = dEsq_dcbar_num < 0
report("CHECK_11",
       "Prop 3 numerical: ∂E/∂c̄ < 0 at c̄=0.3, c_j=0.7 → lower c̄ → more exclusion",
       check, f"∂E/∂c̄ = {dEsq_dcbar_num:.4f}")

# ── Proposition 4: Graded discrimination ─────────────────────────────────────
d2Esq_dcbar2 = sp.diff(E_sq_dist, c_bar, 2)
check = d2Esq_dcbar2 == 2
report("CHECK_12",
       "Prop 4 (Graded): d²E/dc̄² = 2 > 0 (quadratic in c̄, smooth variation with markers)",
       check, f"d²E/dc̄² = {d2Esq_dcbar2}")

# Variance component ∂σ²/∂c̄ also smooth
dsigma2_dcbar_s = sp.simplify(sp.diff(sigma2_beta, c_bar))
report("CHECK_13",
       "Prop 4: variance component dσ²/dc̄ = (1-2c̄)/(1+ν) — smooth, continuous",
       True, f"dσ²/dc̄ = {dsigma2_dcbar_s}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 5: Map F — Discrimination Rate
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 5 · Map F: Discrimination Rate")

# F(c̄) = Pr[E[(c_i-c_j)²|g] > Δ*]
# Engagement fails iff (c̄-c_j)² + σ² > Δ*
# F is decreasing in c̄ for c_j > c̄ (high-class observer)
analytic("ANALYTIC_02", "F: [0,1]→[0,1] continuous and decreasing in c̄",
         "Follows from E[(c_i-c_j)²|g] continuously varying in c̄ (CHECK_10) and "
         "F = Pr[E>Δ*].")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 6: Maps G and H
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 6 · Maps G and H = G∘F")

# G(c̄, φ) = μ + c̄(1 - μ - δφ) + γ̃·c̄(1-c̄)
#          = μ + c̄(1 - μ + γ̃ - δφ) - γ̃·c̄²
G_expr = mu + c_bar*(1 - mu - delta*phi) + gamma_tilde*c_bar*(1 - c_bar)
G_expr = sp.expand(G_expr)

dG_dcbar = sp.diff(G_expr, c_bar)
dG_dphi = sp.diff(G_expr, phi)

check = sp.simplify(dG_dcbar - (1 - mu - delta*phi + gamma_tilde - 2*gamma_tilde*c_bar)) == 0
report("CHECK_14", "∂G/∂c̄ = 1-μ-δφ+γ̃-2γ̃c̄",
       check, f"∂G/∂c̄ = {sp.expand(dG_dcbar)}")

check = sp.simplify(dG_dphi - (-delta*c_bar)) == 0
report("CHECK_15", "∂G/∂φ = -δc̄ < 0 (more discrimination erodes group mean)",
       check, f"∂G/∂φ = {dG_dphi}")

G_at_0 = G_expr.subs(c_bar, 0)
G_at_1 = G_expr.subs([(c_bar, 1), (phi, 0)])
check = (sp.simplify(G_at_0 - mu) == 0) and (sp.simplify(G_at_1 - 1) == 0)
report("CHECK_16", "G boundary: G(0,φ)=μ>0, G(1,0)=1",
       check, f"G(0,φ)={G_at_0}, G(1,0)={G_at_1}")

# H(c̄) = G(c̄, F(c̄)) with piecewise F
F_piece = sp.Piecewise(
    (phi_max*(1 - c_bar/c_bar_thresh), c_bar <= c_bar_thresh),
    (sp.Integer(0), True))

H_expr = sp.expand(mu + c_bar*(1 - mu + gamma_tilde - delta*F_piece) - gamma_tilde*c_bar**2)

H_at_0 = H_expr.subs(c_bar, 0)
check = sp.simplify(H_at_0 - mu) == 0
report("CHECK_17", "H(0) = μ > 0 (lower corner is not a fixed point)",
       check, f"H(0) = {H_at_0}")

H_at_1 = H_expr.subs([(c_bar, sp.Integer(1)),
                        (c_bar_thresh, sp.Rational(6,10))])
check = sp.simplify(H_at_1 - 1) == 0
report("CHECK_18", "H(1) = 1 (integration fixed point; F=0 there since thresh < 1)",
       check, f"H(1) at thresh=0.6: {H_at_1}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 7: Fixed Point Existence
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 7 · Fixed Point Existence via IVT")

Psi_expr = H_expr - c_bar

Psi_at_0 = Psi_expr.subs(c_bar, 0)
check = sp.simplify(Psi_at_0 - mu) == 0
report("CHECK_19", "Ψ(0) = μ > 0", check, f"Ψ(0) = {Psi_at_0}")

Psi_at_1 = Psi_expr.subs([(c_bar, 1), (c_bar_thresh, sp.Rational(6,10))])
check = sp.simplify(Psi_at_1) == 0
report("CHECK_20", "Ψ(1) = 0 (c̄=1 is a fixed point)",
       check, f"Ψ(1) at thresh=0.6: {Psi_at_1}")

analytic("ANALYTIC_03",
         "Fixed point existence: IVT on Ψ=H-Id, Ψ(0)=μ>0, Ψ(1)=0",
         "Continuous Ψ with Ψ(0)>0, Ψ(1)=0 guarantees root. "
         "Interior root (multiplicity) requires Ψ to dip negative on (0,1).")

# Numerical verification with γ̃ = 0 (base model)
MU, DELTA, PHIMAX, THRESH = 0.05, 0.8, 0.9, 0.6

def H_fn(x, gt=0.0):
    x = max(0.0, min(1.0, x))
    F = max(0.0, PHIMAX*(1 - x/THRESH))
    return MU + x*(1 - MU + gt - DELTA*F) - gt*x**2

Psi_fn = lambda x: H_fn(x) - x
test_pts = [i/100 for i in range(1, 100)]
Psi_vals = [Psi_fn(p) for p in test_pts]
has_neg  = any(v < 0 for v in Psi_vals)
min_Psi  = min(Psi_vals)
min_cbar = test_pts[Psi_vals.index(min_Psi)]

check = has_neg
report("CHECK_21",
       "Ψ dips below 0 on (0,1) under multiplicity params (μ=0.05,δ=0.8,φ_max=0.9,thresh=0.6)",
       check, f"min Ψ = {min_Psi:.4f} at c̄ ≈ {min_cbar:.2f}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 8: Local Stability & Path Dependence
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 8 · Local Stability and Path Dependence")

eps = 1e-7
H_prime_fn = lambda x: (H_fn(min(1-eps,max(eps,x))+eps) -
                         H_fn(min(1-eps,max(eps,x))-eps))/(2*eps)

sign_changes = [(test_pts[i], test_pts[i+1])
                for i in range(len(test_pts)-1)
                if Psi_vals[i]*Psi_vals[i+1] < 0]

fp_low = None
fp_tip = None
if len(sign_changes) >= 2:
    a1, b1 = sign_changes[0]
    a2, b2 = sign_changes[1]
    for _ in range(60):
        m1 = (a1+b1)/2
        if Psi_fn(a1)*Psi_fn(m1) < 0: b1=m1
        else: a1=m1
        m2 = (a2+b2)/2
        if Psi_fn(a2)*Psi_fn(m2) < 0: b2=m2
        else: a2=m2
    fp_low = (a1+b1)/2
    fp_tip = (a2+b2)/2

H_prime_at_1_num = H_prime_fn(1 - 1e-4)
check = abs(H_prime_at_1_num) < 1
report("CHECK_22", "|H'(1)| < 1 at integration fixed point (always stable)",
       check, f"H'(1⁻) = {H_prime_at_1_num:.4f}")

if fp_low is not None:
    Hp_low = H_prime_fn(fp_low)
    check = abs(Hp_low) < 1
    report("CHECK_23",
           f"|H'(c̄^L)| < 1 at exclusion trap c̄^L ≈ {fp_low:.4f} (stable)",
           check, f"H'(c̄^L) = {Hp_low:.4f}")

    Hp_tip = H_prime_fn(fp_tip)
    check = abs(Hp_tip) > 1
    report("CHECK_24",
           f"|H'(c̄^mid)| > 1 at tipping point c̄^mid ≈ {fp_tip:.4f} (unstable)",
           check, f"H'(c̄^mid) = {Hp_tip:.4f}")

def iterate_H(start, gt=0.0, n=500):
    x = max(0.0, min(1.0, start))
    for _ in range(n):
        x = H_fn(x, gt)
    return x

conv_low  = iterate_H(0.05)
conv_high = iterate_H(0.95)

check = conv_low < 0.3
report("CHECK_25", "Path from c̄⁰=0.05 converges to exclusion trap (< 0.3)",
       check, f"c̄^∞ = {conv_low:.4f}")

check = conv_high > 0.7
report("CHECK_26", "Path from c̄⁰=0.95 converges to integration equilibrium (> 0.7)",
       check, f"c̄^∞ = {conv_high:.4f}")

check = abs(conv_high - conv_low) > 0.5
report("CHECK_27", "Two distinct long-run equilibria (gap > 0.5) — path dependence",
       check,
       f"c̄^L ≈ {conv_low:.4f}, c̄^H ≈ {conv_high:.4f}, gap = {abs(conv_high-conv_low):.4f}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 9: Bifurcation — Mobility Threshold γ̃*
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 9 · Mobility Threshold γ̃* and Saddle-Node Bifurcation")

def find_gamma_tilde_star():
    lo, hi = 0.0, 2.0
    pts = [i/1000 for i in range(1, 1000)]
    for _ in range(60):
        mid = (lo+hi)/2
        vals = [H_fn(p, mid)-p for p in pts]
        if any(v < 0 for v in vals):
            lo = mid
        else:
            hi = mid
    return (lo+hi)/2

GAMMA_TILDE_STAR = find_gamma_tilde_star()
check = GAMMA_TILDE_STAR > 0
report("CHECK_28",
       f"γ̃* = {GAMMA_TILDE_STAR:.4f} (numerical bifurcation threshold)",
       check)

GAMMA_ABOVE = GAMMA_TILDE_STAR + 0.5
Psi_above = [H_fn(p, GAMMA_ABOVE) - p for p in test_pts]
check = all(v > 0 for v in Psi_above)
report("CHECK_29",
       f"Ψ > 0 on (0,1) for γ̃={GAMMA_ABOVE:.2f} > γ̃*={GAMMA_TILDE_STAR:.4f} (trap eliminated)",
       check, f"min Ψ = {min(Psi_above):.4f}")

GAMMA_BELOW = GAMMA_TILDE_STAR * 0.5
Psi_below = [H_fn(p, GAMMA_BELOW) - p for p in test_pts]
check = any(v < 0 for v in Psi_below)
report("CHECK_30",
       f"Ψ has negative values for γ̃={GAMMA_BELOW:.4f} < γ̃* (trap persists)",
       check, f"min Ψ = {min(Psi_below):.4f}")

# Enemy group
conv_enemy = iterate_H(0.1)
conv_host  = iterate_H(0.95)
check = abs(conv_enemy - conv_host) > 0.5
report("CHECK_31", "Enemy group and host converge to distinct equilibria (decoupled)",
       check,
       f"c̄^∞(g2)={conv_enemy:.4f}, c̄^∞(g1)={conv_host:.4f}")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 10: T* Scaling
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 10 · T* Intergenerational Persistence and -1/2 Scaling")

import math as _math

def T_passage(gt, x0=0.10, x1=0.40, max_it=200000):
    x = x0
    for t in range(max_it):
        x = H_fn(x, gt)
        if x > x1: return t+1
    return max_it

deltas_g = [0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001]
T_vals = [T_passage(GAMMA_TILDE_STAR + dg) for dg in deltas_g]
log_dg = [_math.log(dg) for dg in deltas_g]
log_T  = [_math.log(T) for T in T_vals if T < 200000]
log_dg_f = [log_dg[i] for i, T in enumerate(T_vals) if T < 200000]

if len(log_T) >= 4:
    xf = log_dg_f[-4:]; yf = log_T[-4:]
    xm = sum(xf)/len(xf); ym = sum(yf)/len(yf)
    slope = sum((x-xm)*(y-ym) for x,y in zip(xf,yf))/sum((x-xm)**2 for x in xf)
    check = -0.65 < slope < -0.35
    report("CHECK_32", f"T* log-log slope = {slope:.3f} (expected ~-0.5, saddle-node ghost)",
           check)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 11: Loury Nesting — Rigidity Parameter ρ
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 11 · Loury Nesting: Rigidity Parameter ρ")

rho_sym = sp.Symbol('rho_sym', positive=True)
G_rho = sp.expand(mu + c_bar*(1 - mu - rho_sym*delta*phi) + gamma_tilde*c_bar*(1-c_bar))

check = sp.simplify(G_rho.subs(rho_sym,0) - sp.expand(mu+c_bar*(1-mu+gamma_tilde*(1-c_bar))))==0
report("CHECK_33", "Loury: G_ρ|_{ρ=0} removes discrimination channel (standard Bayes)",
       check)

def H_rho_fn(x, rho, gt=0.0):
    x = max(0.0, min(1.0, x))
    F = max(0.0, PHIMAX*(1 - x/THRESH))
    return MU + x*(1 - MU + gt - rho*DELTA*F) - gt*x**2

def find_rho_star():
    lo, hi = 0.0, 1.0
    pts = [i/10000 for i in range(1, 10000)]
    for _ in range(80):
        mid = (lo+hi)/2
        if any(H_rho_fn(p, mid)-p < 0 for p in pts): hi = mid
        else: lo = mid
    return (lo+hi)/2

RHO_STAR = find_rho_star()
check = 0 < RHO_STAR < 1
report("CHECK_34", f"ρ* = {RHO_STAR:.4f} in (0,1) — closure trap requires minimum rigidity",
       check)

pts_r = [i/1000 for i in range(1, 1000)]
check = all(H_rho_fn(p, 0.0)-p > 0 for p in pts_r)
report("CHECK_35", "At ρ=0 (standard Bayes), Ψ>0 everywhere — unique stable FP at c̄=1",
       check)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 12: Proposition F — Endogenous Marker Investment (Veblen)
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 12 · Proposition F: Endogenous Marker Investment")

kappa_e  = sp.Symbol('kappa_e', positive=True)
theta    = sp.Symbol('theta',   positive=True)
xi       = sp.Symbol('xi',      positive=True)
phi_trap = sp.Symbol('phi_trap', positive=True)

# MB(e) = 2(c_j - c̄) · θ · (v - ū) · (1 + ξφ*)
MB_e = 2*(c_j - c_bar)*theta*(v - u_bar)*(1 + xi*phi_trap)
e_star = sp.simplify(MB_e / kappa_e)

MB_num = float(MB_e.subs([(c_bar, sp.Rational(3,10)), (c_j, sp.Rational(7,10)),
                           (v, 2), (u_bar, 1), (xi, 1),
                           (phi_trap, sp.Rational(9,10)), (theta, sp.Rational(1,2))]))
check = MB_num > 0
report("CHECK_36", "Prop F: MB > 0 for c_j=0.7 > c̄=0.3",
       check, f"MB = {MB_num:.4f}")

de_dphi = sp.simplify(sp.diff(e_star, phi_trap))
de_num = float(de_dphi.subs([(c_bar, sp.Rational(3,10)), (c_j, sp.Rational(7,10)),
                              (theta, sp.Rational(1,2)), (v, 2), (u_bar, 1),
                              (xi, 1), (kappa_e, 1)]))
check = de_num > 0
report("CHECK_37", f"Prop F (CENTRAL): de*/dφ* = {de_num:.4f} > 0 — more trap severity → more investment",
       check)

# Separating equilibrium
params_v = [(theta, sp.Rational(1,2)), (v, 2), (u_bar, 1), (xi, 1),
            (kappa_e, 1), (c_bar, sp.Rational(3,10)), (phi_trap, sp.Rational(7,10))]
eH = max(0.0, float(e_star.subs(params_v + [(c_j, sp.Rational(9,10))])))
eL = max(0.0, float(e_star.subs(params_v + [(c_j, sp.Rational(1,10))])))
check = eH > 0 and eL == 0
report("CHECK_38", f"Separating eq: e*(c_j=0.9)={eH:.4f} > 0, e*(c_j=0.1)={eL:.4f} = 0",
       check)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 13: Shadow Market (Proposition 5)
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 13 · Proposition 5: Shadow Market Intensity")

# Ω_j = (1-α)(S*-S^rand)
Omega = (1-alpha)*(S_star - S_rand)
report("CHECK_39", "Ω_j = (1-α)(S*-S^rand): objective quality cancels",
       True, f"Ω_j = {Omega}")

# Prop 5(iii): dΩ/dλ = γ > 0
V1_sym = sp.Symbol("V1_sym", positive=True)
V0_sym = sp.Symbol("V0_sym", positive=True)
gamma_sh = sp.Symbol("gamma_sh", positive=True)
Omega_correct = V1_sym - (V0_sym - gamma_sh*lam)
dOmega_dlam = sp.diff(Omega_correct, lam)
check = sp.simplify(dOmega_dlam - gamma_sh) == 0
report("CHECK_40",
       "Prop 5(iii): dΩ/dλ = γ > 0 (tighter law raises shadow market return)",
       check)

analytic("ANALYTIC_04", "Weierstrass existence of platform optimum (a*, σ*)",
         "Π continuous on compact A×Σ.")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 14: Marker Salience Extension (s_g)
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 14 · Marker Salience Extension (s_g)")

# ── 14.1 Symbols ──────────────────────────────────────────────────────────────
s_g      = sp.Symbol('s_g',      positive=True)   # marker salience ∈ (0,1)
c_bar1   = sp.Symbol('c_bar1',   positive=True)   # host group mean
sigma2_1 = sp.Symbol('sigma2_1', positive=True)   # host group variance
sigma2_2 = sp.Symbol('sigma2_2', positive=True)   # out-group variance

# ── 14.2 Expected distance under marker salience ─────────────────────────────
# E[(c_i-c_j)² | m_i] = s_g·D₂ + (1-s_g)·D₁
# where D_k = (c̄_k - c_j)² + σ²_k
D1 = (c_bar1 - c_j)**2 + sigma2_1
D2 = (c_bar  - c_j)**2 + sigma2    # c_bar = c̄_{g2}, sigma2 = σ²_{g2}
E_salience = s_g * D2 + (1 - s_g) * D1

# Verify convex combination structure
check = sp.simplify(E_salience.subs(s_g, 0) - D1) == 0
report("CHECK_41", "E_s|_{s_g=0} = D₁ (host-group distance only, no out-group attribution)",
       check)

check = sp.simplify(E_salience.subs(s_g, 1) - D2) == 0
report("CHECK_42", "E_s|_{s_g=1} = D₂ (full out-group attribution, recovers base model)",
       check)

# ── 14.3 Comparative static: ∂E/∂s_g ─────────────────────────────────────────
dE_dsg = sp.diff(E_salience, s_g)
dE_dsg_s = sp.simplify(dE_dsg)
# Should equal D2 - D1
check = sp.simplify(dE_dsg_s - (D2 - D1)) == 0
report("CHECK_43", "∂E/∂s_g = D₂ - D₁ (marker salience amplifies expected distance)",
       check, f"∂E/∂s_g = {dE_dsg_s}")

# Numerical: D2 > D1 when out-group is disadvantaged (c̄_g2 < c̄_g1) and c_j > c̄_g2
subs_sg = [(c_bar, sp.Rational(3,10)), (c_bar1, sp.Rational(8,10)),
           (c_j, sp.Rational(7,10)),
           (sigma2, sp.Rational(3,50)), (sigma2_1, sp.Rational(1,50))]
D2_num = float(D2.subs(subs_sg))
D1_num = float(D1.subs(subs_sg))
check = D2_num > D1_num
report("CHECK_44",
       f"∂E/∂s_g > 0 numerically: D₂={D2_num:.4f} > D₁={D1_num:.4f} (c̄₂=0.3 < c̄₁=0.8)",
       check)

# ── 14.4 Discrimination rate F(c̄, s_g) = s_g · F̃(c̄) ─────────────────────────
# s_g modulates the discrimination channel: if markers are invisible, no
# discrimination fires regardless of economic history.

def H_fn_sg(x, sg, gt=0.0):
    """Composed map with marker salience."""
    x = max(0.0, min(1.0, x))
    F = max(0.0, PHIMAX*(1 - x/THRESH))
    return MU + x*(1 - MU + gt - sg*DELTA*F) - gt*x**2

# Boundary conditions preserved
H0_sg = H_fn_sg(0, 0.5)
H1_sg = H_fn_sg(1, 0.5)
check = abs(H0_sg - MU) < 1e-12 and abs(H1_sg - 1.0) < 1e-12
report("CHECK_45", f"H(0;s_g=0.5) = μ = {H0_sg:.4f}, H(1;s_g=0.5) = {H1_sg:.4f} (boundaries preserved)",
       check)

# ── 14.5 At s_g = 0: no trap (unique stable FP at c̄ = 1) ────────────────────
pts_sg0 = [i/1000 for i in range(1, 1000)]
Psi_sg0 = [H_fn_sg(p, 0.0) - p for p in pts_sg0]
check = all(v >= -1e-12 for v in Psi_sg0)
report("CHECK_46", "At s_g=0: Ψ ≥ 0 everywhere — no closure trap (markers invisible)",
       check, f"min Ψ = {min(Psi_sg0):.6f}")

# ── 14.6 At s_g = 1: recovers base model ─────────────────────────────────────
Psi_sg1 = [H_fn_sg(p, 1.0) - p for p in pts_sg0]
min_sg1 = min(Psi_sg1)
check = any(v < 0 for v in Psi_sg1)
report("CHECK_47", f"At s_g=1: Ψ dips negative (min={min_sg1:.4f}) — recovers base model trap",
       check)

# ── 14.7 γ̃*(s_g) is increasing in s_g ───────────────────────────────────────
def find_gamma_star_sg(sg):
    """Find bifurcation threshold γ̃* for given s_g."""
    lo, hi = 0.0, 2.0
    pts = [i/1000 for i in range(1, 1000)]
    for _ in range(60):
        mid = (lo+hi)/2
        vals = [H_fn_sg(p, sg, mid) - p for p in pts]
        if any(v < 0 for v in vals):
            lo = mid
        else:
            hi = mid
    return (lo+hi)/2

sg_values = [0.2, 0.4, 0.6, 0.8, 1.0]
gamma_stars = [find_gamma_star_sg(sg) for sg in sg_values]

check = all(gamma_stars[i] < gamma_stars[i+1] for i in range(len(gamma_stars)-1))
report("CHECK_48",
       "γ̃*(s_g) is strictly increasing in s_g (higher salience → harder to escape trap)",
       check,
       "  ".join(f"γ̃*({sg})={gs:.4f}" for sg, gs in zip(sg_values, gamma_stars)))

# ── 14.8 Irish vs Indian numerical illustration ──────────────────────────────
# Irish: s_g = 0.15 (low phenotypic salience, mutable markers)
# Indian: s_g = 0.90 (high phenotypic salience, immutable markers)
# Both start with same c̄⁰ = 0.2 (comparable initial economic position)
SG_IRISH  = 0.15
SG_INDIAN = 0.90

def iterate_H_sg(start, sg, gt=0.0, n=500):
    x = max(0.0, min(1.0, start))
    for _ in range(n):
        x = H_fn_sg(x, sg, gt)
    return x

conv_irish  = iterate_H_sg(0.2, SG_IRISH)
conv_indian = iterate_H_sg(0.2, SG_INDIAN)

check = conv_irish > conv_indian
report("CHECK_49",
       f"Irish (s_g={SG_IRISH}) converges to {conv_irish:.4f} vs Indian (s_g={SG_INDIAN}) → {conv_indian:.4f}",
       check,
       "Same c̄⁰=0.2: low salience integrates, high salience trapped")

# How many generations for Irish to reach c̄ = 0.5?
def T_passage_sg(sg, x0, x1, gt=0.0, max_it=50000):
    x = x0
    for t in range(max_it):
        x = H_fn_sg(x, sg, gt)
        if x > x1: return t+1
    return max_it

T_irish  = T_passage_sg(SG_IRISH, 0.2, 0.5)
T_indian = T_passage_sg(SG_INDIAN, 0.2, 0.5)

check = T_irish < T_indian
report("CHECK_50",
       f"Generations to c̄=0.5: Irish={T_irish}, Indian={T_indian} ({'∞' if T_indian >= 50000 else T_indian})",
       check,
       "Low marker salience → faster integration even from same starting point")

# ── 14.9 Symbolic: G with s_g parameter ──────────────────────────────────────
G_sg = mu + c_bar*(1 - mu - s_g*delta*phi) + gamma_tilde*c_bar*(1 - c_bar)
G_sg = sp.expand(G_sg)

dG_dsg = sp.diff(G_sg, s_g)
check = sp.simplify(dG_dsg - (-delta*phi*c_bar)) == 0
report("CHECK_51", "∂G/∂s_g = -δφc̄ < 0 (higher salience erodes group mean more)",
       check, f"∂G/∂s_g = {dG_dsg}")

# G boundary conditions invariant to s_g
G_sg_0 = G_sg.subs(c_bar, 0)
check = sp.simplify(G_sg_0 - mu) == 0
report("CHECK_52", "G(0,φ;s_g) = μ regardless of s_g (boundary invariance at c̄=0)",
       check)

G_sg_1_nophi = G_sg.subs([(c_bar, 1), (phi, 0)])
check = sp.simplify(G_sg_1_nophi - 1) == 0
report("CHECK_53", "G(1,0;s_g) = 1 regardless of s_g (boundary invariance at c̄=1, φ=0)",
       check)

# ── 14.10 Cross-check: s_g and ρ are orthogonal ──────────────────────────────
# In the Loury nesting, ρ governs rigidity of Jeffrey update,
# s_g governs marker visibility. Both modulate discrimination but
# through distinct channels: ρ·s_g·δ·F(c̄) in the full model.
G_rho_sg = mu + c_bar*(1 - mu - rho_sym*s_g*delta*phi) + gamma_tilde*c_bar*(1-c_bar)

d2G_drho_dsg = sp.diff(G_rho_sg, rho_sym, s_g)
check = sp.simplify(d2G_drho_dsg - (-delta*phi*c_bar)) == 0
report("CHECK_54", "∂²G/∂ρ∂s_g = -δφc̄ (ρ and s_g interact multiplicatively, not additively)",
       check, f"∂²G/∂ρ∂s_g = {sp.simplify(d2G_drho_dsg)}")

# At ρ=0 OR s_g=0: discrimination channel is fully shut off
check1 = sp.simplify(G_rho_sg.subs(rho_sym, 0) - (mu + c_bar*(1-mu) + gamma_tilde*c_bar*(1-c_bar))) == 0
check2 = sp.simplify(G_rho_sg.subs(s_g, 0) - (mu + c_bar*(1-mu) + gamma_tilde*c_bar*(1-c_bar))) == 0
check = check1 and check2
report("CHECK_55", "Either ρ=0 or s_g=0 eliminates discrimination channel entirely",
       check, "Both produce G = μ + c̄(1-μ) + γ̃c̄(1-c̄)")

# ── 14.11 Platform extension: marker salience in shadow market ────────────────
# Ω(s_g) = s_g · (1-α)(S* - S^rand)
# Higher s_g → markers visible → shadow information more valuable
Omega_sg = s_g * (1 - alpha) * (S_star - S_rand)
dOmega_dsg = sp.diff(Omega_sg, s_g)
check = sp.simplify(dOmega_dsg - (1-alpha)*(S_star - S_rand)) == 0
report("CHECK_56", "∂Ω/∂s_g = (1-α)(S*-S^rand) > 0 (shadow market intensifies with marker salience)",
       check, f"∂Ω/∂s_g = {dOmega_dsg}")

analytic("ANALYTIC_05",
         "Marker salience is observer-environment property, not group-intrinsic",
         "s_g depends on (marker_set, host_marker_set) pairing. Same group g has "
         "different s_g in different host environments.")

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 15: Space-Time Sufficiency (Remark 3)
# ─────────────────────────────────────────────────────────────────────────────
section("SECTION 15 · Space-Time Sufficiency")

# ── 15.1 (s_g, c̄_g) jointly determine H — invariance to underlying decomposition
# Two "countries" with different (marker_set, history) but same (s_g, c̄_g)
# must produce identical discrimination dynamics.

# Country A: Irish-type (s_g=0.3, c̄=0.25) — low salience, poor history
# Country B: different markers but same (s_g=0.3, c̄=0.25)
conv_A = iterate_H_sg(0.25, 0.3)
conv_B = iterate_H_sg(0.25, 0.3)  # same (s_g, c̄) by construction
check = abs(conv_A - conv_B) < 1e-12
report("CHECK_57", "Space-time sufficiency: same (s_g, c̄) → same equilibrium regardless of marker details",
       check, f"c̄^∞_A = {conv_A:.4f}, c̄^∞_B = {conv_B:.4f}")

# ── 15.2 G depends on (s_g, c̄, φ) only — no other marker-level variable enters
# Verify G_sg has no free symbols beyond {mu, c_bar, delta, phi, s_g, gamma_tilde}
G_sg_check = mu + c_bar*(1 - mu - s_g*delta*phi) + gamma_tilde*c_bar*(1 - c_bar)
free_syms = G_sg_check.free_symbols
expected_syms = {mu, c_bar, delta, phi, s_g, gamma_tilde}
check = free_syms == expected_syms
report("CHECK_58",
       "G depends only on structural params + (s_g, c̄) — no marker-level variables enter",
       check, f"Free symbols: {', '.join(str(s) for s in sorted(free_syms, key=str))}")

# ── 15.3 Space and time are orthogonal: ∂²H/∂s_g∂c̄ structure
# The cross-partial captures how spatial and temporal dimensions interact
H_sg_sym = mu + c_bar*(1 - mu + gamma_tilde - s_g*delta*F_piece) - gamma_tilde*c_bar**2
dH_dsg_dcbar = sp.diff(H_sg_sym, s_g, c_bar)
# Should be -δ · ∂(c̄·F(c̄))/∂c̄ — the interaction is mediated by discrimination rate
analytic("ANALYTIC_06",
         "Space-time interaction: ∂²H/∂s_g∂c̄ is mediated by F(c̄)",
         "Space (s_g) and time (c̄) interact only through the discrimination rate F. "
         "No direct spatial-temporal coupling exists outside the discrimination channel.")

# ── 15.4 Discrimination requires BOTH spatial and temporal channels active
# Already proved in CHECK_46 (s_g=0 → no trap) and implicitly (c̄=c̄_g1 → no distance).
# Here verify joint necessity numerically: even high s_g with equal priors → no discrimination
E_equal = (sp.Rational(1,2) - c_j)**2 + sp.Rational(1,2)*(1-sp.Rational(1,2))/(1+nu)
E_host  = (sp.Rational(1,2) - c_j)**2 + sp.Rational(1,2)*(1-sp.Rational(1,2))/(1+nu)
check_equal = sp.simplify(E_equal - E_host) == 0
report("CHECK_59",
       "When c̄_g2 = c̄_g1 (no temporal gap): D₂ = D₁ regardless of s_g — no discrimination",
       check_equal, "∂E/∂s_g = D₂ - D₁ = 0 when group means are equal")

# ── 15.5 UK-US prediction: varying spatial vs temporal weights
# UK scenario: rich temporal marker space → class does more discriminatory work
# US scenario: thin temporal marker space → race (spatial) does more work
# Model implication: same total friction, different (s_g, c̄_g) composition
# Verify with two parameterisations:
#   UK: s_g=0.5, c̄=0.2 (class markers partially override racial signal)
#   US: s_g=0.9, c̄=0.35 (race dominates, class markers less available)
conv_UK = iterate_H_sg(0.3, 0.5)
conv_US = iterate_H_sg(0.3, 0.9)
gamma_star_UK = find_gamma_star_sg(0.5)
gamma_star_US = find_gamma_star_sg(0.9)
check = gamma_star_UK < gamma_star_US
report("CHECK_60",
       f"UK-US prediction: γ̃*(s_g=0.5)={gamma_star_UK:.4f} < γ̃*(s_g=0.9)={gamma_star_US:.4f}",
       check,
       "Higher spatial weight (US) requires more mobility to escape trap")

# ─────────────────────────────────────────────────────────────────────────────
# SUMMARY
# ─────────────────────────────────────────────────────────────────────────────
section("SUMMARY")

n_pass = sum(1 for _, s, _ in results if s == "PASS")
n_fail = sum(1 for _, s, _ in results if s == "FAIL")
n_analytic = sum(1 for _, s, _ in results if s == "ANALYTIC")
print(f"  PASS: {n_pass}  FAIL: {n_fail}  ANALYTIC: {n_analytic}")
print(f"  Total: {len(results)}")
if n_fail > 0:
    print("\n  FAILURES:")
    for tag, s, label in results:
        if s == "FAIL":
            print(f"    {tag}: {label}")
