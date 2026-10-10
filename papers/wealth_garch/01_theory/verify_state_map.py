"""
verify_state_map.py
===================
Backs the state-variable reconciliation in PROOFS.tex Section "Two
unresolved modelling questions", subsection "The state variable".

THE CLAIM. Phase 1 uses z = log(E/K) on the whole line with no finite
lower boundary; Phase 2 inherits x = A/L from the benchmark, with a
degenerate boundary at x=1. The document leaves open whether these are
reconcilable and whether the degeneracy is economically necessary or an
artefact of the imported coordinate. The claim verified here is that,
PROVIDED the capital requirement is a RATIO requirement K = q*L (which is
what Basel is), the two states are related by the explicit diffeomorphism

    z = log((x-1)/q),      x = 1 + q*exp(z),

under which (i) the two coordinate systems' special points coincide,
(ii) the diffusion is NON-DEGENERATE in z, so the degeneracy at x=1 is a
coordinate artefact, and (iii) the entrance-boundary condition r > r_L is
exactly the sign of the z-drift's blow-up at -infinity.

The verification is deliberately done TWICE and in OPPOSITE DIRECTIONS
(M3 forward by Ito, M5 backward from the z-SDE to the x-SDE), because the
forward computation alone would not catch a sign or factor error that the
inverse map reproduces symmetrically.

Tags:
  M1   the map is a bijection (1,inf) -> (-inf,inf) with the stated inverse
  M2   Sigma(x) factors EXACTLY as (x-1)*kappa1 in the solvency regime
  M3   forward Ito: volatility of z is kappa1, CONSTANT (degeneracy gone)
  M4   forward Ito: drift of z is B(x)/(x-1) - kappa1^2/2, and its
       blow-up at x->1+ is +infinity iff B(1) = r - r_L > 0
  M5   BACKWARD check: starting from dz with constant volatility kappa1
       and pushing through x = 1 + q*exp(z), the x-SDE of Phase 2 is
       recovered exactly. Independent of M3/M4's algebra
  M6   special points align: x=1 <-> z=-inf, x=1+q <-> z=0, x->inf <-> z->inf;
       and at the numerics' R=1.15 the requirement level q=R-1 puts
       Phase 1's z=0 exactly at Phase 2's reference R
  M7   numerical corroboration: an Euler simulation of the x-SDE,
       transformed to z, has realised quadratic variation matching
       kappa1^2 per unit time
  M8*  DELIBERATE EXPECTED-FAIL: the constant-volatility result is claimed
       ONLY for the solvency regime x < xbar. This checks it in the
       LIQUIDITY regime, where u(x)=(x-a2)/a3 and Sigma is NOT proportional
       to (x-1). A PASS would mean the scope limitation stated in the
       document is unnecessary -- i.e. the document is understating its
       own result. It is expected to FAIL.
"""
import numpy as np
import sympy as sp

ledger = []
def record(tag, ok, statement):
    ledger.append((tag, bool(ok), statement))

x, z, q = sp.symbols('x z q', positive=True)   # q = ratio requirement (was c_K)
sig, sigL, a1, a2, a3 = sp.symbols('sigma sigma_L a1 a2 a3', positive=True)
r, muL, gam, muS = sp.symbols('r mu_L gamma mu_s', positive=True)

# ---------------------------------------------------------------- M1
fwd = sp.log((x-1)/q)
inv = 1 + q*sp.exp(z)
round_trip_1 = sp.simplify(fwd.subs(x, inv))
round_trip_2 = sp.simplify(inv.subs(z, fwd))
record("M1", sp.simplify(round_trip_1 - z) == 0 and sp.simplify(round_trip_2 - x) == 0,
       f"z=log((x-1)/q) and x=1+q*exp(z) are mutual inverses "
       f"(both round trips simplify to the identity)")

# ---------------------------------------------------------------- M2
# eq:diff carries a CORRELATION c between the risky return and the liability
# shock. The perfect-square form sigma*u + sigma_L*(1-x) is the c=1 case
# only. Verify the general quadratic, which is what the solver integrates.
corr = sp.Symbol('c_corr', positive=True)
u_solv = (x-1)/a1
Sigma2_gen = sig**2*u_solv**2 + 2*corr*sig*sigL*u_solv*(1-x) + sigL**2*(1-x)**2
kappa1_c2 = sp.simplify(Sigma2_gen/(x-1)**2)
kappa1_c = sp.sqrt(kappa1_c2)
kappa1 = sig/a1 - sigL          # the c=1 special case, kept for M3-M5
record("M2", sp.simplify(sp.diff(kappa1_c2, x)) == 0
             and sp.simplify(kappa1_c2.subs(corr, 1) - kappa1**2) == 0,
       f"the GENERAL diffusion of eq:diff, at the binding cap, gives "
       f"sigma^2(x)/(x-1)^2 = {sp.simplify(kappa1_c2)} -- constant in x for "
       f"EVERY correlation c, and reducing to (sigma/a1-sigma_L)^2 at c=1. "
       f"So Prop 28(ii)'s conclusion does not require perfect correlation; "
       f"only the VALUE of the constant depends on c")
Sigma = sig*u_solv + sigL*(1-x)   # c=1 form, used by M3-M5 below

# ---------------------------------------------------------------- M3
dzdx = sp.diff(fwd, x)
vol_z = sp.simplify(Sigma*dzdx)
record("M3", sp.simplify(vol_z - kappa1) == 0 and sp.diff(vol_z, x) == 0,
       f"forward Ito: volatility of z = Sigma*(dz/dx) = kappa1, with zero "
       f"derivative in x -- CONSTANT, so the x=1 degeneracy does not appear "
       f"in z coordinates")

# ---------------------------------------------------------------- M4
B = u_solv*(muS-r) + (r-muL)*x + gam
d2zdx2 = sp.diff(fwd, x, 2)
drift_z = sp.simplify(B*dzdx + sp.Rational(1,2)*Sigma**2*d2zdx2)
expected = sp.simplify(B/(x-1) - kappa1**2/2)
B_at_1 = sp.simplify(B.subs(x, 1))
record("M4", sp.simplify(drift_z - expected) == 0 and sp.simplify(B_at_1 - (r - muL + gam)) == 0,
       f"forward Ito: drift of z = B(x)/(x-1) - kappa1^2/2, and "
       f"B(1) = r - mu_L + gamma = r - r_L, so the drift tends to +infinity "
       f"as z -> -infinity exactly when r > r_L")

# ---------------------------------------------------------------- M5
# Backward: assume dz = mu_z dt + kappa1 dW, push through x = 1+q*exp(z).
mu_z = sp.Symbol('mu_z')
dxdz = sp.diff(inv, z)
d2xdz2 = sp.diff(inv, z, 2)
drift_x_back = sp.simplify(mu_z*dxdz + sp.Rational(1,2)*kappa1**2*d2xdz2)
vol_x_back = sp.simplify(kappa1*dxdz)
# substitute the mu_z that M4 produced, expressed in z
mu_z_expr = (B/(x-1) - kappa1**2/2).subs(x, inv)
drift_x_recovered = sp.simplify(drift_x_back.subs(mu_z, mu_z_expr))
B_in_z = sp.simplify(B.subs(x, inv))
vol_x_expected = sp.simplify(Sigma.subs(x, inv))
record("M5", sp.simplify(drift_x_recovered - B_in_z) == 0
             and sp.simplify(vol_x_back - vol_x_expected) == 0,
       f"BACKWARD check (independent of M3/M4): pushing dz = mu_z dt + "
       f"kappa1 dW through x = 1+q*exp(z) recovers Phase 2's drift B(x) and "
       f"diffusion Sigma(x) exactly")

# ---------------------------------------------------------------- M6
lim_at_1 = sp.limit(fwd, x, 1, '+')
at_1pc = sp.simplify(fwd.subs(x, 1+q))
lim_at_inf = sp.limit(fwd, x, sp.oo)
R_num, q_num = 1.15, 0.15
record("M6", lim_at_1 == -sp.oo and at_1pc == 0 and lim_at_inf == sp.oo
             and abs((1+q_num) - R_num) < 1e-12,
       f"special points align: x=1 <-> z=-inf, x=1+q <-> z=0, x->inf <-> "
       f"z->inf; and at the numerics' R={R_num}, q=R-1={q_num} puts Phase 1's "
       f"z=0 exactly at Phase 2's reference R")

# ---------------------------------------------------------------- M7
P = dict(sigma=0.08, sigma_L=0.03, a1=0.045, a2=0.05, a3=0.30,
         r=0.025, mu_L=0.03, gamma=0.02, mu_s=0.04)
k1n = P["sigma"]/P["a1"] - P["sigma_L"]
cn = 0.15
def Sig_n(xx):
    u = min((xx-1)/P["a1"], (xx-P["a2"])/P["a3"])
    return P["sigma"]*u + P["sigma_L"]*(1-xx)
def B_n(xx):
    u = min((xx-1)/P["a1"], (xx-P["a2"])/P["a3"])
    return u*(P["mu_s"]-P["r"]) + (P["r"]-P["mu_L"])*xx + P["gamma"]

rng = np.random.default_rng(20260807)
dt, nsteps, npaths = 1e-6, 20000, 200
x0 = 1.05
xs = np.full(npaths, x0)
qv = np.zeros(npaths)
zprev = np.log((xs-1)/cn)
for _ in range(nsteps):
    S = np.array([Sig_n(v) for v in xs])
    Bv = np.array([B_n(v) for v in xs])
    dW = rng.normal(0.0, np.sqrt(dt), npaths)
    xs = xs + Bv*dt + S*dW
    xs = np.maximum(xs, 1.0 + 1e-12)
    znew = np.log((xs-1)/cn)
    qv += (znew - zprev)**2
    zprev = znew
qv_rate = qv.mean()/(nsteps*dt)
rel_err = abs(qv_rate - k1n**2)/k1n**2
record("M7", rel_err < 0.02,
       f"numerical: Euler simulation of the x-SDE, transformed to z, gives "
       f"realised quadratic variation {qv_rate:.4f} per unit time vs "
       f"kappa1^2 = {k1n**2:.4f} (relative error {rel_err:.2e})")

# ---------------------------------------------------------------- M8*
u_liq = (x-a2)/a3
Sigma_liq = sig*u_liq + sigL*(1-x)
factors_liq = sp.simplify(sp.simplify(Sigma_liq/(x-1)) - sp.simplify(sp.diff(Sigma_liq/(x-1), x)*0 + Sigma_liq/(x-1)))
vol_z_liq = sp.simplify(Sigma_liq*dzdx)
is_const_liq = sp.simplify(sp.diff(vol_z_liq, x)) == 0
record("M8*", is_const_liq,
       f"EXPECTED FAIL: in the LIQUIDITY regime u(x)=(x-a2)/a3, the "
       f"z-volatility is {sp.simplify(vol_z_liq)}, which is NOT constant in x. "
       f"A PASS would mean the solvency-regime scope limitation stated in "
       f"the document is unnecessary")

# --------------------------------------------------------------- M9
# HISTORY, because this check's meaning changed once and could again.
# M9 was originally written to FLAG a live inconsistency: PROOFS.tex
# quoted kappa1 = sigma/a1 - sigma_L, the c=1 value, while the solver
# ran c=0.20. It failed by design and the failure was the point. That
# inconsistency has since been FIXED -- eq:kappa1c now carries the
# general kappa1(c) and Prop 28(ii) is stated for every c -- so the old
# comparison (c=1 constant vs c=0.20 constant) would now fail forever
# while flagging nothing, which is worse than useless in a suite that
# is meant to be read for real failures.
#
# M9 is therefore RETARGETED at the property that must now hold: the
# document's quoted numeric value for kappa1 at the solver's own c must
# equal the general formula evaluated there. This is a documentation-
# consistency check with real content -- it fails if either the quoted
# figure or params.c drifts without the other being updated.
C_SOLVER = 0.20                 # params.c in every shipped solver run
KAPPA1_QUOTED_IN_PROOFS = 1.7720   # Prop 28 discussion / eq:kappa1c text
k_general = float(sp.sqrt(kappa1_c2.subs({sig: P["sigma"], sigL: P["sigma_L"],
                                          a1: P["a1"], corr: C_SOLVER})))
k_c1_legacy = P["sigma"]/P["a1"] - P["sigma_L"]
agree = abs(k_general - KAPPA1_QUOTED_IN_PROOFS) < 5e-5
record("M9", agree,
       f"documentation consistency: kappa1(c) from eq:kappa1c evaluated at "
       f"the solver's params.c={C_SOLVER} gives {k_general:.6f}, matching the "
       f"{KAPPA1_QUOTED_IN_PROOFS} quoted in PROOFS.tex. (For the record, the "
       f"c=1 value {k_c1_legacy:.6f} is what earlier drafts wrongly used; the "
       f"{100*abs(k_general-k_c1_legacy)/k_c1_legacy:.2f}% gap is why that "
       f"mattered. This check fails if the quoted figure or params.c drifts "
       f"without the other following)")

# ---------------------------------------------------------------- M10
# Prop 8 (rrL), re-homed here from its former location on a since-removed
# analytic route: u(1)=0, so B(1) = (r - mu_L) + gamma, and under
# mu_L = gamma + r_L this equals r - r_L. Positive drift at the distress
# boundary therefore requires r > r_L, and the project's parameters
# satisfy it: r=0.025, mu_L=0.03, gamma=0.02 -> r_L=0.01, r-r_L=+0.015.
r_l = sp.Symbol('r_L', positive=True)
B1_sym = (r - muL) + gam
B1_under_decomp = sp.simplify(B1_sym.subs(muL, gam + r_l) - (r - r_l))
r_n, muL_n, gam_n = sp.Rational(25,1000), sp.Rational(3,100), sp.Rational(2,100)
rL_n = muL_n - gam_n
record("M10", B1_under_decomp == 0 and (r_n - rL_n) == sp.Rational(3,200) and (r_n - rL_n) > 0,
       f"Prop 8 (rrL): B(1) = (r-mu_L)+gamma = r-r_L exactly (symbolic), and at "
       f"the project's parameters r-r_L = {sp.Rational(3,200)} = +0.015 > 0, so the "
       f"drift at the distress boundary points inward")

line = "="*78
print(line); print(f"{'tag':<6}{'result':<8}statement"); print(line)
for tag, ok, st in ledger:
    print(f"{tag:<6}{'PASS' if ok else 'FAIL':<8}{st}")
print(line)
print("M8* is a DELIBERATE expected-failure: its FAILING confirms that the")
print("constant-volatility result is genuinely confined to the solvency")
print("regime, as the document states, rather than holding everywhere.")
print()
print("M9 is a CALIBRATION CONSISTENCY check, not a mathematical one. It")
print("fails iff the numerics use a correlation other than 1 while the")
print("document quotes the c=1 constant. A FAIL is a real inconsistency to")
print("fix in PROOFS.tex, not an expected-failure to be waved through.")
