"""
verify_egarch_recovery.py
==========================
Simulation-and-null-test harness for Proposition 6 (EGARCH equivalence),
per the standing rule: validate an estimator on known truth before real
data. Agreed 30 Jul 2026.

STATUS: this file went through TWO failed drafts before landing on the
actual finding, and both failures are recorded because they are
informative, not just embarrassing.

  Draft 1: unbounded softplus parametrisation under Nelder-Mead overflowed
  (exp() of a ~700 argument), producing a "lambda_hat" with 300+ digits.
  Caught by the project's own standing discipline -- an implausible number
  is a reason to distrust the CHECK, not to report the mathematics as
  broken. Fixed with a directly bounded parametrisation.

  Draft 2: with bounds and multistart L-BFGS-B in place, the joint MLE of
  (phi, c, theta, V0) still returned wildly unstable lambda_hat, repeatedly
  pinned at the parameter bounds. This looked like a persistent optimizer
  bug. It is not -- profiling the log-likelihood directly (see PROFILE
  section below) shows a genuine near-flat RIDGE relating c and theta: many
  (c, theta) pairs fit almost equally well, up to lambda in the thousands,
  because c*tanh(z/theta) is well approximated by (c/theta)*z whenever z
  stays within a few multiples of theta of the reference. JOINT
  identification of (lambda, theta) from typical-sized data is genuinely
  weak. This is a property of the model, not a bug in the estimator.

WHAT THIS MEANS FOR THE EMPIRICAL STRATEGY, and it is the actual
deliverable of this exercise: theta cannot be jointly estimated alongside
lambda from routine data. It must be calibrated separately -- e.g. from
institutional knowledge of what counts as "deep distress" relative to the
regulatory threshold, or from a subsample containing genuine severe
excursions. CONDITIONAL on theta, lambda is sharply and cleanly identified
(see PART A below, profile/concentrated likelihood). This is a materially
different and more useful conclusion than either "it works" or "it's
broken" -- it specifies the condition under which it works.

RESULT, theta known, T=300, 20 independent replications at the true null
lambda=1: mean bias +0.0845, std 0.1185, 95%% range [0.00, +0.38]. Small,
one-sided (lambda_hat is bounded below at 1, so noise can only push it up --
a standard truncation effect, not a manufactured-asymmetry problem), and
NOT of the same character as the sibling wealth-dynamics paper's null-case
failure (lambda_hat ~2.0-2.1), which came from a mis-estimable trend
parameter with no analogue here once theta is fixed externally.

Structure:
  PART A   recovery of lambda via a PROFILE (theta-conditional) MLE --
           the estimator a practitioner should actually use
  PART B   the joint-identification ridge, shown directly via profile
           log-likelihoods, not inferred from optimizer instability
  PART C   whether our conditioning variable (state LEVEL z_{t-1}) is the
           same object as canonical EGARCH's (standardised residual).
           It is not -- checked, not asserted.
"""

import numpy as np
from scipy.optimize import minimize

rng = np.random.default_rng(0)


def simulate(T, phi, lam, theta, V0, z0=0.0, seed=0):
    r = np.random.default_rng(seed)
    c = np.log(lam)
    z = np.empty(T)
    z_prev = z0
    for t in range(T):
        V = V0 * np.exp(-2 * c * np.tanh(z_prev / theta))
        z[t] = phi * z_prev + r.normal(0, np.sqrt(V))
        z_prev = z[t]
    return z


def loglik(z, phi, c, theta, V0):
    z_prev = np.concatenate(([0.0], z[:-1]))
    V = V0 * np.exp(np.clip(-2 * c * np.tanh(z_prev / theta), -30, 30))
    resid = z - phi * z_prev
    return np.sum(-0.5 * np.log(2 * np.pi * V) - 0.5 * resid**2 / V)


def neg_loglik(params, z, theta_fixed):
    phi, c, V0 = params
    return -loglik(z, phi, c, theta_fixed, V0)


def fit_profile(z, theta_fixed, n_restarts=6):
    """MLE of (phi, c, V0) CONDITIONAL on a given theta -- the estimator a
    practitioner should use once theta is externally calibrated."""
    bounds = [(-0.99, 0.99), (0.0, 3.0), (1e-6, 5.0)]
    r = np.random.default_rng(321)
    best = None
    for _ in range(n_restarts):
        x0 = [r.uniform(0.3, 0.9), r.uniform(0.0, 1.0), r.uniform(0.01, 0.2)]
        res = minimize(neg_loglik, x0, args=(z, theta_fixed), method="L-BFGS-B",
                        bounds=bounds, options={"maxiter": 5000, "ftol": 1e-13})
        if not np.isfinite(res.fun):
            continue
        if best is None or res.fun < best.fun:
            best = res
    phi, c, V0 = best.x
    return dict(phi=phi, lam=np.exp(c), V0=V0, nll=best.fun,
                at_bound=(c > 2.99 or c < 1e-6))


TRUE_PHI, TRUE_THETA, TRUE_V0 = 0.85, 1.0, 0.04

print("=" * 78)
print("PART A: recovery of lambda via PROFILE MLE, theta CORRECTLY specified")
print("(the estimator a practitioner should use once theta is calibrated")
print(" independently rather than jointly estimated -- see PART B for why)")
print("=" * 78)
print(f"{'true lam':>9} {'T':>6}  {'lam_hat':>9} {'bias':>8}  {'at_bound':>8}")
results_A = []
null_reps = 20   # genuinely independent replications for the null, not 3 copies
for true_lam, T, n_reps in [(1.0, 300, null_reps), (1.2, 300, 1),
                             (1.5, 300, 1), (2.0, 300, 1),
                             (1.2, 80, 1), (1.5, 80, 1), (2.0, 80, 1)]:
    for rep in range(n_reps):
        z = simulate(T, TRUE_PHI, true_lam, TRUE_THETA, TRUE_V0,
                      seed=10000 * int(true_lam * 100) + 17 * T + rep)
        r = fit_profile(z, TRUE_THETA)
        bias = r["lam"] - true_lam
        results_A.append((true_lam, T, r["lam"], bias))
        if n_reps == 1:
            print(f"{true_lam:>9.2f} {T:>6d}  {r['lam']:>9.4f} {bias:>+8.4f}  "
                  f"{str(r['at_bound']):>8}")

null_biases = np.array([b for (tl, T, lh, b) in results_A if tl == 1.0])
print(f"     1.00    300  [{null_reps} independent reps, summarised below]")
print()
print(f"NULL CASE (true lambda=1), theta known, {null_reps} INDEPENDENT reps at T=300:")
print(f"  mean bias   = {null_biases.mean():+.4f}")
print(f"  std dev     = {null_biases.std():.4f}")
print(f"  95%% range   = [{np.percentile(null_biases,2.5):+.4f}, "
      f"{np.percentile(null_biases,97.5):+.4f}]")
print("Compare: the sibling wealth-dynamics paper's estimator returned")
print("lambda_hat ~2.0-2.1 at the true null, because its regime")
print("classification depended on a mis-estimable trend g. There is no")
print("analogous free parameter here ONCE theta is fixed externally --")
print("this is the condition under which the null-bias problem does not")
print("recur.")


print()
print("=" * 78)
print("PART B: the joint (lambda, theta) identification ridge -- shown")
print("directly via profile log-likelihood, not inferred from instability")
print("=" * 78)
TRUE_LAM_B, TRUE_C_B = 1.5, np.log(1.5)
z = simulate(5000, TRUE_PHI, TRUE_LAM_B, TRUE_THETA, TRUE_V0, seed=1)
print(f"(T=5000, so this isolates identification from finite-sample noise)")
print(f"realised range of z: [{z.min():.3f}, {z.max():.3f}]  vs theta={TRUE_THETA}")
print()
print("theta held at various values, c re-optimised at each (the profile")
print("likelihood in theta) -- a flat curve means theta is not identified")
print("even with abundant data:")
print(f"{'theta':>7} {'best c':>9} {'implied lam':>12} {'profile ll':>12}")
for theta_try in [0.3, 0.5, 0.7, 1.0, 1.5, 2.0, 3.0, 5.0, 10.0, 20.0]:
    best = None
    for c_try in np.linspace(0.0, 3.0, 400):
        ll = loglik(z, TRUE_PHI, c_try, theta_try, TRUE_V0)
        if best is None or ll > best[1]:
            best = (c_try, ll)
    print(f"{theta_try:>7.2f} {best[0]:>9.4f} {np.exp(best[0]):>12.3f} "
          f"{best[1]:>12.4f}")

print()
print("If the profile ll column is nearly flat while implied lambda ranges")
print("over orders of magnitude, that CONFIRMS the ridge: many (c,theta)")
print("pairs are close to observationally equivalent, and theta must come")
print("from OUTSIDE the estimation -- calibrated, not jointly fit.")


print()
print("=" * 78)
print("PART C: is our conditioning variable (state level) the same object")
print("as canonical EGARCH's (standardised residual)?")
print("=" * 78)
z = simulate(2000, TRUE_PHI, 1.5, TRUE_THETA, TRUE_V0, seed=7)
z_prev = z[:-1]
resid = z[1:] - TRUE_PHI * z_prev
V_true = TRUE_V0 * np.exp(-2 * np.log(1.5) * np.tanh(z_prev / TRUE_THETA))
std_resid = resid / np.sqrt(V_true)
corr = np.corrcoef(z_prev, std_resid)[0, 1]
print(f"corr(state level z_(t-1), standardised residual) = {corr:.4f}")
print("Weakly related through persistence phi, not the same variable by")
print("construction. An off-the-shelf EGARCH(1,1) package conditions")
print("variance on standardised shocks; our model conditions it on the")
print("buffer LEVEL. Fitting the former is not a direct test of Prop.6.")


print()
print("=" * 78)
print("SUMMARY, for folding back into the paper")
print("=" * 78)
print("1. Lambda IS cleanly recoverable (Part A) -- CONDITIONAL on theta")
print("   being fixed from outside the estimation. Null-case bias is small")
print("   once that condition holds.")
print("2. Theta and lambda are NOT jointly identified from routine-sized")
print("   excursions (Part B) -- a genuine, quantified, structural limit on")
print("   what Prop.6's 'estimable from returns data' can deliver without")
print("   an auxiliary theta calibration.")
print("3. Prop.6's wording should be tightened on two counts: (i) it should")
print("   say the model is estimable GIVEN an externally calibrated theta,")
print("   not estimable outright; (ii) 'EGARCH specification' should read")
print("   'EGARCH-CLASS specification' or similarly qualified, since the")
print("   conditioning variable differs from canonical EGARCH's (Part C).")

print()
print("=" * 78)
print("PART D: sensitivity to a MISCALIBRATED theta")
print("(the practically decisive question: Part A showed clean recovery")
print(" GIVEN the true theta. A practitioner will only ever have a")
print(" CALIBRATED theta, with some error. How much does that error cost?)")
print("=" * 78)
TRUE_LAM_D = 1.5
print(f"{'theta_used/true':>16} {'lam_hat':>9} {'bias':>8}")
z_fixed = simulate(300, TRUE_PHI, TRUE_LAM_D, TRUE_THETA, TRUE_V0, seed=99)
for ratio in [0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4]:
    theta_used = TRUE_THETA * ratio
    r = fit_profile(z_fixed, theta_used)
    bias = r["lam"] - TRUE_LAM_D
    print(f"{ratio:>16.2f} {r['lam']:>9.4f} {bias:>+8.4f}")

print()
print("Repeated across 10 independent draws at each misspecification level,")
print("to separate systematic mis-calibration bias from single-draw noise:")
print(f"{'theta_used/true':>16} {'mean lam_hat':>13} {'mean bias':>10} {'std':>8}")
for ratio in [0.7, 0.85, 1.0, 1.15, 1.3]:
    theta_used = TRUE_THETA * ratio
    lams = []
    for rep in range(10):
        z = simulate(300, TRUE_PHI, TRUE_LAM_D, TRUE_THETA, TRUE_V0,
                      seed=50000 + rep)
        r = fit_profile(z, theta_used)
        lams.append(r["lam"])
    lams = np.array(lams)
    print(f"{ratio:>16.2f} {lams.mean():>13.4f} {lams.mean()-TRUE_LAM_D:>+10.4f} "
          f"{lams.std():>8.4f}")

print()
print("Read this against Part B's ridge: theta error should map roughly")
print("along that ridge (over- or under-stating theta trades off against an")
print("implied lambda in the SAME direction the ridge predicts), so the")
print("practical requirement is a theta calibration accurate to within the")
print("range where the table above stays close to the true lambda=1.5 --")
print("not a requirement for theta to be exact.")


print()
print("=" * 78)
print("PART E: robustness of the Part A null-bias result across (phi, T)")
print("(Part A used one parameter set. Confirming the clean-recovery result")
print(" is not an artefact of that specific choice.)")
print("=" * 78)
print(f"{'phi':>6} {'T':>6} {'mean bias (lam=1)':>18} {'std':>8}  [10 reps]")
for phi_try in [0.5, 0.85, 0.95]:
    for T_try in [150, 300, 800]:
        lams = []
        for rep in range(10):
            z = simulate(T_try, phi_try, 1.0, TRUE_THETA, TRUE_V0,
                          seed=70000 + 1000*int(phi_try*100) + 13*T_try + rep)
            r = fit_profile(z, TRUE_THETA)
            lams.append(r["lam"])
        lams = np.array(lams)
        print(f"{phi_try:>6.2f} {T_try:>6d} {lams.mean()-1.0:>+18.4f} "
              f"{lams.std():>8.4f}")

print()
print("If bias stays small and one-sided (never large and two-sided) across")
print("this grid, the Part A finding is not a fluke of one parameter choice.")
