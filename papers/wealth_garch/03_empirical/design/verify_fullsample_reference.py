"""
Validation gate for the FULL-SAMPLE-MEAN reference construction
(EMPIRICAL_v2 Section 4, "Secondary analysis").

The primary construction uses a one-sided expanding mean of each country's
own past. It cannot register a deficit quarter in a trending series, which
in the real panel removes essentially every advanced banking system. The
proposed replacement is a constant per-country reference: the full-sample
mean of the log ratio.

That construction buys both regimes for a trending series at two stated
costs: it uses look-ahead (the reference at date t involves data after t),
and for a trending series the regime label partially proxies calendar
time, so an estimate could pick up secular changes in volatility rather
than state-dependence. This script is the pre-committed gate deciding
whether those costs are harmless. It runs BEFORE any real-data
application.

Four parts:

  A  RECOVERY      -- with a known true lambda and a stationary series,
                      does the construction recover it, and how does it
                      compare with classification on the true state?
  B  CLEAN NULL    -- stationary series, true lambda = 1: does the
                      construction manufacture asymmetry from nothing?
  C  TRENDED NULL  -- upward-drifting series, true lambda = 1: THE GATE.
                      This is the case the look-ahead and time-proxy
                      concerns target. If a trend alone moves lambda_hat
                      away from 1, the construction is not usable and no
                      real-data number from it may be quoted.
  D  DEGENERACY    -- on the same trended data, how often does each
                      construction produce an estimate at all? This is the
                      simulated counterpart of the real panel's attrition.

The estimator is copied verbatim from expanding_mean_panel.py so this
tests the shipped code path, not a re-implementation. BURNIN and MIN_OBS
match the panel exactly.

Run:  python3 verify_fullsample_reference.py
"""

import numpy as np
import statsmodels.api as sm

BURNIN = 12
MIN_OBS = 15

REPS = 200
T_DEFAULT = 60          # quarters, close to the real panel's typical length
RHO_DEFAULT = 0.85      # AR(1) persistence of the stationary component
SIGMA_DEFAULT = 0.05

PASS_BIAS = 0.10        # |mean lambda_hat - true lambda| tolerated at a null
PASS_TREND_GAP = 0.10   # extra bias a trend may add over the clean null


def variance_ratio_lambda(z, lab, min_obs=MIN_OBS):
    """Verbatim from expanding_mean_panel.py (orientation: deficit/surplus)."""
    dlag, y = z[:-1], z[1:]
    npl, nmi = int(lab.sum()), int((~lab).sum())
    if npl < min_obs or nmi < min_obs:
        return np.nan, npl, nmi
    v = {}
    for reg, sel in [(1, lab), (0, ~lab)]:
        X = sm.add_constant(dlag[sel])
        v[reg] = np.var(sm.OLS(y[sel], X).fit().resid, ddof=2)
    if v[0] <= 0:
        return np.nan, npl, nmi
    return (v[0] / v[1]) ** 0.25, npl, nmi


def simulate(T, lam_true, drift=0.0, rho=RHO_DEFAULT, sigma=SIGMA_DEFAULT, rng=None):
    """
    Log capital ratio = deterministic drift + stationary AR(1) whose
    innovation scale is lam_true when the state is below its own long-run
    centre and 1 when above. lam_true = 1 gives a symmetric null.
    """
    rng = rng or np.random.default_rng()
    x = np.zeros(T)
    for t in range(1, T):
        below = x[t - 1] < 0
        scale = sigma * (lam_true if below else 1.0)
        x[t] = rho * x[t - 1] + rng.normal(0, scale)
    return x + drift * np.arange(T)


def ref_expanding(series):
    R = np.full_like(series, np.nan)
    for t in range(1, len(series)):
        R[t] = np.mean(series[:t])
    return series - R


def ref_fullsample(series):
    return series - np.mean(series)


def ref_truestate(series, drift, T):
    """Benchmark: classify on the stationary component, which is observable
    only in simulation. This is the ceiling any construction could reach."""
    return series - drift * np.arange(T)


def run(lam_true, drift, T=T_DEFAULT, reps=REPS, seed=0):
    rng = np.random.default_rng(seed)
    out = {k: [] for k in ("fullsample", "expanding", "truestate")}
    degen = {k: 0 for k in out}
    for _ in range(reps):
        s = simulate(T, lam_true, drift=drift, rng=rng)
        for name, z_full in (
            ("fullsample", ref_fullsample(s)),
            ("expanding", ref_expanding(s)),
            ("truestate", ref_truestate(s, drift, T)),
        ):
            z = z_full[BURNIN:]
            if np.any(np.isnan(z)):
                z = z[~np.isnan(z)]
            lab = z[:-1] >= 0
            lam, npl, nmi = variance_ratio_lambda(z, lab)
            if np.isfinite(lam):
                out[name].append(lam)
            else:
                degen[name] += 1
    return out, degen


def summarise(name, values, reps, truth):
    if not values:
        return f"  {name:<12} no estimate produced in any of {reps} reps"
    v = np.array(values)
    return (f"  {name:<12} n={len(v):>3}/{reps}  mean={v.mean():.4f}  "
            f"median={np.median(v):.4f}  sd={v.std(ddof=1):.4f}  "
            f"bias={v.mean() - truth:+.4f}")


results = {}
print("=" * 72)
print("PART A -- RECOVERY (stationary, known true lambda)")
print("=" * 72)
for lam_true in (1.0, 1.3, 1.6):
    out, degen = run(lam_true, drift=0.0, seed=11)
    print(f"true lambda = {lam_true}")
    for k in ("truestate", "fullsample", "expanding"):
        print(summarise(k, out[k], REPS, lam_true))
    results[("A", lam_true)] = out
    print()

print("=" * 72)
print("PART B -- CLEAN NULL (stationary, true lambda = 1)")
print("=" * 72)
out_b, degen_b = run(1.0, drift=0.0, seed=22)
for k in ("truestate", "fullsample", "expanding"):
    print(summarise(k, out_b[k], REPS, 1.0))
fs_b = np.array(out_b["fullsample"])
b_ok = len(fs_b) > 0 and abs(fs_b.mean() - 1.0) < PASS_BIAS
print(f"\n{'B1' if b_ok else 'B1*'}  {'PASS' if b_ok else 'FAIL'}  "
      f"full-sample-mean reference does not manufacture asymmetry at a "
      f"stationary null (|bias| < {PASS_BIAS})")
print()

print("=" * 72)
print("PART C -- TRENDED NULL (upward drift, true lambda = 1)  ** THE GATE **")
print("=" * 72)
print("An upward-drifting ratio with symmetric shocks. Any departure from")
print("lambda_hat = 1 here is manufactured by the reference construction,")
print("not by the data-generating process.")
print()
trend_rows = []
for drift in (0.000, 0.002, 0.005, 0.010, 0.020):
    out_c, degen_c = run(1.0, drift=drift, seed=33)
    fs = np.array(out_c["fullsample"]) if out_c["fullsample"] else np.array([])
    ex = np.array(out_c["expanding"]) if out_c["expanding"] else np.array([])
    trend_rows.append((drift, fs, ex, degen_c))
    print(f"drift = {drift:.3f} per quarter "
          f"({100 * drift * 4:.1f}% per year in log ratio)")
    for k in ("truestate", "fullsample", "expanding"):
        print(summarise(k, out_c[k], REPS, 1.0))
    print()

clean_bias = abs(fs_b.mean() - 1.0) if len(fs_b) else np.inf
worst_trend_bias = 0.0
for drift, fs, ex, _ in trend_rows:
    if len(fs):
        worst_trend_bias = max(worst_trend_bias, abs(fs.mean() - 1.0))
c_ok = worst_trend_bias - clean_bias < PASS_TREND_GAP
print(f"clean-null |bias| = {clean_bias:.4f};  worst trended |bias| = "
      f"{worst_trend_bias:.4f};  extra attributable to trend = "
      f"{worst_trend_bias - clean_bias:+.4f}")
print(f"{'C1' if c_ok else 'C1*'}  {'PASS' if c_ok else 'FAIL'}  "
      f"a trend adds less than {PASS_TREND_GAP} to the null bias of the "
      f"full-sample-mean reference")
print()

print("=" * 72)
print("PART D -- DEGENERACY (same trended data, which construction yields")
print("          an estimate at all?)")
print("=" * 72)
print(f"  {'drift':>7}  {'full-sample mean':>18}  {'expanding mean':>16}")
for drift, fs, ex, degen in trend_rows:
    print(f"  {drift:>7.3f}  {len(fs):>10}/{REPS:<7}  {len(ex):>9}/{REPS:<6}")
print()
print("The expanding-mean column is the simulated counterpart of the real")
print("panel's attrition: as drift rises it stops producing estimates,")
print("which is why advanced banking systems drop out of the primary")
print("analysis. The full-sample column is the proposed remedy.")
print()

print("=" * 72)
print("VERDICT")
print("=" * 72)
if b_ok and c_ok:
    print("Both gates pass. The full-sample-mean reference does not")
    print("manufacture asymmetry from a trend at these drift rates, so the")
    print("secondary analysis may proceed to real data. Its look-ahead cost")
    print("remains and must still be stated wherever its numbers appear.")
else:
    print("A gate FAILED. The full-sample-mean reference manufactures")
    print("asymmetry that the data-generating process does not contain.")
    print("Do not apply it to real data and do not quote any number from")
    print("it; the construction needs rethinking, not rerunning.")
