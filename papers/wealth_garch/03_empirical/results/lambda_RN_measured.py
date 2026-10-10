"""
The model null on the MEASURED scale.

WHY THIS EXISTS. lambda_RN_panelconv.m computes the model's null by
simulating 540,000 near-continuous states and taking the ensemble mean of
the local diffusion variance in each regime. That is the model's TRUE
lambda_V -- a population quantity, computed without an estimator.

The panel figure it was being compared against is not that. The panel fits
an AR(1) within each regime on roughly sixty quarterly observations and
takes the ratio of residual variances. That estimator is biased toward 1,
and the bias does NOT vanish with sample size: a process with true
lambda = 1.30 measures about 1.16 at T=60 and still about 1.14 at
T=20,000, because states near the classification boundary are assigned to
a regime while their innovations reflect a mixture of both. It is a
different estimand, not a small-sample artefact.

Comparing a population null against an attenuated estimate overstates the
gap. This script removes the mismatch the only way that is sound: it
simulates the solved model, samples it at QUARTERLY frequency over a
PANEL-LENGTH window, and applies THE PANEL'S OWN ESTIMATOR, imported
rather than reimplemented. What comes out is what a bank that obeys the
model exactly would be measured at, by this estimator, on data like ours.

That makes the null a DISTRIBUTION rather than a point, which is also more
honest: it shows how much of the cross-country spread in the real panel is
compatible with a single model bank observed for fifteen years.

lambda_RN_panelconv.m is not modified. Both quantities are wanted -- the
population value for theory, this one for comparison with data.

Run:  python3 lambda_RN_measured.py
"""

import sys
from pathlib import Path

import numpy as np
import scipy.io as sio

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "design"))
from panel_estimator import variance_ratio_lambda, expanding_ref, BURNIN, MIN_OBS

RESULTS = Path(__file__).resolve().parent
LAMBDAS = ["1.0000", "1.1000", "1.2500", "1.5000", "1.7500", "2.0000"]

QUARTERS = 60            # panel-typical length, before burn-in
DT = 2.5e-4              # years per step; 1000 steps per quarter
STEPS_PER_QUARTER = int(round(0.25 / DT))
BURN_YEARS = 40          # discarded before sampling begins
REPS = 400
SEED = 20260811


def drift_and_var(x, pi, p):
    mu_pi = (1.0 - pi) * p["r"] + pi * p["mu"]
    b = x * (mu_pi - p["mu_L"]) + p["gamma"]
    s2 = (pi ** 2 * p["sigma"] ** 2 * x ** 2
          + 2 * pi * p["c"] * p["sigma"] * p["sigma_L"] * x * (1 - x)
          + p["sigma_L"] ** 2 * (1 - x) ** 2)
    return b, np.maximum(s2, 0.0)


def load(lam):
    m = sio.loadmat(RESULTS / f"resultM_lambda_{lam}.mat")
    sol = m["sol"]
    p = {f: float(m["params"][f][0, 0].ravel()[0])
         for f in m["params"].dtype.names if m["params"][f][0, 0].size}
    return dict(
        y=sol["y"][0, 0].ravel(),
        pi_star=sol["pi_star"][0, 0].ravel(),
        y_star=float(sol["y_star"][0, 0].ravel()[0]),
        recap_edge=float(m["recap_edge"].ravel()[0]),
        y_post=float(m["y_post"].ravel()[0]),
        p=p,
    )


def simulate_quarterly(mod, reps, quarters, rng):
    """Return (reps, quarters) array of x sampled at quarter ends."""
    y, pi_star = mod["y"], mod["pi_star"]
    dy = y[1] - y[0]
    xL, xT, ystar = mod["recap_edge"], mod["y_post"], mod["y_star"]
    p = mod["p"]

    x = np.full(reps, 0.5 * (xL + ystar))
    burn_steps = int(BURN_YEARS / DT)
    total = burn_steps + quarters * STEPS_PER_QUARTER
    out = np.empty((reps, quarters))
    sqdt = np.sqrt(DT)
    q = 0
    for k in range(total):
        idx = np.clip(np.round((x - y[0]) / dy).astype(int), 0, len(y) - 1)
        pi = pi_star[idx]
        b, s2 = drift_and_var(x, pi, p)
        x = x + b * DT + np.sqrt(s2) * sqdt * rng.standard_normal(reps)
        x = np.where(x <= xL, xT, x)
        x = np.where(x >= ystar, ystar - 1e-12, x)
        if k >= burn_steps:
            if (k - burn_steps + 1) % STEPS_PER_QUARTER == 0:
                out[:, q] = x
                q += 1
    return out


def estimate(paths, convention):
    """Apply the panel's own estimator, expanding-mean reference."""
    lams = []
    for x in paths:
        if convention == "a":
            w = np.log(np.maximum(x - 1.0, 1e-12))
        else:
            w = np.log(np.maximum((x - 1.0) / x, 1e-12))
        z = expanding_ref(w)[BURNIN:]
        z = z[~np.isnan(z)]
        if len(z) < 2 * MIN_OBS + 1:
            continue
        lab = z[:-1] >= 0
        lam, _, _ = variance_ratio_lambda(z, lab)
        if np.isfinite(lam):
            lams.append(lam)
    return np.array(lams)


PANEL_MEDIAN = 1.038

print("Model null on the MEASURED scale: the panel estimator applied to")
print(f"model paths of {QUARTERS} quarters, {REPS} independent paths per lambda_S.")
print(f"(dt={DT}, {STEPS_PER_QUARTER} steps/quarter, {BURN_YEARS}y burn-in,")
print(f" BURNIN={BURNIN}, MIN_OBS={MIN_OBS}, estimator imported from the panel)")
print()

rng = np.random.default_rng(SEED)
print(f"{'lambda_S':>9} {'conv':>5} {'n':>5} {'median':>9} {'mean':>9} "
      f"{'sd':>8} {'5th':>8} {'95th':>8}")
table = {}
for lam in LAMBDAS:
    mod = load(lam)
    paths = simulate_quarterly(mod, REPS, QUARTERS, rng)
    for conv in ("a", "b"):
        est = estimate(paths, conv)
        if len(est) == 0:
            print(f"{lam:>9} {conv:>5} {'0':>5}   no estimate produced")
            continue
        table[(lam, conv)] = est
        print(f"{lam:>9} {conv:>5} {len(est):>5} {np.median(est):>9.4f} "
              f"{est.mean():>9.4f} {est.std(ddof=1):>8.4f} "
              f"{np.percentile(est, 5):>8.4f} {np.percentile(est, 95):>8.4f}")

print()
print("COMPARISON")
print(f"  panel median (corrected universe, n=33): {PANEL_MEDIAN:.4f}")
allm = [np.median(v) for v in table.values()]
if allm:
    print(f"  lowest measured-scale null median anywhere: {min(allm):.4f}")
    print(f"  gap on the measured scale: {PANEL_MEDIAN - min(allm):+.4f}")
    below = [k for k, v in table.items() if np.median(v) <= PANEL_MEDIAN]
    print(f"  (lambda_S, convention) cells whose null median is at or below "
          f"the panel median: {below if below else 'none'}")
    pooled = np.concatenate(list(table.values()))
    print(f"  fraction of ALL simulated model banks measuring at or below "
          f"{PANEL_MEDIAN}: {(pooled <= PANEL_MEDIAN).mean():.3f}")
