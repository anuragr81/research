"""
The theta-free variance-ratio estimator, in one place.

Both the panel (expanding_mean_panel.py) and the model null on the
measured scale (results/lambda_RN_measured.py) import from here. That is
not tidiness: the defect this module exists to prevent is exactly an
estimator mismatch between the two, where the null was a population
quantity computed without an estimator and the panel figure was an
attenuated finite-regime estimate, and the two were compared as though
commensurable.

Orientation follows Theorem 1: lambda_V^4 = deficit variance / surplus
variance.
"""

import numpy as np
import statsmodels.api as sm

BURNIN = 12
MIN_OBS = 15


def expanding_ref(log_ratio):
    """One-sided expanding mean of a series' own past. No look-ahead."""
    R = np.full_like(log_ratio, np.nan)
    for t in range(1, len(log_ratio)):
        R[t] = np.mean(log_ratio[:t])
    return log_ratio - R


def fullsample_ref(log_ratio):
    """Constant per-series reference. Uses look-ahead; see EMPIRICAL_v2 s4."""
    return log_ratio - np.mean(log_ratio)


def variance_ratio_lambda(z, lab, min_obs=MIN_OBS):
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
    # v[1] = variance where lab (z>=0, SURPLUS); v[0] = variance where
    # ~lab (z<0, DEFICIT). Theorem 1 defines lambda_V^4 as
    # deficit/surplus = v[0]/v[1]. An earlier version returned the
    # reciprocal, inverting every lambda_hat produced; fixed 8 Aug 2026.
    return (v[0] / v[1]) ** 0.25, npl, nmi


def phi_hats(z, lab, min_obs=MIN_OBS):
    dlag, y = z[:-1], z[1:]
    npl, nmi = int(lab.sum()), int((~lab).sum())
    phi_p = phi_m = np.nan
    if npl >= min_obs:
        phi_p = sm.OLS(y[lab], dlag[lab].reshape(-1, 1)).fit().params[0]
    if nmi >= min_obs:
        phi_m = sm.OLS(y[~lab], dlag[~lab].reshape(-1, 1)).fit().params[0]
    return phi_p, phi_m
