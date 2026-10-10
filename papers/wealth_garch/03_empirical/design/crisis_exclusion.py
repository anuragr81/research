"""
Apply the SOURCED Laeven-Valencia (2026 update, IMF WP/26/94) crisis
windows to the real 77-country panel, on the CONTINUOUS (unsegmented)
expanding-mean construction -- per his decision to explore the remaining
exclusion grounds before returning to Basel III as a regression-level
control.

Only the 11 countries with a crisis genuinely overlapping their FSI sample
window are affected (found by cross-referencing Table A1 against each
country's actual coverage, not just crisis existence):
  AO 2016-2020, GH 2017-2021, HU 2008-2012, LK 2023-2025 (borderline),
  LV 2008-2012, MD 2014-2018, NG 2009-2012, NI 2018-2020 (borderline),
  NL 2008-2009, TJ 2016-2018, US 2007-2011

PROTOCOL: report the classification and lambda_hat/phi_minus_hat for these
11 countries BOTH with the crisis quarters included and excluded, side by
side. Do not report only the version that "looks better" -- that is what
pre-registration is for.

DIRECTIONAL WARNING carried over from the exclusion-criteria document:
excluding a recapitalisation episode is NOT neutral. A recapitalised
deficit closes fast without being earned, which pushes the observed
phi^-_hat DOWN; removing those quarters therefore RAISES estimated
persistence and flatters the model. This is the opposite of a
conservative adjustment and must be reported as such, not glossed over.
"""
import requests
import numpy as np
import statsmodels.api as sm

CRISIS_WINDOWS = {
    'AO': (2016, 2020), 'GH': (2017, 2021), 'HU': (2008, 2012),
    'LK': (2023, 2025), 'LV': (2008, 2012), 'MD': (2014, 2018),
    'NG': (2009, 2012), 'NI': (2018, 2020), 'NL': (2008, 2009),
    'TJ': (2016, 2018), 'US': (2007, 2011),
}
BORDERLINE = {'LK', 'NI'}  # flagged by Laeven-Valencia as not fully meeting
                             # their own definition -- report separately

BURNIN = 12
MIN_OBS = 15


def fetch_full(indicator_code):
    url = "https://api.db.nomics.world/v22/series/IMF/FSI"
    params = {
        "dimensions": f'{{"FREQ":["Q"],"INDICATOR":["{indicator_code}"]}}',
        "observations": 1,
        "limit": 1000,
    }
    r = requests.get(url, params=params, timeout=60)
    r.raise_for_status()
    data = r.json()
    out = {}
    for s in data["series"]["docs"]:
        area = s["dimensions"]["REF_AREA"]
        periods = s.get("period", [])
        values = s.get("value", [])
        if periods:
            out[area] = (periods, values)
    return out


def year_of(period_str):
    return int(period_str.split('-Q')[0])


def continuous_expanding_ref(log_ratio, burnin=BURNIN):
    R = np.full_like(log_ratio, np.nan)
    for t in range(1, len(log_ratio)):
        R[t] = np.mean(log_ratio[:t])
    return (log_ratio - R), R


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
    return (v[1] / v[0]) ** 0.25, npl, nmi


def phi_minus(z, lab, min_obs=MIN_OBS):
    dlag, y = z[:-1], z[1:]
    if (~lab).sum() < min_obs:
        return np.nan
    return sm.OLS(y[~lab], dlag[~lab].reshape(-1, 1)).fit().params[0]


print("Fetching FSKRC_PT...")
car = fetch_full("FSKRC_PT")

print(f"\n{'area':>5}{'window':>12}{'lambda INCL':>13}{'lambda EXCL':>13}"
      f"{'phi- INCL':>11}{'phi- EXCL':>11}   flag")

for area, (cs, ce) in CRISIS_WINDOWS.items():
    if area not in car:
        print(f"{area:>5}  not in fetch"); continue
    periods, values = car[area]
    vals = np.array([v if v is not None else np.nan for v in values], dtype=float)
    log_ratio = np.log(np.maximum(vals, 1e-6))

    z_full, R_full = continuous_expanding_ref(log_ratio)
    yrs = np.array([year_of(p) for p in periods])

    # INCLUDING crisis quarters (status quo)
    z_incl = z_full[BURNIN:]
    lab_incl = z_incl[:-1] >= 0
    lam_incl, npl_i, nmi_i = variance_ratio_lambda(z_incl, lab_incl)
    phim_incl = phi_minus(z_incl, lab_incl)

    # EXCLUDING crisis quarters -- drop them from the REGRESSION SAMPLE
    # (the reference itself still uses the full history, so the exclusion
    # is on which (z_{t-1}, z_t) PAIRS contribute to the moment estimates,
    # not a re-estimation of the reference from a gapped series)
    crisis_mask_full = (yrs >= cs) & (yrs <= ce)
    keep_full = ~crisis_mask_full
    keep_full[:BURNIN] = False
    z_excl_pairs = z_full[keep_full]
    # need matching consecutive (t-1,t) pairs that are BOTH kept and BOTH
    # temporally adjacent in the original series -- build explicitly
    idx_keep = np.where(keep_full)[0]
    pair_mask = np.diff(idx_keep) == 1
    t_idx = idx_keep[1:][pair_mask]
    if len(t_idx) < 2 * MIN_OBS:
        print(f"{area:>5}{f'{cs}-{ce}':>12}{'--':>13}{'too few pairs':>13}"
              f"{'--':>11}{'--':>11}   {'BORDERLINE' if area in BORDERLINE else ''}")
        continue
    z_prev = z_full[t_idx - 1]
    z_next = z_full[t_idx]
    lab_excl = z_prev >= 0
    npl_e, nmi_e = int(lab_excl.sum()), int((~lab_excl).sum())
    lam_excl = np.nan
    if npl_e >= MIN_OBS and nmi_e >= MIN_OBS:
        v = {}
        for reg, sel in [(1, lab_excl), (0, ~lab_excl)]:
            X = sm.add_constant(z_prev[sel])
            v[reg] = np.var(sm.OLS(z_next[sel], X).fit().resid, ddof=2)
        if v[0] > 0:
            lam_excl = (v[1] / v[0]) ** 0.25
    phim_excl = np.nan
    if nmi_e >= MIN_OBS:
        X = sm.add_constant(z_prev[~lab_excl])
        phim_excl = sm.OLS(z_next[~lab_excl], X).fit().params[0]

    flag = "BORDERLINE (LV definition not fully met)" if area in BORDERLINE else ""
    def fmt(x): return f"{x:.4f}" if np.isfinite(x) else "degenerate"
    print(f"{area:>5}{f'{cs}-{ce}':>12}{fmt(lam_incl):>13}{fmt(lam_excl):>13}"
          f"{fmt(phim_incl):>11}{fmt(phim_excl):>11}   {flag}")

print("\nREADING THIS TABLE:")
print("  INCL = crisis quarters left in (status quo, what we've been using)")
print("  EXCL = crisis quarters dropped from the regime-conditional moments")
print("  Per the directional warning: EXCL should generally show HIGHER")
print("  phi-_hat than INCL, since recapitalisation-style fast closure is")
print("  removed. If EXCL comes back LOWER, that is worth a specific look")
print("  before trusting either number.")
