"""
Apply the SEGMENTED expanding-mean reference to the real 77-country panel,
using the sourced Basel III capital conservation buffer full-effect date
(1 Jan 2019 -- BIS's own international schedule, confirmed directly for
South Africa via SARB's Banks Act Circular 4 of 2016, and consistent with
Indonesia's OJK Reg. 11/2016).

This is the first REAL-DATA application of the segmentation mechanism
already validated by simulation (cuts degeneracy ~40%->22%, does NOT by
itself fix the downward bias in lambda_hat -- that still needs the AR-style
bias correction applied afterward, on the resulting shorter windows).

MALDIVES (MV) IS FLAGGED, not silently assumed to match: it is not a BCBS
member and no country-specific adoption date was found. Its output is
still produced but should be read with that caveat attached.
"""
import requests
import numpy as np
import statsmodels.api as sm
from panel_universe import sovereign_universe

# Not verified against a source -- flag rather than silently trust:
UNVERIFIED_BASEL_DATE = {'MV'}

BASEL_BREAK = '2019-Q1'   # sourced: BIS schedule + SARB direct confirmation
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
    docs = data["series"]["docs"]
    num_found = data["series"].get("num_found", len(docs))
    if num_found > len(docs):
        raise RuntimeError(
            f"{indicator_code}: source reports {num_found} matching series "
            f"but the query returned only {len(docs)} -- paginate before "
            f"trusting this fetch, do not silently truncate the universe.")
    out = {}
    for s in data["series"]["docs"]:
        area = s["dimensions"]["REF_AREA"]
        periods = s.get("period", [])
        values = s.get("value", [])
        if periods:
            out[area] = (periods, values)
    return out


def qidx(period_str):
    y, q = period_str.split('-Q')
    return int(y) * 4 + int(q)


def segmented_expanding_ref(log_ratio, periods, break_period, burnin=BURNIN):
    """Restart the expanding mean at the Basel break. Pre-break quarters
    keep their own continuous expanding mean; post-break quarters get a
    FRESH expanding mean starting from the break. Burn-in applied at the
    start AND right after the break."""
    n = len(log_ratio)
    break_idx = None
    bq = qidx(break_period)
    for i, p in enumerate(periods):
        if qidx(p) >= bq:
            break_idx = i
            break
    if break_idx is None or break_idx < 5 or (n - break_idx) < 5:
        # break falls outside (or too near the edge of) this country's
        # window -- segmentation isn't meaningful here, fall back to
        # continuous
        R = np.full(n, np.nan)
        for t in range(1, n):
            R[t] = np.mean(log_ratio[:t])
        z = log_ratio - R
        keep = np.ones(n, dtype=bool)
        keep[:burnin] = False
        return z[keep], False

    R = np.full(n, np.nan)
    for t in range(1, n):
        if t < break_idx:
            R[t] = np.mean(log_ratio[:t])
        else:
            start = break_idx
            R[t] = np.mean(log_ratio[start:t]) if t > start else log_ratio[start]
    z = log_ratio - R
    keep = np.ones(n, dtype=bool)
    keep[:burnin] = False
    post_burn_end = min(break_idx + burnin, n)
    keep[break_idx:post_burn_end] = False
    return z[keep], True


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


def phi_hats(z, lab, min_obs=MIN_OBS):
    dlag, y = z[:-1], z[1:]
    npl, nmi = int(lab.sum()), int((~lab).sum())
    phi_p = phi_m = np.nan
    if npl >= min_obs:
        phi_p = sm.OLS(y[lab], dlag[lab].reshape(-1, 1)).fit().params[0]
    if nmi >= min_obs:
        phi_m = sm.OLS(y[~lab], dlag[~lab].reshape(-1, 1)).fit().params[0]
    return phi_p, phi_m


print(f"Segmenting at {BASEL_BREAK} (sourced: BIS schedule + SARB direct confirmation)")
print("Fetching FSKRC_PT...")
car = fetch_full("FSKRC_PT")
UNIVERSE, EXCLUDED_NONSOVEREIGN = sovereign_universe(car.keys())
print(f"areas reporting FSKRC_PT: {len(car)}  sovereign universe: {len(UNIVERSE)}  "
      f"excluded as non-sovereign: {len(EXCLUDED_NONSOVEREIGN)}")

rows, skipped = [], []
for area in UNIVERSE:
    if area not in car:
        skipped.append((area, "not in fetch")); continue
    periods, values = car[area]
    vals = np.array([v if v is not None else np.nan for v in values], dtype=float)
    log_ratio = np.log(np.maximum(vals, 1e-6))
    if len(log_ratio) < BURNIN + 2 * MIN_OBS:
        skipped.append((area, f"too short: n={len(log_ratio)}")); continue

    z, was_segmented = segmented_expanding_ref(log_ratio, periods, BASEL_BREAK)
    lab = z[:-1] >= 0
    lam, npl, nmi = variance_ratio_lambda(z, lab)
    phi_p, phi_m = phi_hats(z, lab)

    rows.append(dict(area=area, n=len(z), n_surplus=npl, n_deficit=nmi,
                     lambda_hat=lam, phi_plus_hat=phi_p, phi_minus_hat=phi_m,
                     segmented=was_segmented,
                     unverified_date=area in UNVERIFIED_BASEL_DATE))

print(f"\nusable: {len(rows)}   skipped: {len(skipped)}")

print(f"\n{'area':>5}{'seg?':>6}{'n':>5}{'n_sur':>7}{'n_def':>7}{'lambda_hat':>12}"
      f"{'phi-_hat':>10}   flag")
n_degenerate_before_after = {'before': 0, 'after': 0}
for r in sorted(rows, key=lambda d: d['area']):
    flag = ""
    if r['unverified_date']:
        flag += "UNVERIFIED Basel date "
    if not np.isfinite(r['lambda_hat']):
        flag += "degenerate"
        n_degenerate_before_after['after'] += 1
    elif min(r['n_surplus'], r['n_deficit']) < 20:
        flag += "thin"
    print(f"{r['area']:>5}{('Y' if r['segmented'] else 'n'):>6}{r['n']:>5}"
          f"{r['n_surplus']:>7}{r['n_deficit']:>7}{r['lambda_hat']:>12.4f}"
          f"{r['phi_minus_hat']:>10.4f}   {flag}")

lams = np.array([r['lambda_hat'] for r in rows if np.isfinite(r['lambda_hat'])])
print(f"\nnon-degenerate: {len(lams)} of {len(rows)}  "
      f"(compare to 26 of 77 under the CONTINUOUS, unsegmented construction)")
print(f"lambda_hat: median={np.median(lams):.3f}  mean={np.mean(lams):.3f}  "
      f"IQR=[{np.percentile(lams,25):.3f}, {np.percentile(lams,75):.3f}]")
print(f"fraction with lambda_hat > 1: {(lams>1).mean():.2f}")
print("\nRemember: segmentation is validated to help ACCESS (more usable")
print("countries), NOT to fix the downward BIAS in lambda_hat by itself.")
print("The AR-style bias correction still needs applying next.")
