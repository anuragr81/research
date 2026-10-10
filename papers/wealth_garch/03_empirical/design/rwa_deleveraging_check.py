"""
Does RWA contract in deficit quarters, and does that contraction get bigger
as trend growth falls? If yes, the model is missing a real channel (B2 only
allows earning back a deficit; it should also allow shrinking the
denominator) and the estimated slope on (ROE - g) is contaminated by an
omitted variable correlated with g.
"""
import requests
import numpy as np

COUNTRIES = ['AE','AL','AM','AO','AR','AU','BA','BN','BO','BR','BT','BW','BY','CA','CL','CO',
'CR','CZ','DJ','FI','FJ','GE','GH','GM','GT','HN','HU','ID','KE','KG','KH','KR','LK','LS','LT',
'MD','ME','MK','MO','MU','MV','MX','MY','NA','NG','NI','NO','PA','PE','PG','PH','PL','PY','RO',
'RW','SA','SB','SZ','TH','TJ','TO','TR','TZ','UG','ZA','ZM']

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
        if not periods:
            continue
        out[area] = dict(zip(periods, values))
    return out

print("Fetching RWA levels (FS_ODX_ARW_FSKRC_XDC)...")
rwa = fetch_full("FS_ODX_ARW_FSKRC_XDC")
print("Fetching buffer ratio (FSKRC_PT) for regime sign...")
car = fetch_full("FSKRC_PT")

G_NOMINAL = {
 'AE':4.30,'AL':5.51,'AM':8.32,'AO':19.81,'AR':58.81,'AU':5.67,'BA':6.44,'BN':1.89,
 'BO':7.17,'BR':9.40,'BT':10.39,'BW':7.27,'BY':22.22,'CA':4.40,'CL':8.76,'CO':9.10,
 'CR':7.89,'CZ':4.80,'DJ':6.83,'FI':2.63,'FJ':4.72,'GE':12.24,'GH':23.33,'GM':9.05,
 'GT':6.83,'HN':8.71,'HU':7.14,'ID':8.72,'KE':11.72,'KG':15.11,'KH':8.91,'KR':4.85,
 'LK':11.56,'LS':7.57,'LT':6.35,'MD':11.08,'ME':8.30,'MK':6.52,'MO':9.62,'MU':5.84,
 'MV':9.17,'MX':6.65,'MY':6.88,'NA':8.32,'NG':13.51,'NI':10.41,'NO':4.83,'PA':9.20,
 'PE':7.78,'PG':9.10,'PH':7.71,'PL':6.89,'PY':9.42,'RO':8.43,'RW':14.00,'SA':6.36,
 'SB':5.07,'SZ':7.69,'TH':4.75,'TJ':16.17,'TO':5.65,'TR':27.04,'TZ':11.77,'UG':12.98,
 'ZA':6.99,'ZM':16.12,
}

results = []
skipped = []
for area in COUNTRIES:
    if area not in rwa or area not in car:
        skipped.append(area); continue
    rwa_series = rwa[area]
    car_series = car[area]
    common = sorted(set(rwa_series) & set(car_series))
    if len(common) < 20:
        skipped.append(area); continue

    rwa_vals = np.array([rwa_series[p] for p in common], dtype=float)
    car_vals = np.array([car_series[p] for p in common], dtype=float)

    demeaned = car_vals - np.nanmean(car_vals)
    lab = demeaned[:-1] < 0
    log_rwa = np.log(np.maximum(rwa_vals, 1e-6))
    d_log_rwa = np.diff(log_rwa) * 100

    ok = np.isfinite(d_log_rwa) & np.isfinite(lab.astype(float))
    lab, d_log_rwa = lab[ok], d_log_rwa[ok]
    if lab.sum() < 8 or (~lab).sum() < 8:
        skipped.append(area); continue

    g_def = d_log_rwa[lab].mean()
    g_sur = d_log_rwa[~lab].mean()
    gap = g_sur - g_def
    results.append((area, g_def, g_sur, gap, G_NOMINAL.get(area), lab.sum(), (~lab).sum()))

print(f"\nusable: {len(results)}   skipped: {len(skipped)} -> {skipped}\n")
print(f"{'area':>5}{'RWA gr,deficit':>16}{'RWA gr,surplus':>16}{'gap':>8}{'g_nom':>8}{'n_def':>7}{'n_sur':>7}")
for a, gd, gs, gap, gn, nd, ns in sorted(results, key=lambda r: r[3], reverse=True):
    gn_str = f"{gn:.1f}" if gn is not None else "  n/a"
    print(f"{a:>5}{gd:>16.3f}{gs:>16.3f}{gap:>8.3f}{gn_str:>8}{nd:>7}{ns:>7}")

gaps = np.array([r[3] for r in results])
print(f"\nmean gap (surplus growth minus deficit growth): {gaps.mean():.3f}pp/qtr")
print(f"fraction of countries with gap > 0 (RWA grows slower in deficit): "
      f"{(gaps>0).mean():.2f}")

paired = [(gn, gap) for a,gd,gs,gap,gn,nd,ns in results if gn is not None]
if len(paired) >= 10:
    gn_arr = np.array([p[0] for p in paired]); gap_arr = np.array([p[1] for p in paired])
    corr = np.corrcoef(gn_arr, gap_arr)[0,1]
    print(f"\ncorr(country avg nominal growth, deficit-vs-surplus RWA gap) = {corr:.3f}")
    print("(negative => the gap widens as growth falls -- deleveraging is a")
    print(" bigger part of deficit-closing in slow-growth countries, which is")
    print(" exactly the missing-channel concern)")
