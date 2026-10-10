"""
Determine the ACTUAL available country universe for FSKRC_PT, and diff it
against the hardcoded FINAL_77 used by expanding_mean_panel.py.

Motivation: FINAL_77 is a hardcoded list whose provenance is not recorded
in any design document in the bundle. Five of the seven largest advanced
economies (FR, DE, IT, JP, GB) are absent from it. This script determines
whether they were ABSENT FROM THE SOURCE (a genuine coverage gap, which
is a data fact to be documented) or merely ABSENT FROM THE LIST (a
selection step with no recorded rationale, which must be either justified
or removed).

It computes NOTHING about lambda. It only reports availability and
history length, so that no result can be tuned to it.

Run:  python3 universe_diagnostic.py
"""
import requests
import numpy as np

FINAL_77 = ['AE','AL','AM','AO','AR','AU','BA','BN','BO','BR','BT','BW','BY','CA','CL','CO',
'CR','CZ','DJ','EC','EE','FI','FJ','GE','GH','GM','GT','HK','HN','HR','HU','ID','KE','KG','KH',
'KR','LK','LS','LT','LV','MD','ME','MK','MO','MT','MU','MV','MX','MY','NA','NG','NI','NL','NO',
'PA','PE','PG','PH','PL','PS','PY','RO','RW','SA','SB','SK','SZ','TH','TJ','TO','TR','TZ','UG',
'US','XK','ZA','ZM']

G7 = {'CA': 'Canada', 'FR': 'France', 'DE': 'Germany', 'IT': 'Italy',
      'JP': 'Japan', 'GB': 'United Kingdom', 'US': 'United States'}

G10_EXTRA = {'BE': 'Belgium', 'NL': 'Netherlands', 'SE': 'Sweden',
             'CH': 'Switzerland'}

BURNIN = 12
MIN_OBS = 15
MIN_HISTORY = BURNIN + 2 * MIN_OBS


def fetch_all_areas(indicator_code="FSKRC_PT"):
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
    total = data["series"].get("num_found", len(docs))
    out = {}
    for s in docs:
        area = s["dimensions"]["REF_AREA"]
        periods = s.get("period", [])
        values = s.get("value", [])
        if periods:
            clean = [v for v in values if v is not None and not (isinstance(v, float) and np.isnan(v))]
            out[area] = len(clean)
    return out, len(docs), total


available, n_docs, num_found = fetch_all_areas()

print(f"series returned by the query: {n_docs}")
print(f"series the source reports as matching (num_found): {num_found}")
if num_found > n_docs:
    print(f"  *** WARNING: the limit=1000 page did NOT return everything. "
          f"{num_found - n_docs} series were truncated. Paginate before "
          f"drawing any conclusion from this diagnostic. ***")
print(f"distinct REF_AREA codes available: {len(available)}")
print()

avail = set(available)
listed = set(FINAL_77)

print(f"in FINAL_77 and available     : {len(listed & avail)}")
print(f"in FINAL_77 but NOT available : {len(listed - avail)}  {sorted(listed - avail)}")
print(f"AVAILABLE BUT NOT IN FINAL_77 : {len(avail - listed)}")
print()

extra = sorted(avail - listed, key=lambda a: -available[a])
if extra:
    print("Available-but-unused areas, by usable observation count:")
    print(f"  {'area':<6} {'n_obs':>6}  {'enough history?':<16}")
    for a in extra:
        n = available[a]
        verdict = "yes" if n >= MIN_HISTORY else f"no (< {MIN_HISTORY})"
        label = G7.get(a, G10_EXTRA.get(a, ""))
        print(f"  {a:<6} {n:>6}  {verdict:<16} {label}")
    print()

print("Specifically, the largest advanced economies:")
for code, name in {**G7, **G10_EXTRA}.items():
    if code in avail:
        n = available[code]
        status = "IN FINAL_77" if code in listed else "*** AVAILABLE BUT EXCLUDED FROM FINAL_77 ***"
        enough = "sufficient history" if n >= MIN_HISTORY else f"only {n} obs, under the {MIN_HISTORY} minimum"
        print(f"  {code} {name:<16} available, {n:>3} obs, {enough:<38} {status}")
    else:
        print(f"  {code} {name:<16} NOT AVAILABLE IN SOURCE (genuine coverage gap)")

print()
print("READING THIS OUTPUT")
print("  If the missing economies appear as AVAILABLE BUT EXCLUDED, then")
print("  FINAL_77 is a selection step with no recorded rationale, and the")
print("  panel must be rebuilt on a stated, reproducible universe rule.")
print("  If they appear as NOT AVAILABLE, the exclusion is a data fact and")
print("  belongs in the design document as one -- with the consequence that")
print("  the panel cannot speak to the deepest banking systems at all.")
