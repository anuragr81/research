"""
Check whether the suspected shared-reporting clusters in the panel
universe (Eastern Caribbean-adjacent, CEMAC-adjacent -- flagged by
expanding_mean_panel.py on observation-count similarity alone) are
genuinely independent national series or near-duplicates of a shared
aggregate.

This does NOT decide inclusion. Per the standing rule, suspicion is not a
screen: the panel includes every sovereign area by default and reports
this check's result as a flag. If a cluster turns out to be duplicated
reporting, the fix is a stated exclusion with this evidence attached, not
a silent drop.

TEST: for each cluster, fetch the actual value series (not just observed
length) for every member and report the pairwise correlation and the
fraction of quarters where two members' values are numerically identical.
High correlation with near-identical values across every pair is strong
evidence of shared reporting; low correlation or materially different
values means the countries are reporting independently despite similar
history length, and the flag can be removed.

Computes no lambda. Run:  python3 panel_currency_union_check.py
"""
import requests
import numpy as np

CLUSTERS = {
    "Eastern Caribbean-adjacent": ['AI', 'AG', 'DM', 'GD', 'KN', 'LC', 'MS', 'VC'],
    "CEMAC-adjacent": ['CM', 'CF', 'CG', 'GA', 'GQ', 'TD'],
}


def fetch_full(indicator_code="FSKRC_PT"):
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
        raise RuntimeError(f"truncated fetch: {num_found} matching, "
                            f"{len(docs)} returned -- paginate first")
    out = {}
    for s in docs:
        area = s["dimensions"]["REF_AREA"]
        periods = s.get("period", [])
        values = s.get("value", [])
        if periods:
            out[area] = dict(zip(periods, values))
    return out


car = fetch_full()

for cluster_name, members in CLUSTERS.items():
    print(f"=== {cluster_name} ===")
    present = [m for m in members if m in car]
    missing = [m for m in members if m not in car]
    if missing:
        print(f"  not in source: {missing}")
    if len(present) < 2:
        print("  fewer than two members present -- nothing to compare")
        print()
        continue

    all_periods = sorted(set.intersection(*(set(car[m].keys()) for m in present)))
    if len(all_periods) < 5:
        print(f"  only {len(all_periods)} overlapping quarters across members "
              f"-- too few to judge")
        print()
        continue

    print(f"  {len(present)} members, {len(all_periods)} overlapping quarters")
    for i, a in enumerate(present):
        for b in present[i + 1:]:
            va = np.array([car[a].get(p, np.nan) for p in all_periods], dtype=float)
            vb = np.array([car[b].get(p, np.nan) for p in all_periods], dtype=float)
            both = ~(np.isnan(va) | np.isnan(vb))
            if both.sum() < 5:
                print(f"  {a}-{b}: too few jointly observed quarters")
                continue
            corr = np.corrcoef(va[both], vb[both])[0, 1]
            identical_frac = np.mean(np.isclose(va[both], vb[both], atol=1e-9))
            verdict = ("LIKELY SHARED REPORTING" if corr > 0.98 and identical_frac > 0.5
                        else "appears independent" if corr < 0.7
                        else "correlated but not identical -- inspect by hand")
            print(f"  {a}-{b}: corr={corr:+.3f}  identical={identical_frac:.0%}  "
                  f"-> {verdict}")
    print()

print("READING THIS OUTPUT")
print("  A cluster where every pair shows LIKELY SHARED REPORTING should be")
print("  collapsed to one observation (or excluded with that stated reason)")
print("  before treating its members as independent points in any median")
print("  or dispersion statistic. A cluster with independent-looking pairs")
print("  needs no exclusion -- the flag in expanding_mean_panel.py can be")
print("  removed with this output as the record of why.")
