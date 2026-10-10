"""
Test one specific hypothesis about where FINAL_77 came from.

HYPOTHESIS. The panel universe was built for the cross-sectional test of
phi^- = 1 - (ROE - g), which needs the capital ratio (FSKRC_PT), return on
equity (FSERE_PT) and WEO nominal growth per country. FINAL_77 would then
be the set of areas reporting BOTH FSI series -- a correct constraint for
that test -- which was later reused unchanged for the lambda_hat estimator,
which needs FSKRC_PT alone. On this hypothesis the excluded-but-available
areas (France, UK, Belgium, Sweden, Switzerland, India, Spain, Luxembourg,
Russia, Singapore) are areas that report capital ratios but not ROE.

PREDICTION, stated before the fetch: FINAL_77 == FSKRC_PT areas INTERSECT
FSERE_PT areas, and every one of the ten named above is absent from
FSERE_PT.

If the intersection reproduces FINAL_77 exactly, the provenance is
established and the exclusion is stale-constraint reuse, not arbitrary
selection. If it does not, this hypothesis is wrong and the residual --
printed below -- is what still needs explaining.

Computes no lambda. Run:  python3 universe_provenance_test.py
"""
import requests

FINAL_77 = ['AE','AL','AM','AO','AR','AU','BA','BN','BO','BR','BT','BW','BY','CA','CL','CO',
'CR','CZ','DJ','EC','EE','FI','FJ','GE','GH','GM','GT','HK','HN','HR','HU','ID','KE','KG','KH',
'KR','LK','LS','LT','LV','MD','ME','MK','MO','MT','MU','MV','MX','MY','NA','NG','NI','NL','NO',
'PA','PE','PG','PH','PL','PS','PY','RO','RW','SA','SB','SK','SZ','TH','TJ','TO','TR','TZ','UG',
'US','XK','ZA','ZM']

PREDICTED_ABSENT_FROM_ROE = ['FR', 'GB', 'BE', 'SE', 'CH', 'IN', 'ES', 'LU', 'RU', 'SG']


def areas_for(indicator_code):
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
    found = data["series"].get("num_found", len(docs))
    if found > len(docs):
        print(f"  *** WARNING: {indicator_code} truncated -- {found} matching, "
              f"{len(docs)} returned. Paginate before trusting this. ***")
    return {s["dimensions"]["REF_AREA"] for s in docs if s.get("period")}


capital = areas_for("FSKRC_PT")
roe = areas_for("FSERE_PT")

print(f"areas reporting FSKRC_PT (capital ratio) : {len(capital)}")
print(f"areas reporting FSERE_PT (return on equity): {len(roe)}")

both = capital & roe
listed = set(FINAL_77)

print(f"areas reporting BOTH                      : {len(both)}")
print(f"FINAL_77 size                             : {len(listed)}")
print()

if both == listed:
    print("HYPOTHESIS CONFIRMED: FINAL_77 is exactly the FSKRC_PT & FSERE_PT")
    print("intersection. The universe was inherited from the phi^- test, whose")
    print("ROE requirement does not apply to the lambda_hat estimator. The")
    print("exclusion is a stale constraint, and dropping the ROE requirement")
    print("for the lambda_hat panel is a correction, not a new design choice.")
else:
    print("HYPOTHESIS NOT CONFIRMED. Residual to explain:")
    print(f"  in the intersection but NOT in FINAL_77 ({len(both - listed)}): "
          f"{sorted(both - listed)}")
    print(f"  in FINAL_77 but NOT in the intersection ({len(listed - both)}): "
          f"{sorted(listed - both)}")
    print()
    print("  A small residual may mean an additional WEO-growth requirement was")
    print("  also applied. A large or unstructured residual means the list has")
    print("  some other origin entirely and cannot be reconstructed.")

print()
print("The ten excluded-but-available areas, against the ROE requirement:")
for code in PREDICTED_ABSENT_FROM_ROE:
    has_cap = code in capital
    has_roe = code in roe
    if has_cap and not has_roe:
        verdict = "capital yes, ROE NO  -- consistent with the hypothesis"
    elif has_cap and has_roe:
        verdict = "capital yes, ROE yes -- INCONSISTENT, unexplained exclusion"
    else:
        verdict = "no capital series    -- unexpected, check the diagnostic"
    print(f"  {code}: {verdict}")
