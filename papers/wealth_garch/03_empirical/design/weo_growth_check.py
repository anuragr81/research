"""
Reads the IMF WEO database bulk file (downloaded manually -- see instructions)
and computes each country's average real GDP growth over its OWN FSI sample
window (established from the FSKRC_PT / FSERE_PT panel already built).

SET THIS to wherever you saved the downloaded WEO file:
"""
WEO_FILE = "WEOOct2024all.xls"   # <-- change to your actual downloaded filename

import pandas as pd

# --- per-country (start_year, end_year), from the FSKRC_PT/FSERE_PT panel ---
WINDOWS = {
 'AE': (2009, 2024), 'AL': (2010, 2025), 'AM': (2010, 2024), 'AO': (2010, 2022),
 'AR': (2012, 2025), 'AU': (2006, 2024), 'BA': (2003, 2024), 'BN': (2010, 2023),
 'BO': (2010, 2024), 'BR': (2005, 2025), 'BT': (2010, 2024), 'BW': (2012, 2025),
 'BY': (2010, 2025), 'CA': (2005, 2025), 'CL': (2001, 2025), 'CO': (2005, 2025),
 'CR': (2008, 2024), 'CZ': (2007, 2025), 'DJ': (2012, 2025), 'EC': (2003, 2025),
 'EE': (2008, 2025), 'FI': (2007, 2025), 'FJ': (2005, 2022), 'GE': (2002, 2025),
 'GH': (2008, 2025), 'GM': (2005, 2023), 'GT': (2009, 2025), 'HK': (2008, 2025),
 'HN': (2010, 2025), 'HR': (2006, 2025), 'HU': (2008, 2024), 'ID': (2011, 2025),
 'KE': (2006, 2024), 'KG': (2010, 2024), 'KH': (2010, 2024), 'KR': (2009, 2024),
 'LK': (2012, 2024), 'LS': (2010, 2025), 'LT': (2008, 2023), 'LV': (2011, 2025),
 'MD': (2009, 2025), 'ME': (2006, 2024), 'MK': (2005, 2025), 'MO': (2010, 2025),
 'MT': (2005, 2025), 'MU': (2009, 2024), 'MV': (2012, 2025), 'MX': (2005, 2025),
 'MY': (2005, 2025), 'NA': (2010, 2025), 'NG': (2007, 2025), 'NI': (2008, 2024),
 'NL': (2008, 2025), 'NO': (2009, 2025), 'PA': (2005, 2025), 'PE': (2010, 2024),
 'PG': (2008, 2023), 'PH': (2009, 2025), 'PL': (2008, 2024), 'PS': (2008, 2024),
 'PY': (2005, 2025), 'RO': (2010, 2025), 'RW': (2008, 2024), 'SA': (2009, 2025),
 'SB': (2010, 2024), 'SK': (2011, 2025), 'SZ': (2009, 2025), 'TH': (2006, 2025),
 'TJ': (2008, 2021), 'TO': (2012, 2024), 'TR': (2005, 2025), 'TZ': (2011, 2023),
 'UG': (2005, 2024), 'US': (2009, 2024), 'XK': (2010, 2025), 'ZA': (2008, 2024),
 'ZM': (2007, 2024),
}

# ISO-2 (FSI convention) -> ISO-3 (WEO convention). FLAGGED entries are
# genuinely uncertain -- verify these four against the country list in your
# downloaded file before trusting their results: Hong Kong and Macao are
# SARs and may or may not appear as separate WEO entries; Kosovo's code
# varies by source (WEO commonly uses UVK); Palestine may not have a WEO
# growth series at all.
ISO2_TO_ISO3 = {
 'AE':'ARE','AL':'ALB','AM':'ARM','AO':'AGO','AR':'ARG','AU':'AUS','BA':'BIH',
 'BN':'BRN','BO':'BOL','BR':'BRA','BT':'BTN','BW':'BWA','BY':'BLR','CA':'CAN',
 'CL':'CHL','CO':'COL','CR':'CRI','CZ':'CZE','DJ':'DJI','EC':'ECU','EE':'EST',
 'FI':'FIN','FJ':'FJI','GE':'GEO','GH':'GHA','GM':'GMB','GT':'GTM',
 'HK':'HKG',   # FLAG: verify present in your file
 'HN':'HND','HR':'HRV','HU':'HUN','ID':'IDN','KE':'KEN','KG':'KGZ','KH':'KHM',
 'KR':'KOR','LK':'LKA','LS':'LSO','LT':'LTU','LV':'LVA','MD':'MDA','ME':'MNE',
 'MK':'MKD',
 'MO':'MAC',   # FLAG: verify present in your file
 'MT':'MLT','MU':'MUS','MV':'MDV','MX':'MEX','MY':'MYS','NA':'NAM','NG':'NGA',
 'NI':'NIC','NL':'NLD','NO':'NOR','PA':'PAN','PE':'PER','PG':'PNG','PH':'PHL',
 'PL':'POL',
 'PS':'PSE',   # FLAG: may not exist in WEO's growth table at all
 'PY':'PRY','RO':'ROU','RW':'RWA','SA':'SAU','SB':'SLB','SK':'SVK','SZ':'SWZ',
 'TH':'THA','TJ':'TJK','TO':'TON','TR':'TUR','TZ':'TZA','UG':'UGA','US':'USA',
 'XK':'UVK',   # FLAG: confirm this is the code your file actually uses
 'ZA':'ZAF','ZM':'ZMB',
}

# --- load and filter to the growth subject ---
if WEO_FILE.endswith(('.xls', '.xlsx')):
    df = pd.read_excel(WEO_FILE, dtype=str)
else:
    df = pd.read_csv(WEO_FILE, dtype=str, sep='\t', encoding='latin1')

print("columns found:", list(df.columns)[:10], "...")
assert 'WEO Subject Code' in df.columns, \
    "column name differs from expected -- print(df.columns) fully and check"

growth = df[df['WEO Subject Code'] == 'NGDP_RPCH'].copy()
year_cols = [c for c in growth.columns if str(c).strip().isdigit()]
for c in year_cols:
    growth[c] = pd.to_numeric(
        growth[c].astype(str).str.replace(',', ''), errors='coerce')
growth = growth.set_index('ISO')[year_cols]

results = {}
missing = []
for area, (y0, y1) in WINDOWS.items():
    iso3 = ISO2_TO_ISO3.get(area)
    if iso3 is None or iso3 not in growth.index:
        missing.append(area)
        continue
    yrs = [str(y) for y in range(y0, y1 + 1) if str(y) in growth.columns]
    vals = growth.loc[iso3, yrs].dropna()
    if len(vals):
        results[area] = vals.mean()
    else:
        missing.append(area)

if missing:
    print(f"\nCOULD NOT MATCH {len(missing)} countries: {missing}")
    print("(check the four FLAGGED codes above first, then any others)\n")

below = {a: v for a, v in results.items() if v < 1.5}
print(f"countries with usable growth data: {len(results)} of {len(WINDOWS)}")
print(f"average real GDP growth < 1.5%: {len(below)}\n")
for a, v in sorted(below.items(), key=lambda kv: kv[1]):
    print(f"  {a}: {v:.2f}%")

print("\nfull distribution, lowest to highest:")
for a, v in sorted(results.items(), key=lambda kv: kv[1]):
    print(f"  {a}: {v:.2f}%")
