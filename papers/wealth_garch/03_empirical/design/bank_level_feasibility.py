"""
Feasibility probe for the empirical companion paper.

WHAT THE COMPANION NEEDS, and why this probe checks what it checks.

The theory paper's identification result says the preference parameter is
recoverable from the institution's committed THRESHOLDS -- the payout
barrier and the recapitalisation trigger -- and not from the volatility its
policy induces. Testing that on real institutions requires three things
per bank per quarter, at bank level, over a long window:

  1. a regulatory capital ratio            (the state variable)
  2. dividends declared                     (the payout barrier, revealed)
  3. capital issuance                       (the recapitalisation trigger,
                                             revealed)

Country aggregates cannot supply 2 and 3 at all, which is the deeper
reason the FSI panel was never going to carry this -- not merely that it
mixes regulatory and political shocks, but that the events the theory
predicts are invisible in an aggregate.

US Call Report data plausibly supplies all three, quarterly, free, for
every insured depository institution, back to the early 1990s. That is a
far better match to the theory than anything the FSI panel offered, and it
costs nothing. This probe checks whether that is actually true rather than
assuming it.

WHAT THIS PROBE DOES NOT DO. It does not estimate anything. It reports
coverage, field availability, and event frequency, so the go/no-go
decision rests on measured facts.

Run:  python3 bank_level_feasibility.py
Needs network access the analysis sandbox does not have.
"""

import json
import sys

import requests

FDIC = "https://banks.data.fdic.gov/api"

# FDIC-computed quarterly financial fields. Names are taken from the FDIC
# financial-data dictionary and are VERIFIED BY THIS PROBE rather than
# trusted: any field that comes back absent is reported as absent instead
# of silently becoming a missing column later.
WANTED = {
    "CERT": "institution identifier",
    "REPDTE": "report date",
    "ASSET": "total assets",
    "EQ": "total equity capital",
    "RBCT1J": "tier 1 (leverage) capital",
    "RBCRWAJ": "total risk-based capital ratio",
    "RWAJT": "risk-weighted assets",
    "EQCDIV": "cash dividends declared (year to date)",
    "EQCSTOCK": "sale/issuance of common stock (year to date)",
}


def probe(params, label):
    try:
        r = requests.get(f"{FDIC}/financials", params=params, timeout=90)
    except Exception as exc:
        print(f"  {label}: request failed -- {exc}")
        return None
    if r.status_code != 200:
        print(f"  {label}: HTTP {r.status_code}")
        return None
    try:
        return r.json()
    except Exception:
        print(f"  {label}: response was not JSON (first 200 chars)")
        print("   ", r.text[:200].replace("\n", " "))
        return None


print("=" * 72)
print("PART 1 -- is the endpoint reachable, and which fields exist?")
print("=" * 72)

d = probe({"filters": "REPDTE:20241231",
           "fields": ",".join(WANTED),
           "limit": 5, "format": "json"}, "field probe")
if d is None:
    print("\nCannot proceed. If this is a network restriction rather than an")
    print("outage, run the probe somewhere with outbound access.")
    sys.exit(1)

rows = d.get("data", [])
total = d.get("meta", {}).get("total")
print(f"  institutions reporting 2024-Q4: {total}")

if rows:
    present = set()
    for row in rows:
        present |= set(row.get("data", row).keys())
    print("\n  field availability:")
    for f, desc in WANTED.items():
        mark = "yes" if f in present else "ABSENT"
        print(f"    {f:<10} {mark:<7} {desc}")
    missing = [f for f in WANTED if f not in present]
    if missing:
        print(f"\n  *** {len(missing)} wanted field(s) absent: {missing}")
        print("  *** Check the FDIC financial data dictionary for the current")
        print("  *** names before concluding the data is unavailable -- field")
        print("  *** names change and an absent name is not an absent series.")

print()
print("=" * 72)
print("PART 2 -- how far back does coverage run?")
print("=" * 72)
print("  A long window matters: the theory's thresholds are revealed by")
print("  repeated payout and issuance decisions, so a bank contributes")
print("  information in proportion to how many such decisions are observed.")
print()
for year in (1995, 2000, 2005, 2010, 2015, 2020, 2024):
    dd = probe({"filters": f"REPDTE:{year}1231",
                "fields": "CERT,REPDTE,ASSET,EQ",
                "limit": 1, "format": "json"}, f"{year}")
    if dd is not None:
        n = dd.get("meta", {}).get("total")
        print(f"  {year}-Q4: {n} institutions")

print()
print("=" * 72)
print("PART 3 -- are the EVENTS frequent enough to identify thresholds?")
print("=" * 72)
print("  This is the question that decides the companion paper. The panel")
print("  needs banks that actually cross their thresholds: paying dividends")
print("  (upper barrier) and raising capital (lower trigger). If issuance")
print("  events are vanishingly rare outside crises, the trigger is not")
print("  identified and the companion needs rethinking -- exactly the way")
print("  the deficit regime turned out to be unobservable in the FSI panel.")
print()

sample = probe({"filters": "REPDTE:20241231",
                "fields": "CERT,REPDTE,ASSET,EQ,EQCDIV,EQCSTOCK",
                "limit": 1000, "format": "json"}, "event sample")
if sample and sample.get("data"):
    recs = [r.get("data", r) for r in sample["data"]]
    n = len(recs)

    def frac(field):
        vals = [r.get(field) for r in recs]
        nonzero = [v for v in vals if isinstance(v, (int, float)) and v != 0]
        return len(nonzero), n

    for field, what in (("EQCDIV", "declared a dividend"),
                        ("EQCSTOCK", "issued common stock")):
        k, n = frac(field)
        if n:
            print(f"  {what:<24} {k:>5} of {n} sampled banks "
                  f"({100.0 * k / n:.1f}%)")
    print()
    print("  Read: a high dividend fraction and a low-but-nonzero issuance")
    print("  fraction is the expected and workable pattern -- issuance is")
    print("  supposed to be rare, since it is an impulse control. Issuance")
    print("  at or near zero across a full quarter would be the warning")
    print("  sign, and would mean the trigger must be identified from a")
    print("  crisis window or from a different population.")

print()
print("=" * 72)
print("VERDICT TEMPLATE")
print("=" * 72)
print("  GO if: the endpoint responds, the capital-ratio and both event")
print("  fields exist, coverage reaches back at least to the early 2000s,")
print("  and issuance events are rare but present.")
print()
print("  RETHINK if: event fields are absent or always zero (the revealed-")
print("  threshold strategy has no observable events), or coverage is too")
print("  short to see a bank cross its thresholds more than once or twice.")
print()
print("  Either way this is measured, not assumed -- which is the whole")
print("  point of running it before committing to the paper.")
