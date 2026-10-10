"""
quarterly_event_rate.py  --  STEP 1 of the KSW empirical check.

WHY THIS EXISTS. bank_level_feasibility.py returned GO, but it sampled a
single Q4 snapshot of EQCDIV / EQCSTKRX, which are Schedule RI-A
YEAR-TO-DATE fields. Its headline numbers (89.8% dividends, 6.6% stock
issuance) are therefore ANNUAL rates: they say "this bank issued stock at
some point during 2024", not "this bank issued stock in Q4".

The number that decides whether the recapitalisation trigger is
identifiable is the QUARTERLY rate, computed on decumulated flows. It will
be lower than the annual rate. This script computes it, plus the figure
that actually matters for KSW: how many banks are observed issuing MORE
THAN ONCE, since a bank seen issuing only once yields a single gap and no
within-bank variation in cost conditions.

WHAT IT DOES NOT DO. It estimates nothing and tests no prediction. It
reports rates and counts, so the go/no-go on step 2 rests on measured
facts rather than on the annual figure standing in for a quarterly one.

Run:   python3 quarterly_event_rate.py
       python3 quarterly_event_rate.py 2015 2024     (custom year range)
       python3 quarterly_event_rate.py --self-test   (no network needed)

Needs: requests. Needs network access to banks.data.fdic.gov.
Uses decumulate_ytd() from threshold_gap_design.py -- one implementation,
so this script and the event detector can never disagree about what a
quarterly flow is. Keep both files in the same directory.
"""

import sys
from collections import defaultdict

from threshold_gap_design import QuarterRecord, decumulate_ytd

FDIC = "https://banks.data.fdic.gov/api"
QUARTER_ENDS = ("0331", "0630", "0930", "1231")
FIELDS = "CERT,REPDTE,ASSET,EQ,EQCDIV,EQCSTKRX"
PAGE = 10000  # FDIC API maximum


def repdte_to_period(repdte) -> str:
    """20240630 -> '2024Q2'."""
    s = str(repdte)
    year, mmdd = s[:4], s[4:8]
    q = {"0331": 1, "0630": 2, "0930": 3, "1231": 4}.get(mmdd)
    if q is None:
        raise ValueError(f"not a quarter-end report date: {repdte}")
    return f"{year}Q{q}"


def fetch_quarter(year: int, mmdd: str):
    """All institutions for one quarter, paging until exhausted."""
    import requests

    out, offset = [], 0
    while True:
        r = requests.get(
            f"{FDIC}/financials",
            params={"filters": f"REPDTE:{year}{mmdd}", "fields": FIELDS,
                    "limit": PAGE, "offset": offset, "format": "json"},
            timeout=120,
        )
        r.raise_for_status()
        batch = r.json().get("data", [])
        if not batch:
            break
        out.extend(row.get("data", row) for row in batch)
        if len(batch) < PAGE:
            break
        offset += PAGE
    return out


def to_records(raw_rows):
    """FDIC rows -> QuarterRecord, dropping rows missing an identifier or
    a usable report date. Rows missing EQCDIV/EQCSTKRX keep None: absent is
    not zero, and decumulate_ytd propagates that distinction."""
    recs, skipped = [], 0
    for row in raw_rows:
        cert, repdte = row.get("CERT"), row.get("REPDTE")
        if cert is None or repdte is None:
            skipped += 1
            continue
        try:
            period = repdte_to_period(repdte)
        except ValueError:
            skipped += 1
            continue
        recs.append(QuarterRecord(
            institution_id=str(cert),
            period=period,
            capital_ratio=(row.get("EQ") or 0.0) / row["ASSET"] if row.get("ASSET") else 0.0,
            assets=row.get("ASSET") or 0.0,
            stock_issuance=row.get("EQCSTKRX"),
            dividends_declared=row.get("EQCDIV"),
        ))
    return recs, skipped


def report(records):
    by_bank = defaultdict(list)
    for r in records:
        by_bank[r.institution_id].append(r)

    flows = []
    for bank_recs in by_bank.values():
        flows.extend(decumulate_ytd(bank_recs))

    n = len(flows)
    undiff_iss = sum(1 for f in flows if f.stock_issuance is None)
    undiff_div = sum(1 for f in flows if f.dividends_declared is None)

    iss = sum(1 for f in flows if f.stock_issuance not in (None,) and f.stock_issuance != 0)
    div = sum(1 for f in flows if f.dividends_declared not in (None,) and f.dividends_declared != 0)

    iss_den = n - undiff_iss
    div_den = n - undiff_div

    print()
    print("=" * 72)
    print("QUARTERLY EVENT RATES (on decumulated flows)")
    print("=" * 72)
    print(f"  banks:                        {len(by_bank)}")
    print(f"  bank-quarters:                {n}")
    print(f"  undifferenceable (issuance):  {undiff_iss}  "
          f"({100.0*undiff_iss/n:.1f}% -- missing predecessor quarter)")
    print(f"  undifferenceable (dividends): {undiff_div}")
    print()
    if div_den:
        print(f"  dividend quarters:  {div:>7} of {div_den} ({100.0*div/div_den:.2f}%)")
    if iss_den:
        print(f"  issuance quarters:  {iss:>7} of {iss_den} ({100.0*iss/iss_den:.2f}%)")
    print()
    print("  Compare the issuance figure against the 6.6% ANNUAL rate from")
    print("  the feasibility probe. A quarterly rate materially below that")
    print("  is expected and fine; the question is whether it is nonzero")
    print("  and whether it yields repeat issuers -- see below.")

    # The figure that matters for KSW: repeat issuers.
    per_bank = defaultdict(int)
    for f in flows:
        if f.stock_issuance not in (None,) and f.stock_issuance != 0:
            per_bank[f.institution_id] += 1

    dist = defaultdict(int)
    for c in per_bank.values():
        dist[c] += 1
    once = dist.get(1, 0)
    repeat = sum(v for k, v in dist.items() if k >= 2)

    print()
    print("=" * 72)
    print("REPEAT ISSUERS (the figure KSW actually needs)")
    print("=" * 72)
    print(f"  banks with >=1 issuance quarter: {len(per_bank)}")
    print(f"    exactly 1:                     {once}")
    print(f"    2 or more:                     {repeat}")
    if per_bank:
        top = sorted(dist.items())[:8]
        print("    distribution (events: banks):  " +
              ", ".join(f"{k}:{v}" for k, v in top))
    print()
    print("  A bank seen issuing ONCE gives one gap observation but no")
    print("  within-bank variation in cost conditions. Cross-sectional")
    print("  tests can use single-event banks; any within-bank version of")
    print("  Prediction 2 needs the '2 or more' group to be substantial.")
    print()
    print("  This script reports counts. Whether they are sufficient is a")
    print("  judgement to make on these numbers, not one encoded here.")


def self_test():
    """No network. Confirms the pipeline turns YTD into quarterly flows and
    counts events correctly on data with a KNOWN answer."""
    raw = []
    # Bank A: issues in 2019Q3 only (YTD 0,0,5,5) -> exactly 1 event
    for period, ytd in [("2019Q1", 0.0), ("2019Q2", 0.0), ("2019Q3", 5.0), ("2019Q4", 5.0)]:
        raw.append(QuarterRecord("A", period, 0.1, 1000.0, ytd, 0.0))
    # Bank B: issues 2019Q2 and 2019Q4 (YTD 0,3,3,9) -> exactly 2 events
    for period, ytd in [("2019Q1", 0.0), ("2019Q2", 3.0), ("2019Q3", 3.0), ("2019Q4", 9.0)]:
        raw.append(QuarterRecord("B", period, 0.1, 1000.0, ytd, 0.0))
    # Bank C: never issues
    for period in ("2019Q1", "2019Q2", "2019Q3", "2019Q4"):
        raw.append(QuarterRecord("C", period, 0.1, 1000.0, 0.0, 0.0))

    by_bank = defaultdict(list)
    for r in raw:
        by_bank[r.institution_id].append(r)
    counts = {}
    for bank, recs in by_bank.items():
        flows = decumulate_ytd(recs)
        counts[bank] = sum(1 for f in flows
                           if f.stock_issuance not in (None,) and f.stock_issuance != 0)

    assert counts == {"A": 1, "B": 2, "C": 0}, counts
    print("[self-test] PASS: YTD 0,0,5,5 -> 1 event; 0,3,3,9 -> 2 events; 0,0,0,0 -> 0.")
    print("[self-test] The pipeline counts quarterly events, not annual ones.")
    print("[self-test] Proves the code is right, nothing about real banks.")


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    if "--self-test" in sys.argv:
        self_test()
        return

    start = int(args[0]) if len(args) >= 1 else 2015
    end = int(args[1]) if len(args) >= 2 else 2024
    print(f"Pulling {start}-{end}, all four quarters per year.")
    print("This is 4 requests per year; each returns every reporting bank.")

    records, skipped_total = [], 0
    for year in range(start, end + 1):
        for mmdd in QUARTER_ENDS:
            try:
                rows = fetch_quarter(year, mmdd)
            except Exception as exc:
                print(f"  {year}{mmdd}: FAILED -- {exc}")
                continue
            recs, skipped = to_records(rows)
            records.extend(recs)
            skipped_total += skipped
            print(f"  {year}{mmdd}: {len(recs)} usable rows"
                  + (f"  ({skipped} skipped)" if skipped else ""))

    if not records:
        print("\nNo data retrieved. Nothing to report.")
        sys.exit(1)
    if skipped_total:
        print(f"\n  total rows skipped (no CERT / bad date): {skipped_total}")
    report(records)


if __name__ == "__main__":
    main()
