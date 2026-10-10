"""
issuance_diagnostic.py  --  resolve the quarterly-vs-annual anomaly.

THE PROBLEM THIS ADDRESSES. quarterly_event_rate.py reported an 11.09%
quarterly issuance rate (2015-2024) against the feasibility probe's 6.6%
ANNUAL rate. A quarterly rate cannot exceed the annual rate for the same
population: a bank that issues in some quarter has issued that year. So
one of the two numbers is measuring something other than what it claims.

Dividends do NOT show this problem (52.29% quarterly vs 89.8% annual is
the expected ordering), which localises the issue to EQCSTKRX or to how
its differenced flows are being scored as events.

THREE CANDIDATE CAUSES, all checked here:

  (a) NEGATIVE FLOWS scored as events. quarterly_event_rate.py counts any
      nonzero differenced value. YTD figures get restated; a downward
      revision produces a negative flow, which is not an issuance.

  (b) ROUTINE ISSUANCE scored as recapitalisation. EQCSTKRX is all sale of
      common stock -- employee stock plans, ESOPs, dividend reinvestment
      programmes. These are small, regular, and unrelated to hitting a
      capital trigger. A bank issuing in 8+ of 40 quarters is showing a
      routine trickle, not a rare impulse control. The pre-registered
      materiality threshold exists to exclude these, and was NOT applied
      in the rate above.

  (c) SAMPLE MISMATCH. The 6.6% came from a 1000-bank slice in whatever
      order the API returns, not a random sample of the population. This
      script cannot test (c) -- it is noted so it is not forgotten.

WHAT THIS REPORTS. The sign breakdown, the size distribution of positive
flows relative to assets, and how the event count and repeat-issuer count
fall as a materiality threshold is raised. That last table is the input to
pre-registering the threshold -- pick it on the shape of the distribution
and on what counts as a real capital raise, NOT by choosing the value that
makes a later regression look best.

Run:   python3 issuance_diagnostic.py            (2015-2024)
       python3 issuance_diagnostic.py 2005 2024
       python3 issuance_diagnostic.py --self-test
"""

import sys
from collections import defaultdict

from threshold_gap_design import QuarterRecord, decumulate_ytd
from quarterly_event_rate import fetch_quarter, to_records, QUARTER_ENDS

# Candidate materiality thresholds, as a fraction of the bank's assets.
GRID = [0.0, 1e-6, 1e-5, 5e-5, 1e-4, 5e-4, 1e-3, 2.5e-3, 5e-3, 1e-2, 2e-2]


def analyse(records):
    by_bank = defaultdict(list)
    for r in records:
        by_bank[r.institution_id].append(r)

    flows = []
    for recs in by_bank.values():
        flows.extend(decumulate_ytd(recs))

    usable = [f for f in flows if f.stock_issuance is not None and f.assets > 0]
    n = len(usable)

    neg = [f for f in usable if f.stock_issuance < 0]
    zero = [f for f in usable if f.stock_issuance == 0]
    pos = [f for f in usable if f.stock_issuance > 0]

    print()
    print("=" * 72)
    print("SIGN BREAKDOWN OF DIFFERENCED ISSUANCE FLOWS")
    print("=" * 72)
    print(f"  usable bank-quarters:  {n}")
    print(f"    negative:            {len(neg):>8}  ({100.0*len(neg)/n:.2f}%)")
    print(f"    exactly zero:        {len(zero):>8}  ({100.0*len(zero)/n:.2f}%)")
    print(f"    positive:            {len(pos):>8}  ({100.0*len(pos)/n:.2f}%)")
    print()
    print("  Negative flows are NOT issuances -- they are YTD restatements or")
    print("  netting. Any of them counted in the 11.09% figure was an error.")
    print("  The positive share above is the corrected upper bound, before")
    print("  any materiality screen.")

    if not pos:
        print("\n  No positive flows. Nothing further to report.")
        return

    ratios = sorted(f.stock_issuance / f.assets for f in pos)

    def pct(p):
        i = min(int(p / 100.0 * len(ratios)), len(ratios) - 1)
        return ratios[i]

    print()
    print("=" * 72)
    print("SIZE OF POSITIVE ISSUANCES, AS A FRACTION OF ASSETS")
    print("=" * 72)
    for p in (1, 5, 10, 25, 50, 75, 90, 95, 99):
        print(f"    {p:>3}th percentile:   {pct(p):.8f}")
    print()
    print("  A capital raise that matters for the recapitalisation trigger")
    print("  should be a visible fraction of the balance sheet. If the bulk")
    print("  of this distribution sits at 1e-5 or below, most 'events' are")
    print("  employee-plan and DRIP issuance, and cause (b) is confirmed.")

    print()
    print("=" * 72)
    print("EVENT COUNT AND REPEAT ISSUERS vs MATERIALITY THRESHOLD")
    print("=" * 72)
    print(f"  {'threshold':>12} {'events':>9} {'rate':>8} {'banks>=1':>9} {'banks>=2':>9}")
    for thr in GRID:
        ev = [f for f in pos if (f.stock_issuance / f.assets) >= thr]
        per_bank = defaultdict(int)
        for f in ev:
            per_bank[f.institution_id] += 1
        ge2 = sum(1 for c in per_bank.values() if c >= 2)
        print(f"  {thr:>12.6f} {len(ev):>9} {100.0*len(ev)/n:>7.2f}% "
              f"{len(per_bank):>9} {ge2:>9}")
    print()
    print("  Read down the 'rate' column until it falls below the 6.6%")
    print("  annual figure -- that is where the arithmetic impossibility")
    print("  disappears and the events plausibly are capital raises.")
    print()
    print("  Choose the pre-registered threshold from THIS table plus a")
    print("  judgement about what constitutes a real capital raise. Do not")
    print("  revisit the choice after seeing a regression result.")


def self_test():
    """Known answer: 1 negative, 1 zero, 2 positive, one of which is tiny."""
    raw = [
        QuarterRecord("A", "2019Q1", 0.1, 1000.0, 0.0, 0.0),
        QuarterRecord("A", "2019Q2", 0.1, 1000.0, 50.0, 0.0),   # +50 material
        QuarterRecord("A", "2019Q3", 0.1, 1000.0, 49.0, 0.0),   # -1 restatement
        QuarterRecord("A", "2019Q4", 0.1, 1000.0, 49.001, 0.0),  # +0.001 trivial
    ]
    flows = decumulate_ytd(raw)
    vals = [f.stock_issuance for f in flows]
    assert vals[0] == 0.0
    assert vals[1] == 50.0
    assert abs(vals[2] - (-1.0)) < 1e-9, vals[2]
    assert abs(vals[3] - 0.001) < 1e-9, vals[3]
    pos = [v for v in vals if v > 0]
    neg = [v for v in vals if v < 0]
    assert len(pos) == 2 and len(neg) == 1
    # only ONE clears a 1% materiality screen
    material = [v for v in pos if v / 1000.0 >= 0.01]
    assert len(material) == 1, material
    print("[self-test] PASS: +50 kept, -1 excluded as negative,")
    print("[self-test]       +0.001 excluded by a 1% materiality screen.")
    print("[self-test] Counting any nonzero flow would have scored all three.")


def main():
    if "--self-test" in sys.argv:
        self_test()
        return
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    start = int(args[0]) if args else 2015
    end = int(args[1]) if len(args) >= 2 else 2024

    records = []
    for year in range(start, end + 1):
        for mmdd in QUARTER_ENDS:
            try:
                rows = fetch_quarter(year, mmdd)
            except Exception as exc:
                print(f"  {year}{mmdd}: FAILED -- {exc}")
                continue
            recs, _ = to_records(rows)
            records.extend(recs)
            print(f"  {year}{mmdd}: {len(recs)} rows")
    if not records:
        print("No data retrieved.")
        sys.exit(1)
    analyse(records)


if __name__ == "__main__":
    main()
