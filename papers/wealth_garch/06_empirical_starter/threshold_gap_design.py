"""
threshold_gap_design.py

DESIGN SKELETON for testing the KSW prediction (see EMPIRICAL_TEST_STARTER.md
sec. 1 and 5). This script computes NOTHING resembling a final result. It:

  1. defines event detection and gap computation as pure, testable functions;
  2. is validated here, in __main__, against SYNTHETIC data with a known,
     injected gap-vs-cost relationship -- before it is ever pointed at real
     Call Report data;
  3. leaves the fixed-cost proxy choice and the "materially positive" event
     thresholds as explicit, named parameters at the top of the file, to be
     set via pre-registration (Sec. 5 of the starter pack) BEFORE running on
     real data -- not tuned afterwards to produce a cleaner result.

Real Call Report data must come from bank_level_feasibility.py's probe
results first. This script does not fetch data itself.

CRITICAL -- YEAR-TO-DATE FIELDS. Call Report Schedule RI-A reports EQCDIV
(dividends declared) and EQCSTKRX (sale of common stock) as YEAR-TO-DATE
cumulative figures, NOT as quarterly flows. A Q3 value is the sum of Q1,
Q2 and Q3; a Q4 value is the whole calendar year. Feeding raw YTD values
to the event detectors below would (a) count one annual event as up to
four quarterly events, and (b) put the event in the wrong quarter. Every
raw record must pass through decumulate_ytd() FIRST. The feasibility
probe's headline event rates (89.8% dividends, 6.6% issuance at 2024-Q4)
are ANNUAL rates read off a single Q4 snapshot, not quarterly rates --
recompute them on the decumulated series before using them for anything.

DO NOT treat a clean result from the synthetic self-test as evidence for
the model. It only shows the code correctly recovers a relationship that
was deliberately put into fake data.
"""

import math
import random
from dataclasses import dataclass
from typing import Optional


# ---------------------------------------------------------------------------
# Pre-registration: set BEFORE looking at real data. See starter pack Sec. 5.
# ---------------------------------------------------------------------------

# "Materially positive" thresholds for event detection, as a fraction of the
# institution's assets in the preceding quarter. Placeholder values -- must
# be fixed via the pre-registration document before any real run, not chosen
# to make the real-data result look cleaner.
ISSUANCE_EVENT_MIN_FRAC_OF_ASSETS = 0.005   # 0.5% of assets, PLACEHOLDER
DIVIDEND_EVENT_MIN_FRAC_OF_ASSETS = 0.0005  # PLACEHOLDER

# Candidate fixed-cost proxies (Sec. 5, item 4). Exactly which one(s) to use
# is a pre-registration decision, not a data-driven one.
PROXY_CHOICES = ("assets_percentile", "charter_type_public", "stress_period")


def quarter_index(period: str) -> int:
    """'2019Q3' -> 3. Raises on anything that is not a Qn period label."""
    if len(period) != 6 or period[4] != "Q" or not period[5].isdigit():
        raise ValueError(f"unparseable period label: {period!r}")
    q = int(period[5])
    if not 1 <= q <= 4:
        raise ValueError(f"quarter out of range in {period!r}")
    return q


def year_of(period: str) -> int:
    return int(period[:4])


def decumulate_ytd(records: list["QuarterRecord"]) -> list["QuarterRecord"]:
    """Convert YTD cumulative RI-A fields into true quarterly flows.

    Input: records for ONE institution, any number of years, each carrying
    RAW year-to-date values in stock_issuance / dividends_declared.
    Output: the same records with those two fields replaced by quarterly
    flows. Q1 flow is the Q1 YTD value itself; Qn flow is Qn YTD minus
    Q(n-1) YTD within the same calendar year.

    A quarter whose immediate predecessor within the same year is MISSING
    cannot be differenced -- its flow is genuinely unknown, not zero. Such
    records are returned with the affected field set to None, which makes
    is_recapitalisation_event() raise rather than silently score them as
    non-events. Do not "fix" that by defaulting them to zero: a bank with
    a gap in its filing history is a coverage gap, and coverage gaps that
    quietly become non-events bias the event rate downward.
    """
    if len({r.institution_id for r in records}) > 1:
        raise ValueError("decumulate_ytd expects one institution at a time")

    by_key = {(year_of(r.period), quarter_index(r.period)): r for r in records}
    out = []
    for r in sorted(records, key=lambda r: (year_of(r.period), quarter_index(r.period))):
        y, q = year_of(r.period), quarter_index(r.period)
        new = QuarterRecord(**vars(r))
        for field in ("stock_issuance", "dividends_declared"):
            ytd = getattr(r, field)
            if ytd is None:
                setattr(new, field, None)
                continue
            if q == 1:
                setattr(new, field, ytd)
                continue
            prev = by_key.get((y, q - 1))
            prev_ytd = getattr(prev, field) if prev is not None else None
            if prev_ytd is None:
                setattr(new, field, None)   # undifferenceable: unknown, not zero
            else:
                setattr(new, field, ytd - prev_ytd)
        out.append(new)
    return out


@dataclass
class QuarterRecord:
    """One institution-quarter. Field names are placeholders pending the
    feasibility probe's confirmation of actual FDIC BankFind field names --
    see starter pack Sec. 4 and 6, step 1. Do not assume these are correct."""
    institution_id: str
    period: str                    # e.g. '2019Q3'
    capital_ratio: float           # RC-R derived
    assets: float
    stock_issuance: Optional[float]   # RI-A, None if field absent that period
    dividends_declared: Optional[float]  # RI-A


def is_recapitalisation_event(rec: QuarterRecord) -> bool:
    """Expects a DECUMULATED record (see decumulate_ytd). Passing a raw
    year-to-date record here will overcount and mis-date events.

    True if this quarter's stock issuance clears the pre-registered
    materiality threshold. Returns False (not an error) if the field is
    genuinely absent for this period -- absence must be tracked separately
    by the caller as a coverage gap, never silently coded as 'no event'."""
    if rec.stock_issuance is None:
        raise ValueError(
            f"stock_issuance missing for {rec.institution_id} {rec.period}: "
            "caller must handle coverage gaps explicitly, not call this "
            "function on incomplete records."
        )
    if rec.assets <= 0:
        raise ValueError(f"non-positive assets for {rec.institution_id} {rec.period}")
    return (rec.stock_issuance / rec.assets) >= ISSUANCE_EVENT_MIN_FRAC_OF_ASSETS


def observed_gap(trigger_quarter: QuarterRecord, post_quarter: QuarterRecord) -> float:
    """The empirical analogue of the model's trigger-to-target distance:
    capital ratio the quarter after a recapitalisation event, minus the
    capital ratio in the triggering quarter. Caller is responsible for
    verifying post_quarter immediately follows trigger_quarter for the same
    institution -- this function does not check period adjacency."""
    if trigger_quarter.institution_id != post_quarter.institution_id:
        raise ValueError("gap computed across two different institutions")
    return post_quarter.capital_ratio - trigger_quarter.capital_ratio


def loglog_slope(costs: list[float], gaps: list[float]) -> float:
    """OLS slope of log(gap) on log(cost). This is the number to compare
    against 1/3 (KSW's prediction) and against 1 (the null of a linear,
    non-impulse-control cost response). Requires strictly positive gaps and
    costs -- a gap of zero or below at K>0 is a genuine falsification signal
    for prediction 1 (Sec. 1) and must be reported, not filtered out to make
    this function run."""
    if len(costs) != len(gaps) or len(costs) < 3:
        raise ValueError("need matched, non-trivial cost/gap series")
    if any(c <= 0 for c in costs) or any(g <= 0 for g in gaps):
        raise ValueError(
            "non-positive cost or gap present -- this is itself a result "
            "(possible falsification of prediction 1), do not silently drop"
        )
    xs = [math.log(c) for c in costs]
    ys = [math.log(g) for g in gaps]
    n = len(xs)
    xbar = sum(xs) / n
    ybar = sum(ys) / n
    num = sum((x - xbar) * (y - ybar) for x, y in zip(xs, ys))
    den = sum((x - xbar) ** 2 for x in xs)
    if den == 0:
        raise ValueError("no variation in cost proxy -- cannot estimate a slope")
    return num / den


# ---------------------------------------------------------------------------
# Self-test on synthetic data with a KNOWN, INJECTED relationship.
# This validates the code, not the model. Mirrors the mutation-testing /
# dry-run discipline used elsewhere in this project
# (panel_currency_union_check.py, the M-operator verifier).
# ---------------------------------------------------------------------------

def _synthetic_self_test():
    random.seed(0)
    true_slope = 1.0 / 3.0
    true_intercept = 1.3  # matches the model's own K=0.01..0.02 ratio band

    costs = [0.001, 0.005, 0.01, 0.02, 0.05]
    gaps = []
    for k in costs:
        noise = random.uniform(-0.02, 0.02)
        gap = true_intercept * (k ** true_slope) + noise
        gaps.append(max(gap, 1e-6))  # keep strictly positive for the self-test

    est_slope = loglog_slope(costs, gaps)
    print(f"[self-test] injected slope: {true_slope:.4f}")
    print(f"[self-test] recovered slope: {est_slope:.4f}")
    assert abs(est_slope - true_slope) < 0.05, (
        "loglog_slope failed to recover an injected 1/3 relationship -- "
        "do not trust this function on real data until this passes"
    )
    print("[self-test] PASS: code recovers a known injected relationship.")
    print("[self-test] This proves nothing about real banks. Real data next.")

    # --- decumulation self-test -------------------------------------
    # One bank, two calendar years of YTD values, with 2020Q3 missing.
    # True quarterly issuance flows injected: 2019 = 0,0,5,0 ; 2020 = 3,0,?,4
    raw = [
        QuarterRecord("A", "2019Q1", 0.10, 1000, 0.0,  0.0),
        QuarterRecord("A", "2019Q2", 0.10, 1000, 0.0,  1.0),   # YTD
        QuarterRecord("A", "2019Q3", 0.10, 1000, 5.0,  2.0),
        QuarterRecord("A", "2019Q4", 0.10, 1000, 5.0,  3.0),
        QuarterRecord("A", "2020Q1", 0.10, 1000, 3.0,  0.5),   # YTD resets
        QuarterRecord("A", "2020Q2", 0.10, 1000, 3.0,  1.0),
        # 2020Q3 absent entirely
        QuarterRecord("A", "2020Q4", 0.10, 1000, 7.0,  2.0),
    ]
    dec = {r.period: r for r in decumulate_ytd(raw)}
    assert dec["2019Q1"].stock_issuance == 0.0
    assert dec["2019Q2"].stock_issuance == 0.0
    assert dec["2019Q3"].stock_issuance == 5.0, "Q3 flow should be 5-0"
    assert dec["2019Q4"].stock_issuance == 0.0, "Q4 flow should be 5-5"
    assert dec["2020Q1"].stock_issuance == 3.0, "YTD must reset at the year boundary"
    assert dec["2020Q2"].stock_issuance == 0.0
    assert dec["2020Q4"].stock_issuance is None, (
        "2020Q4 follows a MISSING 2020Q3 -- its flow is unknown, not zero")
    print("[self-test] PASS: decumulation handles year reset and missing quarter.")
    print("[self-test]   raw YTD issuance 2019: 0,0,5,5 -> flows 0,0,5,0")
    print("[self-test]   2020Q4 after missing Q3 -> None (unknown), not 0.")

    # A raw-YTD record must not be silently usable as an event record.
    try:
        is_recapitalisation_event(dec["2020Q4"])
        raise AssertionError("expected ValueError on undifferenceable quarter")
    except ValueError:
        print("[self-test] PASS: undifferenceable quarter raises, not scored 0.")

    # Event-detection self-test with a deliberately missing field, to check
    # the function fails loudly rather than silently treating it as no-event.
    bad_rec = QuarterRecord("X", "2020Q1", capital_ratio=0.10, assets=1000,
                             stock_issuance=None, dividends_declared=0.0)
    try:
        is_recapitalisation_event(bad_rec)
        raise AssertionError("expected ValueError on missing stock_issuance field")
    except ValueError:
        print("[self-test] PASS: missing-field case raises rather than silently passes.")


if __name__ == "__main__":
    print(__doc__)
    print("=" * 70)
    _synthetic_self_test()
    print("=" * 70)
    print("STATUS: design-stage only. Real Call Report data required before")
    print("this script's functions produce anything that counts as a result.")
    print()
    print("Feasibility probe: RUN, verdict GO (12 Aug 2026). All 9 fields")
    print("present; coverage 1995-2024; 2024 annual rates 89.8% dividends,")
    print("6.6% issuance. Those are ANNUAL rates off a Q4 YTD snapshot --")
    print("recompute quarterly rates on the decumulated series.")
    print()
    print("Next step: pull ALL FOUR quarters per institution-year, run them")
    print("through decumulate_ytd(), recompute the quarterly event rate,")
    print("THEN pre-register the placeholder constants at the top of this file.")
