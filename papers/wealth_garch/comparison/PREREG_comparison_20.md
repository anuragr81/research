# Pre-registration: the testing thread's comparison, twentieth file

Written and committed 25 October 2026, before any quantity below is
computed. It freezes the enhanced model taken to the 2025 hold-out, with
the floor set by the sources read (OR-Q1 to Q6, OR-Q13, LM-Q5, LM-Q7)
rather than chosen, and fixes the hold-out test. The author agreed (25
October 2026). Opening the hold-out, that is fetching any 2025 data, waits
for the author's separate agreement.

## What was seen before this file

Everything listed in PREREG_comparison_19.md, results_do_nothing.md,
NOVELTY_20261025.md and the literature records it cites. In particular,
for measure (D): with the floor at 10% throughout, training F2 54.0%
(sample B) and test-window misses of 15.2 (26 banks) and 7.8 (61 banks);
with the floor at R = 10.5% throughout, training F2 71.2% for (N) and,
for (D), test-window F1 1.0% and F2 70.7% on the 61 banks (miss 12.9).
So the frozen model's test-window result is, in effect, already seen; its
training-window result with the floor switching in 2010 is not. No data
from 2025 or later has been fetched or seen.

## The frozen enhanced model (P)

- The non-issuer barrier model of results_book_vol.md: risk sigma_D, the
  standard deviation of yearly changes in the total capital ratio over
  the ten years before the window (PREREG_comparison_17.md's rule,
  unchanged); dividends at u0(mu_i, sigma_D,i, rho = 4%) above the floor;
  failure at the floor; mu_i and sigma_i over the same ten years as in
  sample.py; facts in units of sigma_i over R.
- The floor, from the sources: a total capital ratio of 10% for quarters
  up to 2009Q4 (OR-Q1, OR-Q4) and 10.5% from 2010Q1 (OR-Q5, OR-Q6,
  OR-Q13; LM-Q5). When the floor rises by 0.5 points in 2010Q1, every
  live path's distance to the floor falls by 0.005 at that step, and the
  barrier stays u0 above the new floor.
- The baseline is Peura and Keppo as in results_pk.md, with its own
  minimum of 8% and its parameters over the same ten years.

## Reported now (decides nothing)

The frozen model on the training window (2005-2014, from the 2004Q4
buffer, floor switching in 2010) and the test window (2015-2024, from the
2014Q4 buffer, floor 10.5%), F1 and F2 against observed and against the
baseline, samples B and A, in results_frozen.md.

## The 2025 hold-out (run only when the author agrees)

- Data: the FDIC quarterly Call Report file for 2025, fetched under its
  own dated note naming source and sha256, by the fetch already used for
  common_data; nothing else from 2025 or later.
- Banks: those of sample B (PREREG_comparison_2.md) with a 2024Q4 buffer;
  sample A reported.
- Parameters from 2015-2024 only: mu_i, sigma_i and sigma_D,i by the same
  code with the ten-year window moved to 2015-2024, so that 2014 is the
  first lag year; the baseline's barriers likewise.
- Window: 2025Q1 to 2025Q4 from the 2024Q4 buffer, 4 quarters, 200 paths,
  floor 10.5%; facts F1 and F2 as facts.py, units of sigma_i (2015-2024)
  over R.
- Rule: (P) beats the baseline if its F1 + F2 miss in sample B is
  smaller. Each is also judged by the 10-point rule on F1 and on F2. The
  standard errors of the observed shares (binomial, at the observed value)
  are reported with them.
- The result is reported whatever it is; if (P) does not beat the
  baseline, the enhancement thread's claim rests on the earlier unseen
  banks only, and the paper says so.
