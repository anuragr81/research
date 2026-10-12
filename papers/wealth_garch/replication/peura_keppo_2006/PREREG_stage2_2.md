# Pre-registration: Peura and Keppo replication, stage 2, second run

Written and committed 12 October 2026, after run 1 (results_stage2.md,
commit cc4d2045) and before any computation under the rule below. Nothing
in this file may change after the first run.

## Why a second file

Run 1 followed PREREG_stage2.md, which had no merger rule. National bank
charters that absorb a much larger bank keep their charter, so a year's net
income is divided by the previous year's risk-weighted assets of a bank a
hundred or a thousand times smaller (CERT 5134, 22488, 3832 and others):
μ up to 292% and σ up to 1,168% of risk-weighted assets. Peura and Keppo's
Compustat holding companies aggregate across mergers and have no such
breaks.

What was seen before this file was written: run 1's output in full,
including its verdicts on F1 to F7, and the yearly series of the three
banks named above.

## The one change

A year's ROA enters only if the bank's total assets at the end of that
year are between one half and twice its total assets a year earlier:
0.5 ≤ ASSET_t / ASSET_{t-1} ≤ 2. The bound is fixed at a doubling or a
halving and is not varied. It plays the role of the growth screen used for
mergers by Baron (2020, p. 15 n. 11, 10% a quarter) at an annual horizon
wide enough to leave organic growth alone.

Everything else is PREREG_stage2.md unchanged: the data and their pins, the
period, the national-bank filter, at least 15 annual ROA values (counted
after the screen), the risk-weighted assets and capital ratio rules, the
buffer, the provision groups, μ > 0, samples A and B (B ranked by mean
total assets as before), the barriers, findings F1 to F7 and their rule.

## Reported, not used for a verdict

- The number of ROA observations removed by the screen and the number of
  banks that fall below 15 ROA values because of it.
- Spearman rank correlations beside the Pearson ones for F1 and F3.
