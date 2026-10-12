# Pre-registration: ownership split, second classification

Written and committed 12 October 2026, after run 1
(results_ownership_split.md, commit acc47087) and before any computation
under the rule below. Nothing in this file may change after the first run.

## Why a second file

Under PREREG_ownership_split.md, group S was empty: `HCTMULT` is 1 or
empty in the fetched data, never 0, and constant within every bank, while
`RSSDHCR` changes for 854 of 6,581 banks. This file replaces only the
classification. Run 1 stays reported.

What was seen before this file was written: run 1's output in full, which
gives E3 and E2 for the banks with `HCTMULT` = 1 and for the rest pooled.
The results for the groups defined below were not computed.

## Data

Unchanged: the pinned panel `fdic_distress_panel_2015_2024.csv.gz` (sha256
`3cb78c0a...`) and the pinned ownership file
`fdic_ownership_2015_2024.csv.gz` (sha256
`2862f658594b19484163ab6db5d7630c4cb1913ede21bdb85e5e904a7081abea`). No new
data are fetched.

## Groups

For each quarter, count the institutions in the ownership file with each
non-empty `RSSDHCR` other than 0. Then, per bank-quarter:

- M, multibank subsidiary: `RSSDHCR` is set and two or more institutions
  share it in that quarter.
- S, stand-alone: `RSSDHCR` is empty or 0 (no holding company), or exactly
  one institution has it in that quarter (a one-bank holding company), the
  split of Holod and Peek (HP-Q11).
- Unclassified: no ownership row for the bank-quarter.

The count covers every FDIC-insured institution in the file, of any size,
so a holder of a bank and a savings institution counts as multibank.

## Tests, verdicts and readings

As in PREREG_ownership_split.md, unchanged: E3 holds for a group if at most
5% of its bank-quarters below $y^*$ pay a dividend and fails otherwise; E2
uses P1 and P2 with the enough-data and verdict rules of
`distress_targeting.py`; the readings fixed in advance apply. Also
reported, not used: the S0 sub-group (no holding company), the number of
banks whose group changes, the recapitalisation composition, and the share
of `HCTMULT` = 1 bank-quarters that this rule classifies M.
