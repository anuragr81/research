# Pre-registration: the testing thread's comparison, eighteenth file

Written and committed 24 October 2026, before any quantity below is
computed. results_book_vol.md found that buffers fall with bank size and
that neither model reproduces this. The sources read (BDF-Q24 to Q29,
HP-Q14 to Q17) offer access to capital as one explanation without a cost
figure. This file tests, on the observed buffers alone, whether access to
capital accounts for the size gradient. No model is scored. The author
agreed (24 October 2026).

## What was seen before this file

Everything listed in PREREG_comparison_17.md, results_book_vol.md, and
the quotations above. In sample A the Spearman correlation of the median
buffer over R with log size is -0.341 (training) and -0.387 (test). The
listed-parent match of results_match.md covers sample B only; no bank of
sample A outside sample B has been classified by holding company or
listing, and no buffer has been set against either.

## Banks and windows

Sample A, each window separately: training (2005-2014; banks with a
2004Q4 buffer; classification at 2004Q4) and test (2015-2024; banks with
a 2014Q4 buffer; classification at 2014Q4).

## Classifications, at the window's start quarter

- Stand-alone, after HP-Q11: the bank has no holding company (RSSDHCR
  empty or 0), or its holding company is the top holder of no other bank
  in the FDIC file of that quarter (all banks of the file, not only the
  sample). Otherwise affiliated (a multibank holding company).
- Parent: the holding company, or the bank itself if it has none. Its
  country and names from the NIC attributes files, its listing by the
  rules of PREREG_comparison_12.md and _13.md (match_tickers.py's
  functions, unchanged). Three groups: listed (a US parent with a unique
  match), unlisted (a US parent with no match, or two or more), foreign
  (a parent outside the United States). A parent with no NIC record is
  left out of the listing test and counted.
- Caveat fixed now: the listing directory is of October 2026, so a parent
  listed in 2004 or 2014 and since absorbed or failed counts as unlisted.
  This misclassifies more at 2004Q4 than at 2014Q4, so the listing test
  decides on the test window, and the training window is reported.

## Outcome and covariates

- Level: the bank's median buffer over R across the window's quarters, in
  points, winsorised at the 1st and 99th percentiles across the window's
  banks (as results_book_vol.md shows, a few banks have medians of tens or
  hundreds of points).
- Size: log of mean total assets over 2005-2014 (sample.py). Risk:
  sigma_i.

## Tests

1. Regression across banks, ordinary least squares with heteroskedasticity-
   robust (HC1) standard errors:
   level = a + b1 log size + b2 sigma_i + b3 standalone + b4 unlisted
   + b5 foreign + error, listed affiliated banks the base.
   Predictions (HP-Q14, HP-Q15, HP-Q17, BDF-Q28): b3 > 0 and b4 > 0.
   - Stand-alone holds if b3 > 0 with two-sided p below 0.05 in both
     windows; it is mixed if in one only.
   - Unlisted holds if b4 > 0 with p below 0.05 in the test window; the
     training window is reported.
2. Share of the size gradient carried by access: b1 from the same
   regression without b3, b4 and b5, against b1 with them; reported as
   1 - b1(with) / b1(without), with both b1 and their standard errors.
   Nothing is decided by it.
3. Reported: the same regression on ranks (every variable replaced by its
   rank; the dummies as they are); the Spearman correlation of level with
   log size within the stand-alone banks and within the affiliated banks;
   the median level by size tercile within each group; the counts in each
   group.
- No model is involved; the result is a description of the data with
  directions fixed in advance. Nothing from 2025 or later enters.
