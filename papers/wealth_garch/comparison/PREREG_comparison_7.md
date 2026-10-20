# Pre-registration: the testing thread's comparison, seventh file

Written and committed 20 October 2026, before any data below is fetched.
results_scale.md found that the non-issuer barrier model matches the share
of buffers far above R only if the risk the bank guards against is two to
three times its measured earnings volatility; Berger et al. find targets
rise with risk (BDF-Q21) and reflect counterparties' sensitivity to default
risk (BDF-Q22), and Milne and Whalley tie the buffer to a costly regulatory
intervention or market response (MW-Q2). The author asked (20 October
2026) that two measures of the risk a bank is judged on be tried and
compared: the regulator's and the market's.

## What was seen before this file

Everything listed in PREREG_comparison_6.md and results_scale.md. No
stress-test result and no share price has been read or fetched.

## Banks

Sample B of PREREG_comparison_2.md. A bank enters the comparison if its
top holding company (RSSDHCR at 2014Q4) is covered by both measures below;
the comparison runs on those banks only, so that the two measures are
scored on the same banks. The number covered by each and by both is
reported.

## (A) The regulator's measure: stress-test declines

- Source, to be fetched after this commit: the Federal Reserve's published
  results of the Dodd-Frank Act stress tests of March 2013 and March 2014
  (federalreserve.gov), read for each firm's starting and projected minimum
  Tier 1 common (or common equity tier 1) ratio under the severely adverse
  scenario, and recorded in lit/ with page-checked quotations.
- Firm to bank: the firm's RSSD ID from the NIC attributes files already
  pinned in replication/gordon_hirtle_2026, matched to RSSDHCR. Each match
  is listed in the results.
- Decline D_i: the starting ratio minus the projected minimum, in
  percentage points, averaged over the two years when a firm is in both.
- Judged volatility: sigma_A,i = D_i / (z sqrt(2.25)), the volatility under
  which a 9-quarter (2.25-year) decline of D_i is the z-quantile of a
  normal loss. Grid z in {1.645, 2.326, 3.090} (95%, 99%, 99.9%); central
  z = 2.326. No source read gives the scenario's probability, so the grid
  is reported whole and the central point decides.

## (B) The market's measure: equity volatility

- Source, to be fetched after this commit: daily closing prices of the
  holding companies' common shares, 2005-2014, from a free public source
  (Stooq, stooq.com), pinned by sha256. Tickers: matched by name between
  the NIC legal name and the SEC's company ticker list (sec.gov), each
  match listed and checked by hand in the results; a firm with no
  unambiguous match, or no price history covering at least 2010-2014, is
  not covered.
- sigma_E,i: the annualised standard deviation of daily log returns,
  2005-2014 (sqrt(252) times the daily standard deviation).
- Judged volatility: sigma_B,i = sigma_E,i X_i, with X_i the bank's mean
  total capital ratio over the December reports 2005-2014: the volatility
  of the capital ratio if book capital moved in proportion to the market
  value of equity.

## The check (judged_risk.py)

- For each measure, the non-issuer barrier model of results_scale.md with
  sigma_i replaced by the judged volatility in the dynamics and the barrier
  u0; mu_i, rho = 4% and the rest unchanged; the facts still measured in
  units of the measured sigma_i; minimum at 8% and at R; training window
  2005-2014. F3 and F4 are not scored (no issuance).
- Rule: the better measure is the one whose predicted F2 and F1, at its
  central point, with the minimum at 8%, are closer to the observed values
  on the common banks, by the sum of the absolute differences in points;
  each is also judged by the 10-point rule. The minimum at R and sample A
  (where covered) are reported and decide nothing.
- Nothing from 2015-2024 enters. The test window is scored later, under its
  own file, with the measure (or both) fixed by this one.
