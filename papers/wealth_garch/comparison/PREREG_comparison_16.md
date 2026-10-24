# Pre-registration: the testing thread's comparison, sixteenth file

Written and committed 24 October 2026, before any quantity below is
computed. results_diag_f2.md found that measure (B), equity volatility,
has the right sign and about five to seven times the size of the risk
that banks' buffers imply, and results_scale.md found that the barrier
model matches the training-window F2 at about three times the measured
sigma_i. The author chose (24 October 2026) to look first for a measure
that lands near that scale without being fitted, and, failing that, to
scale the market measure by how much book capital moves with market
equity. This file fixes measure (C), the bank's own worst crisis loss.

## What was seen before this file

Everything listed in PREREG_comparison_15.md and results_diag_f2.md:
per-bank training- and test-window buffers for the 35 banks covered by
(B); the pooled facts and baseline predictions for all 61 banks of sample
B in both windows (results_facts.md, results_pk.md, results_scale.md).
For the 26 banks of sample B outside those 35, no per-bank or
26-bank quantity has been computed on the test window. No quantity of
measure (C) has been computed for any bank. Blends of measures (A) and
(B) are excluded: both have been seen, so any blend would be chosen to
land on the target.

## Measure (C)

- Flows: for each quarter of 2005Q1-2014Q4, net income less common and
  preferred dividends declared, NETINC minus EQCDIV, as quarterly flows
  (the year-to-date fields differenced within each year, as qflow does).
- Windows: every run of 9 consecutive quarters inside 2005Q1-2014Q4 (32
  windows). A window counts if all 9 flows are present, risk-weighted
  assets (RWAJT) are reported for the quarter before it, and total assets
  at its last quarter are between half and twice those of the quarter
  before it (the steady screen of Peura and Keppo, applied to the window).
- The decline over a window is minus the sum of its 9 flows over RWAJT of
  the quarter before it. D_i is the largest decline over the bank's
  counted windows, or zero if every one is a gain. A bank with no counted
  window is not covered and is counted.
- Judged volatility, by the conversion of (A) in PREREG_comparison_7.md:
  sigma_C,i = D_i / (z sqrt(2.25)), z in {1.645, 2.326, 3.090}, central
  z = 2.326. Floor: if sigma_C,i is below sigma_i, sigma_i is used (the
  bank is never judged safer than its own earnings), and the number of
  banks at the floor is reported.

## Stage 1: training window (decides whether (C) goes forward)

- Sample B (61 banks), 2005-2014, from each bank's 2004Q4 buffer; the
  non-issuer barrier model of results_scale.md with sigma_C,i in the
  dynamics and in the barrier u0(mu_i, sigma_C,i, rho = 4%); 40 quarters,
  200 paths, Euler steps of 1/360 year, seeds as in judged_risk.py's paths
  (20261024, certificate number, minimum, judged volatility); facts in
  units of sigma_i over R = 10.5%; minimum 8% central, R reported. F1 and
  F2 scored (PREREG_comparison_11.md).
- (C) goes forward if, at the central z with the minimum at 8%, its
  predicted F2 is within 10 points of the observed training F2 (48.0%).
  F1 is judged by the 10-point rule and reported.
- Reported: the median and quartiles of sigma_C,i over sigma_i, set
  against the scale 3 of results_scale.md; the other z values; sample A.
- If (C) does not go forward, stage 2 is not run and the fallback (the
  market measure scaled by book capital's response to market equity) is
  fixed in its own file.

## Stage 2: test window (only if stage 1 passes)

- Sample B, 2015-2024, from each bank's 2014Q4 buffer, (C) at the central
  z against the baseline of results_pk.md, minimum 8%, as in
  PREREG_comparison_10.md; sigma_C,i from 2005-2014 data only.
- Decision on the 26 banks of sample B not scored in
  results_test_judged.md or results_extend.md: (C) beats the baseline if
  its F1 + F2 miss is smaller. Each model is also judged by the 10-point
  rule on F1 and F2.
- Reported, deciding nothing: all 61 banks; the 35 already scored, with
  (C) set beside (B); the minimum at R; sample A.
- Nothing from 2025 or later enters.
