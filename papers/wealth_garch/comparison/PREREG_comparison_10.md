# Pre-registration: the testing thread's comparison, tenth file

Written and committed 24 October 2026, before any computation below. The
test window (2015-2024) is scored with measure (B), equity volatility, as
results_judged.md fixed under PREREG_comparison_7.md. The author confirmed
(24 October 2026) that (B) goes to the test window as fixed.

## What was seen before this file

Everything listed in PREREG_comparison_9.md, and results_judged.md. On the
test window: the observed facts and the baseline's predictions for the
whole of samples A and B (results_facts.md, results_pk.md: sample B
observed F1 4.8%, F2 61.6%; baseline F1 90.0%, F2 0.0%). Nothing has been
computed on the test window for the 17 banks below, observed or
predicted.

## Banks

The 17 banks of sample B whose top holding company (RSSDHCR at 2014Q4) is
one of the 11 firms of PREREG_comparison_9.md, as listed in
results_judged.md. Sample A's 19 such banks are reported and decide
nothing.

## Parameters

All from 2004-2014 data, as in results_judged.md, with nothing from
2015-2024: mu_i and sigma_i as under PREREG_comparison_2.md;
sigma_B,i = sigma_E,i X_i, with sigma_E,i from the pinned 2005-2014 prices
and X_i the mean total capital ratio over the December reports 2005-2014;
rho = 4%.

## The two models

- Enhanced, (B): the non-issuer barrier model of results_judged.md, with
  the buffer's volatility sigma_B,i in the dynamics and in the barrier
  u0(mu_i, sigma_B,i, rho); no issuance; failure at the minimum.
- Baseline: Peura and Keppo as in results_pk.md (predict_pk.py's solve and
  simulate, unchanged), with issuance where the bank has an issuance
  barrier.
- Both start from the bank's 2014Q4 buffer, run 40 quarters with Euler
  steps of 1/360 year, 200 paths per bank; the facts are measured in units
  of sigma_i over R = 10.5%, exactly as in results_pk.md. The minimum is
  8% (central); (B) is also run with the minimum at R, reported, deciding
  nothing. Seeds: the baseline keeps predict_pk.py's; (B) uses 20261024
  with the bank's certificate number.

## Observed

The four facts on the 17 banks, 2015-2024, computed by facts.py's
observed function, unchanged.

## Rule

- (B) beats the baseline if, on the 17 banks with the minimum at 8%, its
  miss |predicted F1 - observed F1| + |predicted F2 - observed F2|, in
  points, is smaller than the baseline's. A tie is not a win.
- Each model is also judged by the 10-point rule on F1 and F2. F3 and F4
  are reported for the baseline and are not scored, since (B) has no
  issuance.
- Whatever the outcome, it is reported; the result is a comparison on 17
  of the 61 banks of sample B, and says so.
- Nothing from 2025 or later enters.
