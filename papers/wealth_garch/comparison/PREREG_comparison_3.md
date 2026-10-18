# Pre-registration: the testing thread's comparison, third file

Written and committed 18 October 2026, before any computation below. Two
parts: how Mankart, Michaelides and Pagratis (MMP) are scored (agreed with
the author today), and a check of whether the shortfall-penalty
enhancement can close the baseline's gap, run on the training window
only.

## What was seen before this file

results_facts.md and results_pk.md: on 2015-2024 the baseline agrees on
none of the four facts (sample B: F1 90.0% against 4.8%, F2 0.0% against
61.6%, F3 70.8% against 15.2%, F4 81.9% against 30.4%). No fact has been
measured on 2005-2014, and no MMP fact has been computed.

## Part 1: MMP

- Size class (MMP's own, PREREG_stage1.md of that replication): a
  bank-quarter is small if its ASSET is below the 95th percentile of ASSET
  over the commercial banks (BKCLASS N, NM or SM) in the common data that
  quarter, as in MMP stage 1. MMP's small-bank model
  is scored against the small bank-quarters of sample A; sample B (the 61
  largest banks) is not scoreable for MMP, since its large-bank version is
  left out (PREREG_stage2_7.md of that replication). Observed facts for
  the small class are computed as in results_facts.md, restricted to
  small bank-quarters (a dividend quarter or episode is counted when its
  quarter q is small).
- Solution: replication/mankart_michaelides_pagratis_2020/
  solved_fast_small_41.npz, the small-bank solution at the published
  parameters by the faster solver, whose moments equal results_stage2.md's
  to five decimal places (results_validation_fast.md, F3). Pinned by
  sha256 in the results file.
- Simulation: stage2.simulate (10,000 banks, 2,000 quarters, seed
  20261014), last 500 quarters. From each live bank-quarter: assets =
  1/(deposits/assets), loans, equity after dividends eq, liquid assets
  s = assets - loans; risk-weighted assets RWA = loans + 0.2 s (the
  model's weights); buffer b = eq / RWA - 0.105; profit pi.
- sigma of a simulated bank: annual ROA = the sum over four consecutive
  quarters of pi / RWA, over the bank's complete years in the 500
  quarters (live in all four quarters); sigma is their standard deviation
  (banks with fewer than 8 complete years are dropped from the facts and
  counted). No risk-free return is subtracted: the model's rates are fixed
  at their means.
- F1 over quarters with a dividend (x > 0) and b observed in the quarter
  before; F2 over live bank-quarters; F3 and F4: the model has no equity
  issue, so it predicts no episodes, a fail where 20 or more episodes are
  observed in the small class (PREREG_comparison.md).

## Part 2: can a shortfall penalty close the gap? (training window)

- Window: facts measured on 2005Q1-2014Q4, buffers from the common data
  2004Q4-2014Q4, with the rules of results_facts.md (R = 10.5%, sigma_i
  and samples as PREREG_comparison_2.md). Banks of A and B without a
  buffer at 2004Q4 are left out of this part and counted. Nothing from
  2015-2024 enters this part.
- Models, both simulated from each bank's 2004Q4 buffer over 40 quarters
  exactly as predict_pk.py, with mu_i, sigma_i, Delta, K, rho unchanged:
  (a) the baseline, minimum 8%; (b) the proxy, the baseline with its
  minimum moved to R = 10.5%: the state is the buffer over 10.5%, and
  reaching it ends the path, as failure does in the baseline. Banks
  starting at or below the proxy's minimum start failed and are counted.
- Reading of (b): it is a bank that treats falling below R as failing,
  the strongest form of the shortfall penalty that leaves the rest of the
  model unchanged. It is a proxy, not a proven limit of the penalized
  model.
- Rule: if, in sample B, the proxy's F2 is more than 10 points below the
  observed training-window F2, the penalty enhancement alone is not
  expected to close F2, and this is reported with the next enhancement
  pass to be considered; otherwise the enhancement is built as planned.
  All four facts are reported for both models and both samples, and the
  other three facts are read in the same way but decide nothing.
