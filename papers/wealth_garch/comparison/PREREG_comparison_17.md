# Pre-registration: the testing thread's comparison, seventeenth file

Written and committed 24 October 2026, before any quantity below is
computed. Two parts, agreed with the author on 24 October 2026: (1) the
fallback for the level of buffers, measure (D), with the well-capitalised
floor as the model's minimum; (2) a test of the two directions of
Berger et al. (BDF-Q21), that buffers rise with risk and fall with bank
size, for the data and for each model.

## What was seen before this file

Everything listed in PREREG_comparison_16.md, and results_own_loss.md.
In particular:

- results_scale.md: with the minimum at R the non-issuer barrier model's
  training F2 in sample B is 30.3% at a risk scale of 1.5 and 73.3% at 2;
  with the minimum at 8%, 5.8% and 16.8%.
- results_diag_f2.md: for the 35 banks covered by (B), the standard
  deviation of Q4-to-Q4 changes of the buffer over 2005-2014 has median
  1.07 points, 1.81 times sigma_i.
- results_own_loss.md: the median 2005-2014 buffer over R, median over
  banks, is 2.68 points in sample B and 4.56 points in sample A.

So the choice of the well-capitalised floor as central below is made after
seeing that a minimum near R and a risk scale near 1.8 together land near
the training F2. The floor rests on OCC-Q3 (well capitalized needs a total
risk-based ratio of 10.0 percent or greater, the line at which the
supervisor's restrictions begin, ledger action A9), not on F2; but the
choice is not blind, and the test-window decision is therefore taken only
on banks whose test-window facts have not been computed. For the 26 banks
of sample B outside the 35 of results_test_judged.md and
results_extend.md no per-bank or 26-bank test-window quantity has been
computed. No quantity of measure (D) has been computed beyond the 35-bank
median above. No per-bank buffer median has been set against bank size.

## Part 1: measure (D) and the well-capitalised floor

- sigma_D,i: the standard deviation of the changes in the bank's total
  capital ratio ((RBCT1J + RBCT2) / RWAJT) from each Q4 to the next, Q4
  2004 to Q4 2014 (up to 10 changes). A change counts if both Q4 ratios
  exist and total assets at the later Q4 are between half and twice those
  at the earlier. A bank with fewer than 8 counted changes is not covered
  and is counted. sigma_D,i is the observed yearly volatility of book
  capital, including the bank's own dividends and issues; nothing in it
  refers to F1 or F2.
- Model: the non-issuer barrier model of results_scale.md with sigma_D,i
  in the dynamics and in the barrier u0(mu_i, sigma_D,i, rho = 4%), and
  the minimum at 10% (central), with 8% and R = 10.5% reported. Paths,
  steps and seeds as in judged_risk.py's paths (20261024, certificate
  number, minimum, judged volatility); facts in units of sigma_i over R.
  A bank starting at or below the minimum fails at once and is counted.
- Stage 1, training window (sample B, 2005-2014, from the 2004Q4
  buffer): (D) goes forward if its predicted F2, minimum 10%, is within 10
  points of the observed 48.0%. F1 is judged by the 10-point rule and
  reported; the median of sigma_D,i over sigma_i is reported.
- Stage 2, test window (only if stage 1 passes; sample B, 2015-2024, from
  the 2014Q4 buffer, sigma_D,i from 2004-2014): decision on the 26
  unscored banks; (D) with the minimum at 10% beats the baseline (Peura
  and Keppo as in results_pk.md, minimum 8%, its own specification) if its
  F1 + F2 miss is smaller. Reported: all 61, the 35, the other minima,
  sample A.
- If stage 1 fails, stage 2 is not run.

## Part 2: the directions of BDF-Q21

- Sample A (all banks of PREREG_comparison_2.md with the inputs; it spans
  the size range that the 61 largest of sample B do not), and sample B
  reported. Each window separately: training (2005-2014, banks with a
  2004Q4 buffer) and test (2015-2024, banks with a 2014Q4 buffer).
- Per bank, the observed level: the median over the window's quarters of
  the buffer (total capital ratio less R), in points. Risk: sigma_i. Size:
  the log of the bank's mean total assets over 2005-2014 (as sample.py).
- Observed directions: Spearman's rank correlation across banks of the
  level with size (BDF-Q21 predicts negative) and with risk (predicts
  positive), with its two-sided p-value. A direction holds in the data if
  the correlation has the predicted sign with p below 0.05. Also reported:
  the median level by tercile of size and of risk.
- Model directions: per bank, the median buffer over R over surviving
  bank-quarters of the simulated paths, for (i) the baseline, Peura and
  Keppo with issuance, minimum 8%, as in predict_pk.py (the copy used here
  must reproduce predict_pk.py's sample B test-window F1 and F2 exactly,
  or the run stops), and (ii) (D) with the minimum at 10%, whether or not
  it passes stage 1. Each model's Spearman correlations with size and risk
  as above. A model reproduces a direction if its correlation has the
  same sign as the data's and p below 0.05. Reported for each model and
  window: the two correlations, against the data's.
- The directions decide nothing about the level and the level decides
  nothing about the directions; they are reported side by side.
- No split by geography, politics or macroeconomic conditions is made:
  no source read predicts one, and such splits wait for a source that
  does.
- Nothing from 2025 or later enters.
