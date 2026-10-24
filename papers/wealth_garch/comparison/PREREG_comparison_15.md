# Pre-registration: the testing thread's comparison, fifteenth file

Written and committed 24 October 2026, after results_test_judged.md and
results_extend.md were seen, before the computations below. This is a
diagnostic of why measure (B) overshoots F2. It is run on data already
scored, so it describes and decides nothing; no result below is a score
of any model, and no model is chosen from it. The author asked for the
diagnosis (24 October 2026).

## What was seen before this file

Everything listed in PREREG_comparison_14.md, results_extend.md and the
output of test_extend.py, including the barrier u0 over sigma_i of the 18
new banks (4.7 to 98.3, median 46.7).

## Banks

The 35 covered banks of sample B (results_test_judged.md and
results_extend.md), with sigma_B,i as there.

## Computations (diag_f2.py)

1. Per bank: sigma_i and sigma_B,i in points of risk-weighted assets;
   u0(mu_i, sigma_B,i, rho) in points; the observed buffer (ratio minus R)
   over 2015-2024: its median, its standard deviation of year-on-year
   (Q4 to Q4) changes, and the bank's own F2 share; the model's buffer
   under (B), minimum 8%, with the same seeds as test_extend.py: its
   median over surviving bank-quarters and the bank's F2 share; and the
   same observed quantities over 2005-2014.
2. Banks grouped by terciles of sigma_i: observed and model F2 in each,
   and each tercile's contribution to the overall gap in points.
3. Over banks: the median of u0 over the observed median buffer (both
   windows); the median of the observed yearly standard deviation of the
   buffer over sigma_i and over sigma_B,i.
4. Per bank, the scale k_i at which u0(mu_i, k_i sigma_i, rho) equals the
   observed median buffer of 2005-2014 (bisection on k in [0.1, 50]), and
   the distribution of k_i; the same with sigma_B,i replaced, giving the
   factor by which sigma_B,i overstates the risk the bank's buffer
   implies. These are descriptions of the training window, not estimates
   for use.

## Rule

None. Anything suggested by this diagnostic is a new mechanism, to be
fixed in its own pre-registration before any scoring, with the test-window
exposure of these 35 banks stated there.
