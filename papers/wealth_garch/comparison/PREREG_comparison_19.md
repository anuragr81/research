# Pre-registration: the testing thread's comparison, nineteenth file

Written and committed 25 October 2026, before any quantity below is
computed. results_book_vol.md matched the level of buffers with measure
(D), the yearly volatility of book capital, and the well-capitalised 10%
floor; but (D) contains the banks' own dividend and issue decisions, which
the model is meant to explain. This file replaces it with the volatility
of the do-nothing ratio of Berger et al., which excludes them, and adds a
bank-by-bank test of the level. The author agreed (25 October 2026).

## What was seen before this file

Everything listed in PREREG_comparison_18.md, results_book_vol.md and
results_access.md. In particular every test-window quantity of sample B
and of sample A's other banks has now been seen at the pooled level, so
no bank in 2015-2024 is unseen. The test window below therefore confirms
or not; it no longer decides alone. The decision on the final model is
left to the 2025 hold-out, under its own file, opened only with the
author's agreement. No quantity of measure (N) has been computed, and no
bank-by-bank predicted level has been set against an observed one.

## Measure (N): the volatility of capital before capital actions

- Do-nothing ratio, after Berger et al. (BDF-Q30, BDF-Q5): for each year t
  of 2005-2014, DNK_t = (K_(t-1) + NI_t - DIV_(t-1)) / RWA_t, with K the
  total capital (RBCT1J + RBCT2) at Q4, NI the year's net income (NETINC
  at Q4), DIV the year's dividends declared (EQCDIV at Q4), RWA the
  risk-weighted assets at Q4.
- Change before capital actions: DNK_t less the total capital ratio at
  Q4 of t-1. It counts if all inputs exist and total assets at Q4 of t
  are between half and twice those at Q4 of t-1.
- sigma_N,i: the standard deviation of the counted changes, 2005-2014; a
  bank with fewer than 8 is not covered and is counted. It moves with
  earnings and with risk-weighted assets, holds dividends at last year's
  level, and excludes issues, repurchases and dividend changes.

## Model

As results_book_vol.md part 1, with sigma_N,i in place of sigma_D,i: the
non-issuer barrier model, sigma_N,i in the dynamics and in u0(mu_i,
sigma_N,i, rho = 4%), minimum at 10% (central), 8% and R reported; seeds
as judged_risk.py's paths; facts in units of sigma_i over R.

## Tests

1. Training gate (sample B, 2005-2014, from the 2004Q4 buffer): (N)
   passes if its F2, minimum 10%, is within 10 points of the observed
   48.0%. F1 by the 10-point rule, reported. If (N) passes, it replaces
   (D) as the enhanced model taken to the 2025 hold-out; if not, (D)
   stays, with the circularity stated.
2. Test window, reported (sample B, 2015-2024, from the 2014Q4 buffer):
   (N) against the baseline and against (D), F1 and F2, minimum 10%; the
   26 banks of PREREG_comparison_17.md and all 61 shown apart.
3. Bank by bank (sample B and sample A, each window): the model's median
   buffer over R per bank (over surviving bank-quarters of its paths)
   against the bank's observed median buffer over R. Reported: Spearman's
   rank correlation with p, and the ordinary least squares slope and
   intercept of observed on predicted with HC1 standard errors, for (N),
   (D) and the baseline. A model places buffers in a sample if its
   Spearman correlation is positive with p below 0.05 in both windows.
   The pooled facts do not show this; it is new.
- Nothing from 2025 or later enters.
