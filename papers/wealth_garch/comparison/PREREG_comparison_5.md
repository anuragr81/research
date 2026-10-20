# Pre-registration: the testing thread's comparison, fifth file

Written and committed 20 October 2026, before any computation below. Four
candidate mechanisms for large voluntary buffers, each given the same
training-window bound check as the shortfall penalty (results_bound.md)
and the reward for capital (results_reward.md), all fixed here before any
is run. The author asked that all four be tested before any evidence for
them is sought (20 October 2026).

## What was seen before this file

Everything listed in PREREG_comparison_4.md, and results_reward.md: on
2005-2014, sample B, observed F2 48.0%; with no issuance, the dividend
barrier at u0 and failure at zero, F2 is 0.5% with the model's minimum at
8% and 5.2% with it at R; with a reward for capital at up to three times
the insurer's channel, at most 17.4%.

## Common to all four

- Training window 2005-2014, banks, sigma_i, mu_i, R = 10.5%, facts and
  their rules as in results_reward.md; simulation as there (Euler steps of
  1/360 year, 200 paths per bank, 40 quarters from the 2004Q4 buffer, seed
  20261020), no issuance, dividends at the barrier, failure at the
  minimum. F2 decides; F1 is reported.
- Barrier: the first point where the solution f of the bank's value
  equation, started at f(0) = 0, f'(0) = 1, turns from concave to convex
  (smooth fit), searched up to a buffer of 2.0; banks with none are
  counted and left out, with the effect bounded as in results_reward.md.
- Solver validation: with no jumps and no growth the numerical barrier
  equals the closed form u0 to 1e-6 for every bank of sample A, or the run
  stops there.
- Each mechanism is run with the model's minimum at 8% and at R (with the
  penalty proxy) unless stated.
- Rule for each mechanism: it can close F2 if, at its central point and
  in sample B, the predicted F2 is within 10 points of the observed
  training-window F2 for either minimum. Sample A is reported and decides
  nothing.

## The four mechanisms

1. Rare large losses. Earnings lose J of risk-weighted assets at the
   events of a Poisson process of rate lambda, with the drift raised by
   lambda J so that mean earnings stay at mu_i; sigma_i unchanged. Value
   equation (1/2) sigma^2 f'' + (mu_i + lambda J) f' - (rho + lambda) f +
   lambda f(x - J) = 0, with f(x - J) = 0 below zero. Grid: J in {0.02,
   0.04, 0.06}, lambda in {1/25, 1/10} per year; central point J = 0.04,
   lambda = 1/25.
2. Raising capital impossible in a crisis. The strongest form within this
   family is a bank that can never issue, which is the c = 0 rows of
   results_reward.md (F2 0.5% and 5.2% in sample B). No new run; the
   mechanism is judged by the rule on those rows.
3. Capital needed to grow. Risk-weighted assets grow at the bank's mean
   yearly asset growth g_i over 2005-2014 (December reports, years with
   the asset-growth screen of stage 3). The buffer's drift falls by
   g_i (x + m), m the minimum, and value scales with the balance sheet, so
   the equation is (1/2) sigma^2 f'' + (mu_i - g_i (x + m)) f' -
   (rho - g_i) f = 0. Grid: g in {0.5, 1, 1.5} times g_i; central g_i.
   Banks with g >= rho have no finite value and no barrier; they are
   counted and bounded as above.
4. Supervisors expecting more than the minimums. The model's minimum is
   raised to R + d, d in {0.01, 0.02, 0.03}, with the barrier u0 above it.
   No source read gives d, so there is no central point: the check reports
   the smallest d at which F2 in sample B comes within 10 points, if any,
   and the evidence for such a d is sought afterwards.

rho = 4% throughout, as Peura and Keppo stage 3.
