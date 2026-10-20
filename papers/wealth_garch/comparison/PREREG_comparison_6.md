# Pre-registration: the testing thread's comparison, sixth file

Written and committed 20 October 2026, before any computation below. Every
mechanism of results_mechanisms.md leaves the dividend barrier within one
or two points of the model's minimum, while real buffers are far larger.
Milne and Whalley (lit/milne_whalley_2001, MW-D1) show that issuance costs
and random audits cannot raise the buffer above the barrier u0 of the
non-issuer model, and that a lower cost of equity raises it (MW-D2); Berger
et al. find that targets rise with risk (BDF-Q21). This check turns the
gap into two numbers: how much larger than measured earnings risk, and how
much lower than 4% the cost of equity, would have to be for the
non-issuer model to produce the observed share of buffers far above R.

## What was seen before this file

Everything listed in PREREG_comparison_5.md, results_mechanisms.md, and the
Milne and Whalley and Berger et al. quotations above.

## The check (scale_check.py)

- Banks, mu_i, sigma_i, R = 10.5%, facts and rules as results_reward.md;
  training window 2005-2014; no issuance; dividends at the barrier u0
  (closed form); failure at the minimum; simulation as results_reward.md.
  The facts are always measured in units of the bank's measured sigma_i.
- Risk scale: the buffer follows dx = mu_i dt + k sigma_i dW, with the
  barrier u0(mu_i, k sigma_i, rho), rho = 4%, for k in {1, 1.5, 2, 3, 4,
  6}.
- Cost of equity: rho in {4%, 2%, 1%, 0.5%} with k = 1.
- Both with the model's minimum at 8% and at R.

## Rule

- Reported for each: the smallest grid value at which the predicted F2 in
  sample B is within 10 points of the observed 48.0%, if any, with F1 at
  that value. These are the sizes the evidence must then support. Nothing
  is chosen for the enhanced model by this check; any value used later is
  justified from sources, not from this fit.
- Sample A is reported and decides nothing.
