# Pre-registration: Mankart, Michaelides and Pagratis, stage 2, second file

Written and committed 14 October 2026, after the first validation run and
before any run of the full model. It amends PREREG_stage2.md (commit
3e8a7a24) in two items; everything else stands.

## What was seen before this file

The first run of validate_stage2.py, and a comparison of write-off
discretisations against the stated process (both reported in the results
file of stage 2):
- V1 failed (largest relative error 0.40 for the large-bank parameters,
  0.09 for the small). The test was wrong: x = kappa (e + 1) assumes the
  bank can borrow against the future income from deposits, but the model
  requires post-dividend equity e - x > 0, which binds in the future when
  kappa > r.
- V2 failed for write-offs (worst relative error 0.26 for small banks,
  0.11 for large): the floor at zero (our R4) truncates a normal AR(1)
  whose mean is close to zero, and 9 nodes on one grid spanning both
  states are coarse. With no floor, 15 nodes give worst errors of 0.085
  (large) and 0.035 (small); more nodes do not lower the large-bank error,
  which comes from the mixing of the two states' processes.

## Amendments

1. V1 becomes pure cake-eating: the deposit rate equals the liquid-asset
   return r = 0.01, so profit is r (e - x) and e' = (1 + r)(e - x), with
   the exact policy x = kappa e, kappa = 1 - beta^(1/gamma)
   (1 + r)^((1 - gamma)/gamma), and e - x > 0 never binding. Passes if
   the solved x is within 1% of kappa e at every grid point with e in
   [0.05, 0.30], for both size classes' beta and gamma.
2. Write-offs (R2, R4): 15 Tauchen nodes on one grid spanning both states'
   means plus and minus three standard deviations, with no floor at zero,
   as equation (1) states a normal AR(1) and the paper states no bound.
   Negative nodes are recoveries in excess of charge-offs.
