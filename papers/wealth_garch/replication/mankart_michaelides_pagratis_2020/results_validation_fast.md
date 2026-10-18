# Mankart, Michaelides and Pagratis stage 3: tests of the faster solver

Run 18 October 2026 (about 6.4 hours) as the systemd unit
mmp-validate-fast, with the code of commit 93f3c820,
run under PREREG_stage3_1.md.

## Findings

- F1 passes: with light sweeps and 50 evaluation steps the cake-eating
  policy is within 0.363% (large) and 0.349% (small) of kappa e at 1,281
  points.
- F2 fails: large banks at 41 points, solved cold, differ from the stage 2
  solution by up to 0.560 of the value range. The changed solver's value
  is higher in 79.2% of cells and lower in 2.1%; a converged value
  function is the value of its policy, so in most cells the changed
  solver found a better policy than the stage 2 solver did.
- F3 passes: small banks at 41 points, solved cold, give the 11 moments of
  results_stage2.md to five decimal places in every moment, with no
  non-finite state.
- F4 fails on values and agrees on moments: at the perturbed parameters
  the warm and cold solves differ by up to 0.094 of the value range, and
  every moment agrees within 1% (failure rate 0.16982 against 0.16974).
- Under the rule of PREREG_stage3_1.md the changed solver is not adopted,
  since F2 and F4 fail.
- Times (from the log, not in the output below): F2 80 sweeps (11 full)
  in 3,743 s against 46 sweeps in 12,724 s for the stage 2 solver; F3 50
  sweeps (8 full) in 2,321 s against 37 sweeps in 9,591 s; F4 warm 190
  sweeps (22 full) in 7,543 s and cold 194 sweeps (23 full) in 7,898 s.
  The warm start saved 4 sweeps.

## Limits

- Two converged solutions that differ this much mean that the fixed point
  the search reaches depends on its path: the solver's optimisation error
  is not small in some cells, for either solver. Where those cells lie
  (states the simulated banks visit, or not) was not examined.
- F2's reference is a large-bank solution, which PREREG_stage2_7.md left
  out of the reported results because of the unbounded equity region.

Pinned: mmp_model.py sha256 aa51c30d5bc933ff2491cfec74db19884a5c60a312c6d66fc91611d049112752
Pinned: mmp_config.py sha256 06cc133c320e1bb8c5156be77776c6e320ee960372e7ead0f1160a4a2af60c29
Pinned: mmp_a1.py sha256 4a6f85c1696af0a712920b6a31a5bc91221a73546e168f1cfc00ad675a6e2bf6
Pinned: mmp_fast.py sha256 26d33708a8b36cb98d50ecc939ec7239d22d16b0c72b421304cb2ac33c607a16
Pinned: mmp_sim.py sha256 ffd95ca41801e7024a6ff06fc262e105889c156e97fd50d576123a8713eb910e
Pinned: stage2.py sha256 b6be175625a9ce996d838975d872a1f4d01840725f6fa8c02028e299fab4aac6
Pinned: validate_fast.py sha256 04b2d1a89a04759fecc950e95ec00e0ada5bfebec1b736d2e79a76c41b75ded4
Pinned: solved_large_41.npz sha256 803df60b6655e4a1bd3749e28000077e14c531a59fa7b49e4fa510e35df7d9fe
Pinned: results_stage2.md sha256 5aa158e5445480408061d7df0217d05f834f7e356690751c62f64d8982654e3b

Reproduce: python3 validate_fast.py

```
F1 large cake, 1281 points: converged after 15 sweeps (5 full), last change 7.2e-04, largest fall 7.1e-01
F1 large: policy error 3.63e-03 -> passes
F1 small cake, 1281 points: converged after 21 sweeps (5 full), last change 1.4e+01, largest fall 1.2e+00
F1 small: policy error 3.49e-03 -> passes
F2 large 41 cold: converged after 80 sweeps (11 full), last change 1.6e-03, largest fall 0.0e+00
F2: largest value difference 5.60e-01 of the value range -> fails; cells where the changed solver is higher 0.792, lower 0.021
F3 small 41 cold: converged after 50 sweeps (8 full), last change 7.1e-02, largest fall 0.0e+00
  F3 failure rate         0.019540 against 0.019540 -> agrees
  F3 loans/assets         0.613830 against 0.613830 -> agrees
  F3 deposits/assets      0.865423 against 0.865423 -> agrees
  F3 equity/assets        0.092603 against 0.092603 -> agrees
  F3 profit/equity        0.021263 against 0.021263 -> agrees
  F3 dividends/equity     0.009163 against 0.009163 -> agrees
  F3 sd loans/assets      0.055596 against 0.055597 -> agrees
  F3 sd deposits/assets   0.025396 against 0.025395 -> agrees
  F3 sd equity/assets     0.022995 against 0.022995 -> agrees
  F3 sd profit/equity     0.017823 against 0.017823 -> agrees
  F3 sd dividends/equity  0.001581 against 0.001581 -> agrees
F3: 0 non-finite bank-quarters -> passes
F4 perturbed, warm: converged after 190 sweeps (22 full), last change 3.6e-02, largest fall 0.0e+00
F4 perturbed, cold: converged after 194 sweeps (23 full), last change 0.0e+00, largest fall 0.0e+00
  F4 failure rate         0.169820 against 0.169740 -> agrees
  F4 loans/assets         0.561877 against 0.561868 -> agrees
  F4 deposits/assets      0.881381 against 0.881382 -> agrees
  F4 equity/assets        0.078127 against 0.078126 -> agrees
  F4 profit/equity        0.017071 against 0.017072 -> agrees
  F4 dividends/equity     0.006315 against 0.006315 -> agrees
  F4 sd loans/assets      0.071388 against 0.071396 -> agrees
  F4 sd deposits/assets   0.025376 against 0.025377 -> agrees
  F4 sd equity/assets     0.022697 against 0.022698 -> agrees
  F4 sd profit/equity     0.021816 against 0.021816 -> agrees
  F4 sd dividends/equity  0.001000 against 0.001000 -> agrees
F4: largest value difference 9.43e-02 of the value range -> fails
F1 passes; F2 fails; F3 passes; F4 fails
```
