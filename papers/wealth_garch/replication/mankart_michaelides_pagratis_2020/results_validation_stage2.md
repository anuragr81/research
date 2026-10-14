# Mankart, Michaelides and Pagratis stage 2: solver validation

Run 14 October 2026 under PREREG_stage2.md (commit 3e8a7a24) and
under PREREG_stage2_2.md (commit 7b998d98), before any run of the full
model. Solver: mmp_model.py (value function iteration over equity and
loans by deposits, the write-off node and the cycle state; grid search
then Nelder-Mead over dividends, new loans and wholesale funding; policy
evaluation between optimisations), inputs in mmp_config.py.

## History

- Run 1 failed both tests. V1's known answer x = kappa (e + 1) was wrong
  for this model: it assumes borrowing against future deposit income,
  which the constraint e - x > 0 rules out. V2 failed for write-offs
  because the floor at zero (our R4) truncated a near-zero-mean normal
  process and 9 nodes were coarse. Both were amended in PREREG_stage2_2.md
  before any further run: V1 becomes pure cake-eating with x = kappa e;
  write-offs use 15 nodes with no floor, as equation (1) states.
- Run 2: V2 passed; V1 failed (policy errors 0.096 and 0.065 on 81
  linear equity points). Diagnosis: with gamma > 1 the true value tends
  to minus infinity at zero equity, and the grid's node at zero (value
  -1e30) corrupted the interpolated values above it; with the exact value
  plugged in, a coarse search over x still missed kappa e by about 7%,
  because linear interpolation makes the slope of V piecewise constant.
  The paper also uses linear interpolation (n. 26).
- Run 3 (this file): V1 on geometric equity grids from 1e-6 to 0.4 with
  161 to 1,281 points. The policy and value errors fall with the grid
  (large: policy 3.4% to 0.4%; small: 4.3% to 0.3%), so the solver
  converges to the known answer; V1 is judged at 1,281 points and passes
  for both parameter sets. With the small-bank parameters the iteration
  stops at its 400-sweep cap with a last change of about 5e-08, above the
  1e-08 tolerance; the answer is accurate there all the same. The V1 grid
  is the test's, not the model's: the model's 41-point grid is checked by
  V3 in step 2d.

## Findings

- V1 passes: at 1,281 points the dividend policy is within 0.4% of the
  exact kappa e for both size classes' beta and gamma, and the value
  within 0.12%.
- V2 passes: the 15-node write-off chain reproduces each state's mean,
  s.d. and AR(1) within 10% for both size classes (worst: large banks'
  recession mean, 0.335 against 0.37); the deposit-growth quadrature is
  exact.

Pinned: mmp_model.py sha256 aa51c30d5bc933ff2491cfec74db19884a5c60a312c6d66fc91611d049112752
Pinned: mmp_config.py sha256 06cc133c320e1bb8c5156be77776c6e320ee960372e7ead0f1160a4a2af60c29
Pinned: validate_stage2.py sha256 846906878c2b0646e10ef84de340140ee973a2eec7d096a85c1d86fa83026ff3

Reproduce: python3 validate_stage2.py

```
V1 large (gamma 1.3023, beta 0.9753, kappa 0.02128), 161 geometric equity points: policy error 3.35e-02, value error 7.16e-03, 11 sweeps, last change 2.0e-11
V1 large (gamma 1.3023, beta 0.9753, kappa 0.02128), 321 geometric equity points: policy error 1.08e-02, value error 3.01e-03, 14 sweeps, last change 1.1e-11
V1 large (gamma 1.3023, beta 0.9753, kappa 0.02128), 641 geometric equity points: policy error 8.23e-04, value error 8.99e-04, 12 sweeps, last change 1.0e-08
V1 large (gamma 1.3023, beta 0.9753, kappa 0.02128), 1281 geometric equity points: policy error 3.63e-03, value error 1.13e-04, 22 sweeps, last change 1.9e-11
V1 large at the finest grid -> passes
V1 small (gamma 1.9054, beta 0.9859, kappa 0.01211), 161 geometric equity points: policy error 4.29e-02, value error 1.12e-02, 400 sweeps, last change 5.6e-08
V1 small (gamma 1.9054, beta 0.9859, kappa 0.01211), 321 geometric equity points: policy error 2.07e-02, value error 5.73e-03, 400 sweeps, last change 1.7e-08
V1 small (gamma 1.9054, beta 0.9859, kappa 0.01211), 641 geometric equity points: policy error 9.25e-03, value error 2.77e-03, 400 sweeps, last change 5.4e-08
V1 small (gamma 1.9054, beta 0.9859, kappa 0.01211), 1281 geometric equity points: policy error 3.45e-03, value error 1.23e-03, 400 sweeps, last change 4.4e-08
V1 small at the finest grid -> passes
V2 large expansion write-offs: mean 0.225 [0.22], s.d. 0.150 [0.14], AR(1) 0.521 [0.51] -> passes
V2 large recession write-offs: mean 0.335 [0.37], s.d. 0.275 [0.28], AR(1) 0.688 [0.7] -> passes
V2 large expansion deposit growth quadrature: mean 1.6400 [1.64], s.d. 5.4100 [5.41] -> passes
V2 large recession deposit growth quadrature: mean 1.6000 [1.6], s.d. 5.3100 [5.31] -> passes
V2 small expansion write-offs: mean 0.081 [0.08], s.d. 0.103 [0.1], AR(1) 0.144 [0.14] -> passes
V2 small recession write-offs: mean 0.129 [0.13], s.d. 0.201 [0.2], AR(1) 0.199 [0.2] -> passes
V2 small expansion deposit growth quadrature: mean 0.9000 [0.9], s.d. 3.4800 [3.48] -> passes
V2 small recession deposit growth quadrature: mean 0.6500 [0.65], s.d. 3.5000 [3.5] -> passes
V1 passes; V2 passes
```
