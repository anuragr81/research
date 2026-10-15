# Mankart, Michaelides and Pagratis stage 2: validation of the amended solver

Run 15 October 2026 under PREREG_stage2_3.md (commit 9f2e3f20), with the
code of commit 390dea33 (mmp_a1.py: amendments A1 and A2), before the
rerun of the full model. The tests are those of PREREG_stage2.md and
PREREG_stage2_2.md, unchanged; V2 does not use the solver and is
repeated through validate_stage2.py. This is a separate file from
results_validation_stage2.md, whose output block belongs to the original
solver and is reproduced from its own pins.

## Findings

- V1 passes with the amended solver: at 1,281 geometric equity points the
  dividend policy is within 0.363% (large) and 0.368% (small) of the exact
  kappa e, and the value within 0.011% and 0.123%. The policy error falls
  from 3.35% and 4.29% at 161 points, as with the original solver.
- Every V1 solve stops by the amended rule (a change of at most 1e-6 of
  the value range, over cells with a finite value) in 7 to 13 sweeps.
- V1's iteration starts from zero, above the true values for gamma > 1,
  so values fall as it converges; the "largest fall" column there is
  that descent and not a loss of ground by the optimiser. The test
  of no fall in value is D5 of results_diagnosis_stage2.md, which starts
  from the failure value.
- V2 passes, with the same values as in results_validation_stage2.md.

Pinned: mmp_model.py sha256 aa51c30d5bc933ff2491cfec74db19884a5c60a312c6d66fc91611d049112752
Pinned: mmp_config.py sha256 06cc133c320e1bb8c5156be77776c6e320ee960372e7ead0f1160a4a2af60c29
Pinned: mmp_a1.py sha256 4a6f85c1696af0a712920b6a31a5bc91221a73546e168f1cfc00ad675a6e2bf6
Pinned: validate_stage2.py sha256 846906878c2b0646e10ef84de340140ee973a2eec7d096a85c1d86fa83026ff3
Pinned: validate_stage2_a1.py sha256 d4cb0bedce3de7b3098fdeec27faa577890a18b911ab9ae21aa640c82a2be455

Reproduce: python3 validate_stage2_a1.py

```
V1 large (gamma 1.3023, beta 0.9753, kappa 0.02128), 161 geometric equity points: policy error 3.35e-02, value error 7.16e-03, 7 sweeps, converged, last change 7.7e-03, largest fall 2.3e-02
V1 large (gamma 1.3023, beta 0.9753, kappa 0.02128), 321 geometric equity points: policy error 1.08e-02, value error 3.01e-03, 8 sweeps, converged, last change 1.4e-03, largest fall 2.3e-02
V1 large (gamma 1.3023, beta 0.9753, kappa 0.02128), 641 geometric equity points: policy error 8.23e-04, value error 8.99e-04, 9 sweeps, converged, last change 5.4e-03, largest fall 2.3e-02
V1 large (gamma 1.3023, beta 0.9753, kappa 0.02128), 1281 geometric equity points: policy error 3.63e-03, value error 1.13e-04, 11 sweeps, converged, last change 4.4e-03, largest fall 2.3e-02
V1 large at the finest grid -> passes
V1 small (gamma 1.9054, beta 0.9859, kappa 0.01211), 161 geometric equity points: policy error 4.29e-02, value error 1.12e-02, 10 sweeps, converged, last change 1.2e+00, largest fall 2.5e-01
V1 small (gamma 1.9054, beta 0.9859, kappa 0.01211), 321 geometric equity points: policy error 2.08e-02, value error 5.73e-03, 11 sweeps, converged, last change 1.3e+01, largest fall 2.5e-01
V1 small (gamma 1.9054, beta 0.9859, kappa 0.01211), 641 geometric equity points: policy error 9.39e-03, value error 2.77e-03, 13 sweeps, converged, last change 1.7e+00, largest fall 2.5e-01
V1 small (gamma 1.9054, beta 0.9859, kappa 0.01211), 1281 geometric equity points: policy error 3.68e-03, value error 1.23e-03, 13 sweeps, converged, last change 3.8e+00, largest fall 2.5e-01
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
