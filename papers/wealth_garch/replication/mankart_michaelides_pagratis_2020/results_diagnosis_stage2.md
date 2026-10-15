# Mankart, Michaelides and Pagratis stage 2: diagnosis of the solver stall

Run 15 October 2026 (76 minutes) under PREREG_stage2_3.md (commit
9f2e3f20), with the code committed in 124df091 before the run. The
first run of the full model it follows is recorded in stage2_log.txt
and stage2_run_log.txt. No simulation and no Table 5 moment is
computed here.

## Findings

- D1, the stall appears on the small grid for both size classes. Large
  banks repeat a six-value cycle from sweep 10 (5.926, 5.139, 8.248,
  6.006, 5.122, 8.156), the same period as on the full grid; small banks
  sit at 2.532e+04 to four digits from sweep 8.
- D2, the optimiser falls short of the previous policy. In 58 of 59
  comparisons for each size class there are cells where the value of the
  previous sweep's policy under the current value function exceeds the
  optimiser's by more than 1e-9 of the value range. From the tenth
  comparison on that is 262 to 289 cells (large) and 562 to 622 cells
  (small) every sweep; the largest shortfall is 0.513 of the value range
  (large) and 0.065 (small).
- D3, few cells move. In sweeps 30 to 59, between 1 and 2 cells (large)
  and exactly 69 cells (small) move by more than 1e-3 of the value range
  per sweep. Among the ten largest moves in sweep 59, large banks have
  three cells at e = 0.12 and l = 2.0 (wholesale funding f from 0.83 to
  0.99, loans being repaid) and seven at e = 0.04 and l of at most 0.125
  (f about 0.28, new loans about 0.05); small banks have cells at high
  loans (l from 1.75 to 2.5, e from 0.12 to 0.30, f from 0.56 to 1.05).
  The policies differ between consecutive sweeps by small amounts, except
  in a few cells where dividends or new loans change by a factor of two
  or more.
- D4, the evaluation steps do not drive the stall. With 20 steps both
  size classes reach the same plateau as with 500 (large: the same
  six-value cycle with values within 0.2%; small: 2.532e+04). With 0 steps the
  change never settles (large 6.4e+01 to 6.4e+02, small 9.2e+03 to
  8.7e+04 over sweeps 0 to 59).
- D5, amendment A1 converges with no fall in value. Large banks
  converged at sweep 68 with a last change of 1.008e-04 against a
  tolerance of 3.043e-03; small banks at sweep 54 with 4.497e-01 against
  7.377e-01. The largest fall in any cell between sweeps was 0 of the
  value range in both.

## Decision under PREREG_stage2_3.md

D2 reports cells where the optimiser falls short, for both size classes,
and D5 shows no fall in value and a change below the A2 tolerance within
400 sweeps for both: A1 is adopted, together with A2 and A3 as written.

## Limits

- The diagnosis grid is coarse (21 by 21, 7 write-off nodes, 3 quadrature
  nodes). That A1 converges there does not show that it converges on the
  41 and 61 point grids; the full rerun does.
- A converged A1 solve is a fixed point of the search (no candidate
  improves any cell), not a proof of a global optimum. V1 and V2 are the
  check, to be rerun on the amended solver.
- The size of the largest D2 shortfall (half the value range for large
  banks) says that at some cell the optimiser returned a value far below
  a feasible policy it had before. What causes it was not tested; a
  grid search that misses a narrow feasible region near the capital
  constraints is the candidate.
- In the large-bank D5 run the change was 312 at sweep 66, then 0.0586
  and 1.0e-04: the iteration can still jump late before settling.

Pinned: mmp_model.py sha256 aa51c30d5bc933ff2491cfec74db19884a5c60a312c6d66fc91611d049112752
Pinned: mmp_config.py sha256 06cc133c320e1bb8c5156be77776c6e320ee960372e7ead0f1160a4a2af60c29
Pinned: mmp_keep.py sha256 194ade3e11432b4cb81bec3e01d3cccec0f0504ab2033bae43fe1b427ee073d1
Pinned: diagnose_stage2.py sha256 c8eb2e2fee259fbfe8afe8507c95d5e6f1174d3b5f2e40868da877464cc84845

Reproduce: python3 diagnose_stage2.py

```
mmp_model.py sha256 aa51c30d5bc933ff2491cfec74db19884a5c60a312c6d66fc91611d049112752
mmp_config.py sha256 06cc133c320e1bb8c5156be77776c6e320ee960372e7ead0f1160a4a2af60c29
mmp_keep.py sha256 194ade3e11432b4cb81bec3e01d3cccec0f0504ab2033bae43fe1b427ee073d1
diagnose_stage2.py sha256 c8eb2e2fee259fbfe8afe8507c95d5e6f1174d3b5f2e40868da877464cc84845

===== large: 21x21 equity and loan points, 7 write-off nodes, 3 quadrature nodes; v^d -3.5267e+03
D1. Pre-registered solver, 500 evaluation steps
  500 steps: largest change by sweep
   2.645e+02 1.255e+03 1.992e+03 1.259e+03 6.777e+02 6.497e+02 4.163e+02 2.567e+02 1.731e+02 8.166e+00
   5.926e+00 5.139e+00 8.248e+00 6.006e+00 5.122e+00 8.156e+00 5.926e+00 5.139e+00 8.248e+00 6.006e+00
   5.122e+00 8.156e+00 5.926e+00 5.139e+00 8.248e+00 6.006e+00 5.122e+00 8.156e+00 5.926e+00 5.139e+00
   8.248e+00 6.006e+00 5.122e+00 8.156e+00 5.926e+00 5.139e+00 8.248e+00 6.006e+00 5.122e+00 8.156e+00
   5.926e+00 5.139e+00 8.248e+00 6.006e+00 5.122e+00 8.156e+00 5.926e+00 5.139e+00 8.248e+00 6.006e+00
   5.122e+00 8.156e+00 5.926e+00 5.139e+00 8.248e+00 6.006e+00 5.122e+00 8.156e+00 5.926e+00 5.139e+00
D2. Cells where the optimiser falls short of the previous policy (more than 1e-9 of the value range)
      0    4   47   74  113  263  558  596  324  285
    274  282  274  269  263  285  270  284  271  277
    265  282  267  289  274  272  262  287  270  284
    271  277  265  282  267  289  274  272  262  287
    270  284  271  277  265  282  267  289  274  272
    262  287  270  284  271  277  265  282  267
  sweeps with a shortfall: 58 of 59; largest shortfall 5.133e-01 of the value range
D3. Cells moving by more than 1e-3 of the value range, sweeps 30 to 59
  per sweep: min 1, max 2
   e 0.120 l 2.000 w -0.470% b 1: change +1.689e-03 of range; policy (x, n, f) before (0.0016, -0.0622, 0.9924) after (0.0018, -0.0437, 0.8540)
   e 0.120 l 2.000 w +0.650% b 1: change +1.155e-03 of range; policy (x, n, f) before (0.0005, -0.0566, 0.8252) after (0.0004, -0.0444, 0.8367)
   e 0.120 l 2.000 w +0.090% b 1: change +7.475e-04 of range; policy (x, n, f) before (0.0016, -0.0384, 0.8888) after (0.0017, -0.0305, 0.8516)
   e 0.040 l 0.125 w +0.090% b 0: change +6.241e-04 of range; policy (x, n, f) before (0.0004, +0.0429, 0.2791) after (0.0003, +0.0460, 0.2830)
   e 0.040 l 0.000 w -0.190% b 0: change +3.398e-04 of range; policy (x, n, f) before (0.0003, +0.0491, 0.2832) after (0.0003, +0.0495, 0.2833)
   e 0.040 l 0.000 w +0.090% b 0: change +3.219e-04 of range; policy (x, n, f) before (0.0003, +0.0456, 0.2830) after (0.0003, +0.0459, 0.2833)
   e 0.040 l 0.000 w -0.470% b 0: change +3.091e-04 of range; policy (x, n, f) before (0.0003, +0.0537, 0.2832) after (0.0003, +0.0528, 0.2831)
   e 0.040 l 0.125 w -0.190% b 0: change +3.079e-04 of range; policy (x, n, f) before (0.0003, +0.0487, 0.2829) after (0.0003, +0.0499, 0.2832)
   e 0.040 l 0.000 w +0.370% b 0: change +2.663e-04 of range; policy (x, n, f) before (0.0003, +0.0433, 0.2824) after (0.0003, +0.0432, 0.2831)
   e 0.040 l 0.125 w -0.470% b 0: change +2.172e-04 of range; policy (x, n, f) before (0.0003, +0.0522, 0.2830) after (0.0003, +0.0525, 0.2828)
D4. Pre-registered solver with fewer evaluation steps
  0 steps: largest change by sweep
   9.917e+01 9.435e+01 9.064e+01 8.732e+01 8.800e+01 1.277e+02 1.312e+02 2.269e+02 1.322e+02 1.860e+02
   1.757e+02 1.294e+02 1.990e+02 1.691e+02 2.168e+02 2.476e+02 2.895e+02 3.074e+02 3.461e+02 3.813e+02
   2.933e+02 3.946e+02 3.570e+02 2.942e+02 4.071e+02 2.874e+02 1.575e+02 3.566e+02 4.127e+02 2.101e+02
   2.489e+02 2.562e+02 4.388e+02 4.502e+02 1.589e+02 2.248e+02 3.956e+02 4.700e+02 5.372e+02 5.523e+02
   4.994e+02 2.741e+02 5.702e+02 1.776e+02 3.112e+02 4.207e+02 6.380e+01 5.190e+02 1.562e+02 3.817e+02
   3.795e+02 5.876e+02 3.697e+02 6.354e+02 4.268e+02 4.022e+02 2.945e+02 3.944e+02 4.267e+02 6.003e+02
  20 steps: largest change by sweep
   2.645e+02 1.092e+03 1.533e+03 9.586e+02 4.895e+02 7.131e+02 6.964e+02 5.567e+02 5.273e+02 6.094e+02
   6.303e+02 6.537e+02 1.372e+02 5.236e+01 4.522e+01 2.567e+02 7.967e+01 8.104e+00 5.958e+00 5.157e+00
   8.240e+00 6.012e+00 5.124e+00 8.155e+00 5.927e+00 5.140e+00 8.237e+00 6.007e+00 5.122e+00 8.156e+00
   5.926e+00 5.139e+00 8.237e+00 6.007e+00 5.122e+00 8.156e+00 5.926e+00 5.139e+00 8.237e+00 6.007e+00
   5.122e+00 8.156e+00 5.926e+00 5.139e+00 8.237e+00 6.007e+00 5.122e+00 8.156e+00 5.926e+00 5.139e+00
   8.237e+00 6.007e+00 5.122e+00 8.156e+00 5.926e+00 5.139e+00 8.237e+00 6.007e+00 5.122e+00 8.156e+00
D5. Amendment A1 (keep the previous policy), 500 evaluation steps, tolerance 1e-6 of the value range
  converged at sweep 68, last change 1.008e-04; largest fall in any cell 0.000e+00 of the value range
  outer 0: max change 2.645e+02 (tolerance 2.645e-04)
  outer 1: max change 1.253e+03 (tolerance 1.356e-03)
  outer 2: max change 1.981e+03 (tolerance 2.644e-03)
  outer 3: max change 1.309e+03 (tolerance 2.993e-03)
  outer 4: max change 5.648e+02 (tolerance 3.032e-03)
  ...
  outer 64: max change 9.905e+00 (tolerance 3.043e-03)
  outer 65: max change 4.865e+01 (tolerance 3.043e-03)
  outer 66: max change 3.126e+02 (tolerance 3.043e-03)
  outer 67: max change 5.855e-02 (tolerance 3.043e-03)
  outer 68: max change 1.008e-04 (tolerance 3.043e-03)

===== small: 21x21 equity and loan points, 7 write-off nodes, 3 quadrature nodes; v^d -7.5134e+05
D1. Pre-registered solver, 500 evaluation steps
  500 steps: largest change by sweep
   3.524e+04 2.666e+05 5.317e+05 2.862e+05 8.092e+04 4.724e+04 3.843e+04 2.543e+04 2.532e+04 2.532e+04
   2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04
   2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04
   2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04
   2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04
   2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04
D2. Cells where the optimiser falls short of the previous policy (more than 1e-9 of the value range)
      0    6   63  234  298  455  700  845  646  619
    592  589  582  576  580  576  584  622  605  578
    595  592  600  570  574  563  607  562  581  573
    611  601  590  578  571  585  609  602  590  578
    571  585  609  602  590  578  571  585  609  602
    590  578  571  585  609  602  590  578  571
  sweeps with a shortfall: 58 of 59; largest shortfall 6.534e-02 of the value range
D3. Cells moving by more than 1e-3 of the value range, sweeps 30 to 59
  per sweep: min 69, max 69
   e 0.120 l 1.750 w +0.130% b 1: change +3.432e-02 of range; policy (x, n, f) before (0.0020, -0.0271, 0.6050) after (0.0011, -0.0736, 0.5577)
   e 0.220 l 2.375 w -0.270% b 1: change -2.117e-02 of range; policy (x, n, f) before (0.0002, -0.1307, 1.0245) after (0.0001, -0.1049, 1.0504)
   e 0.300 l 2.500 w +0.330% b 0: change -1.273e-02 of range; policy (x, n, f) before (0.0005, -0.1841, 1.0179) after (0.0066, -0.1752, 1.0330)
   e 0.240 l 2.375 w -0.470% b 0: change +6.746e-03 of range; policy (x, n, f) before (0.0003, -0.1470, 1.0076) after (0.0003, -0.1657, 0.9909)
   e 0.160 l 2.000 w +0.130% b 1: change +6.205e-03 of range; policy (x, n, f) before (0.0003, -0.0844, 0.7560) after (0.0002, -0.0868, 0.7536)
   e 0.160 l 2.000 w +0.330% b 1: change +6.163e-03 of range; policy (x, n, f) before (0.0002, -0.0872, 0.7530) after (0.0003, -0.0864, 0.7539)
   e 0.280 l 2.375 w +0.530% b 1: change +6.098e-03 of range; policy (x, n, f) before (0.0006, -0.1173, 0.9783) after (0.0009, -0.1839, 0.9150)
   e 0.220 l 2.250 w +0.530% b 1: change +6.044e-03 of range; policy (x, n, f) before (0.0003, -0.1098, 0.9237) after (0.0003, -0.1143, 0.9161)
   e 0.160 l 2.000 w -0.270% b 1: change +5.947e-03 of range; policy (x, n, f) before (0.0004, -0.0912, 0.7493) after (0.0003, -0.0870, 0.7534)
   e 0.160 l 2.000 w -0.070% b 1: change +5.866e-03 of range; policy (x, n, f) before (0.0003, -0.0871, 0.7533) after (0.0004, -0.0901, 0.7503)
D4. Pre-registered solver with fewer evaluation steps
  0 steps: largest change by sweep
   1.615e+04 1.574e+04 1.534e+04 1.544e+04 2.030e+04 1.427e+04 2.122e+04 1.393e+04 1.346e+04 1.336e+04
   1.406e+04 1.884e+04 2.021e+04 1.477e+04 4.168e+04 1.752e+04 1.951e+04 2.215e+04 2.783e+04 1.944e+04
   1.737e+04 2.067e+04 1.281e+04 2.590e+04 2.273e+04 4.844e+04 5.845e+04 1.521e+04 3.435e+04 1.277e+04
   1.799e+04 2.247e+04 1.855e+04 1.508e+04 3.343e+04 2.303e+04 7.684e+04 2.109e+04 1.112e+04 8.717e+04
   1.586e+04 1.355e+04 3.553e+04 2.678e+04 2.578e+04 9.179e+03 2.153e+04 2.019e+04 1.537e+04 2.623e+04
   2.754e+04 3.927e+04 2.001e+04 2.150e+04 7.077e+04 2.955e+04 2.475e+04 4.284e+04 4.633e+04 3.029e+04
  20 steps: largest change by sweep
   3.524e+04 2.194e+05 3.417e+05 2.058e+05 9.660e+04 6.881e+04 6.619e+04 1.114e+05 1.146e+05 4.623e+04
   4.732e+04 2.216e+04 2.686e+04 2.412e+04 2.594e+04 2.485e+04 2.556e+04 2.514e+04 2.541e+04 2.525e+04
   2.535e+04 2.529e+04 2.533e+04 2.531e+04 2.532e+04 2.531e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04
   2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04
   2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04
   2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04 2.532e+04
D5. Amendment A1 (keep the previous policy), 500 evaluation steps, tolerance 1e-6 of the value range
  converged at sweep 54, last change 4.497e-01; largest fall in any cell 0.000e+00 of the value range
  outer 0: max change 3.524e+04 (tolerance 3.524e-02)
  outer 1: max change 2.666e+05 (tolerance 2.814e-01)
  outer 2: max change 5.277e+05 (tolerance 6.214e-01)
  outer 3: max change 3.181e+05 (tolerance 7.263e-01)
  outer 4: max change 8.016e+04 (tolerance 7.346e-01)
  ...
  outer 50: max change 1.218e+01 (tolerance 7.377e-01)
  outer 51: max change 1.251e+01 (tolerance 7.377e-01)
  outer 52: max change 8.888e+00 (tolerance 7.377e-01)
  outer 53: max change 2.312e+00 (tolerance 7.377e-01)
  outer 54: max change 4.497e-01 (tolerance 7.377e-01)
```
