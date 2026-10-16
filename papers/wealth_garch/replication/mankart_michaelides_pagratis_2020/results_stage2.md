# Mankart, Michaelides and Pagratis stage 2: the model at their parameters

Run 15 to 17 October 2026 as the systemd unit mmp-stage2-rerun, with the
code of commit 390dea33 (amendments A1 to A3):
run under PREREG_stage2.md (commit 3e8a7a24),
under PREREG_stage2_2.md (commit 7b998d98)
and under PREREG_stage2_3.md (commit 9f2e3f20). The NaN moments below are the subject of
PREREG_stage2_4.md (commit 7a7481b1), written after the large-bank 41-point
result was seen and before any change to code; its findings are reported
separately.

## History

- The first run (commit 21e1a556, stage2_log.txt and stage2_run_log.txt)
  did not converge: large banks cycled with period 6 to the 200-sweep cap,
  small banks stalled at a change of about 426 and were stopped at sweep
  34. No moment was printed. The diagnosis (results_diagnosis_stage2.md)
  found that the optimiser could fall short of the previous policy, and
  amendment A1 was adopted under the rules of PREREG_stage2_3.md.
- Two smoke tests of scripts were run outside any pre-registration and
  printed moments that are not results: stage2.py on a 7 by 7 grid (500
  banks, 40 quarters) before this run, and inspect_stage2.py on a 5 by 5
  grid after it.

## Findings

- All four solves converged under rule A2, with no fall in value in any
  cell: large banks in 46 sweeps (41 points, 3.5 h) and 48 (61 points,
  8.5 h); small banks in 37 (2.7 h) and 49 (8.6 h). No simulated bank was
  without a live quarter.
- Large banks: 0 of 11 moments reproduce. Seven are finite and all differ
  (loans/assets 0.364 against 0.704, deposits/assets 0.337 against 0.638,
  failure rate 0.0095% against 0.092%); equity/assets, profit/equity and
  their standard deviations are NaN on both grids.
- Small banks: 6 of 11 moments reproduce (failure rate, loans/assets,
  deposits/assets, dividends/equity, and the standard deviations of
  loans/assets and deposits/assets). Equity/assets is 0.093 against
  0.067, profit/equity 0.021 against 0.029, and the standard deviations
  of equity/assets, profit/equity and dividends/equity differ.
- V3, grid check: between 41 and 61 points, five large-bank moments move
  by more than 10% (failure rate, loans/assets, deposits/assets,
  dividends/equity, s.d. of loans/assets) and so do four small-bank
  moments (failure rate, dividends/equity, s.d. of equity/assets, s.d.
  of dividends/equity); these are not converged in the grid. The check
  does not flag a NaN moment, which cannot be judged.

## Limits

- The large-bank NaN makes the large-bank comparison uninformative until
  PREREG_stage2_4.md has been carried out; its rules decide whether the
  NaN comes from the moment code or from the simulated dynamics.
- Reproducing this file takes about 24 hours of computation on the
  machine used (8 threads), so it is not in the MM-14 standing list.

Pinned: mmp_model.py sha256 aa51c30d5bc933ff2491cfec74db19884a5c60a312c6d66fc91611d049112752
Pinned: mmp_config.py sha256 06cc133c320e1bb8c5156be77776c6e320ee960372e7ead0f1160a4a2af60c29
Pinned: mmp_a1.py sha256 4a6f85c1696af0a712920b6a31a5bc91221a73546e168f1cfc00ad675a6e2bf6
Pinned: stage2.py sha256 b6be175625a9ce996d838975d872a1f4d01840725f6fa8c02028e299fab4aac6

Reproduce: python3 stage2.py

```
mmp_model.py sha256 aa51c30d5bc933ff2491cfec74db19884a5c60a312c6d66fc91611d049112752
mmp_config.py sha256 06cc133c320e1bb8c5156be77776c6e320ee960372e7ead0f1160a4a2af60c29
mmp_a1.py sha256 4a6f85c1696af0a712920b6a31a5bc91221a73546e168f1cfc00ad675a6e2bf6
stage2.py sha256 b6be175625a9ce996d838975d872a1f4d01840725f6fa8c02028e299fab4aac6
large 41x41: converged after 46 sweeps; moments {'failure rate': np.float64(0.00948), 'banks with no live quarter': 0, 'loans/assets': 0.3636817738028072, 'sd loans/assets': 0.06740647733297916, 'deposits/assets': 0.3369104770608793, 'sd deposits/assets': 0.0398634443563017, 'equity/assets': nan, 'sd equity/assets': nan, 'profit/equity': nan, 'sd profit/equity': nan, 'dividends/equity': 0.014830792271885895, 'sd dividends/equity': 0.0036828196525863603}
small 41x41: converged after 37 sweeps; moments {'failure rate': np.float64(0.019540000000000002), 'banks with no live quarter': 0, 'loans/assets': 0.6138297708070061, 'sd loans/assets': 0.05559669501251658, 'deposits/assets': 0.865423141938981, 'sd deposits/assets': 0.025395274024704335, 'equity/assets': 0.09260349231629941, 'sd equity/assets': 0.022995185667503755, 'profit/equity': 0.0212632246843358, 'sd profit/equity': 0.01782317450286003, 'dividends/equity': 0.00916262872622702, 'sd dividends/equity': 0.0015806424717217233}
large 61x61: converged after 48 sweeps; moments {'failure rate': np.float64(0.01096), 'banks with no live quarter': 0, 'loans/assets': 0.297391166446982, 'sd loans/assets': 0.06014049864665076, 'deposits/assets': 0.2731397585863174, 'sd deposits/assets': 0.0369163826870809, 'equity/assets': nan, 'sd equity/assets': nan, 'profit/equity': nan, 'sd profit/equity': nan, 'dividends/equity': 0.01294237691873837, 'sd dividends/equity': 0.003417067137833274}
small 61x61: converged after 49 sweeps; moments {'failure rate': np.float64(0.0263), 'banks with no live quarter': 0, 'loans/assets': 0.6208294878076178, 'sd loans/assets': 0.05608033785501871, 'deposits/assets': 0.8738142920696231, 'sd deposits/assets': 0.023434368826011593, 'equity/assets': 0.08385414096459896, 'sd equity/assets': 0.020582957011262698, 'profit/equity': 0.023059351148349092, 'sd profit/equity': 0.019490084926259022, 'dividends/equity': 0.010686656468350491, 'sd dividends/equity': 0.0019533053772076278}

===== large: 41x41 grid (converged, 46 sweeps, last change 2.7e-04, 0 banks with no live quarter); 61x61 grid (converged, 48 sweeps, last change 2.4e-03, 0 banks with no live quarter)
  failure rate         ours   0.0095 [0.092] -> differs; 61x61   0.0110 (moves more than 10%: not converged)
  loans/assets         ours   0.3637 [0.704] -> differs; 61x61   0.2974 (moves more than 10%: not converged)
  deposits/assets      ours   0.3369 [0.638] -> differs; 61x61   0.2731 (moves more than 10%: not converged)
  equity/assets        ours      nan [0.065] -> differs; 61x61      nan
  profit/equity        ours      nan [0.055] -> differs; 61x61      nan
  dividends/equity     ours   0.0148 [0.029] -> differs; 61x61   0.0129 (moves more than 10%: not converged)
  sd loans/assets      ours   0.0674 [0.1] -> differs; 61x61   0.0601 (moves more than 10%: not converged)
  sd deposits/assets   ours   0.0399 [0.062] -> differs; 61x61   0.0369
  sd equity/assets     ours      nan [0.013] -> differs; 61x61      nan
  sd profit/equity     ours      nan [0.035] -> differs; 61x61      nan
  sd dividends/equity  ours   0.0037 [0.011] -> differs; 61x61   0.0034
  large: 0 of 11 moments reproduce

===== small: 41x41 grid (converged, 37 sweeps, last change 2.3e-01, 0 banks with no live quarter); 61x61 grid (converged, 49 sweeps, last change 2.4e-01, 0 banks with no live quarter)
  failure rate         ours   0.0195 [0.051] -> reproduces; 61x61   0.0263 (moves more than 10%: not converged)
  loans/assets         ours   0.6138 [0.626] -> reproduces; 61x61   0.6208
  deposits/assets      ours   0.8654 [0.891] -> reproduces; 61x61   0.8738
  equity/assets        ours   0.0926 [0.067] -> differs; 61x61   0.0839
  profit/equity        ours   0.0213 [0.029] -> differs; 61x61   0.0231
  dividends/equity     ours   0.0092 [0.012] -> reproduces; 61x61   0.0107 (moves more than 10%: not converged)
  sd loans/assets      ours   0.0556 [0.05] -> reproduces; 61x61   0.0561
  sd deposits/assets   ours   0.0254 [0.021] -> reproduces; 61x61   0.0234
  sd equity/assets     ours   0.0230 [0.015] -> differs; 61x61   0.0206 (moves more than 10%: not converged)
  sd profit/equity     ours   0.0178 [0.024] -> differs; 61x61   0.0195
  sd dividends/equity  ours   0.0016 [0.007] -> differs; 61x61   0.0020 (moves more than 10%: not converged)
  small: 6 of 11 moments reproduce
```
