# Mankart, Michaelides and Pagratis stage 2: where the simulated equity becomes infinite

Run 17 October 2026 under PREREG_stage2_4.md (commit 7a7481b1) for the
inspection and under PREREG_stage2_5.md (commit 3afaaeab) for the trace,
on the large-bank solution at the 41-point grids (solved_large_41.npz,
solved again from the code of commit 390dea33).

## Findings

- Inspection (inspect_out.txt, commit 11052624 for the script; its
  reproduction takes 3.5 hours and is not in the MM-14 list). The re-solve
  converged in 46 sweeps with a last change of 2.7e-04, as in the run, and
  the moments simulated from it equal the printed ones. Equity/assets and
  profit/equity are NaN in 2,246,360 of 5,000,000 bank-quarters (4,210
  banks have no finite value) and profit/equity is +inf in 4 more.
  Loans/assets, deposits/assets and dividends/equity have no non-finite
  value. In the affected quarters the state e is infinite.
- Trace (below). The first non-finite state is at quarter 150: bank 6303
  starts the quarter with e = 7.5e+267, so the profit overflows in the
  step. At quarter 100 the largest finite e is 0.561 and two banks are
  above the top of the equity grid (0.4); no bank is above the top of the
  loan grid at that quarter. The bank was not a replacement.
- The mechanism, as the transition of PREREG_stage2.md writes it. Profit
  contains the term + phi_E (e - x)^2 (equity lowers the funding cost),
  which is convex in equity. Above the top of the grid the simulation uses
  the policy of the top grid point, a dividend x of about 0.02 whatever e
  is (interp2 holds the value at the edge), so a bank above the grid
  cannot pay out in proportion to its equity. Then e' = (e - x + (1 - tau)
  (phi_E (e - x)^2 + ...)) / Gamma' grows faster than exponentially and
  reaches 1e+267 by quarter 150 in the trace. This is read from the
  formula and the single bank in the trace; it was not tested by a run in
  which the policy above the grid differs.
- Consequence for the moments. An infinite e makes assets infinite, so
  loans/assets and deposits/assets are zero, dividends/equity is zero, and
  equity/assets and profit/equity are NaN. The large-bank moments in
  results_stage2.md, finite or not, are therefore affected by banks that
  left the grid. Whether small banks (phi_E 0.0072, no non-finite value)
  are affected too was not examined.

## Limits

- One solution (large banks, 41 points) and one run of the trace, which
  stops at the first non-finite state.
- The trace does not say how many banks left the grid, nor how far the
  moments of the banks that stayed move; the first is available from the
  summary lines every 100 quarters, the second only from a changed
  simulation, which needs its own pre-registration (the rule of
  PREREG_stage2_5.md).

Pinned: mmp_config.py sha256 06cc133c320e1bb8c5156be77776c6e320ee960372e7ead0f1160a4a2af60c29
Pinned: stage2.py sha256 b6be175625a9ce996d838975d872a1f4d01840725f6fa8c02028e299fab4aac6
Pinned: trace_stage2.py sha256 ec30e6eab52e90c81caeb829e2a25e23034570be4c9c7757d0e9f7fa6dee9d68
Pinned: solved_large_41.npz sha256 803df60b6655e4a1bd3749e28000077e14c531a59fa7b49e4fa510e35df7d9fe

Reproduce: python3 trace_stage2.py

```
top of the equity grid 0.4, of the loan grid 2.5
quarter 0 start: e not finite 0, l not finite 0, max finite e 0.1, e above grid 0, l above grid 0, replaced 0, replacement mean e nan l nan
quarter 100 start: e not finite 0, l not finite 0, max finite e 0.5613, e above grid 2, l above grid 0, replaced 46, replacement mean e 0.09397 l 0.8861
FIRST non-finite state at quarter 150, after the step: 1 banks, 0 of them replaced this quarter
   bank 6303: replaced False; start e 7.50822e+267 l 1.98306 w node 5; policy x 0.0212664 n 0.125501 f 0.729829 value -481.409; eq 7.50822e+267 s 7.50822e+267 w' 0.0013 pi inf after inf Gamma 1.00825; now e inf l 1.9631
```
