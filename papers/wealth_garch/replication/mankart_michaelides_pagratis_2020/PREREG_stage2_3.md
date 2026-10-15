# Pre-registration: Mankart, Michaelides and Pagratis, stage 2, third file

Written 15 October 2026, after the first run of the full model was stopped
and before any diagnosis was run. It amends PREREG_stage2.md (commit
3e8a7a24) and PREREG_stage2_2.md (commit 7b998d98) in the items listed
under Amendments; everything else stands.

## What was seen before this file

The first run of the full model (stage2.py, mmp_model.py and mmp_config.py
at commit 21e1a556), launched twice with identical logs for the sweeps
both covered (the first launch was stopped at sweep 49 to move the run
out of the editor's process group; its log is stage2_log.txt, the second
launch's is stage2_run_log.txt):
- Large banks, 41-point grids: not converged. The iteration ran to its
  200-sweep cap (54,104 s). From sweep 20 the largest change per sweep
  repeated exactly with period 6 (951.4, 951.4, 143.5, 951.4, 951.4,
  60.46); the last change was 60.5 against a tolerance of 1e-6. The
  failure value v^d is about -3,527, so the swings are about a quarter of
  the value range.
- Small banks, 41-point grids: stopped by us at sweep 34. From sweep 13
  the largest change stayed between 425.9 and 426.7 with no trend (v^d is
  about -751,000).
- While computing the large-bank moments the script raised numpy's "Mean
  of empty slice" warning: at least one simulated bank had no live quarter
  among the 500 recorded, so a replaced bank starting at the survivors'
  mean failed again at once, repeatedly.
- No moment was printed or seen: stage2.py prints only after all four
  solves, and stage2_out.txt is empty. The 61-point solves never started.

Reading of the logs. From v^d everywhere, policy iteration with exact
improvement raises the value function monotonically, so values that go
down as well as up mean the improvement step is not exact. The grid
search and Nelder-Mead start afresh each sweep and nothing keeps the
previous policy as a candidate, so a cell can switch to a worse policy
when the value function moves slightly, and the 500 evaluation steps that
follow magnify the loss before the next sweep switches it back. This is a
hypothesis; the diagnosis below tests it.

## Diagnosis (diagnose_stage2.py)

Run on a small grid for speed: 21 points each for e and l, 7 write-off
nodes, 3 quadrature nodes, both size classes, everything else as in
mmp_config.py. No simulation and no Table 5 moment is computed in the
diagnosis.
- D1. The pre-registered solver (500 evaluation steps) for 60 sweeps;
  the largest change per sweep, to confirm the stall appears on this
  grid.
- D2. Monotonicity: at every sweep, the value of the previous sweep's
  policy under the current value function (one evaluation step) is
  compared with the optimiser's value at every cell; the number of cells
  where the optimiser falls short by more than 1e-9 times the value
  range, and the largest shortfall, are reported.
- D3. The cells whose value moves by more than 1e-3 of the value range
  between successive sweeps once the stall has set in; their count and,
  for up to ten of them, the state (e, l, w, b) and the policy (x, n, f)
  on both sides of the move.
- D4. The same 60 sweeps with 0 and 20 evaluation steps; the size of the
  stalled changes against the 500-step run.
- D5. The candidate change A1 below on the same grid for up to 400
  sweeps: whether any cell's value ever falls (by more than 1e-9 of the
  value range), and the largest change per sweep.

## Decision rules, fixed now

- If D2 reports cells where the optimiser falls short of the previous
  policy, for either size class, the cause is inexact policy improvement
  and A1 is adopted, provided D5 shows no fall in value and a largest
  change below the A2 tolerance within 400 sweeps for both size classes.
- If D2 reports no such cell, or D5 fails, the cause lies elsewhere and a
  further dated file is written before any other change is tried.
- D3 and D4 are reported; they decide nothing.

## Amendments, conditional on the rules above

A1. Policy improvement keeps the previous policy: at every cell the
    candidates are the grid search's best point, Nelder-Mead started from
    it, Nelder-Mead started from the previous sweep's policy, and the
    previous policy itself; the best of the four is taken. The first sweep
    has no previous policy and runs as before.
A2. Stopping rule: the iteration stops when the largest change is at most
    1e-6 of the value range (the largest value on the grid minus v^d), in
    place of the absolute 1e-6, with the cap raised to 400 sweeps. A solve
    that reaches the cap is reported as not converged, with its last
    change, and its moments are reported but do not count as reproduced.
A3. Banks with no live quarter among the 500 recorded contribute to the
    failure rate only; the ratio means and standard deviations are over
    banks with at least one live quarter, and the number of banks with
    none is reported for each solve. This states what R5 already does by
    taking means over the available quarters.

The amended solver must pass V1 and V2 again (unchanged tests) before the
full model is rerun; those results are appended to
results_validation_stage2.md. The full rerun then follows PREREG_stage2.md
as amended, with the 61-point V3 solves unchanged.
