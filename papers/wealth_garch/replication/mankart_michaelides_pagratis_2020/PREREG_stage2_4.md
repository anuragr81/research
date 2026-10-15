# Pre-registration: Mankart, Michaelides and Pagratis, stage 2, fourth file

Written 15 October 2026, while the rerun of the full model is in progress
(stage2.py at commit 390dea33, unit mmp-stage2-rerun) and before any
change to code or any further look at its output. It amends
PREREG_stage2.md, PREREG_stage2_2.md and PREREG_stage2_3.md only in the
items listed under Amendments.

## What was seen before this file

The first result of the rerun, large banks on the 41-point grids
(rerun_out.txt and rerun_log.txt):
- The solve converged by rule A2 after 46 sweeps (12,724 s), last change
  2.7e-04, largest fall in any cell 0.0 of the value range; no bank had
  no live quarter.
- Moments (theirs in brackets): failure rate 0.0095 [0.092]; loans/assets
  0.364 [0.704]; deposits/assets 0.337 [0.638]; dividends/equity 0.0148
  [0.029]; s.d. loans/assets 0.067 [0.100]; s.d. deposits/assets 0.040
  [0.062]; s.d. dividends/equity 0.0037 [0.011]. Equity/assets,
  profit/equity and their s.d.s are NaN, and numpy printed "Mean of
  empty slice", "Degrees of freedom <= 0" and "invalid value encountered
  in subtract".
- By the rule of PREREG_stage2.md none of the seven finite moments
  reproduces; the four NaN moments cannot be judged.
- Before the rerun, a smoke test of stage2.py (7 by 7 grid, 500 banks,
  40 quarters, small banks) printed moments; it is not a result and is
  not used.
The solved value function and policy are not saved by stage2.py, so the
cause of the NaN cannot be examined from the run.

## Reading

The NaN lies in how the moments are computed from the simulated
records, not in the solved model; why dividends/equity (x/eq) is finite
while equity/assets (eq/assets) is not is not understood, and no
explanation is adopted before the arrays are examined.

## Amendments

C1. The running job is not touched. Its output is the pre-registered
    result of the rerun and is reported as printed, NaN included; the
    V3 comparison for the 61-point solves and the small-bank solves are
    taken from it.
C2. After it finishes, large banks are solved again on the 41-point grid
    with the same solver and parameters (mmp_a1.solve_a1, rel 1e-6,
    howard 500, cap 400), by a new script, solve_save.py, which writes the
    value function and policy to solved_large_41.npz and prints their
    sha256 and the sweep count. The solve is deterministic, so the sweep
    count and last change must equal those of the run (46 and 2.7e-04);
    if they differ, that is reported before anything else is done.
C3. A new script, inspect_stage2.py, simulates from the saved arrays with
    the code and seed of stage2.py and, with no change to the dynamics,
    reports for each output column the number of bank-quarters that are
    NaN, +inf or -inf, by the state (e, l, w, b) and the policy (x, n, f)
    in which they occur, and the number of banks with no finite value in
    each column.
C4. Decision rules, fixed now:
    - If every non-finite value comes from a division in the moment
      definitions (a ratio with a zero or non-finite denominator), then
      the moment code is amended to treat non-finite ratio values as
      missing, reporting their count per column; the model, solver and
      simulation dynamics stay as they are. The corrected moments are
      reported next to the printed ones, labelled as amended.
    - If any non-finite value originates in the dynamics (a state or a
      policy that is itself NaN or infinite, or a failure not counted),
      nothing is changed and a further dated file is written first.
    - Whatever the corrected moments are, no model choice (R1-R6) and no
      parameter is changed on the strength of them. A study of the
      sensitivity to the open choices is a separate pre-registration,
      listing the alternatives before any run and not chosen by fit.
