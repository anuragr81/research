# Pre-registration: Mankart, Michaelides and Pagratis, stage 3, second file: where the solutions differ

Written 18 October 2026, after results_validation_fast.md (commit
879a9e81) and before any further run. It is written after F2 and F4 of
PREREG_stage3_1.md failed, and says so: the test it fixes would replace
the every-cell value criterion of F2 and F4 for the decision on the
faster solver.

## What was seen before this file

results_validation_fast.md: F1 and F3 pass; F2 fails (large banks: up to
0.560 of the value range from the stage 2 solution, the faster solver
higher in 79.2% of cells and lower in 2.1%); F4 fails on values (warm
against cold, up to 0.094 of the range) while all 11 moments agree within
1%. The solutions of that run were not saved.

## Reading

Two converged solutions that differ by this much show that the fixed
point the search reaches depends on its path. If the differences sit in
cells the simulated banks do not visit, the moments, which are all the
estimation uses, do not depend on them, which would fit F3 and F4's
agreement on moments.

## Check (check_visits.py)

- Small banks, 41 points, with the faster solver (mmp_fast.solve_fast,
  rel 1e-6, 50 evaluation steps, cap 400): the F3 solve (cold, Table 4
  parameters), then at F4's perturbed parameters a warm solve from it and
  a cold solve. Each is saved (solved_fast_small_41.npz,
  solved_fast_small_41_p_warm.npz, solved_fast_small_41_p_cold.npz) and
  its sweep count and last change are printed; they must equal those of
  results_validation_fast.md (50, 190 and 194 sweeps), or that is
  reported first.
- Both perturbed solutions are simulated with the seed and rules of
  stage2.py. A cell (e node, l node, write-off node, cycle state) counts
  as visited if it is one of the four corners used to interpolate the
  state of any live bank at the start of any of the 2,000 quarters, in
  either simulation.
- Reported: the number of cells visited and their share of all cells;
  the largest value difference between the warm and cold solutions over
  visited cells and over cells not visited, as shares of the value
  range; the share of corner-visits (four per bank-quarter) at cells whose
  difference exceeds 1e-4 of the range, and the number of such visited
  cells.

## Rule

- If the largest difference over visited cells is at most 1e-4 of the
  value range, then for stage 3 the faster solver's tests become: F1, F3,
  and agreement within 1e-4 of the range on visited cells together with
  agreement of the 11 moments within 1% (F4's moment criterion), all of
  which this check and results_validation_fast.md then meet, and the
  faster solver is adopted for stage 3 with this reason stated.
- Otherwise it is not adopted, and stage 3 uses the stage 2 solver, or a
  further dated file comes first.
- F2 (large banks) is not part of the decision, since large banks are
  left out of stage 3 under PREREG_stage2_7.md.
