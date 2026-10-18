# Pre-registration: Mankart, Michaelides and Pagratis, stage 3, first file: a faster solver

Written 18 October 2026, before any code for it. Stage 3 estimates the
model by the method of simulated moments, which needs hundreds of solves;
the solver of stage 2 (mmp_a1.solve_a1) takes 37 to 63 sweeps of about 5
minutes each on the 41-point grid. This file fixes three changes to the
iteration, the tests the changed solver must pass, and the rule for
adopting it. It changes nothing in the model.

## What was seen before this file

Everything in the stage 2 files, and a profile of one sweep of solve_a1
(large banks, 41 points, from the saved solution, with another process on
the machine): grid search 210 s (61%), 500 policy-evaluation steps 102 s
(29%), Nelder-Mead 17 s, keep-better 17 s; 113,467,500 candidate
evaluations per sweep, 57.3% feasible, each feasible one 150
interpolations; the simulation 113 s. A cold solve on a 21-point grid
took 30 sweeps and 2,372 s.

A smoke test of mmp_fast.py, written before this file was committed, on a
7 by 7 grid (small banks): the changed solver converged in 83 sweeps (13
full) and 112 s against solve_a1's 69 sweeps and 501 s, and the two value
functions differ by up to 0.0118 of the value range; a warm start at the
same parameters converged in one sweep. The tests and thresholds below
were not changed after it.

## Changes (mmp_fast.py)

S1. Light sweeps. A sweep is full (grid search, Nelder-Mead from its best
    point, then A1's keep-better) or light (A1's keep-better alone: the
    previous policy and Nelder-Mead started from it). The first three
    sweeps are full, then every tenth; a light sweep whose largest change
    is within the tolerance is followed by a full sweep, and convergence
    is declared only on a full sweep whose change is within the
    tolerance (A2's rule). Both kinds keep the A1 property that no cell's
    policy is worse than the previous one, so values still only rise.
S2. Policy evaluation: 50 steps per sweep instead of 500.
S3. Warm start, for use between estimation trials: the iteration may
    start from a previous solution V' and policy p' (other parameters)
    instead of from v^d, as V0 = max(V' - k, v^d) with k = max(0, largest
    of V' - T_p' V') / (1 - beta) under the new parameters, T_p' being
    one policy-evaluation step. This makes V0 <= T V0, so the iteration
    still rises monotonically to a fixed point.

## Tests (validate_fast.py)

F1. V1 of PREREG_stage2_2.md with the changed solver (S1 and S2): the
    policy within 1% of kappa e at 1,281 geometric points, both size
    classes.
F2. Large banks, 41 points, cold start: the converged value function
    within 1e-4 of the value range of the stage 2 solution
    (solved_large_41.npz) at every cell. Reported as well: the share of
    cells where each solution is higher (a converged value function is
    the value of its policy, so the higher one belongs to the better
    policy).
F3. Small banks, 41 points, cold start: each of the 11 simulated moments
    within 1% (relative) of the 41-point moments of results_stage2.md,
    the failure rate within 0.005 percentage points; and no non-finite
    state.
F4. Warm start: small banks with phi_F, phi_E, c, phi_N and c_D times
    1.05, gamma times 1.02 and beta unchanged, solved warm from the F3
    solution and solved cold; the two value functions within 1e-4 of the
    value range, and the 11 moments within 1% (failure rate 0.005 pp).
For each solve the sweep count, the number of full sweeps and the time
are reported.

## Rule

The changed solver is adopted for stage 3 if F1 to F4 all pass. If any
fails, nothing is changed and a further dated file comes first. No
moment is compared with their Table 5 in these tests; F3 compares with
our own stage 2 solution only.
