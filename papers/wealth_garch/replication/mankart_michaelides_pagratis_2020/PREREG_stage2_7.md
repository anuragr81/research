# Pre-registration: Mankart, Michaelides and Pagratis, stage 2, seventh file

Written 17 October 2026, after the grid candidate e_top 1.0 of
PREREG_stage2_6.md failed and candidate 2.0 was stopped, and before any
further run. It ends the grid search of PREREG_stage2_6.md and fixes how
stage 2 is reported.

## What was seen before this file

- select_e1_out.txt (candidate 1.0): both solves converged. Large banks
  fail the adoption rule: in 20,000,000 bank-quarters, 6,256,639 have a
  non-finite state, 522,855 have e above 0.4, 133,861 have e above 1.0 and
  1 has l above 2.5. Small banks pass: no non-finite state, 120
  bank-quarters with e above 0.4, none above 1.0, largest e 0.570.
- Candidate 2.0 was started as the rule required and stopped by the
  author's decision after three sweeps of the large-bank solve
  (select_e2_log.txt); it produced no count.
- Equation (13) of the 2018 manuscript, checked against the pinned PDF
  (printed page 19): g_F(F_t-1, D_t-1, E_t-1) = r_Dt F_t-1 + phi_F
  F_t-1^2 / D_t-1 - phi_E E_t-1^2 / D_t-1, with phi_E "the degree of cost
  reduction associated with bank equity holdings". The May 2016 draft has
  the same form. The paper states no bound on the equity grid and no rule
  for simulated states outside it (algorithm, printed pages 55-58).
- breakeven.py (illustration, results_runaway_stage2.md): ignoring
  deposit growth, risk and the constraints, retaining a unit of equity
  pays more than 1/beta - 1 once e exceeds about 0.18 (large banks,
  expansion) and 0.88 (small banks); their Table 5 implies e of about
  0.10 and 0.075.

## Reading

The term -phi_E E^2 / D does not scale with wholesale funding and has no
bound, so at the large-bank parameters a bank with enough equity gains
more from retaining it than from paying it out, and simulated equity can
grow without limit. A wider grid gives such banks more room, which fits
candidate 1.0 doing worse than 0.4. The authors' large-bank results then
rest on a bound they do not state. This reading rests on the formula, the
trace and the illustration, not on a proof.

## Decisions

G1. The grid search of PREREG_stage2_6.md ends; candidates 2.0 and 4.0
    are not run.
G2. The replication target stays the August 2018 manuscript. The
    published article (Financial Management 49(2):473-502, 2020) was not
    read; whether its equation (13) differs is stated as a limit.
G3. Stage 2 is reported as follows. Small banks: the moments of
    results_stage2.md (41 and 61 points on [0, 0.4]) stand as the
    replication result, with 0 non-finite states and their V3 notes; the
    candidate 1.0 run confirms that small banks stay below 0.57. Large
    banks: no moment comparison is reported as a result; the finding is
    that equation (13) at their Table 4 large-bank parameters lets
    simulated equity grow without bound, and that their large-bank
    moments depend on a bound the paper does not state.
G4. For the testing thread, MMP is carried with the small-bank
    parameters, and the reason large banks are left out is stated. A
    funding premium that applies to borrowed funds and has a floor is an
    enhancement-thread variant, specified and pre-registered there, and
    never presented as the authors' model.
