# Pre-registration: Mankart, Michaelides and Pagratis, stage 2, sixth file

Written 17 October 2026, after results_nonfinite_stage2.md (commit
d4cfdcc3) and before any change to code or any run with a changed grid.
It amends the equity grid of PREREG_stage2.md (R2: e on [0, 0.4]); every
other item of the five earlier files stands.

## What was seen before this file

Everything listed in PREREG_stage2_4.md and PREREG_stage2_5.md, and the
results of the trace (results_nonfinite_stage2.md): with the large-bank
solution at 41 points, two banks are above the top of the equity grid by
quarter 100, the largest e there is 0.561, and one bank's equity reaches
7.5e+267 by quarter 150, where profit overflows. Above the grid the
simulation holds the policy of the top grid point, and the profit term
phi_E (e - x)^2 makes equity grow without bound. Nothing has been run on
any other grid.

## Amendment

E1. The equity grid keeps its 41 (or, for V3, 61) evenly spaced points on
    [0, 0.4] and gains points above 0.4 up to a top value e_top, spaced
    at five times the spacing below 0.4 (0.05 with 41 points, 0.0333 with
    61), the last point being e_top. The loan grid and everything else
    are unchanged. The code is a new module, mmp_grid.py, so that earlier
    results files keep their pins.

## Choice of e_top (select_grid.py), fixed now

- Candidates in order: 1.0, 2.0, 4.0.
- For a candidate, both size classes are solved on the 41-point version
  with the amended solver (solve_a1, rel 1e-6, howard 500, cap 400) and
  simulated with the seed, size and replacement rule of stage2.py. The
  script prints, for all 2,000 quarters and for the last 500: the number
  of bank-quarters with e or l not finite, with e above 0.4, with e above
  e_top, with l above the top of the loan grid (2.5), the largest e
  reached, and the number of failures. It prints no moment.
- A candidate is adopted when, for both size classes, the solve converges
  and no bank-quarter in any of the 2,000 quarters has a non-finite state
  or e above e_top. The first candidate that passes is adopted; the later
  ones are not run.
- If any bank-quarter has l above 2.5, that is reported and a further
  dated file is written before the full rerun.
- If no candidate passes, nothing else is tried before a further dated
  file.

## Full rerun (stage2_ext.py), after the choice

stage2.py's run as amended by A1-A3, on the adopted grid with 41 and with
61 points below 0.4 (V3), for both size classes, with the same seed and
the same comparison with their Table 5 and the same rule. It also reports,
per solve, the counts printed by select_grid.py, so that any bank leaving
a grid in the full run is visible. The rerun of results_stage2.md stays
on record unchanged; the new moments are reported as the result under
this file.

## Known limits

The grid above 0.4 is coarser than below it; V3 refines both parts. The
choice of e_top uses only whether simulated banks stay finite and on the
grid, not the moments.
