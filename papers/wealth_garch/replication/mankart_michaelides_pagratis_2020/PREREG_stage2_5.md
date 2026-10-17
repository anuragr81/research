# Pre-registration: Mankart, Michaelides and Pagratis, stage 2, fifth file

Written 17 October 2026, after the inspection of PREREG_stage2_4.md (C2
and C3) was carried out and before any change to code or any further run.

## What was seen before this file

inspect_out.txt and inspect_log.txt (units mmp-inspect; commit 11052624):
- C2: the re-solve of large banks on the 41-point grid converged in 46
  sweeps with a last change of 2.7e-04, as in the run, and the moments
  simulated from the saved arrays equal the printed ones to every digit.
  The arrays are in solved_large_41.npz (V sha256 1cca4aff..., pol sha256
  bdbc9ada...).
- C3: over 5,000,000 bank-quarters (500 quarters of 10,000 banks) with 474
  failures, equity/assets and profit/equity are NaN in 2,246,360
  bank-quarters outside failure quarters (4,210 banks have no finite
  value), and profit/equity is +inf in 4 more. Loans/assets,
  deposits/assets and dividends/equity have no non-finite value. In the
  first ten affected bank-quarters (the first quarter of the window) the
  state is e = inf with finite l, and the policy at that state is finite
  (x about 0.02, n about 0.13, f about 0.56). An infinite e makes the
  assets s = 1 + f + e - x - l - n infinite and eq = e - x infinite, so
  equity/assets is inf/inf; it makes loans/assets and deposits/assets
  zero and dividends/equity x/eq zero. These explain which moments are
  NaN and why the other three are finite.
- A look at the saved policy array (not a pre-registered run): at l = 1,
  the middle write-off node and expansion, the policy at the top equity
  point (e = 0.4) is x 0.0167, n 0.130, f 0.564, and at e = 0.2 it is x
  0.0057, n 0.084, f 0.564.
By C4 of PREREG_stage2_4.md the infinity originates in the dynamics, so
nothing is changed. It has not been seen where the first infinite value
arises.

## Reading

The simulated state e becomes infinite for a large share of banks. How a
finite state can become infinite in the transition of stage2.step, or
through the replacement of failed banks by the mean of the live ones
(stage2.simulate: e[dead] = e[alive].mean()), has not been established.
The three candidates are: an infinite value created in the transition of
a live bank from finite inputs (overflow by compounding), an infinite or
NaN policy returned by choose, and the replacement mean being taken over
a set that contains a non-finite bank. No explanation is adopted.

## Trace (trace_stage2.py)

It reuses solved_large_41.npz and the code and seed of stage2.py, with no
change to the dynamics, and reports, for the large banks on the 41-point
grid:
- T1. Every 100 quarters: the number of banks with e not finite, the
  maximum finite e, the number with e above the top of the equity grid
  (0.4), the number with l above the top of the loan grid (2.5), the
  number replaced that quarter and the mean e and l used for the
  replacement.
- T2. The first quarter at which any bank has e or l not finite, checked
  at two points: after the replacement of failed banks and before the
  step, and after the step. For up to ten such banks: whether the bank
  was replaced that quarter; its state at the start of the quarter; the
  policy x, n, f and value that choose returns; and the pieces of the
  transition (eq, s, assets, w', pi, profit after tax, Gamma', the new e
  and l). The trace stops at that quarter.
- T3. The same statistics for the finite part of the distribution up to
  that quarter: the largest finite e and l reached before it.

## Rule

The trace reports and decides nothing. Whatever it shows, any change to
the simulation or to the moments gets its own dated pre-registration,
listing the alternatives before any run and chosen by whether simulated
banks stay finite and on their grids, not by how the moments compare
with their Table 5.
