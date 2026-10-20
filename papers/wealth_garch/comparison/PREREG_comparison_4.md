# Pre-registration: the testing thread's comparison, fourth file

Written and committed 20 October 2026, before any computation below. A
training-window check of whether a reward for capital, the counterpart of
the insurer's and the uninsured creditors' pricing (ledger actions A11 and
A7), can produce the buffers far above the reference that the shortfall
penalty could not (results_bound.md). Nothing from 2015-2024 enters.

## What was seen before this file

results_facts.md, results_pk.md, results_mmp.md, results_bound.md (on
2005-2014, sample B: observed F2 48.0%, the baseline 0.0%, the baseline
with its minimum at R 0.2%), and the ledger annotations R1 to R10
(00_reader/ledger_model.py). The FDIC assessment rule
(lit/fdic_assess_2024): a small bank's rate is linear in seven ratios with
a negative multiplier on the leverage ratio (ASMT-D1), down to a floor rate
for its rating (ASMT-Q5).

## The reward

- Size: the multiplier on the leverage ratio is -1.264 basis points of the
  assessment base per percentage point (ASMT-D1, the schedule for a reserve
  ratio below 2%). Taking the assessment base as total assets, one more
  unit of capital lowers the yearly assessment by c_ins = 1.264e-4 x 100 =
  0.01264 per unit of capital, whatever the bank's size. This is recorded
  as reading ASMT-D2.
- Form: in the baseline's buffer dynamics the drift becomes mu_i + c (x -
  xbar_i), where x is the buffer over the model's minimum and xbar_i the
  bank's mean buffer over the same minimum in its December reports
  2005-2014, so that the measured mu_i is the drift at the bank's own mean
  capital and only deviations from it are rewarded or charged.
- No cap: the floor rate of ASMT-Q5 is ignored, which makes the reward as
  large as the rule allows and the check an upper bound for this channel.
- Grid, fixed now: c in {0, c_ins, 2 c_ins, 3 c_ins}. The central point is
  c_ins, the insurer's channel alone; 2 c_ins and 3 c_ins allow the
  uninsured creditors' channel (A7), which no source read quantifies, to
  be as large as the insurer's or twice as large. All points stay below
  rho = 4%.

## The check (reward_check.py)

- Banks, sigma_i, mu_i, the reference R = 10.5%, the facts and their rules
  as in results_bound.md (training window 2005-2014, starts at 2004Q4).
- The dividend barrier: with dividends at a barrier b and failure at zero,
  the value solves (1/2) sigma^2 V'' + (mu_i + c (x - xbar_i)) V' - rho V =
  0 with V(0) = 0, and b is the inflection point of the solution, V''(b) =
  0 (smooth fit); rho = 4%. Solved numerically from 0 up to x = 2. At c = 0
  it must equal the closed form u0 of stage1_barriers.u0 to 1e-6 for every
  bank, or the run stops there and that is reported.
- Simplification: no issuance; every bank pays dividends at b and fails at
  zero, so F3 and F4 are not scored here. The check is about F2 (and F1).
- Two minima, as in results_bound.md: 8% (the reward alone) and R (the
  reward with the penalty proxy). Simulation as predict_pk.py (Euler steps
  of 1/360 year, 200 paths per bank, seed 20261020), 40 quarters from the
  2004Q4 buffer. Banks starting at or below the minimum start failed and
  are counted.

## Rule

- If at c_ins with the minimum at R the predicted F2 in sample B is within
  10 points of the observed training-window F2, the full enhanced model
  (penalty and reward, with issuance) is built next.
- If that holds only at 2 c_ins or 3 c_ins, it is reported and the author
  decides, since the size of the creditors' channel is not measured.
- If it fails at every grid point, the reward channel (linear, uncapped, up
  to three times the insurer's) cannot close F2, and that is reported.
- F1 is reported for every configuration and decides nothing; sample A is
  reported and decides nothing.
