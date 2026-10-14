# Pre-registration: Berger et al. replication, stage 1, sensitivity

Written and committed 14 October 2026, before any of the runs below. It
adds a sensitivity analysis to PREREG_stage1.md (commit 4906480e); the
stage 1 verdicts in results_stage1.md stand and are not replaced.

## What was seen before this file

results_stage1.md (run 2) and its run 1: Step 1 speeds 0.15-0.19 against
their 0.49-0.60; 5 of 18 targets hold; means of STDROA 0.0007, NMERGER
0.454, RETAILDEP 0.300, BUSINESSLOAN 0.501 against 0.0034, 0.066, 0.605,
0.281; on all bank holding company filers, business loans are 0.488 of
total loans and 0.319 of total assets.

## Question

Is the gap in adjustment speeds due to our choices (instruments, variable
definitions) or to the data?

## Grid, fixed here: four instrument sets times two data versions

Instrument sets, for Steps 1 and 3:
- I1: DNK GMM-style lags 2 to 3 (stage 1).
- I2: DNK lags 2 to 4.
- I3: DNK lags 2 and longer, collapsed (one instrument per lag, not per
  period; Roodman 2008, section 4).
- I4: difference GMM (no levels equation), DNK lags 2 to 3.

Data versions:
- D1: stage 1 variables.
- D2: three definitions changed together.
  - BUSINESSLOAN over total assets `BHCK2170` (their text, p. 17).
  - NMERGER counts only transformations whose predecessor is a bank:
    NIC attributes entity type (active or closed file, pinned in
    `replication/gordon_hirtle_2026/`) NAT, SMB, NMB, SSB, SAL, FSB or CPB.
  - RETAILDEP adds nontransaction savings deposits (`BHCB2389` +
    `BHOD2389`), whose exclusion is the likeliest reason our mean is half
    of theirs.

All eight runs use the stage 1 sample, trimming, Step 2 and target rules,
and report T1 to T6 with the stage 1 rules, the Step 1 speed, the Step 2
mean speed and the Step 3 mean target.

## Reading, fixed here

- The speed gap is attributed to our choices if the Step 1 speed is within
  0.10 of theirs, for at least two of the three measures, in at least four
  of the eight runs; to the data otherwise.
- No run replaces stage 1 as the replication's result.

## Code

gmm.py gains a collapse option. Before any Berger run it must reproduce
pydynpd's collapsed system GMM on the abdata benchmark of
results_validation.md to 7 decimals; the check is added to
validate_gmm.py.

## Not varied

STDROA annualised: multiplying a regressor by four divides its coefficient
by four and leaves speeds, targets and signs unchanged. Its annualised
mean is reported for comparison with theirs.
