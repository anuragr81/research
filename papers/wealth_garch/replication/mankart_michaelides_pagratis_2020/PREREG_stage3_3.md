# Mankart, Michaelides and Pagratis, stage 3, third file: stage 3 deferred

Written 18 October 2026, while the check of PREREG_stage3_2.md is running
and before its result. The author decided today to defer stage 3 (our own
estimation by the method of simulated moments).

## Reasons

- The testing thread needs each paper's answer to the common question
  (how banks manage their capital buffer) on the common data, not a rerun
  of each paper's estimation. For MMP that answer is the solved policy at
  the published small-bank parameters, which stage 2 provides.
- Stage 2 already replicates the model's reported output at the published
  parameters (small banks: 6 of 11 moments; large banks: the unbounded
  equity finding of PREREG_stage2_7.md). Stage 3 would test whether their
  Table 4 minimises their criterion, a further check that the testing
  thread does not need.
- A solve at parameters other than the published ones took up to 194
  sweeps and 2.2 hours with the faster solver (results_validation_fast.md),
  so an estimation would take one to four weeks on the machine used.

## Consequences

- The replication of MMP ends at stage 2 for now; re-estimation is
  optional and would need its own pre-registration and, probably, more
  computing power.
- The check of PREREG_stage3_2.md completes and its results are recorded,
  but no decision depends on them while stage 3 is deferred. If the
  testing thread solves MMP at other parameters, which solver it uses is
  decided in that thread's pre-registration, with those results in view.
