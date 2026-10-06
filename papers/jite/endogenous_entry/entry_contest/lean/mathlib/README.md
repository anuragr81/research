# Lean with Mathlib, for the analytic steps

This lake project holds the parts of the proofs that need measure theory,
which `../EntryContest.lean` keeps out by design. The core file stays on core
Lean, so its audit admits only `propext` and `Quot.sound`. Files here import
Mathlib, so their audit admits the three standard axioms `propext`,
`Classical.choice` and `Quot.sound` and nothing else (`checks/verify_mathlib.py`,
suite 12 of `verify.sh`). The project also builds `../EntryContest.lean` as a
library, so the theorems here can apply its results directly.

These files are for our verification and are not part of the submission.

## Setup

The project pins Lean `v4.24.0-rc1` (`lean-toolchain`) and Mathlib at commit
`14871d5175e8011a94c78e54b431080aabeeab00` (`lakefile.toml`, with every
dependency fixed in `lake-manifest.json`). From this directory run

    lake exe cache get
    lake build

`cache get` downloads Mathlib's prebuilt files, about 5 GB unpacked, or unpacks
them from `~/.cache/mathlib` when they are already there. Without it, `lake
build` would compile Mathlib from source. `.lake/` is not committed.

## StepIdentity.lean, the step identity in full generality

For probability measures `α`, `β`, `κ` on the reals with no atoms, with
CDFs `F`, `G`, `K`,

    (∫ K F dα − ∫ K F dβ) − (∫ K G dα − ∫ K G dβ) = −½ ∫ (F − G)² dκ.

No densities and no integration by parts are used. Each integral of a product
of two CDFs against a third measure is the probability that one coordinate of
an independent triple is the largest (`EX_eq`, `EY_eq`, `EZ_eq`). With no
atoms, ties have probability zero (`tie_xy_null`, `tie_xz_null`,
`tie_yz_null`), so the three probabilities sum to one (`three_max_sum`). The
identity follows from that sum for the triples `(κ, α, α)`, `(κ, β, β)` and
`(κ, α, β)` (`step_identity`), and its sign from the square (`step_nonpos`).

## EntryContestModel.lean, from the score laws to P5

| Theorem or definition | Content |
|---|---|
| `maxLaw`, `cdf_maxLaw`, `maxLaw_noAtoms` | The law of the larger of two independent scores has CDF equal to the product of the CDFs, and no atoms when neither factor has any |
| `rivals`, `cdf_rivals` | The law of the best rival score, with CDF `C · F^m · G^(Q−1−m)`, which is `H_m` in `PROOFS.tex` |
| `Delta` | `Δ(m) = V (∫ H_m dF − ∫ H_m dG)`, built from the score laws |
| `Delta_step` | `Δ(m+1) − Δ(m) = −(V/2) ∫ (F − G)² dK` with `K = C · F^m · G^(Q−2−m)`, for `m + 2 ≤ Q` |
| `Delta_step_nonpos`, `DeltaUpTo_step` | `Δ` does not increase in `m` (extended as a constant beyond `Q − 1`) |
| `p5_threshold_from_primitives` | `EntryContest.equilibrium_is_threshold` applied to this `Δ`, with its hypothesis `step_nonpos` discharged rather than assumed |

The only assumptions are the model's own. The score laws `α` (investor, `F`),
`β` (non-investor, `G`) and the incumbent's law `C` are probability measures
with no atoms, which is the continuity of `F` and `G` stated in the primitives,
and `V ≥ 0`.

## What is still assumed

- That the score laws have no atoms. The primitives assume it for `F` and `G`;
  deriving it from the laws of `r` and `s` is not attempted.
- The P1 representation `Δ(m) = V E[φ(M_m)]` is not yet in Lean (pass 3b in
  `TODO.md`). The chain above does not use it.

`StepNonpos.lean`, compiled in pass 2, assumed densities. It was removed on
6 October 2026 because `StepIdentity.lean` proves the same sign without them.
