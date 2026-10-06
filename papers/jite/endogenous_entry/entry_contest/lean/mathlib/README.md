# Lean with Mathlib, for the analytic steps

This lake project holds the parts of the proofs that need real analysis, which
`../EntryContest.lean` keeps out by design. The core file stays on core Lean, so
its audit admits only `propext` and `Quot.sound`. Files here import Mathlib, so
their audit admits the three standard axioms `propext`, `Classical.choice` and
`Quot.sound` and nothing else (`checks/verify_mathlib.py`, suite 12 of
`verify.sh`).

## Setup

The project pins Lean `v4.24.0-rc1` (`lean-toolchain`) and Mathlib at commit
`14871d5175e8011a94c78e54b431080aabeeab00` (`lakefile.toml`, with every
dependency fixed in `lake-manifest.json`). From this directory run

    lake exe cache get
    lake build

`cache get` downloads Mathlib's prebuilt files, about 5 GB unpacked, or unpacks
them from `~/.cache/mathlib` when they are already there. Without it, `lake
build` would compile Mathlib from source. `.lake/` is not committed.

## StepNonpos.lean

Compiled 6 October 2026, first draft written 2 September (item A2 in
`TODO.md`). It proves the analytic step that `EntryContest.lean` takes as the
hypothesis `step_nonpos`, the antitonicity of `Delta` that drives P5.

| Theorem | What it proves |
|---|---|
| `step_integral_eq` | Integration by parts against `d(φ²/2)`, with the boundary term zero because `φ a = φ b = 0`, gives `∫ K φ φ' = -∫ K' φ²/2` |
| `step_integral_nonpos` | With `K' ≥ 0` on `[a, b]`, the integral `∫ K φ φ'` is at most zero |
| `step_nonpos_of_representation` | If `Δ(m+1) - Δ(m) = V ∫ K_m φ φ'` for every `m` with `V ≥ 0`, then `Δ(m+1) ≤ Δ(m)` for every `m` |

No distribution family is assumed. `K` is any differentiable function with
`K' ≥ 0`, which a product of CDFs with densities satisfies, and `φ` is any
differentiable function vanishing at both ends.

Axioms, from the suite on 6 October 2026. Each of the three theorems depends
on `propext`, `Classical.choice` and `Quot.sound`, and on nothing else.

## What this closes and what it does not

It closes the integration by parts and the sign of the step, which had been
checked only at tier S, on five concrete pairs (S4) and for the boundary term
(S5).

It does not close three things, which are the next clusters of pass 3 in
`TODO.md` item PLAN.

- The algebraic identity that reduces the step difference to `∫ K φ dφ`
  (SymPy S1), here the hypothesis `hrep`.
- The P1 representation (SymPy S7).
- That the smoothness hypotheses hold for the model's induced `F`, `G` and
  `C`, which needs the score distributions to have densities.

So the claim that no result is machine-checked from primitives still stands,
with fewer analytic inputs left than before.
