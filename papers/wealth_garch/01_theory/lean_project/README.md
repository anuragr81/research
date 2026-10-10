# Lean formalization — asymmetric capital control

Pinned to Lean v4.32.0-rc1 and mathlib v4.32.0-rc1.

## Build

    cd 01_theory/lean_project
    lake exe cache get     # download prebuilt mathlib (minutes, not hours)
    lake build

## What compiles to what

- `AsymCapital/SmoothFit.lean` — Proposition SMF. Step 1 (`kink_is_convex`)
  and Step 2 (`no_convex_kink`, the novel crux) compile with no `sorry`;
  `smooth_fit` composes them. Compiler-verified: `#print axioms
  SmoothFit.smooth_fit` reports only `[propext, Classical.choice,
  Quot.sound]`.
- `AsymCapital/QVI_Part1.lean` — exact nesting at λ=1, concavity of −Λ.
  Compiler-verified.
- `AsymCapital/RateBased.lean` — persistence/saturation real-analysis
  content; carries one `sorry` at the tanh leaf (`saturated_ratio_tendsto`),
  on which no cited result depends. Compiler-verified apart from that leaf.
- `AsymCapital/Envelope.lean` — the algebraic cores of ENV, ENVK, and SMFN.
  Five declarations: `smf_permits` (rem:smfn), `wode_of_envelope`
  (lem:envelope, eq:Wode), `node_of_envelope` + `neumann_at_ystar` +
  `trigger_jump` (lem:envelopeK, eq:Node and its two boundary conditions).
  Imports `AsymCapital.SmoothFit` for `TriggerData`/`OperatorLe`.
  Compiler-verified: all five report axioms
  `[propext, Classical.choice, Quot.sound]`. Scope: each takes the paper's
  analytic inputs (the Danskin/envelope identity, FOC, smooth fit, value
  matching) as hypotheses and machine-checks what follows; the registry
  records these as `lean_partial` with the exact boundary per entry in
  `LEAN_PARTIAL_SCOPE` (00_reader/proof_registry.py).

- `AsymCapital/ImpulseCount.lean` — the summability core of FIN (lem:fin):
  `partial_sums_bounded` (the K-division step), `count_summable` + `count_le`
  (bounded partial sums of the nonnegative discount sequence make the impulse
  count N summable, with N <= B/K), and `impulse_sum_summable` (the comparison
  test giving absolute convergence of the impulse sum VER cites). The cost
  bound from "each impulse costs at least K" plus finite value enters as a
  hypothesis, so lem:fin is `lean_partial` in the registry. Compiler-verified:
  all four report `[propext, Classical.choice, Quot.sound]`.

## Status

QVI_Part1, RateBased, SmoothFit, Envelope, and ImpulseCount are
compiler-verified against Lean v4.32.0-rc1 + mathlib v4.32.0-rc1. The one
remaining `sorry` is the tanh leaf in RateBased.lean,
on which no cited result depends. The reproduction harness does not run
Lean — rebuild with the commands above.
