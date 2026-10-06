# Mathlib-backed proof of the analytic step

`StepNonpos.lean` closes the one link that keeps every count claim from being
machine-checked from primitives to conclusion: the antitonicity of `Delta`,
assumed in `../EntryContest.lean` as the hypothesis `step_nonpos` and
previously discharged only at tier S over stated families (S4 on five concrete
pairs; S5 the boundary term).

Deliberately kept OUT of `../EntryContest.lean` and out of `verify.sh`. The
core development stays core-only so `#print axioms` can certify every theorem
at the granularity the paper claims; a Mathlib import brings `Classical.choice`
into the profile. This file is a separate obligation with a separate,
declared axiom profile.

## Status

DRAFTED, NOT COMPILED. Written in an environment where the Mathlib build cache
returns HTTP 403 (`cache.mathlib.org` is outside that sandbox's egress
allowlist) and building 8849 modules from source exceeded the disk available.
So every statement below is a conjecture about Mathlib's API, not a verified
proof.

## To run

    lake env lean lean/mathlib/StepNonpos.lean

then, for the axiom profile:

    #print axioms EntryContestAnalytic.step_integral_eq
    #print axioms EntryContestAnalytic.step_integral_nonpos
    #print axioms EntryContestAnalytic.step_nonpos_of_representation

## Where it is most likely to break

1. `integral_mul_deriv_eq_deriv_mul` — the integration-by-parts lemma. Name and
   namespace have moved across Mathlib versions; it may need
   `intervalIntegral.` explicitly, or the argument order may differ.
2. The `hv` block deriving `HasDerivAt (fun y => φ y ^ 2 / 2) (φ x * φ' x) x`.
   `(hφ x hx).pow 2` produces `2 * φ x ^ 1 * φ' x`; the `simpa` normalises that
   through `div_const 2`. If it fails, `field_simp` then `ring_nf`, or replace
   with `HasDerivAt.mul` on `φ * φ`.
3. `integral_nonneg` expects the pointwise bound on `Icc a b`; if the ambient
   measure or the `uIcc`/`Icc` mismatch bites, add `hab` rewriting first.
4. `mul_nonpos_of_nonneg_of_nonpos` — name varies; `mul_nonpos_iff` or
   `mul_nonpos` may be the current spelling.

## What it does and does not close

Closes: the integration by parts and the sign, for arbitrary `K` non-decreasing
and arbitrary `φ` vanishing at both endpoints. No family assumed.

Does not close: the algebraic identity reducing the step difference to
`∫ K φ dφ` (S1, already abstract in `F, G, C` but produced by a CAS whose
correctness is assumed), nor the P1 representation (S7). The differentiability
and integrability hypotheses above are smoothness assumptions on the model's
CDFs; they hold for the model's induced `F, G, C` but that implication is
itself not formalised here.

So a successful compile changes the abstract's claim from "no claim is
machine-checked end to end from primitives" to a narrower and true statement:
one analytic input remains, and it is an algebraic identity rather than an
analytic one.
