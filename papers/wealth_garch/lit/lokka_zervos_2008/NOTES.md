# Notes: Løkka and Zervos (2008)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF. Controls: a fabricated quotation and a true quotation on the
  wrong page are both refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/LokkaZervos.lean`, which builds with no `sorry` and audits
  within propext, Classical.choice and Quot.sound.

## Not verified

- The verification theorem (Theorem 5.1) and the uniqueness of the root of
  (4.9) (Lemma 4.1). Read, not formalised.
- The published version.

## Discrepancies

- None in the text. LZ-D2 records the version.

## Controls

- `LokkaZervos.control_concave_needs_below_barrier`.
- `LokkaZervos.control_coeffs_need_distinct_roots`.

## Consequences for this bundle

- The baseline question in `TODO.md`. With proportional costs only, the
  model predicts minimal top-ups at zero, neither a lump nor a target. The
  FDIC data show lumps from far above any trigger (G15).
