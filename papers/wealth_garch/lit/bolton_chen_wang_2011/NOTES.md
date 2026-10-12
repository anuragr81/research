# Notes: Bolton, Chen and Wang (2011)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF (offset -2). Controls: a fabricated quotation, a true
  quotation on the wrong page, and a page before the first PDF page are all
  refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/BoltonChenWang.lean`, which builds with no `sorry` and
  audits within propext, Classical.choice and Quot.sound. The benchmark
  values $i^{FB}=15.1\%$, $q^{FB}=1.23$ are confirmed.

## Not verified

- The ODE solution and every numerical figure.
- Appendices B and D, and the published version.

## Discrepancies

- None in the text. BCW-D2 and BCW-D3 record the version.

## Controls

- `BoltonChenWang.control_sensitivity_needs_concavity`.
- `BoltonChenWang.control_ifb_needs_root_condition`.

## Consequences for this bundle

- BCW-Q1 is a statement, by the authors, that the double-barrier
  structure is established. It supports calling the class of models
  standard, not any single paper.
- Exact targeting (BCW-D1) is a prediction of this model too.
