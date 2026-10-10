# Notes: FFIEC (2026)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated PDF page
  of the pinned PDF. Control: a fabricated quotation (a leverage ratio over
  item 27) and a true quotation on the wrong page are both refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/MeasurementMap.lean`, which builds with no `sorry` and
  audits within propext, Classical.choice and Quot.sound.

## Not verified

- Anything outside the pages listed in `RECONSTRUCTION.md`.
- The FDIC field names of the empirical scripts.

## Discrepancies

- None in the source. RCR-D1 to RCR-D3 record limits of observation.

## Controls

- `MeasurementMap.control_rwa_needs_zero_weight_on_riskless`.
- `MeasurementMap.control_leverage_needs_l0_lt_one`.

## Consequences for this bundle

- `MEASUREMENT_MAP.tex`, rows G1 to G4 and G7.
