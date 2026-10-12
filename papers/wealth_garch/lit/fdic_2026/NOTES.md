# Notes: FDIC (2026)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim in the entry of its
  stated variable in the pinned workbook. Controls: a fabricated
  definition, a true quotation under the wrong variable, and a variable
  absent from the sheet are all refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/MeasurementMap.lean`.

## Not verified

- The unit (FDIC-D1), read from one API record, not from the workbook.
- Item-number mappings to the Call Report.

## Discrepancies

- FDIC-D2 and FDIC-D3.

## Controls

- `MeasurementMap.control_rwa_needs_zero_weight_on_riskless`.
- `MeasurementMap.control_leverage_needs_l0_lt_one`.

## Consequences for this bundle

- `MEASUREMENT_MAP.tex`, rows G1 to G4, G14 and G15, and the results
  table.
