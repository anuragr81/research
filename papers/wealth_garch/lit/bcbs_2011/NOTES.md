# Notes: Basel Committee on Banking Supervision (2011)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated printed
  page of the pinned PDF (offset 8). Control: a fabricated quotation (a 3.0%
  CET1 minimum) and a true quotation on the wrong page are both refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/MeasurementMap.lean`, which builds with no `sorry` and
  audits within propext, Classical.choice and Quot.sound.

## Not verified

- Anything outside the pages listed in `RECONSTRUCTION.md`, including the
  December 2017 revisions.
- How risk-weighted assets are computed.

## Discrepancies

- B3-D1. BCVW names the Tier 1 ratio but calibrates to the CET1 minimum.

## Controls

- `MeasurementMap.control_solvency_needs_positive_exposure`. The ratio form
  of the solvency constraint and its cap form differ at zero exposure.
- `MeasurementMap.control_rwa_needs_zero_weight_on_riskless`. A positive
  risk weight on the riskless asset breaks risk-weighted assets $=\pi X$.
- `MeasurementMap.control_leverage_needs_l0_lt_one`. The leverage
  rearrangement needs a floor below one.

## Consequences for this bundle

- `MEASUREMENT_MAP.tex`, rows G3, G4, G7 and G14.
