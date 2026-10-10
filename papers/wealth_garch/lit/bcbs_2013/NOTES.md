# Notes: Basel Committee on Banking Supervision (2013)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated printed
  page of the pinned PDF (offset 6). Control: a fabricated quotation (a
  120% floor) and a true quotation on the wrong page are both refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/MeasurementMap.lean`, which builds with no `sorry` and
  audits within propext, Classical.choice and Quot.sound.

## Not verified

- Anything outside the pages listed in `RECONSTRUCTION.md`.

## Discrepancies

- LCR-D2. BCVW's baseline haircut 0.30 is not a haircut of the standard.

## Controls

- `MeasurementMap.control_lcr_needs_positive_runoff`. The ratio form of the
  liquidity constraint and its cap form differ at zero run-off.

## Consequences for this bundle

- `MEASUREMENT_MAP.tex`, rows G5 and G6.
