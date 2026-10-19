# Notes: United States Code (2023), 12 U.S.C. 1831O, Prompt Corrective Action

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated printed
  page of the pinned PDF (offset 1160), read in column order.
  Control: a fabricated quotation and a true quotation on the wrong page
  are both refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/MeasurementMap.lean`, which builds with no `sorry` and
  audits within propext, Classical.choice and Quot.sound.

## Not verified

- Anything outside the pages named in `CLAIMS.md`.
