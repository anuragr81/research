# Notes: Black and Perold (1992)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF (printed page = PDF page + 402). Controls: a fabricated
  quotation, a true quotation on the wrong page and a page before the first
  PDF page are refused.
- The Lean results are declared in `lean/mathlib/BlackPerold.lean`, which
  builds with no `sorry` and audits within propext, Classical.choice and
  Quot.sound.

## Not verified

- Prop. 11's verification (A.7), whose constants are garbled in the text
  layer. Its content, that the slopes are set by preferences, is quoted
  (BP-Q8 to BP-Q10), not formalised.
- Props. 4-10, read and not formalised; the theory paper uses none of them.

## Discrepancies

- None in the paper. BP-D2 records OCR symbol losses.

## Controls

- `BlackPerold.control_ratio_needs_multiplier_branch`.
- `BlackPerold.control_limit_needs_fixed_floor`.

## Consequences for this bundle

- The regulatory cap with $a_2=0$ is capped CPPI. The constant buffer
  variance on the solvency branch is Prop. 2's constant log-cushion
  volatility, in print since 1992.
- In Black and Perold the two slopes come from preferences, so the two
  volatility levels carry preference information. The theory paper's
  partition (its K2) rests on the slopes being regulatory and the liability
  unhedged, with this paper as the contrast.
