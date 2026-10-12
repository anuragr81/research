# Notes: Mankart, Michaelides and Pagratis (2020)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF (offset -2), including rows of Tables 4 and 5. Controls: a
  fabricated quotation, a true quotation on the wrong page, and a page
  before the first PDF page are all refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/MankartMichaelidesPagratis.lean`, which builds with no
  `sorry` and audits within propext, Classical.choice and Quot.sound. The
  minimum equity of Figure 4, the limit-to-ratio mapping and the smoothing
  gain of a strictly concave objective are proved.

## Not verified

- The numerical solution, the estimates and every simulated figure.
- The published version.

## Discrepancies

- MMP-D3 (a percentile in one table note), MMP-D4 (equation (15) and an
  unresolved reference), MMP-D5 (a page range in the references). None
  changes a result relied on.

## Controls

- `MankartMichaelidesPagratis.control_min_equity_needs_limit_above_one`.
- `MankartMichaelidesPagratis.control_linear_objective_not_strictly_concave`.

## Consequences for this bundle

- The source is not yet in the Drive folder; the author is asked to
  upload it.
- Concave dividend utility is a second, estimated mechanism for dividends
  paid below any barrier (E3). Our baseline class has linear dividend
  value, so it pays only at the barrier. Candidate for the enhancement
  discussion.
