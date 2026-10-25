# Notes: Orozco and Rubio (2025)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated printed
  page of the pinned PDF (offset 1420), line-break hyphens as printed.
  Controls: a fabricated quotation, a true quotation on the wrong page and
  a page before the first are refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/OrozcoRubio.lean`.

## Not verified

- The Internet Appendix (public versus private banks, other integers,
  incentive decomposition), cited by them and not read.
- Their regressions, read for orientation only.

## Consequences for this bundle

- That banks guard the well-capitalised line is their finding (OR-Q1 to
  OR-Q3); the enhancement paper cannot claim it. What it can claim, if the
  novelty check holds, is the model result: the dynamic barrier model with
  that line as its floor matches the level of buffers out of sample.
- Their design is local to the line (OR-Q10, OR-Q11), so the level of
  buffers far above it, F2, is not covered by their result.
- The floor of our enhanced model now has an independent, verified source
  for 1996-2009 (10%), and for the period after 2009 the source points to
  10.5% (OR-Q5, OR-Q6), which is R. The pre-registered central floor of
  PREREG_comparison_17.md was 10% in both windows; on the test window the
  source favours R. results_book_vol.md reports the minimum at R as well
  (all 61 banks: F1 1.0%, F2 70.7%, miss 12.9 against 7.8 at 10%).
- They read the weak effect of regulation in Gropp and Heider (2010) as
  measuring the wrong line (OR-Q3).
- Bunching holds for public and private banks alike (OR-Q8), in line with
  results_access.md, where listing did not raise buffers.
