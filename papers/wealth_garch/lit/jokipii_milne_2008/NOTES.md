# Notes: Jokipii and Milne (2008)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF (offset 1439), with the text read in reading order because of
  the two-column layout. Controls: a fabricated quotation, a true quotation
  on the wrong page, and a page before the first PDF page are all refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/JokipiiMilne.lean`, which builds with no `sorry` and audits
  within propext, Classical.choice and Quot.sound. The averages of Table 1
  are reproduced; two ranges stated in the text are not (JM-D2, JM-D3).

## Not verified

- The GMM estimates.

## Discrepancies

- JM-D2 and JM-D3 (the stated ranges of Table 1), JM-D5 (sample size),
  JM-D6 (accession date), JM-D7 (hypothesis labels), JM-D8 (speed wording).
  None changes the direction of a result.

## Controls

- `JokipiiMilne.control_rate_is_not_lag`.
- `JokipiiMilne.control_eu15_average_not_303`.

## Consequences for this bundle

- JM-Q1 names the asymmetry our model builds on and records that it was
  not investigated. With Berger et al. (BDF-Q15), two empirical papers
  point to asymmetric capital adjustment without modelling it.
