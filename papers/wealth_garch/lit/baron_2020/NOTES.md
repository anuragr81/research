# Notes: Baron (2020)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated PDF page of
  the pinned file (offset 0), including a row of Table 4. Controls: a
  fabricated quotation, a true quotation on the wrong page, and a page
  before the first PDF page are all refused. Table 4 was also read on the
  page image.
- The Lean results in `CLAIMS.md` are declared in `lean/mathlib/Baron.lean`,
  which builds with no `sorry` and audits within propext,
  Classical.choice and Quot.sound. The step from (A8) to (A9) and its
  stated comparative statics in δ and V are proved.

## Not verified

- The model's calibrated outcomes (Figures 1-2) and every estimate.

## Discrepancies

- BAR-D2 ("monotonically increasing" against Table 4), BAR-D3 (the N
  column), BAR-D4 (Table 1's caption). None changes a result relied on.

## Controls

- `Baron.control_zero_denominator_leaves_a8_unsolved`.
- `Baron.control_landesbank_coefficients_not_swapped`.

## Consequences for this bundle

- Recapitalisation to the minimum at a proportional cost is an established
  modelling choice in the empirical literature (BAR-Q2, BAR-Q3), beside the
  fixed-cost impulse to a target of Peura and Keppo; E2 fails exact
  targeting, and BAR-Q2 is the other limiting case to test.
- Issuance at smaller banks is frequent and large (BAR-Q5 to BAR-Q8),
  which bears on any recapitalisation proxy built on bank-level issuance.
