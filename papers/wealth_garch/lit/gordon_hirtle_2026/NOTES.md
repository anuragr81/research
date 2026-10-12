# Notes: Gordon and Hirtle (2026)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated PDF page of
  the pinned file (offset 0), including three rows of Table 1. Controls: a
  fabricated quotation, a true quotation on the wrong page, and a page
  before the first PDF page are all refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/GordonHirtle.lean`, which builds with no `sorry` and audits
  within propext, Classical.choice and Quot.sound. The magnitudes stated on
  pp. 28-29 follow from Tables 1 to 3 (GH-D2 for the lower end of one
  range).

## Not verified

- The charts and every regression.

## Discrepancies

- GH-D2 to GH-D5. None changes a result relied on.

## Controls

- `GordonHirtle.control_ratio_needs_positive_earnings`.
- `GordonHirtle.control_lower_persistence_below_half`.

## Consequences for this bundle

- E3 is measured on banks; at holding companies, the unit that pays
  outside shareholders, dividends are paid in more than 85% of bank-years
  (GH-Q1). The class of baseline models pays only at the upper barrier.
- The capital-management channel (GH-Q3, GH-Q10) and sticky payouts under
  earnings shocks (GH-Q4, GH-Q18) are the authors' two readings of payouts
  above earnings; both are outside the baseline models.
- The authors name the interaction of payouts with internal capital
  targets and supervisory requirements as open (GH-Q9).
