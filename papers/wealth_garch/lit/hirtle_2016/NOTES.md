# Notes: Hirtle (2016)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF (offset -2), including rows of Tables 1 to 3. The text uses
  the hyphen U+2010, which the quotations reproduce. Controls: a
  fabricated quotation, a true quotation on the wrong page, and a page
  before the first PDF page are all refused.
- The Lean results in `CLAIMS.md` are declared in `lean/mathlib/Hirtle.lean`,
  which builds with no `sorry` and audits within propext, Classical.choice
  and Quot.sound. Every percentage in Tables 2 and 3 matches its counts.

## Not verified

- The Cox and Heckman estimates and the figures.

## Discrepancies

- H-D2 to H-D5: citation years and names. H-D5 leaves open which title and
  first name Kanas (2013) has.

## Controls

- `Hirtle.control_rounding_rejects_wrong_percentage`.
- `Hirtle.control_rounding_rejects_wrong_count`.

## Consequences for this bundle

- With Gordon and Hirtle, holding-company evidence for E3: dividends paid
  through losses and cut late.
- A signalling cost of cutting dividends is a candidate mechanism for the
  enhancement discussion, beside concave dividend value (Mankart,
  Michaelides and Pagratis).
