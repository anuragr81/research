# Notes: Peura and Keppo (2006)

## Verified

- Every quotation in `CLAIMS.md` was read on the page image and is found
  verbatim in the OCR text of its printed page. Controls: a fabricated
  quotation, a true quotation on the wrong page, and a page outside the
  page map are all refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/PeuraKeppo.lean`, which builds with no `sorry` and audits
  within propext, Classical.choice and Quot.sound. Our reading of the
  closed forms (A2), (A4), (A5) and (17) is consistent with the source.

## Not verified

- The function $M$ of (14), Lemmas 1 to 5 and the existence of the
  solution of (18i)-(18ii). Read, not formalised.
- Every numerical result (Figures 2 to 7, Tables 1 to 3).

## Discrepancies

- PK-D4: the page range differs from BCVW's citation by one page.

## Controls

- `PeuraKeppo.control_dplus_needs_positive_rho`.
- `PeuraKeppo.control_paste_needs_distinct_roots`.

## Consequences for this bundle

- The baseline question in `TODO.md`. Peura and Keppo is a published bank
  capital model that was itself calibrated to US bank data and judged by
  its fit to capital ratios, with sigma defined on accounting returns
  (PK-D3), which removes the book-versus-economic problem of G8 for this
  baseline.
- Exact targeting (G15) is a prediction of this baseline too (PK-D2), and
  so is the underprediction of capital (PK-Q12).
- PK-Q18 calls for a graduated penalty below the requirement, which is the
  role of the shortfall penalty with $\lambda_S$ in our model.
