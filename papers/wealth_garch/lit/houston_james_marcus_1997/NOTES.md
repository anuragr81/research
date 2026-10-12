# Notes: Houston, James and Marcus (1997)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF's text layer (offset 134). Controls: a fabricated quotation, a
  true quotation on the wrong page, and a page before the first PDF page
  are all refused. HJM-Q1 and HJM-Q5 were also read on the page images of
  pp. 139 and 148.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/HoustonJamesMarcus.lean`, which builds with no `sorry` and
  audits within propext, Classical.choice and Quot.sound. The figures 3.83,
  nearly 13 percentage points and about 3% follow from the numbers stated.

## Not verified

- The tables (HJM-D2) and the Heckman fee model.

## Discrepancies

- HJM-D3: the holding-company sample starts in 1981 in one place and 1982
  in two others.

## Controls

- `HoustonJamesMarcus.control_sensitivity_rises_if_b_positive`.
- `HoustonJamesMarcus.control_zero_not_at_13_with_other_sign`.

## Consequences for this bundle

- With Holod and Peek, evidence that for a subsidiary of a multibank
  holding company the capital decision is made at the parent. A subsidiary
  held at its minimum while paying its excess up as dividends (HJM-Q1)
  would appear in E3 as dividends paid below the model's barrier.
