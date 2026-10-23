# Notes: Dai, Huang and Keppo (2019)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated PDF page of
  the pinned thesis. Controls: a fabricated quotation, a true quotation on the
  wrong page and a page before the first are refused.
- The Lean results are declared in `lean/mathlib/DaiHuangKeppo.lean`, which
  builds with no `sorry` and audits within propext, Classical.choice and
  Quot.sound.

## Not verified

- The solution of the HJB equations and the numerics behind Table 2.1. The
  table's values are used as printed.
- The published version.

## Discrepancies

- DHK-D3 and DHK-D4, inconsistencies in the source. DHK-D2, the averaging
  grid.

## Controls

- `DaiHuangKeppo.control_proportional_columns_singular`.

## Consequences for this bundle

- A bank model with an optimally chosen recapitalisation barrier, a fixed
  issue cost and a delay. Table 2.1 tabulates the responses of both barriers
  in seven parameters, and the fixed cost moves them with the theory paper's
  signs. The paper states no identification, has no shareholder asymmetry
  parameter, and calibrates by fitting equity values. The theory paper's
  identification headline cites it as a numerical precedent.
