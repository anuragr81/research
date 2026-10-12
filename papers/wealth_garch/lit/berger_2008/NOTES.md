# Notes: Berger, DeYoung, Flannery, Lee and Öztekin (2008)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF (offset -2), including the Table 1 means and the fitted
  targets of Table 3. Controls: a fabricated quotation, a true quotation on
  the wrong page, and a page before the first PDF page are all refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/BergerEtAl.lean`, which builds with no `sorry` and audits
  within propext, Classical.choice and Quot.sound. The partial adjustment
  algebra, every row of Table 1, footnotes 28 and 29 and the Step 3 less
  Step 1 differences are checked.

## Not verified

- The GMM estimates and every coefficient beyond those used above.
- The published version.

## Discrepancies

- BDF-D2 (Step 1 speeds stated as 50%-61%, tabulated 49%-60%), BDF-D3
  (rounding in Table 1), BDF-D4 (two dummy definitions interchanged in
  Table 2).

## Controls

- `BergerEtAl.control_overshoot_without_speed_bound`.
- `BergerEtAl.control_step1_lower_below_half`.

## Consequences for this bundle

- The sample is profitable and 94-98% well capitalized (BDF-Q10); the loss
  side, where our model's lower barrier acts, is outside it (BDF-Q14).
- Speeds rise as capital falls toward the thresholds (BDF-Q8): an
  asymmetry of the kind M12 measures, estimated here as a step function of
  the distance to the threshold.
- The do-nothing ratio holds dividends at last year's level: sticky
  dividends are built into the benchmark (BDF-Q2, BDF-Q5).
