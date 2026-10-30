# Notes: Milne and Whalley (2001)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated printed
  page of the pinned PDF (offset -1). Control: a fabricated quotation and
  a true quotation on the wrong page are both refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/MilneWhalley.lean`, which builds with no `sorry` and
  audits within propext, Classical.choice and Quot.sound.

## Not verified

- Their figures, which are read for orientation only.

## Added 24 October 2026, for the theory paper's Section 3.1

- MW-Q10 to MW-Q12 (pp. 9, 23). The capital decision as an (s, S) problem,
  the buffer as a joint function of audit frequency and recapitalisation
  costs, and Result 12, by which both frictions move desired capital and the
  liquidation-or-recapitalisation boundary in the same direction. The theory
  paper uses them to motivate its question; the same-direction response is
  the case the control of M3 shows can leave the Jacobian singular.
