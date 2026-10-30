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

## Added 30 October 2026, for the theory paper's Section 3.4

- MW-Q13, MW-Q14 (p. 18) and MW-D3. Results 5 and 6, their control of
  variance. The theory paper's exposure rule under the cap term reduces to
  their bang-bang rule when the risk premium is zero, and M4's convexity
  above the trigger is their risk-loving region with the cap in place of
  their assumed upper volatility. The theory paper had cited them for the
  fixed cost and the random audit only (L16, L37 to L40).
