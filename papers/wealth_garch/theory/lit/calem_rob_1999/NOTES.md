# Notes: Calem and Rob (1996)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated printed page
  of the pinned PDF (offset -1). Control: a fabricated quotation and a true
  quotation on the wrong page are both refused.
- The Lean results are declared in `theory/lean/CalemRob.lean`, which builds
  with no `sorry` and audits within propext, Classical.choice and Quot.sound.

## Not verified

- The published version, Journal of Financial Intermediation 8(4) (1999),
  titled "The Impact of Capital-Based Regulation on Bank Risk-Taking", was not
  read. The record is of the FEDS working paper, the version Milne and Whalley
  (2001) cite; the manuscript cites the published version and states the
  version read.
- Their numerical solutions, which are read as printed.

## Added 30 October 2026, for the theory paper's Sections 3 and 4

- The bank cannot issue (CR-D1), so the risk-seeking near insolvency (CR-Q3)
  and the effect of the premium surcharge (CR-Q5) work through recapitalisation
  from earnings. The theory paper's bank issues at the trigger, which is the
  contrast its rows draw (M2, M4).
