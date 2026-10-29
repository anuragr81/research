# Notes: Corbae and D'Erasmo (2021)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated printed page
  (offset -2). Lean in `lean/mathlib/CorbaeDErasmo.lean`.

## Not verified

- The Econometrica version; the online appendix tables beyond those read.

## Consequences for the enhancement paper

- A dynamic model of US banks fitted to buffer levels by size exists
  (CD-Q2, CD-Q3, CD-Q5). The enhancement paper is not the first to take
  dynamic theory to buffer data; its difference is that no parameter is
  fitted to the facts it predicts, the models of different authors are
  scored on common pre-registered facts, and the tests are out of sample.
- Even fitted, their model reproduces about a fifth of the size gap and
  misses the far-above tail (CD-Q4, CD-Q9).
- Their size channels are funding volatility and issuance costs, assumed and
  calibrated, not tested (CD-Q1, CD-Q6, CD-Q7). results_access.md tests
  access by affiliation and listing, not issuance costs directly; funding
  volatility is untested in the bundle.
