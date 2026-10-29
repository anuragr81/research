# Notes: Peura and Jokivuolle (2003)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated printed page
  (offset -3). Lean in `lean/mathlib/PeuraJokivuolle.lean`.

## Not verified

- The 2004 journal version; Tables 8 to 13 beyond the figures quoted.

## Consequences for the enhancement paper

- The closest precedent for inverting a buffer model on observed capital
  ratios (PJ-Q2 to PJ-Q5): they back out a confidence level with the risk model
  fixed; we back out a risk scale with the decision rule fixed. They compare
  the implied quantity with no market measure and no realised loss. C6
  survives, with this paper as its counterpart.
- On the conversion of stress-test declines to a volatility (M4): a
  deterministic scenario has no likelihood (PJ-Q10), and credit losses are
  long-tailed, the 99th percentile about 3.5 SD out (PJ-Q11, PJ-Q12). With
  such tails a normal quantile overstates the volatility a decline implies, so
  the stress-test judged risk of E3 (about one times earnings volatility) is
  if anything an upper value.
