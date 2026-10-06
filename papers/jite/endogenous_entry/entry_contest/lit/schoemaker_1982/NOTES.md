# Schoemaker (1982), verification notes

## Source

Published article, *Journal of Economic Literature* 20(2), pp. 529-563, read
in full on 6 Oct 2026, references included (Drive folder
`1r7J3xJ2rotpmlRaBANCNhviy2d8a1bx_`). The extracted text drops some symbols
(equals signs, summation signs, superscripts), so formulas are described
rather than quoted, and the one quotation with a restored "=" says so in
`CLAIMS.md`. Odd pages carry their number between the two columns in the
extraction, which is how page locators were assigned.

## Lean (`Schoemaker1982.lean`), theorem by theorem

| Theorem | Claim | What it proves |
|---|---|---|
| `eu_affine` | SCH-1 | Expected utility under `aU + b` equals `a·EU + b·(total weight)`, for integer weights |
| `affine_preserves_ranking` | SCH-1 | With `a > 0` and equal total weight, the ranking of two lotteries is unchanged |
| `affine_preserves_difference_order` | SCH-4 | With `a > 0`, the order of two utility differences is unchanged |
| `sure_outcomes_ordinal` | SCH-5 | Under certainty any strictly increasing map preserves the ranking |
| `control_monotone_nonaffine_reverses_ranking` | SCH-2, control | A map increasing on the support (0, 3, 5 to 0, 3, 10) reverses a lottery ranking |
| `control_stretch_monotone_on_support` | SCH-2, control | That map is increasing on the support |
| `control_stretch_not_affine_on_support` | SCH-2, control | No `a, b` make that map affine on the support |
| `control_monotone_nonaffine_reverses_difference_order` | SCH-4, control | The same map reverses an order of differences |
| `control_unequal_total_weight_breaks_invariance` | SCH-1, control | Without equal total weight a shift `b` reverses a ranking, so probabilities summing to one is load-bearing |

Weights are integers rather than probabilities. Two lotteries whose
probabilities each sum to one, scaled by a common denominator, have equal
total weight, which is the hypothesis `hw`.

## Non-vacuity

Each positive theorem has a control in which a dropped hypothesis (affinity, or
equal total weight) lets the conclusion fail on concrete numbers.

## What is not formalised

- The representation theorem and the "only if" half of SCH-1.
- SCH-3, SCH-5 (beyond its formal part), SCH-6 and SCH-7 are claims about
  interpretation, verified by quotation.

## Findings

- None against our documents. The measurement map quotes pp.531, 532, 533 and
  535, and each quotation matches the text.
- The map's step from this paper to the entry contest is not Schoemaker's. It
  rests on `checks/verify_measurement_map.py` MM9, which shows the entry
  condition is an expected-payoff comparison, so the model is of his variant 3.
