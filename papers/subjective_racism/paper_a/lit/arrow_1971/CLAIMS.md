# Arrow (1971), claims

Evidence tag [P]: the 1971 Princeton working paper, cited as `Arrow1971`,
read in full from page images on 15 October 2026. The 1973 chapter is not
read (AR-D1).
Pages are PDF pages of the scan. Quotations are checked against an OCR of the
images with whitespace removed.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| AR-Q1 | 3 | "the operation of these markets has led to a greater degree of simple exclusion and less of price differentials." | Outside the labor market discrimination shows as exclusion rather than as a price. Paper A's channel is exclusion, refused contact (M4) |
| AR-Q2 | 20 | "discrimination by W workers will not result in market wage differentials but instead does result in segregation." | Taste discrimination by co-workers gives segregation, not a wage gap. Not a parallel to any row |
| AR-Q3 | 25 | "history matters a good deal." | With adjustment costs the outcome depends on the start. Paper A's basins make the long-run standing depend on the start (M6) |
| AR-Q4 | 29 | "the probability of being qualified is reflected in a wage differential." | Eq. (14), formalised as `wage_differential` |
| AR-Q5 | 29 | "Once we shift the explanation of discriminatory behavior from unanalyzable (or at any rate unanalyzed) tastes to beliefs, we are led to seek to explain these beliefs." | Beliefs are to be explained, not assumed. Paper A explains the group prior by the group's standing (D1) |
| AR-Q6 | 30 | "if individuals act in a discriminatory manner, they will tend to acquire or develop beliefs which justify such actions." | Beliefs and actions come into equilibrium, the first of his two explanations |
| AR-Q7 | 31 | "who qualify is an increasing function of the gain from qualifying." | Eq. (15). Qualification is an unobserved investment, the mechanism that Coate and Loury later formalise |
| AR-Q8 | 32 | "a result which strongly suggests, though [...] that there are equilibria other than the symmetric, non-discriminatory, one." | Self-fulfilling discriminatory equilibria are suggested, not proved. The elision is "it does not prove,", which the OCR renders with a capital I |
| AR-Q9 | 33 | "schedule for qualified labor, the more likely is the system to be unstable." | The stability condition, of which `diff_mode_coeff` reproduces the left side |
| AR-Q10 | 33 | "how individual employers acquire knowledge which will modify their initial estimates of distributions as differing between groups" | Learning about the group is left open. The paper has no benchmark in which counter-examples erode a stereotype |

## Lean results

`lean/mathlib/Arrow1971.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `Arrow1971.wage_eq14` | Eq. (14) from the two equations (13) |
| `Arrow1971.wage_differential` | `p_B < p_W` and `w_B < MP_S` give `w_W > w_B` |
| `Arrow1971.control_equal_beliefs` | Control. Equal beliefs give equal wages |
| `Arrow1971.wage_gap_of_belief_gap` | The wage gap is `r (p_W − p_B)/(p_W p_B)`, our derivation |
| `Arrow1971.diff_mode_coeff` | The difference mode's coefficient is `E (MP_S − w_S)/(w_S − w_U)`, our derivation |
| `Arrow1971.control_no_personnel_cost` | Control. With `r = 0` the coefficient is 0, so the symmetric equilibrium is stable, as the paper says |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| AR-D1 | The version read is the 1971 Princeton working paper No. 30A, and the record cites it as `Arrow1971`, not the 1973 chapter | The Drive file's first page. Claims are about the working paper. Whether the chapter differs is not known |
| AR-D2 | The OCR drops some lines, for example "reality. That is, if employers have preconceived ideas that B workers" on PDF page 27 | Those lines were read from the images, and no quotation is taken from them |
| AR-D3 | The stability condition's derivation is ours for the difference mode only | The paper refers the proof to Arrow (1971), Technical Note F |
| AR-D4 | Pages are PDF pages | The working paper's own numbering restarts twice |
