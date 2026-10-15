# Phelps (1972), claims

Evidence tag [P]: page images of pp. 659 to 661 were read in full on 15
October 2026. Quotations are checked against an OCR of those images, with
whitespace removed (`RECONSTRUCTION.md`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| PH-Q1 | 659 | "Skin color or sex is taken as a proxy for relevant data not sampled." | The group enters only as a prior about the individual, as in Paper A's M1 to M3 |
| PH-Q2 | 659 | "might stem from the employer's previous statistical experience with the two groups" | The group prior can come from experience of the group, the reading Paper A adopts with the re-learned prior (D1) |
| PH-Q3 | 659 | "(in which latter case the discrimination is self-perpetuating)" | Phelps names persistence but does not model it. Nothing in the paper has beliefs eroding with counter-examples |
| PH-Q4 | 660 | "The weights applied to the test information and the skin color information are inversely related to the variances of the respective disturbance terms corresponding to them." | Eq. (5′). His test score is evidence about the individual, which group sufficiency rules out for Paper A's marker (M1) |
| PH-Q5 | 661 | "The employer credits an equally good test score by the white applicant as a less credible indication in view of the prior notions of the comparatively narrow range of white promise." | Case 2, formalised as `case2_crossing` |
| PH-Q6 | 661 | "Discrimination is no less damaging to its victims for being statistical." | His own framing, not relied on for any row |

## Lean results

`lean/mathlib/Phelps1972.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `Phelps1972.a_mem` | Eqs. (2) and (5). The weight on the test lies in `(0, 1)` |
| `Phelps1972.weights_sum` | Eq. (5′). The two weights sum to 1 |
| `Phelps1972.a_strictMono` | Case 2. A larger variance gives a larger weight on the test |
| `Phelps1972.case1_parallel` | Case 1. Equal variances give parallel predictions, the black one lower by `β (1 − a)` |
| `Phelps1972.case2_crossing` | Case 2. A larger variance for blacks gives a score above which blacks are predicted higher |
| `Phelps1972.control_no_crossing_equal_variance` | Control. With equal variances there is no crossing |
| `Phelps1972.control_weight_below_one` | Control. The weight on the test is never 1 |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| PH-D1 | The predictor of eq. (5′) is read in levels as `α + z + a (y − α − z)` | Eq. (5′) is in deviations from population means, and the level form is the same line |
| PH-D2 | The paper's reference to Arrow is the 1971 RAND memorandum, not Arrow (1973) | Its reference list. The two are not assumed to coincide |
| PH-D3 | OCR renders some symbols wrongly, for example `λ` as `入` and `ε` as `e` | Quotations are taken only from passages without symbols |
