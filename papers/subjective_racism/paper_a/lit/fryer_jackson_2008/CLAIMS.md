# Fryer and Jackson (2008), claims

Evidence tag [F]. The published article was read in full from its text layer on
16 October 2026, all 44 PDF pages, with page images read where the extraction
garbles an equation (PDF pages 20, 21, 22, 34 and 38). Pages are PDF pages. The
journal page is the PDF page minus 2 (FJ-D1).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| FJ-Q1 | 2 | "types of experiences and objects that are less frequent in the population tend to be more coarsely categorized and lumped together" | The headline result. A minority is pooled into more heterogeneous categories, so its members are predicted from a wider category |
| FJ-Q2 | 6 | "This means that the white workers end up perfectly sorted, but the black workers end up only sorted by race and not by their human capital level!" | Example 1, formalised as `FryerJackson2008.example1_table` and `FryerJackson2008.example1_race_optimal` |
| FJ-Q3 | 6 | "This prototype works against the high human capital blacks, as the expectation from the prototype of their category is lower than their type." | The penalty falls on the high member of the pooled category through the category mean, not through its spread, `FryerJackson2008.example1_prototype_error` |
| FJ-Q4 | 7 | "This also contrasts with statistical discrimination since it is not a multiple equilibrium phenomenon where it could equally as well be the majority that is discriminated against" | The minority is disadvantaged by its size, not by a self-confirming equilibrium |
| FJ-Q5 | 7 | "it results from an inherent bias against minority interactions in the process of categorization of human memory, even when qualifications are fully observable." | The observer sees every attribute and stores it coarsely. Nothing is unseen, unlike class in Paper A1 |
| FJ-Q6 | 9 | "then minimizing the variance is equivalent to maximizing expected utility" | The objective is the decision maker's prediction accuracy, `FryerJackson2008.meanDev_eq` |
| FJ-Q7 | 15 | "The decision maker views an object only through the prototype of the category that the object is identified with." | Eq. (3). The prediction is the category mean |
| FJ-Q8 | 15 | "We define an optimal categorization as categorizing past objects in a way to minimize the total sum (across objects) of within-category variance." | The optimisation is over the observer's memory, not over whom to meet |
| FJ-Q9 | 16 | "Let the variation of a group of objects simply be the total sum of distances from the mean" | Eq. (4) in the city-block metric, formalised by attribute as `varAttr` and `FryerJackson2008.lemma4_one` |
| FJ-Q10 | 30 | "with only two categories the optimal sorting is to have all high human capital types in one category and all low human capital types in the other." | The coarse sorting of the minority depends on the number of categories, `FryerJackson2008.control_two_categories` |
| FJ-Q11 | 32 | "In our model, discriminating behavior can exist even when skills are observable, while a statistical discrimination model would not allow for such discrimination." | The rival to Paper A1's test (contribution 4). In this model the group penalty need not vanish when the relevant trait is shown |
| FJ-Q12 | 32 | "whereas a taste-based model would predict that larger numbers of blacks would result in more discrimination – the categorization model has the opposite prediction." | A size prediction that separates categorization from tastes |
| FJ-Q13 | 33 | "Minority group members will benefit from congregating together." | Sorting into places with more minority contact, stated as a policy remark and not derived |
| FJ-Q14 | 28 | "we can view the choice of attributes as a choice of identity" | Chosen attributes as identity, stated informally in Section 7 and not modelled |

## Lean results

`lean/mathlib/FryerJackson2008.lean`, in the paper's notation, for one binary
attribute at a time and the four types of Example 1.

| Theorem | What it checks |
|---|---|
| `FryerJackson2008.lemma4_one` | Lemma 4 for one attribute. Eq. (4) equals `2 n⁺ n⁻ / n` |
| `FryerJackson2008.prop1` | Proposition 1. `Var(O₃ ∪ O₄) > Var(O₁ ∪ O₂)` if and only if `h₃₄ (1/n₁ + 1/n₂) > h₁₂ (1/n₃ + 1/n₄)` |
| `FryerJackson2008.example1_table` | Table 1. Totals 5, 9, 9, 18, 18 and 45 |
| `FryerJackson2008.example1_race_optimal` | Example 1. Pooling the two black types is strictly best of the six three-category merges |
| `FryerJackson2008.example1_prototype_error` | The black category's prototype is `1/2` in human capital, so a black worker of high human capital is predicted with error `1/2`, a white one with error 0 |
| `FryerJackson2008.meanDev_eq` | The expected prediction error per object of a category is `2 p (1 − p)` in one attribute, twice the variance of the share |
| `FryerJackson2008.prop2_change` | The proof of Proposition 2, with the factor `n_new` the printed proof omits (FJ-D2) |
| `FryerJackson2008.example4_not_reproduced` | At the stated counts of Example 4, `V₁₃₄,₂ < V₁₂,₃₄` (FJ-D3) |
| `FryerJackson2008.control_no_minority` | Control. With equal counts of all four types, pooling the black types is not strictly better than pooling the low types |
| `FryerJackson2008.control_two_categories` | Control. With two categories the split by human capital (18) is strictly best of the seven partitions, and the race split gives 50 |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| FJ-D1 | Pages are PDF pages. PDF page 1 is the bepress cover, the journal page is the PDF page minus 2, and the text file has a 45th, empty page | One page numbering for the suite |
| FJ-D2 | The proof of Proposition 2 (PDF p. 37) writes `(2nᵢ − Var) n_new/(nᵢ + n_new) = 2 − (Var + 2n_new)/(nᵢ + n_new)`. The right side lacks the factor `n_new`. The proposition is unaffected, since `n_new` is common to every category | `FryerJackson2008.prop2_change` |
| FJ-D3 | Example 4 and footnote 22 (PDF pp. 18–19) are not reproduced from the stated types, counts and the city-block metric. Our computation gives `V₁₂,₃₄ ≈ 180.4` and `V₁₃₄,₂ ≈ 179.3` at 70 objects of type 1, against the printed 222 and 267, so the printed optimum is not the optimum we compute. The distances .696 and .712 are reproduced. Example 4 is not relied on | `FryerJackson2008.example4_not_reproduced`, and a script computation of all seven partitions under city-block, Euclidean and squared Euclidean distance, none of which gives the printed numbers |
| FJ-D4 | The last step of the proof of Corollary 1 (PDF p. 38) cites `n₃/n₄ ≥ r_E r_I`, a lower bound on the term subtracted in `2n₃/n₁ − n₃/n₄ > 1`, where an upper bound is needed. The step as printed does not deliver the inequality. Corollary 1 is not relied on and not formalised | Page image of PDF p. 38 |
| FJ-D5 | Corollary 1 states the bound on categories as `7/8 − 1/(number of types)` of the number of types (PDF p. 24), Lemma 5 as `n > (7/8) 2^m` and its proof as `n > (7/8) 2^m − 1` (PDF p. 36) | The three bounds differ slightly. Not relied on |
| FJ-D6 | In Example 3 (PDF p. 11) the text says race, the third attribute, is ignored by the categorization. The categorization it gives splits on the third attribute and ignores the second, human capital, which is the rare one | A brute-force check over all three-category partitions confirms the printed categorization is optimal (total 120.8). Only the label is at odds. Not relied on |
| FJ-D7 | The text layer garbles fractions, for example `( 21 , 1)` for `(1/2, 1)` on PDF p. 9 | Equations were read from page images, and quotations avoid them |
