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
| FJ-Q6 | 9 | "then minimizing the variance is equivalent to maximizing expected utility" | The objective is the decision maker's prediction accuracy, `FryerJackson2008.eu_iff_var` and `FryerJackson2008.meanDev_eq` |
| FJ-Q7 | 15 | "The decision maker views an object only through the prototype of the category that the object is identified with." | Eq. (3). The prediction is the category mean |
| FJ-Q8 | 15 | "We define an optimal categorization as categorizing past objects in a way to minimize the total sum (across objects) of within-category variance." | Eqs. (4) and (5). The optimisation is over the observer's memory, not over whom to meet. Eq. (4) is `Var`, and Lemma 4 is `FryerJackson2008.lemma4` |
| FJ-Q9 | 23 | "keep in mind that this will be in reference to the set of objects that a given observer will have encountered." | Who counts as a minority depends on the observer's own encounters, so each group can be the coarsely stored one in the other's memory |
| FJ-Q10 | 30 | "with only two categories the optimal sorting is to have all high human capital types in one category and all low human capital types in the other." | The coarse sorting of the minority depends on the number of categories, `FryerJackson2008.control_two_categories` |
| FJ-Q11 | 32 | "In our model, discriminating behavior can exist even when skills are observable, while a statistical discrimination model would not allow for such discrimination." | The authors' statement that showing the trait removes a statistical penalty but not a categorical one |
| FJ-Q12 | 32 | "whereas a taste-based model would predict that larger numbers of blacks would result in more discrimination – the categorization model has the opposite prediction." | More minority encounters give finer categories and less categorical discrimination |
| FJ-Q13 | 33 | "Minority group members will benefit from congregating together." | Choosing where to apply or live as a response to observers' categories, stated as a policy remark and not derived |
| FJ-Q14 | 33 | "Title VII's disparate treatment model of discrimination is premised on the notion that intergroup bias is malevolent in origin. [...] Regulating cognitive processes, on the other hand, is an impossible assignment." | Discrimination that an intent standard in the law does not reach |

## Lean results

`lean/mathlib/FryerJackson2008.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `FryerJackson2008.lemma4` | Lemma 4 for any finite set of objects and any number of binary attributes |
| `FryerJackson2008.lemma3` | Lemma 3 in the same generality |
| `FryerJackson2008.lemma4_one` | Lemma 4 for one attribute, `Var = 2 n⁺ n⁻ / n` |
| `FryerJackson2008.nPlus_union` | Counts add over a disjoint union, so merging groups adds their counts |
| `FryerJackson2008.theorem1_cost` | Merging two groups adds twice the factor (6) to the variation |
| `FryerJackson2008.theorem1` | Theorem 1 in the appendix's form, eq. (10), for heterogeneous groups and any number of attributes |
| `FryerJackson2008.prop1` | Proposition 1. `Var(O₃ ∪ O₄) > Var(O₁ ∪ O₂)` if and only if `h₃₄ (1/n₁ + 1/n₂) > h₁₂ (1/n₃ + 1/n₄)` |
| `FryerJackson2008.prop1_from_theorem1` | For homogeneous groups the factor (6) has the term that gives Proposition 1 |
| `FryerJackson2008.lemma1_triangle` | Lemma 1, step (8), from the triangle inequality |
| `FryerJackson2008.lemma1` | Lemma 1. Splitting a block of one type between two categories is no better than one of the two endpoints |
| `FryerJackson2008.lemma2` | Lemma 2. Replication preserves the ranking of categorizations |
| `FryerJackson2008.prop2` | Proposition 2 in the equidistant world |
| `FryerJackson2008.prop2_change` | The identity in the proof of Proposition 2, with the factor `n_new` the printed proof omits (FJ-D2) |
| `FryerJackson2008.hypercube_adjacent` | The fact used for Lemma 5. More than `2^y` vertices of `{0,1}^(y+1)` include two adjacent ones |
| `FryerJackson2008.lemma5` | Lemma 5, from the counting step `FryerJackson2008.lemma5_count` and the fact |
| `FryerJackson2008.corollary1_step` | Corollary 1, the last step of the proof, with the bound `n₁ ≤ r_I n₄` it uses (FJ-D4) |
| `FryerJackson2008.eu_iff_var` | Section 3. With equal weights, expected utility ranks categorizations as eq. (5) does in reverse |
| `FryerJackson2008.example1_table` | Table 1. Totals 5, 9, 9, 18, 18 and 45 |
| `FryerJackson2008.example1_race_optimal` | Example 1. Pooling the two black types is strictly best of the six three-category merges |
| `FryerJackson2008.example1_prototype_error` | The black category's prototype is `1/2` in human capital, so a black worker of high human capital is predicted with error `1/2`, a white one with error 0 |
| `FryerJackson2008.example2` | Example 2. Payoffs 100, 95, 91, 100 and the condition `p_eV_e > 1.8 p_sV_s` |
| `FryerJackson2008.example3_value` | Example 3. The printed categorization has variation `12201/101` |
| `FryerJackson2008.example4_distances` | Example 4. The distances .696 and .712 are reproduced |
| `FryerJackson2008.example4_not_reproduced` | Example 4. At the stated counts `V₁₃₄,₂ < V₁₂,₃₄` (FJ-D3) |
| `FryerJackson2008.meanDev_eq` | The expected prediction error per object of a category is `2 p (1 − p)` in one attribute, twice the variance of the share |
| `FryerJackson2008.control_lemma4_needs_binary` | Control for Lemmas 3 and 4. With an attribute at `0, 1/2, 1` the variation is 1 and the formula gives `4/3` |
| `FryerJackson2008.control_no_minority` | Control for Theorem 1, Proposition 1 and Example 1. With equal counts of all four types, pooling the black types is not strictly better than pooling the low types |
| `FryerJackson2008.control_lemma1_needs_triangle` | Control for Lemma 1. Without the triangle inequality an interior split beats both endpoints |
| `FryerJackson2008.control_hypercube_tight` | Control for Lemma 5. Two opposite vertices of `{0,1}²` are not adjacent, so the bound `2^y` is tight |
| `FryerJackson2008.control_corollary1` | Control for Corollary 1. With `r_E (2 − r_I) ≤ 1` the merging criterion can fail |
| `FryerJackson2008.control_eu_unequal_weights` | Control for Section 3. With unequal weights the categorization with the larger variation can have the higher expected utility |
| `FryerJackson2008.control_two_categories` | Section 7's claim, and the control for the coarse sorting of the minority. With two categories the split by human capital (18) is strictly best of the seven partitions, and the race split gives 50 |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Eqs. (1) to (5), definitions | `mean`, `Var`, `d`, `varAttr` | general | |
| Section 3, expected utility and variance | `FryerJackson2008.eu_iff_var` | general | |
| Example 1, Table 1 | `FryerJackson2008.example1_table`, `FryerJackson2008.example1_race_optimal` | general | |
| Example 2 | `FryerJackson2008.example2` | general | |
| Example 3 | `FryerJackson2008.example3_value` | special case | The value of the printed categorization only. Its optimality among all partitions was checked by script, not in Lean |
| Lemma 1 | `FryerJackson2008.lemma1`, `FryerJackson2008.lemma1_triangle` | special case | Two categories that both hold other objects, and the weak form (an endpoint is at least as good). The strict form, the case of an otherwise empty category, and the iteration to more than two categories are not formalised |
| Lemma 2 | `FryerJackson2008.lemma2` | general | Stated for groups summarised by counts, which Lemma 4 allows |
| Example 4 | `FryerJackson2008.example4_distances`, `FryerJackson2008.example4_not_reproduced` | special case | The printed distances are reproduced. The printed variations and optimum are not (FJ-D3) |
| Proposition 1 | `FryerJackson2008.prop1`, `FryerJackson2008.prop1_from_theorem1` | general | |
| Theorem 1 | `FryerJackson2008.theorem1`, `FryerJackson2008.theorem1_cost` | general | In the appendix's form (10). The theorem's wording for `J` groups follows by comparing pairs |
| Proposition 2 | `FryerJackson2008.prop2` | general | |
| Corollary 1 | `FryerJackson2008.corollary1_step` | special case | The final step only. The reduction to the extreme case, through Lemma 5 and Theorem 1, is not formalised |
| Lemma 3 | `FryerJackson2008.lemma3` | general | |
| Lemma 4 | `FryerJackson2008.lemma4` | general | |
| Lemma 5 | `FryerJackson2008.lemma5`, `FryerJackson2008.hypercube_adjacent` | special case | The hypercube fact is general. The counting step takes as given the proof's first claim, that more than `(3/8) 2^m` categories hold only minority types, and that each such category is nonempty |
| Section 7, two categories | `FryerJackson2008.control_two_categories` | general | |

Counts. General 11, special case 5, not formalised 0.

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| FJ-D1 | Pages are PDF pages. PDF page 1 is the bepress cover, the journal page is the PDF page minus 2, and the text file has a 45th, empty page | One page numbering for the suite |
| FJ-D2 | The proof of Proposition 2 (PDF p. 37) writes `(2nᵢ − Var) n_new/(nᵢ + n_new) = 2 − (Var + 2n_new)/(nᵢ + n_new)`. The right side lacks the factor `n_new`. The proposition is unaffected, since `n_new` is common to every category | `FryerJackson2008.prop2_change`, `FryerJackson2008.prop2` |
| FJ-D3 | Example 4 and footnote 22 (PDF pp. 18–19) are not reproduced from the stated types, counts and the city-block metric. Our computation gives `V₁₂,₃₄ ≈ 180.4` and `V₁₃₄,₂ ≈ 179.3` at 70 objects of type 1, against the printed 222 and 267, so the printed optimum is not the optimum we compute. The distances .696 and .712 are reproduced. Example 4 is not relied on | `FryerJackson2008.example4_not_reproduced`, `FryerJackson2008.example4_distances`, and a script computation of all seven partitions under city-block, Euclidean and squared Euclidean distance, none of which gives the printed numbers |
| FJ-D4 | The last step of the proof of Corollary 1 (PDF p. 38) cites `n₃/n₁ ≥ r_E` and `n₃/n₄ ≥ r_E r_I`. The step holds, but through `n₁ ≤ r_I n₄` (both are minority groups and `n₄` is the smallest), which the proof does not state, rather than through `n₃/n₁ ≥ r_E`. Then `2n₃/n₁ − n₃/n₄ ≥ (n₃/n₄)(2 − r_I)/r_I ≥ r_E (2 − r_I) > 1`. The first pass recorded this step as not following, and that was wrong | `FryerJackson2008.corollary1_step` |
| FJ-D5 | Corollary 1 states the bound on categories as `7/8 − 1/(number of types)` of the number of types (PDF p. 24), Lemma 5 as `n > (7/8) 2^m` and its proof as `n > (7/8) 2^m − 1` (PDF p. 36) | The three bounds differ slightly. Not relied on |
| FJ-D6 | In Example 3 (PDF p. 11) the text says race, the third attribute, is ignored by the categorization. The categorization it gives splits on the third attribute and ignores the second, human capital, which is the rare one | A brute-force check over all three-category partitions confirms the printed categorization is optimal (total `12201/101 ≈ 120.80`). Only the label is at odds. Not relied on |
| FJ-D7 | The text layer garbles fractions, for example `( 21 , 1)` for `(1/2, 1)` on PDF p. 9 | Equations were read from page images, and quotations avoid them |
