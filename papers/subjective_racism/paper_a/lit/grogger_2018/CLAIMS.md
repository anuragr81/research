# Grogger (2018), claims

Evidence tag [F]. The full text of "Speech and Wages", working paper Version 8
of 2 February 2018, 64 PDF pages, was read on 16 October 2026 from its text
layer, with PDF page 26 read from its image. This is not the Grogger (2011)
article, which is not read (GRG-D1). Pages are PDF pages. The printed page is
the PDF page minus 1 on PDF pp. 3 to 40.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| GRG-Q1 | 1 | "For blacks, it is explained by sorting: workers whose speech is racially indistinct sort toward occupations that involve intensive interpersonal interactions and earn a sizeable wage premium in those jobs." | The abstract's finding. The premium lives in contact-intensive work |
| GRG-Q2 | 3 | "One could similarly imagine a consumer taste for mainstream speech." | Customers' preference as one source of the premium, after Hamermesh and Biddle |
| GRG-Q3 | 6 | "linguists have shown that listeners can identify the race of a speaker based on very short audio clips" | Speech is read as race. The author's summary of linguistics, [C] for it |
| GRG-Q4 | 7 | "one tends to acquire one’s native accent from one’s linguistic peers during the sensitive period, rather than from one’s parents" | Speech is set in childhood by peers, so it is hard to change as an adult. [C] for the cited literature |
| GRG-Q5 | 7 | "both black and white listeners routinely rate AAVE speakers lower than SAE speakers in terms of socioeconomic status, intelligence, and even personal attractiveness" | Speech is read as class by listeners of both races. [C] for the social psychology cited |
| GRG-Q6 | 13 | "family income tends to be higher, the fewer the number of listeners who reported the speaker to be black or Southern." | Table 2. Speech correlates with family income within race |
| GRG-Q7 | 18 | "black workers with mainstream speech patterns earn 1.6 percent (standard error =5.1 percent) less than comparably skilled whites, on average. The results also show that whites whose speech is perceived as black earn 16.2 percent less than whites with mainstream speech." | Table 3, column 5. The penalty follows the race heard, `table3_col5` |
| GRG-Q8 | 20 | "However, adding these variables has much less effect on the mainstream speech premium for blacks, reducing it from 0.136 only to 0.123." | The premium survives family background. Table 4 prints 0.109 (GRG-D2) |
| GRG-Q9 | 25 | "a strong relationship between mainstream speech and wages among African Americans that cannot be explained by omitted variable bias involving location, AFQT scores, household income, family structure, parental education, personality traits, or criminal records." | The premium survives controls for skill and background |
| GRG-Q10 | 26 | "it is natural to think that mainstream speech might raise the worker’s productivity in interactive tasks involving consumers and coworkers. This could stem from consumer discrimination or arise if mainstream speech represents a social skill" | The two explanations the paper leaves open |
| GRG-Q11 | 28 | "However, workers do not earn a wage premium from sorting into occupations that are intensive in these tasks." | Table 6. No premium in work intensive in tasks other than interaction, read against speech as general skill |
| GRG-Q12 | 30 | "This suggests that there is little adaptation toward mainstream speech." | Table 7. Speech does not move toward the mainstream in adulthood, `table7` |
| GRG-Q13 | 32 | "Considering what is known about language acquisition, it is impossible say that the regression analysis isolates the effect of mainstream speech." | The author's own limit on a causal reading. "impossible say" is in the source |
| GRG-Q14 | 33 | "One explanation is customer and coworker discrimination." | The conclusion's leading explanation |
| GRG-Q15 | 25 | "The joint F-statistic for all the variables that appear in column (1) has a p-value of 0.483." | Table 5 prints 0.442 (GRG-D3) |

## Lean results

`lean/mathlib/Grogger2018.lean`, in the paper's notation, with `l` for `λ`.

| Theorem | What it checks |
|---|---|
| `Grogger2018.three_report_mainstream_iff` | A speaker heard on one prompt is mainstream exactly when at most one of three listeners heard him as black |
| `Grogger2018.w_diff` | Eq. (1). `w₁ − w₀` is the index of eq. (2) plus `ε₁ − ε₀` |
| `Grogger2018.sorting` | Eq. (2). With `λ₁ > λ₀` and a strictly increasing `F`, the probability of the task-intensive sector rises in `S` |
| `Grogger2018.fn19` | Footnote 19 without disturbances. A worker with `S ≥ 0` who earns more in sector 0 forces `α₁ < α₀` |
| `Grogger2018.table3_col2` | `5 × 0.037 = 0.185` and `5 × 0.046 = 0.23` |
| `Grogger2018.table3_col5` | `−0.152 + 0.136 = −0.016` |
| `Grogger2018.table4_col2` | `−0.069 + 0.119 = 0.050` |
| `Grogger2018.table7` | `2/25 = 8%` and `4/9` lies between 44% and 45% |
| `Grogger2018.control_doubling_needed` | Control. Without the doubling rule, a speaker all three of whose listeners heard him as black would count as mainstream |
| `Grogger2018.control_no_sorting` | Control. With equal skill prices the choice probability does not depend on `S` |
| `Grogger2018.control_fn19` | Control. With `α₁ ≥ α₀` and `λ₁ ≥ λ₀`, no worker with `S ≥ 0` earns more in sector 0 |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| GRG-D1 | The paper read is "Speech and Wages", Version 8, 2 February 2018, cited as the working paper. It is not Grogger (2011), "Speech Patterns and Racial Wage Inequality", Journal of Human Resources 46 | The title page and the reference list (PDF p. 37). What the paper says of the 2011 article, 2006 data and five listener reports (footnote 8 on PDF p. 18, and PDF p. 30), is [C] for that article. No published version of the 2018 paper is read |
| GRG-D2 | Table 4 is read as printed, not as the text describes it | The text (PDF p. 20) gives −0.099 for the black dummy and 0.123 for the premium with family background (GRG-Q8), and 0.063 and 0.036 for the Southern premium. Table 4 prints −0.050, 0.109, 0.061 and 0.034. The qualitative statement holds in both |
| GRG-D3 | The p-value of the joint test in Table 5 is read as 0.442 | The text (GRG-Q15) gives 0.483. Both are far from significance |
| GRG-D4 | "Table 6 shows just the opposite" (PDF p. 30) refers to Table 7 | The cross-tabulation of 2006 and 2011 speech is Table 7 (PDF p. 48) |
| GRG-D5 | The section numbers are read by content | PDF p. 5 announces Section IV for the regressions, which are headed V. Two sections are headed VI. Section III has subsections A, B, C and E |
| GRG-D6 | Appendix Tables 5 and 6 are read by their contents | Both are titled "Southern white*mainstream speech coefficients from sorting probits and sector-specific linear wage regressions" but report black times mainstream coefficients from log wage regressions |
| GRG-D7 | The O*NET elements are in Appendix Table 8 | The Data Appendix (PDF p. 63) refers to Appendix Table 9, which does not exist |
| GRG-D8 | "Austin-Smith and Fryer 2004" (PDF p. 4) is Austen-Smith and Fryer (2005) | The reference list (PDF p. 34) |
| GRG-D9 | Log-point coefficients are written as percentages | "13.6 percent" for 0.136, "18.5 percent" for `5 × 0.037`. `table3_col2`, `table3_col5` |
| GRG-D10 | The wage-availability model of Table 5 is a probit, and its "P for joint F-statistic" is read as the p-value of a joint test | The table's title and row label |
| GRG-D11 | Typos not relied on | "I see to explain" (PDF p. 3), "it is worth nothing" (PDF p. 29), "it is impossible say" (GRG-Q13) |
| GRG-D12 | Statements of linguistics, social psychology and the Moving to Opportunity results are the author's summaries | Tagged [C] for those sources. GRG-Q3, GRG-Q4 and GRG-Q5 are of this kind |
