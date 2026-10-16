# Mill and Stein (2012), claims

Evidence tag [F]. The full text of the working paper of December 2012, 60 PDF
pages, was read on 16 October 2026 from its text layer, with PDF pages 33 and
51 to 53 read from page images (MLS-D1). Pages are PDF pages. The printed page
is the PDF page minus 1 from PDF p. 2 on.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| MLS-Q1 | 1 | "Passing for White meant that individuals changed their racial affiliation by changing their social ties, while skin color remained unchanged." | Passing changes the marker others see by changing contact, with appearance fixed |
| MLS-Q2 | 5 | "Mulattoes who passed for White earned 31%-42% more than [...] Because passers tended to be more educated than non-passers, the increase in earnings from being classified as White decreases when we control for education, though it remains considerable at 20%-31%." | The return to passing before and after a schooling control, Table 10, columns 1 and 2 |
| MLS-Q3 | 5 | "When comparing siblings we find that passers earned 14%-21% more than their brothers who did not pass." | Table 10, column 4. The labour-income estimate is not significant (MLS-D5) |
| MLS-Q4 | 9 | "the African-American community reached this pooling equilibrium over skin colors [...] Time was when there were blue-vein societies [...] have in great part folded their tents and crept quietly into the ranks of the whites." | The authors' gloss and Bond (1931) as quoted, [C] for Bond. Exit by passing emptied the venues that screened on the marker |
| MLS-Q5 | 9 | "Blue-vein societies, which he mentions in the quotation, were African-American social clubs that accepted only members whose skin was light enough so that their veins could be seen through it." | Footnote 10. A club that admits on a visible marker within the group |
| MLS-Q6 | 9 | "likely to be the outcome of some context-based classification that is more likely to classify children in households of higher status as Mulattoes." | Footnote 11. The recorded marker responds to household status, stated by the authors as unverified |
| MLS-Q7 | 11 | "not everyone could; both light skin color and sufficient skills to “act White” were necessary." | Passing needs a marker one can change and skills, so it selects |
| MLS-Q8 | 11 | "the higher social status he could enjoy in the African-American community relative to in the White community, the economic stability he gained from the relative scarcity of skilled African-American workers, and his richer social life among the higher ranks of African-Americans." | Myrdal's teacher, as the authors report him, [C] for Myrdal. Reasons not to pass that depend on one's own standing in the group |
| MLS-Q9 | 32 | "the racial sorting according to economic outcomes mattered for the long-run development of African-Americans." | The composition of those who remain, stated and not measured |
| MLS-Q10 | 34 | "we should expect those who had higher earning potential to be more likely to pass because the variance of income among Whites was higher." | The Roy argument, formalised in our reading as `passers_upper` |
| MLS-Q11 | 36 | "Selection across families accounts for 1/3-2/3 of the differences between passers and non-passers in the general population." | Computed as 0.30 and 0.63, `selection_share` (MLS-D4) |
| MLS-Q12 | 36 | "the returns to schooling are higher for those who are White." | Table 10, columns 3 and 6. The steeper slope that drives selection in `gain_strictMono` |
| MLS-Q13 | 37 | "We expect those who had more to gain from passing to have been more likely to pass, and indeed we see that passers were positively selected on education." | Positive selection into passing on schooling |
| MLS-Q14 | 3 | "lighter-skinned African-Americans completed 1.1 more years of schooling on average, and their weekly salaries were 11.4% higher." | Footnote 5. The 11.4% does not match Table 6 (MLS-D2) |
| MLS-Q15 | 3 | "which stands at 15 to 30% in our case" | Footnote 4. The match rates of Table 3 are 34% and 11% (MLS-D3) |
| MLS-Q16 | 25 | "In the general population, adding this control decreases the color premium dramatically and renders it not statistically different from zero (column 2)." | Table 6. The colour gap in earnings vanishes once schooling is controlled |
| MLS-Q17 | 10 | "agents in White markets were not willing to hire or to trade with African-Americans in many cases, so professional passing opened many doors." | Passing as a way around exclusion from markets on the marker |
| MLS-Q19 | 10 | "every bus ride or play at a theater offered an opportunity to pass temporarily, only to later return to the African-American world." | Partial passing, venue by venue, around segregated public places |
| MLS-Q20 | 7 | "African-Americans in the first half of the 20th century lived in a separate public realm due to the Caucasian majority’s refusal to interact with them in public places." | Avoidance in contact running from the high group, collective and partly legal |
| MLS-Q18 | 8 | "even people of African-American descent who looked “fully European” were considered Black if their ancestry was known." | The formal rule ran on ancestry, so appearance mattered only when ancestry was unknown |

## Lean results

`lean/mathlib/MillStein2012.lean`, in the paper's notation. The Roy section is
our reading of the argument on PDF p. 34.

| Theorem | What it checks |
|---|---|
| `MillStein2012.eq_5_2` | Eq. (5.2). The measured difference is the return plus the selection term |
| `MillStein2012.measured_eq_return_iff` | The measured difference is the return exactly when the selection term is zero |
| `MillStein2012.within_family` | Eq. (5.4). Between brothers the family effect cancels |
| `MillStein2012.across_families` | Across families the difference keeps `φ_f − φ_g` |
| `MillStein2012.gain_strictMono` | Our reading. With a steeper White slope, the gain from passing rises in earning potential |
| `MillStein2012.passers_upper` | Our reading. Passers form an upper set in earning potential |
| `MillStein2012.exp_sandwich` | `1 + x + x²/2 + x³/6 ≤ exp x ≤ 1 + x + x²/2 + (2/9) x³` on `[0, 1]` |
| `MillStein2012.col1_percent` | Table 10, column 1. `exp 0.273` lies in `(1.31, 1.32)` and `exp 0.351` in `(1.41, 1.43)` |
| `MillStein2012.col2_percent` | Table 10, column 2. `exp 0.185` lies in `(1.20, 1.21)` and `exp 0.269` in `(1.30, 1.31)` |
| `MillStein2012.col4_percent` | Table 10, column 4. `exp 0.191` lies in `(1.21, 1.22)` and `exp 0.130` in `(1.13, 1.14)` |
| `MillStein2012.selection_share` | `(0.273 − 0.191)/0.273` lies in `(0.30, 1/3)` and `(0.351 − 0.130)/0.351` in `(0.6, 2/3)` |
| `MillStein2012.avg_sub` | The mean of a difference is the difference of means over a finite set |
| `MillStein2012.eq_5_2_avg` | Eq. (5.2) over finite populations of passers and non-passers |
| `MillStein2012.eq_4_1` | Eqs. (4.1) and (5.3). Two men with the same `x` differ by `β (d₁ − d₂)` plus the difference of their errors |
| `MillStein2012.eq_4_3` | Eq. (4.3). District effects cancel within a district |
| `MillStein2012.within_estimator` | Eq. (4.2). With errors orthogonal to colour differences, the within-family slope is `β` |
| `MillStein2012.sibling_inflation` | PDF p. 29. Dividing the sibling gap by the relative colour distance recovers the population gap |
| `MillStein2012.roy_variance` | PDF p. 34. A higher White variance with a nonnegative White slope gives a steeper White slope |
| `MillStein2012.table1b_mulatto_parent` | Table 1b. 68.38% of households have a parent coded Mulatto, above two thirds |
| `MillStein2012.match_rates` | Table 3. Match rates of 33.7% and 10.8% |
| `MillStein2012.illiteracy` | PDF p. 22. Illiteracy is one minus the literacy means of Table 4 |
| `MillStein2012.table4_col2_gap` | MLS-D12. The boys' gap with family controls is printed as 0.039, not 4.8 points |
| `MillStein2012.table6_percent` | Table 6. Bounds on `exp 0.116`, `exp 0.168` and `exp 0.035` |
| `MillStein2012.table6_less` | Table 6. `1 − 1/exp 0.116` and `1 − 1/exp 0.168` are the text's 10.9% and 15.5% |
| `MillStein2012.schooling_accounts` | PDF p. 35. Schooling accounts for 9 to 13 points of the gap |
| `MillStein2012.schooling_return_ratio` | MLS-D13. The White interaction is below half the base slope for total income and above it for labour income |
| `MillStein2012.spencer_dollars` | PDF p. 2. The dollar conversions |
| `MillStein2012.control_family_effect` | Control. Across families with different effects, the comparison misses `β` |
| `MillStein2012.control_equal_slopes` | Control. With equal slopes, passing is not selected on earning potential |
| `MillStein2012.control_log_not_percent` | Control. `0.351` log points exceed 35.1 percent by more than six points |
| `MillStein2012.control_selection_biases` | Control. With a constant return and selection, the measured difference exceeds the return |
| `MillStein2012.control_within_orthogonality` | Control. Without orthogonality the within-family slope is off by `Σ Δx Δε / Σ Δx²` |
| `MillStein2012.control_no_inflation` | Control. Without the inflation the sibling gap at `r = 1/2` is half the population gap |
| `MillStein2012.control_roy_sign` | Control. With a negative White slope, a higher White variance goes with a gain falling in potential |

## Coverage of the source's results

The paper has numbered equations and no numbered propositions. Each numbered
equation, each displayed argument and each number in the text that follows
from printed numbers has a row.

| Result (as the source numbers it) | Lean name | Status | Reason for anything short of general |
|---|---|---|---|
| Eq. (4.1) | `eq_4_1` | general | |
| Eq. (4.2) | `y`, `within_family`, `within_estimator`, `control_within_orthogonality` | special case | The estimator is for families of two brothers. Larger families and controls `x` are not formalised |
| Eq. (4.3) | `eq_4_3` | general | Same algebra as eq. (4.2) |
| Eq. (5.1) | `eq_5_2_avg` (its first term) | general | Over finite populations |
| Eq. (5.2) | `eq_5_2`, `eq_5_2_avg`, `measured_eq_return_iff`, `control_selection_biases` | general | Over finite populations |
| Eq. (5.3) | `eq_4_1` | general | Same specification with `white1940` |
| Eq. (5.4) | `within_family`, `across_families`, `control_family_effect` | general | |
| Colour distance between siblings, PDF p. 29 | `sibling_inflation`, `control_no_inflation` | general | |
| Attenuation within families, PDF p. 29 | none | not formalised | Griliches's argument is stated in words without a model |
| Roy argument, PDF p. 34 | `roy_variance`, `gain_strictMono`, `passers_upper`, `control_equal_slopes`, `control_roy_sign` | special case | Log income linear in one potential `s`. Our reading |
| Estimates of Tables 4 to 10 | none | not formalised | Microdata not available here |
| Table 1b, two thirds and 16%, 20%, 4% | `table1b_mulatto_parent` | general | Arithmetic of printed cells |
| Match rates, PDF p. 19 and Table 3 | `match_rates` | general | |
| Illiteracy, PDF p. 22 | `illiteracy` | general | |
| Gaps of 4.8 and 7.9 points, PDF p. 22 | `table4_col2_gap` | general | Records MLS-D12 |
| Percentages of Table 6, PDF p. 25 | `table6_percent`, `table6_less` | general | |
| Percentages of Table 10, PDF pp. 34 to 35 | `exp_sandwich`, `col1_percent`, `col2_percent`, `col4_percent`, `control_log_not_percent` | general | |
| About 10 points from schooling, PDF p. 35 | `schooling_accounts` | general | |
| 1/3 to 2/3 from selection, PDF p. 36 | `selection_share` | general | |
| Returns to schooling higher by more than 50%, PDF p. 36 | `schooling_return_ratio` | general | Records MLS-D13 |
| Dollar conversions, PDF p. 2 | `spencer_dollars` | general | |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| MLS-D1 | The version read is the working paper of December 2012, updated 30 December 2012, cited as such | The title page. Dahis, Nix and Qian (2019) cite a 2016 version from Arizona State University, which is not read. Nothing is attributed to it |
| MLS-D2 | The population colour gap in earnings is read from Table 6, column 1, as −0.116 log points | The text gives 10.9% less for darker sons (PDF p. 25), and `exp(−0.116) − 1` is about −0.1095. Footnote 5 gives 11.4% higher for lighter sons (MLS-Q14), which matches neither `exp(0.116) − 1` nor 10.9% |
| MLS-D3 | The match rates are 33.7% (Match 1) and 10.8% (Match 2) | Table 3, 20,574 and 6,613 of 61,055, read from the image. Footnote 4 says 15 to 30% (MLS-Q15), and PDF p. 19 says 34% and 11% |
| MLS-D4 | "1/3-2/3" (MLS-Q11) is read as 0.30 to 0.63 | `selection_share`. The lower end lies just below one third |
| MLS-D5 | Between brothers the passing premium is significant only for total income and only at 10% | Table 10, column 4, 0.191 (s.e. 0.097) and 0.130 (s.e. 0.125). With schooling, column 5 gives 0.054 and 0.001. The text (PDF p. 36) calls these "minor differences" |
| MLS-D6 | With the interaction of White and schooling, the White main effect is the premium at zero schooling | Table 10, columns 3 and 6, −0.041 and −0.442 for total income. The premium works through the schooling slope |
| MLS-D7 | The linking appendix is absent | The text and the note to Table 3 cite Appendix A. PDF p. 46 reads "To be added" |
| MLS-D8 | Some statistical comparisons are announced but absent | Footnote 25 says the neighbours sample is not fully digitised, and footnote 28 that tests of equal sibling and population gaps are in progress |
| MLS-D9 | Typos not relied on | Eq. (5.2) is described with "individual k" while the formula uses `j` (PDF p. 33). A sentence on alleles (PDF p. 28) is garbled. The notes to Tables 6 and 7 describe "difference in schooling" for income columns |
| MLS-D10 | Quotations and paraphrases of Bond (1931), Myrdal (1944) and Stephenson (1910) are the authors' | Tagged [C] for those sources. MLS-Q4 and MLS-Q8 are of this kind |
| MLS-D12 | The boys' colour gap in literacy with family controls is read as 3.9 points | Table 4, column 2, prints −0.039. The text (PDF p. 22) says 4.8 points. The girls' 7.9 matches column 7. `table4_col2_gap` |
| MLS-D13 | "Higher by more than 50% for Whites" (PDF p. 36) holds for labour income only | Table 10, column 3. `0.029/0.064` is below one half and `0.042/0.061` above it. `schooling_return_ratio` |
| MLS-D11 | The passing sample is all sons coded Mulatto in 1910 with brothers aged 3 to 18, matched to 1940, with passers those coded White in 1940 | Table 10, note 2. Footnote 35 assumes every son coded Mulatto could pass |
