# Crabtree, Gaddis, Holbein and Larsen (2022), claims

Evidence tag [F]: the published article, 19 PDF pages, was read in full on 16
October 2026 from its text layer, with pages 6 to 9 also read as images for
Figures 1 to 5. Pages are PDF pages. Journal page = PDF page + 453. Where the text layer
breaks a word with a hyphen across lines, the quotation elides the break with
`[...]` and keeps the rest of the word (CR-Q8, CR-Q13, CR-D6).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CR-Q1 | 1 | "We find evidence that names commonly used to signal race/ethnicity also influence perceptions about socioeconomic status and social class." | Abstract. A racial signal moves perceived class. The direct evidence for A1's inference from marker to class, in perception |
| CR-Q2 | 1 | "Even when we present respondents with the educational attainment of a named individual, respondents still perceive Black people as lower social class than White people." | Abstract. The group still moves perceived class when an individual class cue is shown |
| CR-Q3 | 2 | "which can occur when researchers present experimental subjects with information about one attribute in an experimental treatment (i.e., race) and this causes them to update their beliefs about others too (i.e., social class)." | The authors' framing is belief updating from the racial attribute to class, the inference A1 assumes |
| CR-Q4 | 3 | "a potential problem with social class signaling is that people often misperceive economic reality." | Perceived class from a name need not equal the group's actual class. Bears on A1's consistency condition, which the paper does not test |
| CR-Q5 | 3 | "In total, 11,530 respondents provided 151,899 perceptions on 1,000 different combinations of first and last names." | Scale, checked as `sample_total` and `names_total` |
| CR-Q6 | 3 | "respondents perceive educational attainment, income, and social class from names in a racially tiered pattern: White and Asian people are perceived at the top of the social class hierarchy, followed by Black people and then Hispanic people." | The ordering of perceived class by intended group |
| CR-Q7 | 6 | "we estimate a series of ordinary least squares models that regress respondent answers to our social class questions on a set of dummy variables that indicate the racial signal each name was intended to convey." | Identification. The regressor is the intended signal, not the perceived race, and no respondent covariates are reported |
| CR-Q8 | 9 | "respondents perceive names across a wide range of social class categories for each racial/ethnic group. [...] dents perceive a few White names as less than middle class, on average, and a few Black names as greater than middle class. However, the vast majority of the White distribution does not overlap with the vast majority of the Black distribution." | Names within a group are not perceived alike, so a name is not a group-sufficient marker (`control_name_heterogeneity`). The group still separates most names |
| CR-Q9 | 9 | "This perception gap between individuals with Black and White names is reduced considerably (p < 0.05) when respondents are told that a named individual has a bachelor's degree from a private Ivy League" | Study 5. Showing a strong class cue narrows the group gap in perceived class |
| CR-Q10 | 10 | "However, the difference in perception between individuals with Black and White names is still statistically significant (p < 0.05)." | Study 5. The gap does not vanish, `control_gap_persists` |
| CR-Q11 | 10 | "for respondents to perceive Black people as having (roughly) equal SES levels as White people, one has to signal that Black people have incredibly elite levels of education. [...] This is particularly true in cases where researchers do not include independent cues about social class in their experimental designs, such as in most audit studies outside employment contexts." | The authors' reading of Figure 5, and their point that the class content of a name matters most where no other class cue is present, outside employment |
| CR-Q12 | 10 | "part of the mechanism of racial/ethnic discrimination might be individuals' associations of lower SES or social class with Black and Hispanic people. [...] Researchers could develop new name-based, factorial experimental designs that focus on identifying social class as a mechanism driving discrimination." | The class channel of racial discrimination is named as one interpretation, and the test that crosses race with class is proposed as future work, not run on behaviour |
| CR-Q13 | 10 | "The survey experiment context [...] sitates that we prompt respondents to think about their social class perceptions of names. [...] Moreover, in some contexts, decision-makers may have access to explicit signals of social class." | A limitation the authors state. Perceived class is elicited, not shown to drive any decision, and real decision-makers may see class directly |
| CR-Q14 | 12 | "Respondents answered questions first on racial perceptions, followed by citizenship status, income, and finally, education perceptions." | Footnote 4. In Studies 1–3 the race question primes the class questions. Study 4 asks only about class |
| CR-Q15 | 11 | "This "crispness," however, comes at the expense of the additional assumption that perceptions of race do not cause perceptions of class." | The authors allow that perceived race causes perceived class |

## Lean results

`lean/mathlib/CrabtreeEtAl2022.lean`. The counts, the estimating model and
the name-selection rule are the paper's. Group sufficiency is A1's property
tested against the paper's figures. The Study 5 mixture is our reading.

| Theorem | What it checks |
|---|---|
| `CrabtreeEtAl2022.sample_total` | The five samples sum to 11,530 |
| `CrabtreeEtAl2022.names_total` | 400 + 600 names make the 1,000 combinations, with 240 White or Black and 120 Hispanic names in Study 4 |
| `CrabtreeEtAl2022.resid_sum_zero` | Within each group, residuals from the group mean sum to zero |
| `CrabtreeEtAl2022.cross_zero` | Residuals from group means are orthogonal to any function of the group |
| `CrabtreeEtAl2022.ols_dummies_group_means` | Least squares on group dummies is fitted by the group means, so the plotted predictions are mean answers by intended group |
| `CrabtreeEtAl2022.distinctive_unique` | With thresholds above one half a name is distinctive for at most one group |
| `CrabtreeEtAl2022.sufficient_iff_factor` | A perception is group-sufficient exactly when it factors through the group |
| `CrabtreeEtAl2022.within_dispersion_zero` | Under group sufficiency the names of one group have zero dispersion about their mean |
| `CrabtreeEtAl2022.gap_change_did` | The change in the Black–White gap across education levels is a difference in differences of cell means |
| `CrabtreeEtAl2022.gap_eq` | Under a common-weight mixture the gap is `(1 − w)(μW − μB)` |
| `CrabtreeEtAl2022.gap_indep_signal` | That gap is the same at every education level |
| `CrabtreeEtAl2022.varying_gap_needs_varying_weight` | A gap that changes across education levels rules out every common-weight mixture |
| `CrabtreeEtAl2022.gap_shrinks` | A larger weight on the stated education gives a smaller gap |
| `CrabtreeEtAl2022.gap_zero_iff` | The gap vanishes exactly at full weight on the stated education |
| `CrabtreeEtAl2022.control_lad_not_mean` | Control. Under least absolute deviations the group mean is not the fit |
| `CrabtreeEtAl2022.control_black_surname_threshold` | Control. With the 0.45 threshold for Black surnames, a surname used mostly by White people can pass |
| `CrabtreeEtAl2022.control_name_heterogeneity` | Control. Two names of one group perceived differently fail group sufficiency |
| `CrabtreeEtAl2022.control_gap_persists` | Control. Below full weight the gap is positive at every education level |

## Coverage of the source's results

The paper numbers no propositions. Its results are Figures 1 to 5, read
through the estimating model of p. 6, and the counts and rules of pp. 4–5.

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Sample sizes and total respondents (pp. 1, 3, 4) | `sample_total` | general | |
| Names, 400 and 600, 1,000 in all (pp. 3, 5) | `names_total` | general | Study 5's own count is inconsistent (CR-D1) |
| Name selection by usage thresholds (p. 5) | `distinctive`, `distinctive_unique`, `control_black_surname_threshold` | general | |
| Estimating model, OLS on intended-race dummies (CR-Q7) | `ssr`, `groupMean`, `ols_dummies_group_means`, `resid_sum_zero`, `cross_zero`, `control_lad_not_mean` | special case | The point estimates in general. Robust standard errors and the confidence intervals are not formalised |
| Figure 1, education and income by intended group, Studies 1–3 (CR-Q6) | `ols_dummies_group_means` | special case | The plotted points are group means. Their values are data, read from the image |
| Figure 2 and Figure 4, name-level heterogeneity (CR-Q8) | `GroupSufficient`, `sufficient_iff_factor`, `within_dispersion_zero`, `control_name_heterogeneity` | special case | What the heterogeneity rules out, group sufficiency of names. The name means are data |
| Figure 3, class by intended group, Study 4 | `ols_dummies_group_means` | special case | As Figure 1 |
| Figure 5, class by intended race and stated education, Study 5 (CR-Q9, CR-Q10) | `cellGap`, `gap_change_did`, `varying_gap_needs_varying_weight` | special case | The difference in differences is formalised. The `p < 0.05` tests are not |
| Our reading of Figure 5 as a common-weight mixture | `perc`, `gap_eq`, `gap_indep_signal`, `gap_shrinks`, `gap_zero_iff`, `control_gap_persists` | general | Not a result of the paper |
| Study 4 recoding of free text, 9.7% removed (p. 5) | none | not formalised | A coding rule on data |
| Question order in Studies 1–3 (CR-Q14) | none | not formalised | A design fact |
| The two interpretations, "feature" and "bug" (pp. 10–12) | none | not formalised | Stated in words with no structure beyond CR-Q12 and CR-Q15 |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| CR-D1 | Study 5 used 80 names, 40 White and 40 Black, with each respondent shown one of 40 (20 and 20) | Page 5 states both, "a subset (from Study 4) of 80 total names" and "one of 40 randomly assigned names (20 White and 20 Black)". The record does not resolve which set of 40 a respondent drew from. No row relies on it |
| CR-D2 | "Roberts et al. 2019" (p. 3) is cited for media coverage of Black poverty, but the reference list gives Roberts et al. (2019) as a review of satisficing in surveys | Reference list, p. 18. The cited claim is not attributed to that reference here |
| CR-D3 | "Fryer and Levitt 2004" (p. 3) is cited for demographic data on names and SES, but the reference list gives their paper on the Black–White test score gap | Reference list, p. 15. The names paper of the same authors is not in the list |
| CR-D4 | Figure values are read from the images, to about 0.05 on a three-point scale | The text gives orderings and significance only |
| CR-D5 | The cached text splits into 20 segments, the last empty | `pdfinfo` gives 19 pages. Pages 1 to 19 are used |
| CR-D6 | "Respon-dents" (p. 9) and "neces-sitates" (p. 10) are hyphenated across lines in the text layer | The suite joins a hyphen only before a lower-case letter at the start of the next line, and these breaks are followed by indentation. The quotations elide the break |
