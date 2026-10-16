# Levon, Sharma and Ilbury (2022), claims

Evidence tag [F]: the Sutton Trust report *Speaking Up* of November 2022, all
40 PDF pages, was read in full on 16 October 2026 from the text layer, with
page images of PDF pages 1, 12, 13, 16 to 19, 33, 34 and 40 for the cover and
the figures. Pages below are PDF pages. The printed page is the PDF page
minus 1.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| ST-Q1 | 9 | "For traits that are not visible, such as socio-economic status, accent is often the primary signal." | The authors' premise. Class is unseen and accent is the marker it is read from |
| ST-Q2 | 28 | "In Oxford, it feels like those with posh London accents (which represents a lot of people at Oxford) gravitate towards each other, starting even in Freshers' week. In contrast, I feel like my accent gives away that I am working class." | A testimonial of contact sorted by accent, with class read from accent. Not a representative finding (PDF p. 16) |
| ST-Q3 | 14 | "recordings whereas a milder MLE accent was rated well. This sensitivity to accent strength was not as strong for Northern accents, suggesting a penalty for not modifying certain accents." | The marker is graded. A milder accent within the same accent group is rated better, so partial modification pays, and pays more for some accents than for others |
| ST-Q4 | 7 | "Encouraging employees to change their accents will maintain an over-representation of RP in contexts of authority, and such an expectation also places an unequal cognitive burden on certain applicants and employees. It can also lead to alienation from one's own social group." | Two costs of passing, a cognitive burden and the loss of standing in one's own group |
| ST-Q5 | 8 | "Suppressing an accent out of self-consciousness can come across as a lack of confidence, which has the opposite of the intended effect." | Imperfect passing is itself read by listeners. The authors' advice, resting on the confidence finding of the Accent Bias Britain studies |
| ST-Q6 | 23 | "large numbers of respondents also reported changing their accent when they moved to secondary school or sixth form, or when they entered the independent school system" | Who modifies, from the 178 open-ended student responses. Modification happens on entry to a more heterogeneous or higher-class setting |
| ST-Q7 | 36 | "I felt as though in school people would be able to tell I was poor from the way I spoke so I changed the way that I spoke to try and sound less common." | A testimonial of passing motivated by class inference from speech |
| ST-Q8 | 27 | "Most students would excuse themselves immediately after hearing my accent to find friends elsewhere." | A testimonial of contact refused after an accent is heard |
| ST-Q9 | 27 | "I consciously minimised my Lancashire accent into a more standard English accent, hoping to be perceived as more intelligent." | A testimonial of passing aimed at a competence reading, after years of exclusion |
| ST-Q10 | 28 | "I feel that my accent has allowed people to more quickly judge me to be of lower class or wealth or not having attended a private school which for some people means they are no longer interested in speaking with you." | The full chain A1 models, in one testimonial: class inferred from the marker, then contact declined |
| ST-Q11 | 28 | "My accent is perceived as reasonably middle-class where I grew up (and I am reasonably middle-class), but at university I think some people perceived me as working class." | The same marker is read as a different class by a different population of observers |
| ST-Q12 | 33 | "our surveys suggest that the pattern may be due to older professionals having suppressed their non-RP accents to a greater extent in earlier decades, assimilating to the standard in order to avoid discrimination." | The authors' inference that older senior managers modified more. Formalised in our reading, with `control_not_identified` showing the figure does not identify it |
| ST-Q13 | 34 | "29% of respondents from lower socio-economic grades reported such experiences in the workplace, as against 22% from higher socio-economic grades. And 55% of senior professionals from lower socio-economic backgrounds report such experiences in social settings, as compared to 34% from higher socio-economic backgrounds." | Senior managers, by parental class. The gap is larger in social settings than in the workplace |
| ST-Q14 | 36 | "Relatedly, standard accent speakers feel much less pressure to modify their accents." | Pressure to modify falls on speakers of non-standard accents |
| ST-Q15 | 39 | "mock interview candidates who demonstrated technical expert knowledge in their response were subject to less accent-based differential rating" | Information about the individual's competence narrows the accent penalty. The study is Levon et al. (2021), not read |

## Lean results

`lean/mathlib/LevonEtAl2022.lean`. The report states no model. The file
formalises the definitions and identities its numbers rest on, and our
reading of one inference it draws.

| Theorem | What it checks |
|---|---|
| `LevonEtAl2022.agree_shares` | Figures 3 to 6. Strongly plus slightly agree gives the shares stated in the text, 24, 35, 23; 18, 33, 19; 29, 30, 25; 40, 47, 46 |
| `LevonEtAl2022.bars_sum` | Each bar of Figures 3 to 6 sums to 100 or, in four bars, 101 |
| `LevonEtAl2022.overall_is_weighted` | The overall 24% is the sample-weighted mean of 18%, 33% and 19% |
| `LevonEtAl2022.modification_lowers_reports` | With a fixed rate of mockery, more modification lowers the reported rate |
| `LevonEtAl2022.fig17_workplace_not_antitone` | Figure 17's workplace series rises from 19% to 20% between the two oldest groups |
| `LevonEtAl2022.control_not_identified` | Control. The fall from 52% to 23% in Figure 17 arises from no modification with falling mockery and from fixed mockery with rising modification |
| `LevonEtAl2022.control_unweighted_not_24` | Control. Without weighting by sample size the mean is below 23.5% |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| ST-D1 | The version read is the November 2022 report downloaded from `suttontrust.com/wp-content/uploads/2022/11/Accents-and-social-mobility.pdf` (PDF metadata, created 2 November 2022) | The contents page (PDF p. 3) carries production defects, page numbers "43" and "65" and "Error! Bookmark not defined.", so a corrected reissue may exist. It is not read |
| ST-D2 | The report's account of the Accent Bias Britain studies gives N = 821 for labels, N = 848 for real voices and a threshold of 45 years (PDF pp. 11 to 13), against N = 827, N = 1062 and 40 years in the 2020 report | The two reports by overlapping authors disagree. This record cites its own numbers only as this report's account. See AB-D2 |
| ST-D3 | "Employees" in Figures 3 to 6 are read as the early-career sample (N = 1014) | The overall 24% (PDF p. 16) is the sample-weighted mean with N = 511, 1029, 1014 (24.4%). Adding the senior sample gives 22.9%, which does not round to 24. `overall_is_weighted`, `control_unweighted_not_24` |
| ST-D4 | "the reverse of that observed for early career respondents in Figure 15" (PDF p. 32) is read as Figure 14 | Figure 14 is the early-career age figure. Figure 15 is senior managers by class |
| ST-D5 | "This age profile also holds" for Figure 17 (PDF p. 33) is read as holding for social settings at every step and for the workplace only between the youngest and the older groups | The workplace series is 27, 25, 19, 20. `fig17_workplace_not_antitone` |
| ST-D6 | Four bars of Figures 3 to 6 sum to 101 and are read as rounded | `bars_sum` |
| ST-D7 | The report says only one of five bias-reduction strategies had an effect (PDF p. 39) | The 2020 report says all interventions are beneficial (AB-D7). Neither gives estimates |
| ST-D8 | Accent modification is not a survey item. The evidence on it is testimonial, and the authors' inference from the age pattern (ST-Q12) | The three survey statements (PDF p. 15) ask about self-consciousness, career anxiety and having been mocked. The authors say the open-ended samples are not necessarily representative (PDF p. 15) |
| ST-D9 | The foreword (PDF p. 4), in which the chairman says he learned a Surrey accent to fit in, is not quoted | The text layer interleaves it with a pull quote set beside it |
| ST-D10 | Note 17 (PDF p. 13) gives the first author as "Lev-On E" and is read as Levon E | Notes 16 and 23 cite the same article as Levon et al. 2021 |
| ST-D11 | The contents (PDF p. 3) title the first life stage "Secondary school and sixth form", while the heading (PDF p. 20) reads "Secondary school and university applicants" | Read as the same section |
