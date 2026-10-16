# Levon, Sharma, Watt and Perry (2020), claims

Evidence tag [F]: the project report of February 2020, all 28 PDF pages, was
read in full on 16 October 2026 from the text layer, with page images of PDF
pages 2, 17 to 21, 26 and 27 for the figures and the blank pages. Pages below
are PDF pages. The printed page is the PDF page minus 2.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| AB-Q1 | 7 | "accents in the UK reflect differences in what region people come from, their family's social class background, their age, and their social networks." | Accent is read as family class among other things. The marker carries region, age and networks as well as class |
| AB-Q2 | 8 | "we all have automatic associations with accents, particularly as accents often trigger social stereotypes relating to specific regions, cultures, ages, genders, and social classes." | The authors' account of how a listener reads an accent, by stereotype of the speaker's group |
| AB-Q3 | 8 | "A person's accent reflects their social background, not their intelligence or expertise, but because accents are often linked to ethnicity, social class, and other" | The authors' premise that accent carries no information about competence. Rating competence by accent is a proxy, not an inference from the individual |
| AB-Q4 | 9 | "accent often becomes a proxy for other forms of discrimination that are prohibited." | Accent stands in for class and ethnicity, neither of which the report separates from the other |
| AB-Q5 | 5 | "People evaluated job candidates who spoke in a Received Pronunciation (RP) accent as more informed and more suitable for professional employment, even when speakers of other accents gave identical answers." | The design holds the answer fixed and varies the accent. Formalised in our reading as `identical_answer_penalty` |
| AB-Q6 | 17 | "These accents correspond to working class social groups, often based in industrial towns, and ethnic minority groups." | The accents at the bottom of the label ranking are working-class and ethnic-minority accents |
| AB-Q7 | 19 | "Expert content improves the judgement of all accents." | Expert content raises every accent's rating. It is a level effect on all accents, not by itself a narrowing of the gap |
| AB-Q8 | 19 | "The expertise effect is not strong enough to override all bias. [...] This bias was particularly prevalent among respondents from a higher social-class." | The penalty survives expert content for older Southern listeners, and is larger among listeners of higher class. The penalty depends on the listener's own class |
| AB-Q9 | 21 | "it is only those listeners with low levels of MCPR who show a dispreference for EE and/or MLE." | The penalty among older Southern and Midlands listeners is confined to those with low motivation to control prejudice |
| AB-Q10 | 22 | "lawyers and law firm recruiters showed a consistent ability to rate responses according to whether the response was of marginally higher or lower quality, regardless of accent." | Professional raters rate by the answer alone. Formalised in our reading as `quality_only_no_penalty` |
| AB-Q11 | 22 | "received by far the lowest individual ratings of any accent when giving high quality responses. This might suggest pockets of strong bias against these accents." | Among lawyers EE and MLE still receive the lowest single ratings for high-quality answers |
| AB-Q12 | 22 | "This slightly lower rating of RP may indicate high status but low solidarity associations or a higher expectation of quality answers for RP speakers" | Lawyers rate RP marginally lower on likeability. The authors offer two readings and test neither |
| AB-Q13 | 23 | "We do not interpret it as an absence of bias altogether, but rather a likely effect of professionalism and heightened diversity awareness and training in top tier law firms." | The authors do not read the lawyers' result as an absence of the association between accent and class |
| AB-Q14 | 25 | "We therefore cannot conclude that accent bias does not play a role in the overall hiring process." | The scope. The recruiter study covers only the rating of answer content |
| AB-Q15 | 18 | "they showed some similar accent biases, but the differences were much smaller." | Real voices give smaller gaps than labels. `control_signal_can_widen_gap` shows this is an empirical finding and not implied by a Bayesian reading |
| AB-Q16 | 8 | "Unlike ethnicity, social class, or race, accent is not a protected characteristic under the Equality Act 2010. As such, employers can legally discriminate against candidates based on their accent." | Accent is a marker correlated with protected groups that the law does not reach, so screening on it is lawful. The report lists social class as protected, which the Sutton Trust report contradicts (AB-D10) |
| AB-Q17 | 25 | "It does not provide information on the potential for accent or language bias in other aspects of professional life, e.g. informal interaction during the interview, post-hiring experiences of candidates from non-traditional backgrounds, or progression through more senior career stages." | The recruiter study covers formal rating of content only. Informal interaction, the contact channel, is outside it |
| AB-Q18 | 18 | "So it is more likely that the pattern in figure 2 means that people's attitudes about accents conform increasingly to established norms as they enter the workforce and get older." | The life-stage reading of the age pattern, formalised against a cohort reading as `cohort_sections_differ` |

## Lean results

`lean/mathlib/LevonEtAl2020.lean`. The report states no model. The file
formalises the printed numbers that follow from others and our reading of
every structural claim the report makes about listeners.

| Theorem | What it checks |
|---|---|
| `LevonEtAl2020.voices_sample` | 890 + 51 + 90 + 31 = 1062, the real-voice sample by nation |
| `LevonEtAl2020.voices_design` | Two speakers of each of five accents make the ten clips |
| `LevonEtAl2020.identical_answer_penalty` | With a prior gap in favour of RP and an answer that does not reveal competence, the RP speaker is rated higher for the same good answer |
| `LevonEtAl2020.revealing_no_penalty` | An answer that reveals competence gives a zero penalty for every accent with a positive prior |
| `LevonEtAl2020.uninformative_is_label` | An uninformative answer leaves the rating at the prior, the reading of the label survey |
| `LevonEtAl2020.expert_raises_all` | An answer more likely from a competent speaker raises the rating at every prior, so expert content raises every accent |
| `LevonEtAl2020.quality_only_no_penalty` | A rating that depends on the answer alone gives every accent the same rating, the lawyers' reading |
| `LevonEtAl2020.rating_penalty` | With weight `γ` on content, the penalty for an identical answer is `(1 − γ)` times the stereotype gap |
| `LevonEtAl2020.penalty_antitone_in_control` | More attention to content gives a smaller penalty, the reading of the awareness intervention and of MCPR |
| `LevonEtAl2020.full_control_no_penalty` | Full attention to content gives no penalty |
| `LevonEtAl2020.cohort_shifts` | Under a cohort model a later cross-section is the earlier one shifted in age |
| `LevonEtAl2020.cohort_sections_differ` | Under a strictly monotone cohort profile two cross-sections years apart differ at every age, so the same pattern in 2007 and 2019 favours the life-stage reading |
| `LevonEtAl2020.higher_expectation_lower_rating` | A higher expectation for RP rates the same answer lower in RP, one of the two readings of the lawyers' likeability ratings |
| `LevonEtAl2020.control_partial_signal` | Control. Without full revelation the penalty on a good answer is positive |
| `LevonEtAl2020.control_signal_can_widen_gap` | Control. An informative answer can widen the gap beyond the prior gap, so the smaller gap with real voices is not implied by the reading |
| `LevonEtAl2020.control_additive_accent_effect` | Control. Without the answer-only condition an additive accent effect survives at every answer quality |
| `LevonEtAl2020.control_partial_control` | Control. At any weight on content below 1 the penalty is positive |
| `LevonEtAl2020.control_flat_cohort` | Control. A flat cohort profile gives identical cross-sections, so the life-stage inference needs cohorts that differ |
| `LevonEtAl2020.control_equal_expectation` | Control. With equal expectations the same answer is not rated lower in RP |

## Coverage of the source's results

The report numbers no results. Each row is a finding or structural claim as
the report states it, by PDF page.

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Hierarchy of labels, stable over fifty years (pp. 5, 17, 18) | none | not formalised | No values are printed and the figure axes cannot be read (AB-D3) |
| Real-voice sample by nation (p. 11) | `voices_sample` | general | |
| Two speakers per accent, ten clips (pp. 12, 13) | `voices_design` | general | |
| Identical answers rated higher in RP (p. 5) | `identical_answer_penalty` | special case | Our reading, with two competence types |
| Smaller gaps with real voices than with labels (p. 18) | `uninformative_is_label`, `control_signal_can_widen_gap` | general | The reading shows the finding is empirical, not implied |
| Age pattern read as life stage, not change over time (p. 18) | `cohort_shifts`, `cohort_sections_differ` | general | |
| Expert content improves all accents (p. 19) | `expert_raises_all` | special case | Two competence types |
| Expertise does not override bias for older Southern listeners (p. 19) | `identical_answer_penalty` | special case | Two competence types |
| Bias more prevalent among listeners of higher class (p. 19) | none | not formalised | A finding with no stated structure or numbers |
| High MCPR raises all ratings and removes the dispreference (pp. 20, 21) | `rating_penalty`, `full_control_no_penalty` | general | Our reading of control over attention |
| Lawyers rate by quality regardless of accent (p. 22) | `quality_only_no_penalty`, `full_control_no_penalty` | general | |
| EE and MLE get the lowest single ratings among lawyers (p. 22) | none | not formalised | Read from Figure 6, no numbers printed |
| Lawyers rate RP lower on likeability, two readings (p. 22) | `higher_expectation_lower_rating` | special case | The expectation reading only. The solidarity reading has no stated structure |
| Raising awareness narrows the gap most (pp. 23, 24) | `penalty_antitone_in_control` | special case | Narrowing is formalised. The ranking of the five interventions has no printed estimates |
| Accent is not protected by the Equality Act (p. 8) | none | not formalised | A legal statement. The two reports disagree on social class (AB-D10) |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| AB-D1 | The version read is the project report of February 2020, downloaded from `accentbiasbritain.org/wp-content/uploads/2020/02/Accent-Bias-Report-2020.pdf` (PDF metadata 26 February 2020). It is cited as the report, not as the journal articles | The Sutton Trust report (2022, its note 19) cites a file at `.../2020/03/Accent-Bias-Britain-Report-2020.pdf`, which is not read and is not assumed identical. The journal articles on the same studies (Sharma et al. 2019, Levon et al. 2021, Sharma, Levon and Ye 2022) are not read |
| AB-D2 | The report's own sample sizes and age threshold are used, N = 827 for the labels, N = 1062 for the real voices, and "over 40 years of age" (PDF pp. 11, 19) | The Sutton Trust report gives N = 821 and N = 848 and "over 45" for what it describes as the same studies (its PDF pp. 11 to 13), citing the 2021 journal article. The difference may come from exclusions in the article and is not resolved here |
| AB-D3 | The values in Figures 1 to 6 are not read as numbers, and no row relies on them | Figure 1 labels its vertical axis 0, 3, 4, 5, 6 at equal spacing, and Figure 5 labels it 7, 6, 6, 5, so the scales cannot be read exactly. Figures 1 to 5 have no error bars |
| AB-D4 | Claims of statistical significance are the authors', and cannot be checked from the report | The report gives no coefficients, standard errors, test statistics or model specifications |
| AB-D5 | "Older speakers generally rate all five accents less positively" (PDF p. 18) is read as older listeners | The figure plots ratings against the listeners' age, and the next sentence speaks of young people rating accents |
| AB-D6 | The intervention study's five scales are read as those of the real-voice study (PDF p. 13), not of the label survey | PDF p. 15 says "the same five evaluation scales used in the accent label survey", but the label survey rated only prestige and pleasantness (PDF p. 11) |
| AB-D7 | The report's statement that "All interventions are beneficial" (PDF p. 24) is recorded against the Sutton Trust's statement that only one of five strategies had an effect (Sutton Trust 2022, PDF p. 39) | The two reports by overlapping authors disagree. Neither gives the estimates |
| AB-D8 | The class associations of RP and MLE are read from the page image of PDF p. 7, not quoted | The two boxes are set in parallel columns that the text layer interleaves line by line |
| AB-D9 | Pages 2, 26 and 27 are blank or carry only the logo, and PDF p. 28 is the back cover | Page images. The cached text has 29 segments for 28 pages, the last one empty |
| AB-D10 | The statement that social class is a protected characteristic under the Equality Act 2010 (PDF p. 8, AB-Q16) is not relied on. Only the part on accent is | The Sutton Trust report by overlapping authors says "accent, alongside social class, is not protected" (ST-Q17). The Act itself is not read |
