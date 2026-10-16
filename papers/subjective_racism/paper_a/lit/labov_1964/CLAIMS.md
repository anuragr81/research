# Labov (1964), claims

Evidence tag [F]. The OCR text layer of the University Microfilms copy of the
Columbia Ph.D. dissertation was read in full, PDF pp. 1 to 646, on 16 October
2026, with the figure pages that carry no text read from images
(`RECONSTRUCTION.md`). Pages are PDF pages. The dissertation's own page numbers
are not used, because the copy carries two series of them (LV-D4). The cache file
is named `labov_1966`, but the version read is the 1964 dissertation, not the 1966
book (LV-D1).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| LV-Q1 | 88 | "if any two sub-groups of New York City speakers are ranked in a scale of social stratification, then they will be ranked in the same order by their [...] use of (r)." | The hypothesis of the department store study. The speech variable is ordered by the social ranking of groups. Formalised as `Stratified` |
| LV-Q2 | 98 | "62% of Saks employees used all or some (r-1), 51% of Macy's, and 20% of Klein's." | The department store result, the numbers A1 would cite. `store_stratified` |
| LV-Q3 | 112 | "the tendency of the store officials to select girls who speak in a certain way will play a part in the stratification of language, as well as the adjustment made by the employees to their situation." | A store screens its staff on speech. The screening is by the employer of employees, not by clients of each other |
| LV-Q4 | 237 | "This is equivalent to looking for the social significance of a speech pattern as we first hear it; when we meet a New Yorker for the first time, what can we infer about him from [...] the way he talks?" | The "second approach" reads class from speech. Our reading as Bayes' rule is `posterior_gt_prior` |
| LV-Q5 | 312 | "If we speak for some time to a New Yorker in careful conversation, and we note that he uses no stops or affricates for (dh), we can affirm that he has had some high school training, and that he is not likely to be a manual worker— in fact, the chances are about even that he is a professional." | The inference is stated as a probability over class given a speech frequency, from the author's own sample, not from listeners |
| LV-Q6 | 432 | "to imagine himself in the position of a personnel manager, interviewing people for a large corporation. [...] A mark across the scale at a certain job meant that the person speaking could hold that job, as far as her speech was concerned, and all those jobs listed below, but none of the jobs above." | The subjective reaction test asks for the highest job the speech would allow, on one ordered scale |
| LV-Q7 | 434 | "It was also explained that the listener was not trying to judge what job the speaker actually held" | The test does not ask listeners to infer the speaker's class or job. It measures evaluation of the speech |
| LV-Q8 | 437 | "We may say that the middle class stigmatizes the speech of all but the most cultured speaker, and the other classes do not penalize these speakers to the same extent." | The listener's own class changes how hard a speaker is penalised, on a scale all classes order alike |
| LV-Q9 | 454 | "The zero cell of this table represents an absolute regularity which illustrates the uniformity of the New York City speech community on the plane of normative evaluation." | Every respondent aged 18 to 39 rated the prestige form up. The evaluation is shared across classes |
| LV-Q10 | 462 | "the pressure towards identification with a particular ethnic, neighborhood, or occupational group and [2] the need to conform to the over-all hierarchy of values imposed by the community." | Two pressures, one toward one's own group and one toward a common ranking. The test measures only the second |
| LV-Q11 | 474 | "The number of items in which these two reports are different forms the index of linguistic insecurity. [...] In this measure of linguistic insecurity, the lower middle class outdistances all others." | The index of linguistic insecurity and its result. `ili`, `ili_eq_zero_iff` |
| LV-Q12 | 540 | "we have used the term hypercorrection to describe the lower middle class tendency to outdo the upper middle class in" | The definition of hypercorrection adopted here (LV-D6). The sentence ends on PDF p. 541 |
| LV-Q13 | 146 | "Not one speaker in the sample who was raised in New York City was able to use 100% (r-1) in conversation, and this includes a great many speakers who were consciously aiming in that direction after (r) had been discussed." | Adopting the prestige marker is partial and fails under conscious effort. A limit on passing by speech |
| LV-Q14 | 280 | "it is evident that some sections of upper middle class society cannot tolerate a speaker who uses such a high [...] percentage of socially stigmatized forms." | A class 9 Ph.D. was refused an academic post for his speech. The marker is penalised although his class is known |
| LV-Q15 | 554 | "The degree of predictability within the structures that have been studied extends to small groups of utterances and small groups of speakers, but not to the individual utterance nor the individual speaker." | The speech variables locate groups, not individuals. A single speaker's class is read from them only with error |

## Lean results

`lean/mathlib/Labov1964.lean`. The file formalises our reading of the structures
the dissertation states, not the dissertation's data.

| Theorem | What it checks |
|---|---|
| `Labov1964.rIndex_mem` | PDF p. 75. The (r) index, the percentage of (r-1) among determinate tokens, lies in [0, 100] |
| `Labov1964.rIndex_ends` | Only (r-1) gives (r)-100, only (r-0) gives (r)-00 |
| `Labov1964.thIndex_eq` | PDF p. 80. The (th) index, 100 times the average value less 1, equals `100 (b + 2c)/(a + b + c)` |
| `Labov1964.thIndex_ends` | Only fricatives give (th)-00, only stops give (th)-200 |
| `Labov1964.thIndex_mem` | Every (th) index lies in [0, 200] |
| `Labov1964.ehIndex_examples` | PDF p. 77. Half (eh-3) and half (eh-2) gives (eh)-25, and (eh-4) throughout gives (eh)-40 |
| `Labov1964.sec_le_nine` | PDF pp. 243 to 244. The MFY index, three ranks I to IV with equal weight, runs from 0 to 9 |
| `Labov1964.sec_extremes_uniform` | Classes 0 and 9 each have one configuration of occupation, education and income |
| `Labov1964.sec_twelve_configurations` | Classes 4 and 5 each have twelve configurations, as PDF p. 244 states |
| `Labov1964.control_sec_middle_not_uniform` | Control. A middle class does not determine its configuration |
| `Labov1964.ili_le` | PDF p. 177. The index of linguistic insecurity is at most the number of words |
| `Labov1964.ili_eq_zero_iff` | The index is zero exactly when the reported usage is always the form called correct |
| `Labov1964.store_stratified` | LV-Q1, LV-Q2. The (r) percentages are ordered as the stores' prestige |
| `Labov1964.control_wage_order_not_stratified` | Control. Under the wage order of PDF p. 92, Saks below Macy's, the ordering fails |
| `Labov1964.fig3_deviation` | PDF p. 260. Nine deviant relations of thirty give a deviation of .30, below .50 |
| `Labov1964.crossover_breaks_order` | A cross-over of two class lines means neither class is above the other at every style |
| `Labov1964.r_crossover` | PDF pp. 269 to 271. Lower middle class (r)-04 below class 9's (r)-18 in casual speech, above it in isolated words |
| `Labov1964.control_regular_no_crossover` | Control. A class below another at every style has no cross-over |
| `Labov1964.posterior_gt_prior` | Our reading of LV-Q4 and LV-Q5. A form used more by the higher class raises the posterior on that class |
| `Labov1964.control_posterior_eq_prior_unstratified` | Control. A form used equally by both classes leaves the posterior at the prior |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| LV-D1 | The version read is the Columbia Ph.D. dissertation, title page dated 1964, University Microfilms 67-9411, and the record is `labov_1964` with key `Labov1964`. The 1966 book of the Center for Applied Linguistics is not read | The cached file is named `labov_1966` and `notes/motivation_scan.md` cites "Labov (1966) [F]", but PDF pp. 1 and 2 give 1964. The two versions are not assumed to coincide in wording or page |
| LV-D2 | Quotations are elided where a line ends in a soft hyphen (U+00AD) | The OCR keeps the soft hyphen, which the suite's normalisation does not remove, so a word broken at a line end cannot be matched whole |
| LV-D3 | OCR errors are not quoted: "o£" for "of" (p. 98), "(r)-Q4" for "(r)-04" (p. 270), "D*" for "D'" (pp. 270 and 374), "axe" for "are" (p. 428), "ashed" for "asked" (p. 432) | Each was read in context. Quotations avoid or elide them, and the reconstruction uses the corrected values |
| LV-D4 | Pages are PDF pages. Each page carries a typed and a stamped number that differ, and from Chapter XII the printed numbers jump (PDF p. 482 is printed 507, while the contents give Chapter XII at p. 459) | The PDF page is the only unambiguous locator |
| LV-D5 | PDF p. 435 gives the subjective reaction sample as "122 native New York informants: 85 adults and 49 children", which sums to 134. No row relies on the count | An inconsistency in the source |
| LV-D6 | Hypercorrection is read in the sense of LV-Q12, the lower middle class outdoing the upper middle class in prestige forms in formal styles | The dissertation also uses the word for women's wider stylistic shift (PDF p. 339), for overcompensation against a stigmatised foreign pronunciation (PDF p. 344), and "in a more general sense as the mechanism of change" (PDF p. 541) |
| LV-D7 | The subjective reaction test is read as an evaluation of speech on a scale of occupational suitability, not as listeners inferring the speaker's class | LV-Q6 and LV-Q7, and the instructions on PDF p. 590, "you wouldn't hire them on their speech alone" |
| LV-D8 | The glossary refers the "array of relations" to Chapter VIII, while the definition is in Chapter VII (PDF p. 258) | A cross-reference error in the glossary (PDF p. 569) |
| LV-D9 | The (r) value of class 9 in isolated words is not given in the text. `r_crossover` takes the lower middle class lead there as a hypothesis | PDF pp. 269 and 374 state the lead in words, and Figure 11 (PDF p. 268, read as an image) shows it without printed values |
