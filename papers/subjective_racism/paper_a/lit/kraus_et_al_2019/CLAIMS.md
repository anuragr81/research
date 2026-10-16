# Kraus, Torrez, Park and Ghayebi (2019), claims

Evidence tag [F]: the PubMed Central HTML text of the PNAS article was read in
full on 16 October 2026. The text has no page numbers. Every quotation cites
page 1, the single page of the cached text, and the section heading is given in
the last column (KT-D1). The SI Appendix is not read (KT-D2).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| KT-Q1 | 1 | "We find that brief speech spoken out of context is sufficient to allow respondents to discern the social class of speakers at levels above chance accuracy" | Abstract. The paper's claim, read with KT-Q2 for what "accuracy" means |
| KT-Q2 | 1 | "In the studies we benchmark accuracy against random chance variation in social class perceptions. This means that any minimal tendency of perceivers to track the social class of speakers is considered accurate. Notably, this criteria for accuracy falls well short of error-free perceptions of social class." | Introduction. Class is read weakly from speech. A1's "unseen" is an idealisation of a noisy reading, not a contradiction of it |
| KT-Q3 | 1 | "perceivers accurately judged the social class of speakers (55.49%) based on brief speech spoken out of context T(227) = 7.33, P < 0.001, D = 0.97." | Results, Detecting Social Class in Brief Speech. The number A1 would cite for how accurately class is read from seven words |
| KT-Q4 | 1 | "social class judgments were significantly less accurate relative to speaker race (64.05%) T(227) = −8.63, P < 0.001, age (66.28%) T(227) = −10.92, P < 0.001, and gender judgments (92.37%)" | Results, Detecting Social Class in Brief Speech. The racial marker is read more accurately than class from the same words, the ordering A1 assumes (marker visible, class concealed) |
| KT-Q5 | 1 | "This lower accuracy of social class perception is perhaps due to the relatively concealable nature of social class, given that norms in the United States dictate concealing social class" | Results, Detecting Social Class in Brief Speech. The authors' explanation, offered as a conjecture |
| KT-Q6 | 1 | "perceiving speakers as higher in social class was also associated with fewer deviations from these digital standards β = 0.28, T(211) = 4.20, P < 0.001. No other effects in either model were significant" | Results, Speech Pronunciation Patterns as Cues of Social Class. Speaker race is a control and is not significant for perceived class given pronunciation. The study does not report race read as class |
| KT-Q7 | 1 | "The average correlation across all time points revealed a significant positive association between speaker social class and perceiver-judged social class in the speech condition R = 0.21, T(148) = 2.61, P = 0.010. In contrast, a negative association emerged in the transcript condition R = −0.22" | Results, study 4. Accuracy of class reading from 90 s of speech is a correlation of about 0.2 |
| KT-Q8 | 1 | "especially true when the perceivers listened to the speech of the applicants (M High = 5.50, SD High = 1.23; M Low = 4.55, SD Low = 1.17) versus read a transcript of what was said (M High = 4.97, SD High = 1.23; M Low = 4.36, SD Low = 1.29)" | Results, Reproduction of Social Class in Hiring Decisions. On a 10-rung ladder the high and low candidates are separated by under one rung |
| KT-Q9 | 1 | "perceivers with hiring experience judged higher-social-class applicants as more likely to be competent for the job T(273) = 11.32, P < 0.001, D = 0.69, a better fit for the job T(273) = 10.9, P < 0.001, D = 0.66, as more similar to the perceivers with hiring experience" | Results, study 5. A class reading feeds a decision. The setting is hiring, a vertical ranking, not contact |
| KT-Q10 | 1 | "Thus, class-relevant cues may be used as a means to perpetuate racial inequality, given the overlap between social class and race in America ( 24 ). Of course, the reverse is also a possibility, and future research that examines how cues of social class perpetuate these inequalities independent of and in tandem with racial stereotypes is an important topic of future inquiry" | Discussion. The link from race to class reading is named as future work, not studied |
| KT-Q11 | 1 | "In study 1 we found that more highly educated participants were more accurate in determining the social class of college- versus high school-educated speakers" | Discussion. Accuracy depends on the perceiver's own class, from the SI Appendix, not read |
| KT-Q12 | 1 | "Perceiver judgments of social class were positively correlated with their own social class" | Discussion. Self-referencing in class reading, from the SI Appendix, not read |
| KT-Q13 | 1 | "Of the speaker sample, 189 reported their educational attainment and were included as speaker stimuli in our study." | Materials and Methods, study 1. Read against the results' "larger sample of 189 speakers" (KT-D4) |
| KT-Q14 | 1 | "The speaker sample for study 4 again consisted of 50 speakers from the IDEA sample" | Materials and Methods, study 4. The results say 35 speakers (KT-D5) |

## Lean results

`lean/mathlib/KrausEtAl2019.lean`. The paper states the metric and the 50%
benchmark. The theorems are our reading of that metric.

| Theorem | What it checks |
|---|---|
| `KrausEtAl2019.chance_half_balanced` | With a balanced set of speakers every stimulus-blind responder scores 1/2 |
| `KrausEtAl2019.chance_half_unbiased` | A stimulus-blind responder who says Yes half the time scores 1/2 at every base rate |
| `KrausEtAl2019.raceJudgePc_eq_pc` | The race-only perceiver's score is the proportion correct `pc` at its implied hit and correct rejection rates |
| `KrausEtAl2019.race_judge_above_chance` | A perceiver who answers from race alone is above 50% when graduation rates straddle one half across the groups |
| `KrausEtAl2019.race_judge_gain` | With a balanced set, that perceiver's excess over 1/2 is `2 w (1 − w)(p₁ − p₂)` |
| `KrausEtAl2019.control_majority_beats_half` | Control. Without balance, always saying Yes beats 50% with no cue at all |
| `KrausEtAl2019.control_no_gap_chance` | Control. Without a gap in graduation rates the race-only perceiver is at chance |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| KT-D1 | Every quotation cites page 1 and gives its section heading | The source is the PubMed Central HTML text, with no page numbers and no form feeds. The journal pages are 22998 to 23003, and no quotation is mapped to them |
| KT-D2 | Nothing is attributed to the SI Appendix beyond the main text's description of it | The SI PDF is linked from the page and not cached. KT-Q11 and KT-Q12 report SI results in the main text's words only |
| KT-D3 | The 50% line of study 1 is read as chance only for a balanced set of stimuli or a balanced responder | The paper does not report the share of college graduates among the 27 speakers per perceiver. `control_majority_beats_half` shows a stimulus-blind responder can exceed 50% otherwise |
| KT-D4 | Study 1 speakers are 27 per perceiver, drawn from the 189 of the 246 IDEA speakers who reported education | The results say "a larger sample of 189 speakers", study 2 says "the 246 speakers from the IDEA sample", and the methods give n = 246 with 189 reporting education (KT-Q13) |
| KT-D5 | Study 4 speaker count is not fixed. The results say 35, the methods 50 (KT-Q14) | Both are in the text. No row relies on the count |
| KT-D6 | The study 1 test statistics have 227 degrees of freedom with 229 perceivers | A one-sample test on 229 perceivers would have 228. The text does not say whether a perceiver was dropped. No row relies on it |
| KT-D7 | The study 2 perceiver panel (568 Qualtrics perceivers, 7 words) is the panel of Kraus, Park and Tan (2017) | Study 2 cites its reference 10, Kraus, Park and Tan (2017), for the panel. The two records report the same panel, not independent evidence |
