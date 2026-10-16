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
| KT-Q15 | 1 | "such that hiring managers may intentionally seek out these cues in applicants in ways that would be illegal if racial or gender cues were utilized in a similar fashion" | Discussion. A class cue substitutes for a criterion the law bans, the shadow-market mechanism of A1's venues |
| KT-Q16 | 1 | "these findings about fit and similarity suggest that cultural matching may be a second viable explanation of these results" | Results, study 5. The origin offered for preferring similar candidates is cultural matching, not budget compatibility |
| KT-Q17 | 1 | "constraining the ways in which informal interview settings can perpetuate inequalities by standardizing or avoiding the use of interviews is a domain of future inquiry ( 27 ), although these policy proposals would not eliminate the ways that bias enters into hiring practices at other stages of the process" | Discussion. A rule on the formal stage leaves the informal stages unreached |

## Lean results

`lean/mathlib/KrausEtAl2019.lean`. The paper states the metrics, the composite
and the printed statistics. The race-only perceiver and the proxy reading of
hypothesis 3 are our reading.

| Theorem | What it checks |
|---|---|
| `KrausEtAl2019.chance_half_balanced` | With a balanced set of speakers every stimulus-blind responder scores 1/2 |
| `KrausEtAl2019.chance_half_unbiased` | A stimulus-blind responder who says Yes half the time scores 1/2 at every base rate |
| `KrausEtAl2019.raceJudgePc_eq_pc` | The race-only perceiver's score is the proportion correct `pc` at its implied hit and correct rejection rates |
| `KrausEtAl2019.race_judge_above_chance` | A perceiver who answers from race alone is above 50% when graduation rates straddle one half across the groups |
| `KrausEtAl2019.race_judge_gain` | With a balanced set, that perceiver's excess over 1/2 is `2 w (1 − w)(p₁ − p₂)` |
| `KrausEtAl2019.mean_zscore` | Each z score has sample mean zero |
| `KrausEtAl2019.mean_composite` | The class composite, the average of z scores, has sample mean zero |
| `KrausEtAl2019.corr_affine_pos` | The profile correlation does not depend on the rating scale, under any increasing affine recoding |
| `KrausEtAl2019.study1_D` | Study 1, `D = 0.97` is `2t/√df` at `T(227) = 7.33` |
| `KrausEtAl2019.study1_d_one_sample` | Study 1, the one-sample `t/√n` is under 0.49 (KT-D8) |
| `KrausEtAl2019.study1_education_t` | Study 1, `R(227) = 0.161` gives `t` between 2.45 and 2.47 |
| `KrausEtAl2019.study4_D` | Study 4, `D = 0.44` is `2t/√df` at `T(282.51) = 3.69` |
| `KrausEtAl2019.study4_speech_t` | Study 4, `R = 0.21` gives `T(148) = 2.61` |
| `KrausEtAl2019.study4_transcript_t_mismatch` | Study 4, no `R` that rounds to 0.22 gives `T(148) = 2.64` (KT-D10) |
| `KrausEtAl2019.study4_30s_t` | Study 4, `R = 0.22` and `T(144) = 2.66` agree within rounding |
| `KrausEtAl2019.study5_D_competence` | Study 5, `D = 0.69` is `t/√df` at `T(273) = 11.32` |
| `KrausEtAl2019.study5_D_fit` | Study 5, `D = 0.66` at `T(273) = 10.9` |
| `KrausEtAl2019.study5_D_similar` | Study 5, `D = 0.69` at `T(273) = 11.43` |
| `KrausEtAl2019.study5_D_hire` | Study 5, `T(273) = 10.16` gives under 0.615, not the printed 0.62 (KT-D9) |
| `KrausEtAl2019.study5_D_salary` | Study 5, `D = 0.57` at `T(273) = 9.34` |
| `KrausEtAl2019.study5_D_bonus` | Study 5, `D = 0.48` at `T(273) = 7.97` |
| `KrausEtAl2019.study5_class_interaction` | Study 5, the speech gap in perceived class exceeds the transcript gap by 0.34 |
| `KrausEtAl2019.power_goal` | Methods, 100 per condition suffices for `D = 0.40` at 80% power under the normal approximation |
| `KrausEtAl2019.proxy_bias` | Hypothesis 3 in our reading. Competence judged by an increasing function of perceived class favours the higher-class applicant |
| `KrausEtAl2019.control_majority_beats_half` | Control. Without balance, always saying Yes beats 50% with no cue at all |
| `KrausEtAl2019.control_no_gap_chance` | Control. Without a gap in graduation rates the race-only perceiver is at chance |
| `KrausEtAl2019.control_raw_average_not_centered` | Control. Without z scores the raw average of indicators is not centred |
| `KrausEtAl2019.control_corr_neg_scale` | Control. A decreasing recoding flips the sign of the correlation |
| `KrausEtAl2019.control_no_proxy` | Control. A judgement that ignores class gives both applicants the same competence |

## Coverage of the source's results

The paper numbers no propositions. Its results are three hypotheses (the
introduction's "3 predictions"), the five studies' statistics, and the methods.

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Hypothesis 1, brief speech suffices for above-chance accuracy | `pc`, `chance_half_balanced`, `chance_half_unbiased`, `control_majority_beats_half` | special case | The metric and its chance line are formalised. That accuracy exceeds chance is a finding in data |
| Hypothesis 2, adherence to speech standards accounts for accuracy beyond content | none | not formalised | A regression and an experiment on coded speech, with no structure beyond the estimates |
| Hypothesis 3, class cues bias hiring as a proxy for merit | `proxy_bias`, `control_no_proxy` | special case | Our reading as a monotone proxy. The paper leaves open whether competence informs class or class informs competence |
| Study 1, proportion correct and the 50% line | `pc`, `blindPc`, `chance_half_balanced` | general | |
| Study 1, `D = 0.97` | `study1_D`, `study1_d_one_sample` | general | Arithmetic on printed numbers |
| Study 1, perceiver education `R(227) = 0.161`, `P = 0.015` | `study1_education_t` | special case | The `t` statistic only. The `P` value needs the `t` distribution, not in Mathlib in usable form |
| Study 1, accuracy for race, age, gender and the `T` contrasts | none | not formalised | Data |
| Study 2, `β = 0.28` (actual and perceived class), ICC = 0.32 | none | not formalised | Estimates from data. No printed number follows from another |
| Study 3, `F(1,198) = 613.40` and the null gender effects | none | not formalised | Data |
| Study 4, profile correlation as the accuracy measure | `corr`, `corr_affine_pos`, `control_corr_neg_scale` | general | |
| Study 4, `D = 0.44` | `study4_D` | general | |
| Study 4, speech `R = 0.21`, `T(148) = 2.61` | `study4_speech_t` | general | |
| Study 4, transcript `R = −0.22`, `T(148) = −2.64` | `study4_transcript_t_mismatch` | general | The two printed numbers disagree (KT-D10) |
| Study 4, 30 s `R = 0.22`, `T(144) = 2.66` | `study4_30s_t` | general | |
| Study 4, linear improvement `F(1,138) = 5.96` | none | not formalised | Data |
| Study 5, ANOVA on perceived class, means by condition | `study5_class_interaction` | special case | The difference in differences of the printed means. The `F` statistics are not derivable |
| Study 5, six effect sizes | `study5_D_competence`, `study5_D_fit`, `study5_D_similar`, `study5_D_hire`, `study5_D_salary`, `study5_D_bonus` | general | Hiring disagrees (KT-D9) |
| Study 5, salary and bonus means and SDs, `α = 0.967` | none | not formalised | Data (KT-D11) |
| Methods, the class composite | `zscore`, `composite`, `mean_zscore`, `mean_composite`, `control_raw_average_not_centered` | general | The SD used for the z scores is left free. Every theorem holds for any scale |
| Methods, 100 per condition for `D = 0.40` at 80% power | `power_goal` | special case | Normal approximation with the quantiles 1.96 and 0.8416, which the paper does not print |
| Our reading, the race-only perceiver | `raceJudgePc_eq_pc`, `race_judge_above_chance`, `race_judge_gain`, `control_no_gap_chance` | general | Not a result of the paper |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| KT-D1 | Every quotation cites page 1 and gives its section heading | The source is the PubMed Central HTML text, with no page numbers and no form feeds. The journal pages are 22998 to 23003, and no quotation is mapped to them |
| KT-D2 | Nothing is attributed to the SI Appendix beyond the main text's description of it | The SI PDF is linked from the page and not cached. KT-Q11 and KT-Q12 report SI results in the main text's words only |
| KT-D3 | The 50% line of study 1 is read as chance only for a balanced set of stimuli or a balanced responder | The paper does not report the share of college graduates among the 27 speakers per perceiver. `control_majority_beats_half` shows a stimulus-blind responder can exceed 50% otherwise |
| KT-D4 | Study 1 speakers are 27 per perceiver, drawn from the 189 of the 246 IDEA speakers who reported education | The results say "a larger sample of 189 speakers", study 2 says "the 246 speakers from the IDEA sample", and the methods give n = 246, of whom "189 reported their educational attainment" |
| KT-D5 | Study 4 speaker count is not fixed. The results say 35, the methods "50 speakers from the IDEA sample" | Both are in the text. No row relies on the count |
| KT-D6 | The study 1 test statistics have 227 degrees of freedom with 229 perceivers | A one-sample test on 229 perceivers would have 228. The text does not say whether a perceiver was dropped. No row relies on it |
| KT-D7 | The study 2 perceiver panel (568 Qualtrics perceivers, 7 words) is the panel of Kraus, Park and Tan (2017) | Study 2 cites its reference 10, Kraus, Park and Tan (2017), for the panel. The two records report the same panel, not independent evidence |
| KT-D8 | Study 1's `D = 0.97` uses the two-group conversion `2t/√df` on a one-sample test | `study1_D`. The one-sample `t/√n` gives under 0.49 (`study1_d_one_sample`). Study 5 uses `t/√df` for its paired tests, so the two studies' `D` are on different conventions. The class accuracy effect is about half the size its `D` suggests |
| KT-D9 | Study 5's hiring `D = 0.62` is read as 0.61 | `study5_D_hire`. The conversion that reproduces the five other study 5 values gives under 0.615 |
| KT-D10 | Study 4's transcript `R = −0.22` and `T(148) = −2.64` disagree | `study4_transcript_t_mismatch`. `T = −2.64` fits `R ≈ −0.212`, which rounds to −0.21. No row relies on the transcript value |
| KT-D11 | The salary and bonus SDs are both printed as $1,680 | Materials and Methods, study 5. On a $55,000–65,000 scale and a $0–10,000 scale an identical SD is possible but suggests a copying error. No row relies on it |
