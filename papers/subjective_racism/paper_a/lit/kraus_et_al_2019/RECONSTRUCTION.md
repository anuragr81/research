# Kraus, Torrez, Park and Ghayebi (2019), reconstruction

Michael W. Kraus, Brittany Torrez, Jun Won Park and Fariba Ghayebi, "Evidence
for the reproduction of social class in brief speech", *Proceedings of the
National Academy of Sciences* 116(46), 2019, pp. 22998–23003,
doi:10.1073/pnas.1900500116.

## Source read

- The PubMed Central HTML text of the published article (PMCID PMC6859342),
  cached at `~/.cache/paper_a/sources/kraus_2019.html`, sha256
  `c9470e6e…073f`, with its extracted text `kraus_2019.txt`, sha256
  `be676006…9967`. The text has no page numbers and no form feeds, so the
  suite treats it as one page and every quotation cites page 1 with its
  section heading (KT-D1).
- Read in full on 16 October 2026, covering the significance statement, abstract,
  introduction, results (studies 1 to 5), discussion, materials and methods,
  and the reference list. Evidence tag [F].
- The SI Appendix (`pnas.1900500116.sapp.pdf`) is linked from the page but is
  not in the cached text and is not read. Every result the main text sends to
  the SI Appendix (the perceiver-education correlation, the self-referencing
  correlations, the aggregate impression measure, stimulus durations) is known
  here only by the main text's one-line description (KT-D2).
- The figures are not in the text. Their captions are, and every number used
  below is stated in the running text.

## The studies, from their own primitives

Social class is "one's overall societal status ... measured by indices of
income, educational attainment, and occupation status". Accuracy is
benchmarked "against random chance variation", so "any minimal tendency of
perceivers to track the social class of speakers is considered accurate"
(KT-Q2).

1. **Study 1** (signal detection). 229 MTurk perceivers each hear 7 isolated
   words ("And", "From", "Thought", "Beautiful", "Imagine", "Yellow", "The")
   from 27 speakers drawn from the IDEA archive, and answer Yes or No to "Is
   this person a college graduate/woman/young/white?". Class is the binary of
   high school against college degree. Proportion correct is 55.49% for
   class, against 64.05% for race, 66.28% for age and 92.37% for gender, all
   above 50% (KT-Q3, KT-Q4). Class is the least accurate, which the authors
   attribute to the concealability of class (KT-Q5). Perceiver education
   correlates with accuracy for college-educated speakers, R(227) = 0.161.
2. **Study 2** (pronunciation against a digital standard). Three coders rate
   each syllable of "Beautiful", "Imagine", "Yellow" as matching or deviating
   from the pronunciation of online search and retail standards. In
   regressions with speaker age, gender, race and vocal pitch as controls,
   higher actual class goes with fewer deviations, β = 0.28, T(186) = 3.99,
   and so does higher perceived class from a separate panel of 568 perceivers
   (the panel of Kraus, Park and Tan 2017), β = 0.28, T(211) = 4.20. "No other
   effects in either model were significant" (KT-Q6).
3. **Study 3** (subjective ideal speech). 200 perceivers rate the same three
   words of 5 speakers (high and low class by occupation and education) on
   closeness to their own ideal English. Speaker class has F(1,198) = 613.40.
4. **Study 4** (speech against transcript). 302 perceivers hear, or read the
   transcript of, 90 s of self-description in 15 s steps and rate class on
   the 10-rung ladder. Accuracy is the profile correlation between rating and
   actual class. Speech gives R = 0.21, transcript R = −0.22 (KT-Q7), with
   speech accuracy significant from 30 s.
5. **Study 5** (hiring). 274 Prolific perceivers with hiring experience hear,
   or read, 20 New Haven job candidates' answer to "How would you describe
   yourself?", with explicit mentions of class removed. Perceived class
   separates high from low candidates, more for speech (5.50 against 4.55)
   than for text (4.97 against 4.36) (KT-Q8). From speech, higher-class
   candidates are judged more competent, a better fit, more similar to the
   perceiver, more hireable, and are offered a higher salary and bonus
   (KT-Q9).

## Derivation chain of what Paper A1 would use

- The accuracy of class reading is a proportion correct against a 50% line
  (study 1) or a correlation against zero (studies 2, 4). Proportion correct
  is `π H + (1 − π) CR` for base rate `π`, hit rate `H` and correct rejection
  rate `CR` (`pc`). A stimulus-blind responder scores exactly 1/2 when the set
  of speakers is balanced (`chance_half_balanced`) or the responder says Yes
  half the time (`chance_half_unbiased`), and can exceed 1/2 otherwise
  (`control_majority_beats_half`). The paper does not report the share of
  college graduates among the 27 speakers each perceiver heard (KT-D3).
- A perceiver who answers from the speaker's race alone scores above 50% when
  graduation is more common than not in one group and less common in the
  other (`race_judge_above_chance`). With a balanced set the excess is
  `2 w (1 − w)(p₁ − p₂)` (`race_judge_gain`), zero without a gap
  (`control_no_gap_chance`). These are our reading of the metric, not results
  of the paper. They show that above-chance class accuracy by itself does not
  separate a speech cue from inference from the group, which is why the
  study 2 controls for speaker race (KT-Q6) carry the weight.
- The composite measure of class averages z scores of the available
  indicators. Each z score and so the composite has sample mean zero
  (`mean_zscore`, `mean_composite`), which raw averages do not
  (`control_raw_average_not_centered`).
- Study 4's accuracy is a Pearson correlation, invariant to any increasing
  affine recoding of the ratings (`corr_affine_pos`) and reversed by a
  decreasing one (`control_corr_neg_scale`). The ladder's scale does not
  matter.
- The printed effect sizes follow from the printed `t` statistics by two
  conventions. Study 1 and study 4 use `2t/√df` (`study1_D`, `study4_D`);
  study 5 uses `t/√df` (`study5_D_*`). Under the one-sample convention study
  1's class accuracy has `d` under 0.49, not 0.97 (KT-D8). Study 5's hiring
  value does not reproduce (KT-D9).
- Correlations and their `t` statistics agree through `t² = r² df/(1 − r²)`
  for study 4 speech and the 30 s check (`study4_speech_t`, `study4_30s_t`)
  and for study 1's perceiver education (`study1_education_t`), and disagree
  for study 4 transcript (`study4_transcript_t_mismatch`, KT-D10).
- Hypothesis 3, read as class cues used as a proxy for merit. An increasing
  map from perceived class to judged competence favours the higher-class
  applicant with no information on qualifications (`proxy_bias`).

## What could not be reconstructed

- Nothing in the SI Appendix, which is not cached (KT-D2).
- The base rate of college graduates among study 1 stimuli, and so whether
  50% is the right chance line for that study (KT-D3).
- Every estimate from data. These are the study 2 regressions and ICC, the
  study 3 ANOVA, the study 4 linear trend, the study 5 `F` statistics,
  salaries, bonuses and `α`. No printed number among them follows from another.
- The regression coefficient on speaker race in study 2. The text says only
  that no control was significant (KT-Q6).
- Every `P` value, which needs the `t` or `F` distribution. The `t`
  statistics behind them are checked where they follow from printed numbers.
- The power target is checked only under the normal approximation with
  quantiles the paper does not print (`power_goal`).
- Hypothesis 2, which has no structure beyond its estimates.
- The sample counts disagree between the results and the methods for studies
  1, 2 and 4 (KT-D4, KT-D5).
