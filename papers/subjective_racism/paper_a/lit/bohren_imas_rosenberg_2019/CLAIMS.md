# Bohren, Imas and Rosenberg (2019), claims

Evidence tag [F]. The working paper of January 2019 was read in full from its
text layer on 16 October 2026, all 83 PDF pages, with page images read for
PDF pages 26 and 55. Pages are PDF pages. The printed page is the PDF page
minus 1 (BIR-D1).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| BIR-Q1 | 2 | "If the source is belief-based – for example, the quality of output is imperfectly observed and evaluators believe that on average, men have higher abilities than women – then observing prior evaluations will reduce discrimination against women, relative to men with similar evaluations." | Belief-based discrimination falls as individual evidence accumulates |
| BIR-Q2 | 3 | "if discrimination is caused by a taste or preference against rewarding or interacting with women (Becker 1957), then a woman who receives a similar sequence of evaluations to a man will continue to face discrimination in future periods." | The rival prediction. A taste penalty survives individual evidence |
| BIR-Q3 | 11 | "We allow for the possibility that the evaluator has a misspecified model of the relationship between gender and ability, in that the evaluator's subjective belief may differ from the true population average ability" | Inaccurate group beliefs are a primitive, `μ̂_g ≠ μ_g` |
| BIR-Q4 | 13 | "In contrast to partiality, which is a property of the primitives of the model (preferences, beliefs), discrimination is a property of behavior." | The separation of source (partiality) from behaviour (discrimination) |
| BIR-Q5 | 15 | "the discrimination literature has tended to classify discrimination driven by distorted stereotypes as taste-based." | Biased beliefs are classed with beliefs here, not with tastes. A1 must say where its class channel sits |
| BIR-Q6 | 17 | "It is not possible to identify the source of discrimination from observing initial evaluations at a single set of parameters." | Observational equivalence at one precision, `BohrenEtAl2019.fn9_equivalence` |
| BIR-Q7 | 17 | "As the signal becomes perfectly objective, τη → ∞, there is initial discrimination if and only if the evaluator has preference-based partiality." | Proposition 1, `BohrenEtAl2019.prop1_limit` and `BohrenEtAl2019.prop1_decreasing` |
| BIR-Q8 | 18 | "In the limit, when quality is perfectly observable, differences in beliefs about ability do not lead to discrimination" | The form of A1's contribution 4. The belief-based penalty vanishes when the unseen trait is shown |
| BIR-Q9 | 18 | "Even if quality is perfectly observable, the female workers will still face discrimination." | The taste penalty persists, `BohrenEtAl2019.control_taste_persists` |
| BIR-Q10 | 19 | "Suppose there is a single type of evaluator with belief-based partiality and no preference-based partiality. Then fixing an evaluation history, discrimination decreases across periods but never reverses." | Proposition 2, `BohrenEtAl2019.prop2` |
| BIR-Q11 | 19 | "Therefore, observing a reversal is indicative of some form of misspecification" | A reversal separates biased from correct belief-based discrimination |
| BIR-Q12 | 21 | "the impartial type's posterior belief about average ability immediately favors females" | The step behind Proposition 3, `BohrenEtAl2019.control_reversal_misspecified` |
| BIR-Q13 | 25 | "If the variance of ability differs for females and males, then discrimination may be non-monotonic in the signal of quality." | A group's variance enters only as an alternative model, through the slope of the evaluation, not as a penalty |
| BIR-Q14 | 33 | "These differences correspond to roughly 0.4 standard deviations of the average change in reputation and average number of votes." | Size of the novice gap against female questions (Table 1, columns 3 and 4) |
| BIR-Q15 | 35 | "These differences in evaluation correspond to roughly 0.6 standard deviations for both ∆Rep and net votes." | Size of the reversed gap for advanced accounts (Table 2, columns 1 and 2) |
| BIR-Q16 | 12 | "This would result in partiality at the individual level, in that some evaluators are partial towards men and others are partial towards women, but no aggregate partiality." | Footnote 5. Partiality can run in either direction across evaluators, but the direction is not tied to the evaluator's own group |
| BIR-Q17 | 16 | "Target groups anticipate greater scope for discrimination when judgment is more subjective, and in response, generate output with more objective assessment criteria (Parsons et al. 2011)." | Belief-based discrimination concentrates where judgement is subjective, and the target group moves to objective channels |
| BIR-Q18 | 42 | "campaigns that aim to correct initial beliefs will improve choices along both dimensions, as will designing more objective measures of quality." | The policy reading. Objective measures remove the belief-based penalty |

## Lean results

`lean/mathlib/BohrenEtAl2019.lean`, in the paper's notation, for the closed
forms of the normal model.

| Theorem | What it checks |
|---|---|
| `BohrenEtAl2019.eq5` | Eq. (5) from eq. (4). Initial discrimination is `τq/(τq + τη)(μ̂_M − μ̂_F) + c_F` and does not depend on the signal |
| `BohrenEtAl2019.prop1_decreasing` | Proposition 1. With `μ̂_F < μ̂_M`, initial discrimination is strictly decreasing in `τη` |
| `BohrenEtAl2019.prop1_constant` | Proposition 1. Without belief-based partiality, initial discrimination is `c_F` at every precision |
| `BohrenEtAl2019.prop1_limit` | Proposition 1. As `τη → ∞`, initial discrimination tends to `c_F` |
| `BohrenEtAl2019.fn9_equivalence` | Footnote 9. A belief gap and the matching taste give the same evaluation of every woman |
| `BohrenEtAl2019.eq12` | Eq. (12). The gap in posterior means after a common evaluation is the prior gap times the factor `k` |
| `BohrenEtAl2019.k_eq` | Our closed form of the factor, `k = τa/(τa + τε)` (BIR-D6) |
| `BohrenEtAl2019.k_mem` | Eq. (13). The factor lies in `(0, 1)` |
| `BohrenEtAl2019.lemma2` | Lemma 2. The gap stays positive and shrinks |
| `BohrenEtAl2019.prop2` | Proposition 2. Along any history the gap stays positive and strictly decreases |
| `BohrenEtAl2019.control_taste_persists` | Control. With `c_F > 0` and no belief gap, discrimination is positive at every precision, so the vanishing of `prop1_limit` needs `c_F = 0` |
| `BohrenEtAl2019.control_reversal_misspecified` | Control. An impartial evaluator who attributes a woman's evaluation to a heuristic evaluator ends with a higher posterior mean for her, so the no-reversal of `prop2` needs a single type with common knowledge |
| `prefPartial`, `beliefPartial`, `unbiased`, `againstWomen`, `againstMen`, `reversal` (definitions) | Definitions 1 to 4 |
| `BohrenEtAl2019.eq1_expected_loss` | Eq. (1). The expected loss is the squared distance of the report from `m − c` plus the posterior variance |
| `BohrenEtAl2019.eq2_optimal` | Eq. (2). The report `m − c` is optimal |
| `BohrenEtAl2019.tq_inv` | `1/τq = 1/τa + 1/τε` |
| `BohrenEtAl2019.normal_update` | The normal update behind eq. (4), as an identity of log kernels |
| `BohrenEtAl2019.agg_initial` | Section 2.3, aggregate initial discrimination over a finite type set |
| `BohrenEtAl2019.heterogeneous_taste` | Section 2.5.2. With correct beliefs, aggregate initial discrimination does not depend on `τη` beyond the belief term |
| `BohrenEtAl2019.eq7` | Eq. (7) |
| `BohrenEtAl2019.invert` | Eq. (9) inverted, the signal revealed by a report |
| `BohrenEtAl2019.lemma1` | Lemma 1, eqs. (10) and (11), the closed form of the recursion |
| `BohrenEtAl2019.lemma3` | Lemma 3. A discrimination reversal is a belief reversal |
| `BohrenEtAl2019.prop2_D_decreases` | Proposition 2, that discrimination decreases, which the paper leaves at a display |
| `BohrenEtAl2019.prop3_p_zero` | Proposition 3, no discrimination at `p = 0` |
| `BohrenEtAl2019.prop3_signal_order` | Proof of Proposition 3, `s¹₁(v₁) > s²₁(v₁)` |
| `BohrenEtAl2019.coarse_upvote_iff` | Appendix A.3.1. The upvote rule is a signal cut-off |
| `BohrenEtAl2019.coarse_cut_mono` | Appendix A.3.1. The cut-off rises in the taste and falls in the prior mean |
| `BohrenEtAl2019.coarse_cut_limit` | Appendix A.3.1. The cut-off tends to `c_g` as `τη → ∞` |
| `BohrenEtAl2019.lemma4_tau_x` | Lemma 4, the step `τx ≥ 1` |
| `BohrenEtAl2019.shifting_cancels` | Eq. (27). The benchmark cancels from discrimination |
| `BohrenEtAl2019.phi_strictMono` | Appendix A.2.2. `φ(p)` is increasing |
| `BohrenEtAl2019.binary_signal` | Appendix A.2.2, the inversion of eq. (19) |
| `BohrenEtAl2019.stereo_sum` | Eq. (30). The stereotype is a distribution |
| `BohrenEtAl2019.stereo_zero` | Eq. (30). At `θ = 0` it is the true distribution |
| `BohrenEtAl2019.saturated` | Tables 1 to 6. The saturated dummy model and its cell means |
| `BohrenEtAl2019.tables_exact` | Tables 1, 2, 5 and 6. Pooled columns reproduce the single-sample columns |
| `BohrenEtAl2019.tables_rounding` | Tables 2, 3, 4 and 6. Four constants differ in the last digit (BIR-D9) |
| `BohrenEtAl2019.control_D_rises_without_shrinking` | Control. Without the shrinking factor of Lemma 2 discrimination would rise |
| `BohrenEtAl2019.control_coarse_taste` | Control. A taste keeps the woman's cut-off above the man's at every precision |
| `BohrenEtAl2019.control_stereo_distorts` | Control. At `θ = 1` the stereotype differs from the true distribution |
| `BohrenEtAl2019.control_shifting_difference_depends_on_v` | BIR-D8. The difference of required signals across histories depends on `v` |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| BIR-D1 | The version read is the working paper of January 2019, not the 2019 AER article. Pages are PDF pages, the printed page is the PDF page minus 1, and the text file has an 84th, empty page | The suite pins the working paper. Rows citing this record cite it as the working paper |
| BIR-D2 | PDF p. 26 states prediction (ii) of the heterogeneous-taste case as "when judgement is objective, τη ≈ 0". By Section 2.1 (PDF p. 11) lower `τη` means more subjective judgement, so objective judgement is large `τη`. We read "τη → ∞" | Page image of PDF p. 26 confirms the printed `τη ≈ 0`. The argument that follows needs only that `D` does not depend on `τη`, so it is unaffected |
| BIR-D3 | PDF p. 66 writes the shifting-standards payoff as `(v − (q − c(r) − c_g))²`, without the minus sign of the payoff on PDF p. 11. We read it as `−(v − (q − c(r) − c_g))²` | Eq. (27) is the maximiser of the negative of the square |
| BIR-D4 | PDF p. 15 reads "because of course categorization of experiences". We read "coarse" | The cited model is Fryer and Jackson (2008), whose subject is coarse categories |
| BIR-D5 | PDF p. 10 assumes `τε > 1`. No step read uses it, and the Lean results need only `τε > 0` | Lemmas 1 to 3 and eq. (13) hold for all positive precisions |
| BIR-D6 | The factor of eq. (12) simplifies to `τa(t)/(τa(t) + τε)`, so the signal precision `τη` cancels. The paper shows only that the factor lies in `(0, 1)` | `BohrenEtAl2019.k_eq`. Not a discrepancy. The closed form is ours |
| BIR-D7 | Proposition 1 speaks of initial discrimination "decreasing" in `τη`, and its proof of `|D(h₁, s₁)|`. `prop1_decreasing` is stated for `D` with `μ̂_F < μ̂_M`, the case the experiment tests | With a taste against women and a belief gap favouring them, `D` could pass through zero. That case is not relied on |
| BIR-D8 | After Definition 6 (PDF p. 67) the paper says that `s(v, h₂, g) − s(v, h₁, g)` does not depend on `v`. Under eq. (27) the required signal is `(τq,t + τη)/τη (v + c(r) + c_g) − τq,t/τη μ̂`, so the difference has slope `(τq,2 − τq,1)/τη` in `v`, which is positive because `τq,t` rises with the history (Lemma 1). The claim holds only if `τq,t` is held fixed | `BohrenEtAl2019.control_shifting_difference_depends_on_v`. Shifting standards are not relied on |
| BIR-D9 | Four printed constants differ by 0.01 from the sum of the pooled coefficients, Table 2 net votes (1.38 against 1.37), Table 3 (0.97 against 0.98), Table 4 (−0.64 against −0.63), Table 6 net votes (0.93 against 0.92) | Rounding of separately estimated coefficients. `BohrenEtAl2019.tables_rounding` |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Definition 1 | `prefPartial` | general | |
| Definition 2 | `beliefPartial`, `unbiased` | general | |
| Definition 3 | `againstWomen`, `againstMen`, `eq5` | general | |
| Definition 4 | `reversal`, `lemma3` | special case | Stated for two histories, the case the paper uses |
| Eqs. (1), (2) | `eq1_expected_loss`, `eq2_optimal` | general | Given the posterior mean and variance |
| Eq. (4) | `v`, `normal_update`, `tq_inv` | special case | The normal update as an identity of log kernels. Densities and integrals are not formalised |
| Eq. (5) | `eq5` | general | |
| Proposition 1 | `prop1_decreasing`, `prop1_constant`, `prop1_limit` | general | For the normal model. "Decreasing" is stated for `μ̂_F < μ̂_M` (BIR-D7) |
| Footnote 9 | `fn9_equivalence` | general | |
| Aggregate initial discrimination (PDF p. 18) | `agg_initial` | general | Finite type sets, as the paper assumes |
| Eq. (6) | `sReq` | general | |
| Proposition 2 | `prop2`, `prop2_D_decreases` | general | For the normal model |
| Eq. (7) | `eq7` | general | |
| Proposition 3 | `prop3_p_zero`, `prop3_signal_order`, `control_reversal_misspecified` | special case | The boundary `p = 0`, the signal order, and the degenerate case `π̂₂(θ₁) = 1`. The mixture posterior and the derivative in `p` are not formalised |
| Heterogeneous tastes (PDF p. 26) | `heterogeneous_taste` | general | |
| Attrition, variance of ability, self-fulfilling beliefs (PDF pp. 25–27) | none | not formalised | Stated in words without a model |
| Lemma 1, eqs. (8) to (11) | `muSeq`, `lemma1`, `invert`, `normal_update` | general | Normality of the posterior is the kernel identity of `normal_update` |
| Lemma 2, eqs. (12), (13) | `eq12`, `k_eq`, `k_mem`, `lemma2` | general | |
| Lemma 3, eq. (14) | `lemma3` | general | |
| Appendix A.2.1, eqs. (15) to (18) | none | not formalised | The paper's checks are numerical, with code in an unread supplement |
| Appendix A.2.2, eqs. (19) to (22) | `phi_strictMono`, `binary_signal` | special case | The monotonicity of `φ` and the inversion. Eq. (22) is checked numerically by the paper |
| Appendix A.3.1, eq. (23) and initial discrimination | `sCut`, `coarse_upvote_iff`, `coarse_cut_mono`, `coarse_cut_limit` | general | |
| Proposition 4 | none | not formalised | Needs preservation of MLRP under convolution (Keilson and Sumita), not in Mathlib |
| Lemma 4, eqs. (24) to (26) | `lemma4_tau_x` | special case | Only the step `τx ≥ 1`. The bound on the normal hazard terms is taken by the paper from Stack Exchange |
| Lemma 5 | none | not formalised | Keilson and Sumita (1982), Theorem 2.1(d) |
| Eq. (27) | `shifting_cancels` | general | |
| Definition 6 and the claim after it | `sShift`, `control_shifting_difference_depends_on_v` | general | The claim fails as printed (BIR-D8) |
| Eqs. (28), (29) | none | not formalised | Regression specifications whose results do not follow from printed numbers |
| Eq. (30) | `stereo`, `stereo_sum`, `stereo_zero` | general | |
| Tables 1 to 6, internal arithmetic | `saturated`, `tables_exact`, `tables_rounding` | general | |
