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
