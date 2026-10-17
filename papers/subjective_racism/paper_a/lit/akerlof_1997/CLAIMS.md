# Akerlof (1997), claims

Evidence tag [F]. The JSTOR copy of the published article, Econometrica 65(5),
1005–1027, supplied by the author on 17 October 2026, was read in full on
17 October 2026, all 24 PDF pages with the appendix and the references.
Pages are PDF pages. PDF page 1 is the JSTOR cover, and PDF page n is journal
page 1003 + n. The text layer prints the paper's dashes as hyphens, and the
quotations keep them.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| AK-Q1 | 2 | "In this generalization agents have inherited positions in social space and an expected value of trade between two individuals as a function of the difference in their initial positions." | The value of contact is a function of the distance between the two people's positions, the paper's central modelling choice |
| AK-Q2 | 2 | "Agents who are initially close interact strongly while those who are socially distant have little interaction." | Contact falls with social distance. Positions are known, so nothing is read from a marker |
| AK-Q3 | 5 | "in the case of conformist behavior, utility declines as distance between the individual's behavior and that of everyone else increases." | The conformist model, eq. (3), a loss in the distance of one's own choice from others' |
| AK-Q4 | 5 | "Because of the competitive race for status the value of x exceeds its optimum by the amount d/2a." | Eq. (2), `eq2_status_opt` and `eq2_excess` |
| AK-Q5 | 6 | "In fact the range of values of x between (b - d)/2a and (b + d)/2a constitutes a zone of equilibria for given d." | Eq. (4), `eq4_zone`, with `control_conform_outside_zone` |
| AK-Q6 | 6 | "It is important to recognize that the existence of multiple equilibria in the conformist model is sensitive to the choice of utility function." | The paper's own statement that the form of the distance loss drives the result |
| AK-Q7 | 6 | "If, in contrast, the utility function were quadratic, of the form" | Eq. (5), the quadratic distance loss `d (x − x̄)²` |
| AK-Q8 | 7 | "there would be only one equilibrium value of x-precisely at the optimum value" | Eq. (5) has one equilibrium, `eq5_unique` and `eq5_opt` |
| AK-Q9 | 7 | "I have considered utility functions of this sort-with the marginal utility of decreased distance vanishing at zero-to be a special case." | The quadratic loss that A1's A14 uses is the case the paper sets aside. A1 derives it from budgets (M19) |
| AK-Q10 | 7 | "Social interaction, which is represented as mutually beneficial trade between individuals, will increase with proximity in this space." | The microfoundation in words. Contact is trade whose value rises with proximity |
| AK-Q11 | 8 | "I shall follow the common practice of basing trade, and, correspondingly, also the benefits of trade" | The sentence ends "on a "gravity"model". The form of the benefit is taken from the trade literature by convention, not derived |
| AK-Q12 | 8 | "Adoption of this exact formula to represent social exchange would place excessive benefits on proximity in social exchange." | The reason for the first modification, a finite benefit at zero distance, `benefit_le_at_zero` |
| AK-Q13 | 8 | "with the inclusion of a negative constant term the social interactions could yield negative benefits." | The level of the benefit can be shifted, and only its slope in distance matters |
| AK-Q14 | 9 | "as either the initial or the acquired social distance between a pair of individuals increases, the value of social exchange between them declines asymptotically toward zero." | The benefit `e/((f + d₀)(g + d))` falls in distance toward zero, `benefit_antitone` and `benefit_tendsto_zero` |
| AK-Q15 | 9 | "The simplest assumption is static expectations that the acquired social position of all the other individuals will coincide with their initial position." | Eq. (6) is solved under static expectations, `U6` |
| AK-Q16 | 10 | "since person 3 does not much value trade with persons 1 or 2 since she is initially so socially distant from both of them, she chooses a value of x that is close to the economic optimum value of b/2a." | The value of contact depends on the chooser's own position. Nothing is hidden, so the dependence is not read through a marker |
| AK-Q17 | 11 | "the social optimum need not be the only rational expectations equilibrium" | The paper's solution concept, with static expectations as the simplest case |
| AK-Q18 | 12 | "Thus dialects act as a diagnostic for social interaction." | Dialect follows the clustering of contact, the reading of a marker that A1 takes as given |
| AK-Q19 | 12 | "x could represent pronunciation or language, with the model showing theoretically why there might be a correlation between a choice variable, such as dialect, and subcultural membership." | The marker is chosen in the paper, by conformity, where A1 takes it as given (A4) |
| AK-Q20 | 14 | "the relevant cost includes not just lost wages, tuition, and the disutility of schoolwork-the ordinary interpretation of those costs." | Leaving one's origin costs lost contacts, the cost of passing in A1's A12 |
| AK-Q21 | 17 | "there was the cost of being labeled a White man in a Black neighborhood" | The same cost, in the paper's biographical cases |
| AK-Q22 | 18 | "Interventions in a closed environment that attempt to change the social position of an entire social network simultaneously" | Policy acts on the whole network, not one person |
| AK-Q23 | 20 | "the analysis of social programs must include an evaluation of an intervention's impact on group interactions and not just the direct effects of the program." | The paper's policy conclusion |
| AK-Q24 | 20 | "the community is endogenously defined in terms of peoples' sense of social location." | The setting of contact is chosen by the people in it |
| AK-Q25 | 20 | "these externalities may be possible to capture in small, near total institutions such as schools." | Schools as the unit of intervention |

## Lean results

`lean/mathlib/Akerlof1997.lean`, in the paper's notation. Eq. (1) is
`Ustatus`, eq. (3) `Uconf`, eq. (5) `Uquad`, the benefit of Section 3
`benefit`, and eq. (6) `U6`. The theorems named `contrast_*` and
`control_benefit_range_differs` are ours. They compare the paper's benefit
with A1's quadratic loss and are not results of the paper.

| Theorem | What it checks |
|---|---|
| `Akerlof1997.eq2_status_opt` | Eq. (2). With `a > 0`, `(b + d)/2a` maximises eq. (1) whatever others choose |
| `Akerlof1997.eq2_excess` | The excess over `b/2a` is `d/2a` |
| `Akerlof1997.eq4_zone` | Eq. (4). Every `x̄` in `[(b − d)/2a, (b + d)/2a]` is a best reply to itself under eq. (3) |
| `Akerlof1997.control_conform_outside_zone` | Control. With `a = 1`, `b = c = 0`, `d = 1`, `x̄ = 1` lies outside the zone and `1/2` does better |
| `Akerlof1997.eq5_unique` | Eq. (5). A best reply to itself must equal `b/2a` |
| `Akerlof1997.eq5_opt` | The converse. `b/2a` is a best reply to itself |
| `Akerlof1997.benefit_antitone` | The benefit does not rise in the acquired distance |
| `Akerlof1997.benefit_le_at_zero` | The benefit is at most its finite value at zero distance |
| `Akerlof1997.benefit_tendsto_zero` | The benefit tends to zero as distance grows |
| `Akerlof1997.eq6_intrinsic_opt` | Eq. (6) with no value of exchange is maximised at `b/2a` |
| `Akerlof1997.contrast_spread_helps_distant` | Ours. For an observer below two classes `m ± s`, the mean benefit `1/(g + distance)` does not fall as `s` grows |
| `Akerlof1997.contrast_spread_hurts_at_mean` | Ours. For an observer at the mean, the mean benefit does not rise as `s` grows |
| `Akerlof1997.contrast_quadratic_spread` | Ours. The mean squared distance is the squared gap plus `s²` for every observer |
| `Akerlof1997.contrast_shift_up_above` | Ours. For any benefit that does not rise in distance, an observer above every member does not lose when the group's classes rise toward the observer |
| `Akerlof1997.contrast_shift_up_below` | Ours. The same shift does not help an observer below every member |
| `Akerlof1997.control_benefit_range_differs` | Ours, control. Classes `0` and `10` and an observer at `6`, above the standing `5`. A shift of `1` lowers the mean squared distance from `26` to `25` and also lowers the mean benefit from `12/70` to `1/6` |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Eqs. (1) and (2), the status model | `Akerlof1997.eq2_status_opt`, `Akerlof1997.eq2_excess` | general | |
| Eqs. (3) and (4), the zone of equilibria | `Akerlof1997.eq4_zone`, `Akerlof1997.control_conform_outside_zone` | general | The zone is shown to be a set of equilibria. That no point outside it is one is shown by one control, not in general |
| Eq. (5), one equilibrium | `Akerlof1997.eq5_unique`, `Akerlof1997.eq5_opt` | general | |
| The benefit of exchange, Section 3 | `Akerlof1997.benefit_antitone`, `Akerlof1997.benefit_le_at_zero`, `Akerlof1997.benefit_tendsto_zero` | general | |
| Eq. (6) under static expectations | `Akerlof1997.eq6_intrinsic_opt` | special case | The case with no value of exchange only |
| The three-person example, Section 4 and the Appendix | none | not formalised | The Appendix argues from "sufficiently" close, distant and high values and gives no explicit bounds |
| Rational expectations and the social optimum, p. 10 | none | not formalised | Stated in words |
| The gravity analogy and the choice of how positions update, pp. 11–12 | none | not formalised | Stated in words, and outside A1 |
| Sections 5 and 6, ethnography and interventions | none | not formalised | Cases, not results |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| AK-D1 | The version read is the published article, from JSTOR, accessed 9 May 2010 according to the cover page | The cover page |
| AK-D2 | In eq. (1) the loss `d(x̄ − x)` is read as linear and signed, so that being ahead gains utility at the same rate | The text says the person "loses utility in amount d(x̄ − x) insofar as she falls behind", and eq. (2) follows only with the linear signed form |
| AK-D3 | In eq. (6) the expected acquired distance is read as `|x₁ᵢ − x₀ⱼ|` | The text on p. 9 states it under static expectations, and the printed eq. (6) is garbled in the text layer |
| AK-D4 | The contrasts are ours | The paper does not compare its benefit with a quadratic loss in the other's type, and has no hidden type |
