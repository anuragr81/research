# Eguia (2013), claims

Evidence tag [F]. The text layer of the 2013 working paper was read in full,
pp. 1 to 40, on 16 October 2026, with the equation pages also read from
images (`RECONSTRUCTION.md`). Pages are PDF pages, which equal printed pages.
The text layer prints ligatures such as `ﬀ` and `ﬃ`, and the quotations keep
them.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| EG-Q1 | 1 | "The cost of assimilation is endogenous and strategically chosen by the advantaged group to screen those seeking to assimilate." | Incumbents set an entry cost that screens on an unseen type. The nearest precedent in this source for A1's price screening (contribution 3) |
| EG-Q2 | 6 | "Individual ability is private information." | The unseen type. Nobody else learns it, from a marker or otherwise |
| EG-Q3 | 6 | "the distribution of ability over J is uniform in [0, 1]." | Ability has the same distribution in both backgrounds, so the background is no evidence about ability. A1 assumes the opposite, a group class distribution that differs by group |
| EG-Q4 | 8 | "which captures the intuition that more skilled agents can adapt at a lower cost" | The cost of assimilating falls with the unseen type, the single-crossing that makes screening work (`screen_exists`) |
| EG-Q5 | 9 | "The externalities occur when agents who have more in common and take similar actions, interact with each other." | The payoff from a group comes from interaction within it, but is modelled through group averages only (eq. 1) |
| EG-Q6 | 9 | "Agents with greater exogenous endowment ei and with greater skill si generate more positive externalities to their friends and members of their group." | Preferences over partners are vertical. Every member gains from a richer, more skilled group, which is not class homophily |
| EG-Q7 | 10 | "there is a cutoﬀ s(d) in the level of skill such that any member of DY chooses to assimilate if and only if her skill is above s(d)." | The cutoff rule behind eq. (3) and the composition fixed point |
| EG-Q8 | 12 | "highly skilled individuals, who appreciate their group’s average skill more than less skilled individuals (vsi sJ ≥ 0), want to discriminate less" | The preferred barrier depends on the incumbent's own skill, through an assumed complementarity, not a derived one (contribution 1) |
| EG-Q9 | 13 | "is a screening device that the advantaged agents use to separate high skilled from low skilled agents, without a need to observe the actual skill level of the agent who assimilates." | Screening without observing the type, the role a price plays in A1's contribution 3 |
| EG-Q10 | 13 | "Since each agent knows her own skill, in equilibrium no agent is misallocated." | Self-sorting. Nothing in the model is inferred about a person from the group |
| EG-Q11 | 14 | "assuming that v depends on the whole distribution of skills in group J, and not only on the average" | Footnote 11. The externality depends on the group mean only. No within-group spread enters, so there is no analogue of A1's σ² term |
| EG-Q12 | 14 | "In an overlapping generations model, if the most able agents with a disadvantaged background assimilate, the distribution of ability among the older generation is not identical across groups; rather, it favors the advantaged group." | Dynamics are outside the model, as they are outside A1 |
| EG-Q13 | 22 | "They all find that the acting white problem is more severe in less segregated (that is, in more racially integrated) schools" | The source's empirical reading in integrated schools, beside A1's hook in Chetty et al. (2022) |
| EG-Q14 | 27 | "rather than on an ascriptive characteristic that oﬀers no information about the person’s skills." | The case against discriminating on immutable traits rests on EG-Q3, a marker with no information. Formalised as `control_lump_sum_no_screen` |
| EG-Q15 | 28 | "If agents have intrinsic preferences over exogenous attributes such as race or place of birth, they may prefer ceteris paribus to associate with those who look like them or come from the same town." | Homophily on the marker is an add-on in words, a taste. A1 derives avoidance from homophily on class |
| EG-Q16 | 2 | "acceptance into the advantaged group must be based on malleable individual traits and behaviors that correlate with ability, and not on immutable characteristics that are uncorrelated with talent, such as skin color or place of birth." | Screening moves from the marker to traits correlated with the unseen type. The motive is efficiency, since the marker is uninformative (EG-Q3), not a legal ban |
| EG-Q17 | 9 | "Leisure and job opportunities, friendships, private and professional relationships develop more readily among agents who follow the same norms and take part in the same activities." | The source's origin of the value of interaction is shared norms and shared activities. The nearest the source comes to budget compatibility |
| EG-Q18 | 12 | "Less skilled agents, who do not care as much for the increase in average skill that comes with assimilation, resent the decrease in average endowment and prefer higher barriers to assimilation to let fewer agents assimilate." | The low-skilled members of the advantaged group are the most exclusionary, the opposite of A1's direction (ii). Formalised as `prop1iii` with `maximiser_antitone` |
| EG-Q19 | 24 | "they must be more popular among students outside her ethnicity (whom the agent is joining as she assimilates)." | The ablest members of the disadvantaged group turn toward the other group, the mirror image of A1's direction (ii) |
| EG-Q20 | 28 | "If both sets of agents dislike the exogenous attributes of the other set, then the eﬀect on d is ambiguous, but the number of agents who assimilate is smaller, resulting in voluntary segregation." | Avoidance in both directions, but as a taste for the marker, in words only |
| EG-Q21 | 27 | "Agents with an advantaged background can construct and use a set of norms that are less costly to acquire for highly skilled agents" | A group-neutral rule on a variable correlated with the unseen type, built by the incumbents |


## Lean results

`lean/mathlib/Eguia2013.lean`, in the paper's notation with `l` for `λ`. The
primitives are the structure `Primitives`, eq. (1) is `U`, eq. (2) `Upeer`,
`d(s)` is `dcut`, eq. (6) is `foc6`, eq. (7) `eAbar2` and eq. (9) `foc9`.

| Theorem | What it checks |
|---|---|
| `Eguia2013.eq1_nonassimilator` | Eq. (1) for an agent who does not assimilate |
| `Eguia2013.eq2_pressure_cost` | Eq. (2). Skill above the threshold costs `K` |
| `Eguia2013.eq3_sA` | Eq. (3). The first form of `s_A(s)` equals its closed form, the mean skill of the members of `A` |
| `Eguia2013.eq3_sD` | Eq. (3). The same for `s_D(s)` |
| `Eguia2013.total_mean` | The masses of `A` and `D` times their mean skills add to 1/2 |
| `Eguia2013.sA_gt_half` | With `λ > 0` and `0 < s < 1`, assimilation raises the mean skill of `A` above 1/2 |
| `Eguia2013.sD_lt_half` | With the same conditions, it lowers the mean skill of `D` below 1/2 |
| `Eguia2013.eAbar_lt` | It dilutes the mean endowment of `A` |
| `Eguia2013.composition_lambda_zero` | At `λ = 0` nothing moves |
| `Eguia2013.step1_skill_monotone` | First step of the proof of Proposition 1. More skill never lowers utility |
| `Eguia2013.cutoff_rule` | Second step. Assimilation is a cutoff rule when the gain does not fall with own skill (EG-D7) |
| `Eguia2013.control_cutoff_needs_monotone_gain` | Control. With a gain falling in own skill the cutoff rule fails |
| `Eguia2013.dcut_indifferent` | p. 30. The agent at the cutoff is indifferent at `d(s)` |
| `Eguia2013.dcut_strictMono_lambda_zero` | Third step at `λ = 0`. `d(s) = K/c(s)` is strictly increasing |
| `Eguia2013.control_dcut_no_gap` | Control. With no endowment gap `d(s)` is flat |
| `Eguia2013.cutoff_unique` | Proposition 1 (i). A strictly increasing `d(s)` gives a unique cutoff |
| `Eguia2013.eAbar_hasDerivAt` | p. 32. The derivative of `e_A(s)` |
| `Eguia2013.sA_hasDerivAt` | p. 32. The derivative of `s_A(s)` |
| `Eguia2013.sA_deriv_one` | At `s = 1` that derivative is `−λ/2` (EG-D1) |
| `Eguia2013.eq6_at_one` | Eq. (6) at `s = 1` gives `−λ/2`, not the printed `(−λ − λ²)/2` (EG-D1) |
| `Eguia2013.eq5_iff_eq6` | Eqs. (5) and (6) are equivalent |
| `Eguia2013.foc_pos_le_half` | p. 33. `dv/ds > 0` for `s ≤ 1/2` |
| `Eguia2013.foc_root_gt_half` | p. 33. Every root of eq. (6) exceeds 1/2, so `θ* > 1/2` |
| `Eguia2013.control_foc_no_mobility` | Control. At `λ = 0` eq. (6) holds everywhere |
| `Eguia2013.foc_neg_at_one` | p. 32. `dv/ds < 0` at `s = 1` for a low endowment |
| `Eguia2013.control_foc_large_endowment` | Control. The sign reverses for a high endowment |
| `Eguia2013.Flin_sub_one` | The gain from an interior cutoff when `v` is linear in the group averages |
| `Eguia2013.prop1ii_linear` | Proposition 1 (ii), linear case. A small endowment gives `θ* < 1` |
| `Eguia2013.control_prop1ii_large_endowment` | Control. A large endowment makes `s = 1` optimal |
| `Eguia2013.maximiser_antitone` | Proposition 1 (iii). Strictly decreasing differences make the preferred cutoff fall with the incumbent's skill |
| `Eguia2013.prop1iii` | Proposition 1 (iii). `d*_1 ≥ … ≥ d*_N` |
| `Eguia2013.control_prop1iii_increasing_differences` | Control. With increasing differences the order reverses |
| `Eguia2013.clamp_truthful` | Fifth step. Under single-peaked preferences truthful proposals are a best response to an order-statistic rule in clamp form |
| `Eguia2013.control_clamp_not_single_peaked` | Control. Without single-peakedness a misreport can pay |
| `Eguia2013.eq8` | Eqs. (7) and (8) |
| `Eguia2013.eq9_eq_eq6` | Eq. (9) is eq. (6) with the gap `e_𝒜 − e_𝒟` |
| `Eguia2013.prop2_linear` | Proposition 2 (i), linear case. A richer `𝒟` lowers the cutoff |
| `Eguia2013.prop2ii_linear` | Proposition 2 (ii), linear case. Equal increases leave the cutoff unchanged |
| `Eguia2013.control_prop2_large_gap` | Control. A large gap leaves no interior cutoff |
| `Eguia2013.chosen_le` | Under peer pressure nobody's skill exceeds ability |
| `Eguia2013.prop3_sPA_one` | Proposition 3, the `𝒜` side. `s^P_𝒜 = 1` gives every member the most skill |
| `Eguia2013.control_prop3_pressure_bites` | Control. A threshold below ability with a biting punishment lowers skill |
| `Eguia2013.example4_differences` | Example 4. Column (3)−(2) of the table follows from columns (2) and (3) |
| `Eguia2013.screen_exists` | p. 27. A cost strictly decreasing in skill can screen |
| `Eguia2013.control_lump_sum_no_screen` | Control. A cost that does not depend on skill cannot |
| `Eguia2013.control_no_mobility` | Control. With `λ = 0` both groups keep mean skill 1/2, so `sD_lt_half` needs `λ > 0` |
| `Eguia2013.control_cutoff_zero` | Control. With `s = 0` the mean skill of `A` stays 1/2, so `sA_gt_half` needs `s > 0` |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Eq. (1), utility | `Eguia2013.eq1_nonassimilator` | general | |
| Eq. (2), utility under peer pressure | `Eguia2013.eq2_pressure_cost` | general | |
| Eq. (3), group averages | `Eguia2013.eq3_sA`, `Eguia2013.eq3_sD`, `Eguia2013.total_mean` | general | |
| Composition effects of eq. (3) | `Eguia2013.sA_gt_half`, `Eguia2013.sD_lt_half`, `Eguia2013.eAbar_lt` | general | |
| Proof of Prop. 1, first step, `s_i = θ_i` | `Eguia2013.step1_skill_monotone` | general | Weak dominance is shown; the paper says strict |
| Proof of Prop. 1, second step, cutoff rule | `Eguia2013.cutoff_rule` | general | Needs a gain that does not fall with own skill, which the paper leaves implicit (EG-D7) |
| `d(s)`, p. 30 | `Eguia2013.dcut_indifferent` | general | |
| Proof of Prop. 1, third step at `λ = 0` | `Eguia2013.dcut_strictMono_lambda_zero` | general | |
| Third step for small `λ > 0` | none | not formalised | Argued by continuity in `λ` with no explicit bound |
| Third step for `c′/c` very negative | none | not formalised | Argued by a bound that is not made explicit |
| Derivatives on p. 32 | `Eguia2013.eAbar_hasDerivAt`, `Eguia2013.sA_hasDerivAt` | general | |
| Eqs. (5) and (6) | `Eguia2013.eq5_iff_eq6` | general | |
| `s = 1` not optimal for low `e_𝒜`, p. 32 | `Eguia2013.foc_neg_at_one` | general | In the partial derivatives at `s = 1`, with the corrected bound (EG-D1) |
| `s*(s_i) > 1/2`, p. 33 | `Eguia2013.foc_root_gt_half` | general | |
| Uniqueness of `s*(s_i)`, pp. 32–33 | none | not formalised | Rests on signs of second derivatives of an unspecified `v` |
| Proof of Prop. 1, fifth step, strategy-proofness | `Eguia2013.clamp_truthful` | special case | The order statistic is taken in clamp form; the representation is not proved |
| Prop. 1 (i), unique equilibrium with `θ* ∈ (1/2, 1]` | `Eguia2013.cutoff_unique`, `Eguia2013.foc_root_gt_half` | special case | Uniqueness of the cutoff given a strictly increasing `d(s)`, and the bound `θ* > 1/2`; existence and the conditions that make `d(s)` increase are not formalised |
| Prop. 1 (ii), `θ* < 1` for a small gap | `Eguia2013.prop1ii_linear` | special case | `v` linear in `e_J` and `s_J` |
| Prop. 1 (iii), `d*_1 ≥ … ≥ d*_N` | `Eguia2013.maximiser_antitone`, `Eguia2013.prop1iii` | special case | Decreasing differences are assumed rather than derived from `v_{s_i s_J} ≥ 0` |
| Eqs. (7) and (8) | `Eguia2013.eq8` | general | |
| Eq. (9) | `Eguia2013.eq9_eq_eq6` | general | |
| Prop. 2 (i) | `Eguia2013.prop2_linear` | special case | `v` linear; the cutoff only, not `d*` |
| Prop. 2 (ii) | `Eguia2013.prop2ii_linear` | special case | `v` linear, where the cutoff is unchanged; a strict decrease needs `v_{e_J e_J} < 0` |
| Prop. 3, `s^P_𝒜 = 1` | `Eguia2013.prop3_sPA_one` | special case | Linear direct utility, assimilation set aside |
| Prop. 3, `s^P_𝒟 < γ` in some and in every equilibrium | none | not formalised | The multi-stage argument with stage-three multiplicity is not formalised |
| Example 4, column (3)−(2) | `Eguia2013.example4_differences` | general | |
| Example 4, columns (2) and (3) | none | not formalised | A numerical optimum with a square root; column (2) was reproduced numerically for reading only |
| Screening on malleable traits, p. 27 | `Eguia2013.screen_exists`, `Eguia2013.control_lump_sum_no_screen` | general | |
| Intrinsic preferences over diversity, p. 28 | none | not formalised | Stated in words; the formal version is in the unread add-on file |
| Symmetric society, pp. 28–29 | none | not formalised | As above |
| Cascades and multiplicity, p. 13 | none | not formalised | Examples are said to be available from the author |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| EG-D1 | On p. 32, `∂s_A/∂s` at `s = 1` is read as `−λ/2`, not the printed `(−2λ(1 + λ) + λ)/2`, which the next line simplifies to `(−λ − λ²)/2` | Eq. (3) and the paper's own eq. (6) both give `−λ/2` (`sA_deriv_one`, `eq6_at_one`). The bound below it should read `e_𝒜 < (1/2)(∂v/∂s_A)/(∂v/∂e_A)` rather than `(1 + λ)/2` times the ratio. The claim that `s = 1` is not optimal for low enough `e_𝒜` is unaffected |
| EG-D2 | Proposition 1's conditions are read as one statement | p. 11 states `λ ≤ λ_c` and `c′/c < c_λ` with `c_λ ∈ ℝ`, while p. 29 states `λ < λ_c` and `−c′/c > c_λ` with `c_λ ∈ ℝ₊₊`. The two differ in sign convention and the boundary case only |
| EG-D3 | In Example 4, `U_𝒟` is the average over `𝒟` and `U_𝒟−` the average over `θ ≤ 1/2` | p. 19 names `U_𝒜, U_𝒟−, U_𝒟+`, and the table on p. 20 reports `U_𝒜, U_𝒟, U_𝒟−`. Not relied on |
| EG-D4 | On p. 33, `dv(s_i, ω_A, s_A)/ds` is read with `e_A` for `ω_A`. On p. 36, "a decrease in θ_D" is read as a decrease in `s_D` | Typos. Not relied on |
| EG-D5 | The version read is the working paper of 10 September 2013, titled "Discrimination and Assimilation", not the 2017 *JPubE* article "Discrimination and assimilation at school" | The title page. Rows that cite the published article carry an unverified mark until it is read |
| EG-D6 | The three generalizations (pp. 27–29) are attributed only as the verbal claims printed there | Their formal statements are in an add-on file (footnote 18) that was not read |
| EG-D7 | The second step of the proof of Proposition 1 is read as needing a gain from assimilating that does not fall with own skill | The paper derives the cutoff rule from `c` falling in skill alone. The gain `v(s_i, e_A, s_A) − v(s_i, 0, s_D)` can fall in `s_i` unless, for example, `v_{s_i s_J} ≥ 0` and `s_A ≥ s_D`, and `control_cutoff_needs_monotone_gain` shows the rule can then fail |
