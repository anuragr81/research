# Eeckhout (2006), claims

Evidence tag [F]: the published article, all 23 PDF pages, read on 16
October 2026 from the text layer, with PDF pages 4, 10, 13, 14, 15, 20 and 22
also read as images. Pages are PDF pages (journal page minus 30). Quotations
are checked against the cached text layer.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| EE-Q1 | 1 | "The main feature of the current analysis is that the stochastically drawn colour of the new partner acts as a public randomization device." | Colour coordinates play. It is not evidence about anything |
| EE-Q2 | 1 | "Of course, in reality there is heterogeneity which leads to selection, that is, the matching is not uniformly random." | Heterogeneity among partners is set aside, footnote 1 |
| EE-Q3 | 2 | "In the standard model of statistical discrimination, size does not matter." | The author's contrast with statistical discrimination on group size |
| EE-Q4 | 3 | "the implication being that discrimination against blacks does not affect the utility of whites." | The author's second contrast with statistical discrimination |
| EE-Q5 | 3 | "Moreover, our result is derived in a context where agents are homogeneous." | No unseen attribute differs across agents |
| EE-Q6 | 3 | "the type of coordination provided by the random arrival of different coloured partners can be mimicked by any public randomization device." | Colour's role could be played by a coin toss. Nothing about colour itself matters |
| EE-Q7 | 4 | "All members of society are identical, except for a pay-off-irrelevant characteristic c ∈ C, say colour." | The primitive. Colour enters no payoff |
| EE-Q8 | 4 | "The characteristic is costlessly observable and" | Colour is observed. The printed sentence ends "cannot be hidden" (EE-D4) |
| EE-Q9 | 9 | "partners can use the pay-off-irrelevant characteristic as a coordination device." | Segregation is equilibrium coordination, not inference or taste |
| EE-Q10 | 13 | "These strategies are match specific, but conditional on the match (either same colour or mixed), they prescribe the same behaviour." | Treatment depends on whether the partner shares one's colour, the form of own-type dependence in the source |
| EE-Q11 | 14 | "They are symmetric type-dependent strategies in which there is neither cooperation nor repeated interaction in mixed matches" | Exclusive segregation, formalised through `Eeckhout2006.singles_quadratic` and `Eeckhout2006.minority_worse` |
| EE-Q12 | 15 | "Exclusive segregation is still an equilibrium, but now for large π, it no longer Pareto dominates the colour-blind equilibrium strategy, and the minority is worse off under segregation than under colour-blind equilibrium." | The minority loses on `(Π_1, Π_0)`, formalised as `Eeckhout2006.minority_worse`, `Eeckhout2006.majority_ic` and `Eeckhout2006.interval_nonempty` |
| EE-Q13 | 17 | "whites may search for social interaction only with other whites. This type of directed search is certainly a promising path for future research." | Choice of where to meet is named and not modelled |
| EE-Q14 | 17 | "it is desirable to introduce heterogeneity as in Burdett and Coles (1997) in order to account for selection." | Heterogeneity is left to future work |
| EE-Q15 | 1 | "For example, in the 1990’s only 5·5% of black men in the U.S. married white women." | The fact the paper sets out to explain |
| EE-Q16 | 1 | "Wong (2003) estimates that even when accounting for selection, the fraction of such mixed matches should be 64%" | Footnote 1. Selection on heterogeneity is a rival origin of assortative matching, which the paper sets aside |
| EE-Q17 | 10 | "This strategy implies discrimination against the own type." | Footnote 14. An equilibrium in which players treat their own colour worse than the other colour |
| EE-Q18 | 14 | "This strategy has whites “acting in good faith” , even though they know they will suffer in a mixed match." | Minority bias. The majority cooperates with outsiders at a cost to itself |
| EE-Q19 | 3 | "Still, policy intervention may be needed in order to induce this strategy, for example, to move equilibrium in society from segregation into minority bias." | Policy selects an equilibrium rather than constraining a choice |
| EE-Q20 | 15 | "In a decentralized trading environment, it seems plausible to rely on such a device rather than on a centralized (i.e. provided by a planner) correlation device (e.g. traffic lights)." | A device available in the population substitutes for a central one |

## Lean results

`lean/mathlib/Eeckhout2006.lean`, in the paper's notation with `Π` written
`Ps` and `V̄` written `Vbar`.

| Theorem | What it checks |
|---|---|
| `Eeckhout2006.stage_dominant` | Section 2. `D` strictly dominates `C` in the stage game |
| `Eeckhout2006.coop_forever` | Eq. (1). Cooperation forever is worth 1 |
| `Eeckhout2006.incubation_value` | Eq. (1). Incubation is worth `δ` |
| `Eeckhout2006.incubation_ic` | Eq. (2). Incubation survives a one-shot deviation exactly when `g ≤ δ` |
| `Eeckhout2006.eq3` | Eq. (3). No deviation in the cooperation phase exactly when `v ≤ V̄` |
| `Eeckhout2006.k_incubation_ic` | Footnote 13. `k`-incubation survives exactly when `g ≤ δ(1 − δ^k)/(1 − δ)` |
| `Eeckhout2006.k_incubation_two` | Footnote 13. At `k = 2` the bound is `δ(1 + δ)` |
| `Eeckhout2006.lemmaA1` | Lemma A1. With `v < 1` the continuation equilibria are `(0, 0)` and `(1, 1)` |
| `Eeckhout2006.control_lemmaA1_needs_v_lt_one` | Control. With `v = 1` the pair `(1, 0)` is also an equilibrium |
| `Eeckhout2006.lemmaA2` | Lemma A2. `σ = 1` forces `v = 1 > V̄` |
| `Eeckhout2006.prop1_incubation` | Proposition 1, the case `σ = 0`. `δ < V̄` exactly when `g < δ` |
| `Eeckhout2006.control_prop1_boundary` | Control. At `g = δ`, `V̄ = δ` and incubation is constrained efficient |
| `Eeckhout2006.eqA1` | (A.1) at indifference |
| `Eeckhout2006.sigma_interior` | Proof of Proposition 1. Indifference `F = G` gives the printed formula for `σ` |
| `Eeckhout2006.example_l05` | §3.3 and §4.1.1, `l = 0·5`. `σ = (10v − 7)/3` and `(1 − v)² = 3/80` |
| `Eeckhout2006.example_l05_numbers` | The same. `0·80635 < v < 0·80636` and `0·3545 < σ < 0·3546` (EE-D9) |
| `Eeckhout2006.Vbar_example` | §3.3. `V̄ = 0·875` at `g = 0·5`, `δ = 0·8` |
| `Eeckhout2006.prop2_incubation_pareto` | Proposition 2, incubation case. `π + (1 − π)δ > δ` for `π > 0` |
| `Eeckhout2006.prop2_incubation_ic` | Proposition 2, incubation case. `v(s^S) ≤ V̄` exactly when `π ≤ 1 − g/δ` |
| `Eeckhout2006.inclusive_pareto` | Proposition 2 and §4.1.1. `π + (1 − π)v(s*) > v(s*)` for any `v(s*) < 1` |
| `Eeckhout2006.control_no_same_colour_no_gain` | Control. At `π = 0` the segregation pay-off equals `δ` |
| `Eeckhout2006.table2_l1` | Table 2, row `l = 1`. The entries `0·9`, `0·8667`, `0·72`, and `0·9 > V̄` |
| `Eeckhout2006.table2_discrepancy` | Table 2, row `l = 1`. The formula gives `56/75`, not the printed `0·6133` (EE-D8) |
| `Eeckhout2006.prop3_step` | Proposition 3, the step in A.4 |
| `Eeckhout2006.control_prop3_equal_shares` | Control. At `Π = 1/2` the two colours' pay-offs are equal whatever the match pay-offs |
| `Eeckhout2006.inclusive_majority_gains` | §4.2.1. `v_w > v_b` when `π > 1/2` |
| `Eeckhout2006.prop4_identity` | A.5. `W^m = W + (1 − π)(1 − δ)[π(2 + g − l) − 1]` |
| `Eeckhout2006.prop4_welfare` | Proposition 4. `W^m > W` exactly when `π > 1/(2 + g − l)` |
| `Eeckhout2006.eqA10` | (A.10), the white no-deviation condition in a mixed match |
| `Eeckhout2006.eqA11` | (A.11), the black incentive constraint |
| `Eeckhout2006.minority_bias_vb_gt_vw` | A.5. `v_b > v_w` under minority bias |
| `Eeckhout2006.prop4` | Proposition 4, assembled over the printed range of `π` |
| `Eeckhout2006.control_prop4_threshold` | Control. At `π = 1/(2 + g − l)` minority bias leaves welfare unchanged |
| `Eeckhout2006.singles_quadratic` | A.6. Stationarity of singles gives `Π²(1 − 2π) + 2πΠ − π = 0` |
| `Eeckhout2006.singles_root` | A.6. The stated root solves the quadratic |
| `Eeckhout2006.singles_share_bounds` | §4.2.3. For `π > 1/2` any root in `(0, 1)` lies in `(1/2, π)` |
| `Eeckhout2006.minority_worse` | §4.2.3. The minority's pay-off is below `δ` exactly when `Π > 1/(1 + δ)` |
| `Eeckhout2006.majority_ic` | §4.2.3. The majority's incentive constraint holds exactly when `Π < 1 − g/((1 + g)δ)` |
| `Eeckhout2006.interval_nonempty` | §4.2.3. `(Π_1, Π_0)` is non-empty exactly when `g < δ²/(1 + δ − δ²)` |
| `Eeckhout2006.control_equal_shares_no_loss` | Control. At `Π = 1/2` the minority's pay-off is not below `δ` |
| `Eeckhout2006.exclusive_below_inclusive` | §4.2.3. With `Π < π` the majority's exclusive pay-off is below `π + (1 − π)δ` |
| `Eeckhout2006.public_randomization` | §5.1. The exogenous device meets the constraint exactly when `q̄ ≤ 1 − g/((1 + g)δ)` |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Section 2, the stage game | `Eeckhout2006.stage_dominant` | general | |
| Eq. (1) | `Eeckhout2006.coop_forever`, `Eeckhout2006.incubation_value` | general | |
| Eq. (2) | `Eeckhout2006.incubation_ic` | general | |
| Eq. (3) | `Eeckhout2006.eq3` | general | |
| Footnote 13 | `Eeckhout2006.k_incubation_ic`, `Eeckhout2006.k_incubation_two` | general | |
| Proposition 1 | `Eeckhout2006.prop1_incubation`, `Eeckhout2006.sigma_interior` | special case | The case `σ ∈ {0, 1}` and the formula for interior `σ` are proved. The genericity argument on (A.4) and (A.5) is not |
| Lemma A1 | `Eeckhout2006.lemmaA1` | general | |
| Lemma A2 | `Eeckhout2006.lemmaA2` | general | |
| (A.1) to (A.3) | `Eeckhout2006.eqA1`, `Eeckhout2006.sigma_interior` | general | |
| (A.4) and (A.5) | | not formalised | A polynomial identity followed by a genericity claim about its zero set. The zero-set argument would need algebraic geometry not set up here |
| Table 1 | `Eeckhout2006.example_l05`, `Eeckhout2006.example_l05_numbers` | special case | The strategy `(σ, (1, 1, 0))` at `l = 0·5` only. The other rows and the cut-offs `l = 0·11`, `0·53` are not recomputed |
| §3.3, `V̄ = 0·875` | `Eeckhout2006.Vbar_example` | general | |
| Proposition 2 | `Eeckhout2006.prop2_incubation_pareto`, `Eeckhout2006.prop2_incubation_ic`, `Eeckhout2006.inclusive_pareto` | special case | The incubation case is proved. The interior case uses the implicit function theorem on (A.6) and (A.7) with signs the paper calls generic, and is not formalised |
| Table 2 | `Eeckhout2006.table2_l1`, `Eeckhout2006.table2_discrepancy` | special case | Row `l = 1` only. The other rows need `σ₁*` and `σ₂*` at each `π`, which are not printed |
| §4.1.1, age-dependent strategies | `Eeckhout2006.inclusive_pareto` | special case | The Pareto gain is proved. The stationary measure `M^S` of (A.9) and the value `0·8238` are not |
| §4.1.2 | | not formalised | A verbal argument about the number of groups |
| Proposition 3 | `Eeckhout2006.prop3_step` | general | The algebraic step. Its last appeal to Proposition 1 inherits that result's status |
| §4.2.1, `v_w > v_b` | `Eeckhout2006.inclusive_majority_gains` | general | |
| Eqs. (4), (5) and Proposition 4 | `Eeckhout2006.prop4`, `Eeckhout2006.prop4_identity`, `Eeckhout2006.prop4_welfare`, `Eeckhout2006.eqA10`, `Eeckhout2006.eqA11`, `Eeckhout2006.minority_bias_vb_gt_vw` | general | |
| §4.2.3, the minority's loss | `Eeckhout2006.minority_worse`, `Eeckhout2006.majority_ic`, `Eeckhout2006.interval_nonempty` | general | |
| §4.2.3, `π ≥ Π ≥ 1/2` | `Eeckhout2006.singles_share_bounds` | general | Proved strictly for `π > 1/2` |
| §4.2.3, `Π` increasing in `π` | | not formalised | Needs the derivative of the root, not set up |
| §4.2.3, exclusive below inclusive | `Eeckhout2006.exclusive_below_inclusive` | general | |
| A.6 | `Eeckhout2006.singles_quadratic`, `Eeckhout2006.singles_root` | general | |
| §5.1 | `Eeckhout2006.public_randomization` | general | |
| (A.8) and (A.9) | | not formalised | Stationary measures of singles under mixed strategies, used only in §4.1.1 |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| EE-D1 | "From Section 4.4 onwards" (PDF p. 4) is read as Section 4.2 | The article has no Section 4.4. The two-colour analysis begins in Section 4.2 |
| EE-D2 | Proposition 3 reads "Whenever π ≠ 1/2" | The text layer drops `≠`. The image of PDF p. 13 shows it, and the proof on PDF p. 21 needs `Π ≠ 1/2` |
| EE-D3 | `Π(c)` is read as the share of colour `c` among singles | PDF p. 13 calls it "the proportion of types c that are single", while A.6 defines `Π = n_w/(n_w + n_b)`. The formalisation follows A.6 |
| EE-D4 | The text layer interleaves two lines on PDF p. 4, so EE-Q8 stops at "and" | The image shows the sentence "The characteristic is costlessly observable and cannot be hidden." |
| EE-D5 | The indifference condition on PDF p. 10 is read as `v(s_1*) = σ_1*[(1 + g)(1 − δ) + δ] + (1 − σ_1*)δ v(s_1^S)` | The printed `(1 − σ_1* δ v(s_1^S))` misplaces a parenthesis. The colour-blind analogue on PDF p. 11 has the form adopted |
| EE-D6 | In A.2 (PDF p. 20) the second strategy is `s_2*` with `σ_2*` | The text says "Denote this strategy by s_1*, with σ_1*", a slip, since the following equations use `σ_2*` |
| EE-D7 | Proposition 3 is stated with the population share `π` and proved with the singles share `Π` | Under the inclusive strategies of §4.2.1 no match separates and `π = Π` (PDF p. 13). We formalise the step in `Π` |
| EE-D8 | Table 2's entry `v(s₂^S) = 0·6133` at `l = 1`, `π = 1/3` is read as a misprint | With `s₂* = s*` the printed formula `[πδ + (1 − π)]v(s*)` gives `56/75 ≈ 0·7467` (`Eeckhout2006.table2_discrepancy`). The same formula reproduces the neighbouring `0·72` |
| EE-D9 | The printed `v(s*) = 0·8063` at `l = 0·5` is a truncation of `1 − √(3/80) ≈ 0·80635` | `Eeckhout2006.example_l05_numbers`. The printed `σ = 0·3545` agrees with the exact value |
