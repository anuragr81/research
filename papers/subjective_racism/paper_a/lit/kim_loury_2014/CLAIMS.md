# Kim and Loury (2014), claims

Evidence tag [F]: the full text of MPRA Paper 54950, version of 31 March
2014, all 38 PDF pages, read on 16 October 2026. Pages are PDF pages, and PDF
page 1 is the MPRA cover, so PDF page `n` is the paper's page `n − 1`.
Quotations are checked against the publisher's text layer.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| KLB-Q1 | 3 | "When rational, information-seeking decision makers use aggregate group characteristics to evaluate relevant personal characteristics of individuals with whom they interact, individuals from different groups may be treated differently, even if they share identical observable characteristics (Moro, 2009)." | The group as a proxy for an unseen trait, the premise Paper A1 shares, stated for decision makers in general and modelled for employers only |
| KLB-Q2 | 3 | "Thus, when groups coordinate on different equilibria, the model displays inequality across ex-ante identical groups." | The groups differ by equilibrium and not by fundamentals, unlike A1's groups, whose class distributions differ |
| KLB-Q3 | 7 | "Employers, however, observe each candidate’s group identity and a noisy signal θ, the distribution of which depends on whether he is qualified." | The employer sees evidence about the individual as well as the group, which A1's group sufficiency rules out for the marker |
| KLB-Q4 | 7 | "Therefore, newborn workers are “mixed” with the rest of the group population, and “age” becomes another source of imperfect information that makes the behavior of previous cohorts relevant to decisions made today" | Pooling within the group is what makes the group's mean the prior, the role group sufficiency plays in A1 |
| KLB-Q5 | 7 | "Once his true capabilities are revealed, he leaves the temporary job market and joins the permanent job market, where he secures a permanent job and is assigned to a task according to his qualifications: Task One, if qualified; Task Zero, otherwise." | Once the type is seen the group plays no part in assignment. The paper assumes this for a labour market, and A1 has the same property for contact |
| KLB-Q6 | 7 | "If “age” is perfectly observable, a cohort’s investment decision is not affected by the group’s collective reputation." | Footnote 1. The reputation channel needs the pooling of KLB-Q4 |
| KLB-Q7 | 8 | "That is, the screening process is more effective in identifying the unqualified than the qualified." | The condition `Pq < Pu` behind `beta_strictMono` |
| KLB-Q8 | 8 | "When BOD is given, the expected net return from investing in human capital is ωPu , and ωPq otherwise, given a wage rate of ω for Task One." | Eq. (2), derived from the signal structure in `beta_eq_gap` |
| KLB-Q9 | 8 | "It is notable that if the opposite assumption holds (0 < Pu < Pq < 1), the expected net return to the skill investment βτ (ξτ ) is decreasing in the probability that the worker receives the benefit of the doubt" | Formalised as `control_beta_strictAnti` |
| KLB-Q10 | 9 | "That is, the benefit of the doubt is given only when the “collective reputation” of the identity group to which a worker belongs is equivalent to or exceeds some threshold level Π∗" | The employer's rule, formalised as `bod_iff`. It depends on the group's share of qualified workers only, with no term for the employer's own type and none for the group's spread |
| KLB-Q11 | 9 | "Thus, a worker receives the benefit of the doubt if and only if the employer is sufficiently optimistic about the rate of human capital acquisition in the population from which the worker is drawn." | The same rule in words |
| KLB-Q12 | 10 | "Therefore, different groups may undertake differing equilibrium human capital investments, exhibit contrasting levels of employment in the more demanding job, and obtain different average returns to skill investment, despite having identical fundamentals with respect to investment cost and information technology." | Lemma 1, formalised as `lemma1` |
| KLB-Q13 | 11 | "We assume that employers have correct beliefs at each date about human capital acquisition within each population group. This implies that the collective reputation variable Πit indicates the true skill investment rate of the group i population in the temporary job market." | The consistency condition. The imputed group standing equals the group's actual share, the condition A1 imposes on visible group means |
| KLB-Q14 | 24 | "Additionally, in our model, given the assumption that wages are exogenously determined, inter-group interactions in the labor market are not fully examined." | The paper has no contact between members of different groups, so it says nothing about the puzzle A1 addresses |
| KLB-Q15 | 6 | "Imagine a large number of identical employers and a larger population of workers, with each employer randomly matched to many workers from this population." | Employers have no type of their own and meet workers at random. Nothing in the paper makes matching assortative |
| KLB-Q16 | 22 | "We assume that employers impose the favorable hiring standard θ̃ as long as the group’s collective reputation (belief about the average productivity of its members) remains below the threshold level" | The policy binds employers by assumption. The paper has no channel through which screening could move out of the policy's reach |
| KLB-Q17 | 16 | "Note that π o (π p ) increases (decreases) with the time discount" | The comparative statics in `δ`. The claim on `π°` is formalised as `piO_strictMono_delta`. The claim on `πᵖ` fails once `v*` moves with `δ` (KLB-D10) |

## Lean results

`lean/mathlib/KimLoury2014.lean`, in the paper's notation, with `Π` written
`pi` and `λ'` written `lam'` (both are reserved in Lean), and `A` standing
for `δ + λ'`.

| Theorem | What it checks |
|---|---|
| `KimLoury2014.beta_eq_gap` | Eq. (2) is the qualified wage minus the unqualified wage under the three-outcome test |
| `KimLoury2014.beta_zero` | `β(0) = ωPq` |
| `KimLoury2014.beta_one` | `β(1) = ωPu` |
| `KimLoury2014.beta_strictMono` | With `Pq < Pu` the return to investment rises with the benefit of the doubt |
| `KimLoury2014.piS_mem` | The threshold `Π*` lies in `(0, 1)` |
| `KimLoury2014.bod_iff` | The employer's expected return from Task One on an unclear result is non-negative iff `Π ≥ Π*` |
| `KimLoury2014.lemma1` | Lemma 1. Under its two conditions both `Π_l` and `Π_h` reproduce themselves |
| `KimLoury2014.lemma2_iff` | Lemma 2, eq. (7). The newborn invests iff the switch date satisfies `T ≤ (1/(δ+λ')) ln(·)` |
| `KimLoury2014.trap_iff` | Theorem 2, eq. (19). `Π_l < π°` iff `(Π_h − Π*)/(Π_h − Π_l) < v*^{λ'/(δ+λ')}` |
| `KimLoury2014.eq1` | Eq. (1), `λ + η = λ/n` |
| `KimLoury2014.eq8_quotient` | Eq. (8) rearranged into the difference quotient |
| `KimLoury2014.eq9` | Eq. (9), the limit `λ'(φ − Π)` |
| `KimLoury2014.eq3` | Eq. (3). The two integrals sum to the compressed return of eq. (4), with `ω' = ηω/(δ + λ)` |
| `KimLoury2014.eq4_R0` | `R({0}) = (ωPq + ω')/(δ + λ')` |
| `KimLoury2014.eq4_R1` | `R({1}) = (ωPu + ω')/(δ + λ')` |
| `KimLoury2014.eq6` | Eq. (6), the return with a switch date, from the integrals |
| `KimLoury2014.linear_unique` | The linear equations of the system have unique solutions |
| `KimLoury2014.eq10` | Eq. (10). The rising reputation hits `Π*` exactly at `T` |
| `KimLoury2014.eq13_switch` | Eq. (13) along a switch path |
| `KimLoury2014.eq14_switch` | Eq. (14) along a switch path |
| `KimLoury2014.steady_states` | Theorem 1 and Section 4.1. The steady states are exactly `Q_l` and `Q_h` |
| `KimLoury2014.eq15_return` | Eq. (15), the return side. The time from `(δ+λ')cm` to `ωPu + ω'` |
| `KimLoury2014.eq17` | Eq. (17). Equal times give `π°` |
| `KimLoury2014.eq10_down` | The falling reputation hits `Π*` exactly at `Tdown` |
| `KimLoury2014.eq16_return` | Eq. (16), the return side |
| `KimLoury2014.eq18` | Eq. (18). Equal times give `πᵖ` |
| `KimLoury2014.piO_lt_piStar` | `π° < Π*` |
| `KimLoury2014.piStar_lt_piP` | `Π* < πᵖ` |
| `KimLoury2014.opt_feasible_iff` | Lemma 3, lower boundary. From `π < Π*` the optimistic conjecture makes the newborn invest iff `π ≥ π°` |
| `KimLoury2014.pes_feasible_iff` | Lemma 3, upper boundary. From `π ≥ Π*` the pessimistic conjecture stops investment iff `π < πᵖ` |
| `KimLoury2014.prop1` | Proposition 1, eq. (11) |
| `KimLoury2014.prop2` | Proposition 2. `Π_l < π°` is eq. (11) |
| `KimLoury2014.cor1` | Corollary 1. Eq. (11) iff eq. (12) |
| `KimLoury2014.prop3` | Proposition 3 on `[Π_l, Π_h]`. The overlap is `[π°, πᵖ)` |
| `KimLoury2014.theorem2` | Theorem 2 with Definition 2. `Π_l` is trapped iff eq. (19) |
| `KimLoury2014.piO_mono_fixed_v` | With `v*` fixed, `π°` rises with `δ` |
| `KimLoury2014.piP_anti_fixed_v` | With `v*` fixed, `πᵖ` falls with `δ` |
| `KimLoury2014.vSd_strictMono` | `v*` rises with `δ` |
| `KimLoury2014.piO_strictMono_delta` | `π°` rises with `δ` when `v*` moves with `δ` |
| `KimLoury2014.piP_strictMono_delta` | `πᵖ` rises with `δ` when `v*` moves with `δ` and `ωPq > 0`, against the paper's claim (KLB-D10) |
| `KimLoury2014.piP_rises_example` | A numerical case satisfying Lemma 1 at both rates in which `πᵖ` rises with `δ` |
| `KimLoury2014.cor3_medium` | Corollary 3, the medium-cost worker |
| `KimLoury2014.cor3_workers` | Corollary 3, the low- and high-cost workers |
| `KimLoury2014.cor3_employers` | Corollary 3, the employers |
| `KimLoury2014.eq20` | Eq. (20), the target cost `c̃m` |
| `KimLoury2014.lemma4` | Lemma 4 with eqs. (21) and (22) |
| `KimLoury2014.betaHat_lt` | `β̂ < (δ+λ')cm − ω'` |
| `KimLoury2014.betaHat_gt_iff` | `ωPq < β̂` iff the group is trapped |
| `KimLoury2014.Pq_lt_Pu_iff` | `Pq < Pu` iff `θq + θu > θ̄` |
| `KimLoury2014.eq23` | Eq. (23) |
| `KimLoury2014.eq_below` | The return below `θq`, `θ̃ω/θu` |
| `KimLoury2014.eq23_anti` | The return falls with the standard on `(θq, θu)` |
| `KimLoury2014.eq24` | Eq. (24) |
| `KimLoury2014.eq25` | Eq. (25) |
| `KimLoury2014.prop5` | Proposition 5, including the patronizing outcome |
| `KimLoury2014.quota` | The quota standard `θ̃*` and its range |
| `KimLoury2014.sStd_antitone` | Eq. (26). `s(Π)` does not rise with `Π` |
| `KimLoury2014.betaGen_nonneg` | Appendix A. `ω(F_u − F_q) ≥ 0` |
| `KimLoury2014.JE_trace` | Lemma 5. The trace is `δ` |
| `KimLoury2014.JE_det` | Lemma 5. The determinant is `λ'(δ + λ')(β'φ' − 1)` |
| `KimLoury2014.lemma5_saddle` | Lemma 5. A negative determinant gives roots of opposite sign |
| `KimLoury2014.lemma5_source` | Lemma 5. A positive determinant and trace give no non-positive real root |
| `KimLoury2014.lemma5_det_neg_iff` | Lemma 5. The determinant is negative iff `β'φ' < 1` |
| `KimLoury2014.lemma6` | Lemma 6. The critical `λ'/δ` |
| `KimLoury2014.lemma6_no_real_root` | Lemma 6. A negative discriminant gives no real root |
| `KimLoury2014.lemma6_node` | Lemma 6. A positive discriminant gives two distinct positive roots |
| `KimLoury2014.eq27` | Eqs. (27) and (28). The change of variable to `v` |
| `KimLoury2014.eq27_vS` | `Vᵉ = (δ+λ')cm` is `v = v*` |
| `KimLoury2014.va_eq31` | The curve `v_a(v_c)` solves eq. (31) |
| `KimLoury2014.va'_eq34` | The curve `v_a'(v_c)` solves eq. (34) |
| `KimLoury2014.prop4_condition1` | Condition 1 of the proof of Proposition 4. The curves meet at `(v*, v*)` with slope `−(Π* − Π_l)/(Π_h − Π*)`, eqs. (35) and (36) |
| `KimLoury2014.control_beta_strictAnti` | Control. With `Pu < Pq` the return falls with the benefit of the doubt |
| `KimLoury2014.control_low_not_equilibrium_without_threshold` | Control. Without `Π_l < Π*`, `Π_l` does not reproduce itself |
| `KimLoury2014.control_low_not_equilibrium_cheap_cost` | Control. Without `(ωPq + ω')/(δ + λ') < cm`, `Π_l` does not reproduce itself |
| `KimLoury2014.control_steady_needs_threshold` | Control. Without `Π_l < Π*`, `(ωPq + ω', Π_l)` is not a steady state |
| `KimLoury2014.control_lemma2_cheap` | Control. Without `(δ+λ')cm > ωPq + ω'`, the newborn invests for every `T` |
| `KimLoury2014.control_no_trap` | Control. Without eq. (19), `Π_l` can recover |
| `KimLoury2014.control_cor3_needs_threshold` | Control. Without `Π* ≤ Π_h` the employers' ranking can fail |
| `KimLoury2014.control_lemma4_T_zero` | Control. At `T = 0` the standard has no effect on the return |
| `KimLoury2014.control_eq23_rises` | Control. Without `θq + θu > θ̄` the return rises with the standard |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Eq. (1) | `eq1` | general | |
| Eq. (2) | `beta_eq_gap`, `beta_strictMono` | general | |
| Threshold `Π*` (PDF p. 9, unnumbered) | `bod_iff`, `piS_mem` | general | |
| Eq. (3) | `eq3` | general | |
| Eq. (4) and `R({0})`, `R({1})` | `eq4_R0`, `eq4_R1` | general | |
| Lemma 1, eq. (5) | `lemma1` | general | |
| Eq. (6) | `eq6` | general | |
| Lemma 2, eq. (7) | `lemma2_iff` | general | |
| Eqs. (8), (9) | `eq8_quotient`, `eq9` | general | |
| Eq. (10) | `eq10`, `linear_unique` | general | |
| Proposition 1, eq. (11) | `prop1` | general | |
| Corollary 1, eq. (12) | `cor1` | general | |
| Eqs. (13), (14) | `eq13_switch`, `eq14_switch` | special case | Along paths whose flow return switches once, the only paths the paper uses. The general case needs the derivative of an improper integral in its lower limit |
| Theorem 1 | `steady_states` | special case | The steady states are characterised. The direction arrows of Figure 1 are not formalised |
| Eqs. (15), (17) | `eq15_return`, `eq17` | general | |
| Eqs. (16), (18) | `eq16_return`, `eq18` | general | |
| `∂π°/∂δ > 0` (PDF p. 16) | `piO_strictMono_delta` | general | In difference form |
| `∂πᵖ/∂δ < 0` (PDF p. 16) | `piP_anti_fixed_v`, `piP_strictMono_delta`, `piP_rises_example` | general | Holds with `v*` fixed and fails once `v*` moves with `δ` (KLB-D10) |
| Proposition 2 | `prop2` | general | |
| Definition 1, Lemma 3 | `opt_feasible_iff`, `pes_feasible_iff` | special case | For reputations in `[Π_l, Π_h]` and paths that switch at once. The clause "zero otherwise" depends on paths from below `Π_l`, which spiral (Proposition 4) |
| Proposition 3 | `prop3` | special case | On `[Π_l, Π_h]`, as for Lemma 3. The upper end is open (KLB-D11) |
| Definition 2, Theorem 2, eq. (19) | `theorem2`, `trap_iff` | general | |
| Proposition 4 | `prop4_condition1`, `va_eq31`, `va'_eq34` | special case | Condition 1 only. Condition 2 (eqs. 37, 38, concavity) and the step from the two conditions to the spiral are not formalised |
| Eqs. (29), (30), (32), (33) | none | not formalised | Separable integrations in each region of the phase plane, of the same form as eq. (17). Eqs. (31) and (34), which they yield, are taken as the definitions of the curves |
| Corollary 2 | none | not formalised | Needs second derivatives along the solution paths |
| Corollary 3 | `cor3_medium`, `cor3_workers`, `cor3_employers` | general | |
| Eq. (20) | `eq20` | general | |
| Eq. (21), Lemma 4, eq. (22) | `lemma4` | general | |
| `ωPq < β̂ < (δ+λ')cm − ω'` (PDF p. 22) | `betaHat_lt`, `betaHat_gt_iff` | general | The lower bound holds iff the group is trapped |
| Eq. (23) | `eq23`, `eq23_anti`, `Pq_lt_Pu_iff` | general | |
| Eqs. (24), (25) | `eq24`, `eq25`, `eq_below` | general | |
| Proposition 5 | `prop5` | general | |
| The quota (PDF pp. 23–24) | `quota` | general | |
| Eq. (26) | `sStd_antitone` | special case | For `Π < 1`. The value `s(1) = 0` is not formalised, since Lean sets `x/0 = 0` |
| Appendix A, hump shape of `β(Π)` and existence of a steady state | `betaGen_nonneg` | special case | Non-negativity only. The hump shape and the existence argument need assumptions on `F_q`, `F_u` and `G` that the paper does not state |
| Lemma 5 | `JE_trace`, `JE_det`, `lemma5_saddle`, `lemma5_source`, `lemma5_det_neg_iff` | special case | The linear algebra. The slope ordering at `Q_h` and `Q_l` is read from a figure, and the step from the linearisation to the nonlinear system is not formalised |
| Lemma 6 | `lemma6`, `lemma6_no_real_root`, `lemma6_node` | special case | The algebra of the discriminant. The step from the linearisation to the nonlinear system is not formalised |
| Eqs. (27), (28) | `eq27`, `eq27_vS` | general | |
| Eqs. (35), (36) | `prop4_condition1` | general | At the point `(v*, v*)`, which is where the proof uses them |
| Eqs. (37), (38) | none | not formalised | Part of Condition 2 |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| KLB-D1 | The version read is MPRA Paper 54950, title page dated 31 March 2014, cover dated 12 August 2012, posted 3 April 2014. It is cited as the 2014 working paper | The published version, *International Economic Review* (2018), is not read. Rows citing this record cite the working paper until the published version is read |
| KLB-D2 | MPRA Paper 18765 (2009), "Group Reputation and the Dynamics of Statistical Discrimination", is an earlier version of this paper | It is handled by a separate record and is not used here |
| KLB-D3 | On PDF pp. 22 and 23, eq. (23) and the expressions for `βτ(θ̃)` below `θq` write `w` for the wage `ω` | Read as `ω`. The algebra of (23) checks only with the wage of Task One |
| KLB-D4 | The proof of Lemma 5 (PDF p. 32) says "its transpose is trJE = δ" | Read as the trace, which is `(δ + λ') − λ' = δ` |
| KLB-D5 | The proof of Corollary 2 (PDF p. 30) writes `λ` and `δ + λ` where the system has `λ'` and `δ + λ'`, and labels the region-I slope with `IV` | Read with `λ'`, as in Theorem 1 and in the proof of Proposition 4 |
| KLB-D6 | Corollary 1, eq. (12), puts `δ` on the left, but `δ` also enters the right through `(δ + λ')cm` and `ω' = ηω/(δ + λ)` | Read as a restatement of eq. (11) and not as an explicit bound on `δ` |
| KLB-D7 | `Pq` and `Pu` are the probabilities that the test proves the worker qualified and unqualified, which Coate and Loury (1993) write `1 − pq` and `1 − pu` | Footnote 2 (PDF p. 8). The record uses the paper's own notation |
| KLB-D8 | Section 3.3 calls `e^{−η(τ−t)}` the probability of no revelation "over the time interval (t, τ − t)" | Read as the interval `(t, τ)` |
| KLB-D9 | The cost distribution `G` is `Π_l` on `(ε, cm)` and `Π_h` on `[cm, K)`, so `G(cm) = Π_h` | The Lean `G` puts the step at `cm` on the `Π_h` side, matching the paper's half-open intervals |
| KLB-D10 | The claim on PDF p. 16 that `πᵖ` decreases with `δ` holds only with `v*` held fixed | `v*` contains `(δ + λ')cm` and `ω' = ηω/(δ + λ)` and so rises with `δ`. With that dependence `πᵖ` rises with `δ` whenever `ωPq > 0` and Lemma 1 holds (`piP_strictMono_delta`, `piP_rises_example`). The claim on `π°` holds either way |
| KLB-D11 | The overlap of Proposition 3 is read as `[π°, πᵖ)` | Newborns invest at `V = (δ+λ')cm` because `G(cm) = Π_h` (KLB-D9), so the pessimistic path needs `V < (δ+λ')cm` strictly |
| KLB-D12 | Lemma 3's lower boundary "is zero otherwise" when `π° ≤ Π_l` | Reputations below `Π_l` rise even under pessimism, and the paper reaches them through the spiral of Proposition 4. The record formalises Lemma 3 on `[Π_l, Π_h]` only |
