# Kim and Loury (2009), claims

Evidence tag [F]. The full text of MPRA Paper No. 18765, version of November
12, 2009, PDF pp. 1 to 42, was read on 16 October 2026, and the Lean
implementation was extended to the whole model on the same day. Pages are PDF pages,
page 1 being the MPRA cover. Quotations are checked against the PDF's text
layer.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| KLA-Q1 | 3 | "Group reputation is defined as the average characteristics of the group members, which is shared by principals." | The group's mean, and nothing else about its distribution, is what employers carry. No variance term enters (contribution 1) |
| KLA-Q2 | 3 | "The essential point is that an individual’s reputation is influenced by the reputation of the group to which he belongs, when his personal records are insufficient to clarify his qualification for the task." | Unseen type inferred from the group in a labour market, with individual evidence alongside. Paper A's marker carries no individual evidence |
| KLA-Q3 | 6 | "Imagine a large number of identical employers and a larger population of workers." | The observer has no type, so no observer-type dependence of the preference over groups can arise (contribution 1) |
| KLA-Q4 | 7 | "However, the static model does not provide an explanation for where the employers’ prior beliefs come from, and why employers start to have different beliefs about different social groups." | The authors' motive for the dynamics. Paper A, which has no law of motion, does not take this route |
| KLA-Q5 | 8 | "Instead of relying on the employers’ prior beliefs, we propose that employers use the objective information about the overall quality of each group in their decision to set up the hiring standard applied to a group." | The imputed group standing equals the actual proportion qualified, read from the aggregate signal distribution (eq. 2). A consistency condition on group means, imposed as an informational assumption rather than derived as a static fixed point |
| KLA-Q6 | 11 | "If the investment of workers can be easily confirmed, workers do not have to worry that their chance to be assigned to a good job is affected by their group’s reputation." | The group penalty vanishes when type is shown, stated in words for a labour market (contribution 4). Formalised as `KimLoury2009.revealed_type_no_group_effect` |
| KLA-Q7 | 11 | "If they give BOD to a group, the hiring standard for the group is θu , but, if not, the hiring standard for the group is θq ." | The standards are swapped relative to the paper's own eq. (10) derivation (KLA-D1) |
| KLA-Q8 | 12 | "Note that the latter is not consistent with the hiring standard rule defined in equation (1)" | The middle steady state `Qm` needs a mixed assignment (`KimLoury2009.Qm_needs_mixing`). `Qh` and `Ql` are steady under pure assignment (`KimLoury2009.two_steady_states`) |
| KLA-Q9 | 14 | "all economic agents, including employers and workers with different investment costs, are better off when the group state" | Proposition 3, the employer half formalised as `KimLoury2009.employer_prefers_Qh` |
| KLA-Q10 | 19 | "Note that we have assumed that the underlying characteristics of the two groups are identical." | Group disparity arises with identical primitives, as in Coate and Loury (1993) |
| KLA-Q11 | 19 | "However, this never happens in the competitive situation, which is defined as the market condition in which there are numerous employers and the size of each employer is relatively insignificant." | Section 5. Only coordinated employers move a trapped group. Not a venue or a price that screens on type (contribution 3) |
| KLA-Q12 | 22 | "White residents may use the overall quality of the move-in minority group in their calculation of the expected housing price in the future" | A residential setting with group reputation, proposed as an application and not modelled (contribution 3) |
| KLA-Q13 | 23 | "the overall quality of alumni determines the collective reputation, and the reputation affects the quality of entering students and their willingness to pay the tuition" | An institution whose reputation and price depend on its members, proposed as further work and not modelled (contribution 3) |
| KLA-Q14 | 25 | "Therefore, if the market can learn the true characteristics of workers very quickly, there will be no group disparity caused by the difference of initial group reputations or statistical discrimination practice in the market." | Appendix A. Revelation of type removes the group disparity, through the threshold `KimLoury2009.market_learning_threshold` (contribution 4) |
| KLA-Q15 | 29 | "Consequently, its transpose is trJE = δ and the determinant is" | "Transpose" is read as trace (KLA-D4). Formalised as `KimLoury2009.jacobian_trace_det` |
| KLA-Q16 | 23 | "The policies may include colorblind hiring enforcement, quota system and asymmetric training subsidy." | A group-neutral rule is named as a remedy, with no analysis of what it does to group-correlated signals (the shadow market) |
| KLA-Q17 | 23 | "The examination of those policies in the given dynamic framework are left for the further research." | The colorblind rule of KLA-Q16 is not modelled |
| KLA-Q18 | 15 | "even high investment cost individuals who will never invest for the job qualification are better off in this state." | A low type gains from the reputation of the group it belongs to, in the contracting channel. Formalised as `KimLoury2009.prop3_workers` |
| KLA-Q19 | 9 | "Suppose that a worker is randomly matched with employers every period" | Matching is random. No assortative matching or homophily is assumed or derived |
| KLA-Q20 | 19 | "depending on the initial reputation levels of groups, the gap between two groups can grow or shrink over time." | Each group's reputation evolves on its own. Group composition of a setting plays no role |

## Lean results

`lean/mathlib/KimLoury2009.lean`, in the paper's notation, with `pi`, `piL`,
`piH`, `piS` for `Π`, `Πl`, `Πh`, `Π*` because Lean reserves `Π`, `rp` for
`ρ′ = λ/(δ + λ)`, `l` for `λ`, `s` for `δ + λ` and `K` for `(δ + λ)cm`.

| Theorem | What it checks |
|---|---|
| `KimLoury2009.assumption1_iff` | Assumption 1. `Pq < Pu` iff `θ̄ < θq + θu` |
| `KimLoury2009.bod_iff` | Pp. 10–11. The payoff of BOD is nonnegative iff `Π ≥ Π*` |
| `KimLoury2009.piStar_uniform` | P. 11. `Π* = (θ̄ − θq)/(ρθu + θ̄ − θq)` in the uniform case |
| `KimLoury2009.beta_values` | P. 11. `β = ωPu` with BOD and `ωPq` without |
| `KimLoury2009.beta_mono` | Eq. (10). With `Pq ≤ Pu` the benefit of investing is nondecreasing in reputation |
| `KimLoury2009.R_strictMono` | P. 8. Individual reputation, eq. (3), is strictly increasing in group reputation |
| `KimLoury2009.two_steady_states` | P. 11. `Qh` and `Ql` are steady states when `Π* ∈ (Πl, Πh)` and `(δ + λ)cm ∈ (ωPq, ωPu)` |
| `KimLoury2009.employer_prefers_Qh` | Footnote 3 and Proposition 3. Per-period profit is higher at `Qh` than at `Ql` |
| `KimLoury2009.jacobian_trace_det` | Lemma 4. Trace `δ`, determinant `λ(δ + λ)(β′φ′ − 1)` |
| `KimLoury2009.spiral_iff` | Lemma 5. The discriminant is negative iff `(1 + λ/δ)λ/δ > 1/(4(φ′β′ − 1))` |
| `KimLoury2009.market_learning_threshold` | Appendix A. `Ql` is not a steady state iff `η > η*` |
| `KimLoury2009.revealed_type_no_group_effect` | When the signal reveals type, the benefit of investing does not depend on reputation |
| `KimLoury2009.sStar_antitone` | Eq. (1). `s*` is nonincreasing in `Π` |
| `KimLoury2009.sStar_one` | P. 5. `s*(1) = 0` |
| `KimLoury2009.sStar_mem` | P. 5. `0 ≤ s*(Π) ≤ θ̄` when some signal meets the standard |
| `KimLoury2009.betaTilde_ends` | P. 6. `G(β̃(0)) = G(β̃(θ̄)) = G(0)` |
| `KimLoury2009.prop1` | Proposition 1. Three distinct steady states, by the intermediate value theorem |
| `KimLoury2009.Fq_le_Fu_uniform` | P. 5. `Fq ≤ Fu`, uniform case |
| `KimLoury2009.eq2` | Eq. (2). The proportion qualified is recovered from the group's signal distribution |
| `KimLoury2009.eq4_constant` | Eq. (4) with constant `β` gives `V = β` |
| `KimLoury2009.footnote2` | Footnote 2. `W = ω/(δ + λ)` |
| `KimLoury2009.V_path` | Eq. (5) on an interval of constant `β` |
| `KimLoury2009.reputation_path` | Eq. (7) with constant `φ`, and the paths of section 5 |
| `KimLoury2009.theorem1_loci` | Theorem 1, the demarcation loci (9) |
| `KimLoury2009.Qm_needs_mixing` | P. 11 and footnote 1. `Qm` is not a steady state under pure assignment |
| `KimLoury2009.v_transform` | Section 3.3.1. The system in `v` |
| `KimLoury2009.vStar_iff` | P. 12. `V = (δ + λ)cm` iff `v = v*` |
| `KimLoury2009.prop2_Qh` | Proposition 2 and Definition 1. `Qh` is economically stable |
| `KimLoury2009.prop2_Ql` | Proposition 2 and Definition 1. `Ql` is economically stable |
| `KimLoury2009.region_invariant` | Eqs. (15), (16), (18), (19). The invariant of a path within a region |
| `KimLoury2009.eq17` | Eq. (17) from (15) and (16) |
| `KimLoury2009.eq20` | Eq. (20) from (18) and (19) |
| `KimLoury2009.lemma1_spiral_out` | Lemma 1. With `δ > 0` a path returns farther from `Qm` than it left |
| `KimLoury2009.lemma2_right` | Lemma 2. Paths are concave right of `v*` |
| `KimLoury2009.lemma2_left` | Lemma 2. Paths are convex left of `v*` |
| `KimLoury2009.prop3_workers` | Proposition 3. Every worker type is better off at `Qh` |
| `KimLoury2009.piO_on_path` | P. 15. `π^o` lies on the optimistic path through `(1, Π*)` |
| `KimLoury2009.piP_on_path` | P. 15. `π^p` lies on the pessimistic path through `(0, Π*)` |
| `KimLoury2009.lemma3_o` | Lemma 3, corrected (KLA-D2). `π^o > Πl` iff `ρ′(−ln v*) < ln(Πh − Πl) − ln(Πh − Π*)` |
| `KimLoury2009.lemma3_p` | Lemma 3, corrected (KLA-D2). `π^p < Πh` iff `ρ′(−ln(1 − v*)) < ln(Πh − Πl) − ln(Π* − Πl)` |
| `KimLoury2009.eq12` | Eq. (12), the size of the overlap |
| `KimLoury2009.prop4_overlap_rp` | Proposition 4. The overlap grows with `ρ′` |
| `KimLoury2009.prop4_delta` | Proposition 4. The overlap shrinks as `δ` rises |
| `KimLoury2009.prop4_lambda` | Proposition 4. The overlap grows as `λ` rises |
| `KimLoury2009.prop4_cm` | Proposition 4. `π^o` and `π^p` rise with `cm` |
| `KimLoury2009.jacobian_charpoly` | Lemma 4. The characteristic polynomial |
| `KimLoury2009.lemma4_saddle` | Lemma 4. A negative determinant gives roots of opposite sign |
| `KimLoury2009.lemma4_source` | Lemma 4. A positive determinant with trace `δ > 0` gives two positive roots |
| `KimLoury2009.lemma4_det_neg_iff` | Lemma 4. The determinant is negative iff `β′φ′ < 1` |
| `KimLoury2009.lemma5_roots` | Lemma 5. The printed roots solve the characteristic equation |
| `KimLoury2009.prop5_threshold_path` | P. 19. With `Π*′` the optimistic path reaches `(v*, Πl)` |
| `KimLoury2009.prop5_tb` | P. 19. `t_b − t_0 = −ln v*/(δ + λ)` |
| `KimLoury2009.prop5_subsidy_path` | P. 19. With `v*′` the optimistic path reaches `(v*′, Πl)` |
| `KimLoury2009.prop5_subsidy_size` | P. 19. `S′ = (v* − v*′)ω(Pu − Pq)/(δ + λ)` |
| `KimLoury2009.prop5_tc` | P. 20. Reputation reaches `Π*` at `t_c` |
| `KimLoury2009.prop5_TC` | P. 20. The cost of the subsidy |
| `KimLoury2009.YQl_eq` | P. 18. `Y_{Ql} = Pq Πl xq / r` |
| `KimLoury2009.prop5` | Proposition 5 as the coordinated employers' choice |
| `KimLoury2009.appA_revelation_value` | Appendix A. The lifetime value of revelation, `ηw/((δ + λ)(δ + λ + η))` |
| `KimLoury2009.appA_locus_shifts` | Appendix A. `∂Ṽ/∂η > 0` |
| `KimLoury2009.appA_piP_decreasing` | Appendix A and its proof. `∂π^p/∂η < 0` |
| `KimLoury2009.control_no_low_state_if_cutoff_low` | Control for `two_steady_states`. With `(δ + λ)cm < ωPq` the low state is not a steady state |
| `KimLoury2009.control_equal_informativeness` | Control for `beta_mono`. With `Pq = Pu` reputation does not move the benefit of investing |
| `KimLoury2009.control_employer_ranking_needs_threshold` | Control for `employer_prefers_Qh`. With `Πh < Π*` profit at `Ql` exceeds profit at `Qh` |
| `KimLoury2009.control_prop2_needs_vStar` | Control for Proposition 2. With `v* > 1` the high state is not a rest point |
| `KimLoury2009.control_lemma1_vortex` | Control for Lemma 1, and the paper's remark. With `δ = 0` the path closes, `va′ = va` |
| `KimLoury2009.control_lemma2_sign` | Control for Lemma 2. With the wrong sign of the constant the path is not concave |
| `KimLoury2009.control_prop3_medium` | Control for Proposition 3. The medium type gains only if `(δ + λ)cm < ω` |
| `KimLoury2009.control_prop4_degenerate` | Control for Proposition 4. With `Π* = Πl = Πh` the overlap does not move |
| `KimLoury2009.control_lemma4` | Control for Lemma 4. With a positive determinant there is no saddle |
| `KimLoury2009.control_lemma5` | Control for Lemma 5. With `β′φ′ ≤ 1` the discriminant is nonnegative and there is no spiral |

## Coverage of the source's results

| Result | Lean name | Status | Reason for anything short of general |
|---|---|---|---|
| Eq. (1) and its properties, p. 5 | `sStar_antitone`, `sStar_one`, `sStar_mem` | general | `s*(0) ≤ θ̄` needs a signal that meets the standard, which the paper's `min` presumes |
| `ψ` nonincreasing implies `Fq ≤ Fu`, p. 5 | `Fq_le_Fu_uniform` | special case | Uniform signals of section 3.3. The general implication needs densities and integration |
| Investment rule and `G(β̃(0)) = G(β̃(θ̄))`, p. 6 | `Invests`, `betaTilde_ends` | general | |
| Proposition 1 | `prop1` | general | The boundary conditions at `0` and `θ̄`, left by the paper to "checking", are stated as hypotheses (KLA-D10) |
| More informative signals tend to a unique steady state, p. 6 | | not formalised | A tendency, not a precise claim |
| Eq. (2) | `eq2` | general | |
| Eq. (3) and `∂R/∂Π > 0` | `R_strictMono` | general | |
| Eq. (4) | `eq4_constant` | special case | Constant `β`. A general `β` needs its integrability on `[t, ∞)` |
| Eq. (5) | `V_path` | special case | `β` constant on the interval. Every path of the simple model is of this kind piece by piece |
| Eqs. (6), (7) | `reputation_path` | special case | Constant `φ`. The discrete approximation (6) and the integral form of `Π_t` are not formalised |
| Theorem 1 | `theorem1_loci` | general | |
| Assumption 1 | `assumption1_iff` | general | |
| `Π*`, pp. 10–11 | `bod_iff`, `piStar_uniform` | general | |
| Eq. (10) | `beta_values`, `beta_mono` | general | |
| Eq. (11) | `phi` | general | Definition, simple model |
| Steady states `Qh`, `Ql`, p. 11 | `two_steady_states` | general | |
| `Qm` and footnote 1 | `Qm_needs_mixing` | general | |
| Section 3.3.1, `v_t` and `v*` | `v_transform`, `vStar_iff` | general | |
| Definition 1 | `EconStable` | general | Definition |
| Lemma 1 | `lemma1_spiral_out`, `region_invariant`, `eq17`, `eq20` | general | Proved by tangent lines to `x^{ρ′}` rather than by the paper's conditions 1 and 2. The crossing points `a`, `c`, `a′` of Appendix Figure 2 enter as hypotheses |
| Remark on `δ = 0`, p. 26 | `control_lemma1_vortex` | general | |
| Lemma 2 | `lemma2_right`, `lemma2_left` | general | The path in each region is in the closed form that `region_invariant` gives |
| Proposition 2 | `prop2_Qh`, `prop2_Ql` | general | |
| Proposition 3 and footnote 3 | `prop3_workers`, `employer_prefers_Qh` | general | |
| Definitions 2, 3 | `Overlap`, `IsTrap` | general | Definitions |
| `π^o`, `π^p`, p. 15 | `piO_on_path`, `piP_on_path` | general | |
| Lemma 3 | `lemma3_o`, `lemma3_p` | special case | Proved in the form that follows from the printed `π^o` and `π^p`. The printed condition is not proved (KLA-D2) |
| Eq. (12) | `eq12` | general | |
| Proposition 4 | `prop4_overlap_rp`, `prop4_delta`, `prop4_lambda`, `prop4_cm` | general | Stated as strict monotonicity, the content of the signs of the partial derivatives |
| Lemma 4 | `jacobian_trace_det`, `jacobian_charpoly`, `lemma4_saddle`, `lemma4_source`, `lemma4_det_neg_iff` | special case | Linear algebra at a steady state. That `β′φ′ < 1` at `Qh`, `Ql` and `> 1` at `Qm` is read from Figure 5 and enters as a hypothesis. Linearisation itself (Hartman and Grobman) is not in Mathlib |
| Lemma 5 | `spiral_iff`, `lemma5_roots` | general | For the linearised system |
| Theorem 2 | | not formalised | Rests on the stable manifold theorem and on the phase diagram of Figure 5, neither available |
| `Π*′` and `t_b`, p. 19 | `prop5_threshold_path`, `prop5_tb` | general | |
| `v*′` and `S′`, p. 19 | `prop5_subsidy_path`, `prop5_subsidy_size` | general | |
| `t_c` and `TC`, p. 20 | `prop5_tc`, `prop5_TC` | general | |
| `Y_{Ql}`, p. 18 | `YQl_eq` | general | |
| Eqs. (13), (14) | | not formalised | The integrals are defined in the text and not evaluated there. Only their parts `YQl_eq` and `prop5_TC` are |
| Proposition 5 | `prop5` | general | As a choice rule, which is all the proposition states |
| Footnote 2 | `footnote2` | general | |
| Appendix A, lifetime payoff | `appA_revelation_value` | general | |
| Appendix A, `V̇` with learning | `Vlocus` | special case | Only the locus. The differential equation is `V_path` with `δ + λ + η` |
| Appendix A, `∂Ṽ/∂η > 0` | `appA_locus_shifts` | general | |
| Appendix A, `Πh`, `Πl` rise with `η` | | not formalised | General model, read from Appendix Figure 1 |
| Appendix A, `∂π^p/∂η < 0` | `appA_piP_decreasing` | general | |
| Appendix A, `η*` | `market_learning_threshold` | general | |
| Footnote 4 | | not formalised | States that a sign is unclear |
| Section 4 and section 6 | | not formalised | Interpretive, no numbered claim |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| KLA-D1 | With BOD the standard is `θq`, without it `θu`, the reverse of KLA-Q7 | Assignment is to signals above the standard, and p. 12 computes the BOD benefit as `ω[Fu(θq) − Fq(θq)]` |
| KLA-D2 | Lemma 3's condition (p. 16) is not used. From the printed `π^o`, `π^p`, separation needs `−ln v*·λ/(δ+λ) < ln(Πh − Πl) − ln(Πh − Π*)` and `−ln(1 − v*)·λ/(δ+λ) < ln(Πh − Πl) − ln(Π* − Πl)` | The printed form has one denominator and subtracts 1 in the numerator, which the two conditions do not give |
| KLA-D3 | `v* = ((δ + λ)cm − ωPq)/(ω(Pu − Pq))`, as on p. 13 | P. 20 prints `wPu` in the numerator, which contradicts `V = ωPq + ω(Pu − Pq)v` |
| KLA-D4 | "transpose" on p. 29 is read as trace | `trJE = δ` is the trace of the printed matrix |
| KLA-D5 | At `Π = Π*` BOD is given | Eq. (10) puts `Π*` in the BOD interval, while p. 13 sets `ξ = 1` only for `Π > Π*`. The tie has no bearing |
| KLA-D6 | `w`, `W` and `ω` all denote the task-one wage, `W` its static lifetime value | The paper switches between them (pp. 6, 9, 19–21) |
| KLA-D7 | The middle steady state `Qm` is formalised only as failing under pure assignment, `KimLoury2009.Qm_needs_mixing` | Footnote 1 (KLA-Q8). It needs a mixed assignment, which eq. (1) does not allow |
| KLA-D8 | The version read is dated November 12, 2009 | The MPRA cover says 1 May 2009 and posted 20 Nov 2009. The title page date is used |
| KLA-D9 | MPRA 54950 (2014), "Collective Reputation and the Dynamics of Statistical Discrimination", is a later version with its own record. The *International Economic Review* (2018) article is not read | A search of the 2014 text layer finds no "monopolistic", "tipping", "alumni" or "market learning", while "benefit of the doubt" and "subsid" occur. The monopolistic principals of section 5, the housing and college applications (KLA-Q12, KLA-Q13) and the market learning of Appendix A are therefore taken as particular to this version, on a search and not a read |
| KLA-D10 | Proposition 1 is read with `G(β̃(0)) < ψ(0)/(ρ + ψ(0))` and `G(β̃(θ̄)) > ψ(θ̄)/(ρ + ψ(θ̄))` added | Three crossings need the boundary signs. The paper says only "Checking the boundary conditions" (p. 7) |
