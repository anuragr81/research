# Kim and Loury (2009), claims

Evidence tag [F]. The full text of MPRA Paper No. 18765, version of November
12, 2009, PDF pp. 1 to 42, was read on 16 October 2026. Pages are PDF pages,
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
| KLA-Q8 | 12 | "Note that the latter is not consistent with the hiring standard rule defined in equation (1)" | The middle steady state `Qm` needs a mixed assignment. Only `Qh` and `Ql` are formalised (`KimLoury2009.two_steady_states`) |
| KLA-Q9 | 14 | "all economic agents, including employers and workers with different investment costs, are better off when the group state" | Proposition 3, the employer half formalised as `KimLoury2009.employer_prefers_Qh` |
| KLA-Q10 | 19 | "Note that we have assumed that the underlying characteristics of the two groups are identical." | Group disparity arises with identical primitives, as in Coate and Loury (1993) |
| KLA-Q11 | 19 | "However, this never happens in the competitive situation, which is defined as the market condition in which there are numerous employers and the size of each employer is relatively insignificant." | Section 5. Only coordinated employers move a trapped group. Not a venue or a price that screens on type (contribution 3) |
| KLA-Q12 | 22 | "White residents may use the overall quality of the move-in minority group in their calculation of the expected housing price in the future" | A residential setting with group reputation, proposed as an application and not modelled (contribution 3) |
| KLA-Q13 | 23 | "the overall quality of alumni determines the collective reputation, and the reputation affects the quality of entering students and their willingness to pay the tuition" | An institution whose reputation and price depend on its members, proposed as further work and not modelled (contribution 3) |
| KLA-Q14 | 25 | "Therefore, if the market can learn the true characteristics of workers very quickly, there will be no group disparity caused by the difference of initial group reputations or statistical discrimination practice in the market." | Appendix A. Revelation of type removes the group disparity, through the threshold `KimLoury2009.market_learning_threshold` (contribution 4) |
| KLA-Q15 | 29 | "Consequently, its transpose is trJE = δ and the determinant is" | "Transpose" is read as trace (KLA-D4). Formalised as `KimLoury2009.jacobian_trace_det` |

## Lean results

`lean/mathlib/KimLoury2009.lean`, in the paper's notation, with `pi`, `piL`,
`piH` for `Π`, `Πl`, `Πh` because Lean reserves `Π`.

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
| `KimLoury2009.control_no_low_state_if_cutoff_low` | Control. With `(δ + λ)cm < ωPq` the low state is not a steady state |
| `KimLoury2009.control_equal_informativeness` | Control. With `Pq = Pu` reputation does not move the benefit of investing |
| `KimLoury2009.control_employer_ranking_needs_threshold` | Control. With `Πh < Π*` profit at `Ql` exceeds profit at `Qh` |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| KLA-D1 | With BOD the standard is `θq`, without it `θu`, the reverse of KLA-Q7 | Assignment is to signals above the standard, and p. 12 computes the BOD benefit as `ω[Fu(θq) − Fq(θq)]` |
| KLA-D2 | Lemma 3's condition (p. 16) is not used. From the printed `π^o`, `π^p`, separation needs `−ln v*·λ/(δ+λ) < ln(Πh − Πl) − ln(Πh − Π*)` and `−ln(1 − v*)·λ/(δ+λ) < ln(Πh − Πl) − ln(Π* − Πl)` | The printed form has one denominator and subtracts 1 in the numerator, which the two conditions do not give |
| KLA-D3 | `v* = ((δ + λ)cm − ωPq)/(ω(Pu − Pq))`, as on p. 13 | P. 20 prints `wPu` in the numerator, which contradicts `V = ωPq + ω(Pu − Pq)v` |
| KLA-D4 | "transpose" on p. 29 is read as trace | `trJE = δ` is the trace of the printed matrix |
| KLA-D5 | At `Π = Π*` BOD is given | Eq. (10) puts `Π*` in the BOD interval, while p. 13 sets `ξ = 1` only for `Π > Π*`. The tie has no bearing |
| KLA-D6 | `w`, `W` and `ω` all denote the task-one wage, `W` its static lifetime value | The paper switches between them (pp. 6, 9, 19–21) |
| KLA-D7 | The middle steady state `Qm` is not formalised | Footnote 1 (KLA-Q8). It needs a mixed assignment |
| KLA-D8 | The version read is dated November 12, 2009 | The MPRA cover says 1 May 2009 and posted 20 Nov 2009. The title page date is used |
| KLA-D9 | MPRA 54950 (2014), "Collective Reputation and the Dynamics of Statistical Discrimination", is a later version with its own record. The *International Economic Review* (2018) article is not read | A search of the 2014 text layer finds no "monopolistic", "tipping", "alumni" or "market learning", while "benefit of the doubt" and "subsid" occur. The monopolistic principals of section 5, the housing and college applications (KLA-Q12, KLA-Q13) and the market learning of Appendix A are therefore taken as particular to this version, on a search and not a read |
