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

## Lean results

`lean/mathlib/Eguia2013.lean`, in the paper's notation with `l` for `λ`.

| Theorem | What it checks |
|---|---|
| `Eguia2013.eq3_sA` | Eq. (3). The first form of `s_A(s)` equals its closed form, the mean skill of the members of `A` |
| `Eguia2013.eq3_sD` | Eq. (3). The same for `s_D(s)` |
| `Eguia2013.total_mean` | The masses of `A` and `D` times their mean skills add to 1/2. Assimilation moves the group means and not the population mean |
| `Eguia2013.sA_gt_half` | With `λ > 0` and `0 < s < 1`, assimilation raises the mean skill of `A` above 1/2 |
| `Eguia2013.sD_lt_half` | With the same conditions, it lowers the mean skill of `D` below 1/2 |
| `Eguia2013.eAbar_lt` | It dilutes the mean endowment of `A` |
| `Eguia2013.sA_hasDerivAt` | The derivative of `s_A` printed on p. 32 is correct |
| `Eguia2013.sA_deriv_one` | At `s = 1` that derivative is `−λ/2` (EG-D1) |
| `Eguia2013.eq6_at_one` | Eq. (6) at `s = 1` gives `−λ/2`, which differs from the printed `(−λ − λ²)/2` for `λ > 0` (EG-D1) |
| `Eguia2013.screen_exists` | With a cost strictly decreasing in skill, some `d ≥ 0` admits a more skilled agent and turns away a less skilled one |
| `Eguia2013.control_lump_sum_no_screen` | Control. A cost that does not depend on skill admits everyone or no one |
| `Eguia2013.control_no_mobility` | Control. With `λ = 0` both groups keep mean skill 1/2, so `sD_lt_half` needs `λ > 0` |
| `Eguia2013.control_cutoff_zero` | Control. With `s = 0` the mean skill of `A` stays 1/2, so `sA_gt_half` needs `s > 0` |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| EG-D1 | On p. 32, `∂s_A/∂s` at `s = 1` is read as `−λ/2`, not the printed `(−2λ(1 + λ) + λ)/2`, which the next line simplifies to `(−λ − λ²)/2` | Eq. (3) and the paper's own eq. (6) both give `−λ/2` (`sA_deriv_one`, `eq6_at_one`). The bound below it should read `e_𝒜 < (1/2)(∂v/∂s_A)/(∂v/∂e_A)` rather than `(1 + λ)/2` times the ratio. The claim that `s = 1` is not optimal for low enough `e_𝒜` is unaffected |
| EG-D2 | Proposition 1's conditions are read as one statement | p. 11 states `λ ≤ λ_c` and `c′/c < c_λ` with `c_λ ∈ ℝ`, while p. 29 states `λ < λ_c` and `−c′/c > c_λ` with `c_λ ∈ ℝ₊₊`. The two differ in sign convention and the boundary case only |
| EG-D3 | In Example 4, `U_𝒟` is the average over `𝒟` and `U_𝒟−` the average over `θ ≤ 1/2` | p. 19 names `U_𝒜, U_𝒟−, U_𝒟+`, and the table on p. 20 reports `U_𝒜, U_𝒟, U_𝒟−`. Not relied on |
| EG-D4 | On p. 33, `dv(s_i, ω_A, s_A)/ds` is read with `e_A` for `ω_A`. On p. 36, "a decrease in θ_D" is read as a decrease in `s_D` | Typos. Not relied on |
| EG-D5 | The version read is the working paper of 10 September 2013, titled "Discrimination and Assimilation", not the 2017 *JPubE* article "Discrimination and assimilation at school" | The title page. Rows that cite the published article carry an unverified mark until it is read |
| EG-D6 | The three generalizations (pp. 27–29) are attributed only as the verbal claims printed there | Their formal statements are in an add-on file (footnote 18) that was not read |
