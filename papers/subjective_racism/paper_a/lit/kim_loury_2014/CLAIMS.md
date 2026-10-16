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
| KLB-Q5 | 7 | "Once his true capabilities are revealed, he leaves the temporary job market and joins the permanent job market, where he secures a permanent job and is assigned to a task according to his qualifications: Task One, if qualified; Task Zero, otherwise." | Once the type is seen the group plays no part in assignment. This is assumed in a labour market, the analogue of A1's candidate test 4 in contact |
| KLB-Q6 | 7 | "If “age” is perfectly observable, a cohort’s investment decision is not affected by the group’s collective reputation." | Footnote 1. The reputation channel needs the pooling of KLB-Q4 |
| KLB-Q7 | 8 | "That is, the screening process is more effective in identifying the unqualified than the qualified." | The condition `Pq < Pu` behind `beta_strictMono` |
| KLB-Q8 | 8 | "When BOD is given, the expected net return from investing in human capital is ωPu , and ωPq otherwise, given a wage rate of ω for Task One." | Eq. (2), derived from the signal structure in `beta_eq_gap` |
| KLB-Q9 | 8 | "It is notable that if the opposite assumption holds (0 < Pu < Pq < 1), the expected net return to the skill investment βτ (ξτ ) is decreasing in the probability that the worker receives the benefit of the doubt" | Formalised as `control_beta_strictAnti` |
| KLB-Q10 | 9 | "That is, the benefit of the doubt is given only when the “collective reputation” of the identity group to which a worker belongs is equivalent to or exceeds some threshold level Π∗" | The employer's rule, formalised as `bod_iff`. It depends on the group's share of qualified workers only, with no term for the employer's own type and none for the group's spread |
| KLB-Q11 | 9 | "Thus, a worker receives the benefit of the doubt if and only if the employer is sufficiently optimistic about the rate of human capital acquisition in the population from which the worker is drawn." | The same rule in words |
| KLB-Q12 | 10 | "Therefore, different groups may undertake differing equilibrium human capital investments, exhibit contrasting levels of employment in the more demanding job, and obtain different average returns to skill investment, despite having identical fundamentals with respect to investment cost and information technology." | Lemma 1, formalised as `lemma1` |
| KLB-Q13 | 11 | "We assume that employers have correct beliefs at each date about human capital acquisition within each population group. This implies that the collective reputation variable Πit indicates the true skill investment rate of the group i population in the temporary job market." | The consistency condition. The imputed group standing equals the group's actual share, the condition A1 imposes on visible group means |
| KLB-Q14 | 24 | "Additionally, in our model, given the assumption that wages are exogenously determined, inter-group interactions in the labor market are not fully examined." | The paper has no contact between members of different groups, so nothing in it bears on A1's engagement decision directly |

## Lean results

`lean/mathlib/KimLoury2014.lean`, in the paper's notation, with `Π` written
`pi` and `λ'` written `lam'` (both are reserved in Lean).

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
| `KimLoury2014.control_beta_strictAnti` | Control. With `Pu < Pq` the return falls with the benefit of the doubt |
| `KimLoury2014.control_low_not_equilibrium_without_threshold` | Control. Without `Π_l < Π*`, `Π_l` does not reproduce itself |
| `KimLoury2014.control_low_not_equilibrium_cheap_cost` | Control. Without `(ωPq + ω')/(δ + λ') < cm`, `Π_l` does not reproduce itself |

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
