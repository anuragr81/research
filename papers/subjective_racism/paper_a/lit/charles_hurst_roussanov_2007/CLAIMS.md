# Charles, Hurst and Roussanov (2007), claims

Evidence tag [F]: the full text of NBER Working Paper 13392 (September 2007),
65 PDF pages, was read on 16 October 2026 from its text layer, with the page
images of PDF pages 22, 23, 44, 45 and 62 to 65 read where the extraction
garbles equations or figures. The published version (QJE 2009) is not read
(CH-D1). Pages are PDF pages. The printed page number is the PDF page minus 2
on pp. 3 to 38. For the second pass on 16 October 2026, PDF pages 4, 7, 11,
19, 25, 27, 36, 37 and 44 were rechecked for the added quotations.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CH-Q1 | 3 | "few easily observable variables are as strongly correlated with economic status as is an individual's race" | The motivation treats race as correlated with economic status. The model does not use race as a signal of income (CH-Q2, CH-Q3) |
| CH-Q2 | 22 | "outsiders have two bits of information: the person's visible consumption, and what is known about the income of the group from which the person is drawn" | The observer sees the individual's consumption and knows the group's income. In A1 the observer sees the marker only |
| CH-Q3 | 22 | "In equilibrium, the inferences made about a person's income are correct, so that observed levels of visible consumption are associated with particular income levels." | The signalling equilibrium separates. Income is read from consumption, not imputed from the group |
| CH-Q4 | 22 | "In general, equilibrium status can also depend on higher moments of the income distribution, such as the variance. [...] In addition, variance of group income will matter in a pure signaling model, but its contribution is hard to disentangle from that of the mean." | Footnote 24. The group variance is named and left out of the model |
| CH-Q5 | 22 | "presents a model in which simply belonging to a richer reference group confers higher status" | Footnote 25, on Ireland (1994). The sign of the group-mean effect on status is a modelling choice, `control_status_gain_rising` |
| CH-Q6 | 23 | "in which the status conferred upon an individual is simply how his income compares (or is thought to compare) to the average income of the group" | The group mean is the yardstick of status, giving `s_c_mu` and `eq4_sign` |
| CH-Q7 | 25 | "race partly determines a person's reference group" | Race enters as the reference group, defined as race by state |
| CH-Q8 | 29 | "the estimate on the variance is negative, which implies that a more disperse population weakens the propensity to consume visibly, all else equal." | Whites, Table 6 column 4. The dispersion measure is uncertain (CH-D3) |
| CH-Q9 | 30 | "The relative standard deviation measure is positive, but not statistically different from zero." | Blacks and Hispanics, Table 7 column 6. The dispersion sign is not the same across groups |
| CH-Q10 | 31 | "our control for reference group income explains nearly the entire gap in spending across races." | Table 8. The race gap in visible spending vanishes once the own-group mean income is controlled |
| CH-Q11 | 32 | "The only role plaid by race is in determining one's reference group." | The authors' reading of Table 8, with the typo "plaid" in the source |
| CH-Q12 | 36 | "The items we study are probably most important for random, anonymous interactions in society and not for status forthcoming from interaction with friends, family members or colleagues." | The setting is anonymous display to outsiders, not chosen contact |
| CH-Q13 | 44 | "because income is not observed, status will depend not on own income but on society's inference about income, given the person's [...] observable consumption." | Theory Appendix. Status is `g(c)/μ_G` |
| CH-Q14 | 45 | "is increasing in individual income and decreasing in the mean income of the social group" | The appendix's conclusion, formalised for the stated solution as `share_decreasing_in_mu` |
| CH-Q15 | 51 | "Log relative standard deviation is computed as the log standard deviation of income for White men in a state divided by the log of mean income of White men in a state." | The note to Table 6, which differs from the text's description (CH-D3) |
| CH-Q16 | 25 | "we argue that the long- [...] standing cultural separation of races in the U.S, combined with high levels of residential segregation, suggest that race partly determines a person's reference group" | The grouping is assumed from separation and segregation, not derived |
| CH-Q17 | 36 | "people's reference groups consist merely of those of who live around them, and that social networks are truly color blind" | The authors leave open whether reference groups are racial or local |
| CH-Q18 | 36 | "Also interesting is the issue of the degree to which people choose their neighbors with an eye to satisfying the status considerations that we have identified." | The choice of reference group is left open |
| CH-Q19 | 19 | "Blacks spend 17 percent less on education, approximately 50 percent less on entertainment, and 56 percent less on health spending." | At equal total expenditure, budgets are allocated differently by race (Table 5) |
| CH-Q20 | 4 | "Blacks consume less than Whites in essentially every other expenditure category (aside from housing) to maintain higher visible consumption." | Visible spending is taken from the rest of the budget |
| CH-Q21 | 44 | "suppose that if personal income were known, status would simply be "relative income"" | With income shown, the group mean still enters status, `known_income_status` |
| CH-Q22 | 11 | "Our concern is that racial differences in housing expenditure might derive from differential treatment in the housing market" | Discrimination in a contracting market is set aside, not modelled |
| CH-Q23 | 37 | "participation in means-tested social programs, if observable, is a signal associated with being a poor type, giving rise to a stigma attached to such programs." | An observable correlated with class becomes a signal of class |
| CH-Q24 | 7 | "gap between Blacks and Whites, conditional on permanent income, declines by 50 percent after controlling for visible spending." | Not matched by Table 10 (CH-D7) |
| CH-Q25 | 27 | "housing expenditure differences explain no more than twenty percent of the negative relationship between visible expenditures and mean state income" | Checked against Figures 2a and 2c (CH-D8) |

## Lean results

`lean/mathlib/CharlesEtAl2007.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `CharlesEtAl2007.eq3_foc` | Eq. (3). An interior optimum of eq. (2) has `s_c = u'(y − c*) − α u'(c*)` |
| `CharlesEtAl2007.status_raises_c` | The first implication of eq. (3). With `s_c > 0`, visible consumption exceeds the no-status level |
| `CharlesEtAl2007.own_income_increasing` | The second implication of eq. (3). Visible consumption rises in own income |
| `CharlesEtAl2007.eq4_derivation` | Eq. (4) from differentiating eq. (3) in `μ_G` |
| `CharlesEtAl2007.eq4_sign` | Eq. (4). With the bracket negative, `c*_μ < 0` exactly when `s_cμ < 0` |
| `CharlesEtAl2007.s_c_mu` | Theory Appendix. With `s = g(c)/μ_G`, the derivative in `μ_G` of `g'(c)/μ_G` is `−g'(c)/μ_G²` |
| `CharlesEtAl2007.s_c_mu_neg` | That derivative is negative when `g'(c) > 0` |
| `CharlesEtAl2007.appendix_foc` | Theory Appendix. The derivative of the appendix's objective |
| `CharlesEtAl2007.appendix_ode` | Theory Appendix. The ODE `g'(c) = μ_G (1/(g(c) − c) − α/c)` |
| `CharlesEtAl2007.no_status_share` | Theory Appendix. The no-status first order condition holds at the share `α/(1 + α)` |
| `CharlesEtAl2007.chain_rule_gap` | The appendix's change of variables differs from the chain rule whenever `x g' w' ≠ 0` (CH-D2) |
| `CharlesEtAl2007.appendix_solution` | The stated solution has the derivative the appendix writes |
| `CharlesEtAl2007.appendix_K` | The initial condition fixes `K` |
| `CharlesEtAl2007.F_strictMonoOn` | `F(x) = −ln(1 − x) − α ln x` rises on `[α/(1 + α), 1)` |
| `CharlesEtAl2007.share_decreasing_in_mu` | For the stated solution, the visible share falls in `μ_G` |
| `CharlesEtAl2007.share_increasing_in_income` | For the stated solution, the visible share rises in income |
| `CharlesEtAl2007.known_income_status` | With income known, status `y/μ_G` still falls in the group mean |
| `CharlesEtAl2007.fn12_lower_bound` | Footnote 12. Lower saving by Blacks biases the Black coefficient down |
| `CharlesEtAl2007.ovb` | Table 8. The short coefficient is the long one plus `δ` times the coefficient of the race dummy on log group income |
| `CharlesEtAl2007.table8_implied` | Table 8. The implied coefficient of the race dummy on log own-group income is `−41/52` |
| `CharlesEtAl2007.doubling_reduction_lt` | Doubling the group mean reduces spending by less than `−δ` (CH-D4) |
| `CharlesEtAl2007.numbers_table1` | The income and spending ratios of p. 7 |
| `CharlesEtAl2007.numbers_visible_dollars` | `$7,204` and `$2,300` |
| `CharlesEtAl2007.numbers_samples` | The sample sizes that add up |
| `CharlesEtAl2007.numbers_table3_mismatch` | The sample sizes of Table 3 that do not (CH-D9) |
| `CharlesEtAl2007.numbers_fig2` | The slopes of Figures 2a and 2c and the share removed by housing (CH-D8) |
| `CharlesEtAl2007.numbers_table9` | The changes in Table 9 quoted on pp. 31–32 |
| `CharlesEtAl2007.numbers_diversion` | The dollar diversions of p. 32 and "nearly sixty percent" |
| `CharlesEtAl2007.numbers_table10` | The two "60 percent" shares of p. 33 |
| `CharlesEtAl2007.control_status_gain_rising` | Control. With `s_cμ > 0`, Ireland's case, visible consumption rises in the group mean |
| `CharlesEtAl2007.control_soc_needed` | Control. Without the second order condition, `s_cμ < 0` gives visible consumption rising in the group mean |
| `CharlesEtAl2007.control_status_negative` | Control. With `s_c < 0` visible consumption falls below the no-status level |
| `CharlesEtAl2007.control_concavity_needed` | Control. Without `u'' < 0` visible consumption falls in own income |
| `CharlesEtAl2007.control_fn12_no_bias` | Control. With equal saving there is no bias |
| `CharlesEtAl2007.control_ovb_no_channel` | Control. With `δ = 0` the short and long coefficients coincide |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Eq. (1), the specification | `Spec`, `Spec.predict` | general | A definition. The estimates need the CEX microdata, which are not available here |
| Survey, Tables A2 and A3 | none | not formalised | Survey data |
| Eq. (2) | `objective` | general | A definition |
| Eq. (3) | `eq3_foc` | general | At an interior local maximum with differentiable `u` and `s` |
| The two implications of eq. (3) | `status_raises_c`, `own_income_increasing` | general | The second given the implicit derivative of eq. (3) in `y` |
| Eq. (4) | `eq4_derivation`, `eq4_sign` | general | Given the implicit derivative of eq. (3) in `μ_G` |
| `s_cμ < 0` for relative status | `s_c_mu`, `s_c_mu_neg` | general | For `s = g(c)/μ_G` |
| Footnote 24, variance | none | not formalised | A verbal remark. The model has no variance term |
| Footnote 25, Ireland's case | `control_status_gain_rising` | general | |
| Eqs. (5), (6), (7) | `Spec`, `Spec.predict` | general | Definitions. Estimates not reproduced |
| Footnote 12, lower bound | `fn12_lower_bound` | special case | A linear model of permanent income and expenditure |
| Table 8, the vanishing race gap | `ovb`, `table8_implied` | special case | Population covariance algebra and the printed numbers. The regression is not reproduced |
| The "doubling" readings | `doubling_reduction_lt` | general | |
| Numbers that follow from printed numbers | `numbers_table1`, `numbers_visible_dollars`, `numbers_samples`, `numbers_table3_mismatch`, `numbers_fig2`, `numbers_table9`, `numbers_diversion`, `numbers_table10` | general | |
| Theory Appendix, first order condition and ODE | `appendix_foc`, `appendix_ode` | general | |
| Theory Appendix, change of variables | `chain_rule_gap` | general | Shows the step is not the chain rule (CH-D2) |
| Theory Appendix, solution and constant | `appendix_solution`, `appendix_K`, `no_status_share` | general | `no_status_share` is the first order condition, not the maximum |
| Theory Appendix, summary | `share_decreasing_in_mu`, `share_increasing_in_income` | special case | For the stated solution. For the solution of the correct ODE not formalised |
| Status with income known | `known_income_status` | general | |
| Tables 2 to 10 and Figures 1, 2a to 2f, the estimates | none | not formalised | Data |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| CH-D1 | The version read is NBER Working Paper 13392 (September 2007), cited as such. The QJE (2009) article is not read | The cached file's title page. Coefficients and wording may differ in the published version, and nothing is attributed to it |
| CH-D2 | The Theory Appendix's ODE in the share `x` drops a factor. With `c = x w(x)` and `w = g(c)`, the chain rule gives `w' = g'(c) w / (1 − x g'(c))`, while the appendix writes `w' = μ_G (1/(1 − x) − α/x)`, which is `g'(c) w` | The appendix treats `dc/dx` as `y` at fixed income. `chain_rule_gap`. The stated solution still gives a share falling in `μ_G` (`share_decreasing_in_mu`). Whether the corrected ODE gives the same sign is not checked here |
| CH-D3 | The dispersion regressor is read only by its sign | The text (p. 29) calls it the variance and describes the log standard deviation relative to the mean. The note to Table 6 (CH-Q15) divides the log standard deviation by the log mean |
| CH-D4 | The coefficients on log mean group income are elasticities. "Doubling mean state income ... reduces visible expenditures ... by 94 percent" (p. 28) is the authors' approximation | In a log-log regression a doubling multiplies spending by `2^{−0.94}`, about 0.52, and the exact reduction is below `−δ` (`doubling_reduction_lt`). No row relies on the percentages |
| CH-D5 | Typos not relied on | The last row of Table 7 is labelled for White men in a table of Black and Hispanic households; eq. (4) writes `s_cc(c*, μ)` without the subscript `G`; footnote 26 has "Ireland (!994)"; p. 15 compares ages 26 to 29 with a base of 18 to 30; Figure 2f prints a slope of −0.003 with a t-statistic of 2.94; the Hispanic education coefficient is −0.86 in Table 5 and −0.85 in Table 9 |
| CH-D6 | Moav and Neeman (2012, p. 937) describe this paper as showing that the racial gap in visible spending "increases with the average wage gap between races" and that "race provides a signal of income" | The working paper read tests visible spending against the own-group mean and models race as the reference group. The description may fit the QJE version, which is not read. Recorded in `lit/moav_neeman_2012/` as MN-D5 |
| CH-D7 | The effect of visible spending on the wealth gap is read from Table 10 | The introduction says the gap "declines by 50 percent after controlling for visible spending" (CH-Q24). Table 10 controls for own-group mean income, not visible spending, and its numbers give about 60 percent (`numbers_table10`) |
| CH-D8 | Housing removes about 23.5 percent of the slope of Figure 2a | The text says "no more than twenty percent" (CH-Q25). The printed slopes, 0.017 and 0.013, give `4/17` (`numbers_fig2`) |
| CH-D9 | The sample sizes of Table 3's education and age rows are not used | Within ages 18 to 49 the age rows sum to 60,539 and the education rows to 55,521, not to the 48,758 of the sample (`numbers_table3_mismatch`) |
