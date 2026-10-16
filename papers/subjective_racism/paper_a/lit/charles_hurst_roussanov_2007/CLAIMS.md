# Charles, Hurst and Roussanov (2007), claims

Evidence tag [F]: the full text of NBER Working Paper 13392 (September 2007),
65 PDF pages, was read on 16 October 2026 from its text layer, with the page
images of PDF pages 22, 23, 44, 45 and 62 to 65 read where the extraction
garbles equations or figures. The published version (QJE 2009) is not read
(CH-D1). Pages are PDF pages. The printed page number is the PDF page minus 2
on pp. 3 to 38.

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

## Lean results

`lean/mathlib/CharlesEtAl2007.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `CharlesEtAl2007.s_c_mu` | Theory Appendix. With `s = g(c)/μ_G`, the derivative in `μ_G` of the marginal status return `g'(c)/μ_G` is `−g'(c)/μ_G²` |
| `CharlesEtAl2007.s_c_mu_neg` | That derivative is negative when `g'(c) > 0` |
| `CharlesEtAl2007.eq4_sign` | Eq. (4). With the bracket negative, `c*_μ < 0` exactly when `s_cμ < 0` |
| `CharlesEtAl2007.no_status_share` | Theory Appendix. The no-status first order condition holds at the share `α/(1 + α)` |
| `CharlesEtAl2007.F_strictMonoOn` | `F(x) = −ln(1 − x) − α ln x` rises on `[α/(1 + α), 1)` |
| `CharlesEtAl2007.share_decreasing_in_mu` | For the solution the appendix states, the visible share falls in `μ_G` at fixed income and minimum income |
| `CharlesEtAl2007.chain_rule_gap` | The appendix's change of variables differs from the chain rule whenever `x g' w' ≠ 0` (CH-D2) |
| `CharlesEtAl2007.control_status_gain_rising` | Control. With `s_cμ > 0`, Ireland's case, visible consumption rises in the group mean |
| `CharlesEtAl2007.control_soc_needed` | Control. Without the second order condition, `s_cμ < 0` gives visible consumption rising in the group mean |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| CH-D1 | The version read is NBER Working Paper 13392 (September 2007), cited as such. The QJE (2009) article is not read | The cached file's title page. Coefficients and wording may differ in the published version, and nothing is attributed to it |
| CH-D2 | The Theory Appendix's ODE in the share `x` drops a factor. With `c = x w(x)` and `w = g(c)`, the chain rule gives `w' = g'(c) w / (1 − x g'(c))`, while the appendix writes `w' = μ_G (1/(1 − x) − α/x)`, which is `g'(c) w` | The appendix treats `dc/dx` as `y` at fixed income. `chain_rule_gap`. The stated solution still gives a share falling in `μ_G` (`share_decreasing_in_mu`). Whether the corrected ODE gives the same sign is not checked here |
| CH-D3 | The dispersion regressor is read only by its sign | The text (p. 29) calls it the variance and describes the log standard deviation relative to the mean. The note to Table 6 (CH-Q15) divides the log standard deviation by the log mean |
| CH-D4 | The coefficients on log mean group income are elasticities. "Doubling mean state income ... reduces visible expenditures ... by 94 percent" (p. 28) is the authors' approximation | In a log-log regression a doubling multiplies spending by `2^{−0.94}`, about 0.52. No row relies on the percentages |
| CH-D5 | Typos not relied on | The last row of Table 7 is labelled for White men in a table of Black and Hispanic households; eq. (4) writes `s_cc(c*, μ)` without the subscript `G`; footnote 26 has "Ireland (!994)"; p. 15 compares ages 26 to 29 with a base of 18 to 30; Figure 2f prints a slope of −0.003 with a t-statistic of 2.94 |
| CH-D6 | Moav and Neeman (2012, p. 937) describe this paper as showing that the racial gap in visible spending "increases with the average wage gap between races" and that "race provides a signal of income" | The working paper read tests visible spending against the own-group mean and models race as the reference group. The description may fit the QJE version, which is not read. Recorded in `lit/moav_neeman_2012/` as MN-D5 |
