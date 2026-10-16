# Fang and Moro (2010), claims

Evidence tag [F]: the full text of all 78 PDF pages of NBER working paper
15860 (April 2010) was read on 16 October 2026, with PDF pages 9, 27, 57
and 66 also read from 110 dpi images. Pages are PDF pages; PDF page `n` is
printed page `n − 4` from PDF page 5. The text layer keeps the ligatures `ﬁ`, `ﬀ`,
`ﬂ`, and quotations copy them as they are (FM-D7).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| FM-Q1 | 5 | "But, sometimes the group characteristics can also be endogenously chosen, such as club membership or language." | The survey allows the marker to be chosen, the setting of A1's passing |
| FM-Q2 | 7 | "The concept of statistical discrimination has been applied mostly to labor market examples where employers discriminate against one group of workers." | The scope of the literature surveyed. Contact and social settings are not its subject |
| FM-Q3 | 8 | "on average workers of the two groups receive the same average wage, unless average productivity is assumed to be exogenously diﬀerent as in Case 1" | In Phelps's Case 2 a noisier signal gives no average penalty without a further ingredient |
| FM-Q4 | 8 | "This assumption can be rationalized assuming some communication of language barriers between employers and minorities, see, e.g., Lang (1996)." | Footnote 2. The only place the survey ties signal noise to the pair of employer and worker groups. The noise depends on the pair, not the employer's preference (FM-D5) |
| FM-Q5 | 9 | "Aigner and Cain (1977) proposed to incorporate employers' risk aversion into the standard Phelps' setup." | Section 2.2.1, the variance penalty through risk aversion |
| FM-Q6 | 9 | "Employers are compensated for the risk factor incorporated in each" | "... B worker's higher uncertainty about productivity, measured by the term cVar(q\|θ)/2". `condVar_strictMono_noise` |
| FM-Q7 | 10 | "Their observation was that if one group has a more informative signal, then this group's variance of the expected productivity is higher." | Cornell and Welch (1996). In selection at the top, the group with the less noisy signal wins, a variance effect in selection with no risk aversion |
| FM-Q8 | 29 | "This paper therefore shows how search friction might generate group inequality even when employers have perfect information about their workers and would strictly prefer to hire workers from the discriminated group." | Mailath, Samuelson and Shaked (2000). Firms search only one group, and the group penalty survives full information, the rival prediction to contribution 4 |
| FM-Q9 | 33 | "a skilled worker is more willing than an unskilled one to endure disutility from activity" | Fang (2001). A chosen, a priori irrelevant activity becomes a marker of skill in equilibrium |
| FM-Q10 | 34 | "This parameterization allows the model to investigate the relationship between integration and discrimination. It shows that in some cases integration makes it impossible for negative stereotypes to be sustained." | Chaudhuri and Sethi (2008). Integration enters through peer effects on the cost of skill, not through contact choices |
| FM-Q11 | 35 | "Altonji and Pierret (1997) proposed and implemented a test of statistical discrimination based on the eﬀect of race on worker wages over time with employer learning." | Footnote 12. The labour-market test that the group effect falls as the individual becomes known, the counterpart of contribution 4 (FM-D6) |
| FM-Q12 | 36 | "This paper generates endogenous segregation in a dynamic environment where partners randomly match to play a repeated prisoner's dilemma game." | Eeckhout (2006), the one marriage-market model in the survey |
| FM-Q13 | 58 | "Vertically diﬀerentiated colleges compete for desirable students using ﬁnancial aid and admission policies." | Epple, Romano and Sieg (2008). Venues that screen on ability with prices, with income and race in the colleges' objective |
| FM-Q14 | 68 | "everybody is paid a wage equal to the average productivity in the pool of workers." | Schwab (1986). A pool valued at its average type, with the marginal worker leaving it |
| FM-Q16 | 8 | "then the signal provides a precise estimate of the worker's ability." | With a precise signal the group mean drops out of eq. (1), `phelps_precise_signal` |
| FM-Q17 | 23 | "sustaining extreme segregation in equilibrium against large groups may be diﬃcult" | Moro and Norman. Group size, and so the composition of a setting, changes what segregation can be sustained [C] |
| FM-Q18 | 24 | "given skills, color does not play any additional role in explaining racial wage diﬀerences once we control for racial diﬀerences in their skill investment decisions" | In the information-friction models the group penalty is gone once skill is known |
| FM-Q19 | 36 | "Fang and Loury (2005a, 2005b) explored a theory of dysfunctional collective identity in a repeated risk sharing game." | Footnote 13. Risk sharing within a group, the survey's only touch on shared resources [C] |
| FM-Q20 | 44 | "the ﬁrms continue to view black workers as less productive in equilibrium and adopt a patronizing assignment policy on the blacks in order to fulﬁll the aﬃrmative action employment quotas." | Coate and Loury. A quota on outcomes is met while the stereotype persists, `patron_eq51` |
| FM-Q21 | 45 | "In the simple task, workers are therefore paid above the marginal product if they belong to the dominant group, and below their marginal product if they belong to the discriminated group." | Moro and Norman (2003a). Under a quota on assignment the group difference moves into the wage [C] |
| FM-Q22 | 47 | "companies may use variables that are correlated with race to evade the imposition of policies that monitor the employment process, such as aﬃrmative action" | Footnote 19, Lundberg (1991). Correlated variables substitute for a monitored criterion [C] |
| FM-Q23 | 53 | "the State of Texas passed a law guaranteeing entry to any state university of a student's choice if they ﬁnished in the top 10% of their graduating class." | A group-neutral rule on a variable correlated with group, adopted after a ban on the use of race |
| FM-Q24 | 56 | "is coming from a black applicant is given by the likelihood ratio" | Chan and Eyster. Under a colour-blind rule the office values a score partly for the group it signals, `ce_eq58` [C] |
| FM-Q15 | 71 | "we still have a relatively poor theoretical understanding on the evolution of stereotypes" | The survey's own statement of a gap, outside A1's static scope |

## Lean results

`lean/mathlib/FangMoro2010.lean`, in the survey's notation, one section per
model the survey presents.

| Theorem | Section | What it checks |
|---|---|---|
| `FangMoro2010.eq1_weights_sum` | 2.1 | Eq. (1). The weights on the signal and on the group mean sum to one |
| `FangMoro2010.phelps_case1` | 2.1 | Case 1. Equal variances, `μ_B < μ_W`: lower `B` wage at every signal |
| `FangMoro2010.phelps_case2` | 2.1 | Case 2. A noisier `B` signal: `B` paid less exactly above the mean |
| `FangMoro2010.phelps_case2_average` | 2.1 | Case 2. Equal average wages |
| `FangMoro2010.phelps_precise_signal` | 2.1 | With `σ_εj² = 0` the group mean drops out, `E(q\|θ) = θ` |
| `FangMoro2010.control_phelps_noisy_signal` | 2.1 | Control. With noise and spread the group mean keeps a positive weight |
| `FangMoro2010.control_phelps_case2_equal_noise` | 2.1 | Control. Equal noise, no crossing |
| `FangMoro2010.condVar_strictMono_noise` | 2.2.1 | Section 2.2.1. `Var(q\|θ)` rises with the noise when `σ_j² > 0` |
| `FangMoro2010.control_condVar_zero_spread` | 2.2.1 | Control. With `σ_j² = 0` the conditional variance is zero whatever the noise |
| `FangMoro2010.aignerCain_average_wage` | 2.2.1 | Section 2.2.1. The average wage `μ − (c/2)Var(q\|θ)` falls with the noise |
| `FangMoro2010.control_survey_exponent` | 2.2.1 | Control. The printed exponent `(c/2)Var` ranks prospects differently from eq. (8)'s `(c²/2)Var` (FM-D11) |
| `FangMoro2010.ls_eq2` | 2.2.2 | Eq. (2). `X* = (b/c)σ²/(σ² + σ_εj²)` maximises the worker's objective |
| `FangMoro2010.ls_noisier_invests_less` | 2.2.2 | The noisier group invests less and earns less on average |
| `FangMoro2010.control_ls_uninformative` | 2.2.2 | Control. With an uninformative signal there is no investment |
| `FangMoro2010.cw_variance` | 2.2.3 | Cornell and Welch. The variance of expected productivity is higher for the more informative group |
| `FangMoro2010.arrow_wage_relation` | 3.1 | Arrow. The zero-profit conditions give the displayed wage relation |
| `FangMoro2010.arrow_lower_wage` | 3.1 | Arrow. `π_B < π_W` gives `w_B < w_W` |
| `FangMoro2010.control_arrow_equal_shares` | 3.1 | Control. Equal shares skilled, equal wages |
| `FangMoro2010.post_strictMono_prior` | 3.2 | Eq. (4). The posterior rises with the prior `π` |
| `FangMoro2010.postL_eq_post` | 3.2 | Eq. (4) in terms of the likelihood ratio `l` |
| `FangMoro2010.cl_posterior_mono_signal` | 3.2 | PDF p. 11. Under MLRP the posterior rises with the signal |
| `FangMoro2010.eq7_iff` | 3.2 | Eqs. (6) and (7). Assignment iff `f_q/f_u ≥ (1 − π)x_u/(π x_q)` |
| `FangMoro2010.control_uninformative_signal` | 3.2 | Control. With `f_q = f_u` the posterior is the prior |
| `FangMoro2010.cl_eq8_unique` | 3.2 | Eq. (8). The threshold is unique under MLRP |
| `FangMoro2010.cl_eq9` | 3.2 | Eq. (9) in sign. A higher prior lowers the threshold |
| `FangMoro2010.cl_eq12` | 3.2 | Eqs. (10) to (12). Invest iff `c ≤ [F_u − F_q]ω` |
| `FangMoro2010.cl_I_endpoints` | 3.2 | PDF p. 13. `I(0) = I(1) = 0` |
| `FangMoro2010.cl_eq13` | 3.2 | Eq. (13). `I′ = ω(f_u − f_q)`, positive iff `l < 1` |
| `FangMoro2010.cl_trivial_equilibrium` | 3.2 | Eqs. (15), (16). With `G(0) = 0`, `(θ̃, π) = (1, 0)` is an equilibrium |
| `FangMoro2010.control_cl_trivial` | 3.2 | Control. With `G(0) > 0` it is not |
| `FangMoro2010.mn_eq18` | 4.1 | Eq. (18). The first-order condition equates the marginal products at the threshold |
| `FangMoro2010.mn_wage_at_threshold` | 4.1 | Eq. (19). The wage is continuous at the threshold |
| `FangMoro2010.mn_eq20` | 4.1 | Eq. (20). If the best `B` signal is assigned to the simple task, every `B` signal is |
| `FangMoro2010.mss_eq24_25` | 4.2 | Eqs. (24), (25) from the value equations and the unprinted firm equation (FM-D12) |
| `FangMoro2010.mss_HB_lt_HW` | 4.2 | Eqs. (26), (27). `V_B < V_W` gives `H_B < H_W` |
| `FangMoro2010.mss_VB_lt_VW` | 4.2 | Eqs. (32), (33). `V_B < V_W` when `γ_F > 0` |
| `FangMoro2010.control_mss_no_firm_search` | 4.2 | Control. With `γ_F = 0` the two values are equal |
| `FangMoro2010.fang_low_cost` | 4.3 | Fang (2001). The low-cost type's choice rule, threshold `Ṽ_q` |
| `FangMoro2010.fang_high_cost` | 4.3 | Fang (2001). The high-cost type's choice rule, threshold `Ṽ_u` |
| `FangMoro2010.fang_eq38` | 4.3 | Eq. (38) and single crossing |
| `FangMoro2010.control_fang_eq38` | 4.3 | Control. If `W_q − W_u < c_l` single crossing fails |
| `FangMoro2010.fang_fixed_point_necessary` | 4.3 | `Δ ≥ 0` is necessary for an `A`-cultural equilibrium |
| `FangMoro2010.fang_pareto` | 4.3 | Welfare. With `W_u ≥ 1` every worker does at least as well as in the non-cultural equilibrium |
| `FangMoro2010.cs_peer_gap` | 4.4 | Chaudhuri and Sethi. The peer-quality gap is `η(π_B − π_W)`, zero at full integration |
| `FangMoro2010.cl_aa_sufficient` | 6.2.1 | Eq. (41). A strictly decreasing `ρ̂` makes the quota force equal thresholds and beliefs |
| `FangMoro2010.patron_eq47` | 6.2.2 | Eq. (47). An unclear score is assigned iff `π ≥ π̂` |
| `FangMoro2010.patron_eq48_49` | 6.2.2 | Eqs. (48), (49). The incentives under the liberal and conservative policies |
| `FangMoro2010.patron_eq50` | 6.2.2 | Eq. (50) and footnote 17. The patronising probability meets the quota; the return is `(1 − α)π_l` |
| `FangMoro2010.patron_eq51` | 6.2.2 | Eq. (51). Solutions `π_B = π_l` and `π_B = 1 − π_l` |
| `FangMoro2010.patron_dynamics_slope` | 6.2.2 | PDF p. 40. The patronising fixed point is locally stable iff `π_l > 1/2` |
| `FangMoro2010.control_patron_half` | 6.2.2 | Control. At `π_l = 1/2` the two solutions coincide |
| `FangMoro2010.mnaa_threshold_order` | 6.3 | Section 6.3. Under the quota `π_B < π_W` gives `θ̂_B < θ̂_W` (FM-D13) |
| `FangMoro2010.control_mnaa_uninformative` | 6.3 | Control. An uninformative signal does not separate thresholds |
| `FangMoro2010.fn_pihat` | 6.4 | Fang and Norman. `π̂_h`, `π̂_l` solve `w_θ(π̂_θ) = g` |
| `FangMoro2010.fn_eq54` | 6.4 | Eq. (54) from the payoffs after each signal |
| `FangMoro2010.fn_eq55` | 6.4 | Eq. (55). The derivatives in `ρ` |
| `FangMoro2010.fn_precision` | 6.4 | PDF p. 46. `pβ < g < 4p(1 − p)β` is possible iff `p < 3/4` |
| `FangMoro2010.control_fn_precision` | 6.4 | Control. At `p = 3/4` it is not |
| `FangMoro2010.chung_fn20` | 6.5 | Footnote 20. The quadratic is the fixed-point condition for `q*` |
| `FangMoro2010.chung_role_model` | 6.5 | PDF p. 48. A first complex placement lowers the belief in the bad state, `q_n < q*` |
| `FangMoro2010.ce_eq58` | 6.6.2 | Eqs. (58), (59). The colour-blind objective with `γ(t)` |
| `FangMoro2010.ce_share_decreasing` | 6.6.2 | PDF p. 52. Under Assumption 2 the diversity term of `γ` falls with `t` |
| `FangMoro2010.ce_foc` | 6.6.2 | PDF p. 51. The colour-sighted first-order condition `t_B + α/C = t_W` |
| `FangMoro2010.fl_colorsighted` | 6.6.3 | Eqs. (69), (70). The colour-sighted `τ_B` and slot clearing |
| `FangMoro2010.fl_eq79` | 6.6.3 | Eqs. (75), (78), (79) |
| `FangMoro2010.pool_variance` | 7.1 | Section 7.1. The law of total variance for the pooled population |
| `FangMoro2010.control_survey_pooled_variance` | 7.1 | Control. The survey's pooled variance is not the law of total variance (FM-D3) |
| `FangMoro2010.eff_total_product` | 7.1 | Section 7.1. Total product is `μ` with or without the use of race |
| `FangMoro2010.ls_net_product` | 7.1 | PDF p. 63. The net social product `(b²/c) w (1 − w/2)` |
| `FangMoro2010.control_ls_printed_net_product` | 7.1 | Control. The printed `(b²/c)(1 − w/2)` differs (FM-D14) |
| `FangMoro2010.norman_example` | 7.2.1 | PDF pp. 65–66. Norman's example |
| `FangMoro2010.norman_eq82` | 7.2.1 | Eq. (82) |
| `FangMoro2010.mn_pareto_chain` | 7.2.2 | PDF p. 67. The step of the Pareto ranking, weak form (FM-D15) |

## Coverage of the source's results

The survey numbers equations (1) to (82) and states results in prose. Each
model it presents has rows for its numbered equations and displayed claims.
Claims about a model rest on the survey's account (FM-D10).

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Eq. (1) and Case 1 | `FangMoro2010.phelps_case1` | general | |
| Case 2, crossing and equal averages | `FangMoro2010.phelps_case2`, `FangMoro2010.phelps_case2_average` | general | |
| Precise signal, PDF p. 8 | `FangMoro2010.phelps_precise_signal` | general | |
| Section 2.2.1, `E(U\|θ)` and `Var(q\|θ)` | `FangMoro2010.condVar_strictMono_noise`, `FangMoro2010.aignerCain_average_wage` | general | The normal expectation itself is in `AignerCain1977.eq8`; the survey's exponent is recorded as FM-D11 |
| Eq. (2), Lundberg and Startz | `FangMoro2010.ls_eq2`, `FangMoro2010.ls_noisier_invests_less` | general | |
| Cornell and Welch, variance of expected productivity | `FangMoro2010.cw_variance` | general | |
| Cornell and Welch, the dominant group fills the top slots | none | not formalised | A tournament limit stated in words |
| Arrow, the wage relation and eq. (3) | `FangMoro2010.arrow_wage_relation`, `FangMoro2010.arrow_lower_wage` | general | Eq. (3) is the definition `ArrowEquilibrium` |
| Arrow, multiple equilibria | none | not formalised | Stated in words |
| Eqs. (4) to (7) | `FangMoro2010.post_strictMono_prior`, `FangMoro2010.eq7_iff` | general | |
| Eq. (8) | `FangMoro2010.cl_eq8_unique` | general | |
| Eq. (9) | `FangMoro2010.cl_eq9` | special case | The sign of the derivative, from monotonicity; the derivative formula needs differentiability of `l`, not assumed |
| Eqs. (10) to (12), (14) | `FangMoro2010.cl_eq12` | general | |
| Eq. (13) and `I(0) = I(1) = 0` | `FangMoro2010.cl_eq13`, `FangMoro2010.cl_I_endpoints` | general | |
| Eqs. (15) to (17), the trivial equilibrium | `FangMoro2010.cl_trivial_equilibrium` | general | `CLEquilibrium` |
| Multiple interior equilibria by construction; Pareto ranking | none | not formalised | Constructed in words with Fig. 2 |
| Moro and Norman, eqs. (18) to (20) | `FangMoro2010.mn_eq18`, `FangMoro2010.mn_wage_at_threshold`, `FangMoro2010.mn_eq20` | general | |
| Moro and Norman, `C/S` rises with investment; cross-group effects; group size | none | not formalised | Argued in words; needs the production function's properties |
| Eqs. (21) to (25) | `FangMoro2010.mss_eq24_25` | special case | Needs the vacant firm's value equation, which the survey does not print (FM-D12) |
| Eqs. (26) to (33), `V_B < V_W`, `H_B < H_W` | `FangMoro2010.mss_VB_lt_VW`, `FangMoro2010.mss_HB_lt_HW` | special case | From the printed (32) and (33); their derivation (31) to (33) is not reproduced |
| Eqs. (34) to (36) and the sufficient condition on `γ_F` | none | not formalised | Values stated without derivation |
| Eq. (37) and the choice rules, Fang (2001) | `FangMoro2010.fang_low_cost`, `FangMoro2010.fang_high_cost` | general | |
| Eq. (38) and single crossing | `FangMoro2010.fang_eq38` | general | |
| `Δ ≥ 0` necessary and sufficient | `FangMoro2010.fang_fixed_point_necessary` | special case | Necessity only; sufficiency needs continuity of `Ψ`, not stated |
| Fang (2001), unique equilibrium `π = 0` without the activity | none | not formalised | Stated with sets `Π_l`, `Π_h` without proof |
| Fang (2001), Pareto dominance | `FangMoro2010.fang_pareto` | general | |
| Chaudhuri and Sethi, `s_j` | `FangMoro2010.cs_peer_gap` | special case | The definition and the gap; the claim that strong integration removes multiplicity is not formalised |
| Section 5, dynamic models | none | not formalised | Described in words |
| Eqs. (39) to (41) and the sufficient condition | `FangMoro2010.cl_aa_sufficient` | general | |
| Eqs. (44) to (47) | `FangMoro2010.patron_eq47` | general | |
| Eqs. (48), (49) | `FangMoro2010.patron_eq48_49` | general | |
| Eq. (50), footnote 17 | `FangMoro2010.patron_eq50` | general | |
| Eq. (51) | `FangMoro2010.patron_eq51` | general | |
| Dynamics, convergence to `1 − π_l` | `FangMoro2010.patron_dynamics_slope` | special case | Local stability by the slope; global convergence from `π_c` is shown by a phase diagram, not formalised |
| Section 6.3, threshold order | `FangMoro2010.mnaa_threshold_order` | general | Proves the reverse of the printed inequality (FM-D13) |
| Eqs. (52), (53) | none | not formalised | Wage schedule stated with a proof sketch |
| Fang and Norman, `π̂_h`, `π̂_l` | `FangMoro2010.fn_pihat` | general | |
| Eq. (54) | `FangMoro2010.fn_eq54` | general | |
| Eq. (55) | `FangMoro2010.fn_eq55` | general | |
| Precision below `3/4` | `FangMoro2010.fn_precision` | general | |
| Group `A` better off than group `B` | none | not formalised | Stated without derivation |
| Chung, `q_n < q*` and footnote 20 | `FangMoro2010.chung_role_model`, `FangMoro2010.chung_fn20` | general | |
| Eqs. (56) to (59) | `FangMoro2010.ce_eq58`, `FangMoro2010.ce_share_decreasing`, `FangMoro2010.ce_foc` | special case | Pointwise in the score; the integral form and the cut-off result `0 ≤ t_W − t_B ≤ α/C` are not formalised |
| Colour-blind randomisation, `Γ` | none | not formalised | Characterisation cited from Chan and Eyster |
| Eqs. (60) to (68), laissez-faire efficiency | none | not formalised | Quantile calculus; eq. (61) has a sign problem (FM-D16) |
| Eqs. (69) to (72) and the colour-sighted policy | `FangMoro2010.fl_colorsighted` | special case | The algebra of `τ_B`; the efficiency of `σ = 0` is not formalised |
| Eqs. (73) to (81) | `FangMoro2010.fl_eq79` | special case | Eq. (79) from (75), (78); the optimisation (80), (81) is not formalised |
| Section 7.1, total product | `FangMoro2010.eff_total_product` | general | |
| Section 7.1, pooled variance | `FangMoro2010.pool_variance` | general | The survey's expression fails (FM-D3) |
| Section 7.1, two-job mismatch | none | not formalised | Stated in words |
| Net social product, Lundberg and Startz | `FangMoro2010.ls_net_product` | general | Printed form fails (FM-D14) |
| Schwab (1986) | none | not formalised | Described in words |
| Norman's example and eq. (82) | `FangMoro2010.norman_example`, `FangMoro2010.norman_eq82` | general | |
| Pareto ranking of equilibria, Section 7.2.2 | `FangMoro2010.mn_pareto_chain` | special case | Weak inequality; the strict one needs a strict step (FM-D15) |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| FM-D1 | The version read is NBER working paper 15860 of April 2010, cited as such, not the Handbook chapter of 2011 | PDF page 1 says the survey "was prepared for the Handbook of Social Economics" and is forthcoming. The slug and key name the 2010 working paper. The published chapter is not read |
| FM-D2 | Aigner and Cain's utility is read as `a − b exp(−cq)` | The survey prints `U(q) = a + b exp(−cq)` (PDF p. 9, image) but the next line has `a − b exp[...]`. Aigner and Cain's eq. (7) has the minus sign (AC-D4) |
| FM-D3 | The pooled variance of Section 7.1 is `λσ_B² + (1 − λ)σ_W² + λ(1 − λ)(μ_B − μ_W)²` | The survey writes `σ² = λ²σ_B² + (1 − λ)²σ_W²` (PDF p. 66, image), the variance of a weighted sum of independent draws, not of the pooled population (`pool_variance`, `control_survey_pooled_variance`). The pooled conditional mean is also not linear in `θ` when the group means differ |
| FM-D4 | "Phelps (1973)" on PDF p. 8 is read as Phelps (1972) | The reference list has only Phelps (1972) |
| FM-D5 | "Lang (1996)" in footnote 2 is read as Lang (1986) | The reference list has Lang (1986), "A Language Theory of Discrimination". Lang's model itself is not read |
| FM-D6 | "Altonji and Pierret (1997)" in footnote 12 is read as Altonji and Pierret (2001) | The reference list has only the 2001 article |
| FM-D7 | Quotations keep the ligature characters of the text layer and avoid passages with mathematical symbols | The text layer sets mathematics in Unicode italic and keeps `ﬁ`, `ﬀ`, `ﬂ`, which the normaliser does not expand |
| FM-D8 | PDF p. 57 is read from the image | The page reproduces Figure 1 of Chan and Eyster (2003) over the text layer, so the extracted text interleaves the two. No quotation is taken from it |
| FM-D9 | The fourth value relation on PDF p. 27 is read with `V_W` on the left | It prints `V_B` twice; the coefficient `ρ_F(γ_I + 2γ_F)` and `Z_W` on the right belong to white workers |
| FM-D10 | The survey's accounts of the models it cites are its own readings | Only Aigner and Cain (1977) and Eeckhout (2006) among them are read in full in this bundle. A claim about any other cited model rests on this survey and carries the tag [C] |
| FM-D11 | The exponent in Section 2.2.1 is read as `−cE(q\|θ) + (c²/2)Var(q\|θ)` | The survey prints `(c/2)Var(q\|θ)` (PDF p. 9, image). Aigner and Cain's eq. (8) has `c²/2`, and the survey's own risk term `cVar(q\|θ)/2` is the certainty-equivalent form of `c²/2` (`control_survey_exponent`) |
| FM-D12 | Eqs. (24), (25) are derived with the vacant firm's value equation `[ρ_I H_I(γ_F + γ_I) + r + δ]V_F = ρ_I H_I(γ_F + γ_I)Z_F` | The survey prints only the worker's value equations, the surplus equation and the bargaining rule, which leave the system underdetermined. With the firm's equation, (24) and (25) follow (`mss_eq24_25`) |
| FM-D13 | Under the quota of Section 6.3, `π_B < π_W` gives `θ̂_B < θ̂_W` | The survey prints `θ̂_B(π) > θ̂_W(π)` (PDF p. 45, image) and in the next sentence says the policy lowers the threshold for the discriminated group. The constraint with `F_q < F_u` gives the lower threshold for `B` (`mnaa_threshold_order`) |
| FM-D14 | The net social product is `(b²/c) w (1 − w/2)` | The survey prints `(b²/c)(1 − w/2)` (PDF p. 67, image), dropping a factor `w = σ²/(σ² + σ_εj²)` (`ls_net_product`, `control_ls_printed_net_product`) |
| FM-D15 | The Pareto ranking step on PDF p. 67 is read with a weak inequality | The chain `∫w(θ,π₁)f_q − c ≥ ∫w(θ,π₁)f_u ≥ ∫w(θ,π₂)f_u` gives `≥`; the survey concludes `>` without a strict step (`mn_pareto_chain`) |
| FM-D16 | Eq. (61)'s right side is read as `∫ (H₀ − H₁)` | With `H_e` distribution functions and `ΔH = H₁ − H₀`, integration by parts gives `∫_{p}^{∞}(v − p) dΔH = −∫_{p}^{∞} ΔH dv`. If investment raises productivity, `H₁ ≤ H₀`, so the survey's `ΔH ≥ 0` and its sign cannot both hold. Not formalised |
