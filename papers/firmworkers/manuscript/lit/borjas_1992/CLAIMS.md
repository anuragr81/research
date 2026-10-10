# Claims about Borjas (1992)

**Source.** *QJE*, February 1992, pp.123–150, read in full [F]; see
`RECONSTRUCTION.md` for the copy, its sha256 and the cache path. Cited in
`claims.yaml` as row L4 and in `refs.bib` as `Borjas1992`. Pages are the
journal's printed pages. Quotations are checked verbatim, page by page,
against the cached text by `verify_borjas_1992.py`. The results we use are
proved in `lean/Lit/Borjas1992.lean` (see `LEAN`), namespace `Borjas1992`.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| BJ-1 | 123 | "I assume that ethnicity acts as an externality in the human capital accumulation process." | The group average enters the production of skills, not only their price |
| BJ-2 | 123 | "The skills of the next generation depend on parental inputs and on the quality of the ethnic environment in which parents make their investments, or "ethnic capital."" | L4 as stated in the ledger |
| BJ-3 | 124 | "if the external effect of ethnicity is sufficiently strong, ethnic differences in skills observed in this generation are likely to persist for many generations (and may never disappear)" | Persistence is the paper's headline implication |
| BJ-4 | 125 | "I assume that workers do not invest in their own human capital, so that the human capital stock of workers in generation t + 1 is completely determined by the actions of generation t." | Accumulation is intergenerational only; no own investment |
| BJ-5 | 126 | "determines whether skill differentials across ethnic groups converge over time" | The sum of the two exponents decides convergence |
| BJ-6 | 127 | "equation (3) implies that parental time and ethnic capital are complements in the production of child quality" | Complementarity of parental time and the group average |
| BJ-7 | 128 | "There is a positive relationship between child quality and both parental human capital and ethnic capital regardless of the value of the elasticity of substitution between own-consumption and child quality." | (7a), (7b) positive for every ρ < 1 |
| BJ-8 | 129 | "If the externality introduced by ethnic capital leads to constant returns in the production function, the relative dispersion that exists in human capital among ethnic groups in the parent's generation will persist indefinitely." | Gap constant when the exponents sum to one |
| BJ-9 | 129 | "a key insight of the model is that the external effects of ethnic capital may greatly retard the process of convergence" | A group-average term slows convergence even below constant returns |
| BJ-10 | 129 | "slow down the regression toward the mean in skills across generations" | Higher β₂ keeps the gap larger at every horizon |
| BJ-11 | 131 | "may be substantially underestimated by the coefficient estimated from regressions that ignore the importance of ethnic capital" | Equation (14) |
| BJ-12 | 131 | "fraction of the variance of earnings that is explained by variation within ethnic groups" | Definition of π in (14) |
| BJ-13 | 142 | "estimated coefficients (asymptotically) sum up to the true intergenerational parameter" | (18a) + (18b) = δ |
| BJ-14 | 142 | "should be 0.29, and that the coefficient of ethnic capital should be 0.11" | The worked example of (18) |
| BJ-15 | 139 | "The respective statistics in the GSS are 0.48 and 0.63." | Text sums of Table III (see BJ-D1) |
| BJ-16 | 140 | "In the NLSY it is 0.50 for education and 0.53 for wages." | Text sums of Table IV (see BJ-D2) |
| BJ-17 | 145 | "this interpretation is not the only one consistent with the data" | Discrimination or access can produce the same correlation |
| BJ-19 | 123 | "The empirical evidence reveals that the skills of today's generation depend not only on the skills of their parents, but also on the average skills of the ethnic group in the parent's generation." | L4 as an empirical finding of the paper |
| BJ-18 | 148 | "differences in skills and labor market outcomes among ethnic groups may persist across generations, and need never converge" | The paper's summary of the mechanism |

## Results in Lean (`lean/Lit/Borjas1992.lean`)

| Result | Lean | What is proved |
|---|---|---|
| (3), externality and complementarity, p.126–127 | `Borjas1992.childCapital_strictMono_ethnic`, `Borjas1992.childCapital_complementarity` | child capital rises in ethnic capital; increasing differences in parental time and ethnic capital for positive exponents |
| (5a), (5b) from the first-order condition | `Borjas1992.foc_time_parent`, `Borjas1992.foc_time_ethnic` | the differentiated log condition has the unique solutions (5a), (5b) |
| Signs of (5a), (5b), p.127–128 | `Borjas1992.denom_pos`, `Borjas1992.rho_beta_lt_one`, `Borjas1992.one_sub_rho_s_pos`, `Borjas1992.elasTimeParent_neg_of_rho_pos`, `Borjas1992.elasTimeParent_pos_of_rho_neg`, `Borjas1992.elasTimeParent_zero_of_cobbDouglas`, `Borjas1992.elasTimeEthnic_pos_of_rho_pos` | parental time falls in own capital iff ρ > 0, is constant at ρ = 0, rises in ethnic capital when ρ > 0 |
| (7a), (7b) from (5) and (6) | `Borjas1992.eq7a_from_eq5a`, `Borjas1992.eq7b_from_eq5b`, `Borjas1992.elasChild_pos` | the printed (7a), (7b) follow from (5) and the log of (6), and are positive for every ρ < 1 |
| (8) and (9) | `Borjas1992.eq8_eq_sum`, `Borjas1992.eta_sub_one`, `Borjas1992.eta_lt_one_iff`, `Borjas1992.eta_eq_one_iff`, `Borjas1992.one_lt_eta_iff` | η = (7a) + (7b); η − 1 = (β₁ + β₂ − 1)(1 − ρs)/D; η is below, at or above one exactly as β₁ + β₂ is |
| Persistence, p.129 | `Borjas1992.log_childCapital_common`, `Borjas1992.group_gap_step`, `Borjas1992.gap_closed_form`, `Borjas1992.gap_persists`, `Borjas1992.gap_tendsto_zero`, `Borjas1992.gap_larger_with_externality` | with common s the log gap between groups is multiplied by β₁ + β₂ each generation; constant at one, vanishing below one, larger at every horizon for larger β₂ |
| (14) | `Borjas1992.sum_within_deviation_zero`, `Borjas1992.cov_groupMean_eq_between`, `Borjas1992.total_eq_within_add_between`, `Borjas1992.ols_slope_omitting_ethnic_capital`, `Borjas1992.sum_dev_mean_zero`, `Borjas1992.ols_understates_transmission` | in a finite population, with the disturbance orthogonal to x, the slope omitting ethnic capital is γ₁ + (1 − π)γ₂, below γ₁ + γ₂ when π > 0 and γ₂ > 0 |
| (18a), (18b) | `Borjas1992.normal_equations_solution`, `Borjas1992.plim_sum_eq_delta`, `Borjas1992.plimEthnic_zero_of_exact`, `Borjas1992.plimEthnic_strictAnti_reliability` | the normal equations under the stated covariances give (18a), (18b); they sum to δ; the ethnic limit is zero at h = 1 and falls in h |
| Worked example, p.142 | `Borjas1992.reliability_of_noise_ratio`, `Borjas1992.example_p142`, `Borjas1992.example_p142_rounds` | noise ratio 0.33 gives h = 100/133; the limits are 12/41 and 22/205, which round to 0.29 and 0.11 |
| Text sums of the tables | `Borjas1992.table3_sums_match_text`, `Borjas1992.table3_gss_occupation_reading`, `Borjas1992.table4_sums_against_text` | which printed sums match their table entries and which do not |
| Controls | `Borjas1992.control_complementarity_needs_beta2_pos`, `Borjas1992.control_eta_needs_rho_lt_one`, `Borjas1992.control_eta_needs_beta1_lt_one`, `Borjas1992.control_gap_needs_lt_one`, `Borjas1992.control_understates_needs_gamma2_pos`, `Borjas1992.control_sum_needs_pi_lt_one` | each conclusion fails when its hypothesis is dropped |

Not formalised: the first-order condition and its differentiation (entered as
hypotheses), existence of an interior optimum, and every estimate in
Tables I–VII.

## Readings recorded

| ID | Page | Text | Our reading |
|---|---|---|---|
| BJ-D1 | 139 | "The respective statistics in the GSS are 0.48 and 0.63." | Table III, column (3), gives 0.2501 + 0.2265 = 0.4766 for education, which matches 0.48, and 0.1829 + 0.4589 = 0.6418 for occupation, which rounds to 0.64. Column (4) gives 0.5699. We use 0.64 from the table |
| BJ-D2 | 140 | "In the NLSY it is 0.50 for education and 0.53 for wages." | Table IV, column (1), gives 0.3015 + 0.1896 = 0.4911 and 0.3372 + 0.1980 = 0.5352, which round to 0.49 and 0.54. Column (2) gives 0.5251 and 0.3477. We use the table entries |
| BJ-D3 | 131 | π is a double sum of squared within-group deviations divided by "var (y_ij(t − 1))" | A ratio of a total to a per-observation variance is not a share. We read numerator and denominator on the same scale, both sums of squares, which makes π the within share in (14) |
| BJ-D4 | 131, 142 | π in (14) and π in (18) | Both are within-group shares, of observed earnings in (14) and of true parental skill in (18). The record keeps them apart |
