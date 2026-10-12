# Claims about Jacobson, LaLonde and Sullivan (1993)

**Source.** JLS, Federal Reserve Bank of Chicago WP-92-28 (revised November
1992), read in full [F]; the AER 83(4) pages are not read. See
`RECONSTRUCTION.md`. Cited in `claims.yaml` as row L26 and in `refs.bib` as
`JacobsonLaLondeSullivan1993`. Pages are the working paper's printed pages; the
unnumbered table pages after p.36 are numbered 37–45 in order (37 Table 1, 38–39
Table 2, 40 Table 3). Quotations are checked verbatim on their page by
`verify_jls.py`; table rows are quoted as printed, and normalisation drops their
minus signs. Results are proved in `lean/Lit/JacobsonLaLondeSullivan1993.lean`
(see `LEAN`), namespace `JacobsonLaLondeSullivan1993`.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| JLS-1 | 1 | "These displaced workers are widely recognized to experience costly spells of unemployment and short-term earnings declines." | The prior view |
| JLS-2 | 1 | "workers losing a job that paid a wage premium are likely to earn less if their subsequent jobs pay standard wages" | The wage-premium channel |
| JLS-3 | 2 | "Our study focuses on high-tenure workers because they are the ones that are most likely to have accumulated a substantial amount of firm-specific human or “match” capital prior to their job losses." | The population: high-tenure workers |
| JLS-4 | 3 | "five years after separating from their former firms, their losses are still equal to 25 percent of their pre-displacement earnings" | Size and persistence |
| JLS-5 | 3 | "Male and female workers, as well as younger and older workers suffer similarly large losses." | Little variation by sex and age |
| JLS-6 | 3 | "Further, even those workers finding work in the same industries as their old jobs experience large earnings losses." | Loss within the industry |
| JLS-7 | 6 | "lack of data does limit the extent to which we can learn how earnings losses vary among different demographic groups" | No breakdown by education or income level |
| JLS-8 | 6 | "we constructed a “mass-layoff” sample that includes separators whose firms’ employment in the year following their departure was 30 percent or more below their maximum levels during the late 1970s" | Displacement identified by the firm's decline |
| JLS-9 | 7 | "we have chosen to eliminate from our sample the approximately 25 percent of high-tenure separators who subsequently appear to never have positive earnings in Pennsylvania" | Sample restriction |
| JLS-10 | 7 | "Thus it is worth noting that without this sample restriction our estimates of the losses would be approximately 15 percentage points larger." | Its effect on the loss |
| JLS-11 | 8 | "Despite being approximately the same age, the separators earned about $2,000 or 9 percent less than the median worker in our sample of nondisplaced workers." | Separators come from lower in the distribution |
| JLS-12 | 10 | "In this study, we define displaced workers’ earnings losses to be the difference between their actual earnings and their expected earnings had the events that lead to their job losses not occurred." | Definition (1) |
| JLS-13 | 10 | "This definition of workers’ earnings losses allows the events that lead to workers’ displacements to affect earnings prior to separation." | Pre-separation losses count |
| JLS-14 | 13 | "Thus our estimates of the displacement effects are unbiased no matter how workers’ permanent characteristics are related to their displacement status." | Fixed effects absorb selection on levels |
| JLS-15 | 14 | "one potential problem with specification (2) is that it does not allow for the possibility that workers might have different trend rates of earnings growth and that firms might be more likely to layoff workers with more slowly growing earnings" | The trend problem, model (3) |
| JLS-16 | 15 | "differences among groups in the time pattern of earnings losses occurred mainly along just three dimensions" | Dip, drop, recovery |
| JLS-17 | 16 | "Specification (5) forces the gap between the estimated losses of two workers to (i) be zero in the period more than three years prior to separation" | The shape of model (5) |
| JLS-18 | 17 | "the average loss for all displaced workers for the k'th quarter after separation simply equals" | Average loss is `δ_k` |
| JLS-19 | 18 | "First, if we assume that the error process for each individual is stationary, the spurious effects of displacement are symmetric about the date of separation." | The p.18 argument |
| JLS-20 | 19 | "Even six years after their separations, their quarterly earnings remain $1,600 below their expected levels" | Main estimate |
| JLS-21 | 19 | "This loss represents 25 percent of workers’ pre-displacement earnings." | As a share |
| JLS-22 | 19 | "there is little evidence that displaced workers’ earnings will ever return to their expected levels" | No recovery |
| JLS-23 | 20 | "these workers’ quarterly earnings begin to diverge meaningfully from their expected levels approximately three years prior to separation" | The dip |
| JLS-24 | 20 | "following separation they drop by only one-half as much as workers in the mass-layoff sample" | Non-mass layoffs |
| JLS-25 | 21 | "if we had only one year of pre-separation earnings data, our earnings loss estimates would have been nearly 50 percent smaller than the estimate based on workers’ long-term earnings histories" | A short base understates |
| JLS-26 | 22 | "the estimated earnings losses are smaller by about 20 percent" | Conditioning on the firm |
| JLS-27 | 22 | "employees who remain employed during mass layoffs experience only modest declines in earnings relative to other nondisplaced workers" | Stayers lose little |
| JLS-28 | 22 | "As shown in Figure 5, the estimated earnings losses are the same whether or not we condition on a displaced worker’s firm." | Figure 5 is the non-mass-layoff sample (JLS-D4) |
| JLS-29 | 23 | "Thus, by the quarter of separation, the gap between men’s and women’s earnings losses is estimated to be $618 (47.5 multiplied by 13, the value of the dip time trend on the date of separation)." | Reading of the dip coefficient |
| JLS-30 | 23 | "less than the average loss depicted in Figure 2 (approximately $2,219)" | Average drop in the text (JLS-D1) |
| JLS-31 | 24 | "During the fifth year after displacement, their earnings losses exceed women’s by $2398" | Fifth-year gap |
| JLS-32 | 25 | "workers’ earnings losses are substantial across a broad range of industries and firm sizes" | Generality |
| JLS-33 | 25 | "The differences in the magnitude of losses across both industries and firm sizes suggest that the loss of rents, including union premiums, may contribute to workers’ earnings losses." | Rents |
| JLS-34 | 25 | "we also find much larger losses among workers displaced from very large firms" | Firm size |
| JLS-35 | 27 | "even workers displaced in the best of circumstances have losses that are at most only one-third less severe than the average losses depicted in Figure 2" | Local conditions matter modestly |
| JLS-36 | 27 | "We view this finding as evidence that some valuable attribute of the employment relationship is generally lost when high-tenure workers are displaced." | What is lost |
| JLS-37 | 28 | "the losses of those displaced workers who leave the manufacturing sector are equal to 38 percent of their pre-displacement earnings" | Sector switch |
| JLS-38 | 28 | "Even those who found re-employment in the same industry, and thus presumably in jobs requiring similar skills, experienced large and persistent losses." | Not industry skills |
| JLS-39 | 29 | "(iv) do not depend very much on workers’ gender and age; and (v) are substantial even for those who find new jobs in similar firms" | Conclusion |
| JLS-40 | 29 | "approximately 5 percent of U.S. workers are employed in Pennsylvania" | National scaling (JLS-D2) |
| JLS-41 | 29 | "our results represent the experiences of 2.6 million workers nationwide" | National scaling (JLS-D2) |
| JLS-42 | 30 | "our results indicate that there is something intrinsic to the employment relationship itself that is lost when workers are displaced" | Conclusion |
| JLS-43 | 30 | "If it is workers’ skills that are lost, these skills must be firm- as opposed to industry-specific." | Firm-specific |
| JLS-44 | 30 | "These conclusions are, of course, only tentative, and reflect considerations beyond the scope of this paper." | The authors' caveat |
| JLS-45 | 33 | "This restriction eliminated approximately 38 percent of our sample of high-tenure prime age separators." | Earnings in every year (JLS-D6) |
| JLS-46 | 33 | "A majority of the eliminated workers (70 percent) never had any positive reported earnings following their job losses." | JLS-D6 |
| JLS-47 | 37 | "Males 7,092 27,363" | Table 1, men's 1979 mean earnings |
| JLS-48 | 37 | "Females 2,415 14,897" | Table 1, women's 1979 mean earnings |
| JLS-49 | 37 | "Mass Layoffs 6,435 24,461" | Table 1, mass-layoff mean 1979 earnings |
| JLS-50 | 38 | "Overall 6,435 -83.3 -2179 15.4" | Table 2, average dip, drop, recovery |
| JLS-51 | 38 | "Male 4972 -10.8 -217 6.5 -545 -7,143" | Table 2, men, without other controls |
| JLS-52 | 38 | "Female 1463 36.7 738 -22.0 1,853 -4,744" | Table 2, women, without other controls |

### Footnotes whose text layer has no spaces (checked with all spaces removed)

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| JLS-F-1 | 19 | "Thus, the substantial earnings losses observed in Figure 2 are largely due to lower earnings for those who work, rather than an increase in the number of workers without quarterly earnings." | Footnote 30: little nonemployment in the sample |
| JLS-F-2 | 24 | "The estimates in the columns labeled “fifth year loss dif’ are equal to four times the drop estimate plus 50 times the recovery estimate." | Footnote 36 |

## Results in Lean (`lean/Lit/JacobsonLaLondeSullivan1993.lean`)

| Result | Lean | What is proved |
|---|---|---|
| Model (2) identifies `δ_k` (pp.12–13) | `JacobsonLaLondeSullivan1993.did_identifies_effect`, `JacobsonLaLondeSullivan1993.fixed_effects_cancel` | without error, the contrast of a displaced worker and a stayer between quarter `t` and a base before the window equals `δ_{t-s}` when the controls move alike; it does not depend on either fixed effect |
| Short base (p.13 fn 24, p.21) | `JacobsonLaLondeSullivan1993.short_base_bias`, `JacobsonLaLondeSullivan1993.short_base_understates` | a base inside the window gives `δ_{t-s} - δ_{b-s}`; with a pre-separation dip the estimated loss is smaller than the true one |
| Simple change (p.13) | `JacobsonLaLondeSullivan1993.simple_change_adequate` | the own before–after change equals `δ_{t-s}` with equal time effects and controls at `t` and `b` and an early base |
| Model (3) (p.14) | `JacobsonLaLondeSullivan1993.trend_bias`, `JacobsonLaLondeSullivan1993.slower_trend_overstates`, `JacobsonLaLondeSullivan1993.two_base_periods_remove_trend` | with trends the model-(2) contrast is `δ + (ω_i - ω_j)(t - b)`, below `δ` when the displaced worker's trend is slower; a contrast detrended over two base quarters before the window recovers `δ` |
| Model (5') (p.16) | `JacobsonLaLondeSullivan1993.loss_before_dip`, `JacobsonLaLondeSullivan1993.loss_dip`, `JacobsonLaLondeSullivan1993.loss_drop`, `JacobsonLaLondeSullivan1993.loss_recovery`, `JacobsonLaLondeSullivan1993.gap_zero_before_dip`, `JacobsonLaLondeSullivan1993.dip_trend_at_separation` | the four-piece loss `δ_k`, `δ_k + φ_1(k+13)`, `δ_k + φ_2`, `δ_k + φ_2 + φ_3(k-6)`; the gap before the dip window is zero; the dip trend is 13 at separation |
| Footnote 36 (p.24) | `JacobsonLaLondeSullivan1993.fifth_year_weights`, `JacobsonLaLondeSullivan1993.fifth_year_loss_dif` | over quarters 17–20 the trends sum to 0, 4 and 50, so the fifth-year difference is `4 φ_2 + 50 φ_3` |
| Average loss (p.17) | `JacobsonLaLondeSullivan1993.average_loss_is_delta` | with weights summing to one and `Σ_j f_j φ_lj = 0` for each `l`, the weighted average loss in quarter `k` is `δ_k` |
| Long-run bias (p.18) | `JacobsonLaLondeSullivan1993.spurious_effects_symmetric`, `JacobsonLaLondeSullivan1993.long_run_bias_bounded` | under a linear projection on a stationary autocovariance the spurious effects at `s ± j` agree; if true and spurious pre-effects share a sign and their sum is at least `-ε`, the spurious post-effect lies in `[-ε, 0]` |
| Text against tables | `JacobsonLaLondeSullivan1993.periods_and_parameters`, `JacobsonLaLondeSullivan1993.group_counts_sum`, `JacobsonLaLondeSullivan1993.sex_rows_balance`, `JacobsonLaLondeSullivan1993.separation_gap_men_women`, `JacobsonLaLondeSullivan1993.fifth_year_dif_from_coefficients`, `JacobsonLaLondeSullivan1993.fifth_year_gap_men_women`, `JacobsonLaLondeSullivan1993.quarterly_loss_share`, `JacobsonLaLondeSullivan1993.comparison_group_gap`, `JacobsonLaLondeSullivan1993.excluded_never_positive`, `JacobsonLaLondeSullivan1993.national_scaling_reading`, `JacobsonLaLondeSullivan1993.level_and_share_by_sex` | 48 quarters `k = -20..27` and 576 industry parameters (p.15, "nearly 600"); each Table 2 grouping sums to 6,435; the sex rows satisfy the restriction within half the printed rounding unit; 47.5 × 13 = 617.5 (p.23); `4 φ_2 + 50 φ_3` from the printed coefficients is within 2 of −545 and 1,853; −545 − 1,853 = −2,398, and −7,143 − (−4,744) within 1 of it; $1,600 against a quarter of $24,461 is within 0.015 of 25 per cent; 1 − 1,200/1,500 = 0.2 (p.22); 0.38 × 0.70 within 0.02 of 0.25 (pp.7, 33); 135,000/0.05 = 2.7 million; men lose more than women in dollars but less as a share of Table 1 earnings |

Controls: `JacobsonLaLondeSullivan1993.control_simple_change_needs_constant_time_effects`,
`JacobsonLaLondeSullivan1993.control_average_needs_balance`,
`JacobsonLaLondeSullivan1993.control_bias_bound_needs_same_sign`.

## Readings recorded

- JLS-D1 (p.23 against Table 2): the text gives the average drop as
  "approximately $2,219" and the average dip as $83.3; Table 2's overall row,
  which belongs to the model with all interactions, prints −83.3 and −2179. The
  text does not say which model its $2,219 comes from; not used.
- JLS-D2 (p.29): 135,000 divided by 5 per cent is 2.7 million
  (`national_scaling_reading`), not the stated 2.6 million; the figure fits a
  Pennsylvania share near 5.2 per cent. Both are stated as approximate; not used.
- JLS-D3 (p.3 against p.19): p.3 puts the 25 per cent loss "five years after
  separating", p.19 puts the $1,600, 25 per cent, "six years after". Since the
  estimated losses are flat after year three the two agree; we cite p.19.
- JLS-D4 (Figure 5): its title names the mass-layoff sample, repeating Figure 4's,
  while p.22 (JLS-28) uses Figure 5 for the non-mass-layoff sample. We follow the
  text.
- JLS-D5 (p.16): the inequality signs of the four-piece display come through the
  text layer as "<" and "^". We read the ranges from the definitions of
  `F^1, F^2, F^3` on the same page: `k ≤ -13`, `-12 ≤ k ≤ 0`, `1 ≤ k ≤ 6`,
  `k ≥ 7`. Footnote 36 (recovery trend 11–14 in the fifth year) confirms them.
- JLS-D6 (p.7 against p.33): "approximately 25 percent" of separators never have
  positive earnings again (p.7); the every-year restriction removed 38 per cent,
  70 per cent of whom never had positive earnings (p.33), which is 26.6 per cent
  (`excluded_never_positive`). Consistent.
- JLS-D7 (footnote 36 against Table 2): `4 φ_2 + 50 φ_3` from the printed men's
  coefficients is −543 against the printed −545, and 1,852 against 1,853 for
  women: rounding of the printed coefficients.
- JLS-D8 (Table 1): the earnings by sex are 1979 means over all separators, not
  the mass-layoff sample, so `level_and_share_by_sex` mixes the two samples. It
  is our arithmetic, not a JLS result.
