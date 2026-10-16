# Laouénan and Rathelot (2022), claims

Evidence tag [F]. The publisher's text layer of the AEJ: Applied article was
read in full, PDF pp. 1 to 26, on 16 October 2026, with the equation pages
also read from images (`RECONSTRUCTION.md`). Pages are PDF pages. Journal page
`n` is PDF page `n − 106`. The author's accepted manuscript in WRAP was
compared at the level of results (LR-D5).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| LR-Q1 | 1 | "hosts from minority groups charge 3.2 percent less for comparable listings." | The size of the residual ethnic price gap, Table 2 column 4. The marked side is the host, and the penalty is a price |
| LR-Q2 | 23 | "we show that the ethnic gap can be fully explained by statistical discrimination." | The headline result, from the conclusion. The group penalty vanishes once the unobserved type is revealed, the form of A1's contribution 4, here for listing quality rather than class |
| LR-Q3 | 2 | "In the presence of statistical discrimination, the price gap should decrease with the number of reviews and tend to zero, conditional on observables and the measure of quality provided by the reviews." | The test. The gap shrinks with information about the individual, conditional on a proxy for true quality. Formalised as `gap_tendsto_zero` |
| LR-Q4 | 2 | "If, instead, the price gap is due to taste-based discrimination or ethnic differentials in variables that are not observable to the econometrician but observable to potential guests, the price gap should remain stable with the number of reviews." | The rival. A taste penalty survives full information. Formalised as `control_taste_gap_survives` |
| LR-Q5 | 3 | "However, if we follow two properties of the same quality—one held by a minority host, the other one by a majority host—there will be an initial price gap of 10 percent that will converge to 0 as they accumulate reviews." | The vanishing holds for listings of the same quality, not for group averages, which keep the true difference (`group_average_gap_constant`) |
| LR-Q6 | 3 | "Of these 3.4 percentage points, 2.5 are due to inaccurate beliefs—that is, the fact that potential guests underestimate the average unobservable quality of minority properties compared to majority ones." | Guests' imputed group mean differs from the actual mean of the group's listings. This bears directly on A1's equilibrium condition |
| LR-Q7 | 11 | "Taste-based discrimination is embedded in this framework: demand is assumed to be divided by Γ > 1 when m = 1, relatively to m = 0." | The taste term `γ = log Γ` in eqs. (1) and (2) |
| LR-Q8 | 12 | "the expected ν for a listing with average r, K reviews, and host ethnicity m is the weighted average between the prior" | The posterior mean `(K r + ρ ν̄_m)/(K + ρ)`. With no reviews it is the group prior (`postMean_no_reviews`), the analogue of A1's group sufficiency |
| LR-Q9 | 13 | "Note that within this framework, we cannot disentangle the other possible channels causing ethnic price gaps. [...] and taste-based discrimination (γ) are pooled together and absorbed by the listing fixed effects." | Only the part of the gap that moves with reviews is identified. A level penalty of any other origin is not |
| LR-Q10 | 14 | "We interpret this result as evidence that roughly three-quarters (i.e., 2.5/3.4) of the gap due to statistical discrimination is driven by inaccurate beliefs and one-quarter (0.9/3.4) is driven by differences in average unobservable quality." | The decomposition `βm = β̃m + (βm − β̃m)`, formalised as `decomposition` |
| LR-Q11 | 15 | "ρ can be interpreted as the number of reviews necessary to reveal half of the relevant information about the unobservables of a listing." | `ρ = 13.7`, Table 4. Formalised as `half_at_rho` |
| LR-Q12 | 18 | "Our analysis shows that shared flats tend to have higher statistical discrimination than entire flats." | Table 6. The penalty is larger where host and guest share the flat, 0.082 against 0.013, and information is slower, ρ of 22 against 12 |
| LR-Q13 | 19 | "While minority hosts seem to receive more minority guests, the magnitude of the difference shows that markets are far from being segregated." | Table 7. Sorting of guests on hosts' group is 0.81 percentage points. No venue-level segregation in this market |
| LR-Q14 | 24 | "we cannot rule out the existence of taste-based discrimination on Airbnb." | The authors' own limit. A taste term offset by an unobserved differential is not excluded |
| LR-Q15 | 10 | "First, we don’t have the statistical power to reject the null hypothesis that all five coefficients are equal." | The cross-section by number of reviews (Table 3) is suggestive only. The result rests on the within-listing estimate |
| LR-Q16 | 24 | "policies consisting of concealing more information about actors’ identity may backfire if ethnic gaps are due to statistical discrimination." | The authors' view of the platform's information design. Hiding the marker is weighed against disclosing more about the individual |
| LR-Q17 | 24 | "We see our results as advocating another way to reduce ethnic gaps: disclosing more abundant and reliable information about candidates, sellers, or hosts." | Their remedy is information about the individual, the route by which the penalty vanishes in their model |
| LR-Q18 | 9 | "Including neighborhood and block fixed effects reduces the ethnic price gap from 17 percent to 7 percent" | Location, a group-neutral variable correlated with the group, carries most of the raw gap. The 3.2 percent is measured net of it |
| LR-Q19 | 24 | "Because ethnic minorities tend to live in neighborhoods that are less valued by potential guests, [...] minority hosts suffer in reality from larger price gaps than those computed conditional on location." | The part of the penalty that passes through location is outside the paper's estimate by construction |
| LR-Q20 | 14 | "Most likely, hosts self-select into Airbnb based on the quality of their homes, and it is possible that minority listings are even more selected" | The authors' account of inaccurate beliefs. Selection makes the visible members' mean differ from the group's |
| LR-Q21 | 14 | "Another way to explain why potential guests, who are primarily from the majority group, have unduly low beliefs" | The observers are mostly from the majority group. Guests' own group does not enter the model |

## Numbers A1 would cite

Table 2 (PDF p. 9), log daily rate on a minority dummy, 2,474,551
observations: −0.169 (0.008) with city-wave effects only; −0.032 (0.006) with
location and property characteristics. Table 3 (PDF p. 10), the same on
listings with 0 reviews, −0.034 (0.009); with 50 or more, −0.021 (0.015).
Table 4 (PDF p. 15), Minority × f(K), 0.034 (0.006) in eq. (3), 0.025 (0.006)
in eq. (4), ρ = 13.7 (0.30). Footnote 23 (PDF p. 15), 95 percent intervals
[0.014, 0.054] for βm, [0.005, 0.046] for β̃m, [0.006, 0.010] for βm − β̃m.
Table 6 (PDF p. 19), Minority × f(K) 0.019 for Arabic/Muslim and 0.049 for
Black hosts, 0.082 for shared and 0.013 for entire flats. Table 7 (PDF p. 20),
0.0081 (0.0014). The sample is 19 cities, 21 waves from June 2014 to November
2017, listings with at least one review (PDF pp. 3, 6).

## Lean results

`lean/mathlib/LaouenanRathelot2022.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `LaouenanRathelot2022.postMean_eq` | PDF p. 12. The posterior mean is the prior plus `K/(K + ρ)` times the surprise, the step from eq. (2) to eqs. (3) and (4) |
| `LaouenanRathelot2022.postMean_no_reviews` | With no reviews the guests' expectation is the group prior |
| `LaouenanRathelot2022.half_at_rho` | PDF p. 15. `f(ρ) = 1/2` |
| `LaouenanRathelot2022.gap_same_quality` | The gap between two listings of the same quality is `−λγ + λβ(ν̄₁ − ν̄₀) ρ/(K + ρ)` |
| `LaouenanRathelot2022.gap_tendsto_zero` | Without a taste term the gap tends to zero as `K` grows |
| `LaouenanRathelot2022.control_taste_gap_survives` | Control. With `λγ ≠ 0` the gap tends to `−λγ` and not to zero |
| `LaouenanRathelot2022.eq3_form` | Eq. (3). Conditioning on `r`, the coefficient on `m f(K)` is `−βm` with `βm = λβ(ν̃₁ − ν̄₀)` |
| `LaouenanRathelot2022.eq4_form` | Eq. (4). With `r` at the group's true mean, the coefficient on `m f(K)` is `−β̃m` with `β̃m = λβ(ν̃₁ − ν̄₁)` |
| `LaouenanRathelot2022.decomposition` | Section III.B. `βm = β̃m + λβ(ν̄₁ − ν̄₀)` |
| `LaouenanRathelot2022.accurate_beliefs_tilde_zero` | PDF p. 14. With accurate beliefs `β̃m = 0` |
| `LaouenanRathelot2022.equal_means_same` | PDF p. 14. With equal true means `βm = β̃m` |
| `LaouenanRathelot2022.control_equal_means_needed` | Control. With `ν̄₁ ≠ ν̄₀` and `λβ ≠ 0`, `βm ≠ β̃m` |
| `LaouenanRathelot2022.group_average_gap_constant` | PDF p. 3. The gap in group averages is `λβ(ν̄₁ − ν̄₀)` at every `K` |
| `LaouenanRathelot2022.objective_eq` | PDF p. 11. The host's program, with `D^{1/α̃} = A^{1/α̃} P^{−κ/α̃}` |
| `LaouenanRathelot2022.hasDerivAt_objective` | The derivative of the host's objective in `P` |
| `LaouenanRathelot2022.foc_log` | A zero derivative gives our log first-order condition |
| `LaouenanRathelot2022.eq1_from_foc` | Eq. (1) from the first-order condition, with constant `λα log(κ/(α̃(κ − 1)))` |
| `LaouenanRathelot2022.control_printed_p0` | Control. The printed constant `λα log(α̃(κ − 1)/κ)` is the negative of ours and agrees only when `κ = α̃(κ − 1)` (LR-D2) |
| `LaouenanRathelot2022.precision_weighted` | PDF pp. 11–12. The precision-weighted mean of prior and review average is `(K r + ρ ν̄_m)/(K + ρ)` with `ρ = σ²/σ_ν²` |
| `LaouenanRathelot2022.f_strictMonoOn` | `f(K) = K/(K + ρ)` rises strictly in `K ≥ 0` |
| `LaouenanRathelot2022.f_concaveOn` | PDF p. 122. `f` is concave on `K ≥ 0`, the reason given for `βm,2 < 0` in eq. (5) |
| `LaouenanRathelot2022.rho_identified` | `ρ` is pinned down by `f` at any `K > 0` |
| `LaouenanRathelot2022.control_rho_needs_reviews` | Control. At `K = 0` every `ρ` gives the same `f` |
| `LaouenanRathelot2022.model_is_eq3` | Predictions 0 and 1. Eq. (2) under the guests' prior is eq. (3) with `βm = λβ(ν̃₁ − ν̄₀)` |
| `LaouenanRathelot2022.model_is_eq4` | Prediction 2. With `r` at its group's true mean, eq. (2) is eq. (4) with `β̃m = λβ(ν̃₁ − ν̄₁)` |
| `LaouenanRathelot2022.within_listing_difference` | The identifying condition of PDF p. 12. A listing's price change between dates is `λβ` times the change in the guests' expected `ν` |
| `LaouenanRathelot2022.control_zeta_varies` | Control. If `ζ` moves within the listing, the change picks up `λβ(ζ₁ − ζ₂)` |
| `LaouenanRathelot2022.new_reviews_average` | PDF p. 126. `r̃_it` is the average rating of the new reviews |
| `LaouenanRathelot2022.table10_initial_difference` | Eq. (9). The minority flow of reviews at `K = 0` exceeds its limit by `bm`, the "0.02" of PDF p. 129 |
| `LaouenanRathelot2022.sample_sizes` | Tables 1, 2 and 3. Group counts and review subsamples each sum to 2,474,551; shares 6.2 and 93.8 percent |
| `LaouenanRathelot2022.raw_gap_percent` | PDF pp. 3 and 9. −0.169 log points is a gap between 15 and 16 percent (LR-D8) |
| `LaouenanRathelot2022.surplus_numbers` | PDF p. 10. $4.35 per night and 16.7 percent of the $26 surplus |
| `LaouenanRathelot2022.decomposition_numbers` | PDF p. 14 and footnote 23. `3.4 − 2.5 = 0.9`, the shares 0.735 and 0.265, and 0.009 in `[0.006, 0.010]` |
| `LaouenanRathelot2022.shared_flat_rho` | PDF p. 18. `22/12` lies between 1.8 and 1.9 |
| `LaouenanRathelot2022.proxy_gap` | Ours, not in the source. With `m` hidden, a proxy with minority shares `π₁, π₀` gives the gap `λβ(π₁ − π₀)(ν̄₁ − ν̄₀) ρ/(K + ρ)` |
| `LaouenanRathelot2022.perfect_proxy_restores_gap` | Ours. A proxy that separates the groups restores the whole statistical gap |
| `LaouenanRathelot2022.control_uninformative_proxy` | Ours. Control. An uninformative proxy leaves no gap |

## Coverage of the source's results

The source is empirical. Its numbered displays are eqs. (1) to (9), the
posterior of PDF p. 12, the formula for `r̃_it` and the Table 8 equation (PDF
p. 126), and Predictions 0, 1 and 2. 17 rows: 10 general, 4 special case, 3 not
formalised.

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Eq. (1), the host's price | `objective_eq`, `hasDerivAt_objective`, `foc_log`, `eq1_from_foc`, `control_printed_p0` | general | The interior first-order condition is solved in general. The second-order condition and existence of a maximum are not formalised; the printed `p₀` is corrected (LR-D2) |
| Posterior mean, PDF p. 12 | `postMean_eq`, `postMean_no_reviews`, `precision_weighted` | special case | The precision-weighted formula is checked. Normal–normal conjugacy is assumed, since Mathlib has no Gaussian conjugacy |
| Eq. (2) | `price`, `gap_same_quality`, `within_listing_difference`, `control_zeta_varies` | general | |
| `ρ` as half the information, PDF p. 121 | `half_at_rho`, `rho_identified`, `control_rho_needs_reviews` | general | |
| Prediction 0 | `model_is_eq3`, `eq3_form` | special case | Read as a statement about the conditional mean with `r` known. The paper proxies `r` by the last rating and estimates by nonlinear least squares, whose consistency is not formalised; the proofs are in the unread online appendix |
| Prediction 1 | `model_is_eq3`, `eq3_form`, `decomposition` | special case | As for Prediction 0 |
| Prediction 2 | `model_is_eq4`, `eq4_form` | special case | `βk` is set to zero, since `r` is replaced by its group mean deterministically |
| Section III.B decomposition | `decomposition`, `decomposition_numbers` | general | With the sign reading LR-D1 |
| PDF p. 14, accurate beliefs and equal means | `accurate_beliefs_tilde_zero`, `equal_means_same`, `control_equal_means_needed` | general | |
| PDF pp. 2–3, the illustration | `gap_tendsto_zero`, `control_taste_gap_survives`, `group_average_gap_constant` | general | |
| Eq. (5) and the concavity claim | `eq5`, `f_concaveOn`, `f_strictMonoOn` | general | That concavity of `f` makes `βm,2` negative in a quadratic fit is the paper's heuristic and is not a theorem about least squares |
| Eq. (6), splines (Figure 2) | none | not formalised | A nonparametric estimate with no derived claim |
| Eqs. (7), (8), (9) | `eq789`, `table10_initial_difference` | general | |
| `r̃_it`, PDF p. 126 | `new_reviews_average` | general | |
| Table 8 equation, PDF p. 126 | none | not formalised | A linear specification with no derived claim beyond the sign of `γ` |
| Online appendix, Section E (beta prior) | none | not formalised | Not in the cached file, not read |
| Numbers derived from printed numbers | `sample_sizes`, `raw_gap_percent`, `surplus_numbers`, `decomposition_numbers`, `shared_flat_rho` | general | |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| LR-D1 | The signs in Section III.B are read through eqs. (3) and (4). `βm = λβ(ν̃₁ − ν̄₀)` is negative when guests think minority listings worse, and the table entry Minority × f(K) estimates `−βm`, so the "3.4 percent" of the text is `−βm`. The sentence on PDF p. 14 that `βm` and `β̃m` are "strictly positive" when `ν̃₁ < ν̄₁ = ν̄₀` is read as a statement about `−βm` and `−β̃m` | Under the stated definitions both are negative in that case (`decomposition`, `eq3_form`, `eq4_form`). The empirical numbers are unaffected |
| LR-D2 | The constant in eq. (1) is read as `p₀ = λα log(κ/(α̃(κ − 1)))`, not the printed `λα log(α̃(κ − 1)/κ)` | Our first-order condition (`foc_log`, `eq1_from_foc`, `control_printed_p0`). `p₀` is absorbed by the listing fixed effects and no result uses it. Eq. (1) also needs `κ > 1` for the logarithm, where the text assumes only `κ > 0` |
| LR-D3 | On PDF p. 14, "the estimate of βm in equation (3) will be equal to ν̄₁ − ν̄₀" is read as `λβ(ν̄₁ − ν̄₀)` | The definition on PDF p. 12 carries the factor `λβ` |
| LR-D4 | Table 6, panel B, column 3 (PDF p. 19), printed "−0.25" for Black hosts, is read as −0.025 | Its standard error is 0.013, and the WRAP version marks it with a single star, which fits −0.025 and not −0.25. Not relied on |
| LR-D5 | The WRAP author's accepted manuscript (`laouenan_rathelot_wrap.pdf`, 32 PDF pages, WRAP item 146995) agrees with the published article in every table entry and every number quoted here, and has the same eqs. (1) to (9), including the constant of LR-D2 and the "−0.25" of LR-D4. It adds significance stars to the tables, gives the JEL codes as J15 and L85 where the article has D83, J15 and L84, refers to the online appendix as "Appendix", and differs only in copy-editing | Compared table by table and section by section on 16 October 2026. The published article is the version cited |
| LR-D6 | The text extraction drops the normal-distribution symbol `𝒩` on PDF p. 11 and garbles eq. (3) and a sentence on PDF p. 15 | Read from images. Quotations avoid those passages |
| LR-D7 | The "quality" guests infer is the listing's unobserved quality `ν`, not a trait of the host. The marker is the host's ethnicity, read from first name or photo | PDF pp. 8, 11. This is the main difference from A1, where the unseen trait is the person's own class |
| LR-D8 | The raw gap is "around 16 percent" on PDF p. 3 and "(17 percent)" on PDF p. 9 for the same −0.169 | The p. 9 figure reads log points as percent; as a price ratio the gap is between 15 and 16 percent (`raw_gap_percent`). The other gaps in the text are log points read as percent |
| LR-D9 | `ρ̂ = 13.6` in the notes to Tables 9 and 10 and `ρ = 13.7` in Table 4 are read as the same estimate rounded differently | Not relied on |
| LR-D10 | `proxyPrior`, `proxy_gap`, `perfect_proxy_restores_gap` and `control_uninformative_proxy` are our extension of the source's posterior to a hidden marker with a visible proxy. The source does not model concealment | Stated in the Lean file. They bear on the shadow market and are not attributed to the authors |
