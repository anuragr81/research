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
