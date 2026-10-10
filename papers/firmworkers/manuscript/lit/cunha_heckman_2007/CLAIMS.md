# Claims about Cunha and Heckman (2007)

**Source.** NBER Working Paper 12840, January 2007, read in full [F]; the
*AER* Papers and Proceedings version is **not** read; the working paper is cited
as the version read (author's decision, 10 Oct 2026). See `RECONSTRUCTION.md` for the
copy, its sha256 and the cache path. Cited in `claims.yaml` as row L1 and in
`refs.bib` as `CunhaHeckman2007`. Pages are the working paper's printed pages.
Quotations are checked verbatim, page by page, by
`verify_cunha_heckman_2007.py`. Results are proved in
`lean/Lit/CunhaHeckman2007.lean` (see `LEAN`), namespace `CunhaHeckman2007`.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CH-1 | 7 | "An important feature of our technology is that the skills produced at one stage augment the skills attained at later stages. This effect is termed self-productivity." | Self-productivity in words |
| CH-2 | 8 | "Skills produced at one stage raise the productivity of investment at subsequent stages." | Dynamic complementarity in words |
| CH-3 | 9 | "We assume that ft is strictly increasing and strictly concave in It , and twice continuously differentiable in all of its arguments." | Regularity of technology (1) |
| CH-4 | 9 | "when stocks of skills acquired by period t − 1 (θt ) make investment in period t (It ) more productive" | Dynamic complementarity as a cross-partial, L1 |
| CH-5 | 10 | "when higher stocks of skills in one period create higher stocks of skills in the next period" | Self-productivity as a partial derivative, L1 |
| CH-6 | 11 | "The literature in economics assumes only one period of childhood." | The one-period benchmark (4) |
| CH-7 | 11 | "According to (4), when γ = 1/2, children A and B will have the same stocks of skills as adults. The timing of investment is irrelevant." | Timing irrelevance at γ = 1/2 |
| CH-8 | 11 | "if investment in period one is zero, I1 = 0, then it does not pay to invest in period two" | Leontief, no early investment |
| CH-9 | 12 | "For the technology of skill formation defined by (5), the best strategy is to distribute investments evenly, so that I1 = I2 ." | Even allocation under Leontief |
| CH-10 | 12 | "The elasticity of substitution 1/ (1 − φ) is a measure of how easy it is to substitute between I1 and I2 ." | Meaning of φ |
| CH-11 | 14 | "It is optimal to invest early if γ > (1 − γ) (1 + r)." | Perfect substitutes with prices |
| CH-12 | 14 | "For an interior solution, we can derive the optimal ratio of early to late investment" | Equation (9) |
| CH-13 | 14 | "the optimal I1 /I2 is close to zero for low values of γ, but explodes to infinity as γ approaches one" | Cobb–Douglas ratio |
| CH-14 | 15 | "the higher the multiplier, the more investment should be concentrated in the early ages" | The ratio rises in γ |
| CH-15 | 15 | "If constraint (10) binds, then early investment under lifetime liquidity constraints" | The second credit constraint (not formalised) |
| CH-16 | 18 | "and investments in different periods are not perfect substitutes, early income matters" | The third credit constraint |
| CH-17 | 18 | "If early income is low with respect to late income, the ratio I1 /I2 will be lower than the optimal ratio." | Constrained ratio below (9) |
| CH-18 | 18 | "Early income would not matter if σ = 1" | Linear utility removes the income effect |
| CH-19 | 23 | "Complementarity implies that early investment is more productive if it is followed up with late investment." | Balanced investment |
| CH-20 | 23 | "For a balanced program, high school graduation and college enrollment rates are, respectively, 91% and 37%." | Text sums of Table 1 (see CH-D2) |

## Results in Lean (`lean/Lit/CunhaHeckman2007.lean`)

| Result | Lean | What is proved |
|---|---|---|
| (4), timing, p.11 | `CunhaHeckman2007.ces_perfect_substitutes`, `CunhaHeckman2007.timing_irrelevant_at_half` | at φ = 1 the CES is linear; at γ = 1/2 profiles (0, I) and (I, 0) give the same output |
| (5), pp.11–12 | `CunhaHeckman2007.leontief_no_early_no_output`, `CunhaHeckman2007.leontief_even_is_optimal`, `CunhaHeckman2007.leontief_even_attains` | no early investment, no output; under a budget the even allocation is optimal |
| Dynamic complementarity, discrete | `CunhaHeckman2007.leontief_dynamic_complementarity`, `CunhaHeckman2007.substitutes_no_complementarity` | `min` has increasing differences; the linear technology has zero cross-difference |
| p.14, perfect substitutes | `CunhaHeckman2007.output_along_budget`, `CunhaHeckman2007.invest_early_iff` | output along the budget is linear in I₁; all-early beats all-late iff γ > (1 − γ)(1 + r) |
| (9) | `CunhaHeckman2007.optimal_ratio_of_foc`, `CunhaHeckman2007.optimal_ratio_cobbDouglas`, `CunhaHeckman2007.optimal_ratio_strictMono_gamma` | the first-order condition has the unique solution (9); at φ = 0 it is γ/((1 − γ)(1 + r)); it rises in γ |
| pp.17–18, binding constraints | `CunhaHeckman2007.constrained_ratio_power_of_focs`, `CunhaHeckman2007.constrained_ratio_of_focs`, `CunhaHeckman2007.derived_income_free_at_sigma_one`, `CunhaHeckman2007.derived_below_optimal_iff` | the conditions from (8) give (I₁/I₂)^{1−φ} = (γβ/(1 − γ))(c₁/c₂)^{1−σ}; at σ = 1 income drops out; with β(1 + r) = 1 and σ < 1 the ratio is below (9) iff c₁ < c₂ |
| The printed display, p.18 | `CunhaHeckman2007.printed_ratio_reading` | at γ = 1/2, r = 0, φ = 0, σ = 0, β = 1/2, c₁ = c₂ = 1 the printed formula gives 2 and the derived one 1/2 |
| Table 1 against the text | `CunhaHeckman2007.table1_text_matches`, `CunhaHeckman2007.table1_college_reading` | the text's percentages match Table 1 except 37% for 0.3755 |
| Controls | `CunhaHeckman2007.control_timing_needs_half`, `CunhaHeckman2007.control_invest_early_needs_budget`, `CunhaHeckman2007.control_ratio_needs_phi_lt_one`, `CunhaHeckman2007.control_below_needs_sigma_lt_one` | each conclusion fails when its hypothesis is dropped |

Not formalised: the first-order conditions themselves (hypotheses), the p.15
result under a binding `b' ≥ 0`, the general CES cross-partial, and every
estimate or simulation.

## Readings recorded

| ID | Page | Text | Our reading |
|---|---|---|---|
| CH-D1 | 18 | `I₁/I₂ = [γ/((1 − γ)(1 + r))]^{1/(1−φ)} [(wh + b − I₁)/(β((1 + α)wh − I₂))]^{(1−σ)/(1−φ)}` | With `s ≥ 0` and `b' ≥ 0` binding no saving links the stages, so `1 + r` cannot enter, and (8) puts `β` on `u(c₂)`, not on `c₂`. The conditions of (8) give `I₁/I₂ = [γβ/(1 − γ)]^{1/(1−φ)} (c₁/c₂)^{(1−σ)/(1−φ)}` (`constrained_ratio_of_focs`). The two differ by a factor of 4 at the point of `printed_ratio_reading`. The paper's three statements on p.18 hold for the derived ratio when `β(1 + r) = 1` (`derived_income_free_at_sigma_one`, `derived_below_optimal_iff`). We use the derived ratio |
| CH-D2 | 23 | "high school graduation and college enrollment rates are, respectively, 91% and 37%" | Table 1 gives 0.3755, which rounds to 38%. We use the table |
| CH-D3 | 9 | Dynamic complementarity is written `∂²f_t/∂θ_t∂I_t'` with a prime on `I_t` | Read as the cross-partial in `θ_t` and `I_t`, as the following sentence says |
