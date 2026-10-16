# Moav and Neeman (2012), claims

Evidence tag [F]: the full text of the published article, 24 PDF pages
(journal pp. 933–956), was read on 16 October 2026 from its text layer, with
the page images of PDF pages 9, 12 to 14 and 16 to 20 read where the
extraction garbles symbols. Pages are PDF pages. The journal page is the PDF
page plus 932. For the second pass on 16 October 2026, PDF pages 2, 3, 4 and
10 were rechecked for the added quotations.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| MN-Q1 | 3 | "An individual's status is defined by the social beliefs about his or her unobservable income." | Status is the public's belief about income, as in eq. (3) |
| MN-Q2 | 3 | "In this equilibrium, it is possible to infer the exact income of each individual based on the individual's level of human capital and expenditure on conspicuous consumption." | The equilibrium separates. The belief about a person is a point, not a group distribution |
| MN-Q3 | 5 | "The idea that there could be trade-offs between different signals of income is consistent with the findings of Charles et al. (2009), who support their claim that race provides a signal of income by showing that the gap between races in spending on visible goods increases with the average wage gap between races across the US." | Their reading of Charles et al. (2009), checked against the working paper in MN-D5 |
| MN-Q4 | 5 | "A testable prediction of the model is that providing an external sign on income should reduce conspicuous spending or spending on visible goods." | A revelation test of a signalling channel, on the sender's spending |
| MN-Q5 | 9 | "we assume that the perceived status of an individual is given by the social belief about the individual's expected income conditional on the individual's level of human capital and conspicuous consumption, both of which we assume to be observable." | Status is `E(y ∣ h, x)`. Human capital is an observed trait correlated with income |
| MN-Q6 | 13 | "This has to be the case because in a fully separating equilibrium an individual's income is known." | The initial condition `x(h, y̲(h)) = 0`, formalised as `x_at_floor` |
| MN-Q7 | 14 | "It follows that for high h human capital becomes a more precise signal of income, relative to the total variance in income." | The authors' gloss on the convexity of `y̲(h)`. The result itself uses only the lower bound (MN-Q9) |
| MN-Q8 | 14 | "if y(h) was non-decreasing but linear or concave, then the share of conspicuous consumption out of total income of the average individuals in the economy would have been constant or increasing in h, respectively." | Checked against `control_prop2_constant_floor` in MN-D1 |
| MN-Q9 | 15 | "Thus, it is enough to know the lowest possible income of an individual with a given level of human capital h to solve for the equilibrium expenditure on conspicuous consumption. [...] then the distribution would have had an effect on the expected income in that set of individuals." | Footnote 15. Under separation only the lower bound matters. Under pooling the group's distribution enters the belief |
| MN-Q10 | 16 | "It is crucial to the existence of a poverty trap that human capital is observable." | The Remark, formalised as `remark_constant_share` and `control_unobserved_constant` |
| MN-Q11 | 18 | "it should lead to a decrease in charitable contributions if they are mostly about signalling." | The revelation test applied to charity |
| MN-Q12 | 19 | "This is non-increasing if and only if y(h)/h is non-decreasing, which follows from the fact that y(h) is non-decreasing and convex." | The proof of Proposition 2, whose first step is `prop2` and whose second step fails in general (MN-D1) |
| MN-Q13 | 19 | "the constant term must be equal to zero because y(h, 0) must be equal to zero for every h" | The proof of Proposition 1, inconsistent with eq. (8) (MN-D2) |
| MN-Q14 | 3 | "banned gold teeth, the use of cell phones in universities and large birthday parties." | A government closes channels of display. The paper reports it and does not model where display goes next |
| MN-Q15 | 3 | "restricted the number of people and amount of food served at weddings" | The same policy, a group-neutral cap on a visible expense |
| MN-Q16 | 2 | "those with high human capital have a recognisable ability (professional titles, degree certificates etc.) and relatively little need to signal success" | A credential substitutes for spending as a signal of income |
| MN-Q17 | 4 | "the trade-off between observable human capital and conspicuous consumption as signals of income may play a crucial role in explaining saving patterns and the persistence of poverty." | The two signals of income are substitutes, `x_anti_floor` |
| MN-Q18 | 4 | "there could be a neighbourhood effect such that wealthier families, or rather those with higher levels of education, live in areas in which the return to education is higher" | Sorting by place is named as a rival mechanism and not modelled |
| MN-Q19 | 10 | "provides a simple expression for the notion that individuals may attempt to signal their wealth by shifting their expenditures towards more visible consumption goods." | Visible spending is taken from the hidden budget, eqs. (4) to (6) |

## Lean results

`lean/mathlib/MoavNeeman2012.lean`, in the paper's notation, with `l` for `λ`,
`yl` for `y̲(h)`, `p` for `|π̲|` and `w` for `π`.

| Theorem | What it checks |
|---|---|
| `MoavNeeman2012.eq45_split` | Eqs. (4) and (5). The split `c = (1 − β)(y − x)`, `b = β(y − x)` maximises the Cobb–Douglas bundle |
| `MoavNeeman2012.eq6_reduced` | Eq. (6). With that split and the constant `B`, utility (3) is `(y − x)^{1−λ} S^λ` |
| `MoavNeeman2012.u_eq_exp_U` | Utility (6) is the exponential of `U = (1 − λ) log(y − x) + λ log ỹ` |
| `SeparatingEq` (structure) | The Definition, parts (i) and (ii), for a fully separating equilibrium at a fixed `h` (a structure, not a theorem) |
| `MoavNeeman2012.eq9_solves_eq7` | Eq. (9) solves the first order condition (7) along `ỹ = y` |
| `MoavNeeman2012.eq8_from_eq9` | Eq. (9) satisfies the implicit eq. (8) |
| `MoavNeeman2012.prop1_ic` | Proposition 1, existence. No income gains by mimicking another income's spending under eq. (9) |
| `MoavNeeman2012.prop1_exists` | Proposition 1, existence. Eq. (9) is a `SeparatingEq` |
| `MoavNeeman2012.prop1_floor` | The lowest type spends nothing in any separating equilibrium |
| `MoavNeeman2012.prop1_foc` | A differentiable separating equilibrium satisfies eq. (7) |
| `MoavNeeman2012.prop1_unique` | Proposition 1, uniqueness, among strategies continuous on `[y̲, ∞)` and differentiable above `y̲` |
| `MoavNeeman2012.x_at_floor` | Property (i). `x(h, y̲(h)) = 0` |
| `MoavNeeman2012.x_strictMonoOn` | Property (ii). Spending rises in income |
| `MoavNeeman2012.x_concaveOn` | Property (iii). Spending is concave in income |
| `MoavNeeman2012.x_slope_at_floor` | Property (iv). The slope at `y̲` is `λ/(1 − λ)` |
| `MoavNeeman2012.x_slope_atTop` | Property (v). The slope tends to `λ` |
| `MoavNeeman2012.x_anti_floor` | Property (vi). At fixed income, a lower floor gives larger spending |
| `MoavNeeman2012.share_avg` | `x(h, h)/h = λ (1 − (y̲(h)/h)^{1/(1−λ)})`, the first line of the proof of Proposition 2 |
| `MoavNeeman2012.prop2` | If `y̲(h)/h` is non-negative and non-decreasing, the share of the average person is non-increasing in `h` |
| `MoavNeeman2012.ratio_mono_of_convex` | A convex `y̲` with `y̲(0) ≤ 0` makes `y̲(h)/h` non-decreasing |
| `MoavNeeman2012.prop2_corrected` | Proposition 2 under reading MN-D1 |
| `MoavNeeman2012.prop3_pointwise` | Proposition 3, the pointwise step of the proof |
| `MoavNeeman2012.prop3_integral` | Proposition 3. The expectation in the proof rises in `h` for any distribution with `π > π̲` almost surely |
| `MoavNeeman2012.prop3` | Proposition 3. The expected share falls in `h` and is at most `λ` |
| `MoavNeeman2012.remark_constant_share` | The Remark. With `y̲ = 0`, `x(y) = λ y` |
| `MoavNeeman2012.fn14_uniform` | Footnote 14. Rank status under a uniform distribution gives eq. (7) |
| `MoavNeeman2012.stepA1_i` | Proposition A1, step (i). Spending more and being believed poorer is dominated |
| `MoavNeeman2012.stepA1_ii` | Proposition A1, step (ii). Single crossing, so spending is non-decreasing in income |
| `MoavNeeman2012.phi_closed_form` | Section 4. `φ` in the paper's closed form |
| `MoavNeeman2012.phi_case_i` | Section 4, case (i). `φ(h) = θ + γ β (1 − λ) h` |
| `MoavNeeman2012.phi_case_ii` | Section 4, case (ii). `φ(h) = θ + γβ((1 − λ) h + λ (h − p)^a h^{1−a})` |
| `MoavNeeman2012.kk_deriv` | The slope of the nonlinear part of `φ` in case (ii) |
| `MoavNeeman2012.phi_ii_strictMono` | Case (ii). `φ` is increasing |
| `MoavNeeman2012.phi_ii_convex` | Case (ii). `φ` is convex |
| `MoavNeeman2012.phi_ii_slope_at_p` | Case (ii). The slope tends to `γβ(1 − λ)` at `p` |
| `MoavNeeman2012.phi_ii_slope_atTop` | Case (ii). The slope tends to `γβ` |
| `MoavNeeman2012.phi_two_crossings` | Figure 2. A low fixed point below `p` and a second above `p` exist under the paper's condition |
| `MoavNeeman2012.control_prop2_constant_floor` | Control. A constant lowest income `c > 0` is non-decreasing and convex, and the share of the average person rises in `h` |
| `MoavNeeman2012.control_unobserved_constant` | Control. With human capital unobserved, the share is `λ` at every income |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Eqs. (4), (5) | `eq45_split` | general | |
| Eq. (6) | `eq6_reduced` | general | |
| Definition (i), (ii) | `SeparatingEq` | special case | Fully separating equilibria at one `h`, with beliefs equal to the revealed income on the path and the worst belief `y̲` after spending nothing. Partially separating and pooling equilibria are not defined |
| Eq. (7) | `eq9_solves_eq7`, `prop1_foc` | general | |
| Eq. (8) | `eq8_from_eq9` | general | |
| Eq. (9) | `eq9_solves_eq7`, `prop1_unique` | general | |
| Proposition 1, existence | `prop1_exists` | general | At each `h` with `y̲(h) > 0` |
| Proposition 1, uniqueness | `prop1_unique` | special case | Among strategies continuous on `[y̲, ∞)` and differentiable above `y̲`. The paper's proof also works through the ODE and so presumes the same |
| Property (i) | `x_at_floor` | general | |
| Property (ii) | `x_strictMonoOn` | general | |
| Property (iii) | `x_concaveOn` | general | |
| Property (iv) | `x_slope_at_floor` | general | As the derivative at `y̲`, which is the limit since the derivative is continuous |
| Property (v) | `x_slope_atTop` | general | |
| Property (vi) | `x_anti_floor` | general | Stated through `y̲`, which falls with a smaller `h` or, at fixed `y`, with a larger `π` |
| Proposition 2 as stated | `control_prop2_constant_floor` | general | False as stated (MN-D1) |
| Proposition 2, corrected | `prop2`, `ratio_mono_of_convex`, `prop2_corrected` | general | Under `y̲(0) ≤ 0` |
| Proposition 3 | `prop3_pointwise`, `prop3_integral`, `prop3` | general | With `π > π̲` almost surely, which a continuous distribution gives |
| Remark | `remark_constant_share`, `control_unobserved_constant` | general | |
| Footnote 14 | `fn14_uniform` | general | |
| Footnote 15 | none | not formalised | A verbal statement on partially separating equilibria, which the record does not define |
| Section 4, `φ` | `phi_closed_form` | general | |
| Section 4, case (i) | `phi_case_i` | general | |
| Section 4, case (ii) | `phi_case_ii`, `kk_deriv`, `phi_ii_strictMono`, `phi_ii_convex`, `phi_ii_slope_at_p`, `phi_ii_slope_atTop` | general | Increasing and convex above `p = −π̲`, with slope tending to `γβ(1 − λ)` at `p` and to `γβ` at infinity |
| Section 4, two crossings (Figure 2) | `phi_two_crossings` | special case | Existence of a fixed point below `p` and one above `p` under the paper's condition. That there are exactly two is not formalised |
| Section 4, growth rate `βγ − 1` | `phi_ii_slope_atTop` | general | As the limit of the slope of `φ` |
| Proposition A1, steps (i), (ii) | `stepA1_i`, `stepA1_ii` | general | |
| Proposition A1, steps (iii) to (v) and the proposition | none | not formalised | Needs belief functions with jumps and the variant of the intuitive criterion as objects |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| MN-D1 | Proposition 2 is read with the condition that `y̲(h)/h` is non-decreasing, which a convex `y̲` with `y̲(0) ≤ 0` gives (`ratio_mono_of_convex`, `prop2_corrected`) and eq. (11) satisfies | The proof (MN-Q12) asserts that a non-decreasing, convex `y̲(h)` makes `y̲(h)/h` non-decreasing. A positive constant is non-decreasing and convex and makes `y̲(h)/h` fall, so the share rises (`control_prop2_constant_floor`). Convexity gives the ratio only with `y̲` extrapolating to at most 0 at `h = 0`. MN-Q8 on linear `y̲` has the same gap, since a linear `y̲` with a positive intercept gives a rising share |
| MN-D2 | The initial condition is `ỹ(h, 0) = y̲(h)`, as on p. 13, not 0 | The proof of Proposition 1 says the constant is zero because `y(h, 0)` is zero (MN-Q13), while eq. (8) has `y̲(h)^{1/(1−λ)}` on its right side |
| MN-D3 | The lower bound in eq. (10) is a negative constant `π̲ < 0` | Proposition 3 says "for some `π̲ > 0`" (p. 16), while Section 4 says "some `π̲ < 0`" (p. 17) and eq. (11) uses the absolute value of `π̲` |
| MN-D4 | Section 4, case (i), is read as `h_{t+1} = φ(h_t) = θ + γ β (1 − λ) h_t` | The source prints `φ(h_{t+1})` on the left side (p. 17). `phi_case_i` |
| MN-D5 | MN-Q3 describes Charles et al. (2009), the QJE version, which is not read. The working paper read (`lit/charles_hurst_roussanov_2007/`) models race as the reference group, not as a signal, and shows visible spending falling in the own-group mean income | No result of Charles et al. is attributed through this paper's description |
| MN-D6 | The text layer renders `y̲(h)` as `y(h)`, `=` as `¼`, `λ` as `k` and opening quotation marks as `Õ` | Quotations avoid symbols other than `y(h)` and `h`, which the reader resolves from the page images |
