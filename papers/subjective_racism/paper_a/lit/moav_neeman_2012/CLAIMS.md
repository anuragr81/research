# Moav and Neeman (2012), claims

Evidence tag [F]: the full text of the published article, 24 PDF pages
(journal pp. 933–956), was read on 16 October 2026 from its text layer, with
the page images of PDF pages 9, 12 to 14 and 16 to 20 read where the
extraction garbles symbols. Pages are PDF pages; the journal page is the PDF
page plus 932.

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

## Lean results

`lean/mathlib/MoavNeeman2012.lean`, in the paper's notation, with `l` for `λ`
and `yl` for `y̲(h)`.

| Theorem | What it checks |
|---|---|
| `MoavNeeman2012.eq9_solves_eq7` | Eq. (9) solves the first order condition (7) in the separating equilibrium, `dx/dy = (λ/(1 − λ))(y − x)/y` |
| `MoavNeeman2012.x_at_floor` | Property (i). `x(h, y̲(h)) = 0` |
| `MoavNeeman2012.share_avg` | `x(h, h)/h = λ (1 − (y̲(h)/h)^{1/(1−λ)})`, the first line of the proof of Proposition 2 |
| `MoavNeeman2012.prop2` | If `y̲(h)/h` is non-negative and non-decreasing, the share of the average person is non-increasing in `h` |
| `MoavNeeman2012.remark_constant_share` | The Remark. With `y̲ = 0`, `x(y) = λ y` |
| `MoavNeeman2012.phi_case_i` | Section 4, case (i). `φ(h) = θ + γ β (1 − λ) h` |
| `MoavNeeman2012.control_prop2_constant_floor` | Control. A constant lowest income `c > 0` is non-decreasing and convex, and the share of the average person rises in `h` |
| `MoavNeeman2012.control_unobserved_constant` | Control. With human capital unobserved, the share is `λ` at every income |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| MN-D1 | Proposition 2 is read with the condition that `y̲(h)/h` is non-decreasing, which eq. (11) satisfies | The proof (MN-Q12) asserts that a non-decreasing, convex `y̲(h)` makes `y̲(h)/h` non-decreasing. A positive constant is non-decreasing and convex and makes `y̲(h)/h` fall, so the share rises (`control_prop2_constant_floor`). Convexity gives the ratio only with `y̲` extrapolating to at most 0 at `h = 0`. MN-Q8 on linear `y̲` has the same gap, since a linear `y̲` with a positive intercept gives a rising share |
| MN-D2 | The initial condition is `ỹ(h, 0) = y̲(h)`, as on p. 13, not 0 | The proof of Proposition 1 says the constant is zero because `y(h, 0)` is zero (MN-Q13), while eq. (8) has `y̲(h)^{1/(1−λ)}` on its right side |
| MN-D3 | The lower bound in eq. (10) is a negative constant `π̲ < 0` | Proposition 3 says "for some `π̲ > 0`" (p. 16), while Section 4 says "some `π̲ < 0`" (p. 17) and eq. (11) uses `|π̲|` |
| MN-D4 | Section 4, case (i), is read as `h_{t+1} = φ(h_t) = θ + γ β (1 − λ) h_t` | The source prints `φ(h_{t+1})` on the left side (p. 17). `phi_case_i` |
| MN-D5 | MN-Q3 describes Charles et al. (2009), the QJE version, which is not read. The working paper read (`lit/charles_hurst_roussanov_2007/`) models race as the reference group, not as a signal, and shows visible spending falling in the own-group mean income | No result of Charles et al. is attributed through this paper's description |
| MN-D6 | The text layer renders `y̲(h)` as `y(h)`, `=` as `¼`, `λ` as `k` and opening quotation marks as `Õ` | Quotations avoid symbols other than `y(h)` and `h`, which the reader resolves from the page images |
