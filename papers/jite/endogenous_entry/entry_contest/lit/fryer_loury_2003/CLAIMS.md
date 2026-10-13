# Claims about Fryer and Loury (2003)

**Source.** Roland G. Fryer, Jr. and Glenn C. Loury, "Categorical Redistribution
in Winner-Take-All Markets", NBER Working Paper 10104 (November 2003), 26
printed pages, downloaded from nber.org; read in full [F]. It was published as
"Affirmative Action in Winner-Take-All Markets", *Journal of Economic
Inequality* 3(3), 263-280 (2005), which is not checked. Pages are the printed
pages; the printed page is the PDF page minus 1. Cited in `MANUSCRIPT.tex` (row
L27) and in `refs.bib` as `FryerLoury2003`. Each quotation is checked verbatim,
on the page named, against the extracted text. The results we use are proved in
`lean/mathlib/FryerLouryTournament.lean` (see `LEAN`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| FT-1 | 2 | "Nature moves first and assigns a marginal cost of investment to each individual." | Private costs |
| FT-2 | 4 | "There is a continuum of workers with unit measure, and a large but finite number, N, of identical employers." | A continuum of workers |
| FT-3 | 5 | "The worker hired from each pair receives an exogenously given wage ω, while the one not hired receives zero." | A fixed prize in pair-wise contests |
| FT-4 | 6 | "In essence, the equilibrium effort supply for any worker depends (in the manner illustrated in equation 1) on the distribution of workers who have cost higher than his." | Proposition 1 |
| FT-5 | 8 | "Because of the asymmetries in the cost distributions there will be a short-fall in the share of category 2 agents hired by the employer, relative to their share of the worker population." | The group gap under laissez-faire |
| FT-6 | 9 | "Thus, the equilibrium handicap is independent of the effort levels of the contestants, and varies positively with the aggressiveness of the redistribution goal." | Sighted redistribution |
| FT-7 | 14 | "We conclude that there must be some pooling in equilibrium." | Blind redistribution |
| FT-8 | 17 | "It follows that in all D1 pooling equilibria, epool must equal zero." | The pool exerts no effort |
| FT-9 | 18 | "Proposition 3 highlights an important feature of category-blind redistribution" | Proposition 3 |
| FT-10 | 18 | "And, this pool increases with the aggressiveness of the redistributive goal." | The pool of non-investors grows with the rule |
| FT-11 | 18 | "In the extreme case where the employer strives for categorical parity, the only D1-equilibrium involves all workers supplying zero effort and the employer picking at random between them!" | The limit |

## Results in Lean (`lean/mathlib/FryerLouryTournament.lean`, namespace `FryerLoury2003`)

| Result | Lean | What is proved |
|---|---|---|
| Proposition 1 (pp.5-6) | `mimic_hasDerivAt`, `prop1_best_reply` | with effort falling in cost at the rate `ω f/c`, a worker who mimics another cost earns a payoff rising up to his own cost and falling after it, so no mimicking pays |
| The natural win rate (footnote 2, p.8) | `half_integral`, `gamma0_eq`, `share_shortfall_iff` | `∫ f_1 (1 - F_1) = 1/2`, so `γ_0 = 1/2 + ∫ f_1 (F_1 - F_2)`; category 2's share of wins is below its population share exactly when `γ_0 > 1/2` |
| Proposition 3 (pp.17-19) | `ehat_iff`, `lambda_iff`, `Gamma_hasDerivAt`, `Gamma_deriv_pos`, `pool_grows`, `Gamma_at_min` | the two indifference conditions give `ê` and the shadow price; the representation constraint rises in the cut-off at the rate `(f_1(1-F_2) - f_2(1-F_1))/2`, positive under the hazard-rate order, so a lower target lowers the cut-off and enlarges the pool exerting zero effort; at parity everyone pools |

Not formalised: that Definition 1's likelihood-ratio order gives the
distribution and hazard-rate orders (hypotheses in the Lean file), existence and
uniqueness of the D1 equilibrium, Proposition 2 (its formulas were checked by
integrating eq. (9) under the stated boundary conditions, by hand), and Section 4.

## Reading recorded

| ID | Page | Text | Our reading |
|---|---|---|---|
| FT-D1 | 7 | "We will assume that category 1 is “advantaged” relative to category 2, in that the category 1 cost distribution monotonically first-order stochastically dominates that of category 2." | Definition 1 gives `F_1 > F_2`, so category 2's costs dominate category 1's; the sentence states the dominance the wrong way round |
| FT-D2 | 6 | "because workers choose effort to maximize their net" | The first-order condition that follows is printed with `1 - G(e)`; the winning probability is `G(e)`, which is what the next line uses |
| FT-D3 | 18 | "and e∗ (c) must be non-decreasing, otherwise workers could not be best-responding" | Footnote 10: effort is non-increasing in cost, as the text before it says |

## Comparison with the model rows

Effort here is continuous and every worker competes, and costs are private.
The blind rule produces an extensive margin inside the contest: a pool of the
highest-cost workers who exert no effort and still compete, winning against
each other at random. The pool grows as the rule rewards effort less, which is
the direction of M28, where fewer pay as the rule weighs talent more. The rule
is a redistribution target enforced through signalling (the D1 refinement),
not a weight on talent, and nobody pays a fee. Under laissez-faire the group
with higher costs wins fewer contests (FT-5), the group gap of C10 for pair-wise
contests.
