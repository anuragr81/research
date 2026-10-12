# Claims about Hopkins and Kornienko (2004)

**Source.** HK, *AER* 94(4), pp.1085–1107, read in full [F]. See
`RECONSTRUCTION.md`. Cited in `claims.yaml` as row L28 and in `refs.bib` as
`HopkinsKornienko2004`. Pages are the journal's. `verify_hk.py` finds each
quotation verbatim on its own page in at least one of four renderings of that
page (layout, reading order, columns, OCR), all derived from the pinned PDF.
Results are proved in `lean/Lit/HopkinsKornienko2004.lean` (see `LEAN`),
namespace `HopkinsKornienko2004`.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| HK-1 | 1085 | "In the symmetric Nash equilibrium, each individual spends an inefficiently high amount on the status good." | Waste |
| HK-2 | 1085 | "In a richer society, almost all individuals spend more on conspicuous consumption, and individual utility is lower at each income level. In a more equal society, the poor are worse off." | Main results |
| HK-3 | 1085 | "That is, everyone increases conspicuous consumption in order to improve status, but any gain in status is cancelled out by the similarly increased expenditure of others." | Red Queen |
| HK-4 | 1086 | "Simply put, those at the bottom are worse off in a more equal society, but at the same time there are fewer of them." | Equality and the poor |
| HK-5 | 1086 | "our analysis concerns changes to utility at a fixed level of income, not at a fixed relative position or quantile" | Footnote 3: the comparison is at fixed income |
| HK-6 | 1086 | "In contrast, at the top of the income distribution an increase in equality will reduce the competition for status as it thins the ranks of the rich." | Equality and the rich |
| HK-7 | 1087 | "Ben Cooper et al. (2001) analyze a growth model where there is concern for status, but agents only interact with other agents of the same wealth, so there is no interaction between consumption choices and the distribution of income as there is here." | Status within the own wealth group |
| HK-8 | 1087 | "In their model, status as indicated by ordinal rank in the distribution is instrumental to a (marriage) matching." | Cole et al. |
| HK-9 | 1087 | "What is distinctive about our approach is the analysis of the interaction between the distribution of income, status, conspicuous consumption, and welfare." | HK's own novelty claim |
| HK-10 | 1087 | "To our knowledge, this has not been explored before." | HK's own novelty claim |
| HK-11 | 1088 | "We assume an economy consisting of a continuum of agents, identical except in terms of income." | Primitives |
| HK-12 | 1089 | "is a constant representing a guaranteed minimum level of status" | `a` |
| HK-13 | 1089 | "It is also more plausible that being uniquely first is more attractive than being equal first." | The role of `γ` |
| HK-14 | 1090 | "an agent's position F(x) in the distribution of consumption will be equal to his rank G(z) in the distribution of income" | Equilibrium rank |
| HK-15 | 1091 | "the additional expenditure on conspicuous consumption has no net effect on the individual's position in the social hierarchy" | Waste |
| HK-16 | 1091 | "Suppose that status, instead of being endogenously determined, was exogenously given as one's rank in the distribution of income" | Exogenous status |
| HK-17 | 1091 | "That is, richer agents have higher status." | Exogenous status |
| HK-18 | 1093 | "are better off under the more unequal distribution" | Proposition 2 |
| HK-19 | 1094 | "Thus, the poor would be better off in a more unequal society." | At the cooperative level too |
| HK-20 | 1095 | "In simple terms, if an income distribution GA >ULR GB, then GA is more equal and less dispersed than GB." | The ULR order |
| HK-21 | 1096 | "the closer together are people, the greater the incentive to differentiate oneself, that is, in a more densely packed society the marginal return to conspicuous consumption is higher" | Density drives spending |
| HK-22 | 1096 | "Indeed the above result indicates that the conspicuous consumption of the middle class rises." | Proposition 4 |
| HK-23 | 1096 | "If the distribution GB is a mean-preserving spread of GA, then in the more equal society A, the population density will typically be thinner in the tails of the distribution." | Tails |
| HK-24 | 1096 | "Consequently, we find that after a rise in equality those who still have low incomes will raise their conspicuous consumption in order to keep up." | `a = 0` |
| HK-25 | 1097 | "This offers a potential explanation for the phenomenon, first identified by Easterlin (1974), that happiness scores in the developed world do not seem to have been rising over time, despite substantial increases in average income." | Easterlin |
| HK-26 | 1097 | "then in the equilibrium each consumer demands" | Example 1 (the expressions are read from the page image) |
| HK-27 | 1098 | "the poorest face a marginal tax rate of 50 percent and the rich a zero marginal rate" | Example 2 |
| HK-28 | 1099 | "We find that the poor are made worse off by greater equality." | Conclusion |
| HK-29 | 1099 | "Such an increase in equality will also increase conspicuous waste by those on middle incomes or low and middle incomes, while conspicuous consumption by the rich may decrease." | Conclusion |
| HK-30 | 1099 | "First, the strength of our results is based on the comparison of the effects of different income distributions on individuals at a given income level." | The authors' caveat |
| HK-31 | 1100 | "Our results cast doubt on this argument and suggest that the presence of relative concerns does not provide an additional rationale for progressive taxation over and above the oldest one" | Policy conclusion |

## Results in Lean (`lean/Lit/HopkinsKornienko2004.lean`)

| Result | Lean | What is proved |
|---|---|---|
| Status (1) (p.1089) | `HopkinsKornienko2004.tie_penalty` | with `γ < 1`, a tie at the top gives `γ + a`, less than the `1 + a` of moving uniquely first |
| Misery loves company (p.1094) | `HopkinsKornienko2004.poor_worse_off_at_cooperative` | at a fixed allocation with `V > 0`, lower `G(z)` means lower utility |
| Cooperative level (7) | `HopkinsKornienko2004.cooperative_tangency` | for `V = xy`, `x_c = z/(2p)` satisfies `V_1/V_2 = p` |
| Tax formula (12) | `HopkinsKornienko2004.tax_formula_cobb_douglas` | for `V = xy` at the cooperative allocation, (12) is `zg(z)/(G(z) + a)` |
| Example 1 (p.1097) | `HopkinsKornienko2004.example1_solves_ode`, `HopkinsKornienko2004.example1_allocation`, `HopkinsKornienko2004.example1_exceeds_cooperative`, `HopkinsKornienko2004.example1_tax_one`, `HopkinsKornienko2004.example1_revenue_neutral` | `x = 2z/(3p)` has derivative `2/(3p)` and solves (6) with `G(z) = z`, `a = 0`; `y = z/3`; `x > z/(2p)`; `τ = 1`; a 1/3 tax on `x` and 1/3 subsidy on `y` give relative price 2, balance the budget at the cooperative allocation and raise as much as they pay out |
| Example 2 (p.1098) | `HopkinsKornienko2004.example2_cdf`, `HopkinsKornienko2004.example2_integrated_cdfs`, `HopkinsKornienko2004.example2_mean_preserving_spread`, `HopkinsKornienko2004.example2_ulr`, `HopkinsKornienko2004.example2_tax_from_formula`, `HopkinsKornienko2004.example2_revenue_neutral_rate`, `HopkinsKornienko2004.example2_rate_endpoints`, `HopkinsKornienko2004.example2_rate_decreasing`, `HopkinsKornienko2004.example2_poor_lower_status`, `HopkinsKornienko2004.example2_rich_higher_status` | `G_A(0) = 0`, `G_A(1) = 1`, `G_A(1/2) = 1/2`, density `6z(1 − z)`; `∫₀^z G_A = z³ − z⁴/2 ≤ z²/2 = ∫₀^z G_B`, equal at 1; the density ratio rises to 1/2 and falls after; `τ_A = 6(1 − z)/(3 − 2z)`; the revenue-neutral rate `3(1 − z)/(6 − 5z)` gives the same relative price, is 1/2 at 0 and 0 at 1 and decreases; `G_A(z) < z` below the median and `> z` above |

Controls: `HopkinsKornienko2004.control_tie_needs_gamma_lt_one`,
`HopkinsKornienko2004.control_example1_tax_needs_a_zero`,
`HopkinsKornienko2004.control_poor_comparison_needs_positive_V`.

## Readings recorded

- HK-D1 (p.1089, (1)): the status display is garbled in all renderings; read
  from the page image as `S(x, F(·)) = γF(x) + (1 − γ)F⁻(x) + a` with
  `γ ∈ [0, 1)`.
- HK-D2 (p.1097, Example 1): the demands "2z/(3p)" and "z/3" and the
  cooperative "z/(2p)" and "z/2" are garbled in the text renderings and read
  from the page image; `example1_solves_ode` confirms that `2z/(3p)` solves (6)
  for the uniform distribution with `a = 0`.
- HK-D3 (p.1098, Example 2): "TA(Z) = 6(1 − z)/(3 − 2z)" and
  "rT(z) = 3(1 − z)/(6 − 5z)" (OCR of `τ_A` and `τ_x`); the two are linked by
  revenue neutrality, `τ_x = τ_A/(2 + τ_A)`, which the paper does not state;
  `example2_revenue_neutral_rate` proves it.
- HK-D4 (p.1093): the bound of the "poor" interval is printed as `ẑ`, the first
  crossing of `G_A` and `G_B`; the renderings drop the accent.
- HK-D5: three quotations considered were dropped because a line-end hyphen or
  a page break splits them in every rendering ("first-price", "mean-preserving",
  and the Corollary 1 gloss on pp.1094).
