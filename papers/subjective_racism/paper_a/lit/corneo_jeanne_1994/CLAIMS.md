# Corneo and Jeanne (1994), claims

Evidence tag [F]: the full text of University of Bonn Discussion Paper A-461
(December 1994), 24 PDF pages, was read on 16 October 2026 from its text
layer, with the page images of PDF pages 1, 8 to 14 and 19 to 21 read where
the extraction drops symbols and ligatures. The published version (JPubE
1997) is not read (CJ-D1). Pages are PDF pages, which equal the printed page
numbers. For the second pass on 16 October 2026, PDF pages 3, 5, 7, 10
and 15 were rechecked for the added quotations.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CJ-Q1 | 6 | "Neither the individual's income nor his consumption of the [...] good is visible to spectators." | Income is hidden, as class is in A1 |
| CJ-Q2 | 6 | "Its sole purpose is to send a message about the individual's rank in the income hierarchy." | The good has no intrinsic utility. It is a pure signal of rank |
| CJ-Q3 | 6 | "Thus the message contained in conspicuous consumption is identically interpreted by all individuals." | Observers are homogeneous. Nothing depends on the observer's own rank |
| CJ-Q4 | 8 | "may separate the population into two groups, a group of rich who choose to [...] and a group of poor who prefer to abstain from it." | The priced good partitions the population by income, a cutoff in rank |
| CJ-Q5 | 8 | "The individuals who purchase the conspicuous good send a simple message, which is that they belong to the n richest part of the population, or equivalently that their rank is better than n." | Buying certifies membership in the top `n`. Each group is valued at its mean rank utility, eq. (8), formalised as `eq8` |
| CJ-Q6 | 9 | "the signaling value of the good may remain positive even if the rank utility is increasing on some interval." | The sign of `σ` depends on the shape of `a`, `control_rising_rank_utility` |
| CJ-Q7 | 9 | "the presence of conformism or snobbism depends on how social status is formed." | Conformism and snobbism come from the rank utility, not the good |
| CJ-Q8 | 10 | "In essence, the demand for conspicuous consumption is motivated mainly by the fear of being [...] with the poor." | Conformism is the cost of being pooled with the poor |
| CJ-Q9 | 11 | "If preferences are snobbish, the price function is decreasing." | Proposition 1, formalised as `prop1_snob` |
| CJ-Q10 | 16 | "These meetings are organized in order to make socially visible who is contributing and who is not." | A club priced above market makes payment visible, a price that certifies income |
| CJ-Q11 | 16 | "This provides an opportunity to signal high relative income and gain social recognition, which may explain why people are ready to pay a price above the market clearing level." | The same, the reason given for the price |
| CJ-Q12 | 17 | "Social status is some function of the individual's relative wealth, which itself is not observable by others." | The concluding summary of the model |
| CJ-Q13 | 18 | "Furthermore, the model is static, and therefore it does not allow to deal with evolution and learning in consumer behavior." | The model is static, as A1 is |
| CJ-Q14 | 3 | "the price will be used to signal the quality of the consumer rather than the quality of the product." | Footnote 2. The price carries information about the buyer |
| CJ-Q15 | 5 | "Our analysis is also closely related to Becker's (1991) model of restaurant pricing." | A venue's price and its clientele, the setting of A1's shadow market |
| CJ-Q16 | 15 | "The reason is that a good that is unexpensive has no signaling value in the snobbish case." | Proposition 2. A low price destroys the signal, `prop2` |
| CJ-Q17 | 15 | "allows for the marketing of socially undesirable status-signals that would not be supplied otherwise." | A seller can create a status signal by holding the price up |
| CJ-Q18 | 7 | "Individuals may want to signal a high relative income because this variable is correlated with the productive ability of individuals." | The labour-market reading. A signal of income stands in for an unobserved quality |
| CJ-Q19 | 10 | "Individuals are more sensitive to being ranked among the rich, than not being ranked among the poor." | Snobbism, the converse of CJ-Q8 |

## Lean results

`lean/mathlib/CorneoJeanne1994.lean`, in the paper's notation, with `s` for
a signaling value function, `abar a` for `ā`, `lam` for `λ` and `ub` for `ū`.

| Theorem | What it checks |
|---|---|
| `CorneoJeanne1994.eq4_buy` | Eq. (4). Buying pays exactly when `σ > u(y) − u(y − p)` |
| `CorneoJeanne1994.concave_increment` | For concave `u`, the cost `u(y) − u(y − p)` falls in income |
| `CorneoJeanne1994.buyers_upper` | Eqs. (5) to (7). Under concave `u` the buyers form an upper set of income |
| `CorneoJeanne1994.eq8` | Eq. (8). The difference of the mean rank utilities of buyers and non-buyers is `(1/(n(1 − n))) ∫_0^n (a − ā)` |
| `CorneoJeanne1994.sigma_lim_zero` | The limit `n → 0` of eq. (8) is `a(0) − ā` |
| `CorneoJeanne1994.sigma_lim_one` | The limit `n → 1` of eq. (8) is `ā − a(1)` |
| `CorneoJeanne1994.eq9` | Eq. (9). A signaling value satisfying (8) is generated by `a(n) = ā + (1 − 2n) σ(n) + n (1 − n) σ'(n)` |
| `CorneoJeanne1994.eq11` | Eq. (11) from eqs. (9) and (10) |
| `CorneoJeanne1994.eq11_gamma_one` | Eq. (11) with `γ = 1` is quadratic |
| `CorneoJeanne1994.gamma_one_conformist` | With `γ = 1`, `β > 0` is conformist |
| `CorneoJeanne1994.gamma_one_snobbish` | With `γ = 1`, `β < 0` is snobbish |
| `CorneoJeanne1994.gamma_one_concave` | With `γ = 1` and `β ≥ 0` the rank utility is concave |
| `CorneoJeanne1994.gamma_one_convex` | With `γ = 1` and `β ≤ 0` the rank utility is convex |
| `CorneoJeanne1994.prop1_price_exists` | Proposition 1. A price solving eq. (12) exists when `0 ≤ σ ≤ u(y_n) − u(0)` |
| `CorneoJeanne1994.prop1_price_unique` | Proposition 1. That price is unique |
| `CorneoJeanne1994.prop1_snob` | Proposition 1. Under snobbism the price function solving eq. (12) falls in `n` |
| `CorneoJeanne1994.eq13_conformist` | Eq. (13). `σ = n^γ` is conformist |
| `CorneoJeanne1994.eq11_gamma_two` | Eq. (11) with `α = 0`, `β = 1`, `γ = 2` is `r²(3 − 4r)` |
| `CorneoJeanne1994.ideal_rank_deriv` | Section 3. The derivative of the rank utility for `α = 0`, `β = 1` |
| `CorneoJeanne1994.ideal_rank` | It vanishes at the rank `γ/(2 + γ)` |
| `CorneoJeanne1994.example_eq16` | Section 3 with `μ = λ = 1`, `γ = 2`. `p(n) = log(1 + n)` solves eq. (12) for `σ(n) = n²` |
| `CorneoJeanne1994.example_eq16_general` | Eq. (16) solves eq. (12) for every `μ ≠ 0`, `λ`, `γ` |
| `CorneoJeanne1994.eq16_region` | The price of eq. (16) is affordable exactly when `n^γ + n^{μ/λ} ≤ 1` |
| `CorneoJeanne1994.eq17` | Eq. (17). `N(y_n) = n` at `p = y_n` |
| `CorneoJeanne1994.eq16_increasing` | For `γ > μ/λ` the price of eq. (16) rises in `n` |
| `CorneoJeanne1994.eq18_status_sums` | Status sums to `ā` across buyers and non-buyers |
| `CorneoJeanne1994.eq18_welfare` | Eq. (18). Welfare with `n` buyers is at most welfare with none |
| `CorneoJeanne1994.prop2` | Proposition 2. With a strictly decreasing price function of limit `p̂`, no semi-separating equilibrium exists below `p̂` |
| `CorneoJeanne1994.prop3_pool0` | Proposition 3. Pooling at 0 holds for all when it holds for the richest |
| `CorneoJeanne1994.prop3_passive` | Proposition 3 under passive conjectures |
| `CorneoJeanne1994.prop4_pool1` | Proposition 4. Pooling at 1 holds for all when it holds for the poorest |
| `CorneoJeanne1994.eq24` | Eq. (24). A supporting belief exists exactly when `ā − a(1) > u(y_1) − u(y_1 − p)` |
| `CorneoJeanne1994.prop4_passive` | Proposition 4. No pooling at 1 under passive conjectures |
| `CorneoJeanne1994.control_conformist_price_rises` | Control. In the conformist example the price rises, `p(1/4) < p(1/2)` |
| `CorneoJeanne1994.control_rising_rank_utility` | Control. With `a(r) = r` the signaling value is `−1/2` |
| `CorneoJeanne1994.control_fn8_conformist_not_concave` | Control, footnote 8. Conformism with a rank utility that is not concave |
| `CorneoJeanne1994.control_convex_buyers` | Control. With convex `u` a poorer consumer buys and a richer one does not |
| `CorneoJeanne1994.control_prop3_concavity` | Control. With convex `u` the condition at the richest does not give pooling for all |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Eqs. (1) to (3) | `Ur` | general | Definitions |
| Eq. (4) | `eq4_buy` | general | |
| Eqs. (5) to (7) | `buyers_upper` | special case | The buyers form an upper set. Existence and uniqueness of the threshold `ȳ(σ, p)` are not stated separately; they follow by the argument of `prop1_price_exists` with income in place of the price |
| Eq. (8) | `eq8` | general | For continuous `a` |
| Limits of eq. (8) at 0 and 1 | `sigma_lim_zero`, `sigma_lim_one` | general | |
| Eq. (9) | `eq9` | general | |
| Eqs. (10), (11) | `eq11` | general | |
| `γ = 1`: conformist exactly when concave | `eq11_gamma_one`, `gamma_one_conformist`, `gamma_one_snobbish`, `gamma_one_concave`, `gamma_one_convex` | general | |
| Footnote 8 | `control_fn8_conformist_not_concave` | general | A counterexample, as the footnote asserts |
| Proposition 1, `p(n)` unique | `prop1_price_exists`, `prop1_price_unique` | general | |
| Proposition 1, `p(n)` continuous | none | not formalised | Needs a continuous inverse of `u` (CJ-D5) |
| Proposition 1, snobbish case | `prop1_snob` | general | Under concave `u` and income falling in rank |
| Eq. (13) and the ideal rank `γ/(2 + γ)` | `eq13_conformist`, `ideal_rank_deriv`, `ideal_rank` | special case | The rank is shown to be stationary, not shown to be the maximum |
| Eqs. (14) to (17) | `example_eq16_general`, `eq16_region`, `eq17`, `example_eq16` | general | |
| Upward slope for `γ > μ/λ` | `eq16_increasing`, `control_conformist_price_rises` | special case | Sufficiency. The text's "only if `γ` and `λ` are sufficiently large and `μ` is sufficiently small" is a verbal reading and is not formalised |
| Eq. (18) and the nil optimum | `eq18_status_sums`, `eq18_welfare` | general | For a price at least 0 and continuous `u(y_r)` |
| Section 4.1, a tax may raise consumption | `eq16_increasing` | special case | Shown for the example of Section 3 |
| Section 4.2, monopoly | none | not formalised | Verbal; the paper has no monopoly model |
| Proposition 2 | `prop2`, `prop3_passive`, `prop4_passive` | general | Given that `p(n)` has a limit `p̂` at 1 |
| Eq. (19), voluntary contributions | none | not formalised | The paper reduces it to Proposition 1 |
| Proposition 3, eqs. (20), (21) | `prop3_pool0`, `prop3_passive` | general | |
| Proposition 4, eqs. (22) to (24) | `prop4_pool1`, `eq24`, `prop4_passive` | general | Beliefs out of equilibrium are taken to range over `[a(1), a(0)]` |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| CJ-D1 | The version read is Discussion Paper A-461, "Conspicuous Consumption and the Existence of Upward Sloping Demand Curves", December 1994, cited as such. The article in *Journal of Public Economics* 66 (1997), "Conspicuous consumption, snobbism and conformism", is not read | The title page (PDF 1). The cache file name says 1997. Nothing is attributed to the 1997 article |
| CJ-D2 | In the proof of Proposition 1, snobbism is read as `σ'(n) < 0` | The proof (p. 11) writes "If preferences are snobbish, `σ'(n) > 0`", against the definition on p. 9, where snobbish means `σ` strictly decreasing. The conclusion needs the decreasing case (`prop1_snob`) |
| CJ-D3 | The text layer drops the ligatures fi, ff and fl, the accent of "numéraire" and the Greek symbols | Quotations avoid them or elide them, as in CJ-Q1, CJ-Q4 and CJ-Q8. CJ-Q7's page has the typo "resposible", which no quotation uses |
| CJ-D4 | This record is not Corneo and Jeanne (1998), "Social organization, status, and savings behavior", *JPubE* 70, which Charles, Hurst and Roussanov (2007, PDF pp. 21 and 34) cite. Moav and Neeman (2012, PDF p. 8) cite Corneo and Jeanne (1997), the published version of this paper, among works that identified a negative effect of status concern on savings and growth | The discussion paper read has no saving, no growth and no dynamics (CJ-Q13). That description fits the 1998 paper's title rather than this one. Neither citing paper's description is attributed to this record |
| CJ-D5 | The continuity of `p(n)` in Proposition 1 is not formalised | Existence, uniqueness and the snobbish monotonicity are |
| CJ-D6 | Figures 1 and 2 (PDF 21) are blank in the copy read, and nothing is attributed to them | The page image shows the captions only. The ideal rank `γ/(2 + γ)` stated for Figure 2 (p. 12) was checked against eq. (11) by hand |
