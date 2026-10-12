# Claims about Genicot and Ray (2017)

**Source.** GR, *Econometrica* 85(2) pp.489–519, read in full [F], the
Supplemental Material not read. See `RECONSTRUCTION.md` for the copy, sha256
and cache path. Cited in `claims.yaml` as row L20 and in `refs.bib` as
`GenicotRay2017`. Pages are the printed pages. Quotations are checked verbatim
on their page by `verify_gr.py`. Results are proved in
`lean/Lit/GenicotRay2017.lean` (see `LEAN`), namespace `GenicotRay2017`.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| GR-1 | 489 | "This paper develops a theory of socially determined aspirations, and the interaction of those aspirations with growth and inequality." | Socially determined reference points |
| GR-2 | 489 | "economy-wide outcomes determine individual aspirations, which in turn determine investment incentives and social outcomes" | The two-way loop |
| GR-3 | 489 | "aspirations that are moderately above an individual’s current standard of living tend to encourage investment, while still higher aspirations may lead to frustration" | Non-monotone response to the gap |
| GR-4 | 490 | "Aspirations are endogenous: they are influenced both by individual circumstances and by the ambient income distribution." | A reference from own income and the distribution |
| GR-5 | 490 | "an individual’s aspirations may be related to the mean of the entire distribution" | A global-mean reference is one case |
| GR-6 | 491 | "They assumed that the aspiration threshold for everyone at any date is given by the common value of average income at that date." | Bogliacino and Ortoleva (2015): a common mean reference |
| GR-7 | 492 | "In this literature, as in Dalton, Ghosal, and Mani (2016), the reference point is personally determined (say, by consumption history)." | Personal against social references |
| GR-8 | 492 | "At the heart of it is the fundamentally non-monotonic way in which individuals react to the gap between their aspirations and achievements." | What separates aspirations from status |
| GR-9 | 492 | "An aspiration thus acts as a reference point for the agent." | Aspirations are reference points |
| GR-10 | 493 | "It is only crucial that the aggregated function w0 + w1 display sufficient nonconcavity around the aspirational threshold." | Nonconcavity at the reference drives the results |
| GR-11 | 493 | "economic models of status achieve closure by using social outcomes external to the individual" | Social closure of the reference |
| GR-12 | 494 | "aspirations are social constructs and so located within the full gamut of observed incomes in society" | Range-boundedness |
| GR-13 | 494 | "Individuals might also use occupation, caste, religion, or geography to form their windows" | Group-defined windows, including inherited identity |
| GR-14 | 494 | "in which the population consists of several subgroups, or “ethnicities,” and their incomes enter the aspirations formation function in different ways" | Group-based aspirations as an extension |
| GR-15 | 494 | "With no cross-identity interactions, such an extension is trivial as it would simply represent several co-existing copies of our model. With interactions, fresh and interesting considerations would arise." | Cross-group interaction is left open |
| GR-16 | 497 | "there is a unique threshold value of aspirations below which aspirations are satisfied, and above which they are frustrated" | Proposition 2 |
| GR-17 | 501 | "If aspirations are range-bound, no steady state can involve perfect equality." | Proposition 5 |
| GR-18 | 501 | "This proposition comes from the convexification of utility caused by aspirations." | Local convexity at the reference |
| GR-19 | 502 | "Becker, Murphy, and Werning (2005) and Ray and Robson (2012) argued that endogenous risk-taking can also serve to disrupt equality" | Their placing of the status-gambling papers |
| GR-20 | 504 | "group-based aspirations within or across ethnic groups or other partitions of the population would increase the number of clusters" | Groups would add clusters |
| GR-21 | 508 | "The local income neighborhood model assumes an extremely segregated world in which no individual “sees” any individuals with incomes that are too different." | Full segregation is the limiting window |
| GR-22 | 510 | "We define aspirations as income or wealth thresholds that enter individual utility as “reference points.”" | Conclusion |
| GR-23 | 511 | "A third extension, also influenced in part by the notion of group-based aspirations, would link aspirations to frustration and subsequent violence." | Group aspirations and conflict, left open |
| GR-24 | 512 | "If aspirations are socially sensitive" | The proof of Observation 1; see GR-D2 |

## Results in Lean (`lean/Lit/GenicotRay2017.lean`)

| Result | Lean | What is proved |
|---|---|---|
| Observation 1 (pp.499, 512) | `GenicotRay2017.aspirations_ratio_decreasing` | if `Ψ(λy, λF) = λΨ(y, F)` and scaling all incomes up by `λ > 1` strictly raises `Ψ(y, ·)`, then `Ψ(y, F)/y` is strictly decreasing in `y > 0` |
| Lemma 1 (p.513) | `GenicotRay2017.gap_grows_with_income` | if `Ψ/y` does not rise and `y₁ ≥ Ψ(y₁)`, the gap `y − Ψ(y)` is larger at `y₂ > y₁` |
| Examples 1 and 3, aspirations as the average of own and mean income | `GenicotRay2017.average_aspirations_ratio`, `GenicotRay2017.average_aspirations_range`, `GenicotRay2017.average_aspirations_scale` | its ratio to income strictly falls, it stays within the income range, and it scales with incomes |
| Proposition 5 (pp.501, 512) | `GenicotRay2017.single_point_aspiration`, `GenicotRay2017.no_stay_at_aspiration` | at a one-point distribution range-boundedness forces `a = y∗`, and with `w1′(0) > 0` the two first-order inequalities for `z = a` cannot both hold |
| From (10) to (11) (p.498) | `GenicotRay2017.scale_reduction` | the objective in `z` equals `y^{1−σ}` times the objective in `g = z/y` and `r = a/y` |
| `g(r) > g` in Proposition 3 (p.512) | `GenicotRay2017.aspirations_raise_growth` | if the left side of the first-order condition rises in `g`, the right side falls, and aspirations add a positive term, the solution with aspirations lies above the one without |

Controls: `GenicotRay2017.control_ratio_needs_social_monotonicity`,
`GenicotRay2017.control_stay_needs_milestone_jump`,
`GenicotRay2017.control_growth_needs_positive_extra`.

## Readings recorded

- GR-D1 (p.512): Observation 1 defines `r(y, F) ≡ Ψ(y, F)/y`; its proof writes
  `r(y, F) = y/Ψ(y, F)` and concludes `r(y₂, F) > r(y₁, F)` for `y₂ > y₁`. With
  the reciprocal this is the stated result, that `Ψ/y` falls; read as a
  notational inversion (`aspirations_ratio_decreasing` proves the statement
  directly). Confirmed on the page image.
- GR-D2 (p.512): the proof assumes aspirations "socially sensitive"; the
  observation assumes them "socially monotone". Read as the same condition,
  applied in its strict form to a uniform scaling up of incomes.
