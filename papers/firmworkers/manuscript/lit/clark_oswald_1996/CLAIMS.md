# Claims about Clark and Oswald (1996)

**Source.** CO, authors' manuscript revised August 1995, pp.1–27, read in full
[F]; the JPubE 61(3) pages are not read. See `RECONSTRUCTION.md`. Cited in
`claims.yaml` as row L25 and in `refs.bib` as `ClarkOswald1996`. Pages are the
manuscript's. Quotations are checked verbatim on their page by `verify_co.py`.
Results are proved in `lean/Lit/ClarkOswald1996.lean` (see `LEAN`), namespace
`ClarkOswald1996`.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CO-1 | 1 | "One of the most interesting ideas in social science is the notion that happiness depends upon relative income." | The hypothesis |
| CO-2 | 1 | "a common theme in the psychology, sociology and administrative science literatures is the concept of a reference level of income against which an individual compares himself or herself" | A reference level of income |
| CO-3 | 2 | "may be consistent with the view that utility depends on the gap between outcomes and aspirations, and that education raises aspiration targets" | Aspirations, as one reading |
| CO-4 | 2 | "where y* is a comparison or reference income level against which the individual compares himself or herself" | `y*` defined |
| CO-5 | 3 | "This equation provides a predicted or expected wage that is taken as a proxy for comparison income." | `y*` as predicted pay |
| CO-6 | 3 | "Alternatively, an individual's peers' wage might simply be measured." | Peers' pay as the alternative |
| CO-7 | 6 | "This paper treats people's reported satisfaction levels as proxy utility data." | The evidence is reported satisfaction |
| CO-8 | 8 | "The distribution of reported satisfaction levels for the sample of 5195 British employees in the BHPS data set is as follows." | The sample |
| CO-9 | 10 | "These y* levels correspond to the income of 'typical' employees of given characteristics." | Comparison income as a group norm |
| CO-10 | 10 | "which he or she knows is the going rate of pay for someone like him or her" | The going rate for people like oneself |
| CO-11 | 10 | "One hypothesis is that the utility of person k depends on the gap between yk and y*k." | The gap to the norm |
| CO-12 | 11 | "Comparison income enters negatively and significantly." | The main finding |
| CO-13 | 11 | "the restriction that y and y* have equal and opposite signs is not rejected by the data" | A pure relative-income form, in Table 2 column 2 |
| CO-14 | 13 | "Comparison-wage effects are quantitatively important as well as statistically significant." | Size |
| CO-15 | 13 | "Taking a conservative central estimate of y*'s coefficient to be -0.2, the implied change in the (latent) satisfaction variable is approximately -0.3 points." | The magnitude calculation |
| CO-16 | 14 | "Counter to what neoclassical economic theory might lead one to expect, highly educated people appear less content." | The education finding |
| CO-17 | 15 | "This may be because education induces higher aspirations, but (as a referee has pointed out) there is no guarantee that this is the right interpretation." | Its interpretation is open |
| CO-18 | 15 | "Despite what economics textbooks say, comparisons in the utility function seem to matter." | Conclusion |
| CO-19 | 17 | "Arguably it is close co-workers' wages that matter most to an individual (as Frank, 1985, suggests), but that cannot easily be explored with these data." | The reference group is not identified |
| CO-20 | 18 | "Most regressions have the counter-intuitive implication that a ten percent rise in both y and y* would leave an individual worse off." | Footnote 16 |

## Results in Lean (`lean/Lit/ClarkOswald1996.lean`)

| Result | Lean | What is proved |
|---|---|---|
| Relative income (p.11) | `ClarkOswald1996.relative_income_form` | with coefficients `b` and `−b`, `b log y − b log y* = b log(y/y*)` |
| Hourly income (p.11) | `ClarkOswald1996.hourly_income_form` | with coefficients `b` and `−b` on income and hours, the form depends on `y/h` |
| Footnote 16 (p.18) | `ClarkOswald1996.common_rise_changes_satisfaction`, `ClarkOswald1996.equal_rise_worse_off` | a common rise by `k` changes the index by `(b_y + b_*) log k`; with 0.11 and −0.20, a 10 per cent rise lowers it |
| Magnitude (p.13) | `ClarkOswald1996.magnitude_of_comparison_effect` | `(7.3 − 5.7) × (−0.2) = −0.32`, within 0.05 of the stated −0.3 |
| Distribution (p.8) | `ClarkOswald1996.satisfaction_distribution_total`, `ClarkOswald1996.top_category_share` | the counts sum to 5195; the top category is 31.7 per cent |

Control: `ClarkOswald1996.control_ratio_form_needs_equal_and_opposite`.

## Readings recorded

- CO-D1 (p.11): "the satisfaction equation look a little like" is the source's
  wording; not quoted.
- CO-D2 (Table 2): the χ² for `y + y** = 0` in column 3 is 10.8, so the
  equal-and-opposite form holds for the Mincer `y*` (column 2, 2.8) but not for
  the NES `y**`; the text states the former only. The 5 per cent critical value
  is not given in the paper and is not used here.
