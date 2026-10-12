# Claims about Kahneman and Tversky (1979)

**Source.** KT, *Econometrica* 47(2) pp.263–291, read in full [F] through a
local OCR of the scan. See `RECONSTRUCTION.md` for the copy, the OCR procedure,
sha256 and cache paths. Cited in `claims.yaml` as row L21 and in `refs.bib` as
`KahnemanTversky1979`. Pages are the printed pages. Quotations are checked
verbatim on their page in the OCR text by `verify_kt.py`. Results are proved in
`lean/Lit/KahnemanTversky1979.lean` (see `LEAN`), namespace
`KahnemanTversky1979`.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| KT-1 | 263 | "This paper presents a critique of expected utility theory as a descriptive model of decision making under risk, and develops an alternative model, called prospect theory." | Scope |
| KT-2 | 263 | "The value function is normally concave for gains, commonly convex for losses, and is generally steeper for losses than for gains." | The shape the sub-section assumes below the reference |
| KT-3 | 264 | "In expected utility theory, risk aversion is equivalent to the concavity of the utility function." | The benchmark |
| KT-4 | 268 | "We label this pattern the reflection effect." | Reflection effect |
| KT-5 | 268 | "implies that risk aversion in the positive domain is accompanied by risk seeking in the negative domain" | Risk seeking for losses |
| KT-6 | 270 | "In contrast to these data, expected utility theory (with a concave u) implies that probabilistic insurance is superior to regular insurance." | The probabilistic-insurance argument |
| KT-7 | 273 | "implies that the carriers of value or utility are changes of wealth, rather than final asset positions that include current wealth" | Value on changes |
| KT-8 | 274 | "Gains and losses, of course, are defined relative to some neutral reference point." | The reference point |
| KT-9 | 274 | "the location of the reference point, and the consequent coding of outcomes as gains or losses, can be affected by the formulation of the offered prospects, and by the expectations of the decision maker" | The reference is not always the status quo |
| KT-10 | 276 | "Markowitz [29] was the first to propose that utility be defined on gains and losses rather than on final asset positions" | Credit to Markowitz |
| KT-11 | 277 | "An essential feature of the present theory is that the carriers of value are changes in wealth or welfare, rather than final states." | Value on changes |
| KT-12 | 278 | "Thus, we hypothesize that the value function for changes of wealth is normally concave above the reference point" | Shape above the reference |
| KT-13 | 279 | "Such perturbations can readily produce convex regions in the value function for gains and concave regions in the value function for losses. The latter case may be more common since large losses often necessitate changes in life style." | Large losses can make losses concave |
| KT-14 | 279 | "A salient characteristic of attitudes to changes in welfare is that losses loom larger than gains." | Loss aversion |
| KT-15 | 279 | "S-shaped value function is steepest at the reference point, in marked contrast to the utility function postulated by Markowitz [29] which is relatively shallow in that region" | Against Markowitz's shape |
| KT-16 | 279 | "Thus, the value function for losses is steeper than the value function for gains." | Loss aversion, derived |
| KT-17 | 286 | "In prospect theory, the overweighting of small probabilities favors both gambling and insurance, while the S-shaped value function tends to inhibit both behaviors." | Gambling from weights, not from the shape |
| KT-18 | 286 | "there are situations in which gains and losses are coded relative to an expectation or aspiration level that differs from the status quo" | Reference as aspiration |
| KT-19 | 287 | "This analysis suggests that a person who has not made peace with his losses is likely to accept gambles that would be unacceptable to him otherwise." | Unadapted losses and gambling |
| KT-20 | 287 | "In this case, the reference point is set to zero on the scale of wealth and the value function is likely to be concave everywhere" | Final-asset framing |

## Results in Lean (`lean/Lit/KahnemanTversky1979.lean`)

| Result | Lean | What is proved |
|---|---|---|
| Shape of the value function (pp.278–279; Problems 13, 13′) | `KahnemanTversky1979.concave_gains_subadditive`, `KahnemanTversky1979.convex_losses_superadditive` | with `v(0) = 0`, concavity on gains gives `v(a + b) ≤ v(a) + v(b)` and convexity on losses gives `v(a) + v(b) ≤ v(a + b)` |
| Loss aversion (p.279) | `KahnemanTversky1979.loss_aversion_from_symmetric_bets` | if smaller symmetric bets are preferred to larger ones and `v(0) = 0`, then `v(x) < −v(−x)` |
| Subcertainty (p.282) | `KahnemanTversky1979.subcertainty_from_allais` | the Allais preferences of Problems 1 and 2 give `π(.66) + π(.34) < 1` |
| Overweighting (p.281) | `KahnemanTversky1979.overweighting_from_lottery` | the lottery preference of Problem 14 with strictly concave `v` gives `π(.001) > .001` |
| Equation (2) (p.276) | `KahnemanTversky1979.equation_two_rewrites` | `v(y) + π(p)[v(x) − v(y)] = π(p)v(x) + [1 − π(p)]v(y)` |
| Probabilistic insurance under expected utility (p.270) | `KahnemanTversky1979.eu_prefers_probabilistic_insurance` | with `u(w) = 1`, `u(w − y) = 1 − p` and `u` strictly concave, `u(w − ry) > 1 − rp` |
| Unadapted losses (p.287) | `KahnemanTversky1979.unadapted_loss_accepts_gamble`, `KahnemanTversky1979.unadapted_loss_from_shape` | if a mixed prospect is just acceptable, then under the shifted prospect, with loss aversion and subcertainty, it beats the sure loss `−z`; the two shape premises follow from concavity for gains and convexity for losses |

Controls: `KahnemanTversky1979.control_loss_aversion_needs_aversion_to_bets`,
`KahnemanTversky1979.control_equations_differ_under_subcertainty`,
`KahnemanTversky1979.control_probabilistic_needs_strict_concavity`,
`KahnemanTversky1979.control_gamble_needs_subcertainty`.

## Readings recorded

- KT-D1: the OCR reads `0` as `O` in several places (e.g. "around O") and
  drops some lines (e.g. on pp.263–264); quotations avoid affected passages, and
  the passages relied upon were checked to be complete in the OCR.
- KT-D2 (p.287): the chain's first step, "by the properties of v", uses
  `v(x − z) ≥ v(x) − v(z)` and `v(−y − z) ≥ v(−y) + v(−z)`; these follow from
  concavity for gains and convexity for losses with `v(0) = 0`
  (`unadapted_loss_from_shape`), so the strict inequality comes from the later
  steps (loss aversion and subcertainty).
