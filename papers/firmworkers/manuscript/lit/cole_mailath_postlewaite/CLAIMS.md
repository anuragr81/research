# Claims about Cole, Mailath and Postlewaite (1992, 1995)

**Sources.** CMP92, *JPE* 100(6) pp.1092–1125, read in full [F]; CMP95, CARESS
WP 95-14, read in full [F], the Quarterly Review version not read. See
`RECONSTRUCTION.md` for copies, sha256 and cache paths. Cited in `claims.yaml`
as rows L14 (CMP92) and L15 (CMP95) and in `refs.bib` as
`ColeMailathPostlewaite1992` and `ColeMailathPostlewaite1995`. Pages are the
printed pages. Quotations are checked verbatim on their page by
`verify_cmp.py`. Results are proved in `lean/Lit/ColeMailathPostlewaite.lean`
(see `LEAN`), namespace `ColeMailathPostlewaite`.

## Quotations, CMP92

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CMP92-1 | 1092 | "We interpret an agent's status as a ranking device that determines how well he or she fares in the nonmarket sector." | Status is a rank, and it governs the non-market sphere: the reading of `h` |
| CMP92-2 | 1093 | "We begin with the assumption that there are goods and decisions that are not allocated or made through markets." | The non-market sphere exists by assumption |
| CMP92-3 | 1093 | "willingness to make such a payment is not sufficient to guarantee inclusion" | Status is not bought |
| CMP92-4 | 1096 | "we interpret an agent's status as a ranking device that determines how well he fares with respect to nonmarket decisions" | Same, in the model section |
| CMP92-5 | 1097 | "Here social mobility refers to the nonmarket benefits of wealth rather than the equality of market opportunities." | Their "social mobility" is the wealth-to-status link, not the ladder |
| CMP92-6 | 1097 | "one in which agents' status is inherited and another in which status is determined solely by relative wealth" | The two norms: categorical versus market-driven status |
| CMP92-7 | 1097 | "In our model, people care only about status because in equilibrium it may affect variables that enter the utility function." | Status is instrumental, not in the utility function |
| CMP92-8 | 1099 | "The role of status in our model will be to rank the men." | Status is ordinal |
| CMP92-9 | 1099 | "A social norm is a status assignment rule that indicates how a man's status is determined and a prescription of status-dependent matching behavior." | What fixes status is a norm, outside the market |
| CMP92-10 | 1101 | "A man's status, then, is determined precisely by his capital relative to other men's capital, higher capital yielding higher status." | Wealth-is-status: status answers to market outcomes |
| CMP92-11 | 1101 | "His savings behavior cannot be distorted (from the level that is optimal when matching considerations are ignored) since any deviation in savings cannot reduce the quality of his match." | The bottom agent is undistorted |
| CMP92-12 | 1106 | "bequests will be larger when a son's match is more sensitive to the level of capital on the margin" | Under wealth-is-status the market decision is distorted by `m′` |
| CMP92-13 | 1107 | "This property states that in an equilibrium, no family's relative position changes." | Property 1 |
| CMP92-14 | 1107 | "matching considerations cause agents to (weakly) increase their savings" | Property 2 |
| CMP92-15 | 1108 | "In this equilibrium, which we call aristocratic, a man's status is inherited." | The aristocratic norm |
| CMP92-16 | 1108 | "In the initial period, there will be an exogenously given status assignment." | Status assigned outside the model, as `h` is |
| CMP92-17 | 1108 | "If a man matches with a woman with an endowment not equal to his status, the family line from that point on has zero status" | The status rule (12), loss branch |
| CMP92-18 | 1109 | "A family line is punished by having its status and, hence, match set to zero forever if one of the men in the line marries inappropriately." | Zero status is absorbing |
| CMP92-19 | 1109 | "An important feature of this equilibrium is that the path of the capital stock is identical to that in the involuntary matching model." | Under inherited status, market decisions are undistorted: the Weberian separation |
| CMP92-20 | 1110 | "there are men of sufficiently low status to make them willing to match with women that are higher than prescribed" | Low status wants to deviate; the norm needs a block |
| CMP92-21 | 1111 | "A man cannot save enough to induce a deviation next period by any woman with an endowment greater than or equal to his status level" | What blocks deviation: spread-out capital |
| CMP92-22 | 1112 | "The inequality in proposition 2 that provides a sufficient condition for an aristocratic equilibrium to exist is essentially that the capital distribution is sufficiently spread out." | Existence condition of the aristocratic equilibrium |
| CMP92-23 | 1113 | "An aristocratic equilibrium is essentially a Nash equilibrium in which each woman responds optimally to any first-round deviation by a man, but not necessarily a subgame perfect Nash equilibrium." | Its solution concept |
| CMP92-24 | 1113 | "the existence of aristocratic equilibria does not depend on adjusting deviating men's status levels to zero" | A proportional loss of status also works |
| CMP92-25 | 1117 | "there is no effect at all for the person with the lowest level of capital" | Competition from below, income-distribution remark |
| CMP92-26 | 1119 | "we believe that it is at least partially accurate for the United States" | Their own view of which norm describes which society |

## Quotations, CMP95

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CMP95-1 | 1 | "not one espoused by economists alone" | Footnote 1 cites Weber's *Protestant Ethic* for the social motivation of wealth |
| CMP95-2 | 2 | "an agent's concern for relative wealth is instrumental: he or she cares about relative wealth only because final consumption is related not just to wealth, but additionally to relative wealth" | Instrumental concern for rank |
| CMP95-3 | 3 | "the instrumental interest in relative wealth is linked to the absence of some relevant markets" | Rank matters because a market is missing |
| CMP95-4 | 5 | "a small change in an individual female's output can have a large effect on her rank" | `m′` is large when the distribution is tight |
| CMP95-5 | 7 | "It can also be seen from this example that it is competition from below that distorts individuals' effort decisions." | Competition from below |
| CMP95-6 | 8 | "The important property is that there is some prize (about which the females care) that is not allocated through standard markets, but rather can be obtained only through the wealth tournament." | The prize is rank-allocated, not sold |
| CMP95-7 | 15 | "when the desirable goods or decisions are allocated as prizes rather than sold, the standard welfare theorems regarding the Pareto optimality of the outcomes no longer apply" | Welfare theorems do not cover rank-allocated prizes |
| CMP95-8 | 15 | "Whenever an increase in an individual's position in the wealth distribution by itself increases the likelihood of obtaining desirable outcomes, optimal individual behavior will exhibit some of the qualitative features exhibited in the models analyzed above." | The generalisation beyond matching |

## Results in Lean (`lean/Lit/ColeMailathPostlewaite.lean`)

| Result | Lean | What is proved |
|---|---|---|
| Status as a rank (CMP92 pp.1092, 1099; CMP95 p.7) | `ColeMailathPostlewaite.rankBelow_mono`, `ColeMailathPostlewaite.rankBelow_bottom`, `ColeMailathPostlewaite.rankBelow_unchanged_by_removing_above` | rank is monotone in the ranked quantity, zero at the bottom, and unchanged when agents above are removed |
| Status rule (12), pp.1108–1109 | `ColeMailathPostlewaite.statusRule_keep`, `ColeMailathPostlewaite.statusRule_lose`, `ColeMailathPostlewaite.status_zero_absorbing`, `ColeMailathPostlewaite.status_path_keep`, `ColeMailathPostlewaite.status_path_lose` | status is kept along appropriate matches, zero after one deviation, and zero stays zero |
| Separability, p.1109 | `ColeMailathPostlewaite.status_stream_value`, `ColeMailathPostlewaite.argmax_unchanged_by_constant`, `ColeMailathPostlewaite.aristocratic_separable`, `ColeMailathPostlewaite.foc_aristocratic_is_exogenous` | the status stream sums to s/(1 − β); adding it changes no maximiser; the aristocratic first-order condition is the exogenous one (4) |
| Wealth-is-status distortion, p.1106 | `ColeMailathPostlewaite.wealth_is_status_foc_distorts` | with m′ > 0 in (10), the marginal utility of current consumption exceeds the exogenous level, the sign of Property 2 |
| Deviation condition (13), p.1110 | `ColeMailathPostlewaite.no_deviation_iff`, `ColeMailathPostlewaite.low_status_deviates`, `ColeMailathPostlewaite.high_status_never_deviates` | (1 − β) j ≤ i is the no-deviation condition; it fails for low i and holds for i ≥ 1 − β |
| L_t(j), pp.1109–1110 | `ColeMailathPostlewaite.lambda_star_identity`, `ColeMailathPostlewaite.value_closed_form_denominator`, `ColeMailathPostlewaite.indifference_level`, `ColeMailathPostlewaite.indifference_printed_form`, `ColeMailathPostlewaite.L_exceeds_k`, `ColeMailathPostlewaite.L_equals_k_at_zero_status` | the denominator of (8) equals λ*; the indifference solves to the printed L_t(j); L exceeds k for γ > 1 and j > 0 and equals it at j = 0 |
| Spread-out block, p.1111 | `ColeMailathPostlewaite.spread_out_blocks_deviation` | if L_t(j) ≥ A k_{t−1}(j), no feasible bequest reaches L_t(j) |
| CMP95 §2.1 closed form | `ColeMailathPostlewaite.cmp95_effort_closed_form`, `ColeMailathPostlewaite.cmp95_match_consistent`, `ColeMailathPostlewaite.cmp95_slope_falls_with_dispersion`, `ColeMailathPostlewaite.bottom_effort_undistorted` | l(j) = a(j)(1 + g)/2; m(y(j)) = j iff g(1 + g)α² = 1; g falls as α rises; the bottom agent's effort is undistorted |
| Controls | `ColeMailathPostlewaite.control_rank_changes_when_removing_below`, `ColeMailathPostlewaite.control_status_rule_needs_exact_match`, `ColeMailathPostlewaite.control_separability_needs_match_independent_of_capital`, `ColeMailathPostlewaite.control_deviation_needs_beta_lt_one`, `ColeMailathPostlewaite.control_L_exceeds_needs_positive_status`, `ColeMailathPostlewaite.control_slope_needs_dispersion_change` | each conclusion fails when its hypothesis is dropped |

Not formalised: existence (Proposition 1 and Appendix A), Properties 1 and 3–6
in full (Property 1 is in the entry-contest record), the Lemma of §III.D and
Proposition 2's inequality (15), CMP95 §3.

## Readings recorded

| ID | Page | Text | Our reading |
|---|---|---|---|
| CMP-D1 | 1110 | "Moreover, for any β, there are men of sufficiently low status to make them willing to match with women that are higher than prescribed." | By (13) the deviators are the men with i < (1 − β) j, so for the firmworkers model a status norm needs a block on low status, which CMP92 supplies by spread-out capital (p.1111). The text layer prints β as "3"; the quotation CMP92-20 starts after it |
| CMP-D2 | 1109 | V(k, s) = V*(k) + (1 − β)^{-1} s | Holds when the per-period match value equals the status index, m(s) = s, as (12) and the matching rule give on the equilibrium path |
| CMP-D3 | 1110, 1122 | The Lemma and Proposition 2 assume γ > 1 | The aristocratic equilibrium is shown to exist only for γ > 1 and spread-out capital; nothing is claimed for γ ≤ 1 |
| CMP-D4 | 1103, 1106, 1108 | Page headers garbled in the text layer ("1 103", "i o6", "1 0o8") | Pages are mapped by block index, not by header |
