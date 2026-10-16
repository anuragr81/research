# Mailath and Postlewaite (2006), claims

Evidence tag [F]: the full text of PIER Working Paper 06-003 (January 5, 2006)
was read on 16 October 2026 from its text layer, with the equation pages also
read as images. Pages are PDF pages of the cached file; the printed page is
the PDF page minus 1 (MP-D2). Quotations are checked against the text layer by
exact normalised containment. Words the text layer garbles are elided as
`[...]` (MP-D3). Nothing is quoted from the earlier versions.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| MP-Q1 | 3 | "it may be that in a particular society agents with lighter skin or a particular accent enjoy higher consumption than those with darker skin or a less desirable accent, even when those attributes have no productive value per se." | Skin colour is the paper's own example of a visible attribute that is valued in matching for reasons other than productivity, the rival channel to Paper A1's inference about class |
| MP-Q2 | 3 | "We refer to an attribute of an agent that has value only because of a social institution as a social asset." | The definition of a social asset |
| MP-Q3 | 4 | "Notice that this has nothing to do with any intrinsic desirability for blue eyes; within this same society it could equally well have been that brown eyes were a desirable attribute." | Which attribute is valued is an equilibrium selection, not a property of the attribute |
| MP-Q4 | 4 | "The consequence of social institutions that value such assets is that the variance of consumption in society is lower." | The variance in the paper is the variance of consumption across society, a welfare object, not a penalty on a group's spread in partner choice |
| MP-Q5 | 6 | "The critical implication of the assumption that consumption is joint is that agents prefer richer partners, all else equal." | Preferences over partners' income are vertical. Every agent prefers a richer partner, whatever the agent's own income |
| MP-Q6 | 7 | "An agent’s income level and the presence/absence of the attribute together constitute that agent’s characteristic." | Income and attribute together are the characteristic on which matching is defined |
| MP-Q7 | 8 | "Note that while matching occurs after income is realized, since matching is assortative on attribute as well as income" | Income is realised before matching and matching conditions on it, so income is seen and not inferred |
| MP-Q8 | 6 | "This attribute does not enter into agents’ utility functions." | The attribute has no direct value to a partner |
| MP-Q9 | 10 | "An Ly child will match with a high income agent (Hn), while an Ln child matches with an Ln" | The value of the attribute is the children's matching prospects under the mixed matching, `eq_3_4` |
| MP-Q10 | 13 | "The mixed matching (weakly) Pareto dominates the assortative matching if and only if the mixed matching is stable, i.e., inequality (3.8) holds." | Proposition 3, formalised through `prop3_Hn` and `prop3_Ln` |
| MP-Q11 | 18 | "The collapse of mixed matching is accompanied by a large increase in the variance of consumption." | The same welfare variance as MP-Q4 |
| MP-Q12 | 20 | "Not surprisingly, since the attribute is not productive, an Ly agent always wants to match with an Hn agent," | The rationed case of Section 5, where our reading of the printed difference and of (5.2) differs (MP-D4) |
| MP-Q13 | 30 | "Fang (2001) analyzes a model in which nonproductive attributes can have value in equilibrium because agents have imperfectly observed ability." | The authors place the inference channel in another paper |
| MP-Q14 | 30 | "Social assets play an entirely [...] role in our model as there is no asymmetric information, hence no role for signalling." | The authors' own statement that nothing is unseen in their model. The elided word is "different" (MP-D3) |
| MP-Q15 | 32 | "the stability of both the mixed and assortative matching is driven by anonymous considerations: the attribute has a certain value (which may be zero) and it is irrelevant how that attribute was acquired." | The equilibria are Markov in characteristics, with no sanctions |

## Lean results

`lean/mathlib/MailathPostlewaite2006.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `MailathPostlewaite2006.assort_values` | `V_y^A = 1/2 + k`, `V_n^A = 1/2 − k` (p. 8) |
| `MailathPostlewaite2006.prop1` | Proposition 1. No `Hn`, `Ly` pair blocks assortative matching if and only if `βk ≤ (1 − u)(1 − β)` or `u(1 − β) ≤ βk` |
| `MailathPostlewaite2006.assort_stable_unproductive` | With `k = 0` no pair blocks assortative matching |
| `MailathPostlewaite2006.eq_3_4` | Eq. (3.4) from (3.2) and (3.3) |
| `MailathPostlewaite2006.prop2` | Proposition 2. The mixed matching is stable if and only if (3.7) |
| `MailathPostlewaite2006.eq_3_8` | Eq. (3.8) is the left inequality of (3.7) at `k = 0` |
| `MailathPostlewaite2006.right_ineq_free` | The right inequality of (3.7) at `k = 0` holds for `u ≥ 1/2` |
| `MailathPostlewaite2006.values_k0` | The appendix values `V_y^M`, `V_n^M` at `k = 0` |
| `MailathPostlewaite2006.prop3_Hn` | Proposition 3 for `Hn` and `Ly`, `V_m^M ≥ V_H^A` if and only if (3.8) |
| `MailathPostlewaite2006.prop3_Ln` | Proposition 3 for `Ln`, `V_Ln^M ≥ V_Ln^A` if and only if (3.8) |
| `MailathPostlewaite2006.rationed_diff` | Our reading of the rationed difference, denominator `4 − 3β + αβ` (MP-D4) |
| `MailathPostlewaite2006.printed_diff_differs` | The printed difference, denominator `4 − β − αβ`, is not the solution of the printed value equations when `0 < β < 1` and `α < 1` |
| `MailathPostlewaite2006.rationed_HnIC` | Our reading of the `Hn` constraint under rationing, `4 − 4β + αβ ≤ u(4 − 4β + 2αβ)` |
| `MailathPostlewaite2006.rationed_threshold_example` | At `β = α = 1/2`, `u = 91/100` the `Hn` constraint holds on our reading while `u` is below the printed threshold (5.2) |
| `MailathPostlewaite2006.control_no_altruism` | Control. With `β = 0` the `Hn` constraint fails for every `u < 1`, so Proposition 2 needs care for descendants |
| `MailathPostlewaite2006.control_risk_neutral` | Control. With `u = 1/2` (linear `U`) and `k = 0` the mixed matching is not stable for any `β < 1`, so (3.8) and Proposition 3 need concavity |
| `MailathPostlewaite2006.control_productive_blocks` | Control. Dropping `k = 0`, at `β = 1/2`, `u = 9/10`, `k = 3/20` an `Hn`, `Ly` pair blocks assortative matching |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| MP-D1 | The version read is PIER WP 06-003, January 5, 2006, titled on its cover "Social Assets, Third Version". The SSRN page bound in at PDF pp. 39 to 41 calls it "Second Version" and "PIER Working Paper No. #". The published article in the International Economic Review is not read | The record cites the working paper. Rows that cite the published article carry an unverified mark until it is read |
| MP-D2 | Pages are PDF pages. The printed page is the PDF page minus 1 from PDF p. 2 | The cover is PDF p. 1 |
| MP-D3 | The text layer renders `fi`, `ff`, `fl` as `…`, `¤`, `‡`, and `β`, `μ`, `α`, `ρ` as `¯`, `¹`, `®`, `½`. Quotations avoid such words or elide them as `[...]`. MP-Q14 elides "different" | Equations were read from page images, PDF pp. 7, 9, 11, 19, 20 |
| MP-D4 | On p. 20 (printed 19) the difference under rationing is printed as `[...] − ((1 − α)/2)[u(1 − β) + (β/2)(V_y^M − V_n^M)]`, giving `2(1 − β)[1 − u + αu]/(4 − β − αβ)`. From the printed value equations (p. 19 and (3.3)) the bracket is `u(1 − β) − (β/2)(V_y^M − V_n^M)`, and the difference is `2(1 − β)[1 − u + αu]/(4 − 3β + αβ)`. The `Hn` constraint then reads `u ≥ 1 − αβ/(2(2 − 2β + αβ))`, not (5.2) `u ≥ 1 − αβ/(2(2 − β))`. The two agree at `α = 1`, both rise to 1 as `α → 0`, and the threshold still rises as `α` falls, so the qualitative statement of Proposition 5 stands while its threshold, and the `μ̄` of Proposition 6 defined from it, move | `rationed_diff`, `printed_diff_differs`, `rationed_HnIC`, `rationed_threshold_example`. The same printed line is in WP 04-025 (p. 20, read as an image). WP 02-003 has no rationed analysis |
| MP-D5 | In the proof of Proposition 8 (p. 35, printed 34) "two cases [...] `μ_y ≤ 1/2` and `μ_y < 1/2`" is read as `μ_y ≥ 1/2` and `μ_y < 1/2` | The next sentence supposes `μ_y ≥ 1/2` and the second case is `μ_y < 1/2` |
| MP-D6 | On p. 4 "some wealthy short people match with less wealthy people with blue eyes" is read as wealthy people without blue eyes | "short" is left over from the height example of WP 02-003 and 04-025 |
| MP-D7 | Four citations print as "?" on pp. 29 to 31. They are read as Corneo and Jeanne (1998), Heller (2003) and Cozzi (1998, twice and in the lawyer example) | WP 04-025 names them at the same places. The 06-003 reference list omits all three |
| MP-D8 | `p` is the probability of an income rise in Section 4 and of inheritance in Section 6; `α` is the income growth factor in Section 4, the rationing probability in Section 5, the cost parameter of Example 1 and the white-shoe fraction in Section 7. Each is read within its section. Example 1 drops the hypothesis `p < 1/2` that WP 04-025 states but still uses it | Notation collisions, not errors of substance |
| MP-D9 | WP 04-025 (June 2004) differs from 06-003 in exposition. It uses height as the example, "genetic" and "cultural" for `ρ = 1/2` and `ρ ≠ 1/2`, has a paragraph defining social institutions as shared principles, has no Fang (2001) paragraph and so no sentence like MP-Q14, lacks footnotes 7 and 9 of 06-003, and resolves the citations of MP-D7. Its models, propositions and the printed slip of MP-D4 are the same | Both versions read in full. No difference bears on Paper A1 |
| MP-D10 | WP 02-003 (February 2002) differs more. It has a longer introduction (Smith, Veblen, Duesenberry, networks, and Corneo and Gruner (2000), described in a footnote as taking a preference for higher-class mates as given), uses "social arrangements", has no rationed analysis and no counterpart of Proposition 5, writes the law of motion for `ρ < 1/2`, `μ_y > 1/2` as `μ_y − (1/2)μ_n(1 − ρ)`, which counts one of the two `Hn`, `Ly` contributions and is inconsistent with its own formula for `ρ > 1/2`, states its Proposition 5 under a garbled hypothesis "4−3β < 2−β", omits the `(1 − β)` and `β` weights in the first-order conditions of Section 6, gives a different white-shoe example (outputs 3 and 1, wage 1), and has a discussion point that two isolated communities can sustain different matchings | Read in full. In all three versions income and attribute are both known at matching, and 02-003 (p. 33) says matching depends only on current income and the attribute, so no difference bears on Paper A1 |
