# Mailath and Postlewaite (2006), claims

Evidence tag [F]: the full text of PIER Working Paper 06-003 (January 5, 2006)
was read on 16 October 2026 from its text layer, with the equation pages also
read as images. Pages are PDF pages of the cached file; the printed page is
the PDF page minus 1 (MP-D2). Quotations are checked against the text layer by
exact normalised containment. Words the text layer garbles are elided as
`[...]` (MP-D3). Nothing is quoted from the earlier versions. Quotation IDs are
kept from the first pass; MP-Q8 and MP-Q11 were dropped in the second.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| MP-Q1 | 3 | "it may be that in a particular society agents with lighter skin or a particular accent enjoy higher consumption than those with darker skin or a less desirable accent, even when those attributes have no productive value per se." | Skin colour is among the authors' own examples of a visible attribute valued for reasons other than productivity, with income seen |
| MP-Q2 | 3 | "We refer to an attribute of an agent that has value only because of a social institution as a social asset." | The definition of a social asset |
| MP-Q3 | 4 | "Notice that this has nothing to do with any intrinsic desirability for blue eyes; within this same society it could equally well have been that brown eyes were a desirable attribute." | Which attribute is valued is an equilibrium selection, not a property of the attribute |
| MP-Q16 | 4 | "If the social institutions make blue eyes a desirable attribute, we see that the degree of assortativeness of matching on wealth is decreased relative to the case that matching is on wealth alone." | A valued visible attribute raises matching across income levels rather than lowering it |
| MP-Q4 | 4 | "The consequence of social institutions that value such assets is that the variance of consumption in society is lower." | The paper's variance is the variance of consumption across society, a welfare object, not a penalty on a group's spread in partner choice |
| MP-Q5 | 6 | "The critical implication of the assumption that consumption is joint is that agents prefer richer partners, all else equal." | The origin of assortative matching on income in the paper. Preferences over a partner's income are vertical |
| MP-Q6 | 7 | "An agent’s income level and the presence/absence of the attribute together constitute that agent’s characteristic." | Income and attribute together are the characteristic on which matching is defined, the type `Ch` |
| MP-Q7 | 8 | "Note that while matching occurs after income is realized, since matching is assortative on attribute as well as income" | Income is realised before matching and matching conditions on it, so income is seen and not inferred |
| MP-Q9 | 10 | "An Ly child will match with a high income agent (Hn), while an Ln child matches with an Ln" | The value of the attribute is the children's matching prospects under the mixed matching, `MailathPostlewaite2006.eq_3_4` |
| MP-Q10 | 13 | "The mixed matching (weakly) Pareto dominates the assortative matching if and only if the mixed matching is stable, i.e., inequality (3.8) holds." | Proposition 3, `MailathPostlewaite2006.prop3` |
| MP-Q12 | 20 | "Not surprisingly, since the attribute is not productive, an Ly agent always wants to match with an Hn agent," | A poor holder of the attribute always wants a rich partner without it, `MailathPostlewaite2006.rationed_LyIC`. The rationed case is where our reading differs (MP-D4) |
| MP-Q13 | 30 | "Fang (2001) analyzes a model in which nonproductive attributes can have value in equilibrium because agents have imperfectly observed ability." | The authors place the inference channel in another paper |
| MP-Q14 | 30 | "Social assets play an entirely [...] role in our model as there is no asymmetric information, hence no role for signalling." | The authors' own statement that nothing is unseen in their model. The elided word is "different" (MP-D3) |
| MP-Q15 | 32 | "the stability of both the mixed and assortative matching is driven by anonymous considerations: the attribute has a certain value (which may be zero) and it is irrelevant how that attribute was acquired." | The equilibria are Markov in characteristics, with no sanctions |
| MP-Q18 | 32 | "since the social skills play the role of a record device, similar to money as memory as described by Kocherlakota (1998)." | The one place the paper treats a social attribute as a record that can be priced |

## Lean results

`lean/mathlib/MailathPostlewaite2006.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `MailathPostlewaite2006.assort_values` | `V_y^A = 1/2 + k`, `V_n^A = 1/2 − k` (p. 8) |
| `MailathPostlewaite2006.valueEq_assort` | The same, from the value equations on characteristics |
| `MailathPostlewaite2006.prop1` | Proposition 1 on the `Hn`, `Ly` pair |
| `MailathPostlewaite2006.stable_assort_iff` | Under assortative matching only an `Hn`, `Ly` pair can block, checked over all pairs of characteristics |
| `MailathPostlewaite2006.prop1_stable` | Proposition 1 with stability over all pairs |
| `MailathPostlewaite2006.assort_stable_unproductive` | With `k = 0` no pair blocks assortative matching |
| `MailathPostlewaite2006.hy_never_deviates` | An `Hy` agent never gains by deviating (p. 8) |
| `MailathPostlewaite2006.valueEq_mixed` | The value equations on characteristics under mixed matching are (3.2) and (3.3) |
| `MailathPostlewaite2006.eq_3_4` | Eq. (3.4) |
| `MailathPostlewaite2006.stable_mixed_iff` | Under mixed matching, stability over all pairs is (3.5) and the `Ly` constraint, (3.6) |
| `MailathPostlewaite2006.prop2` | Proposition 2 on the two constraints, (3.7) |
| `MailathPostlewaite2006.prop2_stable` | Proposition 2 with stability over all pairs |
| `MailathPostlewaite2006.eq_3_8` | Eq. (3.8) is the left inequality of (3.7) at `k = 0` |
| `MailathPostlewaite2006.right_ineq_free` | The right inequality of (3.7) at `k = 0` holds for `u ≥ 1/2` |
| `MailathPostlewaite2006.multiple_equilibria_k0` | Section 3.3. Under (3.8) both matchings are equilibria |
| `MailathPostlewaite2006.values_k0` | The appendix values `V_y^M`, `V_n^M` at `k = 0` |
| `MailathPostlewaite2006.prop3_Hn` | The appendix, `Hn` and `Ly` |
| `MailathPostlewaite2006.prop3_Ln` | The appendix, `Ln` |
| `MailathPostlewaite2006.prop3` | Proposition 3. Every characteristic weakly gains under mixed matching if and only if (3.8) |
| `MailathPostlewaite2006.section34_config` | Section 3.4. Some `u` satisfies (3.7), (3.8) and (3.1) together |
| `MailathPostlewaite2006.crra_u_invariant` | Footnote 9. Under CRRA the normalised `u` is the same at `(αL, αH)` |
| `MailathPostlewaite2006.law_5_1_high` | Eq. (5.1) for `μ_y ≥ 1/2`, from the counted contributions |
| `MailathPostlewaite2006.law_5_1_low` | Eq. (5.1) for `μ_y < 1/2` |
| `MailathPostlewaite2006.rho_half_steady` | With `ρ = 1/2` every share is a steady state |
| `MailathPostlewaite2006.rho_lt_half_tendsto` | With `ρ < 1/2` the share converges to 0 from any `μ_y^0 < 1` |
| `MailathPostlewaite2006.rho_gt_half_tendsto` | With `ρ > 1/2` the share converges to 1 from any `μ_y^0 > 0` |
| `MailathPostlewaite2006.prop5_low` | Proposition 5, `μ_y < 1/2`. The mixed matching is stable if and only if (3.8) |
| `MailathPostlewaite2006.rationed_diff` | Our reading of the rationed difference (MP-D4) |
| `MailathPostlewaite2006.printed_diff_differs` | The printed difference is not the solution of the printed value equations |
| `MailathPostlewaite2006.rationed_HnIC` | Our reading of the `Hn` constraint under rationing |
| `MailathPostlewaite2006.rationed_LyIC` | Under rationing an `Ly` agent always wants an `Hn` partner, MP-Q12 |
| `MailathPostlewaite2006.prop5_high` | Proposition 5, `μ_y > 1/2`, on our reading |
| `MailathPostlewaite2006.rationed_threshold_example` | Values at which our reading and the printed (5.2) disagree |
| `MailathPostlewaite2006.prop7_core` | With a small enough probability of meeting `Hn`, the `Hn` constraint fails |
| `MailathPostlewaite2006.steady_state_assort` | The steady state `μ_y^A` of Section 6 is the fixed point of the law of motion with purchases |
| `MailathPostlewaite2006.example1_muA_zero` | Example 1. With no purchases and `p < 1/2`, `μ_y^A = 0` |
| `MailathPostlewaite2006.q_order` | Section 6. The richer couple buys more, so `q_HH > q_HL > q_LL` |
| `MailathPostlewaite2006.q_pos` | Section 6. With `c'(0) = 0` every couple buys a positive probability |
| `MailathPostlewaite2006.eq_6_3` | Eq. (6.3) is (6.1) rearranged |
| `MailathPostlewaite2006.eq_6_4` | Eq. (6.4) is (6.2) rearranged |
| `MailathPostlewaite2006.example2_gaps` | Example 2. `p_HL − p_LL > 0` and `p_HL − q_HH > ε` |
| `MailathPostlewaite2006.eq_decomp` | Example 2. The decomposition of `V_y − V_n` |
| `MailathPostlewaite2006.phi1_high` | Proof of Proposition 8. The map `Φ₁` for `μ_y ≥ 1/2` simplifies as printed |
| `MailathPostlewaite2006.phi1_low` | Proof of Proposition 8. The map `Φ₁` for `μ_y < 1/2` simplifies as printed |
| `MailathPostlewaite2006.lawyer_accept` | Section 7. A young lawyer accepts the apprenticeship at wage `α` if and only if `i ≤ α` |
| `MailathPostlewaite2006.lawyer_wage_two` | Section 7. At wage 2 every young lawyer is indifferent |
| `MailathPostlewaite2006.control_productive_blocks` | Control for Proposition 1. Dropping `k = 0`, an `Hn`, `Ly` pair blocks assortative matching at `β = 1/2`, `u = 9/10`, `k = 3/20` |
| `MailathPostlewaite2006.control_no_altruism` | Control for Proposition 2. With `β = 0` the `Hn` constraint fails for every `u < 1` |
| `MailathPostlewaite2006.control_risk_neutral` | Control for (3.8) and Proposition 3. With `u = 1/2` and `k = 0` the mixed matching is not stable for any `β < 1` |
| `MailathPostlewaite2006.control_config_needs_small_k` | Control for Section 3.4. With `k ≥ (1 − β)/β` the right inequality of (3.1) fails |
| `MailathPostlewaite2006.control_prop5_risk_neutral` | Control for Proposition 5. With `u = 1/2` the mixed matching is not stable when `μ_y < 1/2` |
| `MailathPostlewaite2006.control_rho_half_stays` | Control for Propositions 6 and 7. With `ρ = 1/2` the share does not move |
| `MailathPostlewaite2006.control_prop7_no_rationing` | Control for `prop7_core`. Without rationing the `Hn` constraint holds |
| `MailathPostlewaite2006.control_q_order_linear` | Control for `q_order`. With linear `U`, couples of different income can buy the same `q` |

## Coverage of the source's results

| Result (as the source numbers it) | Lean name | Status | Reason for anything short of general |
|---|---|---|---|
| The model and stability (Section 2) | `Ch`, `payoff`, `Matching`, `Blocks`, `Stable`, `ValueEq`, `Equilibrium` | special case | Defined for the balanced case `ρ = 1/2`, `μ_y = 1/2`, with deterministic matchings on characteristics. The rationed matchings of Section 5 are handled through their value equations, not on characteristics |
| `V_y^A`, `V_n^A` (p. 8) | `assort_values`, `valueEq_assort` | general | |
| (3.1) and Proposition 1 | `prop1`, `prop1_stable`, `stable_assort_iff`, `hy_never_deviates` | general | |
| (3.2), (3.3), (3.4) | `valueEq_mixed`, `eq_3_4` | general | |
| (3.5), (3.6) | `stable_mixed_iff` | general | |
| Proposition 2, (3.7) | `prop2`, `prop2_stable` | general | |
| (3.8) and the multiplicity of Section 3.3 | `eq_3_8`, `right_ineq_free`, `multiple_equilibria_k0` | general | |
| Proposition 3 and its appendix proof | `prop3`, `values_k0`, `prop3_Hn`, `prop3_Ln` | general | Stated for `0 < β < 1`. At `β = 0` both sides fail for `u < 1`, a degenerate case |
| Section 3.4, the configuration of `k`, `β`, `u` | `section34_config` | general | The story of a small probability that the attribute becomes unproductive is prose and is not formalised |
| Proposition 4 | none | not formalised | A limit of infinite-horizon value functions under a regime switch as `p → 0`. Footnote 9 is formalised as `crra_u_invariant`. The claim that vanishing relative risk aversion eventually breaks the constraint is not formalised |
| (5.1) | `law_5_1_high`, `law_5_1_low`, `rho_half_steady` | general | |
| Proposition 5 | `prop5_low`, `prop5_high`, `rationed_LyIC` | special case | Stability is formalised on both sides of `μ_y = 1/2`. The `μ_y > 1/2` case is our reading and differs from the printed (5.2) (MP-D4). The `μ_y < 1/2` value equations are our reconstruction of the paper's "similar calculation" (MP-D13). The Pareto clause is asserted by analogy in the paper and is not formalised |
| (5.2) | `rationed_HnIC`, `printed_diff_differs`, `rationed_threshold_example` | special case | The printed threshold does not follow from the printed value equations. Our reading is formalised |
| Proposition 6 | `rho_lt_half_tendsto` | special case | The convergence of `μ_y` to 0 is formalised. The existence of the lower bound on `ρ` with the mixed matching stable, and the convergence of utility, are continuity arguments on value equations with time-varying rationing, and are not formalised |
| Proposition 7 | `rho_gt_half_tendsto`, `prop7_core` | special case | The convergence of `μ_y` to 1, and the failure of the `Hn` constraint at a small probability of meeting `Hn`, are formalised. The unravelling by backward induction along a deterministic path is not |
| `μ_y^A` of Section 6 (p. 24) | `steady_state_assort` | general | |
| `1 > q_HH > q_HL > q_LL > 0` (p. 24) | `q_order`, `q_pos` | special case | The ordering and positivity are formalised for abstract `U'`, `c`, `c'`. The bound `q < 1` needs more on `c` than the paper states, and is not formalised |
| Proposition 8 | `phi1_high`, `phi1_low` | not formalised | Existence rests on Brouwer's fixed-point theorem, which Mathlib does not provide. Only the algebra of the map `Φ₁` in the proof is formalised |
| Example 1 | `example1_muA_zero` | special case | The assortative steady state is formalised. The stability of the mixed matching for a high cost parameter, and the continuity in `k`, are not |
| (6.1) to (6.5) and Example 2 | `eq_6_3`, `eq_6_4`, `example2_gaps`, `eq_decomp` | special case | The rearrangements and the two gaps are formalised. The bound `μ_y ≤ 4p(1 − p)`, the lower bound on `V_y − V_n` and the appeal to sufficient risk aversion are not |
| Section 7, the white-shoe lawyers | `lawyer_accept`, `lawyer_wage_two` | special case | The acceptance rule and the equilibrium in which the skills have no value are formalised. The Pareto ranking of the two equilibria is not |

Counts: 10 general, 9 special case, 2 not formalised.

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| MP-D1 | The version read is PIER WP 06-003, January 5, 2006, titled on its cover "Social Assets, Third Version". The SSRN page bound in at PDF pp. 39 to 41 calls it "Second Version" and "PIER Working Paper No. #". The published article in the International Economic Review is not read | The record cites the working paper. Rows that cite the published article carry an unverified mark until it is read |
| MP-D2 | Pages are PDF pages. The printed page is the PDF page minus 1 from PDF p. 2 | The cover is PDF p. 1 |
| MP-D3 | The text layer renders `fi`, `ff`, `fl` as `…`, `¤`, `‡`, and `β`, `μ`, `α`, `ρ` as `¯`, `¹`, `®`, `½`. Quotations avoid such words or elide them as `[...]`. MP-Q14 elides "different" | Equations were read from page images, PDF pp. 7, 9, 11, 19, 20, 24, 26, 27, 28, 35 |
| MP-D4 | On p. 20 (printed 19) the difference under rationing is printed as `[...] − ((1 − α)/2)[u(1 − β) + (β/2)(V_y^M − V_n^M)]`, giving `2(1 − β)[1 − u + αu]/(4 − β − αβ)`. From the printed value equations (p. 19 and (3.3)) the bracket is `u(1 − β) − (β/2)(V_y^M − V_n^M)`, and the difference is `2(1 − β)[1 − u + αu]/(4 − 3β + αβ)`. The `Hn` constraint then reads `u ≥ 1 − αβ/(2(2 − 2β + αβ))`, not (5.2) `u ≥ 1 − αβ/(2(2 − β))`. The two agree at `α = 1`, both rise to 1 as `α → 0`, and the threshold still rises as `α` falls, so the qualitative statement of Proposition 5 stands while its threshold, and the `μ̄` of Proposition 6 defined from it, move | `rationed_diff`, `printed_diff_differs`, `rationed_HnIC`, `rationed_threshold_example`. The same printed line is in WP 04-025 (p. 20, read as an image). WP 02-003 has no rationed analysis |
| MP-D5 | In the proof of Proposition 8 (p. 35, printed 34) "two cases [...] `μ_y ≤ 1/2` and `μ_y < 1/2`" is read as `μ_y ≥ 1/2` and `μ_y < 1/2` | The next sentence supposes `μ_y ≥ 1/2` and the second case is `μ_y < 1/2` |
| MP-D6 | On p. 4 "some wealthy short people match with less wealthy people with blue eyes" is read as wealthy people without blue eyes | "short" is left over from the height example of WP 02-003 and 04-025 |
| MP-D7 | Four citations print as "?" on pp. 29 to 31. They are read as Corneo and Jeanne (1998), Heller (2003) and Cozzi (1998, twice and in the lawyer example) | WP 04-025 names them at the same places. The 06-003 reference list omits all three |
| MP-D8 | `p` is the probability of an income rise in Section 4 and of inheritance in Section 6; `α` is the income growth factor in Section 4, the rationing probability in Section 5, the cost parameter of Example 1 and the white-shoe fraction in Section 7. Each is read within its section. Example 1 drops the hypothesis `p < 1/2` that WP 04-025 states but still uses it | Notation collisions, not errors of substance |
| MP-D9 | WP 04-025 (June 2004) differs from 06-003 in exposition. It uses height as the example, "genetic" and "cultural" for `ρ = 1/2` and `ρ ≠ 1/2`, has a paragraph defining social institutions as shared principles, has no Fang (2001) paragraph and so no sentence like MP-Q14, lacks footnotes 7 and 9 of 06-003, and resolves the citations of MP-D7. Its models, propositions and the printed slip of MP-D4 are the same | Both versions read in full. No difference bears on Paper A1 |
| MP-D10 | WP 02-003 (February 2002) differs more. It has a longer introduction (Smith, Veblen, Duesenberry, networks, and Corneo and Gruner (2000), described in a footnote as taking a preference for higher-class mates as given), uses "social arrangements", has no rationed analysis and no counterpart of Proposition 5, writes the law of motion for `ρ < 1/2`, `μ_y > 1/2` as `μ_y − (1/2)μ_n(1 − ρ)`, which counts one of the two `Hn`, `Ly` contributions and is inconsistent with its own formula for `ρ > 1/2`, states its Proposition 5 under a garbled hypothesis "4−3β < 2−β", omits the `(1 − β)` and `β` weights in the first-order conditions of Section 6, gives a different white-shoe example (outputs 3 and 1, wage 1), and has a discussion point that two isolated communities can sustain different matchings | Read in full. In all three versions income and attribute are both known at matching, and 02-003 (p. 33) says matching depends only on current income and the attribute, so no difference bears on Paper A1 |
| MP-D11 | In the steady state `μ_y^A` on p. 24 (printed 23) the factor 2 multiplies the whole brace of the denominator. The text layer loses the brace | Read from the page image. `steady_state_assort` derives the printed formula from the law of motion with purchases |
| MP-D12 | In (6.1) and (6.2) "(1 − q_HL V_n)" is read as `(1 − q_HL) V_n`, and on p. 28 "U'(H + c(q_HL))" as `U'(H + L − c(q_HL))`, which is `U'(H − c(q_HL))` at `L = 0` | (6.3) follows from (6.1) only on the first reading (`eq_6_3`), and the first-order condition on p. 26 has `l − c(q)` |
| MP-D13 | For `μ_y < 1/2` the paper gives no value equations ("A similar calculation"). `lowValues` is our reconstruction, with an `Hn` agent meeting an `Ly` agent with probability `μ_y/μ_n` | `prop5_low` agrees with the paper's statement that (3.8) is the relevant inequality |
