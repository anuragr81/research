# Cole, Mailath and Postlewaite (1991), claims

Evidence tag [F]: the text of every page of CARESS Working Paper 91-14 (PDF
pages 1 to 36) was read on 16 October 2026, with the equations read from page
images. Pages are PDF pages. PDF page `n` carries the printed number `n − 1`
from PDF page 3 on.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CM-Q1 | 4 | "We interpret an agent's status as a ranking device which determines how well he fares with respect to non-market decisions." | Status is a rank that allocates a non-market good, not a belief about a hidden type |
| CM-Q2 | 5 | "Agents will have common preferences over potential matches." | Preferences over mates are vertical and shared. Nothing depends on the chooser's own type, unlike Paper A's squared gap |
| CM-Q3 | 5 | "In our model, people only care about status because in equilibrium it may affect variables that enter the utility function." | Status is valued only through equilibrium matches. The precedent for deriving a social preference rather than assuming it, as Paper A derives group avoidance |
| CM-Q4 | 7 | "We assume that matching is voluntary and takes place based upon complete current information." | No characteristic is hidden and no one infers anything. The paper has none of Paper A's inference from a marker |
| CM-Q5 | 7 | "The role of status in our model will be to rank the men. This ranking determines the outcome of the matching stage." | The same point as CM-Q1, at the definition of a social norm |
| CM-Q6 | 13 | "A man's match in period t depends, then, only on his relative position in the capital distribution of period t." | Wealth-is-status matching is assortative on observed capital |
| CM-Q7 | 14 | "bequests will be larger when a son's match is more sensitive to the level of capital on the margin." | The paper's mechanism, from eq. (10). Not used by Paper A |
| CM-Q8 | 17 | "if a man matches with a woman with an endowment not equal to his status, the family line from that point on has zero status" | The aristocratic norm is enforced by loss of status, eq. (12), formalised through (13) as `ColeEtAl1991.eq13` |
| CM-Q9 | 22 | "groups, [0,i) and [i,1] called respectively the upper group and lower group." | The printed labels, reversed in reading CM-D3 |
| CM-Q10 | 23 | "For men and women in the upper group, matching is to be random" | Within the upper group a match is a random draw from the group, the setting of `ColeEtAl1991.upper_group_mean` |
| CM-Q11 | 23 | "Since the expected value of a match for any male in the upper group is (1-i)/2" | The printed value, read as `(1 + i)/2` (CM-D2, `ColeEtAl1991.control_printed_value_not_mean`) |
| CM-Q12 | 24 | "men with low status compete on the basis of wealth while men with high status compete on the basis of birth." | The hybrid equilibrium. Group membership by birth coexists with ranking by wealth |
| CM-Q13 | 26 | "why is he giving away goods that people care about rather than selling them?" | The authors chose matching to avoid modelling an allocator who could sell access. Paper A's venue that prices access is the case they set aside |
| CM-Q14 | 24 | "as shown in Proposition I, in the wealth-is-status equilibrium for the" | The cross-reference recorded as CM-D4 |
| CM-Q15 | 6 | "Men and women have identical utility functions defined over joint consumption c. (This is done to avoid distributional issues.)" | A couple pools its budget into one joint consumption. A partner's capital is valued by both through the shared budget, which makes preferences vertical rather than homophilous |
| CM-Q16 | 25 | "Those above the average for the upper group, (i+ 1)/2, are receiving lower expected value from their match in the hybrid equilibrium than in the wealth-is-status equilibrium while those below the average are receiving higher expected utility from the matches." | Within a pooled group the members below its mean gain and those above lose (`ColeEtAl1991.hybrid_welfare`, `ColeEtAl1991.top_man_loss`) |
| CM-Q17 | 26 | "one can imagine a one type model in which there are some goods that are allocated according to status rather than by markets" | The alternative the authors set aside, goods such as invitations and pews allocated by status, which is where an intermediary of social access would sit |

## Lean results

`lean/mathlib/ColeEtAl1991.lean`, in the paper's notation. Definitions (not
audited by name) are `u`, `lamStar`, `VoluntaryMatching` and the status rule
`n` of eq. (12).

| Theorem | What it checks |
|---|---|
| `ColeEtAl1991.lambda0_foc` | Eq. (2). `λ(0) = [1 + (β A^{1−γ})^{1/γ}]⁻¹` lies in `(0, 1)` and solves the first-order condition in ratio form |
| `ColeEtAl1991.lambda_j_antitone` | The two-period example. `λ(j)` falls in `j` |
| `ColeEtAl1991.control_lambda_j_flat` | Control. Without the match term the share does not vary |
| `ColeEtAl1991.stationary_ratio` | Eq. (6) at a stationary point gives `(1 − λ*)^γ = A^{1−γ} β` |
| `ColeEtAl1991.lamStar_stationary` | `λ* = 1 − (A^{1−γ} β)^{1/γ}` satisfies that condition |
| `ColeEtAl1991.eq7` | Eq. (7). `k_{t+n} = (1 − λ*)^n A^n k_t` |
| `ColeEtAl1991.discount_identity` | `β ((1 − λ*) A)^{1−γ} = 1 − λ*` |
| `ColeEtAl1991.pow_rpow_swap` | A lemma for eq. (8) |
| `ColeEtAl1991.eq8` | Eq. (8). The discounted utility sums to `(λ* A k_0)^{1−γ}/((1 − γ) λ*)` |
| `ColeEtAl1991.bequest_rises` | Eq. (10). A larger matching term gives a larger bequest |
| `ColeEtAl1991.control_euler_unique` | Control. With the same matching term the solution is unique |
| `ColeEtAl1991.wis_rank` | Wealth-is-status. With capital strictly increasing in the index, the mass of poorer men is the index, so `m(i) = i` |
| `ColeEtAl1991.control_wis_rank_atom` | Control. With all men equally rich the rank is 0 for everyone |
| `ColeEtAl1991.property1_exchange` | Property 1. The exchange inequality for strictly concave `u` |
| `ColeEtAl1991.property1_no_switch` | Property 1. The richer line does not bequeath less |
| `ColeEtAl1991.control_linear_allows_switch` | Control. With linear `u` the optimality conditions do not rule out a switch |
| `ColeEtAl1991.property3_step` | Property 3. The best woman of a uniform pool beats its average |
| `ColeEtAl1991.eq12` | Eq. (12). Status kept after an appropriate match, lost otherwise, zero absorbing |
| `ColeEtAl1991.eq13` | Condition (13). `V*(k) + j ≤ V*(k) + i/(1 − β)` if and only if `(1 − β) j ≤ i` |
| `ColeEtAl1991.control_zero_status_deviates` | Control. (13) fails for a man of status 0 and any woman with `j > 0` |
| `ColeEtAl1991.L_ge_k` | The compensating capital `L_t(j)`. `V*(L) − V*(k) = β j/(1 − β)`, so `L ≥ k` |
| `ColeEtAl1991.control_L_eq_k_at_zero` | Control. At `j = 0` the compensating capital is `k` itself |
| `ColeEtAl1991.L_closed_form` | The closed form `L^{1−γ} = k^{1−γ} + β j/((1 − β) C)` |
| `ColeEtAl1991.C_inv` | `1/C = (1 − γ) (λ*)^γ A^{γ−1}`, the printed coefficient |
| `ColeEtAl1991.upper_group_mean` | The hybrid equilibrium. A random match in `[i, 1]` is worth `(1 + i)/2` |
| `ColeEtAl1991.banishment_loss` | Banishment costs at least `(1 − i)/2` |
| `ColeEtAl1991.control_printed_value_not_mean` | Control. The printed `(1 − i)/2` is not the group mean for `0 < i < 1` |
| `ColeEtAl1991.hybrid_welfare` | PDF p. 25. A member of the upper group gains in matching exactly when below the group mean |
| `ColeEtAl1991.top_man_loss` | The man of index 1 loses `(1 − i)/2` in matching |
| `ColeEtAl1991.match_value_bound` | Section 5.3. The discounted value of one unit a period is `1/(1 − β)` |
| `ColeEtAl1991.spread_blind` | With `j` linear in utility, a random match in a group is worth its mean whatever its spread |
| `ColeEtAl1991.quadratic_spread` | Control on `spread_blind`. Under a quadratic loss the spread adds `s²/3` |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Eq. (2), `λ(0)` in the two-period example | `ColeEtAl1991.lambda0_foc` | special case | The first-order condition in ratio form. Its reduction from (2) and the concavity that makes it a maximum are not formalised |
| `∂λ(j)/∂j < 0` (PDF p. 10) | `ColeEtAl1991.lambda_j_antitone` | special case | A monotone inverse replaces the implicit derivative. That `g` rises below `λ(0)` is a hypothesis |
| Unequal initial wealth and the compactness claim (PDF pp. 10–11) | none | not formalised | A verbal argument with no closed form |
| Eqs. (3) to (6) and `λ*` | `ColeEtAl1991.stationary_ratio`, `ColeEtAl1991.lamStar_stationary` | special case | Stationarity is checked. That the constant ratio solves (3), "a standard exercise", is not |
| Eq. (7) | `ColeEtAl1991.eq7` | general | |
| Eq. (8) | `ColeEtAl1991.eq8` | general | |
| Eqs. (9), (10) and the claim on bequests (PDF p. 14) | `ColeEtAl1991.bequest_rises` | special case | The Euler equation is taken as given and the matching term is held fixed at the solution. The envelope step is not formalised |
| Eq. (11) | none | not formalised | A restatement of (10) in consumption ratios, not used |
| Proposition I and Appendix A.1 | none | not formalised | Mas-Colell's (1984) existence theorem for anonymous games is not in Mathlib |
| Wealth-is-status matching `m(i) = i` (PDF p. 14) | `ColeEtAl1991.wis_rank` | general | For capital strictly increasing in the index |
| Property 1 | `ColeEtAl1991.property1_no_switch` | special case | The one-period exchange argument with a common continuation. The strict order and the induction over periods are not formalised |
| Property 2 | none | not formalised | Needs the whole equilibrium path |
| Property 3 | `ColeEtAl1991.property3_step` | special case | The step for a uniform pool |
| Property 4 | none | not formalised | Needs the whole equilibrium path |
| Property 5 | none | not formalised | A limit in `t` |
| Property 6 | none | not formalised | A limit in `t` |
| Eq. (12) | `ColeEtAl1991.eq12` | general | |
| `V(k, s) = V*(k) + s/(1 − β)` (PDF p. 18) | `ColeEtAl1991.match_value_bound` | special case | The annuity factor is checked. The additive split of `V` is taken as given |
| `L_t(j)` and its closed form (PDF p. 18) | `ColeEtAl1991.L_ge_k`, `ColeEtAl1991.L_closed_form`, `ColeEtAl1991.C_inv` | general | |
| Eq. (13) | `ColeEtAl1991.eq13` | general | |
| Eq. (14) | none | not formalised | Needs the value function off the path |
| Lemma (PDF p. 19) | none | not formalised | A limit as `β → 1` |
| Proposition II and eq. (15) | none | not formalised | Needs the path of `k_t` and the derivative of `L_t` |
| The modified aristocratic rule (PDF p. 22) | none | not formalised | A verbal argument |
| Hybrid match value and loss from banishment (PDF p. 23) | `ColeEtAl1991.upper_group_mean`, `ColeEtAl1991.banishment_loss` | general | |
| Proposition III | none | not formalised | Rests on Proposition I and the Lemma |
| Welfare comparison (PDF p. 25) | `ColeEtAl1991.hybrid_welfare`, `ColeEtAl1991.top_man_loss` | special case | The matching component. The consumption component is verbal |
| Section 5.3, the bound `1/(1 − β)` | `ColeEtAl1991.match_value_bound` | general | |

Counts. General 8, special case 8, not formalised 12.

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| CM-D1 | The version read is CARESS Working Paper 91-14, June 1990, revised June 1991, and it is cited as such, with the key `ColeEtAl1991` | The cover, PDF p. 1. The cache file name says 1992, the year of the journal version, which is not read |
| CM-D2 | The expected value of a match in the upper group is read as `(1 + i)/2`, not the printed `(1 − i)/2` | Women in the upper group are uniform on `[i, 1]`. The same page gives the loss from banishment as `(1 − i)/2` when the best lower-group match is below `i`, and PDF p. 25 calls `(i + 1)/2` the average for the upper group. `control_printed_value_not_mean` |
| CM-D3 | `[i, 1]` is read as the upper group and `[0, i)` as the lower group, the reverse of the printed labels (CM-Q9) | PDF p. 23 says "for any man in [i,1]" in the upper group, and Proposition III puts `j ≥ i` under the aristocratic rule |
| CM-D4 | The claim that the consumption share in the wealth-is-status equilibrium goes to 0 for `γ > 1` is read as Property 6, not Proposition I (CM-Q14) | Proposition I is the existence result. Property 6 is the limit statement |
| CM-D5 | Footnotes 11 and 15 speak of "type I" and "type II" agents, read as sons and women | A vestige of the earlier paper named in the cover footnote. The text otherwise says men and women |
| CM-D6 | PDF page 36 repeats the reference list of PDF page 35 in another style, and a scan mark on PDF page 2 is extracted as the word "may" | Page images. Nothing is attributed to either |
| CM-D7 | The text layer garbles the equations, so every equation is read from page images at 110 dpi | The listed pages in `RECONSTRUCTION.md` |
| CM-D8 | On PDF p. 14, "u' must be smaller" is read as the `u'` on the right of (10), at `k_{t+1}`, so that `k_{t+1}` rises | A larger `m'_{t+1}` raises the right side, so the left `u'` must rise and the right `u'` fall. Either reading gives a larger `k_{t+1}`, which is what `bequest_rises` checks |
