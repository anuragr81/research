# Austen-Smith and Fryer (2003), claims

Evidence tag [F]. NBER Working Paper 9904, August 2003, read in full from its
text layer on 16 October 2026, all 39 PDF pages, with page images read where
the extraction garbles an equation (PDF pages 11, 14, 26, 28, 35 and 36).
The published version (*Quarterly Journal of Economics* 120(2), 2005, "An
Economic Analysis of 'Acting White'") is not read (AS-D4). Pages are PDF
pages. From PDF page 3 on, the printed page is the PDF page minus 1.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| AS-Q1 | 5 | "individuals face a tension between signaling their type to the outside labor market and signaling their type to a peer group: signals that induce high wages can be signals that induce peer rejection." | One signal read by two audiences |
| AS-Q2 | 5 | "firms are assumed to have no interest in any employee's group membership, and groups are assumed not to have any basic preference over whether a potential member is employed or wealthy" | Group membership is not a marker that either audience reads. The group screens on reliability, not on income |
| AS-Q3 | 5 | "is concerned only to accept those individuals who will be reliable group members" | The unobserved type the group screens on |
| AS-Q4 | 8 | "An individual's abilities (types) are private information to the individual" | The type is unseen, as class is in Paper A1 |
| AS-Q5 | 12 | "describes the firm's (and group's) conditional beliefs regarding the individual's type" | Eq. (8). Both audiences read the type from education by one posterior, and the wage is the conditional mean of output |
| AS-Q6 | 15 | "the belief the group must hold regarding the individual's type, [...] must be no smaller than the quantity" | Eq. (11). The group accepts on the posterior mass of reliable types, formalised as `AustenSmithFryer2003.lemma2_accept_iff` |
| AS-Q7 | 11 | "without the assumption all individuals are always accepted into the group." | Assumption (7) as the non-triviality condition, `AustenSmithFryer2003.F0_mem` and `AustenSmithFryer2003.control_no_assumption7_all_accepted` |
| AS-Q8 | 16 | "then there is no residual incomplete information and only those types [...] would be accepted by the group." | When the type is revealed, the group accepts exactly the reliable types, `AustenSmithFryer2003.prop3_3_core` and `AustenSmithFryer2003.control_separation_rejects` |
| AS-Q9 | 17 | "if types rejected by the group adopt a separating strategy, then necessarily the group accepts some types in equilibrium which they would reject under complete information." | Proposition 3(3). Pooling dilutes the belief, `AustenSmithFryer2003.pool_admits_unreliable` |
| AS-Q10 | 3 | "Promising black students are ridiculed for speaking standard English, showing an interest in ballet or theater, having white friends, or joining activities other than sports" | The epigraph the authors quote from *Time* (1992). Friendship across the group line is among the sanctioned behaviours |
| AS-Q11 | 21 | "we expect to find not only increasing variation in education and wage levels within the group as δ goes up, but also an increase in the type variation of accepted group members." | The spread of the accepted pool is an outcome of patience, not a penalty in the group's choice |
| AS-Q12 | 24 | "some justification for the Phelps (1972) assumption that the ability of minority workers is oftentimes perceived more noisily than" | The authors read pooling as a source of a noisier perceived ability for the minority |
| AS-Q13 | 25 | "Suppose that minority individuals are subject to accusations of 'acting white' at school from which majority individuals are free." | Race enters only here, as which individuals face the peer audience, in the conclusion and outside the model |
| AS-Q14 | 25 | "whereas the latter can do so through separation, to avoid peer rejection the former can only do so through pooling with other minority types." | The same education level carries a different signal by group, attributed to Milgrom and Oster (1987) |
| AS-Q15 | 5 | "the costs of membership are in terms of personal time and eﬀort, not money per se." | Membership is costly in effort, not in money. The source's foundation for who is accepted is reliability under repeated interaction |

## Lean results

`lean/mathlib/AustenSmithFryer2003.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `AustenSmithFryer2003.eq14` | Eq. (14) from (12) and (13) |
| `AustenSmithFryer2003.lemma1_exists_unique` | Lemma 1 (printed "Lemma 10", AS-D1). A unique critical type `θ̂ ∈ (0, 1/k)` solving (15) |
| `AustenSmithFryer2003.lemma1_threshold` | Lemma 1. Strategy [P2] is the best response, contribute at or below `θ̂`, refuse above |
| `AustenSmithFryer2003.lemma1_mono` | Lemma 1. `θ̂(δ)` is strictly increasing in `δ` |
| `AustenSmithFryer2003.lemma1_limit` | Lemma 1. `θ̂(δ)` falls below any `ε > 0` for `δ` small enough |
| `AustenSmithFryer2003.lemma2_accept_iff` | Lemma 2 (printed "Lemma 11"). Accepting has nonnegative value if and only if `F(θ̂|σ(θ)) ≥ F₀` |
| `AustenSmithFryer2003.F0_mem` | Under assumption (7), `0 < F₀ < 1` |
| `AustenSmithFryer2003.F0_anti_pi` | Eq. (11). `F₀` is decreasing in `π` |
| `AustenSmithFryer2003.F0_anti_gamma` | Eq. (11). `F₀` is decreasing in `γ` |
| `AustenSmithFryer2003.F0_anti_b` | Eq. (11). `F₀` is decreasing in `b` |
| `AustenSmithFryer2003.F0_mono_Bk` | Eq. (11). `F₀` is increasing in `Bk` |
| `AustenSmithFryer2003.prop1` | Proposition 1, assembled from Lemmas 1 and 2 |
| `AustenSmithFryer2003.lemma3` | Lemma 3 (printed "Lemma 12"). Education is weakly increasing in type among types with the same group decision |
| `AustenSmithFryer2003.lemma4_step` | Lemma 4 (printed "Lemma 13"), the closing comparison of (18) and (19) |
| `AustenSmithFryer2003.remark1` | Remark 1. The complete-information education of a rejected type exceeds that of an accepted type |
| `AustenSmithFryer2003.prop3_3_core` | Proposition 3(3), the core. A type revealed to lie at or below `θ̂` is strictly accepted |
| `AustenSmithFryer2003.pool_accept_iff` | Under a uniform prior, a pool `[0, θ*]` with `θ* ≥ θ̂` is accepted if and only if `θ* ≤ θ̂/F₀` |
| `AustenSmithFryer2003.pool_admits_unreliable` | With `0 < F₀ < 1`, the pool `[0, θ̂/F₀]` reaches above `θ̂` and is accepted |
| `AustenSmithFryer2003.corollary1_uniform` | Corollary 1 under a uniform prior. Accepted types above `θ̂` refuse the high contribution when first asked |
| `AustenSmithFryer2003.prop8_honest_iff` | Proposition 8. Honest statements of intent are a best response if and only if `δ ≥ 1/(1 + π)` |
| `AustenSmithFryer2003.prop8_tolerant_accepts` | Proposition 8. The tolerant group's value of accepting is nonnegative at every belief |
| `AustenSmithFryer2003.prop9_derived` | Proposition 9 from the paper's payoffs. The threshold is `((1−γ)Bk + F(θ^T) b)/((1−γ)Bk + b)` |
| `AustenSmithFryer2003.prop9_printed_at_gamma_zero` | The printed threshold of Proposition 9 is the derived one at `γ = 0` |
| `AustenSmithFryer2003.prop9_printed_differs` | At `γ = 1/2` the derived threshold is `1/3` and the printed `1/2`, with a belief of `2/5` between them |
| `AustenSmithFryer2003.control_lemma1_needs_7` | Control for Lemma 1. Without `π v(1|1) < v(1|0)` the left side of (15) can be positive at `θ = 1/k` |
| `AustenSmithFryer2003.control_no_assumption7_all_accepted` | Control for Lemma 2. Without (7) accepting has nonnegative value at every belief |
| `AustenSmithFryer2003.control_lemma3_needs_single_crossing` | Control for Lemma 3. Without single crossing a lower type may choose more education |
| `AustenSmithFryer2003.control_remark1` | Control for Remark 1. Without the strict gap in (3) the two choices can coincide |
| `AustenSmithFryer2003.control_separation_rejects` | Control for Proposition 3(3) and Corollary 1. Without pooling a type above `θ̂` gives `F = 0 < F₀` and is rejected |
| `AustenSmithFryer2003.control_prop8_needs_3` | Control for Proposition 8. Without `v(1|1) > v(1|0)` honesty holds below `1/(1 + π)` as well |
| `AustenSmithFryer2003.control_prop9_needs_pi_lt_one` | Control for Proposition 9. At `π = 1` the two group values coincide at every belief |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Assumption (7), the non-triviality claim (PDF p. 11) | `AustenSmithFryer2003.F0_mem`, `AustenSmithFryer2003.control_no_assumption7_all_accepted` | general | |
| Lemma 1 (printed 10), with eqs. (12) to (15) | `AustenSmithFryer2003.eq14`, `AustenSmithFryer2003.lemma1_exists_unique`, `AustenSmithFryer2003.lemma1_threshold`, `AustenSmithFryer2003.lemma1_mono`, `AustenSmithFryer2003.lemma1_limit` | general | The leisure value `v(1 − θk|1)` enters as a continuous, strictly decreasing function of the type, which (3) and the smoothness of `v` give. The limit is stated as an explicit bound on `δ` |
| Lemma 2 (printed 11) | `AustenSmithFryer2003.lemma2_accept_iff` | general | |
| Eq. (11), comparative statics (PDF p. 15) | `AustenSmithFryer2003.F0_anti_pi`, `AustenSmithFryer2003.F0_anti_gamma`, `AustenSmithFryer2003.F0_anti_b`, `AustenSmithFryer2003.F0_mono_Bk` | general | Monotonicity in `γ` and `b` uses (7), as the source's claim does |
| Proposition 1 | `AustenSmithFryer2003.prop1` | general | |
| Lemma 3 (printed 12) | `AustenSmithFryer2003.lemma3` | general | Single crossing enters as strictly decreasing differences of `c`, which `c_sθ < 0` gives |
| Lemma 4 (printed 13) | `AustenSmithFryer2003.lemma4_step` | special case | The closing comparison of (18) and (19). The differential equation (17) for a separating strategy is not derived |
| Remark 1 | `AustenSmithFryer2003.remark1` | special case | The comparison of the two first-order conditions, given that (3) orders them. The derivation of that order from (3) is not formalised |
| Proposition 2 | | not formalised | The proof argues over separating strategies as functions with the concavity of `U` in `s`. Mathlib has no signalling-game framework, and a faithful statement needs one |
| Proposition 3 | `AustenSmithFryer2003.prop3_3_core`, `AustenSmithFryer2003.pool_admits_unreliable` | special case | The core of (3), and the dilution of a pool under a uniform prior. Claims (1) and (2), on the convexity of the accepted and rejected sets and constancy near their boundary, are not formalised |
| Proposition 4 | | not formalised | Rests on the D1 refinement through Cho and Sobel (1990) |
| Proposition 5 | | not formalised | Rests on D1 and on the envelope argument for `Δ(θ̂(δ))` |
| Proposition 6 | | not formalised | Proved by the argument for Proposition 5 |
| Proposition 7 | | not formalised | Rests on Proposition 3 and on D1 |
| Corollary 1 | `AustenSmithFryer2003.corollary1_uniform` | special case | Under a uniform prior and a pool with lower end 0. The general statement follows from Propositions 3 and 4 |
| Proposition 8 | `AustenSmithFryer2003.prop8_honest_iff`, `AustenSmithFryer2003.prop8_tolerant_accepts` | special case | The condition `δ ≥ 1/(1 + π)` and the tolerant group's acceptance of all. The step to `θ^T ≤ θ̂(δ)` and the separating tolerant equilibrium, by the Spence existence argument, are not formalised |
| Proposition 9 | `AustenSmithFryer2003.prop9_derived` | general | Derived from the paper's payoffs. The printed threshold differs for `γ > 0` (AS-D2) |

Counts. General 7, special case 5, not formalised 5.

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| AS-D1 | The appendix lemmas are printed "Lemma 10" to "Lemma 13" (PDF pp. 26–29) while the text calls them Lemmas 1 to 4. They are read as Lemmas 1 to 4 | Page image of PDF p. 26. The text cites "Lemma 2" for the acceptance condition printed as Lemma 11 |
| AS-D2 | Proposition 9 (PDF p. 23) prints the threshold `(Bk + F(θ^T) b)/(Bk + b)`. The difference of the two payoffs in its proof (PDF p. 37) gives `((1−γ)Bk + F(θ^T) b)/((1−γ)Bk + b)`. The two coincide only at `γ = 0`. The derived form is adopted, in line with eq. (11), which carries the same `(1−γ)Bk` | `prop9_derived`, `prop9_printed_at_gamma_zero`, `prop9_printed_differs` |
| AS-D3 | The proof of Proposition 8 (PDF p. 36) writes `v(j|0)` and `v(j|1)`. They are read as `v(1|0)` and `v(1|1)`, leisure 1 in a period with no contribution | With that reading the printed condition `δ ≥ 1/(1 + π)` follows (`prop8_honest_iff`). The proof of Proposition 7 also prints `θ̂(δ; 1₀)` for `θ̂(δ)` |
| AS-D4 | The version read is the August 2003 working paper, titled "The Economics of 'Acting White'". The cache file is named `austen_smith_fryer_2005` after the published version, which has a different title and is not read. The record is keyed to the version read | Cover page, PDF p. 1 |
| AS-D5 | Eq. (10) and the proof of Lemma 2 (PDF pp. 27–28) discount the group's payoff to `t = 0`, giving `γb/(1−γ)`. The proof of Proposition 9 (PDF p. 37) uses `b/(1−γ)`, "discounted one period" | The common factor `γ` does not change either comparison |
