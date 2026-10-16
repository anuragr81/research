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
| AS-Q8 | 15 | "Proposition 2 There exist no fully separating peer pressure equilibria." | Pooling is forced, not assumed |
| AS-Q9 | 17 | "if types rejected by the group adopt a separating strategy, then necessarily the group accepts some types in equilibrium which they would reject under complete information." | Proposition 3(3). Pooling dilutes the belief, `AustenSmithFryer2003.pool_admits_unreliable` and `AustenSmithFryer2003.control_separation_rejects` |
| AS-Q10 | 18 | "pool on a common educational investment level and are accepted by the group, and all types greater than θ∗ separate and are rejected by the group" | Proposition 4, the binary partition of D1 equilibria |
| AS-Q11 | 21 | "we expect to find not only increasing variation in education and wage levels within the group as δ goes up, but also an increase in the type variation of accepted group members." | The spread of the accepted pool is an outcome of patience, not a penalty in the group's choice |
| AS-Q12 | 24 | "some justification for the Phelps (1972) assumption that the ability of minority workers is oftentimes perceived more noisily than" | The authors read pooling as a source of a noisier perceived ability for the minority |
| AS-Q13 | 25 | "Suppose that minority individuals are subject to accusations of 'acting white' at school from which majority individuals are free." | Race enters only here, as which individuals face the peer audience, in the conclusion and outside the model |
| AS-Q14 | 25 | "whereas the latter can do so through separation, to avoid peer rejection the former can only do so through pooling with other minority types." | The same education level carries a different signal by group, attributed to Milgrom and Oster (1987) |
| AS-Q15 | 23 | "Then the group prefers peer pressure to tolerance if and only if" | Proposition 9, whose printed threshold is the `γ = 0` case of the one derived from the paper's payoffs (AS-D2) |

## Lean results

`lean/mathlib/AustenSmithFryer2003.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `AustenSmithFryer2003.lemma2_accept_iff` | Lemma 2 (printed "Lemma 11", AS-D1) and the group's half of Proposition 1. Accepting has nonnegative value if and only if `F(θ̂|σ(θ)) ≥ F₀` |
| `AustenSmithFryer2003.F0_mem` | Under assumption (7), `0 < F₀ < 1` |
| `AustenSmithFryer2003.pool_accept_iff` | Under a uniform prior, a pool `[0, θ*]` with `θ* ≥ θ̂` is accepted if and only if `θ* ≤ θ̂/F₀` |
| `AustenSmithFryer2003.pool_admits_unreliable` | With `0 < F₀ < 1`, the pool `[0, θ̂/F₀]` reaches above `θ̂` and is accepted |
| `AustenSmithFryer2003.prop8_honest_iff` | Proposition 8. Honest statements of intent are a best response if and only if `δ ≥ 1/(1 + π)` |
| `AustenSmithFryer2003.prop9_derived` | Proposition 9 from the paper's payoffs. The threshold is `((1−γ)Bk + F(θ^T) b)/((1−γ)Bk + b)` |
| `AustenSmithFryer2003.prop9_printed_at_gamma_zero` | The printed threshold of Proposition 9 is the derived one at `γ = 0` |
| `AustenSmithFryer2003.prop9_printed_differs` | At `γ = 1/2` the derived threshold is `1/3` and the printed `1/2`, with a belief of `2/5` between them |
| `AustenSmithFryer2003.control_no_assumption7_all_accepted` | Control. Without assumption (7) accepting has nonnegative value at every belief |
| `AustenSmithFryer2003.control_separation_rejects` | Control. Under separation a type above `θ̂` gives `F = 0 < F₀` and is rejected |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| AS-D1 | The appendix lemmas are printed "Lemma 10" to "Lemma 13" (PDF pp. 26–29) while the text calls them Lemmas 1 to 4. They are read as Lemmas 1 to 4 | Page image of PDF p. 26. The text cites "Lemma 2" for the acceptance condition printed as Lemma 11 |
| AS-D2 | Proposition 9 (PDF p. 23) prints the threshold `(Bk + F(θ^T) b)/(Bk + b)`. The difference of the two payoffs in its proof (PDF p. 37) gives `((1−γ)Bk + F(θ^T) b)/((1−γ)Bk + b)`. The two coincide only at `γ = 0`. The derived form is adopted, in line with eq. (11), which carries the same `(1−γ)Bk` | `prop9_derived`, `prop9_printed_at_gamma_zero`, `prop9_printed_differs` |
| AS-D3 | The proof of Proposition 8 (PDF p. 36) writes `v(j|0)` and `v(j|1)`. They are read as `v(1|0)` and `v(1|1)`, leisure 1 in a period with no contribution | With that reading the printed condition `δ ≥ 1/(1 + π)` follows (`prop8_honest_iff`). The proof of Proposition 7 also prints `θ̂(δ; 1₀)` for `θ̂(δ)` |
| AS-D4 | The version read is the August 2003 working paper, titled "The Economics of 'Acting White'". The cache file is named `austen_smith_fryer_2005` after the published version, which has a different title and is not read. The record is keyed to the version read | Cover page, PDF p. 1 |
| AS-D5 | Eq. (10) and the proof of Lemma 2 (PDF pp. 27–28) discount the group's payoff to `t = 0`, giving `γb/(1−γ)`. The proof of Proposition 9 (PDF p. 37) uses `b/(1−γ)`, "discounted one period" | The common factor `γ` does not change either comparison |
