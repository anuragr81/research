# Reconstruction of Fullerton and McAfee, "Auctioning Entry into Tournaments"

**Source read.** Richard L. Fullerton and R. Preston McAfee, "Auctioning Entry
into Tournaments", *Journal of Political Economy* 107(3), 1999, read from
`auction_entry.pdf` (Drive id `1QvwSRLL7LDxhtK_8yIkEo3SyQt4goBTf`, uploaded
7 Oct 2026). The PDF is a scan of the published article with 31 pages,
covering printed pages 573 to 603. Printed pages 604 and 605, which the
citation gives as part of the article, are absent from the upload and were not
read. PDF page `p` is printed page `p + 572`. Printed pages 578 to 603, which
carry every equation, theorem and proof, were read from the rendered page
images. Printed pages 573 to 577 are introduction prose and were read from the
text layer. Locators below are printed page numbers of the published version.

**Order of work.** This file was written from the PDF before `CLAIMS.md`. The
survey's three sentences about the paper (`LITERATURE.tex` §sec:unread and
§sec:entry, `lit/morgan_orzen_sefton_2012/NOTES.md` F6) were known before the
read, so the choice of what to reconstruct in §5 was influenced by them.

## 1. Objects, in the paper's notation and ours

| Paper | Meaning | Locator | Our nearest object |
|---|---|---|---|
| `n` | risk-neutral firms willing to compete | p.578 | `Q` challengers |
| `P` | the prize | p.578 | `V` |
| `c_i`, `H`, `h` | firm `i`'s marginal cost of research, drawn from `H` with density `h` | p.578 | none, since ability is common in our model and wealth enters only the cost of the fee |
| `γ` | fixed cost of entering, avoided when `z_i = 0` | p.578 | the fee `c`, which our model prices in utility as `κ(w)` |
| `z_i` | effort, the number of independent draws from `F` | p.578 | the binary investment |
| `F`, `f` | the law of one innovation draw, support `[0, x̄]` | p.578 | the score laws |
| `M`, `m = |M|` | the set of firms choosing positive effort, and its size | p.579 | the entrant set and `k*` |
| `Π_i` | expected profit without fixed costs, eq. (1) | p.578 | the gain `Δ` net of cost |
| `m̄` | the largest `m` satisfying (5) | p.580 | none |
| `E` | an entry fee set by the sponsor | p.580 | none, since our fee is not chosen by a designer |
| `Z = Σ z_i` | total effort | p.580 | none |
| `Δ_m = m c_m / Σ_{j≤m} c_j` | the highest cost among `m` over the average | p.581 | none, and the symbol clashes with our gain `Δ(m)` |
| `B(c)`, `B(w)` | a symmetric bidding function in the entry auction | p.582, p.585 | none |
| `w`, `Ψ_i(w_i, w_m)` | a general quality attribute, and the expected profit given entry when the marginal entrant has `w_m` | p.584-585 | `w` is wealth in our model, a different object |
| `K` | the interim prize paid to each of the `m` finalists | p.590 | none |

## 2. Primitives, as the paper states them

1. **Timing** (p.578). "first each of the n firms simultaneously chooses
   whether to participate. After this decision, the set of competitors and
   their costs become common knowledge. Then each participating firm
   simultaneously makes its choice of z_i."
2. **Technology** (p.578). Effort `z_i` costs `γ + c_i z_i`, and `γ` is
   avoided at `z_i = 0`. Firm `i`'s best innovation has CDF `F^{z_i}`, "as if
   the firm took z_i identical, independent draws from F(x)", with `z_i` not
   restricted to integers.
3. **Payoff** (p.578, eq. (1)). The sponsor awards `P` to the best innovation,
   so `Π_i = P ∫_0^{x̄} [Π_{j≠i} F^{z_j}(x)] z_i F^{z_i−1}(x) f(x) dx − c_i z_i =
   P z_i / Σ_j z_j − c_i z_i`.
4. **Section III primitives** (p.584-585). A quality attribute `w` drawn from a
   symmetric joint distribution with continuous density, private when bids are
   made and common knowledge after selection. `Ψ_i(w_i, w_m)` is the profit
   given entry, and `h_{m:n−1}(w_m | w_i)` the density of the marginal
   contestant, the `m`th largest of the other `n − 1`.

## 3. Derivation chain

1. **Ratio-form win probability from draws** (p.578, eq. (1)). The
   probability that `i`'s best draw is the overall best is `z_i / Z`. The
   integrand is `z_i F^{Z−1} f`, and the substitution `t = F(x)` gives
   `z_i ∫_0^1 t^{Z−1} dt = z_i / Z`.
2. **Effort subgame** (p.578-579, eqs. (2) to (4), Theorem 1). `Π_i` is
   concave in `z_i`, and the first-order condition is
   `P Σ_{j≠i} z_j / Z² = c_i`. Summing over the active set gives
   `Z = P(m−1)/Σ_{j∈M} c_j`, then `z_i` by (3) and `Π_i = P[1 −
   c_i(m−1)/Σ_{j∈M} c_j]²` by (4). The appendix proof (p.596-597) shows that
   an inactive firm has `c_i ≥ Σ_{j∈M} c_j/(m−1)` and an active one has
   `c_i < Σ_{j∈M} c_j/(m−1)`, so the active set is the lowest-cost prefix,
   characterised by `c_m < Σ_{j≤m} c_j/(m−1) ≤ c_{m+1}`, "which, by
   induction, is unique".
3. **Entry with the fixed cost** (p.579-580, Lemma 1, Theorem 2). An entry
   equilibrium needs entrants' profits at least `γ` and non-entrants' profits
   from entering below `γ`. Lemma 1 bounds how far an excluded firm's cost can
   lie below an entrant's, `c_i ≥ [(m²−m)/(m²−m+1)] c_k`. The proof (p.597)
   splits on whether `i`'s entry would push `k` out of the active set. When it
   would not, `i`'s profit on entry is below `k`'s current profit, which
   rearranges to `c_i m ≥ c_k(m−1)(1 + c_i/Σ_{j∈M} c_j)`, and
   `Σ_{j∈M} c_j ≤ m c_k` finishes it. Theorem 2 (p.580, proof p.597-598)
   says the efficient `m` is unique and entry of the `m` lowest-cost firms is
   an equilibrium, by showing that if firm `k` cannot profitably enter then
   firm `k + 1` cannot.
4. **Designer's cost** (p.580-581, Theorem 3, Lemma 2). The sponsor sets a
   prize `P = Z Σ c_j/(m−1)` to buy total effort `Z` and an entry fee equal to
   the `m`th firm's profit. The total cost is `TC_m = Z Σ_{j≤m} c_j(−1 +
   2Δ_m − ((m−1)/m)Δ_m²) + mγ` (p.598), and the claim is `TC_{m+1} ≥ TC_m`
   when `Δ_{m+1} ≥ Δ_m`, so the optimum is `m = 2`. In the symmetric case the
   cost is `cZ + mγ`.
5. **Uniform-price entry auction with private costs** (p.582-583, eqs. (6),
   (7), Lemma 3). The only symmetric pure-strategy candidate bids the profit
   the bidder would earn as the marginal entrant, eq. (7). It is decreasing in
   cost, hence efficient, when `c h(c)/H(c)` is decreasing, and not when that
   ratio is nondecreasing. Costs uniform on `[0, c̄]` give a constant ratio,
   and "there is no efficient equilibrium" (p.583).
6. **The general failure** (p.585-587, eqs. (8) to (10), Theorem 4). In both
   customary auctions every bidder bids "as though he will wind up being the
   weakest contestant to gain entry" (p.586). If the weakest entrant's profit
   `Ψ(w, w)` fails to rise with `w` on some interval, no symmetric increasing
   pure-strategy equilibrium exists.
7. **Examples and Lemma 4** (p.588-590). Without research `Ψ(w, w) = 0`. With
   research and a common cost `c`, no entrant researches when the best
   entrant's endowment has `H(w_max) ≥ e^{−c/P}` (Lemma 4, proof p.601-602),
   so `Ψ(w, w) = 0` at the top of the support.
8. **Contestant selection auction** (p.590-592, eqs. (11), (12), Theorems 5
   and 6). All bidders pay, and the `m` highest receive an interim prize `K`.
   The bid (12) is strictly increasing in `w` whenever `Ψ(w, w) ≥ 0`, which
   always holds. With independent types it is an equilibrium and efficient
   (Theorem 5), and its expected cost to the sponsor does not depend on `K`
   and equals that of an efficient uniform-price auction (Theorem 6).
9. **Lemma A1** (p.596). Guesnerie and Laffont's (1984) local-to-global
   lemma for incentive compatibility, used for every bidding equilibrium.

## 4. What each result is derived from

| Result | Derived from | Role |
|---|---|---|
| Eq. (1) | the draws technology alone | a contest success function, not an equilibrium |
| Theorem 1, (3), (4) | eq. (1), concavity, the first-order conditions | the effort subgame after entry |
| Lemma 1 | eq. (4) applied before and after a deviation into the active set | a bound on non-assortative entry equilibria |
| Theorem 2 | eq. (4) and a monotonicity step in `k` | existence of the assortative entry equilibrium and uniqueness of its size |
| Theorem 3, Lemma 2 | eqs. (3), (4) with the sponsor choosing `P` and `E` | the sponsor's optimal number of entrants, not an equilibrium count |
| Lemma 3 | eq. (7) differentiated in `c` | a hazard-rate condition for the auction to sort |
| Theorem 4 | eqs. (8) to (10) and Lemma A1 | non-existence of a sorting equilibrium in customary auctions |
| Theorems 5, 6 | eqs. (11), (12), Lemma A1, independence | existence and revenue of the proposed auction |

## 5. What could not be reconstructed

1. The technical appendix "available from the authors on request" (p.595),
   which holds the complete proofs. The appendix in the article gives sketches.
2. The timing for Lemma 1 and Theorem 2. Section II states that costs become
   common knowledge after the entry decision (p.578), while the entry
   equilibria of Lemma 1 and Theorem 2 condition each firm's entry on the
   others' costs. Read literally, those two results assume complete
   information at the entry stage. The paper does not say so.
3. Theorem 5's second-order step with dependent types, which the proof calls
   "ambiguous in sign" and removes by independence (p.603).
4. The order-statistic substitution in the proof of Theorem 6 (p.603), stated
   without the intermediate steps.
5. Footnote 15's claim that the uniform-price auction has no symmetric
   mixed-strategy equilibrium either, which cites Fullerton (1995).
6. Printed pages 604 and 605.
