# Reconstruction — Levin & Smith (1994)

The paper rebuilt from its own primitives, in our notation, **before** checking
what `LITERATURE.tex` says about it. The point of writing this first is that
role-errors — an optimality condition mistaken for an equilibrium condition, a
bound mistaken for a definition — are invisible to arithmetic checks. They are
only visible in the derivation chain.

**Source.** JSTOR scan, AER 84(3), pp. 585–599. **No text layer on pp. 585–599**;
all fifteen content pages were read as rendered images and transcribed by eye.
Every equation number and quotation below carries that caveat.

---

## 1. The question

Most auction theory fixes the number of bidders `n`. Levin and Smith endogenise
it and ask what changes. Their answer has three parts: entry must be modelled
in mixed strategies to preserve symmetry, which makes `n` stochastic; many
fixed-`n` ranking results survive anyway; and market *thickness* — too many
potential bidders — imposes a real coordination cost that sellers may want to
suppress.

## 2. Primitives

- One item, `N` identical risk-neutral potential bidders. Two stages: **stage 1**
  each potential bidder decides whether to sink entry cost `c`; **stage 2** the
  `n` who entered bid under mechanism `m`.
- Assumption 1: seller and bidders risk-neutral.
- Assumption 2: value `V ∈ [0, v̄]`, estimates `x ∈ [0, x̄]`, compact.
- Assumption 3: symmetric information; all bidders draw from the same
  distribution.
- Assumption 4: `m` and `N` common knowledge; **`n` is revealed before stage 2**.
- Assumption 5: a unique symmetric increasing Nash bidding function exists.
  *Assumed, not derived.*
- Assumption 6: `ρ < 0` whenever `R = 0`, where `ρ` is the correlation between
  an entrant's expected profit and the number of rivals she faces.
- Seller's mechanism is `m(R, e)`: reserve prices `R = {R_1,…,R_N}` and an ex
  ante entry fee `e`. "Free entry" means `e = 0`. The seller's own valuation is
  zero throughout.
- `c` is an **ex ante** cost, sunk before learning one's value. This is the
  contrast with Samuelson's interim costs (fn. 7): interim costs screen low
  valuations, ex ante costs do not.

## 3. Objects

| Symbol | Meaning |
|---|---|
| `V_n` | expected value of the item to the highest of `n` bidders |
| `W_n` | expected payment by that bidder, conditional on trade |
| `T_n(R_n)` | probability that trade occurs given `n` and the reserve price |
| `p_n` | binomial probability that exactly `n` of `N` enter |
| `q` | entry probability; `q*` the equilibrium value; `n̄ = qN` |
| `E[π|n,m]` | an entrant's ex ante expected gain, paying `c`, learning `n`, bidding |
| `n*` | the unique integer with `E[π|n*,m] ≥ 0 > E[π|n*+1,m]` |
| `θ = N/n*` | market thickness (§II) |

A bidder's ex ante expected profit, conditional on entering and on trade
occurring as one of `n` bidders, is `(V_n − W_n)/n − (c + e)`.

## 4. The derivation chain

**(a) Entry equilibrium.** Requiring symmetry forces mixing. Indifference gives

> `(1)`  `Σ_{n=1}^{N} C(N−1, n−1) (q*)^{n−1} (1−q*)^{N−n} E[π|n,m] = 0`

so `n ~ Binomial(N, q*)`, with mean `q*N = n̄` and variance `(1−q*)n̄`.
Equivalently `(2)` writes `B_i(q,Ω)` explicitly and `(6)` `B_i(q*,Ω) = 0`
defines `q* = q(Ω)`. **This is the equilibrium entry condition.** Note the
number for later: **(6)**, not (9).

**(b) Accounting.** `(3)` `B(q,Ω) = Nq B_i(q,Ω)`; `(4)` seller revenue
`Π = Σ p_n T_n(R_n) W_n + n̄e`; and

> `(5)`  `S(q,Ω) = B(q,Ω) + Π(q,Ω) = Σ p_n T_n(R_n) V_n − n̄c`

Total welfare is expected value realised minus expected entry costs. `(2)–(5)`
hold in or out of equilibrium.

**(c) The pivot of the whole paper.** In equilibrium `B(q*,Ω) = 0`, so

> `(7)`  `Π(q*,Ω) = S(q*,Ω)`

Induced entry drives bidder profit to zero, so **the seller's expected revenue
*is* total social welfare**. Proposition 1 follows immediately: any
revenue-maximising mechanism induces socially optimal entry. Proposition 2
extends revenue equivalence.

Everything downstream is this identity plus the shape of `V_n`.

**(d) The common-value branch (§I.A).** Wilson's CV model: `V` is independent
of the number of bidders, so **`V_n ≡ V` for every `n`**. Optimal reserve is
zero, `T_n = 1`, and `(5)` collapses to

> `(8)`  `S(q,e) = [1 − (1−q)^N] V − qNc`

Differentiating, `∂S/∂q = N[(1−q)^{N−1}V − c]`, with `∂²S/∂q² < 0`. Setting it
to zero:

> `(9)`  `(1−q*)^{N−1} V = c`

**`(9)` is the first-order condition of the welfare problem `(8)`.** It is the
socially optimal entry probability, which the seller's optimal fee `e*` is
chosen to induce. It is *not* an entry-equilibrium condition — that is `(6)`.
The paper's own notation confirms this: the proof of Proposition 8 (p.595)
writes it as `(1−q^s_N)^{N−1} = c/V`, with `s` for social.

> **PROPOSITION 3:** In CV auctions the seller should discourage entry by
> charging a positive entry fee but no reservation price. Without the entry
> fee, entry would be excessive from social and private points of view.

with `(11)` `e* = Σ_{n=2}^{N} p_n (V − W_n)/n̄ > 0`. The business-stealing
reading is the paper's own (p.590), citing Mankiw & Whinston (1986).

**Why entry is excessive here, exactly.** Because `V_n ≡ V`, the social gain
from the marginal entrant is `V_n − V_{n−1} = 0`, while the social cost is `c`.
The paper states this flatly on p.596: *"In CV auctions, social gains are zero
(and therefore smaller than social costs) for all `n ≥ 2`."* The **only** social
value of entry in the CV case is that trade happens at all — visible in `(8)`,
where `q` enters solely through `1 − (1−q)^N`, the probability of at least one
entrant. Every entrant beyond the first is pure business stealing.

**(e) The private-values branch (§I.B).** Now `V_n` grows with `n`. `(15)`,
`(16)` restate welfare and its derivative; at `e = 0`, `(17)` gives
`qNc = Σ p_n (V_n − W_n)`. The key identity, from fn. 16 via
`W_n = nV_{n−1} − (n−1)V_n`:

> `(18)`  `V_n − W_n = n(V_n − V_{n−1})`

Hence (p.593) the social gain from the `n`-th entrant, `V_n − V_{n−1} − c`,
equals the private gain `(V_n − W_n)/n − c`. They always coincide.

> **PROPOSITION 6:** Optimal entry, for society and the seller, occurs in IPV
> auctions when the seller charges no entry fee or reservation price.

with a Corollary that reserve prices are never desirable in IPV even when entry
fees are unavailable. Proposition 7 extends to affiliated private values:
free entry optimal under second-price, excessive under first-price.

**(f) Market thickness (§II).** With `N > n*`, `q* < 1`, so realised `n` ranges
over `0…N` and unfavourable realisations carry weight.

> **PROPOSITION 8:** As `N` increases beyond `n*` in CV auctions, the
> probability of no entry also increases if the seller is using an optimal
> mechanism.
>
> **PROPOSITION 9:** The level of social welfare generated by optimal auctions
> decreases monotonically as `N` increases beyond `n*`.
>
> **COROLLARY:** The expected revenue of any seller who uses his optimal
> mechanism increases monotonically as the number of potential bidders
> decreases toward `n*`.

`θ = N/n*` is market thickness. Prop 8's argument: from `(9)`,
`(1−q^s_N)^{N−1} = c/V`, so `q^s_N` falls with `N` and
`(1−q^s_N)^N = (1−q^s_N)(c/V)` rises.

Footnote 24 adds a caution the body does not: in IPV the ability to charge
entry fees is almost inconsequential, since fees are not part of the optimal
mechanism when `N > n*`; a seller who cannot charge them might prefer `n*+1`
potential bidders to `n*`, but never more.

## 5. What could not be reconstructed

Stated so that nothing downstream leans on it unknowingly.

1. **Lemma 1** (Appendix A, `(A1)`–`(A5)`). The differential of `(6)` and the
   covariance term `Cov_m`. Read, not verified. Every comparative static on
   `q*` — and Proposition 5 — rests on it.
2. **Appendix B**, `(B1)`–`(B5)`, the sign of `∂T_n(R_n)V_n/∂R_n`. Underpins
   Propositions 1 and 4.
3. **Assumption 5** — existence and uniqueness of the symmetric increasing
   bidding function. Assumed by the authors, so unavailable to us as a result.
4. **Propositions 2, 4, 5, 7** and the affiliated-values machinery.
5. The claim that `E[π|n,m]` is decreasing in `n`, which is what makes `n*`
   well defined. Stated as a conditional ("If `E[π|n,m]` is decreasing in `n`"),
   not proved.

Anything cited from (1)–(5) is effectively **[A]-grade** regardless of the
paper's overall [F] tag.

## 6. What this implies for entry_contest

The reconstruction sharpens the survey's inference (`LS-I`) that a
fixed-prize contest takes Proposition 3 rather than Proposition 6 as its
benchmark. The survey argues this by analogy — "common-value-like in the
relevant sense". The derivation gives a **criterion** instead:

> The branch is selected by whether `V_n` varies with `n`. Proposition 6
> applies exactly when `(18)` holds, which requires the marginal entrant's
> private gain to equal the increment to total value. With a prize fixed at
> `V`, `V_n ≡ V`, so `V_n − V_{n−1} = 0` and the social gain from any entrant
> beyond the first is zero while the cost is `c > 0`.

So the entry_contest model is in the CV branch not by resemblance but by
construction: `V` is exogenous and fixed, which is precisely `V_n ≡ V`. The
expected verdict — excessive entry — follows, and the welfare section can cite
p.596 directly rather than reasoning by analogy.

This also isolates the one thing that could overturn it: if the prize grew with
participation, `V_n` would rise in `n` and the IPV branch could reapply.
`PROOFS.tex` fixes `V` exogenously in §Primitives, so it does not.
