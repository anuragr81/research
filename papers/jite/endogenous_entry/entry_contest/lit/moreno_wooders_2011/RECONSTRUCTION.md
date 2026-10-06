# Reconstruction — Moreno & Wooders (2011)

**Source.** Diego Moreno and John Wooders, "Auctions with heterogeneous entry
costs", *RAND Journal of Economics* 42(2), Summer 2011, pp. 313–336. Published
version, text layer present, 24pp. Based on their 2006 working paper.

This is the **only literature paper `PROOFS.tex` cites for a substantive
modelling contrast** — §Primitives uses it to distinguish P5 from a
private-cost threshold. So the reading has to be right.

---

## 1. The question

With an exogenously fixed number of IPV bidders, the revenue-maximising
auction screens: the reserve price sits above the seller's value and is
independent of `N` (Myerson; Riley–Samuelson). What survives when entry is
costly, and when **entry costs are heterogeneous and privately known**?

Their answer: heterogeneity plus private information changes the design
conclusions substantially — the revenue-maximising reserve price is above the
seller's value, an admission fee does better, and an entry cap plus admission
fee does better still.

## 2. Primitives (§4)

- `N` buyers, IPV. Each buyer `i` has a **privately known entry cost `Z_i`**,
  drawn i.i.d. from `H` on `[c, c̄]`, `0 < c < c̄ ≤ ∞`, `H` increasing with
  `H(c) = 0` and density `h`.
- Regularity to exclude corner cases: `u(0,N) < c̄` and `c < u(0,1)`.
- The seller may set a **screening value** `v` (reserve price) and an
  **admission fee** `φ`, paid in addition to the entry cost.
- Homogeneous costs are the degenerate case of `H` (§3), where the benchmark is
  their **Proposition MM-LS** (McAfee–McMillan 1987; Levin–Smith 1994).

## 3. The entry equilibrium — the part that matters to us

Quoted, p.320:

> "In this setting, an entry strategy for a buyer can be described by a
> **threshold `t ∈ [c, c̄]`** indicating the maximum entry cost for which the
> buyer enters the auction; that is, a buyer enters when her entry cost is less
> than `t`, and does not enter if it is greater than `t`. [...] If all buyers
> employ the same threshold `t`, then **the number of bidders follows a
> binomial distribution `B(N, H(t))`**."

Three structural facts follow, and all three are exactly what `PROOFS.tex`
asserts:

1. The threshold lives in **cost space** — each buyer compares her own `z_i`
   against a number.
2. The threshold is **flat**: in a symmetric equilibrium every buyer uses the
   *same* `t`.
3. The entrant count is **binomial**, hence a genuine random variable.

> **Proposition 2.** For each `v` and `φ` there is a unique symmetric entry
> equilibrium `t*(v,φ) ∈ [c,c̄]`. The mapping `t*` is continuous. When interior,
> `t*(v,φ)` solves `(3) U(v, H(t)) = t + φ`, and is decreasing in both `v` and
> `φ`.

Footnote 8 records that threshold strategies are not assumed: entry rules are
in general mappings `[c,c̄] → [0,1]`, but when `H` is atomless, equilibrium
buyers *follow* a threshold strategy.

## 4. Welfare and revenue

Social surplus given a common threshold, `(4)`: `W(v,t) = S(v,H(t)) − N·c(t)`
with `c(t) = ∫_c^t z dH(z)` the expected entry cost per buyer. `W*` is the
**constrained** maximum — constrained in the sense that buyers enter
independently under a symmetric rule.

> **Proposition 3.** A screening value and an admission fee both equal to zero
> maximize social surplus, that is `W(0, t*(0,0)) = W*`.

> **Proposition 4.** In an interior entry equilibrium, total buyer surplus is
> positive and decreasing in both the screening value and the admission fee,
> and seller revenue is less than the social surplus.

Buyers earn **information rents** — `(6)` gives total buyer surplus as
`N∫_c^{t*}[t* − z]dH(z) > 0` — so the seller does not capture the whole
surplus. `(7)` states revenue as social surplus minus buyer surplus.

The rest (Props 5–10) is design: revenue-maximising screening value
`0 < v* < v^F`, admission fees dominating, entry caps `n̄ = n*(c)` dominating
further, and asymptotic coincidence as `N → ∞`.

## 5. What could not be reconstructed

1. **Proposition 1** and the preliminaries of §2 — the utility representation
   `U(v,p)` and `S(v,p)` on which everything rests.
2. Proposition 2's uniqueness argument.
3. Propositions 3 and 4's proofs beyond the displayed `(6)`–`(7)`.
4. All of Propositions 5–10 (the design results) and §5's example.
5. Proposition MM-LS as they state it for the homogeneous case.

Anything leaning on these is **[A]-grade** despite the [F] tag.

## 6. What this implies for entry_contest

**The `PROOFS.tex` §Primitives contrast is accurate.** It says the alternative
of privately known entry costs "yields a symmetric Bayesian threshold in cost
space and a *binomial* entrant count, as in Moreno–Wooders; there the number of
entrants is random and the identities are not determined." Every element of
that is in the source, quoted above.

**The separation can be stated more sharply than it currently is.** The
survey and `PROOFS.tex` locate the difference in the *information assumption*,
which is right but leaves the mechanism implicit. The mechanical consequence is:

| | Moreno–Wooders | P5 |
|---|---|---|
| threshold lives in | cost space | the wealth *order* |
| threshold shape | **flat** — one number `t` for everyone | **rank-dependent** — `κ(w_(j)) ≤ Δ(j−1)` |
| entrant count | `Binomial(N, H(t))`, variance `NH(1−H) > 0` | deterministic, variance `0` |

The flat-versus-rank-dependent contrast is the crisp version. Under private
costs every buyer faces the same expected utility `U(v,H(t))`, so the
comparison value cannot depend on rank. Under complete information the `j`-th
richest knows exactly how many richer rivals will enter, so her comparison
value is `Δ(j−1)`, which falls with `j` by P3-cor. A rank-dependent rule
collapses to a flat one only if `Δ` is constant, which it never is.

**A welfare finding that bears on the deferred section.** Their Proposition 3
gives a *third* benchmark alongside Levin–Smith's two:

- Levin–Smith Prop 3 — CV, homogeneous costs: free entry **excessive**.
- Levin–Smith Prop 6 — IPV, homogeneous costs: free entry **optimal**.
- Moreno–Wooders Prop 3 — IPV, **heterogeneous private** costs: free entry
  **optimal** (constrained).

So moving from homogeneous to heterogeneous entry costs does **not** flip the
welfare verdict. What flips it is whether `V_n` varies with `n` — LS-7's
criterion. Our model has heterogeneous costs *and* a fixed prize; the
heterogeneity is not what puts it in the excessive-entry branch, the fixed
prize is. This is worth stating explicitly in the welfare section, because
"our costs are heterogeneous, so Levin–Smith may not apply" is the obvious
objection and Moreno–Wooders answers it.
