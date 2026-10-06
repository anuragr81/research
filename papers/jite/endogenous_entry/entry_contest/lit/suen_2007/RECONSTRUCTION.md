# Reconstruction — Suen (2007)

**Source.** Wing Suen, "The comparative statics of differential rents in
two-sided matching markets", *Journal of Economic Inequality* 5 (2007),
pp. 149–158. Received 30 Oct 2005, accepted 11 June 2006. Published version,
text layer present, 10pp. Read in full.

---

## 1. The question

In a two-sided matching market, what happens to the *distribution of rents* on
one side when the distribution of characteristics on the **other** side becomes
more dispersed?

## 2. Primitives

- X-agents indexed by `x`, Y-agents by `y`, both on `[0,1]` (normalised), equal
  measures. Distributions `F` (with density `f`) and `G` (density `g`).
- Match output is **multiplicatively separable**: `Q = θ(x)φ(y)`, with `θ, φ`
  increasing and weakly **concave** (diminishing returns).
- The paper is explicit that multiplicative separability "is restrictive, but
  it allows the derivation of unambiguous comparative statics results."
- Since `∂²Q/∂x∂y > 0`, matching is **positive assortative**.

## 3. The matching function

Assortativity plus equal measures forces `F(x) = G(μ(x))`, i.e.

> `(1)`  `μ(x) = G⁻¹(F(x))`

and the paper observes (p.151): *"the matching function `μ(x) = G⁻¹(F(x))` is
an increasing function with `μ(0) = 0` and `μ(1) = 1`. Indeed, one can regard
the matching function itself as a distribution function."*

This is exactly the class our `T = Λ_β⁻¹ ∘ Λ_α` belongs to.

A Y-agent of type `μ(x)` picking `x` gives the FOC `θ'(x)φ(y) − w'(x) = 0`, so

> `(2)`  `w'(x) = θ'(x) φ(μ(x))`

an ODE for the equilibrium wage schedule. Under PAM the SOC holds, because
`w'' − θ''φ(μ) = θ'·(d/dx)φ(μ(x)) > 0`.

## 4. Proposition 2 and its proof — the technique

`W_i = ∫₀¹ w_i(x) f(x)dx` is total payoff to X-agents when Y's distribution is
`G_i`.

> **Proposition 2** Assume that `θ` and `φ` are both concave, and that `1 − F`
> is log-concave. Then `W₁ < W₀` if `G₁` is more dispersed than `G₀`.

The proof runs in four moves.

**(a) Integrate by parts against the inverse hazard rate.** With
`ρ(x) = (1−F(x))/f(x)`,

> `(4)`  `W₁ − W₀ = ∫₀¹ [w₁'−w₀'](1−F)dx = ∫₀¹ [w₁'−w₀'] f ρ dx = −∫₀¹ H(x)ρ'(x)dx`

where `H(x) = ∫₀ˣ [w₁'(t) − w₀'(t)] f(t)dt`.

**(b) Decompose `H` through the concavity of `θ`.**

> `(5)`  `H(x) = θ'(x)Ĥ(x) − ∫₀ˣ θ''(t)Ĥ(t)dt`,  where
> `Ĥ(x) = ∫₀^{F(x)} [φ(G₁⁻¹(t)) − φ(G₀⁻¹(t))] dt`

**(c) Sign `Ĥ` using the crossings.** Let `0 = k₀ < k₁ < … < k_{2n} = 1` be the
points where `G₁⁻¹` intersects `G₀⁻¹`. By second-order dominance the difference
`G₁⁻¹ − G₀⁻¹` is **negative on `[0,k₁]`, positive on `[k₁,k₂]`, negative on
`[k₂,k₃]`, and so on** — it alternates. So `Ĥ` attains its local maxima at the
even points `k_{2i}`, and at those points `(6)` bounds `Ĥ(k_{2i})` by a sum of
partial integrals, *"where the inequality stems from the concavity of `φ`"*.
Each such integral is negative because

> "if `G₀` second-order stochastically dominates `G₁` and the two distributions
> have the same mean, then `G₁⁻¹` second-order stochastically dominates
> `G₀⁻¹` (see, for example, **[2]**)"

with **[2] = Costrell and Loury**. Hence `Ĥ ≤ 0` at the maxima, so `Ĥ < 0`
almost everywhere; with `θ` concave, `(5)` gives `H < 0` almost everywhere.

**(d) Sign the outer integral.** *"Finally, log-concavity of `1 − F` implies
that `ρ' ≤ 0`."* Then `(4)` gives `W₁ − W₀ < 0`.

**This handles a general Rothschild–Stiglitz spread with an arbitrary finite
number of crossings** — which is precisely what P9-gen's single-pivot
hypothesis currently rules out.

## 5. Two remarks the paper makes that matter to us

**Concavity is sufficient, not necessary** (p.155): *"the concavity assumption
is sufficient but not necessary for the conclusion of Proposition 2. If `θ` and
`φ` are both linear, for example, the proof of Proposition 2 indicates that
`W₁` is still strictly lower than `W₀`."* With `θ'' = 0`, `(5)` collapses to
`H = θ'Ĥ`, and the sign survives.

**Footnote 1 — rank-rescaling destroys concavity.** Discussing exactly the
Costrell–Loury device of re-indexing by rank:

> "In Costrell and Loury [2], the characteristic `y` is redefined by letting
> `ŷ = G(y)`. [...] the production function would then have to be modified to
> `Q = θ(x)φ̂(ŷ)`, where `φ̂(·) = φ(G⁻¹(·))`. **Unless the density function `g`
> is increasing, concavity of `φ` does not imply concavity of `φ̂`.** Since the
> subsequent analysis relies on the curvature properties of the production
> function [...] such re-scaling is not adopted in this paper."

Suen deliberately declines to work in rank space, because curvature does not
survive the change of variable.

## 6. Beyond Proposition 2

- **Proposition 1**: a first-order stochastic increase in `G` raises the wage
  of every X-agent.
- **Proposition 3** and **Corollaries 1–2**: conditions under which *every*
  X-agent loses, not just the aggregate — Corollary 1 needs `f` non-decreasing
  with `θ, φ` concave; Corollary 2 needs `g₀, g₁` symmetric about the median
  and `f` symmetric.

## 7. What could not be reconstructed

1. The derivation of `(4)`'s middle equality and the definition of `H` — read,
   not verified.
2. Inequality `(6)` in detail: the recombination of partial integrals under
   concavity of `φ`. This is the technical core and was **not** checked.
3. The step from "`Ĥ ≤ 0` at the local maxima `k_{2i}`" to "`Ĥ < 0` for all
   `x ≠ k_i`".
4. Propositions 1, 3 and Corollaries 1–2 in full.

Anything leaning on (1)–(4) is **[A]-grade** despite the [F] tag.

## 8. What this implies for entry_contest

**The technique is the right one for open item 2, and Suen owns the
log-concavity condition** — SU-3 shows `1−F` log-concave is *exactly*
`ρ' ≤ 0`, not merely sufficient for it, so the hypothesis is doing precise
work at step (d).

**But footnote 1 is a warning aimed squarely at the proposed route.** The
survey's plan is to "replace the pivot hypothesis with log-concavity of
`1 − Λ` and apply the Suen integration". Suen's own note says that moving to
rank space breaks concavity unless the density is increasing — and the
dispersive-order machinery from Hopkins–Kornienko that motivates our
`T = Λ_β⁻¹∘Λ_α` **is** rank-space machinery. SU-5 exhibits the failure
concretely: `φ = √y` concave, `g` decreasing, and `φ̂'' = +1.12` at `u = 4/5`.
If the analogue of `φ` for us is `κ`, its curvature is not safe to assume after
the change of variable.

**A structural difference to keep.** Suen's `μ` is **cross-side**: the other
market's distribution moves and `μ` maps X-types to Y-partners. Our `T` is
**same-side**: before and after a spread of the *same* population. The survey
states this correctly. The map class is identical; what is being compared is
not.

**And the aggregate/count gap remains.** Suen signs `W`, a smooth integral of
a wage schedule. `k*` is an integer count defined by a threshold. Neither Suen
nor Costrell–Loury sign a count.
