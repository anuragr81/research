# Reconstruction — Costrell & Loury

**Source.** "Distribution of Ability and Earnings in a Hierarchical Job
Assignment Model", **draft dated 11 December 2003**, 37pp, text layer present.
The published article is *JPE* 112(6), 2004. Proposition numbers below are the
draft's and **may not survive to publication** — the survey already lists this
as an outstanding check.

**Partial-contamination disclosure.** Unlike the Levin–Smith reconstruction,
this one was not written blind: I had previously seen the survey's section
heading ("Costrell–Loury and the single-pivot precedent") and the Verdict's
one-sentence summary. I had not read the survey's §sec:assignment body until
after working through §2, Propositions 5, 6, 10 and the Appendix proof.

---

## 1. The question

How does the distribution of *ability* map onto the distribution of *earnings*?
Workers are assigned to jobs that differ in ability-sensitivity, in **fixed
proportions**. The comparative statics of interest are: what does a more
unequal ability distribution do to output, and to wage inequality?

## 2. The two-job model (§2) — where the pivot lives

Two jobs filled in fixed proportions `θ` and `1−θ`. Output per unit of ability
is `β₁ > β₀ > 0`; job 1 is the ability-sensitive one. Ability `μ ∈ [0,1]` with
CDF `F`; `μ(p) ≡ F⁻¹(p)`. Workers sort by ability, so assignment is
`μ ≷ μ̂ ≡ μ(θ) = F⁻¹(θ)`.

Output: `Q = β₀ ∫₀^θ μ(p)dp + β₁ ∫_θ^1 μ(p)dp`.

Two conditions pin the wage schedule — a **no-arbitrage** condition giving its
slope on each job (`W(μ) − W(μ′) = βᵢ(μ − μ′)`) and a **no-profit** condition
`Q = ∫₀^1 W(μ)dF(μ)` giving its level:

> `(1)`  `W(μ) = β₀μ + (1−θ)(β₁−β₀)μ̂`,  `μ ≤ μ̂`
> `(2)`  `W(μ) = β₁μ − θ(β₁−β₀)μ̂`,  `μ ≥ μ̂`

**The key structural fact (p.9):** *"the role of the ability distribution in the
wage function is entirely captured by a single sufficient statistic,
`μ̂ = F⁻¹(θ)`."* Any shift raising `μ̂` raises low-ability wages and lowers
high-ability wages — visible directly in (1)–(2), where `μ̂` enters with
coefficient `+(1−θ)(β₁−β₀) > 0` below the margin and `−θ(β₁−β₀) < 0` above.

The marginal-productivity reading: an extra worker of ability `μ < μ̂` adds
`β₀μ` directly *and* frees `(1−θ)` workers for promotion from the margin, each
gaining `(β₁−β₀)μ̂`. Low-ability workers earn **more** than their direct
contribution; high-ability workers earn less.

**The pivot rule itself (p.9), quoted:**

> "Thus, the effect of a more unequal ability distribution depends on whether
> the worker on the margin between jobs, at quantile `θ`, lies in the upper or
> lower tail of the ability distribution. [...] If proper job assignment is
> most critical in the top jobs (i.e. if most jobs are not skill-sensitive —
> `θ` is high), then the worker on the margin is in the right tail, so a more
> unequal ability distribution raises `μ̂` and narrows the wage distribution.
> Conversely, if assignment matters most toward the bottom of the spectrum
> (`θ` is low) then a more unequal distribution reduces `μ̂` and widens the
> wage distribution."

Note the MPS is described as improving ability in the right tail and reducing
it in the left, *"with possible multiple crossings in between"* — the tails do
the work, not a unique crossing.

A related result: adding a worker at any `μ′` **pivots** the CDF at `μ′`
(their Figure 3). Workers bracketing `μ̂` are complements; workers on the same
side are substitutes.

## 3. The continuum model (§3–§4)

`β(·)` becomes a non-decreasing sensitivity function over a continuum of jobs;
the two-job case is the special case where `β` is a **step function** (fn. 7).

- **Proposition 4**: a first-order stochastic improvement raises output and
  improves the earnings distribution.
- **Proposition 5**: `G` riskier than `F`, same mean ⟹ `Q(G) ≥ Q(F)`. A
  mean-preserving rise in riskiness **raises output**.
- **Proposition 6** (main fixed-hierarchy inequality result): with `G MUT F`
  (`∫₀^1[G−F] = 0` and `∫₀^μ[G−F] ≥ 0 ∀μ`), on `[0,1]` atomless full support:
  (i) `β` **concave** ⟹ `w_G(0) ≤ w_F(0)` and `Δw_G ≥ Δw_F` (span widens);
  (ii) `β` **convex** ⟹ `w_G(1) ≤ w_F(1)` and `Δw_G ≤ Δw_F` (span narrows).
  Proved from **Lemma 1**: `G MUT F` and `ψ` convex [concave] imply
  `∫ψ dG⁻¹ ≤ [≥] ∫ψ dF⁻¹`.
- **Theorem 1**: a Blackwell-more-informative test induces a riskier
  distribution of expected abilities — so every MPS result has an information
  reading.

## 4. Proposition 5's proof — the technique the survey wants to transplant

From the Appendix, transcribed:

> The hypothesis implies `∀μ ≥ 0, ∫₀^μ[G−F]dz ≥ 0` and `∫₀^1[G−F]dz = 0`. From
> this, `∫_μ^1[G−F]dz ≤ 0 ∀μ`. Integrate by parts to see that at every crossing
> `μ₀` where `G(μ₀) = F(μ₀) ≡ ρ`,
> `Γ(ρ) ≡ ∫_ρ^1[G⁻¹(x) − F⁻¹(x)]dx = −∫_{μ₀}^1[G(z)−F(z)]dz ≥ 0`.
> Then `Γ(ρ) ≥ 0` whenever `dΓ/dρ = 0`. Since `Γ(0) = Γ(1) = 0`, it must be
> that `Γ(ρ) ≥ 0 ∀ρ ∈ [0,1]`. Integrate by parts, **and recall that `β(·)` is
> non-decreasing**, to get
> `Q(G) − Q(F) = ∫₀^1 {∫_p^1[G⁻¹−F⁻¹]dx} dβ(p) = ∫₀^1 Γ(p)dβ(p) ≥ 0.`

Three steps, and it is worth separating them:

1. **Quantile-difference decomposition.** Define `Γ(ρ)`, the integrated
   quantile gap above rank `ρ`. Second-order dominance signs it **at the
   crossing points**.
2. **Extension to all `ρ`.** `Γ ≥ 0` at every critical point plus
   `Γ(0) = Γ(1) = 0` gives `Γ ≥ 0` everywhere. This is a real-analysis step
   (it needs the extreme-value argument implicitly).
3. **Integration against the weight.** `Q(G) − Q(F) = ∫ Γ dβ ≥ 0` because
   `Γ ≥ 0` and **`β` is non-decreasing**.

**The condition in step 3 is monotonicity of `β`, not concavity, and there is
no log-concavity or hazard-rate condition anywhere.** Searching the full text
for "log-concav" and "hazard" returns **zero occurrences**. Concavity of `β`
enters only in Proposition 6, a different result about wage *inequality*, not
in Proposition 5's output result.

Step 3 has exactly the shape of our own S8: a non-negative integrand against a
non-decreasing integrator (`∫φ² dK ≥ 0` because `K` is a product of CDFs).

## 5. Variable proportions (§5) — a caution

- **Proposition 10** (Cobb–Douglas): any change raising output raises `w(0)`,
  reduces `w(1)`, and narrows the span — **independent of the shape of `β`**.
  Since an MPS raises output (Prop 5), an MPS narrows the span.
- The paper flags the reversal explicitly: under fixed proportions with `β`
  concave (Prop 6), an MPS *reduces* `w(0)` and *widens* the span; *"the
  reoptimizing assignment reverses these results."*

So the direction of the inequality→wage-span effect **is not robust to whether
assignment reoptimises**. That is a caution for any analogue result, ours
included.

## 6. What could not be reconstructed

1. **Proposition 6's proof beyond Lemma 1** — the step from Lemma 1 to the
   `w(0)`, `w(1)` and span statements via (15)–(16).
2. **Lemma 1's own claim** that `G MUT F ⟹ F⁻¹ MUT G⁻¹` ("see the proof of
   Proposition 5"), which is asserted rather than displayed there.
3. **Theorem 1's proof** (Blackwell garbling argument, Appendix).
4. **Propositions 7, 8, 9, 11** and the CES crowding results of §5.5.
5. Step 2 of the Prop 5 proof above — the passage from "`Γ ≥ 0` at critical
   points" to "`Γ ≥ 0` everywhere" — is stated compactly and was not verified.

Anything leaning on (1)–(5) is effectively **[A]-grade** despite the [F, draft]
tag.

## 7. What this implies for entry_contest

**The pivot analogy is sound but has one structural disanalogy that must be
stated.** In Costrell–Loury the marginal worker's quantile `θ` is **fixed by
technology** — jobs come in fixed proportions, so `θ` does not move when `F`
does; only `μ̂ = F⁻¹(θ)` moves. In P9-gen the marginal entrant's index `k*` is
**endogenous**: it is what the comparative static is about. Their pivot rule
says where a *fixed* margin sits in the distribution; ours says how an
*endogenous* count responds. The analogy is in the sign logic, not in the
status of the margin, and P9-gen should say so when it cites them.

**Their object is also a continuum wage schedule; ours is an integer count.**

**A positioning gain.** Prop 10's reversal of Prop 6 shows this class of result
is sensitive to whether the assignment reoptimises. Our analogue of "fixed
assignment" is that `Δ` does not move under a wealth spread — and that is not
an assumption for us but a *proved* proposition (anonymity,
`PROOFS.tex` Prop `prop:anon`). Worth saying: the literature's own experience
is that this direction flips when the allocation re-optimises, and we are safe
because we proved there is no feedback, not because we assumed it away.
