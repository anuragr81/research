# Reconstruction — Cole, Mailath & Postlewaite

**Sources.** Both read from the Drive PDFs, both with text layers.

| Short | Full | Evidence |
|---|---|---|
| CMP92 | "Social Norms, Savings Behavior, and Growth", *JPE* 100(6), Dec. 1992, Centennial Issue, pp. 1092–1125 | **[F]**, published |
| CMP95 | "Incorporating Concern for Relative Wealth into Economic Models", CARESS Working Paper **#95-14** | **[F, WP]** |

These are the papers cited to justify treating `V` as an exogenous,
rank-allocated, non-market prize.

---

## 1. CMP92 — the question and the device

From the abstract:

> "We argue that many goods and decisions are not allocated or made through
> markets. We interpret an agent's status as a **ranking device that determines
> how well he or she fares in the nonmarket sector**. The existence of a
> nonmarket sector can **endogenously generate a concern for relative position**
> in, for example, the income distribution so that higher income implies higher
> status. Moreover, it can naturally yield multiple equilibria."

§II lists the non-market decisions: "the provision of public goods, the most
attractive mate for oneself or one's child, a favorable location in church or
at the dinner table, a respectful audience when one speaks, or the cemetery
plot with the best view." Their point: a society must reconcile conflicting
preferences over these somehow, and **status is that ranking device**.

**§III the basic model.** Continuum of one-period-lived men and women, matched
into pairs. Women endowed with a non-traded, non-storable good `j ~ U[0,1]`;
men indexed `i ∈ [0,1]`; CRRA utility over joint consumption; only male
offspring welfare enters the pair's utility. Matching is by rank in wealth.
§III.D adds an **aristocratic equilibrium** with an exogenously given status
assignment. §IV extends to capital accumulation and growth trajectories, and
the multiple equilibria deliver cross-country growth differences without
differences in preferences, technology, or endowments.

## 1a. CMP92 §IV.A, the two-period example (reconstructed in pass 2)

The derivation chain, in the paper's notation, pp. 1100–1103.

1. Technology `c = Ak − k′` with `A > 1` (eq. (1)). A father with capital `K`
   consumes the fraction `λ` of first-period output `AK` and bequeaths
   `k′ = AK(1 − λ)`. The son's pair consumes `A²K(1 − λ)` and gets the
   endowment `j` of his mate. The father's utility is
   `u(AKλ) + β[u(A²K(1 − λ)) + j]` with CRRA `u`.
2. Matching is by rank. The wealthiest son gets the woman of highest
   endowment, so a son's `j` is his rank in the sons' capital distribution.
3. One-point economy, every father at `K`. The father whose son gets `j = 0`
   is undistorted and attains `V(0)`, the maximum of eq. (2), at
   `λ(0) = [1 + (βA^{1−γ})^{1/γ}]^{−1}`. Because every father can imitate
   every other, every father attains `V(0)`, so `λ(j)` solves
   `V(j) = V(0)` with `λ(j) < λ(0)`, and `∂λ/∂j < 0`.
4. Two-point economy, fathers at `K − ε` on `[0, ½)` and `K + ε` on `[½, 1]`.
   The lower half repeats step 3 with `K − ε`, which fixes the top bequest of
   the lower half, `k⁻(½)`. The top-half father whose son gets rank `½`
   maximises at `K + ε` subject to his son's capital being at least `k⁻(½)`,
   which gives `V(½)`. Every top-half father then solves `V(j) = V(½)`.
5. Comparison. Hold the compared fathers' initial capital equal, so the
   one-point economy sits at `K + ε`. Both fathers then solve the same
   equation in `λ` with right-hand sides `V(0) − βj` and `V(½) − βj`, so
   `V(½) ≥ V(0)` gives a weakly higher `λ(j)` at every `j ≥ ½`, and the top half
   of the two-point economy saves weakly less. The inequality `V(½) ≥ V(0)`
   holds when the one-point man at rank `½` already satisfies the two-point
   restriction, `k⁻(½) ≤ k₁(½)`, which the paper does not state.

The result is derived *from* the rank-allocation of mates (step 2) and the
equal-welfare property of a one-point start (step 3). It is a statement about
men with equal initial capital in the top half, and it is hedged in the
source ("will tend to", "all other things being equal").

## 2. CMP95 — the conceptual statement we actually use

CMP95 §4 (*Concluding Comments*) contains the passage the handover cites. In
full, because the precision matters:

> "This raises the question of whether there is a simple reinterpretation of
> the equilibrium in which an implicit price can be put on the scarce objects.
> [...] But this is not quite correct. Unlike the situation in which women work
> to buy some inelastically supplied good of varying quality like land, women
> in our models **don't really pay** for mates. [...] To make the land example
> analogous to our models, we should have **the land simply given away, with
> the best given to the wealthiest, and so on**. The allocation of desirable
> goods or decisions in accordance with economic performance can substantially
> differ from the allocation of those goods through normal markets. In
> particular, we should note that **when the desirable goods or decisions are
> allocated as prizes rather than sold, the standard welfare theorems regarding
> the Pareto optimality of the outcomes no longer apply.**"

and the generalisation:

> "Whenever an increase in an individual's position in the wealth distribution
> by itself increases the likelihood of obtaining desirable outcomes, optimal
> individual behavior will exhibit some of the qualitative features exhibited
> in the models analyzed above."

with examples: "Country club memberships, charity board invitations, university
trusteeships, invitations to chic parties, and assigned seats in churches and
synagogues."

## 3. What could not be reconstructed

Most of both papers, and it should be said plainly.

1. **CMP92 §III's equilibrium existence and characterisation**, §III.D's
   aristocratic equilibrium, and §IV.B to §IV.D (capital accumulation, growth
   trajectories, multiple equilibria). §IV.A is reconstructed in §1a above and
   formalised in `ColeMailathPostlewaite.lean`; Property 1's exchange argument
   is formalised there too.
2. **CMP95's §3 incomplete information/signaling** model. Not reconstructed.
   The §2 effort model is formalised through its §2.1 closed form, in which
   the matching function `m(y) = gy` is the distribution function of output
   and `g(1 + g) = 1/α²` (eq. (2.5)).
3. CMP95's Appendix (§5).

**What we take from these papers is the conceptual foundation in CMP92's
abstract and §II and CMP95's §4, which are prose, and the CMP92 §IV.A
comparison, which is now formalised.** The remaining formal results are
unchecked.

## 4. What this implies for entry_contest

**The citation instruction is sound.** The survey's next-step 2 says to cite
CMP92/95 where `V` is introduced, "since they justify a rank-allocated
non-market prize". Both halves are in the sources: a prize allocated by rank
rather than sold (CMP95 §4), and status as the ranking device that governs
non-market allocation (CMP92 abstract, §II).

**Two things this pass adds.**

First, `V`'s role in our model is a **pure scale factor**. P1 gives
`Δ(m) = V·E[φ(M_m)]`, so `∂Δ/∂V` is free of `V` and `Δ` is exactly homogeneous
of degree one in it (CMP-4). That is why we can import CMP's justification for
treating `V` as an exogenous rank-allocated prize *without* importing their
matching machinery: nothing in our results depends on where `V` comes from,
only on its being fixed and rank-allocated.

Second, and more usefully, **CMP95 §4 and Levin–Smith Prop 3 are the same
point**. CMP's observation is conceptual: prizes given by rank are not sold, so
the welfare theorems lapse. Levin–Smith's is algebraic: with `V_n ≡ V` the
social gain from the marginal entrant is zero while her private gain is
positive. CMP-1 and CMP-2 show the conceptual premise *generates* the algebra —
one prize delivered iff at least one competitor enters, each paying `c`, gives

> `S = [1 − (1−q)^N]V − qNc`

which is **Levin–Smith equation (8) exactly**. So the deferred welfare section
has a conceptual authority (CMP95) and a formal apparatus (LS §I.A) that agree,
and the bridge between them is one line.
