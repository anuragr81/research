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
   aristocratic equilibrium, and all of §IV (capital accumulation, growth
   trajectories, multiple equilibria). None of the formal results were checked.
2. **CMP95's §2 effort model with complete information and §3 incomplete
   information/signaling** — the models that *generate* the concern for
   relative rank. Not reconstructed.
3. CMP95's Appendix (§5).

**What we take from these papers is the conceptual foundation in CMP92's
abstract/§II and CMP95's §4, both of which are prose.** Anything else is
[A]-grade despite the [F] tags.

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
