# Dietrich, F. (2021), "Fully Bayesian Aggregation"

*Journal of Economic Theory* 194, 105255, doi 10.1016/j.jet.2021.105255. The
Drive copy (`dietrich.pdf`, 28 pp.) is HAL hal-03194928, "extended version of
January 2021". Page numbers here are that version's printed pages. Theorem and
definition numbers may differ in the typeset JET article.

## Claims formalized

Belief pooling (Section 4, p. 11; Appendix A, pp. 16-20):

* **Geometric pooling (App. A, p. 16):** `G(p)(ω) ∝ ∏ᵢ pᵢ(ω)^{wᵢ}`, with
  `wᵢ ≥ 0` and `∑ wᵢ = 1`.
* **Linear pooling:** `L(p) = ∑ᵢ wᵢ pᵢ`, the belief half of the
  "linear-linear" rules (p. 7).
* **Dynamic Rationality (p. 8; belief form p. 16).** If `p'ᵢ = pᵢ(·|E)` for all
  `i`, then `F(p') = F(p)(·|E)`: pooling commutes with conditioning on a
  commonly learnt event.
* **External Bayesianity (p. 15).** The same, with revision on a common
  likelihood function. Dietrich calls it "well-known" and does not prove it.
* **Theorem 2 (p. 11).** On coherent profiles, a rule is dynamically rational,
  unanimity-preserving and continuous iff it is geometric. `|Ω| = 2` is
  excluded (p. 6).
* **The duel example (Section 2, Table 1, p. 5).** Linear-linear aggregation
  is not dynamically rational. Linear-geometric aggregation is.
* **Fn. 7 (p. 6).** Linear rules with variable weights still violate Bayes,
  except dictatorially.

## Result

`lean/Dietrich.lean` is a symlink to `lean/Literature/Dietrich.lean`. It builds
with no `sorry`, and every headline theorem uses only
`[propext, Classical.choice, Quot.sound]`.

The paper's content:

* `geoPool_dynRational`: **Theorem 2, Part 1 (p. 18).** Geometric pooling
  commutes with conditioning on any event on which the profile is coherent.
  The proof is Dietrich's: states outside `E` get `∏ 0^{wᵢ} = 0` because some
  `wᵢ > 0`, and states inside `E` pick up a common positive constant.
* `geoPool_unanimous`: geometric pooling is unanimity-preserving.
* `geoPool_externallyBayesian`: geometric pooling commutes with revision on any
  common positive likelihood (p. 15).
* `linPool_cond_sub` and `linPool_comm_iff`: the exact two-person defect of
  linear pooling,
  `λ(1−λ)(p₂(E)−p₁(E))(p₁(ω)/p₁(E) − p₂(ω)/p₂(E)) / (λp₁(E)+(1−λ)p₂(E))`.
  Linear pooling commutes with conditioning iff `λ ∈ {0,1}`,
  `p₁(E) = p₂(E)`, or the two conditionals agree. That is the precise content
  of "generically fails".
* `duel_linear_not_dynRational`: Table 1's duel. The linear pool after
  learning `E` gives `13/18` (printed .72). Conditioning the old pool gives
  `5/6` (printed .83).

The project's question, labelled as such in the file:

* `premise_forces_unanimity` and `eb_premise_forces_unanimity`: with a common
  prior, the premise of either criterion makes all posteriors coincide.
* `dynRational_on_common_prior` and `eb_on_common_prior`: hence **every**
  unanimity-preserving rule satisfies both criteria on common-prior profiles.
  `linPool_dynRational_on_common_prior` and `linPool_eb_on_common_prior` state
  this for linear pooling.
* `cond_eq_self_of_pos` and `not_cond_of_fullSupport`: a full-support posterior
  that differs from the prior is not the prior conditioned on *any* event.

`sympy/check_dietrich.py` runs 24 of 24 checks, exact, and exits 0 in about
1 minute.

* Table 1 is reproduced row by row: probabilities and expected utilities for
  both rules, before and after `E`.
* The geometric pool after `E` equals the conditioned pool exactly.
* Theorem 2 Part 1 and External Bayesianity hold symbolically, with a free
  weight `λ` and a free likelihood.
* The two-person linear defect identity holds.
* On Paper B's model:
  * `P^J_AB` has full support and differs from `P`, so it is not a
    conditionalisation.
  * `P^J_AB/P` and `P^J_BA/P` are not proportional, so there is no common
    likelihood.
  * With a common prior and a common event, the linear pool is dynamically
    rational.
  * The **linear and geometric pools of `(P^J_AB, P^J_BA)` both miss `P^B` at
    order `c`, with identical first-order coefficients.** They differ from each
    other only at `O(c²)`, with a nonzero `c²` term.

## What formalizing revealed

1. **Table 1 has a rounding slip.** The old linear-geometric probability of
   `ω₁` is `0.5042`, which rounds to `.50`, but the table prints `.51`. The
   printed row then sums to 1.00. Every other entry checks, including the new
   linear-geometric expected utility `.01` and the Bayes-corrected linear value
   `.08`.
2. **Dynamic Rationality has no bite on common-prior profiles.** When everyone
   starts from the same prior, the premise "each member conditions on the same
   `E`" (or revises on the same likelihood) forces identical posteriors. Every
   unanimity-preserving rule then passes. Dietrich's characterisation gets its
   power from heterogeneous priors (Claims 1-11 build profiles with differing
   beliefs).
3. The "generic" failure of linear pooling is exactly a product of three
   factors (`linPool_cond_sub`). It vanishes only for dictatorship, equal
   probability of the event, or equal conditionals.

## Bearing on Paper B

Paper B's evaluators share the prior `P`, read the same two cues, and differ
only in reading order. The pre-profile is unanimous, `(P, P)`. The
post-profile is `(P^J_AB, P^J_BA)`. This post-profile is neither a
conditionalisation of `P` on an event (`not_cond_of_fullSupport`: the Jeffrey
posteriors have full support and have moved) nor a revision on a common
likelihood (that would need `P^J_AB = P^J_BA`, contrary to Prop. DIV). So
Dietrich's criterion **cannot be evaluated** on Paper B's population mean.
Where it can be evaluated under a common prior, linear pooling **passes**.

The natural extension compares the pooled posterior with the Bayes-factor
benchmark `P^B`, the sequence-free update of the common prior on the common
cues. Under it, linear pooling fails, but geometric pooling fails in the same
way at first order. The first-order effects that Paper B characterises are
properties of the members' Jeffrey updating. They are not an artefact of
choosing linear over geometric pooling.

## Audit findings (2026-09-29)

These findings respond to `verify_updating_theory.md` §2, which covers the
manuscript at `PAPER_B_MANUSCRIPT.tex` lines 270-274.

* **D1 (commute-with-updating as a normative design criterion): VERIFIED.**
* **D2 ("geometric pooling \citep[Def.~1]{Dietrich2021}"): WRONG-LOCATION,
  confirmed.** Def. 1 (p. 7) defines linear-geometric *preference*
  aggregation. Cite Thm 2 (p. 11) or §4, with the definition in App. A (p. 16).
* **D3 (geometric pooling "satisfies the criterion"): VERIFIED for
  event-conditioning and common likelihoods** (`geoPool_dynRational`,
  `geoPool_externallyBayesian`). The audit's caveat stands, and can be
  sharpened: in Paper B's setting, geometric pooling does no better than linear
  pooling against the benchmark at first order.
* **D4 (linear pooling generically fails): VERIFIED**, with the exact defect
  identity and the duel counterexample.
* **D5 (the population mean "is the outcome of linear pooling and generically
  fails the criterion"): NOT merely unsupported, but false where the claim is
  meaningful.** The audit said the criterion "cannot be evaluated here". The
  formalization confirms this: the premise fails for `(P,P) → (P^J_AB, P^J_BA)`.
  It adds that on common-prior profiles, the only ones Paper B has, linear
  pooling satisfies the criterion whenever the premise holds
  (`linPool_dynRational_on_common_prior`). "Is the outcome of linear pooling"
  is true. "Generically fails the criterion" is not.
* **D6 (bib): VERIFIED.**

### Proposed corrected wording (text only, not applied)

Replace lines 270-274, from "A related normative literature …" to "… protected
class of Proposition~\ref{prop:PRO}.", with:

> A related normative literature evaluates group belief formation by whether
> aggregation commutes with updating \citep{Dietrich2021}. Among continuous,
> unanimity-preserving pooling rules, only geometric pooling commutes with
> conditioning on commonly learnt information \citep[Thm.~2]{Dietrich2021};
> linear pooling generally does not. That criterion does not discriminate in
> the present setting. Our evaluators share a prior and receive the same cues,
> differing only in reading order, so their posteriors are neither
> conditionalisations of the common prior on an event nor revisions on a
> common likelihood. Where the criterion does apply under a common prior, every
> unanimity-preserving rule, linear pooling included, satisfies it. Nor does
> the choice of pooling rule drive our results: the linear and geometric pools
> of the two sequence-posteriors differ only at $O(c^2)$ and miss the
> Bayes-factor benchmark at first order by the same amount. The first-order
> divergence outside the protected class of Proposition~\ref{prop:PRO} is a
> property of the members' Jeffrey updating, not of linear aggregation.

## Not formalized

* The sufficiency half of Theorem 2 (Claims 1-11, pp. 18-20), which needs
  Cauchy's equation, continuity and density arguments.
* Theorems 1 and 1⁺ on preference aggregation.
* Harsanyi's Theorem as a corollary.
* Extensions 1-2.
* The conjectured impossibility of Section 5.
