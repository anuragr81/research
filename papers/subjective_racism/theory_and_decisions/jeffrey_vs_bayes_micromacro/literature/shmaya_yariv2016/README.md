# Shmaya, E. & Yariv, L. (2016), "Experiments on Decisions Under Uncertainty: A Theoretical Framework"

*American Economic Review* 106(7), 1775-1801.

**Source.** The Drive copy is the working paper, "Current Version: November 9,
2008" (45 pp.). Every Definition, Theorem, Lemma, Remark and page number below is
the **working paper's**; none is confirmed for the AER version.

## Claims formalized

Definitions 1-3 (on finite probability spaces), Remark 1, Theorem 2, and the
necessity half of Theorem 1.

* `A` is a finite set of alternatives, `S` a finite set of signals, `N` the
  number of available signals and bold **N** `= {0, 1, …, N}` (p.10).
  Experimental observations are a map `σ : S^{≤N} → A`; `σ(s)` is the subject's
  report of the most probable alternative given `s` (p.10).
* A **conjectured experiment** (Definition 1, p.11) is "a triplet
  `(α, τ, ζ = {ζ_n}_{1≤n≤N})` of random variables over some probability space
  `(Ω, 𝒜, ℙ)` with values in `A`, **N**, `S^N` respectively": the conjectured
  alternative, the length of the observed signal sequence, and the signal
  realization. With no restriction on their dependence it is called
  *unrestricted*.
* **Restricted** (Definition 2, p.12): `τ` is independent of the pair `(α, ζ)`.
* **Explains** (Definition 3, p.12): for every `n ∈` **N** and `s₁, …, s_n ∈ S`,
  `ℙ(τ = n, ζᵢ = sᵢ for 1 ≤ i ≤ n) > 0`; and for every instance `s`,
  `σ(s) = argmax_a ℙ(α = a | τ = n, ζᵢ = sᵢ for 1 ≤ i ≤ n)` (1), where "we
  implicitly assert the uniqueness of the maximizer".
* **Remark 1** (p.13): under restriction, (1) becomes
  `σ(s) = argmax_a ℙ(α = a | ζᵢ = sᵢ for 1 ≤ i ≤ n)` (2).

**Theorem 2 (p.18, "anything goes").** For every `σ : S^{≤N} → A`, the
observations `σ` admit an explanation by an unrestricted conjectured experiment.

**Theorem 1 (p.13).** `σ` can be explained by a restricted conjectured experiment
if and only if: for every instance `s`, if for some `a* ∈ A` one has
`σ(s^s) = a*` for every `s ∈ S`, then `σ(s) = a*`. The paper calls this
"reminiscent of the 'Sure Thing Principle' and the notion of dynamic
consistency".

## Result

`lean/ShmayaYariv.lean` is a symlink to `lean/Literature/ShmayaYariv.lean`. It is
standalone on Mathlib, checks with `lake env lean` and contains no `sorry`. Its 25
theorems use only `[propext, Classical.choice, Quot.sound]` or a subset.

**Encoding.** A conjectured experiment is `ConjExp Ω S A N`: a probability vector
`P` (nonnegative, summing to 1) on a finite type `Ω`, with random variables
`α : Ω → A`, `τ : Ω → Fin (N+1)` (bold **N**) and `ζ : Ω → (Fin N → S)`
(`S^N`). This is Definition 1 with a finite probability space, and nothing is
assumed about how the three variables depend on each other. An instance of
length `n` is a pair `(n, x)` read as the first `n` coordinates of `x`; `Obs`
encodes `σ` as a map on such pairs that depends only on the prefix.

| Lean | Content |
|---|---|
| `ConjExp` | **Definition 1** on a finite probability space |
| `ConjExp.Restricted` | **Definition 2**: `ℙ(τ = n, α = a, ζ = x) = ℙ(τ = n) ℙ(α = a, ζ = x)` for all `n, a, x` |
| `ConjExp.Explains` | **Definition 3**, with conditional probabilities `ℙ(α = a, E)/ℙ(E)` and the argmax as a strict maximum |
| `NoReversal` | Theorem 1's condition |
| `jointProb_restricted`, `evProb_restricted` | under Definition 2 the length factors out: `ℙ(α = a, τ = n, ζ|n) = ℙ(τ = n) ℙ(α = a, ζ|n)` |
| `remark1` | **Remark 1, eq. (2)**, derived from Definition 2 |
| `prefJoint_split`, `prefProb_split` | a parent's prefix event is the disjoint union of its children's |
| `posterior_convex_combination` | **eq. (4) as used in the proof (p.15)**: under Definitions 2-3 the parent's posterior is a convex combination of its children's, with strictly positive weights summing to 1 |
| `argmax_of_convex_combination` | the generic step: an alternative maximal at every child is maximal at the parent |
| `no_reversal_of_restricted` | **Theorem 1, necessity**: `E.Restricted → E.Explains σ → NoReversal σ` |
| `canonical`, `alpha_const_on_event`, `jointWt_of_ne`, `jointWt_self` | the Theorem 2 construction: uniform probability on `(Fin N → S) × Fin (N+1)`, `τ`, `ζ` the projections, `α` reading `σ` off the observed prefix; `α` is constant on each conditioning event |
| `anythingGoes` | **Theorem 2** |
| `alpha_depends_on_nu` | if `σ` reports differently at two lengths on one realization, the canonical experiment is **not** restricted (Definition 2 fails) |
| `reversal_example` | a reversing `σ` (`N = 1`, root `false`, both children `true`) is explained by the canonical experiment and by no restricted experiment on any finite space |

The necessity proof follows the paper's (p.15) but derives what the paper takes
from Lemma 1: the factorization under Definition 2 and the nesting of prefix
events. Were `σ(s) = b ≠ a*`, the parent's argmax gives
`ℙ(α = a*, ζ|n = s) < ℙ(α = b, ζ|n = s)`, each child's argmax gives
`ℙ(α = b, ζ|n+1 = s^s) < ℙ(α = a*, ζ|n+1 = s^s)`, and summing over the children
contradicts the first inequality.

`sympy/check_theorems.py` (4/4 checks, exact rationals, `S = {0,1}`, `N = 2`,
`A = {0,1,2}`):

* **Theorem 2** by exhaustive enumeration of all `3^7 = 2187` observation maps on
  the 7-node tree: for each, the construction satisfies Definition 3 with a
  unique argmax. 0 violations. (The script's sample space is the 7 instances
  with singleton events; the Lean construction uses `(Fin N → S) × Fin (N+1)`.
  The two give the same conditional laws but are not the same space.)
* **Theorem 1 necessity** over 400 random restricted conjectures: the induced `σ`
  never reverses.
* the convex-combination identity
  `P(α=a | prefix=s) = Σ_x P(prefix=s^x | prefix=s) · P(α=a | prefix=s^x)`
  over 200 random conjectures. 0 mismatches.
* a **reversing** `σ` (root reports 0, both children report 1), explained by the
  unrestricted construction while violating Theorem 1's condition.

## What formalizing revealed

**Where the non-identification comes from.** The conditioning events
`{τ = n, ζ|n = s}` of distinct instances are disjoint in *every* conjectured
experiment, restricted or not: different lengths are separated by `τ`, equal
lengths by `ζ`. What the unrestricted case adds is that the conditional law of
`α` may depend on `τ`, so nothing ties the law of `α` at a parent to its laws at
the children. The paper's construction for Theorem 2 does not make `τ` a
function of the history; it takes a full-support distribution on **N** `× S^N`
(p.20) and lets `α`'s conditional law depend on the instance. Definition 2
removes the dependence on `τ` (Remark 1, `remark1`): the assessment at `(n, s)`
becomes the assessment on the prefix event `{ζ|n = s}`, and those events are
nested, a parent's being the disjoint union of its children's
(`prefJoint_split`). The posterior at a parent is then a convex combination of
the children's (`posterior_convex_combination`), which forbids reversals.

## Bearing on Paper B

The paper is the standard reference for the claim that choice data may have
**no** testable implications for an updating rule absent design restrictions,
and its Theorem 1 is the matching positive result. It establishes that
non-identification results of this kind are a recognized contribution in this
literature, which supports Paper B's measurement framing.

Two earlier characterizations were corrected. (i) Theorem 2 is stated for
**action rules** (`σ` maps signal sequences to alternatives). Lemma 3 (p.19)
adds that "even if for each instance the full belief were elicited … an
analogous 'anything goes' result would still hold", so the result does extend
to elicited posteriors. (ii) The source of the underdetermination is the
subject's unrestricted **conjecture about the experimental design**, not belief
data being generically underdetermined. So Shmaya-Yariv should be cited as
evidence that non-identification is taken seriously, not as a structural cousin
of Paper B's protected class: Paper B's obstruction is an order-in-`c` gap from
the route-plane geometry, with no analogue of conjectured designs. (For a closer
structural analogue, see Augenblick-Rabin's Proposition 4 in
`../augenblick_rabin2021/`.)

## Audit findings (2026-09-30)

This section answers `notes/citation_audit.md` item L9 and
`notes/citation_audit/verify_record_papers.md` §2.

* **L9(a) "Definitions 1-3 formalized" (was CONTRADICTED).** Definition 2 is
  now encoded (`ConjExp.Restricted`) as independence of `τ` from `(α, ζ)`, and
  Definition 1 is a finite probability space with three unconstrained random
  variables. The earlier file fixed `α` as a function of `(ζ, τ)` and used
  signed weights. The claim is now true for finite probability spaces.
* **L9(b) `no_reversal_of_restricted` (was UNSUPPORTED).** It is now Theorem 1
  necessity itself: from `E.Restricted` and `E.Explains σ` it concludes
  `NoReversal σ`, i.e. `σ(s) = a*`. The convex-combination step is proved from
  Definition 2 (`jointProb_restricted`, `prefJoint_split`), not assumed. The old
  statement (equal scores from an assumed convex combination) is gone; its
  generic core survives as `argmax_of_convex_combination`.
* **L9(c) notation (was MISQUOTED).** Docstrings and this README now use the
  paper's `(α, τ, ζ)` with values in `A`, bold **N** `= {0..N}`, `S^N`.
* **L9(d) "events do not overlap when ν is a function of history" (was
  UNSUPPORTED).** Replaced by the account above. `alpha_depends_on_nu` now
  proves that Definition 2 fails for the canonical construction, instead of
  exhibiting two outcomes.
* **§2 item 11.** "All our results and proofs depend only on the rooted tree
  structure of the set of instances" (p.11) is about the instance tree (it goes
  on: "the results remain true if the number of available signals is
  infinite"). It does not license ignoring the general probability space of
  Definition 1, and it is no longer used that way; the finite-space restriction
  is listed under "Not formalized".
* **§2 item 12.** Theorem 3 (p.23) is the Section 5 result: partially
  restricted conjectures (Definition 4, `τ` independent of `ζ` given `α`),
  binary `A`, and explainability iff "revealed higher" is anti-symmetric.
  Theorem 4 (unordered signals, no Dutch book) was not mentioned before; both
  are listed below.
* **§2 item 2.** Numbering is the 2008 working paper's.

## Not formalized

* The sufficiency half of Theorem 1: Lemma 1 (explainable posteriors, eq. (4)
  with the relative interior) and Lemma 2 (the one-period construction), which
  build a restricted conjecture from a no-reversal `σ`.
* Lemma 3 (explainable posteriors, unrestricted) beyond the point-mass case used
  for Theorem 2.
* Adapted conjectures and Corollary 1 (p.16-17).
* Theorem 3 (Section 5: partially restricted conjectures, Definition 4, binary
  `A`, anti-symmetry of "revealed higher") and Theorem 4 (unordered signals).
* Probability spaces that are not finite (Definition 1 allows any
  `(Ω, 𝒜, ℙ)`).
