# Shmaya, E. & Yariv, L. (2016), "Experiments on Decisions Under Uncertainty: A Theoretical Framework"

*American Economic Review* 106(7), 1775-1801. (Drive copy is the November 2008
working paper.)

## Claims formalized

Their Definitions 1-3 and the mathematical cores of Theorems 1 and 2.

* Experimental observations are a map `σ : S^{≤N} → A`; `σ(s)` is the subject's
  report of the most probable alternative after seeing the signal sequence `s`.
* A **conjectured experiment** (Definition 1) is a triple `(α, ν, ς)` of random
  variables valued in `A`, `ℕ`, `S^N`: the conjectured alternative, the number of
  signals observed, and the full signal realization.
* **Restricted** (Definition 2): `ν` independent of `(α, ς)`. **Unrestricted**:
  nothing assumed.
* **Explains** (Definition 3): every conditioning event `{ν = n, ς_i = s_i, i ≤ n}`
  has positive probability, and
  `σ(s) = argmax_a P(α = a | ν = n, ς_i = s_i for i ≤ n)`.

**Theorem 2 (unrestricted, "anything goes").** For every `σ : S^{≤N} → A`, the
observations `σ` admit an explanation by an unrestricted conjectured experiment.

**Theorem 1 (restricted).** `σ` admits an explanation by a restricted conjectured
experiment if and only if: if for some `a` one has `σ(s^x) = a` for every `x ∈ S`,
then `σ(s) = a`. (A no-reversal / Sure-Thing-Principle condition.)

## Result

`lean/ShmayaYariv.lean` (symlink to `lean/Literature/ShmayaYariv.lean`; builds, no
`sorry`, axioms `[propext, Classical.choice, Quot.sound]` only). The sample space is
`Ω = (Fin N → S) × Fin (N+1)` -- a full signal realization together with the number
observed -- so `ς` and `ν` are the two projections, exactly as the triple suggests,
without dependent-type encodings of variable-length sequences.

* `Explains` -- Definition 3, with the argmax read as strict maximality of the joint
  weight (equivalent, the event having positive weight).
* `alpha_const_on_event` -- the heart of Theorem 2: the conditioning event pins the
  observed prefix, so the constructed `α` is *constant* on it.
* `jointWt_of_ne`, `jointWt_self` -- hence the conditional law of `α` is a point
  mass at `σ(s)`.
* `anythingGoes` -- **Theorem 2**, by construction (uniform weights, `α` reading
  `σ` off the observed prefix).
* `alpha_depends_on_nu` -- why that construction is closed to a restricted
  conjecture: the `α` built is not a function of `ς` alone, so it co-varies with
  `ν`.
* `argmax_of_convex_combination` -- the core of Theorem 1's **necessity**: a parent
  assessment that is a convex combination of the children's inherits any
  alternative maximal at every child.
* `no_reversal_of_restricted` -- that step as the necessity direction.

`sympy/check_theorems.py` (4/4 checks, exact rationals, `S = {0,1}`, `N = 2`,
`A = {0,1,2}`):

* **Theorem 2** verified by *exhaustive enumeration* of all `3^7 = 2187`
  observation maps on the 7-node tree: for each, the construction satisfies
  Definition 3 with the argmax unique. 0 violations.
* **Theorem 1 necessity** over 400 random restricted conjectures: the induced `σ`
  never reverses. 0 reversals, as the theorem predicts.
* the convex-combination identity
  `P(α=a | prefix=s) = Σ_x P(prefix=s^x | prefix=s) · P(α=a | prefix=s^x)`
  over 200 random conjectures. 0 mismatches.
* a **reversing** `σ` (root reports 0, both children report 1) shown to violate
  Theorem 1's condition while still being explained by the unrestricted
  construction -- the two theorems' contrast in one example.

## What formalizing revealed

**Where the non-identification comes from, precisely.** Under an unrestricted
conjecture `ν` may be a function of the history, and then the conditioning events of
distinct instances do not overlap: instances of different length are separated by
`ν`, those of equal length by `ς`. Nothing ties the conditional law of `α` across
disjoint events, so `α` can be set to `σ` node by node. Under the restriction,
conditioning on `{ν = n, ς|n = s}` collapses to conditioning on `{ς|n = s}`, and
those events are *nested*: a parent's is the disjoint union of its children's.
Convexity then forbids reversals. **Testability is bought entirely by the nesting
that the restriction reinstates.**

## Bearing on Paper B

The paper is the standard reference for the claim that belief/choice data may have
**no** testable implications for an updating rule absent design restrictions, and
its Theorem 1 is the matching positive result. It establishes that
non-identification results of this kind are a recognized contribution in this
literature, which is the main support for Paper B's measurement framing.

**Correction to an earlier positioning claim.** Two of my characterizations were
wrong and are corrected here. (i) Theorem 2 is about **action rules** -- `σ` maps
signal sequences to *alternatives* -- not about belief sequences. (ii) The source of
the underdetermination is specifically the subject's unrestricted **conjecture about
the experimental design**, not belief data being generically underdetermined. So
Shmaya-Yariv should be cited as evidence that non-identification is taken seriously,
**not** as a structural cousin of Paper B's protected class: Paper B's obstruction is
an order-in-`c` gap from the route-plane geometry, with no analogue of conjectured
designs. (For a genuine structural analogue, see Augenblick-Rabin's Proposition 4
in `../augenblick_rabin2021/`.)

## Not formalized

The sufficiency half of Theorem 1 (constructing a restricted conjecture from a
no-reversal `σ`); Theorem 3 (the characterization of Bayesian-violating reversals);
Sections 5-6 (the intermediate independence case, and unordered signals); and the
measure-theoretic generality of Definition 1 (immaterial here -- the paper notes
that "all our results and proofs depend only on the rooted tree structure of the set
of instances").
