/-
# Shmaya & Yariv, "Experiments on Decisions Under Uncertainty: A Theoretical Framework"

*American Economic Review* 106(7), 2016, 1775-1801 (working paper, November 2008).

Formalization of the paper's own definitions and the mathematical cores of its
Theorems 1 and 2, not of Paper B's claims.

## The paper's setup

`A` alternatives, `S` signals, `N` the number of available signals.  A
*conjectured experiment* (their Definition 1) is a triple `(α, ν, ς)` of random
variables on a probability space, valued in `A`, `ℕ`, and `S^N`: `α` is the
subject's conjecture about the realized alternative, `ν` the number of signals she
thinks she sees, and `ς` the full signal realization.  It is *restricted*
(Definition 2) when `ν` is independent of `(α, ς)`, *unrestricted* when nothing is
assumed.  It *explains* observations `σ : S^{≤N} → A` (Definition 3) when each
conditioning event `{ν = n, ςᵢ = sᵢ for i ≤ n}` has positive probability and

  `σ s = argmax_{a ∈ A} P(α = a | ν = n, ςᵢ = sᵢ for i ≤ n)`.

**Theorem 2 (unrestricted: "anything goes").** Every `σ` admits an explanation by
an unrestricted conjectured experiment.

**Theorem 1 (restricted).** `σ` admits an explanation by a *restricted*
conjectured experiment iff it never reverses: if `σ (s ^ x) = a` for every
`x ∈ S`, then `σ s = a`.  (A Sure-Thing-Principle condition.)

## What is formalized

The sample space is modelled as the paper's triple suggests: an element of
`Ω = (Fin N → S) × Fin (N+1)` is a full signal realization together with the
number of signals observed, so `ς` and `ν` are the two projections.  This keeps
the paper's structure while avoiding dependent-type encodings of variable-length
sequences.  An *instance* is a pair `(n, x)`, read as "the first `n` signals of
`x`"; observations depend only on that prefix, which is `Obs.prefix_inv`.

* `Explains` — Definition 3 for a finite conjectured experiment, with the argmax
  read as strict maximality of the joint weight (equivalent, since the
  conditioning event has positive weight).
* `anythingGoes` — **Theorem 2**, by construction: uniform positive weights and
  `α ω := σ ω.2 ω.1`.  The work is `alpha_const_on_event`: the conditioning event
  pins the observed prefix, so `α` is *constant* on it; hence the conditional law
  of `α` is a point mass and `σ (n, x)` is the unique argmax.
* `alpha_depends_on_nu` — why that construction is closed to a *restricted*
  conjecture: the `α` built above is not a function of `ς` alone, so it co-varies
  with `ν` and cannot be independent of it.  This is the hinge between the
  theorems.
* `argmax_of_convex_combination` — the core of Theorem 1's *necessity*.  Under the
  restriction, conditioning on `{ν = n, ς|n = s}` is conditioning on `{ς|n = s}`,
  and a parent event is the disjoint union of its children's; so the parent's
  assessment is a convex combination of the children's, and an alternative maximal
  at every child is maximal at the parent.  That is the no-reversal condition.
* `no_reversal_of_restricted` — that step as the necessity direction.

Not formalized: the sufficiency half of Theorem 1 (building a restricted
conjecture from a no-reversal `σ`), and Theorems 3 onwards.
-/
import Mathlib

namespace Literature.ShmayaYariv

open Finset

variable {S A : Type*} {N : ℕ}

/-! ## Sample space, `ν` and `ς` -/

/-- The sample space: a full signal realization together with the number of
signals observed.  `ς` is the first projection, `ν` the second. -/
abbrev Omega (S : Type*) (N : ℕ) := (Fin N → S) × Fin (N + 1)

/-- `ς`, the conjectured signal realization. -/
def sig (ω : Omega S N) : Fin N → S := ω.1

/-- `ν`, the conjectured number of observed signals. -/
def len (ω : Omega S N) : Fin (N + 1) := ω.2

/-- Two signal realizations agree on the first `n` coordinates. -/
def agreeBelow (n : ℕ) (x y : Fin N → S) : Prop :=
  ∀ i : Fin N, (i : ℕ) < n → x i = y i

instance [DecidableEq S] (n : ℕ) (x y : Fin N → S) : Decidable (agreeBelow n x y) := by
  unfold agreeBelow; infer_instance

theorem agreeBelow_refl (n : ℕ) (x : Fin N → S) : agreeBelow n x x := fun _ _ => rfl

/-! ## Observations -/

/-- Experimental observations `σ`, as a prefix-invariant map on instances. -/
structure Obs (S A : Type*) (N : ℕ) where
  /-- the reported alternative for the instance `(n, x)` -/
  f : Fin (N + 1) → (Fin N → S) → A
  /-- the report depends only on the first `n` signals -/
  prefix_inv : ∀ (n : Fin (N + 1)) (x y : Fin N → S), agreeBelow (n : ℕ) x y → f n x = f n y

/-! ## Conditioning events and Definition 3 -/

variable [Fintype S] [DecidableEq S] [DecidableEq A]

/-- The conditioning event of the instance `(n, x)`:
`{ν = n} ∩ {ςᵢ = xᵢ for i < n}`. -/
def event (n : Fin (N + 1)) (x : Fin N → S) : Finset (Omega S N) :=
  Finset.univ.filter fun ω => len ω = n ∧ agreeBelow (n : ℕ) x (sig ω)

theorem self_mem_event (n : Fin (N + 1)) (x : Fin N → S) : (x, n) ∈ event n x := by
  simp [event, len, sig, agreeBelow_refl]

/-- Weight of a set of outcomes. -/
def wt (w : Omega S N → ℝ) (T : Finset (Omega S N)) : ℝ := ∑ ω ∈ T, w ω

/-- The joint weight of `{α = a}` inside the conditioning event of `(n, x)`. -/
def jointWt (w : Omega S N → ℝ) (α : Omega S N → A)
    (n : Fin (N + 1)) (x : Fin N → S) (a : A) : ℝ :=
  wt w ((event n x).filter fun ω => α ω = a)

/-- **Definition 3.**  The conjectured experiment `(α, w)` explains `σ` when every
conditioning event carries positive weight and, on each, `σ (n, x)` strictly
maximizes the conditional law of `α`.  As the event has positive weight, comparing
conditional probabilities is the same as comparing joint weights. -/
structure Explains (w : Omega S N → ℝ) (α : Omega S N → A) (σ : Obs S A N) : Prop where
  pos : ∀ (n : Fin (N + 1)) (x : Fin N → S), 0 < wt w (event n x)
  argmax : ∀ (n : Fin (N + 1)) (x : Fin N → S) (a : A), a ≠ σ.f n x →
    jointWt w α n x a < jointWt w α n x (σ.f n x)

/-! ## Theorem 2: anything goes -/

/-- The construction: `α` reads the observations off the observed prefix. -/
def alphaOf (σ : Obs S A N) (ω : Omega S N) : A := σ.f (len ω) (sig ω)

/-- **The heart of Theorem 2.**  On the conditioning event of `(n, x)` the
constructed `α` is *constant*, equal to `σ (n, x)`: the event fixes `ν = n` and the
first `n` signals, and by prefix-invariance that is all `σ` consults. -/
theorem alpha_const_on_event (σ : Obs S A N) {n : Fin (N + 1)} {x : Fin N → S}
    {ω : Omega S N} (hω : ω ∈ event n x) : alphaOf σ ω = σ.f n x := by
  simp only [event, Finset.mem_filter] at hω
  obtain ⟨-, hlen, hagree⟩ := hω
  unfold alphaOf
  rw [hlen]
  exact (σ.prefix_inv n x (sig ω) hagree).symm

/-- Hence every alternative other than `σ (n, x)` has joint weight zero: the
conditional law of `α` on the event is a point mass. -/
theorem jointWt_of_ne (σ : Obs S A N) (w : Omega S N → ℝ)
    (n : Fin (N + 1)) (x : Fin N → S) {a : A} (ha : a ≠ σ.f n x) :
    jointWt w (alphaOf σ) n x a = 0 := by
  unfold jointWt wt
  apply Finset.sum_eq_zero
  intro ω hω
  simp only [Finset.mem_filter] at hω
  exact absurd (hω.2.symm.trans (alpha_const_on_event σ hω.1)) ha

/-- ... and `σ (n, x)` itself carries the whole weight of the event. -/
theorem jointWt_self (σ : Obs S A N) (w : Omega S N → ℝ)
    (n : Fin (N + 1)) (x : Fin N → S) :
    jointWt w (alphaOf σ) n x (σ.f n x) = wt w (event n x) := by
  unfold jointWt wt
  refine Finset.sum_congr ?_ fun _ _ => rfl
  exact Finset.filter_true_of_mem fun ω hω => alpha_const_on_event σ hω

/-- **Theorem 2 ("anything goes").**  Every prefix-invariant observation map `σ` is
explained by an unrestricted conjectured experiment.  Weights are uniform, and `α`
is the observations themselves read off the observed prefix. -/
theorem anythingGoes (σ : Obs S A N) :
    ∃ (w : Omega S N → ℝ) (α : Omega S N → A), Explains w α σ := by
  refine ⟨fun _ => 1, alphaOf σ, ⟨?_, ?_⟩⟩
  · intro n x
    have hne : (event n x).Nonempty := ⟨(x, n), self_mem_event n x⟩
    unfold wt
    simpa using Finset.card_pos.mpr hne
  · intro n x a ha
    rw [jointWt_of_ne σ _ n x ha, jointWt_self]
    have hne : (event n x).Nonempty := ⟨(x, n), self_mem_event n x⟩
    unfold wt
    simpa using Finset.card_pos.mpr hne

/-- **Why the restriction closes this route.**  Whenever `σ` reports differently at
two lengths on the same signal realization, the constructed `α` takes two values at
outcomes sharing the same `ς`.  So `α` is not a function of `ς` alone: it co-varies
with `ν`, and `ν` cannot be independent of `(α, ς)` as Definition 2 demands. -/
theorem alpha_depends_on_nu (σ : Obs S A N) (n m : Fin (N + 1)) (x : Fin N → S)
    (h : σ.f n x ≠ σ.f m x) :
    ∃ ω₁ ω₂ : Omega S N, sig ω₁ = sig ω₂ ∧ alphaOf σ ω₁ ≠ alphaOf σ ω₂ :=
  ⟨(x, n), (x, m), rfl, h⟩

/-! ## Theorem 1: why the restriction bites

Under Definition 2 the length `ν` is independent of `(α, ς)`, so conditioning on
`{ν = n, ς|n = s}` is conditioning on `{ς|n = s}`.  Those events are *nested*, not
disjoint: a parent's event is the disjoint union of its children's.  Hence the
assessment at the parent is a convex combination of the assessments at the
children, and the freedom exploited by `anythingGoes` is gone. -/

/-- **The core of Theorem 1's necessity.**  If the parent's assessment `f` is a
convex combination, with weights `p`, of the children's assessments `g x`, and an
alternative `a` maximizes every `g x`, then `a` maximizes `f`.

This is the paper's observation that "her assessment of each realized alternative
under `s` is a convex combination of the corresponding assessments over all
continuations `s^x`; in particular, the most likely alternative must be `a`." -/
theorem argmax_of_convex_combination {ι : Type*} [Fintype ι]
    (p : ι → ℝ) (hp : ∀ x, 0 ≤ p x)
    (f : A → ℝ) (g : ι → A → ℝ)
    (hf : ∀ b, f b = ∑ x, p x * g x b)
    (a : A) (hmax : ∀ x, ∀ b, g x b ≤ g x a) :
    ∀ b, f b ≤ f a := by
  intro b
  rw [hf b, hf a]
  exact Finset.sum_le_sum fun x _ => mul_le_mul_of_nonneg_left (hmax x b) (hp x)

/-- **Necessity half of Theorem 1, as no-reversal.**  If the parent's assessment is
the convex combination of its children's, and some `a` is maximal at *every* child,
then the parent's reported alternative scores the same as `a`.  So a `σ` explained
by a restricted conjectured experiment cannot reverse: agreement of all
continuations on `a` forces the parent to report `a` too. -/
theorem no_reversal_of_restricted {ι : Type*} [Fintype ι]
    (p : ι → ℝ) (hp : ∀ x, 0 ≤ p x)
    (f : A → ℝ) (g : ι → A → ℝ)
    (hf : ∀ b, f b = ∑ x, p x * g x b)
    (a : A) (hchild : ∀ x, ∀ b, g x b ≤ g x a)
    (aParent : A) (hparent : ∀ b, f b ≤ f aParent) :
    f aParent = f a := by
  have h1 : f a ≤ f aParent := hparent a
  have h2 : f aParent ≤ f a :=
    argmax_of_convex_combination p hp f g hf a hchild aParent
  linarith

end Literature.ShmayaYariv
