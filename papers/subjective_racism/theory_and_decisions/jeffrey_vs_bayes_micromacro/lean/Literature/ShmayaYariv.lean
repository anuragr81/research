/-
# Shmaya & Yariv, "Experiments on Decisions Under Uncertainty: A Theoretical Framework"

*American Economic Review* 106(7), 2016, 1775-1801.  The copy read is the
working paper, "Current Version: November 9, 2008"; every Definition, Theorem,
Remark and page number below is the **working paper's**, unconfirmed for the AER
version.

Formalization of the paper's own definitions and of Theorem 2 and the necessity
half of Theorem 1, not of Paper B's claims.

## The paper's setup (pp.10-13)

`A` alternatives, `S` signals, `N` the number of available signals, and bold
`𝐍 = {0, …, N}` (p.10).  Experimental observations are a map `σ : S^{≤N} → A`;
`σ(s)` is the subject's report of the most probable alternative after the signal
sequence `s` (p.10).

* **Definition 1** (p.11).  A *conjectured experiment* is a triplet
  `(α, τ, ζ = {ζ_n}_{1≤n≤N})` of random variables over some probability space
  `(Ω, 𝒜, ℙ)` with values in `A`, `𝐍`, `S^N` respectively: `α` is the conjecture
  about the alternative, `τ` the length of the observed signal sequence, `ζ` the
  signal realization.
* **Definition 2** (p.12).  It is *restricted* when `τ` is independent of the
  pair `(α, ζ)`.
* **Definition 3** (p.12).  It *explains* `σ` when (1) for every `n ∈ 𝐍` and
  `s₁, …, s_n ∈ S`, `ℙ(τ = n, ζᵢ = sᵢ for 1 ≤ i ≤ n) > 0`, and (2) for every
  instance `s = (s₁, …, s_n)`,
  `σ(s) = argmax_{a ∈ A} ℙ(α = a | τ = n, ζᵢ = sᵢ for 1 ≤ i ≤ n)`, the argmax
  being asserted unique.
* **Remark 1** (p.13).  Under restriction, (1) becomes
  `σ(s) = argmax_a ℙ(α = a | ζᵢ = sᵢ for 1 ≤ i ≤ n)`   (2).
* **Theorem 1** (p.13).  `σ` can be explained by a restricted conjectured
  experiment iff: for every instance `s`, if `σ(s^s) = a*` for every `s ∈ S`, then
  `σ(s) = a*` (no reversal).
* **Theorem 2** (p.18).  Every `σ` can be explained by an unrestricted conjectured
  experiment ("anything goes").

## Encoding

* Probability spaces are **finite**: `ConjExp Ω S A N` is a probability vector
  `P` on a `Fintype Ω` (nonnegative, summing to `1`) with three random variables
  `α : Ω → A`, `τ : Ω → Fin (N+1)` (bold `𝐍`), `ζ : Ω → (Fin N → S)` (`S^N`).
  This is Definition 1 with `(Ω, 𝒜, ℙ)` finite.  Nothing is assumed about how
  `α`, `τ`, `ζ` depend on one another.
* An *instance* of length `n` is `(n, x)`, read as the first `n` coordinates of
  `x`; `Obs` is `σ` as a prefix-invariant map on such pairs (`Obs.prefix_inv`).
  Coordinates are 0-indexed: the paper's `ζᵢ, 1 ≤ i ≤ n` is `ζ i, i < n`.
* `Restricted` is Definition 2 in the discrete form of independence:
  `ℙ(τ = n, α = a, ζ = x) = ℙ(τ = n) · ℙ(α = a, ζ = x)` for all `n, a, x`.
* `Explains` is Definition 3 verbatim, with conditional probabilities
  `condProb = ℙ(α = a, E) / ℙ(E)` and the argmax as a strict maximum (the paper's
  uniqueness convention).

## What is proved

* `no_reversal_of_restricted` — **Theorem 1, necessity**: if a restricted
  conjectured experiment explains `σ`, then `σ` satisfies the no-reversal
  condition.  The proof derives, rather than assumes, the two facts the paper
  uses: `jointProb_restricted` (under Definition 2 the length factors out, which
  gives `remark1`, the paper's eq. (2)) and `prefJoint_split` (a parent's prefix
  event is the disjoint union of its children's).  `posterior_convex_combination`
  is the paper's eq. (4) as used in the proof on p.15: the parent's posterior is
  a convex combination of the children's with *strictly positive* weights.
* `argmax_of_convex_combination` — the generic step "an alternative maximal at
  every child is maximal at a convex combination".
* `anythingGoes` — **Theorem 2**, by construction on
  `Ω = (Fin N → S) × Fin (N+1)` with the uniform probability, `τ, ζ` the
  projections and `α` reading `σ` off the observed prefix (`canonical`).  The
  work is `alpha_const_on_event`: the conditioning event pins the observed prefix,
  so `α` is constant on it and its conditional law is a point mass.
* `alpha_depends_on_nu` — whenever `σ` reports differently at two lengths on the
  same realization, the canonical experiment is **not** restricted (Definition 2
  fails, proved from the definition).
* `reversal_example` — a reversing `σ` (`N = 1`, root reports `false`, both
  children report `true`) that the canonical experiment explains but no
  restricted conjectured experiment on any finite space explains.

## Not formalized

The sufficiency half of Theorem 1 (Lemmas 1-2: constructing a restricted
conjecture from a no-reversal `σ`), adapted conjectures (Corollary 1),
Theorems 3-4, and probability spaces that are not finite.
-/
import Mathlib

set_option linter.unusedSectionVars false

namespace Literature.ShmayaYariv

open Finset

variable {S A : Type*} {N : ℕ}

/-! ## Instances and observations -/

/-- Two signal realizations agree on the first `n` coordinates. -/
def agreeBelow (n : ℕ) (x y : Fin N → S) : Prop :=
  ∀ i : Fin N, (i : ℕ) < n → x i = y i

instance [DecidableEq S] (n : ℕ) (x y : Fin N → S) : Decidable (agreeBelow n x y) := by
  unfold agreeBelow; infer_instance

theorem agreeBelow_refl (n : ℕ) (x : Fin N → S) : agreeBelow n x x := fun _ _ => rfl

/-- Experimental observations `σ : S^{≤N} → A`, as a map on instances `(n, x)`
("the first `n` signals of `x`") that depends only on the prefix. -/
structure Obs (S A : Type*) (N : ℕ) where
  /-- the reported alternative for the instance `(n, x)` -/
  f : Fin (N + 1) → (Fin N → S) → A
  /-- the report depends only on the first `n` signals -/
  prefix_inv : ∀ (n : Fin (N + 1)) (x y : Fin N → S), agreeBelow (n : ℕ) x y → f n x = f n y

/-- The no-reversal condition of **Theorem 1** (p.13): for an instance `s` of
length `n < N`, if every one-signal continuation `s^s` is reported as `a`, then
`s` is reported as `a`.  The child `s^s` is `(n+1, x[n ↦ s])`. -/
def NoReversal (σ : Obs S A N) : Prop :=
  ∀ (n : Fin (N + 1)) (hn : (n : ℕ) < N) (x : Fin N → S) (a : A),
    (∀ s : S, σ.f ⟨n + 1, by omega⟩ (Function.update x ⟨n, hn⟩ s) = a) → σ.f n x = a

/-! ## Definitions 1-3 on a finite probability space -/

/-- **Definition 1** (p.11), on a finite probability space: a probability vector
`P` on `Ω` and random variables `α`, `τ`, `ζ` valued in `A`, `𝐍 = Fin (N+1)`,
`S^N`. -/
structure ConjExp (Ω : Type*) [Fintype Ω] (S A : Type*) (N : ℕ) where
  /-- the probability of each outcome -/
  P : Ω → ℝ
  nonneg : ∀ ω, 0 ≤ P ω
  total : ∑ ω, P ω = 1
  /-- the conjectured alternative -/
  α : Ω → A
  /-- the length of the observed signal sequence -/
  τ : Ω → Fin (N + 1)
  /-- the signal realization -/
  ζ : Ω → Fin N → S

namespace ConjExp

variable {Ω : Type*} [Fintype Ω] [Fintype S] [DecidableEq S] [Fintype A] [DecidableEq A]
  (E : ConjExp Ω S A N)

/-- `ℙ(τ = n)`. -/
def probT (n : Fin (N + 1)) : ℝ := ∑ ω, if E.τ ω = n then E.P ω else 0

/-- `ℙ(α = a, ζ = x)`. -/
def probAZ (a : A) (x : Fin N → S) : ℝ :=
  ∑ ω, if E.α ω = a ∧ E.ζ ω = x then E.P ω else 0

/-- `ℙ(τ = n, α = a, ζ = x)`. -/
def probTAZ (n : Fin (N + 1)) (a : A) (x : Fin N → S) : ℝ :=
  ∑ ω, if (E.α ω = a ∧ E.τ ω = n) ∧ E.ζ ω = x then E.P ω else 0

/-- **Definition 2** (p.12): `τ` is independent of the pair `(α, ζ)`. -/
def Restricted : Prop :=
  ∀ (n : Fin (N + 1)) (a : A) (x : Fin N → S), E.probTAZ n a x = E.probT n * E.probAZ a x

/-- `ℙ(τ = n, ζᵢ = xᵢ for i < n)`, the conditioning event of the instance `(n, x)`. -/
def evProb (n : Fin (N + 1)) (x : Fin N → S) : ℝ :=
  ∑ ω, if E.τ ω = n ∧ agreeBelow n x (E.ζ ω) then E.P ω else 0

/-- `ℙ(α = a, τ = n, ζᵢ = xᵢ for i < n)`. -/
def jointProb (n : Fin (N + 1)) (x : Fin N → S) (a : A) : ℝ :=
  ∑ ω, if (E.α ω = a ∧ E.τ ω = n) ∧ agreeBelow n x (E.ζ ω) then E.P ω else 0

/-- `ℙ(α = a | τ = n, ζᵢ = xᵢ for i < n)`. -/
noncomputable def condProb (n : Fin (N + 1)) (x : Fin N → S) (a : A) : ℝ :=
  E.jointProb n x a / E.evProb n x

/-- **Definition 3** (p.12): every conditioning event has positive probability,
and `σ(n, x)` is the unique maximizer of the conditional law of `α` on it. -/
structure Explains (σ : Obs S A N) : Prop where
  pos : ∀ (n : Fin (N + 1)) (x : Fin N → S), 0 < E.evProb n x
  argmax : ∀ (n : Fin (N + 1)) (x : Fin N → S) (a : A), a ≠ σ.f n x →
    E.condProb n x a < E.condProb n x (σ.f n x)

/-- `ℙ(ζᵢ = xᵢ for i < n)`. -/
def prefProb (n : ℕ) (x : Fin N → S) : ℝ :=
  ∑ ω, if agreeBelow n x (E.ζ ω) then E.P ω else 0

/-- `ℙ(α = a, ζᵢ = xᵢ for i < n)`. -/
def prefJoint (n : ℕ) (x : Fin N → S) (a : A) : ℝ :=
  ∑ ω, if E.α ω = a ∧ agreeBelow n x (E.ζ ω) then E.P ω else 0

/-- The restricted posterior `ℙ(α = a | ζᵢ = xᵢ for i < n)`, the `p_s[a]` of the
proof of Theorem 1 (p.15). -/
noncomputable def post (n : ℕ) (x : Fin N → S) (a : A) : ℝ :=
  E.prefJoint n x a / E.prefProb n x

theorem probT_nonneg (n : Fin (N + 1)) : 0 ≤ E.probT n :=
  Finset.sum_nonneg fun ω _ => by split_ifs <;> [exact E.nonneg ω; exact le_rfl]

theorem prefProb_nonneg (n : ℕ) (x : Fin N → S) : 0 ≤ E.prefProb n x :=
  Finset.sum_nonneg fun ω _ => by split_ifs <;> [exact E.nonneg ω; exact le_rfl]

/-- Summing over the fibres of `ζ`: for any condition `R` on outcomes and `Q` on
realizations, `ℙ(R, Q(ζ)) = ∑_{z : Q z} ℙ(R, ζ = z)`. -/
theorem sum_fibre (R : Ω → Prop) [DecidablePred R] (Q : (Fin N → S) → Prop)
    [DecidablePred Q] :
    (∑ ω, if R ω ∧ Q (E.ζ ω) then E.P ω else 0) =
      ∑ z, if Q z then (∑ ω, if R ω ∧ E.ζ ω = z then E.P ω else 0) else 0 := by
  have h : ∀ z, (if Q z then (∑ ω, if R ω ∧ E.ζ ω = z then E.P ω else 0) else 0) =
      ∑ ω, if E.ζ ω = z then (if R ω ∧ Q z then E.P ω else 0) else 0 := by
    intro z
    by_cases hq : Q z
    · simp only [hq, if_true, and_true]
      refine Finset.sum_congr rfl fun ω _ => ?_
      by_cases h1 : E.ζ ω = z <;> by_cases h2 : R ω <;> simp [h1, h2]
    · simp only [hq, if_false, and_false]
      simp
  rw [Finset.sum_congr rfl fun z _ => h z, Finset.sum_comm]
  refine Finset.sum_congr rfl fun ω _ => ?_
  rw [Finset.sum_ite_eq]
  simp

/-- **Remark 1, the factorization.**  Under Definition 2,
`ℙ(α = a, τ = n, ζ|n = x|n) = ℙ(τ = n) · ℙ(α = a, ζ|n = x|n)`. -/
theorem jointProb_restricted (hR : E.Restricted) (n : Fin (N + 1)) (x : Fin N → S) (a : A) :
    E.jointProb n x a = E.probT n * E.prefJoint n.val x a := by
  unfold jointProb prefJoint
  rw [E.sum_fibre (fun ω => E.α ω = a ∧ E.τ ω = n) (agreeBelow n x),
    E.sum_fibre (fun ω => E.α ω = a) (agreeBelow n x), Finset.mul_sum]
  refine Finset.sum_congr rfl fun z _ => ?_
  split_ifs
  · have := hR n a z
    unfold probTAZ probAZ at this
    rw [this]
  · simp

/-- The same factorization for the conditioning event itself. -/
theorem evProb_restricted (hR : E.Restricted) (n : Fin (N + 1)) (x : Fin N → S) :
    E.evProb n x = E.probT n * E.prefProb n.val x := by
  have hsplit : ∀ (T : Ω → Prop) [DecidablePred T],
      (∑ ω, if T ω then E.P ω else 0) = ∑ a, ∑ ω, if E.α ω = a ∧ T ω then E.P ω else 0 := by
    intro T _
    rw [Finset.sum_comm]
    refine Finset.sum_congr rfl fun ω _ => ?_
    rw [Finset.sum_eq_single (E.α ω)]
    · simp
    · intro b _ hb; simp [Ne.symm hb]
    · simp
  have h1 : E.evProb n x = ∑ a, E.jointProb n x a := by
    unfold evProb jointProb
    rw [hsplit]
    refine Finset.sum_congr rfl fun a _ => Finset.sum_congr rfl fun ω _ => ?_
    simp only [and_assoc]
  have h2 : E.prefProb n.val x = ∑ a, E.prefJoint n.val x a := by
    unfold prefProb prefJoint
    exact hsplit _
  rw [h1, h2, Finset.mul_sum]
  exact Finset.sum_congr rfl fun a _ => E.jointProb_restricted hR n x a

/-- **Remark 1, eq. (2)** (p.13): under Definition 2 the conditional law given
`{τ = n, ζ|n = x|n}` is the conditional law given `{ζ|n = x|n}`. -/
theorem remark1 (hR : E.Restricted) (n : Fin (N + 1)) (x : Fin N → S) (a : A)
    (hpos : 0 < E.evProb n x) : E.condProb n x a = E.post n.val x a := by
  have hT : E.probT n ≠ 0 := by
    intro h0; rw [E.evProb_restricted hR, h0, zero_mul] at hpos; exact lt_irrefl _ hpos
  unfold condProb post
  rw [E.jointProb_restricted hR, E.evProb_restricted hR]
  exact mul_div_mul_left _ _ hT

/-- A child `x[i ↦ s]` of length `i+1` agrees with `y` iff the parent of length
`i` does and `y i = s`. -/
theorem agreeBelow_succ_update (x y : Fin N → S) (i : Fin N) (s : S) :
    agreeBelow ((i : ℕ) + 1) (Function.update x i s) y ↔ agreeBelow i x y ∧ s = y i := by
  constructor
  · intro h
    refine ⟨fun j hj => ?_, ?_⟩
    · have hne : j ≠ i := fun e => by subst e; exact lt_irrefl _ hj
      have := h j (by omega)
      rwa [Function.update_of_ne hne] at this
    · have := h i (by omega)
      rwa [Function.update_self] at this
  · rintro ⟨h, hs⟩ j hj
    by_cases e : j = i
    · subst e; rw [Function.update_self]; exact hs
    · rw [Function.update_of_ne e]
      exact h j (by
        have : (j : ℕ) ≠ i := fun h' => e (Fin.ext h')
        omega)

/-- **The parent event is the disjoint union of its children's**, for any
condition `R` on outcomes. -/
theorem sum_split_children (R : Ω → Prop) [DecidablePred R] (x : Fin N → S) (i : Fin N) :
    (∑ ω, if R ω ∧ agreeBelow i x (E.ζ ω) then E.P ω else 0) =
      ∑ s, ∑ ω, if R ω ∧ agreeBelow ((i : ℕ) + 1) (Function.update x i s) (E.ζ ω)
        then E.P ω else 0 := by
  rw [Finset.sum_comm]
  refine Finset.sum_congr rfl fun ω _ => ?_
  rw [Finset.sum_eq_single (E.ζ ω i)]
  · simp only [agreeBelow_succ_update, and_true]
  · intro s _ hs
    simp [agreeBelow_succ_update, hs]
  · simp

/-- `ℙ(α = a, ζ|n = x|n) = ∑_s ℙ(α = a, ζ|n+1 = (x|n)^s)`. -/
theorem prefJoint_split (x : Fin N → S) (i : Fin N) (a : A) :
    E.prefJoint i x a = ∑ s, E.prefJoint ((i : ℕ) + 1) (Function.update x i s) a :=
  E.sum_split_children (fun ω => E.α ω = a) x i

/-- `ℙ(ζ|n = x|n) = ∑_s ℙ(ζ|n+1 = (x|n)^s)`. -/
theorem prefProb_split (x : Fin N → S) (i : Fin N) :
    E.prefProb i x = ∑ s, E.prefProb ((i : ℕ) + 1) (Function.update x i s) := by
  have := E.sum_split_children (fun _ => True) x i
  simpa [prefProb] using this

/-- From Definition 3: comparing conditional probabilities on a positive event is
comparing joint probabilities. -/
theorem Explains.joint_lt {σ : Obs S A N} (h : E.Explains σ) (n : Fin (N + 1))
    (x : Fin N → S) {a : A} (ha : a ≠ σ.f n x) :
    E.jointProb n x a < E.jointProb n x (σ.f n x) :=
  (div_lt_div_iff_of_pos_right (h.pos n x)).1 (h.argmax n x a ha)

/-- Under Definitions 2 and 3 every prefix event has positive probability. -/
theorem prefProb_pos {σ : Obs S A N} (hR : E.Restricted) (h : E.Explains σ)
    (n : Fin (N + 1)) (x : Fin N → S) : 0 < E.prefProb n.val x := by
  have hp := h.pos n x
  rw [E.evProb_restricted hR] at hp
  exact pos_of_mul_pos_right hp (E.probT_nonneg n)

/-- **Eq. (4) as used on p.15.**  If a restricted conjectured experiment explains
`σ`, then at every instance `(n, x)` with `n < N` the posterior is a convex
combination of the children's posteriors, with strictly positive weights
`λ_s = ℙ(ζ|n+1 = (x|n)^s) / ℙ(ζ|n = x|n)` summing to `1`. -/
theorem posterior_convex_combination {σ : Obs S A N} (hR : E.Restricted) (h : E.Explains σ)
    (n : Fin (N + 1)) (hn : (n : ℕ) < N) (x : Fin N → S) :
    ∃ w : S → ℝ, (∀ s, 0 < w s) ∧ ∑ s, w s = 1 ∧
      ∀ a, E.post n x a = ∑ s, w s * E.post (n + 1) (Function.update x ⟨n, hn⟩ s) a := by
  set i : Fin N := ⟨n, hn⟩
  have hpp : 0 < E.prefProb n x := E.prefProb_pos hR h n x
  have hpc : ∀ s, 0 < E.prefProb (n + 1) (Function.update x i s) := fun s =>
    E.prefProb_pos hR h ⟨n + 1, by omega⟩ (Function.update x i s)
  refine ⟨fun s => E.prefProb (n + 1) (Function.update x i s) / E.prefProb n x,
    fun s => div_pos (hpc s) hpp, ?_, fun a => ?_⟩
  · rw [← Finset.sum_div, ← (E.prefProb_split x i : _), div_self hpp.ne']
  · unfold post
    rw [(E.prefJoint_split x i a : _), Finset.sum_div]
    refine Finset.sum_congr rfl fun s _ => ?_
    have := (hpc s).ne'
    show E.prefJoint (n + 1) (Function.update x i s) a / E.prefProb n x = _
    field_simp

end ConjExp

section TheoremOne

variable {Ω : Type*} [Fintype Ω] [Fintype S] [DecidableEq S] [Fintype A] [DecidableEq A]
  (E : ConjExp Ω S A N)

/-- The generic step: if the parent's score `f` is a nonnegative combination of
the children's scores `g x`, an alternative maximal at every child is maximal at
the parent. -/
theorem argmax_of_convex_combination {ι : Type*} [Fintype ι]
    (p : ι → ℝ) (hp : ∀ x, 0 ≤ p x)
    (f : A → ℝ) (g : ι → A → ℝ)
    (hf : ∀ b, f b = ∑ x, p x * g x b)
    (a : A) (hmax : ∀ x, ∀ b, g x b ≤ g x a) :
    ∀ b, f b ≤ f a := by
  intro b
  rw [hf b, hf a]
  exact Finset.sum_le_sum fun x _ => mul_le_mul_of_nonneg_left (hmax x b) (hp x)

/-- **Theorem 1, necessity** (p.13; proof p.15).  If a restricted conjectured
experiment (Definition 2) explains `σ` (Definition 3), then `σ` never reverses:
if `σ(s^s) = a` for every `s ∈ S`, then `σ(s) = a`.

The proof: were `σ(s) = b ≠ a`, the parent's argmax gives
`ℙ(α = a, ζ|n) < ℙ(α = b, ζ|n)` after the length factors out
(`jointProb_restricted`), each child's argmax gives the reverse strict inequality
on the child's event, and the parent event is the disjoint union of the
children's (`prefJoint_split`). -/
theorem no_reversal_of_restricted {σ : Obs S A N} (hR : E.Restricted) (h : E.Explains σ) :
    NoReversal σ := by
  intro n hn x a hchild
  by_contra hne
  set i : Fin N := ⟨n, hn⟩
  set b := σ.f n x
  -- the parent: `a` loses to `b`
  have hpar : E.prefJoint n x a < E.prefJoint n x b := by
    have := ConjExp.Explains.joint_lt E h n x (a := a) (Ne.symm hne)
    rw [ConjExp.jointProb_restricted E hR, ConjExp.jointProb_restricted E hR] at this
    exact lt_of_mul_lt_mul_left this (ConjExp.probT_nonneg E n)
  -- each child: `b` loses to `a`
  have hch : ∀ s, E.prefJoint (n + 1) (Function.update x i s) b ≤
      E.prefJoint (n + 1) (Function.update x i s) a := by
    intro s
    have hc := hchild s
    have hba : b ≠ σ.f ⟨n + 1, by omega⟩ (Function.update x i s) := by rw [hc]; exact hne
    have := ConjExp.Explains.joint_lt E h ⟨n + 1, by omega⟩ (Function.update x i s) hba
    rw [hc, ConjExp.jointProb_restricted E hR, ConjExp.jointProb_restricted E hR] at this
    exact (lt_of_mul_lt_mul_left this (ConjExp.probT_nonneg E _)).le
  have hsum : E.prefJoint n x b ≤ E.prefJoint n x a := by
    rw [ConjExp.prefJoint_split E x i b, ConjExp.prefJoint_split E x i a]
    exact Finset.sum_le_sum fun s _ => hch s
  exact absurd hpar (not_lt.2 hsum)

end TheoremOne

/-! ## Theorem 2: anything goes -/

/-- The canonical sample space: a full signal realization together with the
length observed. -/
abbrev Omega (S : Type*) (N : ℕ) := (Fin N → S) × Fin (N + 1)

variable [Fintype S] [DecidableEq S] [Fintype A] [DecidableEq A]

/-- The canonical conditioning event of `(n, x)` on `Omega`:
`{τ = n} ∩ {ζᵢ = xᵢ for i < n}`. -/
def event (n : Fin (N + 1)) (x : Fin N → S) : Finset (Omega S N) :=
  Finset.univ.filter fun ω => ω.2 = n ∧ agreeBelow (n : ℕ) x ω.1

theorem self_mem_event (n : Fin (N + 1)) (x : Fin N → S) : (x, n) ∈ event n x := by
  simp [event, agreeBelow_refl]

/-- The construction: `α` reads the observations off the observed prefix. -/
def alphaOf (σ : Obs S A N) (ω : Omega S N) : A := σ.f ω.2 ω.1

/-- **The heart of Theorem 2.**  On the conditioning event of `(n, x)` the
constructed `α` is *constant*, equal to `σ (n, x)`. -/
theorem alpha_const_on_event (σ : Obs S A N) {n : Fin (N + 1)} {x : Fin N → S}
    {ω : Omega S N} (hω : ω ∈ event n x) : alphaOf σ ω = σ.f n x := by
  simp only [event, Finset.mem_filter] at hω
  obtain ⟨-, hlen, hagree⟩ := hω
  unfold alphaOf
  rw [hlen]
  exact (σ.prefix_inv n x ω.1 hagree).symm

variable [Nonempty S]

theorem card_Omega_pos : 0 < (Fintype.card (Omega S N) : ℝ) := by
  exact_mod_cast Fintype.card_pos

/-- The canonical conjectured experiment: uniform probability on `Omega`,
`τ`, `ζ` the projections, `α = alphaOf σ`. -/
noncomputable def canonical (σ : Obs S A N) : ConjExp (Omega S N) S A N where
  P := fun _ => 1 / (Fintype.card (Omega S N) : ℝ)
  nonneg := fun _ => (one_div_pos.2 card_Omega_pos).le
  total := by
    rw [Finset.sum_const, Finset.card_univ, nsmul_eq_mul, mul_one_div,
      div_self card_Omega_pos.ne']
  α := alphaOf σ
  τ := Prod.snd
  ζ := Prod.fst

/-- Hence every alternative other than `σ (n, x)` has joint probability zero on
the event: the conditional law of `α` is a point mass. -/
theorem jointWt_of_ne (σ : Obs S A N) (n : Fin (N + 1)) (x : Fin N → S) {a : A}
    (ha : a ≠ σ.f n x) : (canonical σ).jointProb n x a = 0 := by
  unfold ConjExp.jointProb
  refine Finset.sum_eq_zero fun ω _ => ?_
  rw [if_neg]
  rintro ⟨⟨hα, hτ⟩, hag⟩
  have hmem : ω ∈ event n x := by simp [event]; exact ⟨hτ, hag⟩
  exact ha (hα.symm.trans (alpha_const_on_event σ hmem))

/-- ... and `σ (n, x)` itself carries the whole probability of the event. -/
theorem jointWt_self (σ : Obs S A N) (n : Fin (N + 1)) (x : Fin N → S) :
    (canonical σ).jointProb n x (σ.f n x) = (canonical σ).evProb n x := by
  unfold ConjExp.jointProb ConjExp.evProb
  refine Finset.sum_congr rfl fun ω _ => ?_
  by_cases hω : (canonical σ).τ ω = n ∧ agreeBelow n x ((canonical σ).ζ ω)
  · have hmem : ω ∈ event n x := by simp [event]; exact hω
    have hα : (canonical σ).α ω = σ.f n x := alpha_const_on_event σ hmem
    simp [hα, hω]
  · rw [if_neg hω, if_neg (fun h => hω ⟨h.1.2, h.2⟩)]

theorem canonical_evProb_pos (σ : Obs S A N) (n : Fin (N + 1)) (x : Fin N → S) :
    0 < (canonical σ).evProb n x := by
  unfold ConjExp.evProb
  refine lt_of_lt_of_le ?_ (Finset.single_le_sum (f := fun ω =>
    if (canonical σ).τ ω = n ∧ agreeBelow n x ((canonical σ).ζ ω) then (canonical σ).P ω else 0)
    (fun ω _ => by split_ifs <;> [exact (canonical σ).nonneg ω; exact le_rfl])
    (Finset.mem_univ (x, n)))
  have : (canonical σ).τ (x, n) = n ∧ agreeBelow n x ((canonical σ).ζ (x, n)) :=
    ⟨rfl, agreeBelow_refl _ _⟩
  rw [if_pos this]
  exact one_div_pos.2 card_Omega_pos

/-- **Theorem 2 ("anything goes")** (p.18).  Every prefix-invariant observation
map `σ` is explained (Definition 3) by an unrestricted conjectured experiment:
the uniform probability on `Omega`, with `α` the observations read off the
observed prefix. -/
theorem anythingGoes (σ : Obs S A N) :
    ∃ E : ConjExp (Omega S N) S A N, E.Explains σ := by
  refine ⟨canonical σ, ⟨canonical_evProb_pos σ, fun n x a ha => ?_⟩⟩
  unfold ConjExp.condProb
  rw [jointWt_of_ne σ n x ha, jointWt_self, zero_div, div_self (canonical_evProb_pos σ n x).ne']
  exact one_pos

/-- **The canonical construction is not restricted** whenever `σ` reports
differently at two lengths on the same realization: Definition 2 fails.  (With
`a = σ(n, x)`, `ℙ(τ = n, α = a, ζ = x) > 0` but `ℙ(τ = m, α = a, ζ = x) = 0`
while `ℙ(τ = m) > 0`.) -/
theorem alpha_depends_on_nu (σ : Obs S A N) (n m : Fin (N + 1)) (x : Fin N → S)
    (h : σ.f n x ≠ σ.f m x) : ¬ (canonical σ).Restricted := by
  intro hR
  set E := canonical σ
  set K := (Fintype.card (Omega S N) : ℝ)
  have hK : 0 < 1 / K := one_div_pos.2 card_Omega_pos
  -- ℙ(τ = n, α = σ(n,x), ζ = x) = 1/K
  have hn : E.probTAZ n (σ.f n x) x = 1 / K := by
    unfold ConjExp.probTAZ
    rw [Finset.sum_eq_single (x, n)]
    · rw [if_pos ⟨⟨rfl, rfl⟩, rfl⟩]; rfl
    · rintro ⟨y, k⟩ _ hne
      rw [if_neg]
      rintro ⟨⟨-, hk⟩, hy⟩
      exact hne (Prod.ext hy hk)
    · simp
  -- ℙ(τ = m, α = σ(n,x), ζ = x) = 0
  have hm : E.probTAZ m (σ.f n x) x = 0 := by
    unfold ConjExp.probTAZ
    refine Finset.sum_eq_zero fun ω _ => ?_
    rw [if_neg]
    rintro ⟨⟨hα, hk⟩, hy⟩
    have : E.α ω = σ.f m x := by
      show σ.f ω.2 ω.1 = _
      rw [show ω.2 = m from hk, show ω.1 = x from hy]
    exact h (hα.symm.trans this)
  -- ℙ(τ = m) > 0
  have hTm : 0 < E.probT m := by
    unfold ConjExp.probT
    refine lt_of_lt_of_le ?_ (Finset.single_le_sum (f := fun ω =>
      if E.τ ω = m then E.P ω else 0)
      (fun ω _ => by split_ifs <;> [exact E.nonneg ω; exact le_rfl]) (Finset.mem_univ (x, m)))
    simp only [if_pos (show E.τ (x, m) = m from rfl)]
    exact hK
  have h1 := hR m (σ.f n x) x
  rw [hm] at h1
  have hAZ : E.probAZ (σ.f n x) x = 0 := by
    rcases mul_eq_zero.1 h1.symm with h0 | h0
    · exact absurd h0 hTm.ne'
    · exact h0
  have h2 := hR n (σ.f n x) x
  rw [hn, hAZ, mul_zero] at h2
  exact hK.ne' h2

/-! ## The two theorems side by side -/

/-- A reversing `σ` with one binary signal: the root reports `false`, both
one-signal instances report `true`. -/
def reversing : Obs Bool Bool 1 where
  f := fun n _ => decide ((n : ℕ) = 1)
  prefix_inv := fun _ _ _ _ => rfl

/-- **The contrast between Theorems 1 and 2.**  The reversing `σ` is explained by
the unrestricted canonical experiment, and by no restricted conjectured
experiment on any finite probability space. -/
theorem reversal_example :
    (canonical reversing).Explains reversing ∧
      ∀ (Ω : Type) [Fintype Ω] (E : ConjExp Ω Bool Bool 1),
        ¬ (E.Restricted ∧ E.Explains reversing) := by
  refine ⟨⟨canonical_evProb_pos reversing, fun n x a ha => ?_⟩, fun Ω _ E ⟨hR, hE⟩ => ?_⟩
  · unfold ConjExp.condProb
    rw [jointWt_of_ne reversing n x ha, jointWt_self, zero_div,
      div_self (canonical_evProb_pos reversing n x).ne']
    exact one_pos
  · have := no_reversal_of_restricted E hR hE 0 (by decide) (fun _ => false) true
      (fun s => by simp [reversing])
    simp [reversing] at this

end Literature.ShmayaYariv
