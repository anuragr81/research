/-
# Dietrich (2021), "Fully Bayesian Aggregation"

*Journal of Economic Theory* 194, 105255. The Drive copy is the HAL version
hal-03194928, "extended version of January 2021". Page numbers below are that
version's printed pages.

This file formalizes the paper's belief-pooling results first. It then examines,
in a separately labelled part, a question that belongs to Paper B and not to
Dietrich: can Paper B's population mean be evaluated against Dietrich's
criterion at all?

## The paper's objects (Section 4 and Appendix A, pp. 11, 16-17)

A finite state space `Ω` and a finite group `ι` of individuals. A belief profile
is `p : ι → Ω → ℝ`. A belief aggregation (opinion pooling) rule maps profiles to
a group belief.

* **Geometric pooling (App. A, p. 16):** weights `wᵢ ≥ 0` with `∑ wᵢ = 1`, and
  `G(p)(ω) ∝ ∏ᵢ pᵢ(ω)^{wᵢ}`, with the convention `0⁰ = 1` (fn. 12). Mathlib's
  `Real.rpow` has `0 ^ 0 = 1`.
* **Linear pooling:** `L(p)(ω) = ∑ᵢ wᵢ pᵢ(ω)`, which is the belief part of the
  "linear-linear" rules (p. 7).
* **Dynamic Rationality (p. 8; belief form p. 16):** if `p'ᵢ = pᵢ(·|E)` for all
  `i`, then `F(p') = F(p)(·|E)`. That is, pooling commutes with conditioning on
  a *common event*.
* **External Bayesianity (p. 15):** the same, with conditioning on a common
  *likelihood function* in place of an event. Dietrich cites it as "well-known"
  and does not prove it.

**Theorem 2 (p. 11):** a belief aggregation rule on the coherent profiles is
dynamically rational, unanimity-preserving and continuous iff it is geometric.
The theorem excludes `|Ω| = 2` (p. 6).

## What is proved from the paper

* `geoPool_dynRational`: **Theorem 2, Part 1 (necessity, p. 18).** Geometric
  pooling commutes with conditioning on any event that the profile is coherent
  on. The proof follows Dietrich's: the conditioned unnormalized pool is the
  restricted pool times a positive constant, and states outside `E` get
  `∏ 0^{wᵢ} = 0` because some `wᵢ > 0`.
* `geoPool_unanimous`: geometric pooling is unanimity-preserving.
* `geoPool_externallyBayesian`: geometric pooling commutes with any common
  positive likelihood (p. 15, "as is well-known").
* `linPool_cond_sub`: an exact identity for two individuals.
  `L(p|E)(ω) − L(p)(ω|E) = λ(1−λ)(p₂(E)−p₁(E))(p₁(ω)/p₁(E) − p₂(ω)/p₂(E)) / (λp₁(E)+(1−λ)p₂(E))`.
  So linear pooling commutes with conditioning only in the degenerate cases
  (dictatorship, equal `pᵢ(E)`, or equal conditionals). That is the precise
  sense of "generically fails", and it matches fn. 7, p. 6.
* `duel_linear_not_dynRational`: the paper's own counterexample, the duel of
  Table 1 (p. 5). The linear pool gives `13/18` where Bayes requires `5/6`.

Not formalized: the sufficiency half of Theorem 2 (Claims 1-11, pp. 18-20, which
need Cauchy's equation and continuity), and Theorems 1 and 1⁺ on preference
aggregation.

## The project's question (not Dietrich's): the population mean

In Paper B every evaluator holds the **same prior** `P` and receives the same
two cues. The evaluators differ only in reading order, so the post-update
profile is `(P^J_AB, P^J_BA)` and the population mean is
`λ P^J_AB + (1−λ) P^J_BA`. What follows is proved:

* `premise_forces_unanimity`: under a common prior, the premise of Dynamic
  Rationality (each member conditions on the same `E`) forces every posterior to
  coincide. `eb_premise_forces_unanimity` shows the same for a common
  likelihood (External Bayesianity).
* `dynRational_on_common_prior` and `eb_on_common_prior`: consequently, **every
  unanimity-preserving rule, and linear pooling in particular, satisfies both
  criteria on every common-prior instance where the premise holds.** On Paper
  B's domain the criterion cannot tell linear from geometric pooling.
* `not_cond_of_fullSupport`: a full-support posterior that differs from the
  prior is *not* the conditionalisation of the prior on any event. Jeffrey
  posteriors with interior credences have full support. So the step
  `P ↦ P^J_AB` is not a Dietrich-type update, and neither is `P ↦ P^J_BA`.
  Their common-likelihood reading needs `P^J_AB = P^J_BA`, which fails
  generically (Paper B, Prop. DIV).

Hence "the population mean … is the outcome of linear pooling and generically
fails the criterion" is not a statement Dietrich's criterion can evaluate. On
common-prior profiles linear pooling never fails the criterion, and the
profile `(P^J_AB, P^J_BA)` never meets its premise. The sympy script
`check_dietrich.py` adds the quantitative comparison. Against the Bayes-factor
benchmark `P^B`, the linear and the geometric pool of `(P^J_AB, P^J_BA)` both
miss by `Θ(c)` with the *same* first-order term, and they differ from each other
only at `O(c²)`.
-/
import Mathlib

namespace Literature.Dietrich

open Finset

set_option linter.unusedSectionVars false

variable {Ω ι : Type*} [Fintype Ω] [DecidableEq Ω] [Fintype ι]

/-! ## Beliefs, conditioning, pooling -/

/-- `f` restricted to `E` (set to zero outside). -/
def restrict (E : Finset Ω) (f : Ω → ℝ) : Ω → ℝ := fun ω => if ω ∈ E then f ω else 0

/-- Normalisation to total mass one. -/
noncomputable def normalize (f : Ω → ℝ) : Ω → ℝ := fun ω => f ω / ∑ ω', f ω'

/-- Conditioning on the event `E`: `f(·|E)`. -/
noncomputable def cond (f : Ω → ℝ) (E : Finset Ω) : Ω → ℝ := normalize (restrict E f)

/-- Bayesian revision on a likelihood function `L` (p. 15). -/
noncomputable def bayesL (f L : Ω → ℝ) : Ω → ℝ := normalize (fun ω => f ω * L ω)

/-- Linear pooling with weights `w`. -/
def linPool (w : ι → ℝ) (p : ι → Ω → ℝ) : Ω → ℝ := fun ω => ∑ i, w i * p i ω

/-- The unnormalized geometric pool `∏ᵢ pᵢ(ω)^{wᵢ}`. -/
noncomputable def geoW (w : ι → ℝ) (p : ι → Ω → ℝ) : Ω → ℝ := fun ω => ∏ i, p i ω ^ w i

/-- **Geometric pooling** (App. A, p. 16). -/
noncomputable def geoPool (w : ι → ℝ) (p : ι → Ω → ℝ) : Ω → ℝ := normalize (geoW w p)

/-- A probability function on `Ω`. -/
def IsProb (f : Ω → ℝ) : Prop := (∀ ω, 0 ≤ f ω) ∧ ∑ ω, f ω = 1

/-- Pooling weights: nonnegative and summing to one. -/
def IsWeights (w : ι → ℝ) : Prop := (∀ i, 0 ≤ w i) ∧ ∑ i, w i = 1

/-! ## Normalisation lemmas -/

theorem normalize_smul (f : Ω → ℝ) {c : ℝ} (hc : c ≠ 0) :
    normalize (fun ω => c * f ω) = normalize f := by
  funext ω
  simp only [normalize, ← Finset.mul_sum]
  exact mul_div_mul_left _ _ hc

theorem normalize_of_sum_one {f : Ω → ℝ} (h : ∑ ω, f ω = 1) : normalize f = f := by
  funext ω; simp [normalize, h]

theorem restrict_smul (E : Finset Ω) (f : Ω → ℝ) (c : ℝ) :
    restrict E (fun ω => c * f ω) = fun ω => c * restrict E f ω := by
  funext ω; simp only [restrict]; split_ifs <;> simp

/-- Conditioning a normalized function is conditioning the function itself. -/
theorem cond_normalize {f : Ω → ℝ} (E : Finset Ω) (hf : ∑ ω, f ω ≠ 0) :
    cond (normalize f) E = cond f E := by
  unfold cond
  have : restrict E (normalize f) = fun ω => (∑ ω', f ω')⁻¹ * restrict E f ω := by
    rw [← restrict_smul]; congr 1; funext ω; simp [normalize, div_eq_inv_mul]
  rw [this, normalize_smul _ (inv_ne_zero hf)]

theorem sum_restrict (E : Finset Ω) (f : Ω → ℝ) : ∑ ω, restrict E f ω = ∑ ω ∈ E, f ω := by
  simp [restrict, Finset.sum_ite_mem]

/-! ## Theorem 2, Part 1: geometric pooling is dynamically rational -/

theorem geoW_nonneg {w : ι → ℝ} {p : ι → Ω → ℝ} (hp : ∀ i ω, 0 ≤ p i ω) (ω : Ω) :
    0 ≤ geoW w p ω :=
  Finset.prod_nonneg fun i _ => Real.rpow_nonneg (hp i ω) _

theorem geoW_pos {w : ι → ℝ} {p : ι → Ω → ℝ} {ω : Ω} (h : ∀ i, 0 < p i ω) : 0 < geoW w p ω :=
  Finset.prod_pos fun i _ => Real.rpow_pos_of_pos (h i) _

theorem sum_geoW_pos {w : ι → ℝ} {p : ι → Ω → ℝ} (hp : ∀ i ω, 0 ≤ p i ω)
    {ω₀ : Ω} (h₀ : ∀ i, 0 < p i ω₀) : 0 < ∑ ω, geoW w p ω :=
  Finset.sum_pos' (fun ω _ => geoW_nonneg hp ω) ⟨ω₀, Finset.mem_univ _, geoW_pos h₀⟩

/-- The conditioned profile's unnormalized geometric pool is the restricted pool
times the positive constant `(∏ᵢ pᵢ(E)^{wᵢ})⁻¹`. -/
theorem geoW_cond {w : ι → ℝ} (hw : IsWeights w) {p : ι → Ω → ℝ} (hp : ∀ i ω, 0 ≤ p i ω)
    (E : Finset Ω) :
    geoW w (fun i => cond (p i) E) =
      fun ω => (∏ i, (∑ ω' ∈ E, p i ω') ^ w i)⁻¹ * restrict E (geoW w p) ω := by
  funext ω
  have hZ : ∀ i, 0 ≤ ∑ ω' ∈ E, p i ω' := fun i => Finset.sum_nonneg fun ω' _ => hp i ω'
  simp only [geoW, cond, normalize, sum_restrict]
  by_cases hω : ω ∈ E
  · simp only [restrict, hω, if_true]
    rw [Finset.prod_congr rfl fun i _ => Real.div_rpow (hp i ω) (hZ i) (w i),
      Finset.prod_div_distrib]
    simp only [geoW]
    ring
  · simp only [restrict, hω, if_false, zero_div, mul_zero]
    -- some weight is nonzero, since the weights sum to one
    obtain ⟨j, -, hj⟩ : ∃ j ∈ (Finset.univ : Finset ι), w j ≠ 0 := by
      by_contra hcon
      push Not at hcon
      have := hw.2
      rw [Finset.sum_eq_zero hcon] at this
      norm_num at this
    exact Finset.prod_eq_zero (Finset.mem_univ j) (Real.zero_rpow hj)

/-- **Theorem 2, Part 1 (Dynamic Rationality of geometric pooling, p. 18).**
Let every individual condition on the same event `E`, and let the conditioned
profile be coherent, with some `ω₀ ∈ E` that everyone gives positive
probability. Then the pool of the conditioned beliefs is the conditioned pool. -/
theorem geoPool_dynRational {w : ι → ℝ} (hw : IsWeights w) {p : ι → Ω → ℝ}
    (hp : ∀ i ω, 0 ≤ p i ω) (E : Finset Ω) {ω₀ : Ω} (hω₀ : ω₀ ∈ E)
    (h₀ : ∀ i, 0 < p i ω₀) :
    geoPool w (fun i => cond (p i) E) = cond (geoPool w p) E := by
  have hZ : ∀ i, 0 < ∑ ω' ∈ E, p i ω' := fun i =>
    Finset.sum_pos' (fun ω' _ => hp i ω') ⟨ω₀, hω₀, h₀ i⟩
  have hC : (∏ i, (∑ ω' ∈ E, p i ω') ^ w i) ≠ 0 :=
    (Finset.prod_pos fun i _ => Real.rpow_pos_of_pos (hZ i) _).ne'
  unfold geoPool
  rw [geoW_cond hw hp E, normalize_smul _ (inv_ne_zero hC),
    cond_normalize E (sum_geoW_pos hp h₀).ne']
  rfl

/-- Geometric pooling preserves unanimity. -/
theorem geoPool_unanimous {w : ι → ℝ} (hw : IsWeights w) {π : Ω → ℝ} (hπ : IsProb π) :
    geoPool w (fun _ => π) = π := by
  have : geoW w (fun _ => π) = π := by
    funext ω
    simp only [geoW]
    rw [← Real.rpow_sum_of_nonneg (hπ.1 ω) (fun i _ => hw.1 i), hw.2, Real.rpow_one]
  rw [geoPool, this, normalize_of_sum_one hπ.2]

/-- **External Bayesianity of geometric pooling** (p. 15, "as is well-known"):
with a common positive likelihood `L`, the pool of the revised beliefs is the
revised pool. -/
theorem geoPool_externallyBayesian {w : ι → ℝ} (hw : IsWeights w) {p : ι → Ω → ℝ}
    (hp : ∀ i ω, 0 ≤ p i ω) {ω₀ : Ω} (h₀ : ∀ i, 0 < p i ω₀) {L : Ω → ℝ}
    (hL : ∀ ω, 0 < L ω) :
    geoPool w (fun i => bayesL (p i) L) = bayesL (geoPool w p) L := by
  have hZ : ∀ i, 0 < ∑ ω, p i ω * L ω := fun i =>
    Finset.sum_pos' (fun ω _ => mul_nonneg (hp i ω) (hL ω).le)
      ⟨ω₀, Finset.mem_univ _, mul_pos (h₀ i) (hL ω₀)⟩
  have hC : (∏ i, (∑ ω, p i ω * L ω) ^ w i) ≠ 0 :=
    (Finset.prod_pos fun i _ => Real.rpow_pos_of_pos (hZ i) _).ne'
  have hG : geoW w (fun i => bayesL (p i) L) =
      fun ω => (∏ i, (∑ ω, p i ω * L ω) ^ w i)⁻¹ * (geoW w p ω * L ω) := by
    funext ω
    simp only [geoW, bayesL, normalize]
    rw [Finset.prod_congr rfl fun i _ =>
        Real.div_rpow (mul_nonneg (hp i ω) (hL ω).le) (hZ i).le (w i),
      Finset.prod_div_distrib,
      Finset.prod_congr rfl fun i _ => Real.mul_rpow (hp i ω) (hL ω).le,
      Finset.prod_mul_distrib, ← Real.rpow_sum_of_pos (hL ω), hw.2, Real.rpow_one]
    ring
  have hS := (sum_geoW_pos (w := w) hp h₀).ne'
  unfold geoPool
  rw [hG, normalize_smul _ (inv_ne_zero hC)]
  unfold bayesL
  have : (fun ω => normalize (geoW w p) ω * L ω) =
      fun ω => (∑ ω', geoW w p ω')⁻¹ * (geoW w p ω * L ω) := by
    funext ω; simp only [normalize]; ring
  rw [this, normalize_smul _ (inv_ne_zero hS)]

/-! ## Linear pooling generically fails -/

/-- **Two-person linear pooling: exact commutation defect.** With weights `λ`
and `1 − λ`, `aᵢ = pᵢ(E) > 0` and `ω ∈ E`, the difference between the pool of
conditionals and the conditional of the pool is
`λ(1−λ)(a₂−a₁)(p₁(ω)/a₁ − p₂(ω)/a₂) / (λa₁ + (1−λ)a₂)`. -/
theorem linPool_cond_sub {lam a₁ a₂ x₁ x₂ : ℝ} (h₁ : a₁ ≠ 0) (h₂ : a₂ ≠ 0)
    (hm : lam * a₁ + (1 - lam) * a₂ ≠ 0) :
    (lam * (x₁ / a₁) + (1 - lam) * (x₂ / a₂)) - (lam * x₁ + (1 - lam) * x₂) /
        (lam * a₁ + (1 - lam) * a₂)
      = lam * (1 - lam) * (a₂ - a₁) * (x₁ / a₁ - x₂ / a₂) / (lam * a₁ + (1 - lam) * a₂) := by
  field_simp
  ring

/-- Hence linear pooling commutes with conditioning at `ω ∈ E` only if
`λ ∈ {0,1}`, `p₁(E) = p₂(E)`, or `p₁(ω|E) = p₂(ω|E)`. -/
theorem linPool_comm_iff {lam a₁ a₂ x₁ x₂ : ℝ} (h₁ : a₁ ≠ 0) (h₂ : a₂ ≠ 0)
    (hm : lam * a₁ + (1 - lam) * a₂ ≠ 0) :
    lam * (x₁ / a₁) + (1 - lam) * (x₂ / a₂) = (lam * x₁ + (1 - lam) * x₂) /
        (lam * a₁ + (1 - lam) * a₂) ↔
      lam = 0 ∨ lam = 1 ∨ a₁ = a₂ ∨ x₁ / a₁ = x₂ / a₂ := by
  rw [← sub_eq_zero, linPool_cond_sub h₁ h₂ hm, div_eq_zero_iff, or_iff_left hm]
  simp only [mul_eq_zero, sub_eq_zero]
  constructor
  · rintro (((h | h) | h) | h)
    · exact Or.inl h
    · exact Or.inr (Or.inl h.symm)
    · exact Or.inr (Or.inr (Or.inl h.symm))
    · exact Or.inr (Or.inr (Or.inr h))
  · rintro (h | h | h | h)
    · exact Or.inl (Or.inl (Or.inl h))
    · exact Or.inl (Or.inl (Or.inr h.symm))
    · exact Or.inl (Or.inr h.symm)
    · exact Or.inr h

/-- Gentleman 1's old probabilities in Table 1 (p. 5): `(.85, .05, .1)`. -/
noncomputable def duel1 : Fin 3 → ℝ := ![85 / 100, 5 / 100, 10 / 100]
/-- Gentleman 2's old probabilities in Table 1 (p. 5): `(.15, .15, .7)`. -/
noncomputable def duel2 : Fin 3 → ℝ := ![15 / 100, 15 / 100, 70 / 100]
/-- The duel profile. -/
noncomputable def duelProfile : Fin 2 → Fin 3 → ℝ := ![duel1, duel2]
/-- Equal weights. -/
noncomputable def half : Fin 2 → ℝ := fun _ => 1 / 2
/-- The information `E = {ω₁, ω₂}` ("2 does not have a superior weapon"). -/
def duelE : Finset (Fin 3) := {0, 1}

/-- **The duel (p. 5): the linear pool is not dynamically rational.** After
learning `E`, the linear pool gives state `ω₁` probability `13/18` (Table 1:
`.72`). Conditioning the old linear pool gives `5/6` (the paper's `.83`). -/
theorem duel_linear_not_dynRational :
    linPool half (fun i => cond (duelProfile i) duelE) 0 = 13 / 18 ∧
    cond (linPool half duelProfile) duelE 0 = 5 / 6 ∧
    linPool half (fun i => cond (duelProfile i) duelE) ≠ cond (linPool half duelProfile) duelE := by
  have h1 : linPool half (fun i => cond (duelProfile i) duelE) 0 = 13 / 18 := by
    simp [linPool, cond, normalize, restrict, duelE, duelProfile, duel1, duel2, half,
      Fin.sum_univ_two, Fin.sum_univ_three]
    norm_num
  have h2 : cond (linPool half duelProfile) duelE 0 = 5 / 6 := by
    simp [linPool, cond, normalize, restrict, duelE, duelProfile, duel1, duel2, half,
      Fin.sum_univ_two, Fin.sum_univ_three]
    norm_num
  refine ⟨h1, h2, fun h => ?_⟩
  have := congrFun h 0
  rw [h1, h2] at this
  norm_num at this

/-! ## THE PROJECT'S QUESTION (not Dietrich's): the common-prior population mean -/

/-- Under a common prior, the premise of Dynamic Rationality makes the
post-information profile unanimous. -/
theorem premise_forces_unanimity {p p' : ι → Ω → ℝ} {π : Ω → ℝ} (hp : ∀ i, p i = π)
    {E : Finset Ω} (hcond : ∀ i, p' i = cond (p i) E) (i j : ι) : p' i = p' j := by
  rw [hcond, hcond, hp, hp]

/-- Under a common prior, the premise of External Bayesianity (a common
likelihood) also makes the posterior profile unanimous. -/
theorem eb_premise_forces_unanimity {p p' : ι → Ω → ℝ} {π : Ω → ℝ} (hp : ∀ i, p i = π)
    {L : Ω → ℝ} (hrev : ∀ i, p' i = bayesL (p i) L) (i j : ι) : p' i = p' j := by
  rw [hrev, hrev, hp, hp]

/-- **On common-prior profiles, every unanimity-preserving rule is dynamically
rational.** When everyone starts from `π` and conditions on `E`, the pool
returns `π(·|E)`, which is the conditioned pool. -/
theorem dynRational_on_common_prior (F : (ι → Ω → ℝ) → Ω → ℝ)
    (hU : ∀ f : Ω → ℝ, F (fun _ => f) = f) {p : ι → Ω → ℝ} {π : Ω → ℝ} (hp : ∀ i, p i = π)
    (E : Finset Ω) : F (fun i => cond (p i) E) = cond (F p) E := by
  have : p = fun _ => π := funext hp
  subst this
  rw [hU, hU]

/-- The same for External Bayesianity. -/
theorem eb_on_common_prior (F : (ι → Ω → ℝ) → Ω → ℝ)
    (hU : ∀ f : Ω → ℝ, F (fun _ => f) = f) {p : ι → Ω → ℝ} {π : Ω → ℝ} (hp : ∀ i, p i = π)
    (L : Ω → ℝ) : F (fun i => bayesL (p i) L) = bayesL (F p) L := by
  have : p = fun _ => π := funext hp
  subst this
  rw [hU, hU]

/-- Linear pooling preserves unanimity. -/
theorem linPool_unanimous {w : ι → ℝ} (hw : IsWeights w) (f : Ω → ℝ) :
    linPool w (fun _ => f) = f := by
  funext ω; simp [linPool, ← Finset.sum_mul, hw.2]

/-- **Linear pooling satisfies Dynamic Rationality on every common-prior
profile.** So the population mean of evaluators who share a prior never
"fails Dietrich's criterion" where the criterion applies. -/
theorem linPool_dynRational_on_common_prior {w : ι → ℝ} (hw : IsWeights w)
    {p : ι → Ω → ℝ} {π : Ω → ℝ} (hp : ∀ i, p i = π) (E : Finset Ω) :
    linPool w (fun i => cond (p i) E) = cond (linPool w p) E :=
  dynRational_on_common_prior (linPool w) (linPool_unanimous hw) hp E

/-- **Linear pooling is externally Bayesian on every common-prior profile.** -/
theorem linPool_eb_on_common_prior {w : ι → ℝ} (hw : IsWeights w)
    {p : ι → Ω → ℝ} {π : Ω → ℝ} (hp : ∀ i, p i = π) (L : Ω → ℝ) :
    linPool w (fun i => bayesL (p i) L) = bayesL (linPool w p) L :=
  eb_on_common_prior (linPool w) (linPool_unanimous hw) hp L

/-- A full-support conditionalisation is trivial: if `π(·|E)` is positive
everywhere, then `E` contains every state and `π(·|E) = π`. -/
theorem cond_eq_self_of_pos {π : Ω → ℝ} (hπ : IsProb π) {E : Finset Ω}
    (hpos : ∀ ω, 0 < cond π E ω) : cond π E = π := by
  have hE : ∀ ω, ω ∈ E := by
    intro ω
    by_contra h
    have := hpos ω
    simp [cond, normalize, restrict, h] at this
  have : restrict E π = π := by funext ω; simp [restrict, hE ω]
  rw [cond, this, normalize_of_sum_one hπ.2]

/-- **A full-support posterior that moved is not a conditionalisation.** If
`Q > 0` everywhere and `Q ≠ π`, then `Q ≠ π(·|E)` for every event `E`. Jeffrey
posteriors with interior credences have full support, so the step
`P ↦ P^J_AB` is outside the premise of Dynamic Rationality. -/
theorem not_cond_of_fullSupport {π Q : Ω → ℝ} (hπ : IsProb π) (hQ : ∀ ω, 0 < Q ω)
    (hne : Q ≠ π) (E : Finset Ω) : Q ≠ cond π E := by
  intro h
  apply hne
  rw [h]
  exact cond_eq_self_of_pos hπ (fun ω => h ▸ hQ ω)

end Literature.Dietrich
