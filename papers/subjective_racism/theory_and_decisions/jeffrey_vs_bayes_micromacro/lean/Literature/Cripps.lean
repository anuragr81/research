/-
# Cripps (2021), "Divisible Updating"

Working paper, UCL, "Originally 2019. This version November 4, 2021" (39 pp.).

This file formalizes the paper's own definitions and results first. It then
examines, in a separately labelled part, a question that belongs to Paper B and
not to Cripps: can the project's Jeffrey two-cue composite be cast as a Cripps
updating rule at all?

## The paper's setup (Section 3, pp. 6-9)

`Θ` is a finite state space. A belief `μ ∈ Δ°(Θ)` has full support. An
experiment `E_n` is a family `(pᶿ)_θ` of full-support distributions on
`n` signals. We store it as `p : Fin n → Θ → ℝ`, where `p s θ` is the
probability of signal `s` in state `θ`, so `p s` is the paper's column `p_s`.
An updating process `U = (U_n)` maps `(μ, E_n)` to a profile of `n` updated
beliefs, one for each signal (p. 7). Here it is the type `Rule Θ`. The four
axioms are stated on valid inputs only, following the paper:

* **Axiom 1 (Uninformativeness, p. 7).** If `pᶿ = pᶿ'` for all `θ, θ'`, then
  every update equals `μ`.
* **Axiom 2 (Symmetry, p. 7).** Permuting the signal labels permutes the profile.
* **Axiom 3 (Divisibility, p. 8).** For `n ≥ 3`: (a) `U_n¹(μ,E_n) = U_2¹(μ,p₁,1-p₁)`;
  (b) `U_nˢ(μ,E_n) = U_{n-1}^{s-1}(U_2²(μ,p₁,1-p₁), E_{n-1})` with
  `E_{n-1} = (pᶿ₋₁ (1-pᶿ₁)⁻¹)_θ`.
  Here the paper's signal `1` is `0 : Fin (m+1)`, and signal `s > 1` is
  `t.succ` for `t : Fin m`.
* **Axiom 4 (Non-Dogmatic, p. 9).** There is a `μᵒ` such that, for every `μ`,
  `U_2¹(μᵒ, p₁, 1-p₁) = μ` has a unique solution `p₁ ∈ Δ°(Θ)`.

**Definition 1 (divisible, p. 11).** There is a bijection `F` of `Δ°(Θ)` with
`U_nˢ(μ,E_n) = F⁻¹(F(μ)∘p_s / F(μ)ᵀp_s)`. That is: map to a shadow prior, apply
Bayes, and map back. Here the bijection is a pair `(F, G)` of mutually inverse
maps on beliefs.

**Proposition 1 (p. 11).** `U` satisfies Axioms 1-4 iff it is divisible.

## What is proved from the paper

* `bayes_bayes`: two successive Bayes updates equal one update on the product
  likelihood.
* `divRule_seq`: the same holds for every divisible rule (the proof's last
  display, p. 29), so sequential updates on independent signals commute.
* `prop1_if`: **the easy direction of Proposition 1.** Every divisible `U`
  satisfies Axioms 1-4 (Appendix, pp. 29).
* `lemma1_i`: Lemma 1(i) for three-signal experiments (Axioms 2 + 3a).
* `seq_eq_product`, i.e. Lemma 1(iii) in the form `u(u(μ,x),y) = u(μ,x∘y)`:
  **Symmetry and Divisibility alone**, with no Axiom 1 or Axiom 4 and no
  bijection `F`, force this.
* `order_invariance`: its corollary `u(u(μ,x),y) = u(u(μ,y),x)`. This is the
  p. 9 remark ("these axioms imply that reversing the order in which two
  signals arrive has no effect on the ultimate beliefs"), proved as a theorem.
  `x` and `y` are arbitrary likelihood vectors, so the result covers two
  conditionally independent binary experiments on *different* partitions, not
  only nested revelation within one experiment.

The hard direction of Proposition 1 is **not formalized**. It needs the
Aczél-Hosszú solution of the translation equation. Corollary 1 and Props 2-6 are
also not formalized.

## The project's question (not Cripps's): the Jeffrey composite

Take `Θ = A × B` (Paper B: `Fin 2 × Fin 2`). A Jeffrey step on `A`'s partition
takes a *delivered credence* `q` on `A`, not an experiment. To ask whether it
satisfies Cripps's axioms, one first has to say which experiment a cue is.
There are two canonical translations:

1. **Matched likelihood** (Paper B's own bridge, Prop. IMM): the cue is the
   experiment with likelihood `ℓ(a,b) = q_a / μ(A=a)`. `jeffreyA_eq_bayes_matched`
   shows the Jeffrey step *is* a Bayes step on that likelihood. The underlying
   rule is then Bayes (`F = id`), which satisfies **all four** axioms by
   `prop1_if`. The composite `J_B ∘ J_A` equals one Bayes update whose
   `B`-likelihood is re-matched to the intermediate belief (`composite_AB_eq_bayes`).
   The order effect therefore comes from the belief-dependence of the
   translation, that is, from which experiment each order feeds in. It does not
   come from any failure of divisibility.
2. **Rigid credence**: the cue fixes `q = φ(p_s)` whatever the prior, which is
   what makes Jeffrey conditioning Jeffrey. This gives `rigidRule φ`. For
   *every* `φ`:
   * it satisfies Symmetry (`rigid_symmetry`);
   * it violates Uninformativeness (`rigid_not_uninformative`);
   * it violates Non-Dogmatic (`rigid_not_nonDogmatic`), because the conditionals
     `μ(B|A)` are frozen;
   * it satisfies Divisibility only if `φ` ignores the evidence
     (`rigid_divisible_imp_evidence_blind`, `rigid_divisible_imp_const`).

Neither translation makes the composite fail "divisibility, and of the four
axioms only divisibility". Under (1) it fails none. Under (2) it fails at least
Axioms 1 and 4, and fails 3 unless it ignores its input.
-/
import Mathlib

namespace Literature.Cripps

set_option linter.unusedSectionVars false

open Finset

variable {Θ : Type*} [Fintype Θ]

/-! ## Beliefs, experiments, rules -/

/-- A full-support belief `μ ∈ Δ°(Θ)`. -/
def IsBelief (μ : Θ → ℝ) : Prop := (∀ θ, 0 < μ θ) ∧ ∑ θ, μ θ = 1

/-- An experiment with `n` signals and full-support signal distributions:
`p s θ` is the probability of signal `s` in state `θ` (p. 7). -/
def IsExperiment {n : ℕ} (p : Fin n → Θ → ℝ) : Prop :=
  (∀ s θ, 0 < p s θ) ∧ ∀ θ, ∑ s, p s θ = 1

/-- An updating process `U = (U_n)_n` (p. 7): given the number of signals, a
belief and an experiment, it returns one updated belief for each signal. -/
def Rule (Θ : Type*) := (n : ℕ) → (Θ → ℝ) → (Fin n → Θ → ℝ) → Fin n → Θ → ℝ

/-- The binary experiment `(p₁, 1 - p₁)`. -/
def binExp (x : Θ → ℝ) : Fin 2 → Θ → ℝ := ![x, 1 - x]

@[simp] theorem binExp_zero (x : Θ → ℝ) : binExp x 0 = x := rfl
@[simp] theorem binExp_one (x : Θ → ℝ) : binExp x 1 = 1 - x := rfl

/-- The paper's `u(μ, p₁) := U_2¹(μ, p₁, 1 - p₁)` (display (2), p. 10). -/
def binU (U : Rule Θ) (μ x : Θ → ℝ) : Θ → ℝ := U 2 μ (binExp x) 0

/-- Bayes' rule, display (3) p. 10: `μ_B = μ∘p_s / μᵀp_s`. -/
noncomputable def bayes (μ x : Θ → ℝ) : Θ → ℝ := fun θ => μ θ * x θ / ∑ θ', μ θ' * x θ'

/-! ## The four axioms -/

/-- **Axiom 1 (Uninformativeness).** -/
def Uninformativeness (U : Rule Θ) : Prop :=
  ∀ (n : ℕ) (μ : Θ → ℝ) (p : Fin n → Θ → ℝ), IsBelief μ → IsExperiment p →
    (∀ s θ θ', p s θ = p s θ') → ∀ s, U n μ p s = μ

/-- **Axiom 2 (Symmetry).** `U_n(μ, (ω(pᶿ))_θ) = (U_n^{ω(1)}, …, U_n^{ω(n)})`. -/
def Symmetry (U : Rule Θ) : Prop :=
  ∀ (n : ℕ) (ω : Equiv.Perm (Fin n)) (μ : Θ → ℝ) (p : Fin n → Θ → ℝ),
    IsBelief μ → IsExperiment p → U n μ (fun s => p (ω s)) = fun s => U n μ p (ω s)

/-- **Axiom 3 (Divisibility)**, for `n = m + 1 ≥ 3`. Part (a) is
consequentialism. Part (b) is the two-step process: first `s = 1` or `s ≠ 1`,
then the residual experiment `E_{n-1}`. -/
def Divisibility (U : Rule Θ) : Prop :=
  ∀ (m : ℕ), 2 ≤ m → ∀ (μ : Θ → ℝ) (p : Fin (m + 1) → Θ → ℝ),
    IsBelief μ → IsExperiment p →
      U (m + 1) μ p 0 = U 2 μ (binExp (p 0)) 0 ∧
      ∀ t : Fin m, U (m + 1) μ p t.succ =
        U m (U 2 μ (binExp (p 0)) 1) (fun t θ => p t.succ θ / (1 - p 0 θ)) t

/-- **Axiom 4 (Non-Dogmatic).** -/
def NonDogmatic (U : Rule Θ) : Prop :=
  ∃ μo : Θ → ℝ, IsBelief μo ∧ ∀ μ : Θ → ℝ, IsBelief μ →
    ∃! p₁ : Θ → ℝ, IsBelief p₁ ∧ U 2 μo (binExp p₁) 0 = μ

/-! ## Divisible updating (Definition 1) -/

/-- `(F, G)` is a bijection of `Δ°(Θ)` with inverse `G`. -/
structure IsBij (F G : (Θ → ℝ) → Θ → ℝ) : Prop where
  F_mem : ∀ μ, IsBelief μ → IsBelief (F μ)
  G_mem : ∀ ν, IsBelief ν → IsBelief (G ν)
  GF : ∀ μ, IsBelief μ → G (F μ) = μ
  FG : ∀ ν, IsBelief ν → F (G ν) = ν

/-- The divisible rule generated by `(F, G)`: shadow prior, then Bayes, then map back. -/
noncomputable def divRule (F G : (Θ → ℝ) → Θ → ℝ) : Rule Θ :=
  fun _ μ p s => G (bayes (F μ) (p s))

/-- **Definition 1.** `U` is divisible if, on valid inputs, it agrees with
`divRule F G` for some bijection `(F, G)`. -/
def IsDivisible (U : Rule Θ) : Prop :=
  ∃ F G, IsBij F G ∧ ∀ (n : ℕ) (μ : Θ → ℝ) (p : Fin n → Θ → ℝ),
    IsBelief μ → IsExperiment p → ∀ s, U n μ p s = G (bayes (F μ) (p s))

/-! ## Bayes lemmas -/

theorem bayes_sum_pos {μ x : Θ → ℝ} [Nonempty Θ] (hμ : ∀ θ, 0 < μ θ) (hx : ∀ θ, 0 < x θ) :
    0 < ∑ θ, μ θ * x θ :=
  Finset.sum_pos (fun θ _ => mul_pos (hμ θ) (hx θ)) Finset.univ_nonempty

theorem bayes_isBelief [Nonempty Θ] {μ x : Θ → ℝ} (hμ : IsBelief μ) (hx : ∀ θ, 0 < x θ) :
    IsBelief (bayes μ x) := by
  have hS := bayes_sum_pos hμ.1 hx
  refine ⟨fun θ => div_pos (mul_pos (hμ.1 θ) (hx θ)) hS, ?_⟩
  simp only [bayes, ← Finset.sum_div]
  exact div_self hS.ne'

/-- An uninformative (constant) likelihood leaves the belief unchanged. -/
theorem bayes_const {μ : Θ → ℝ} (hμ : IsBelief μ) {k : ℝ} (hk : k ≠ 0) :
    bayes μ (fun _ => k) = μ := by
  funext θ
  simp only [bayes, ← Finset.sum_mul, hμ.2, one_mul]
  field_simp

/-- **Two Bayes updates are one Bayes update on the product likelihood.** -/
theorem bayes_bayes [Nonempty Θ] {μ x y : Θ → ℝ} (hμ : ∀ θ, 0 < μ θ) (hx : ∀ θ, 0 < x θ) :
    bayes (bayes μ x) y = bayes μ (x * y) := by
  have hS := (bayes_sum_pos hμ hx).ne'
  funext θ
  simp only [bayes, Pi.mul_apply]
  have h2 : ∑ θ', μ θ' * x θ' / (∑ θ'', μ θ'' * x θ'') * y θ'
      = (∑ θ', μ θ' * (x θ' * y θ')) / ∑ θ'', μ θ'' * x θ'' := by
    rw [Finset.sum_div]
    refine Finset.sum_congr rfl fun θ' _ => ?_
    ring
  rw [h2, show μ θ * x θ / (∑ θ'', μ θ'' * x θ'') * y θ
      = μ θ * (x θ * y θ) / ∑ θ'', μ θ'' * x θ'' by ring, div_div_div_cancel_right₀ hS]

/-- Bayes from the uniform prior on a normalized likelihood returns the likelihood. -/
theorem bayes_uniform [Nonempty Θ] {p : Θ → ℝ} (hp : ∑ θ, p θ = 1) :
    bayes (fun _ => (Fintype.card Θ : ℝ)⁻¹) p = p := by
  have hc : (Fintype.card Θ : ℝ) ≠ 0 := by exact_mod_cast Fintype.card_ne_zero
  funext θ
  simp only [bayes, ← Finset.mul_sum, hp, mul_one]
  field_simp

theorem uniform_isBelief [Nonempty Θ] : IsBelief (fun _ : Θ => (Fintype.card Θ : ℝ)⁻¹) := by
  have hc : (0 : ℝ) < Fintype.card Θ := by exact_mod_cast Fintype.card_pos
  refine ⟨fun _ => inv_pos.mpr hc, ?_⟩
  simp [Finset.card_univ, hc.ne']

/-! ## Divisible rules update sequentially like Bayes -/

/-- **Divisible updating is sequence-free.** For a divisible rule,
`u(u(μ,x),y) = u(μ,x∘y)`. This is the final display of the proof of
Proposition 1 (p. 29), there with `x∘(p_s∘x⁻¹)`. -/
theorem divRule_seq [Nonempty Θ] {F G : (Θ → ℝ) → Θ → ℝ} (hFG : IsBij F G)
    {μ x y : Θ → ℝ} (hμ : IsBelief μ) (hx : ∀ θ, 0 < x θ) :
    G (bayes (F (G (bayes (F μ) x))) y) = G (bayes (F μ) (x * y)) := by
  have hFμ := hFG.F_mem μ hμ
  rw [hFG.FG _ (bayes_isBelief hFμ hx), bayes_bayes hFμ.1 hx]

/-- Corollary: for a divisible rule, two independent signals commute. -/
theorem divRule_comm [Nonempty Θ] {F G : (Θ → ℝ) → Θ → ℝ} (hFG : IsBij F G)
    {μ x y : Θ → ℝ} (hμ : IsBelief μ) (hx : ∀ θ, 0 < x θ) (hy : ∀ θ, 0 < y θ) :
    G (bayes (F (G (bayes (F μ) x))) y) = G (bayes (F (G (bayes (F μ) y))) x) := by
  rw [divRule_seq hFG hμ hx, divRule_seq hFG hμ hy, mul_comm]

/-! ## Proposition 1, the "if" direction -/

theorem binExp_isExperiment {x : Θ → ℝ} (hx : ∀ θ, 0 < x θ ∧ x θ < 1) :
    IsExperiment (binExp x) := by
  refine ⟨fun s θ => ?_, fun θ => ?_⟩
  · fin_cases s
    · simpa [binExp] using (hx θ).1
    · simpa [binExp] using (hx θ).2
  · simp [binExp, Fin.sum_univ_two]

/-- In an experiment with at least two signals, each signal has probability
below one. -/
theorem exp_lt_one {m : ℕ} (hm : 1 ≤ m) {p : Fin (m + 1) → Θ → ℝ} (hp : IsExperiment p) (θ : Θ) :
    p 0 θ < 1 := by
  have h := hp.2 θ
  rw [Fin.sum_univ_succ] at h
  haveI : Nonempty (Fin m) := ⟨⟨0, hm⟩⟩
  have : 0 < ∑ t : Fin m, p t.succ θ :=
    Finset.sum_pos (fun t _ => hp.1 _ θ) Finset.univ_nonempty
  linarith

/-- The residual experiment `E_{n-1}` of Axiom 3(b) is an experiment. -/
theorem residual_isExperiment {m : ℕ} (hm : 1 ≤ m) {p : Fin (m + 1) → Θ → ℝ}
    (hp : IsExperiment p) : IsExperiment (fun (t : Fin m) θ => p t.succ θ / (1 - p 0 θ)) := by
  have hlt := exp_lt_one hm hp
  refine ⟨fun t θ => div_pos (hp.1 _ θ) (by linarith [hlt θ]), fun θ => ?_⟩
  have h := hp.2 θ
  rw [Fin.sum_univ_succ] at h
  have hne : (1 : ℝ) - p 0 θ ≠ 0 := by linarith [hlt θ]
  rw [← Finset.sum_div, div_eq_one_iff_eq hne]
  linarith

/-- In a belief on at least two states, every coordinate is below one. -/
theorem belief_lt_one [Nontrivial Θ] {p : Θ → ℝ} (hp : IsBelief p) (θ : Θ) : p θ < 1 := by
  classical
  obtain ⟨θ', hθ'⟩ := exists_ne θ
  have hsub : ({θ, θ'} : Finset Θ) ⊆ Finset.univ := Finset.subset_univ _
  have h := Finset.sum_le_sum_of_subset_of_nonneg hsub (fun i _ _ => (hp.1 i).le)
  rw [Finset.sum_pair (Ne.symm hθ'), hp.2] at h
  linarith [hp.1 θ']

/-- **Proposition 1, "if" direction** (Appendix, p. 29): a divisible updating
process satisfies Axioms 1-4. -/
theorem prop1_if [Nontrivial Θ] {U : Rule Θ} (hU : IsDivisible U) :
    Uninformativeness U ∧ Symmetry U ∧ Divisibility U ∧ NonDogmatic U := by
  obtain ⟨F, G, hFG, hUe⟩ := hU
  refine ⟨?_, ?_, ?_, ?_⟩
  · -- Axiom 1
    intro n μ p hμ hp hconst s
    obtain ⟨θ₀⟩ := (inferInstance : Nonempty Θ)
    have hps : p s = fun _ => p s θ₀ := funext fun θ => hconst s θ θ₀
    rw [hUe n μ p hμ hp s, hps, bayes_const (hFG.F_mem μ hμ) (hp.1 s θ₀).ne', hFG.GF μ hμ]
  · -- Axiom 2
    intro n ω μ p hμ hp
    have hp' : IsExperiment (fun s => p (ω s)) :=
      ⟨fun s θ => hp.1 _ θ, fun θ => by rw [Equiv.sum_comp ω (fun s => p s θ)]; exact hp.2 θ⟩
    funext s
    rw [hUe n μ _ hμ hp' s, hUe n μ p hμ hp (ω s)]
  · -- Axiom 3
    intro m hm μ p hμ hp
    have hm1 : 1 ≤ m := le_trans (by norm_num) hm
    have hlt := exp_lt_one hm1 hp
    have hb : IsExperiment (binExp (p 0)) :=
      binExp_isExperiment fun θ => ⟨hp.1 0 θ, hlt θ⟩
    have hres := residual_isExperiment hm1 hp
    refine ⟨?_, fun t => ?_⟩
    · rw [hUe _ μ p hμ hp 0, hUe 2 μ _ hμ hb 0]
      rfl
    · have hν : IsBelief (U 2 μ (binExp (p 0)) 1) := by
        rw [hUe 2 μ _ hμ hb 1]
        exact hFG.G_mem _ (bayes_isBelief (hFG.F_mem μ hμ) (hb.1 1))
      rw [hUe _ _ _ hν hres t, hUe _ μ p hμ hp t.succ, hUe 2 μ _ hμ hb 1]
      have hx : ∀ θ, 0 < binExp (p 0) 1 θ := hb.1 1
      rw [divRule_seq hFG hμ hx]
      congr 2
      funext θ
      have hne : (1 : ℝ) - p 0 θ ≠ 0 := by linarith [hlt θ]
      simp only [binExp_one, Pi.mul_apply, Pi.sub_apply, Pi.one_apply]
      field_simp
  · -- Axiom 4
    have hu := uniform_isBelief (Θ := Θ)
    refine ⟨G (fun _ => (Fintype.card Θ : ℝ)⁻¹), hFG.G_mem _ hu, fun μ hμ => ?_⟩
    have key : ∀ p₁ : Θ → ℝ, IsBelief p₁ →
        U 2 (G (fun _ => (Fintype.card Θ : ℝ)⁻¹)) (binExp p₁) 0 = G p₁ := by
      intro p₁ hp₁
      have hb : IsExperiment (binExp p₁) :=
        binExp_isExperiment fun θ => ⟨hp₁.1 θ, belief_lt_one hp₁ θ⟩
      rw [hUe 2 _ _ (hFG.G_mem _ hu) hb 0, hFG.FG _ hu]
      show G (bayes _ p₁) = G p₁
      rw [bayes_uniform hp₁.2]
    refine ⟨F μ, ⟨hFG.F_mem μ hμ, by rw [key _ (hFG.F_mem μ hμ), hFG.GF μ hμ]⟩, ?_⟩
    rintro p₁ ⟨hp₁, hEq⟩
    rw [key p₁ hp₁] at hEq
    rw [← hEq, hFG.FG p₁ hp₁]

/-- Bayesian updating (`F = G = id`) is divisible, hence satisfies Axioms 1-4. -/
theorem bayes_isDivisible [Nonempty Θ] : IsDivisible (divRule (Θ := Θ) id id) :=
  ⟨id, id, ⟨fun _ h => h, fun _ h => h, fun _ _ => rfl, fun _ _ => rfl⟩,
    fun _ _ _ _ _ _ => rfl⟩

/-! ## The p. 9 remark: Symmetry and Divisibility force order-invariance

Here only Axioms 2 and 3 are assumed. There is no Uninformativeness, no
Non-Dogmatic, and no representation. -/

/-- **Lemma 1(i)** for three signals: under Axioms 2 and 3(a), the update after
signal `s` depends only on `p_s`. -/
theorem lemma1_i {U : Rule Θ} (hS : Symmetry U) (hD : Divisibility U) {μ : Θ → ℝ}
    (hμ : IsBelief μ) {p : Fin 3 → Θ → ℝ} (hp : IsExperiment p) :
    U 3 μ p 1 = binU U μ (p 1) := by
  set ω : Equiv.Perm (Fin 3) := Equiv.swap 0 1
  have hp' : IsExperiment (fun s => p (ω s)) :=
    ⟨fun s θ => hp.1 _ θ, fun θ => by rw [Equiv.sum_comp ω (fun s => p s θ)]; exact hp.2 θ⟩
  have h1 := congrFun (hS 3 ω μ p hμ hp) 0
  have h2 := (hD 2 le_rfl μ _ hμ hp').1
  simp only [ω, Equiv.swap_apply_left] at h1 h2
  rw [← h1, h2]
  rfl

/-- **Lemma 1(iii), sequential form.** Under Symmetry and Divisibility, updating
on `x` and then on `y` equals one update on the product likelihood `x∘y`, for
all `x, y ∈ (0,1)^Θ`. -/
theorem seq_eq_product {U : Rule Θ} (hS : Symmetry U) (hD : Divisibility U)
    {μ : Θ → ℝ} (hμ : IsBelief μ) {x y : Θ → ℝ}
    (hx : ∀ θ, 0 < x θ ∧ x θ < 1) (hy : ∀ θ, 0 < y θ ∧ y θ < 1) :
    binU U (binU U μ x) y = binU U μ (x * y) := by
  -- the three-signal experiment (1 - x, x y, x (1 - y))
  set p : Fin 3 → Θ → ℝ := ![1 - x, x * y, x * (1 - y)] with hpdef
  have hp : IsExperiment p := by
    refine ⟨fun s θ => ?_, fun θ => ?_⟩
    · fin_cases s <;> simp [p]
      · linarith [(hx θ).2]
      · exact mul_pos (hx θ).1 (hy θ).1
      · exact mul_pos (hx θ).1 (by linarith [(hy θ).2])
    · simp [p, Fin.sum_univ_three]; ring
  -- Axiom 3(b) at the signal `1 = (0 : Fin 2).succ`
  have hb := (hD 2 le_rfl μ p hμ hp).2 0
  simp only [Fin.succ_zero_eq_one] at hb
  -- the residual experiment is `binExp y`
  have hres : (fun (t : Fin 2) θ => p t.succ θ / (1 - p 0 θ)) = binExp y := by
    funext t θ
    have hx0 : x θ ≠ 0 := (hx θ).1.ne'
    fin_cases t
    · simp [p, binExp]; field_simp
    · simp [p, binExp]; field_simp
  -- the first step `U_2²(μ, 1-x, x)` is `u(μ, x)`, by Symmetry on `Fin 2`
  have hbx : IsExperiment (binExp (p 0)) := by
    apply binExp_isExperiment
    intro θ; simp [p]; constructor <;> linarith [(hx θ).1, (hx θ).2]
  have hsw := congrFun (hS 2 (Equiv.swap 0 1) μ _ hμ hbx) 0
  simp only [Equiv.swap_apply_left] at hsw
  have hswap : (fun s => binExp (p 0) (Equiv.swap (0 : Fin 2) 1 s)) = binExp x := by
    funext s θ
    fin_cases s <;> simp [binExp, p]
  rw [hswap] at hsw
  rw [hres, ← hsw] at hb
  -- combine with Lemma 1(i)
  rw [lemma1_i hS hD hμ hp] at hb
  unfold binU
  rw [← hb]
  rfl

/-- **The p. 9 remark, as a theorem.** Under Symmetry and Divisibility, two
binary signals with fixed likelihoods `x` and `y` give the same final belief in
either order. -/
theorem order_invariance {U : Rule Θ} (hS : Symmetry U) (hD : Divisibility U)
    {μ : Θ → ℝ} (hμ : IsBelief μ) {x y : Θ → ℝ}
    (hx : ∀ θ, 0 < x θ ∧ x θ < 1) (hy : ∀ θ, 0 < y θ ∧ y θ < 1) :
    binU U (binU U μ x) y = binU U (binU U μ y) x := by
  rw [seq_eq_product hS hD hμ hx hy, seq_eq_product hS hD hμ hy hx, mul_comm]

/-! ## THE PROJECT'S QUESTION (not Cripps's): is the Jeffrey composite a Cripps rule?

States are pairs `(a, b) ∈ A × B`. A Jeffrey step on `A`'s partition resets the
`A`-marginal to a delivered credence `q` and keeps `μ(B | A)` fixed. -/

section Jeffrey

variable {A B : Type*} [Fintype A] [Fintype B]

/-- `μ(A = a)`. -/
def margA (μ : A × B → ℝ) (a : A) : ℝ := ∑ b, μ (a, b)

/-- `μ(B = b)`. -/
def margB (μ : A × B → ℝ) (b : B) : ℝ := ∑ a, μ (a, b)

/-- The Jeffrey step on `A`'s partition, `(J_A μ)(a,b) = q_a μ(a,b) / μ(A=a)`. -/
noncomputable def jeffreyA (μ : A × B → ℝ) (q : A → ℝ) : A × B → ℝ :=
  fun x => q x.1 * μ x / margA μ x.1

/-- The Jeffrey step on `B`'s partition. -/
noncomputable def jeffreyB (μ : A × B → ℝ) (r : B → ℝ) : A × B → ℝ :=
  fun x => r x.2 * μ x / margB μ x.2

theorem margA_jeffreyA {μ : A × B → ℝ} {q : A → ℝ} {a : A} (h : margA μ a ≠ 0) :
    margA (jeffreyA μ q) a = q a := by
  simp only [margA, jeffreyA] at h ⊢
  rw [← Finset.sum_div, ← Finset.mul_sum]
  field_simp

/-- A second Jeffrey step on the same partition overrides the first. -/
theorem jeffreyA_jeffreyA {μ : A × B → ℝ} {q q' : A → ℝ} (hm : ∀ a, margA μ a ≠ 0)
    (hq : ∀ a, q a ≠ 0) : jeffreyA (jeffreyA μ q) q' = jeffreyA μ q' := by
  funext x
  show q' x.1 * jeffreyA μ q x / margA (jeffreyA μ q) x.1 = q' x.1 * μ x / margA μ x.1
  rw [margA_jeffreyA (hm x.1)]
  simp only [jeffreyA]
  have := hm x.1; have := hq x.1
  field_simp

/-- A Jeffrey step on `A` keeps the conditional `μ(B | A)`. -/
theorem jeffreyA_cond (μ : A × B → ℝ) (q : A → ℝ) (a : A) (b b' : B) :
    jeffreyA μ q (a, b) * μ (a, b') = jeffreyA μ q (a, b') * μ (a, b) := by
  simp only [jeffreyA]; ring

theorem margA_pos [Nonempty B] {μ : A × B → ℝ} (hμ : IsBelief μ) (a : A) : 0 < margA μ a :=
  Finset.sum_pos (fun b _ => hμ.1 (a, b)) Finset.univ_nonempty

/-! ### Translation 1: matched likelihood (Paper B's Prop. IMM) -/

/-- **A Jeffrey step is a Bayes step on the matched likelihood** `q_a / μ(A=a)`.
This likelihood depends on the current belief `μ`. -/
theorem jeffreyA_eq_bayes_matched [Nonempty B] {μ : A × B → ℝ} (hμ : IsBelief μ) {q : A → ℝ}
    (hq : ∑ a, q a = 1) : jeffreyA μ q = bayes μ (fun x => q x.1 / margA μ x.1) := by
  have hm := margA_pos hμ
  have hS : ∑ x : A × B, μ x * (q x.1 / margA μ x.1) = 1 := by
    rw [Fintype.sum_prod_type, ← hq]
    refine Finset.sum_congr rfl fun a _ => ?_
    have h := (hm a).ne'
    simp only [margA] at h ⊢
    rw [← Finset.sum_mul]
    field_simp
  funext x
  simp only [jeffreyA, bayes, hS, div_one]
  ring

theorem jeffreyB_eq_bayes_matched [Nonempty A] {μ : A × B → ℝ} (hμ : IsBelief μ) {r : B → ℝ}
    (hr : ∑ b, r b = 1) : jeffreyB μ r = bayes μ (fun x => r x.2 / margB μ x.2) := by
  have hm : ∀ b, 0 < margB μ b := fun b =>
    Finset.sum_pos (fun a _ => hμ.1 (a, b)) Finset.univ_nonempty
  have hS : ∑ x : A × B, μ x * (r x.2 / margB μ x.2) = 1 := by
    rw [Fintype.sum_prod_type, Finset.sum_comm, ← hr]
    refine Finset.sum_congr rfl fun b _ => ?_
    have h := (hm b).ne'
    simp only [margB] at h ⊢
    rw [← Finset.sum_mul]
    field_simp
  funext x
  simp only [jeffreyB, bayes, hS, div_one]
  ring

/-- **The composite `J_B ∘ J_A` is one Bayes update** on the product of the
`A`-likelihood matched to the prior and the `B`-likelihood matched to the
*intermediate* belief `J_A μ`. In the other order the `B`-likelihood is matched
to `μ`. The two orders therefore feed different experiments into the same
(Bayes, divisible) rule. -/
theorem composite_AB_eq_bayes [Nonempty A] [Nonempty B] {μ : A × B → ℝ} (hμ : IsBelief μ)
    {q : A → ℝ} {r : B → ℝ} (hq : ∑ a, q a = 1) (hqpos : ∀ a, 0 < q a) (hr : ∑ b, r b = 1) :
    jeffreyB (jeffreyA μ q) r =
      bayes μ ((fun x => q x.1 / margA μ x.1) * (fun x => r x.2 / margB (jeffreyA μ q) x.2)) := by
  have hm := margA_pos hμ
  have hx : ∀ x : A × B, 0 < q x.1 / margA μ x.1 := fun x => div_pos (hqpos _) (hm _)
  have hJ : IsBelief (jeffreyA μ q) := by
    rw [jeffreyA_eq_bayes_matched hμ hq]; exact bayes_isBelief hμ hx
  rw [jeffreyB_eq_bayes_matched hJ hr, jeffreyA_eq_bayes_matched hμ hq,
    bayes_bayes hμ.1 hx]

/-! ### Translation 2: rigid credence

A cue fixes the delivered credence `φ(p_s)` independently of the prior. This
is the defining feature of Jeffrey conditioning. -/

/-- The rigid-credence Jeffrey rule for an evidence-to-credence map `φ`. -/
noncomputable def rigidRule (φ : (A × B → ℝ) → A → ℝ) : Rule (A × B) :=
  fun _ μ p s => jeffreyA μ (φ (p s))

/-- The rigid rule satisfies Symmetry (Axiom 2), whatever `φ` is. -/
theorem rigid_symmetry (φ : (A × B → ℝ) → A → ℝ) : Symmetry (rigidRule φ) := by
  intro n ω μ p _ _
  rfl

/-- Under Symmetry and Divisibility, a rigid rule's credence map cannot respond
to evidence: `φ(x∘y) = φ(y)` on all of `(0,1)^Θ`. -/
theorem rigid_divisible_imp_evidence_blind [Nonempty A] [Nonempty B]
    (φ : (A × B → ℝ) → A → ℝ)
    (hφ : ∀ x : A × B → ℝ, (∀ θ, 0 < x θ ∧ x θ < 1) → ∀ a, 0 < φ x a)
    (hD : Divisibility (rigidRule φ)) {x y : A × B → ℝ}
    (hx : ∀ θ, 0 < x θ ∧ x θ < 1) (hy : ∀ θ, 0 < y θ ∧ y θ < 1) :
    φ (x * y) = φ y := by
  have hu : IsBelief (fun _ : A × B => (Fintype.card (A × B) : ℝ)⁻¹) := uniform_isBelief
  have h : jeffreyA (jeffreyA _ (φ x)) (φ y) = jeffreyA _ (φ (x * y)) :=
    seq_eq_product (rigid_symmetry φ) hD hu hx hy
  have hm := margA_pos hu
  rw [jeffreyA_jeffreyA (fun a => (hm a).ne') (fun a => (hφ x hx a).ne')] at h
  funext a
  have h1 := congrArg (fun μ => margA μ a) h
  rw [margA_jeffreyA (hm a).ne', margA_jeffreyA (hm a).ne'] at h1
  exact h1.symm

/-- Hence a divisible rigid rule has a *constant* credence map on `(0,1)^Θ`, so
it ignores its evidence. -/
theorem rigid_divisible_imp_const [Nonempty A] [Nonempty B]
    (φ : (A × B → ℝ) → A → ℝ)
    (hφ : ∀ x : A × B → ℝ, (∀ θ, 0 < x θ ∧ x θ < 1) → ∀ a, 0 < φ x a)
    (hD : Divisibility (rigidRule φ)) {y z : A × B → ℝ}
    (hy : ∀ θ, 0 < y θ ∧ y θ < 1) (hz : ∀ θ, 0 < z θ ∧ z θ < 1) : φ y = φ z := by
  have half : ∀ w : A × B → ℝ, (∀ θ, 0 < w θ ∧ w θ < 1) →
      ∀ θ, 0 < (fun θ => w θ / 2) θ ∧ (fun θ => w θ / 2) θ < 1 := fun w hw θ =>
    ⟨by simp; linarith [(hw θ).1], by simp; linarith [(hw θ).2]⟩
  have e1 := rigid_divisible_imp_evidence_blind φ hφ hD (half z hz) hy
  have e2 := rigid_divisible_imp_evidence_blind φ hφ hD (half y hy) hz
  have : (fun θ => z θ / 2) * y = (fun θ => y θ / 2) * z := by
    funext θ; simp only [Pi.mul_apply]; ring
  rw [this] at e1
  rw [← e1, e2]

/-! The negative results are stated on Paper B's state space, `Fin 2 × Fin 2`. -/

/-- The uniform belief on `{0,1}²`. -/
noncomputable def mu1 : Fin 2 × Fin 2 → ℝ := fun _ => 1 / 4

/-- A skewed belief: `μ(0,0) = 1/2` and every other cell `1/6`. -/
noncomputable def mu2 : Fin 2 × Fin 2 → ℝ := fun x => if x = (0, 0) then 1 / 2 else 1 / 6

theorem mu1_isBelief : IsBelief mu1 := by
  refine ⟨fun _ => by norm_num [mu1], ?_⟩
  norm_num [mu1, Fintype.sum_prod_type, Fin.sum_univ_two]

theorem mu2_isBelief : IsBelief mu2 := by
  refine ⟨fun x => by unfold mu2; split_ifs <;> norm_num, ?_⟩
  norm_num [mu2, Fintype.sum_prod_type, Fin.sum_univ_two]

/-- **Rigid credence violates Uninformativeness (Axiom 1), for every `φ`.** An
uninformative cue still resets the `A`-marginal to the fixed credence
`φ(const)`. That cannot equal the `A`-marginal of every prior. -/
theorem rigid_not_uninformative (φ : (Fin 2 × Fin 2 → ℝ) → Fin 2 → ℝ) :
    ¬ Uninformativeness (rigidRule φ) := by
  intro h
  let p : Fin 2 → Fin 2 × Fin 2 → ℝ := fun _ _ => 1 / 2
  have hp : IsExperiment p := ⟨fun _ _ => by norm_num [p], fun _ => by
    norm_num [p, Fin.sum_univ_two]⟩
  have hc : ∀ s θ θ', p s θ = p s θ' := fun _ _ _ => rfl
  have e1 := h 2 mu1 p mu1_isBelief hp hc 0
  have e2 := h 2 mu2 p mu2_isBelief hp hc 0
  change jeffreyA mu1 (φ (p 0)) = mu1 at e1
  change jeffreyA mu2 (φ (p 0)) = mu2 at e2
  have m1 := congrArg (fun μ => margA μ 0) e1
  have m2 := congrArg (fun μ => margA μ 0) e2
  rw [margA_jeffreyA (margA_pos mu1_isBelief 0).ne'] at m1
  rw [margA_jeffreyA (margA_pos mu2_isBelief 0).ne'] at m2
  rw [m1] at m2
  simp [margA, mu1, mu2, Fin.sum_univ_two] at m2
  norm_num at m2

/-- **Rigid credence violates Non-Dogmatic (Axiom 4), for every `φ`.** A single
cue on `A` keeps `μᵒ(B | A)`, so `mu1` and `mu2`, whose conditionals
`μ(B=0|A=0)` differ, cannot both be reached from `μᵒ`. -/
theorem rigid_not_nonDogmatic (φ : (Fin 2 × Fin 2 → ℝ) → Fin 2 → ℝ) :
    ¬ NonDogmatic (rigidRule φ) := by
  rintro ⟨μo, hμo, h⟩
  obtain ⟨p1, ⟨-, e1⟩, -⟩ := h mu1 mu1_isBelief
  obtain ⟨p2, ⟨-, e2⟩, -⟩ := h mu2 mu2_isBelief
  change jeffreyA μo (φ (binExp p1 0)) = mu1 at e1
  change jeffreyA μo (φ (binExp p2 0)) = mu2 at e2
  have c1 := jeffreyA_cond μo (φ (binExp p1 0)) 0 0 1
  have c2 := jeffreyA_cond μo (φ (binExp p2 0)) 0 0 1
  rw [e1] at c1
  rw [e2] at c2
  simp [mu1, mu2] at c1 c2
  have := hμo.1 (0, 0)
  linarith

end Jeffrey

end Literature.Cripps
