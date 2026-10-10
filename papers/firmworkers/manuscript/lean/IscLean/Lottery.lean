import Mathlib.Algebra.BigOperators.Fin
import Mathlib.Algebra.BigOperators.Field
import Mathlib.Algebra.Order.Field.Basic
import Mathlib.Data.Real.Basic
import Mathlib.Tactic.Linarith
import Mathlib.Tactic.Positivity
import Mathlib.Tactic.Ring

open Finset

namespace Isc

theorem swap_preserves_total {N : ℕ} (x : Fin N → ℝ) (i k : Fin N) :
    ∑ j, x (Equiv.swap i k j) = ∑ j, x j :=
  Equiv.sum_comp (Equiv.swap i k) x

variable {ι : Type*} [DecidableEq ι]

noncomputable def poolGain (P : Finset ι) (x : ι → ℝ) (L : ℝ) (i : ι) : ℝ :=
  (∑ k ∈ P.erase i, (x k - x i)) / ((P.card : ℝ) - 1) - L

theorem pool_sum_others_diff {P : Finset ι} {i : ι} (hi : i ∈ P) (x : ι → ℝ) :
    ∑ k ∈ P.erase i, (x k - x i) = (∑ k ∈ P, x k) - (P.card : ℝ) * x i := by
  have hS : ∑ k ∈ P.erase i, x k = (∑ k ∈ P, x k) - x i := by
    rw [← add_sum_erase P x hi]
    ring
  have hc : 1 ≤ P.card := Finset.card_pos.mpr ⟨i, hi⟩
  rw [sum_sub_distrib, hS, sum_const, card_erase_of_mem hi, nsmul_eq_mul, Nat.cast_sub hc]
  push_cast
  ring

theorem poolGain_closed {P : Finset ι} {i : ι} (hi : i ∈ P) (x : ι → ℝ) (L : ℝ) :
    poolGain P x L i = ((∑ k ∈ P, x k) - (P.card : ℝ) * x i) / ((P.card : ℝ) - 1) - L := by
  rw [poolGain, pool_sum_others_diff hi]

theorem poolGain_total (P : Finset ι) (x : ι → ℝ) (L : ℝ) :
    ∑ i ∈ P, poolGain P x L i = -((P.card : ℝ) * L) := by
  rw [sum_congr rfl (fun i hi => poolGain_closed hi x L)]
  rw [sum_sub_distrib, ← sum_div, sum_sub_distrib, ← mul_sum, sum_const, sum_const,
    nsmul_eq_mul, nsmul_eq_mul]
  rw [sub_self, zero_div]
  ring

theorem poolGain_pos_iff {P : Finset ι} {i : ι} (h2 : 2 ≤ P.card) (hi : i ∈ P) (x : ι → ℝ)
    (L : ℝ) :
    0 < poolGain P x L i ↔ x i < ((∑ k ∈ P, x k) - ((P.card : ℝ) - 1) * L) / (P.card : ℝ) := by
  have hN2 : (2 : ℝ) ≤ (P.card : ℝ) := by exact_mod_cast h2
  have hpos : (0 : ℝ) < (P.card : ℝ) - 1 := by linarith
  have hNpos : (0 : ℝ) < (P.card : ℝ) := by linarith
  rw [poolGain_closed hi, sub_pos, lt_div_iff₀ hpos, lt_div_iff₀ hNpos]
  constructor <;> intro h <;> linarith

theorem richest_entrant_loses {P : Finset ι} (h2 : 2 ≤ P.card) (x : ι → ℝ) {L : ℝ}
    (hL : 0 < L) : ∃ i ∈ P, poolGain P x L i < 0 := by
  have hne : P.Nonempty := Finset.card_pos.mp (by omega)
  obtain ⟨i, hi, hmax⟩ := Finset.exists_max_image P x hne
  refine ⟨i, hi, ?_⟩
  have hpos : (0 : ℝ) < (P.card : ℝ) - 1 := by
    have : (2 : ℝ) ≤ (P.card : ℝ) := by exact_mod_cast h2
    linarith
  have hsum : ∑ k ∈ P.erase i, (x k - x i) ≤ 0 :=
    Finset.sum_nonpos (fun k hk => by linarith [hmax k (Finset.mem_of_mem_erase hk)])
  have hdiv : (∑ k ∈ P.erase i, (x k - x i)) / ((P.card : ℝ) - 1) ≤ 0 :=
    div_nonpos_of_nonpos_of_nonneg hsum hpos.le
  unfold poolGain
  linarith

theorem no_stable_risk_neutral_pool {P : Finset ι} (h2 : 2 ≤ P.card) (x : ι → ℝ) {L : ℝ}
    (hL : 0 < L) : ¬ ∀ i ∈ P, 0 ≤ poolGain P x L i := by
  intro h
  obtain ⟨i, hi, hneg⟩ := richest_entrant_loses h2 x hL
  linarith [h i hi]

theorem taylor_expectation {ι : Type*} (s : Finset ι) (w e : ι → ℝ)
    (hw : ∑ j ∈ s, w j = 1) (h1 : ∑ j ∈ s, w j * e j = 0) (h2 : ∑ j ∈ s, w j * e j ^ 2 = 1)
    (u0 u1 u2 σ : ℝ) :
    ∑ j ∈ s, w j * (u0 + u1 * (σ * e j) + (1 / 2) * u2 * (σ * e j) ^ 2)
      = u0 + (1 / 2) * u2 * σ ^ 2 := by
  have hexp : ∀ j ∈ s, w j * (u0 + u1 * (σ * e j) + (1 / 2) * u2 * (σ * e j) ^ 2)
      = u0 * w j + (u1 * σ) * (w j * e j) + ((1 / 2) * u2 * σ ^ 2) * (w j * e j ^ 2) := by
    intro j _
    ring
  rw [sum_congr rfl hexp, sum_add_distrib, sum_add_distrib, ← mul_sum, ← mul_sum, ← mul_sum,
    hw, h1, h2]
  ring

theorem premium_pos_iff {u2 σ : ℝ} (hσ : σ ≠ 0) : 0 < (1 / 2) * u2 * σ ^ 2 ↔ 0 < u2 := by
  have hs : 0 < σ ^ 2 := by positivity
  constructor
  · intro h
    by_contra hu
    push_neg at hu
    have : (1 / 2) * u2 * σ ^ 2 ≤ 0 := by
      have : (1 / 2) * u2 ≤ 0 := by linarith
      exact mul_nonpos_of_nonpos_of_nonneg this hs.le
    linarith
  · intro h
    positivity

theorem premium_scales_with_income (u2 s m lam : ℝ) :
    (1 / 2) * u2 * (s * (1 - lam) * m) ^ 2 = ((1 / 2) * u2 * s ^ 2 * m ^ 2) * (1 - lam) ^ 2 := by
  ring

theorem control_taylor_needs_mean_zero :
    (1 : ℝ) * (0 + 1 * (1 * 1) + (1 / 2) * 0 * (1 * 1) ^ 2) ≠ 0 + (1 / 2) * 0 * 1 ^ 2 := by
  norm_num

theorem control_premium_needs_sigma_ne_zero :
    ¬ ((0 : ℝ) < (1 / 2) * 1 * 0 ^ 2 ↔ (0 : ℝ) < 1) := by
  norm_num

theorem control_poolGain_pos_needs_two :
    0 < poolGain ({0} : Finset ℕ) (fun _ => (0 : ℝ)) (-1) 0
      ∧ ¬ ((0 : ℝ) < ((∑ _k ∈ ({0} : Finset ℕ), (0 : ℝ))
        - ((({0} : Finset ℕ).card : ℝ) - 1) * (-1)) / (({0} : Finset ℕ).card : ℝ)) := by
  simp [poolGain]

theorem control_unravelling_needs_positive_cost :
    ∀ i ∈ ({0, 1} : Finset ℕ), 0 ≤ poolGain ({0, 1} : Finset ℕ) (fun _ => (1 : ℝ)) 0 i := by
  intro i _
  simp [poolGain]

theorem premium_sign_affine_invariant {α u2 σ : ℝ} (hα : 0 < α) :
    0 < (1 / 2) * (α * u2) * σ ^ 2 ↔ 0 < (1 / 2) * u2 * σ ^ 2 := by
  have e : (1 / 2) * (α * u2) * σ ^ 2 = α * ((1 / 2) * u2 * σ ^ 2) := by ring
  rw [e]
  constructor
  · intro h
    by_contra hc
    push_neg at hc
    nlinarith
  · intro h
    exact mul_pos hα h

end Isc
