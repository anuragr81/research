import IscLean.Closure
import Mathlib.Tactic.FieldSimp

open Finset

namespace Isc

variable {ι : Type*} [DecidableEq ι]

noncomputable def poolEU (P : Finset ι) (x : ι → ℝ) (L : ℝ) (u : ℝ → ℝ) (i : ι) : ℝ :=
  (∑ k ∈ P.erase i, u (x k - L)) / ((P.card : ℝ) - 1)

noncomputable def closureEU (P : Finset ι) (x : ι → ℝ) (L σ : ℝ) (g : ι → Bool) (u : ℝ → ℝ)
    (i : ι) : ℝ :=
  (∑ k ∈ P.erase i, crossWeight σ g i k * u (x k - L)) / (∑ k ∈ P.erase i, crossWeight σ g i k)

omit [DecidableEq ι] in
theorem weighted_avg_lt {s : Finset ι} {w v : ι → ℝ} {c : ℝ} (hw : ∀ k ∈ s, 0 ≤ w k)
    (hpos : 0 < ∑ k ∈ s, w k) (hv : ∀ k ∈ s, v k < c) :
    (∑ k ∈ s, w k * v k) / (∑ k ∈ s, w k) < c := by
  obtain ⟨j, hj, hwj⟩ : ∃ j ∈ s, 0 < w j := by
    by_contra h
    push_neg at h
    have : ∑ k ∈ s, w k ≤ 0 := sum_nonpos h
    linarith
  rw [div_lt_iff₀ hpos, mul_sum]
  apply sum_lt_sum
  · intro k hk
    rw [mul_comm c]
    exact mul_le_mul_of_nonneg_left (hv k hk).le (hw k hk)
  · exact ⟨j, hj, by rw [mul_comm c]; exact mul_lt_mul_of_pos_left (hv j hj) hwj⟩

theorem card_erase_real {P : Finset ι} {i : ι} (hi : i ∈ P) :
    ((P.erase i).card : ℝ) = (P.card : ℝ) - 1 := by
  rw [card_erase_of_mem hi, Nat.cast_sub (card_pos.mpr ⟨i, hi⟩)]
  simp

theorem poolEU_id {P : Finset ι} (h2 : 2 ≤ P.card) {i : ι} (hi : i ∈ P) (x : ι → ℝ) (L : ℝ) :
    poolEU P x L id i = poolGain P x L i + x i := by
  have hpos : (P.card : ℝ) - 1 ≠ 0 := by
    have : (2 : ℝ) ≤ (P.card : ℝ) := by exact_mod_cast h2
    linarith
  unfold poolEU poolGain
  simp only [id, sum_sub_distrib, sum_const, nsmul_eq_mul, card_erase_real hi]
  field_simp
  ring

theorem richest_entrant_prefers_out {P : Finset ι} (h2 : 2 ≤ P.card) (x : ι → ℝ) {L : ℝ}
    (hL : 0 < L) {u : ℝ → ℝ} {S : Set ℝ} (hu : StrictMonoOn u S)
    (hS : ∀ k ∈ P, x k ∈ S ∧ x k - L ∈ S) :
    ∃ i ∈ P, poolEU P x L u i < u (x i) := by
  have hne : P.Nonempty := card_pos.mp (by omega)
  obtain ⟨i, hi, hmax⟩ := exists_max_image P x hne
  refine ⟨i, hi, ?_⟩
  have hlt : ∀ k ∈ P.erase i, u (x k - L) < u (x i) := fun k hk => by
    have hkP := mem_of_mem_erase hk
    exact hu (hS k hkP).2 (hS i hi).1 (by linarith [hmax k hkP])
  have hpos : (0 : ℝ) < ∑ _k ∈ P.erase i, (1 : ℝ) := by
    rw [sum_const, nsmul_eq_mul, mul_one, card_erase_real hi]
    have : (2 : ℝ) ≤ (P.card : ℝ) := by exact_mod_cast h2
    linarith
  have h := weighted_avg_lt (fun _ _ => zero_le_one) hpos hlt
  simp only [one_mul, sum_const, nsmul_eq_mul, mul_one, card_erase_real hi] at h
  exact h

theorem no_stable_pool {P : Finset ι} (h2 : 2 ≤ P.card) (x : ι → ℝ) {L : ℝ} (hL : 0 < L)
    {u : ℝ → ℝ} {S : Set ℝ} (hu : StrictMonoOn u S) (hS : ∀ k ∈ P, x k ∈ S ∧ x k - L ∈ S) :
    ¬ ∀ i ∈ P, u (x i) ≤ poolEU P x L u i := by
  intro h
  obtain ⟨i, hi, hlt⟩ := richest_entrant_prefers_out h2 x hL hu hS
  linarith [h i hi]

theorem richest_entrant_prefers_out_closure {P : Finset ι} (h2 : 2 ≤ P.card) (x : ι → ℝ)
    {L σ : ℝ} (hL : 0 < L) (hσ : σ < 1) (g : ι → Bool) {u : ℝ → ℝ} {S : Set ℝ}
    (hu : StrictMonoOn u S) (hS : ∀ k ∈ P, x k ∈ S ∧ x k - L ∈ S) :
    ∃ i ∈ P, closureEU P x L σ g u i < u (x i) := by
  have hne : P.Nonempty := card_pos.mp (by omega)
  obtain ⟨i, hi, hmax⟩ := exists_max_image P x hne
  refine ⟨i, hi, ?_⟩
  have hlt : ∀ k ∈ P.erase i, u (x k - L) < u (x i) := fun k hk => by
    have hkP := mem_of_mem_erase hk
    exact hu (hS k hkP).2 (hS i hi).1 (by linarith [hmax k hkP])
  have hw : ∀ k ∈ P.erase i, 0 < crossWeight σ g i k := fun k _ => by
    unfold crossWeight
    split_ifs <;> linarith
  have hj : (P.erase i).Nonempty := by
    rw [← card_pos, card_erase_of_mem hi]
    omega
  exact weighted_avg_lt (fun k hk => (hw k hk).le) (sum_pos hw hj) hlt

theorem control_prefers_out_needs_strict_monotone :
    ∀ i ∈ ({0, 1} : Finset ℕ),
      (0 : ℝ) ≤ poolEU ({0, 1} : Finset ℕ) (fun _ => (0 : ℝ)) 1 (fun _ => 0) i := by
  intro i _
  simp [poolEU]

theorem control_prefers_out_needs_positive_cost :
    ∀ i ∈ ({0, 1} : Finset ℕ),
      id ((fun _ : ℕ => (1 : ℝ)) i) ≤ poolEU ({0, 1} : Finset ℕ) (fun _ => (1 : ℝ)) 0 id i := by
  intro i hi
  simp only [mem_insert, mem_singleton] at hi
  rcases hi with rfl | rfl <;> norm_num [poolEU]

end Isc
