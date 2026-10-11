import IscLean.Lottery

open Finset

namespace Isc

variable {ι : Type*} [DecidableEq ι]

noncomputable def crossWeight (σ : ℝ) (g : ι → Bool) (i k : ι) : ℝ :=
  if g k = g i then 1 else 1 - σ

noncomputable def closureGain (P : Finset ι) (x : ι → ℝ) (L σ : ℝ) (g : ι → Bool) (i : ι) : ℝ :=
  (∑ k ∈ P.erase i, crossWeight σ g i k * (x k - x i)) / (∑ k ∈ P.erase i, crossWeight σ g i k) - L

omit [DecidableEq ι] in
theorem crossWeight_symm (σ : ℝ) (g : ι → Bool) (i k : ι) :
    crossWeight σ g i k = crossWeight σ g k i := by
  unfold crossWeight
  by_cases h : g k = g i
  · rw [if_pos h, if_pos h.symm]
  · have h' : ¬ g i = g k := fun e => h e.symm
    rw [if_neg h, if_neg h']

omit [DecidableEq ι] in
theorem crossWeight_zero (g : ι → Bool) (i k : ι) : crossWeight 0 g i k = 1 := by
  unfold crossWeight
  split_ifs <;> norm_num

theorem closureGain_zero {P : Finset ι} {i : ι} (hi : i ∈ P) (x : ι → ℝ) (L : ℝ) (g : ι → Bool) :
    closureGain P x L 0 g i = poolGain P x L i := by
  unfold closureGain poolGain
  simp only [crossWeight_zero, one_mul, sum_const, nsmul_eq_mul, mul_one]
  rw [card_erase_of_mem hi, Nat.cast_sub (card_pos.mpr ⟨i, hi⟩)]
  simp

theorem closureGain_one {P : Finset ι} {i : ι} (hi : i ∈ P) (x : ι → ℝ) (L : ℝ) (g : ι → Bool) :
    closureGain P x L 1 g i = poolGain (P.filter (fun k => g k = g i)) x L i := by
  have hw : ∀ k, crossWeight 1 g i k = if g k = g i then 1 else 0 := by
    intro k
    unfold crossWeight
    split_ifs <;> norm_num
  have hmem : i ∈ P.filter (fun k => g k = g i) := mem_filter.mpr ⟨hi, rfl⟩
  have hpos : 0 < (P.filter (fun k => g k = g i)).card := card_pos.mpr ⟨i, hmem⟩
  have hnum : ∑ k ∈ P.erase i, crossWeight 1 g i k * (x k - x i)
      = ∑ k ∈ (P.filter (fun k => g k = g i)).erase i, (x k - x i) := by
    rw [← filter_erase, sum_filter]
    refine sum_congr rfl (fun k _ => ?_)
    rw [hw]
    split_ifs <;> ring
  have hden : ∑ k ∈ P.erase i, crossWeight 1 g i k
      = ((P.filter (fun k => g k = g i)).card : ℝ) - 1 := by
    simp only [hw]
    rw [← sum_filter, sum_const, nsmul_eq_mul, mul_one, filter_erase, card_erase_of_mem hmem,
      Nat.cast_sub hpos]
    simp
  unfold closureGain poolGain
  rw [hnum, hden]

theorem closureGain_formula (P : Finset ι) (x : ι → ℝ) (L σ : ℝ) (g : ι → Bool) (i : ι) :
    closureGain P x L σ g i
      = ((∑ k ∈ (P.erase i).filter (fun k => g k = g i), (x k - x i))
          + (1 - σ) * ∑ k ∈ (P.erase i).filter (fun k => ¬ g k = g i), (x k - x i))
        / (((P.erase i).filter (fun k => g k = g i)).card
          + (1 - σ) * ((P.erase i).filter (fun k => ¬ g k = g i)).card) - L := by
  unfold closureGain crossWeight
  simp only [ite_mul, one_mul]
  rw [sum_ite, sum_ite, sum_const, sum_const, nsmul_eq_mul, nsmul_eq_mul, mul_one, mul_sum,
    mul_comm ((((P.erase i).filter (fun k => ¬ g k = g i)).card : ℕ) : ℝ) (1 - σ)]

theorem closure_tightens_when_outgroup_richer {a b n m t₁ t₂ : ℝ} (hn : 0 < n) (hm : 0 < m)
    (ht₂ : 0 ≤ t₂) (ht : t₂ < t₁) (hrich : a / n < b / m) :
    (a + t₂ * b) / (n + t₂ * m) < (a + t₁ * b) / (n + t₁ * m) := by
  have hd₁ : 0 < n + t₁ * m := by nlinarith
  have hd₂ : 0 < n + t₂ * m := by nlinarith
  rw [div_lt_div_iff₀ hd₂ hd₁]
  have hab : a * m < b * n := by
    rw [div_lt_div_iff₀ hn hm] at hrich
    linarith
  nlinarith [mul_pos (sub_pos.mpr ht) (sub_pos.mpr hab)]

theorem control_tightening_needs_richer_outgroup {a b n m t₁ t₂ : ℝ} (hn : 0 < n) (hm : 0 < m)
    (ht₂ : 0 ≤ t₂) (ht : t₂ < t₁) (heq : a / n = b / m) :
    (a + t₂ * b) / (n + t₂ * m) = (a + t₁ * b) / (n + t₁ * m) := by
  have hd₁ : 0 < n + t₁ * m := by nlinarith
  have hd₂ : 0 < n + t₂ * m := by nlinarith
  rw [div_eq_div_iff hd₂.ne' hd₁.ne']
  have hab : a * m = b * n := by
    rw [div_eq_div_iff hn.ne' hm.ne'] at heq
    linarith
  nlinarith [hab]

omit [DecidableEq ι] in
theorem control_closure_needs_sigma_le_one (g : ι → Bool) {i k : ι} (h : g k ≠ g i) :
    crossWeight 2 g i k < 0 := by
  unfold crossWeight
  simp [h]

end Isc
