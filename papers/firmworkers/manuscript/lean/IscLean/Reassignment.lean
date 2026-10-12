import IscLean.Closure
import Mathlib.GroupTheory.Perm.Basic
import Mathlib.Tactic

open Finset

namespace Isc

variable {ι : Type*} [Fintype ι] [DecidableEq ι]

omit [DecidableEq ι] in
theorem reassignment_preserves_total (π : Equiv.Perm ι) (x : ι → ℝ) :
    ∑ i, x (π i) = ∑ i, x i :=
  Equiv.sum_comp π x

theorem reassignment_column_sum (S : Finset (Equiv.Perm ι)) (p : Equiv.Perm ι → ℝ)
    (hp : ∑ π ∈ S, p π = 1) (k : ι) :
    ∑ i, ∑ π ∈ S, (if π i = k then p π else 0) = 1 := by
  rw [sum_comm]
  have e : ∀ π : Equiv.Perm ι, ∑ i, (if π i = k then p π else 0) = p π := by
    intro π
    have h : ∀ i, (π i = k) ↔ (i = π.symm k) := fun i => Equiv.apply_eq_iff_eq_symm_apply π
    simp_rw [h]
    simp
  simp only [e, hp]

theorem reassignment_chances_equal {i j j' : ι} (hj : j ≠ i) (hj' : j' ≠ i) :
    (univ.filter (fun π : Equiv.Perm ι => (∀ x, π x ≠ x) ∧ π i = j)).card
      = (univ.filter (fun π : Equiv.Perm ι => (∀ x, π x ≠ x) ∧ π i = j')).card := by
  have hτi : Equiv.swap j j' i = i := Equiv.swap_apply_of_ne_of_ne (Ne.symm hj) (Ne.symm hj')
  have back : ∀ π : Equiv.Perm ι, Equiv.swap j j' * (Equiv.swap j j' * π * Equiv.swap j j')
      * Equiv.swap j j' = π := by
    intro π
    ext x
    simp [Equiv.Perm.mul_apply, Equiv.swap_apply_self]
  have free : ∀ π : Equiv.Perm ι, (∀ x, π x ≠ x) →
      ∀ x, (Equiv.swap j j' * π * Equiv.swap j j') x ≠ x := by
    intro π hπ x hx
    simp only [Equiv.Perm.mul_apply] at hx
    have h2 := congrArg (Equiv.swap j j') hx
    rw [Equiv.swap_apply_self] at h2
    exact hπ _ h2
  apply card_bij' (fun π _ => Equiv.swap j j' * π * Equiv.swap j j')
    (fun π _ => Equiv.swap j j' * π * Equiv.swap j j')
  · intro π hπ
    rw [mem_filter] at hπ ⊢
    refine ⟨mem_univ _, free π hπ.2.1, ?_⟩
    simp only [Equiv.Perm.mul_apply, hτi, hπ.2.2, Equiv.swap_apply_left]
  · intro π hπ
    rw [mem_filter] at hπ ⊢
    refine ⟨mem_univ _, free π hπ.2.1, ?_⟩
    simp only [Equiv.Perm.mul_apply, hτi, hπ.2.2, Equiv.swap_apply_right]
  · intro π _
    exact back π
  · intro π _
    exact back π

theorem closure_column_sum_eq_one_iff {n₁ n₂ σ : ℝ} (h1 : 1 ≤ n₁) (h2 : 1 ≤ n₂) (hσ0 : 0 ≤ σ)
    (hσ1 : σ < 1) :
    (n₁ - 1) / ((n₁ - 1) + (1 - σ) * n₂) + (1 - σ) * n₂ / ((n₂ - 1) + (1 - σ) * n₁) = 1
      ↔ σ = 0 ∨ n₁ = n₂ := by
  have r1 : 0 < (n₁ - 1) + (1 - σ) * n₂ := by nlinarith
  have r2 : 0 < (n₂ - 1) + (1 - σ) * n₁ := by nlinarith
  rw [div_add_div _ _ r1.ne' r2.ne', div_eq_one_iff_eq (mul_pos r1 r2).ne']
  have key : (n₁ - 1) * ((n₂ - 1) + (1 - σ) * n₁) + ((n₁ - 1) + (1 - σ) * n₂) * ((1 - σ) * n₂)
      - ((n₁ - 1) + (1 - σ) * n₂) * ((n₂ - 1) + (1 - σ) * n₁)
      = (1 - σ) * n₂ * σ * (n₁ - n₂) := by ring
  constructor
  · intro h
    have h0 : (1 - σ) * n₂ * σ * (n₁ - n₂) = 0 := by linarith
    have hne : (1 - σ) * n₂ ≠ 0 := mul_ne_zero (by linarith) (by linarith)
    rcases mul_eq_zero.mp h0 with h3 | h3
    · rcases mul_eq_zero.mp h3 with h4 | h4
      · exact absurd h4 hne
      · exact Or.inl h4
    · exact Or.inr (by linarith)
  · rintro (h | h)
    · subst h
      linarith [key]
    · subst h
      linarith [key]

noncomputable def closureChance (σ : ℝ) (g : ι → Bool) (i k : ι) : ℝ :=
  if k = i then 0 else crossWeight σ g i k / ∑ j ∈ univ.erase i, crossWeight σ g i j

theorem closure_weight_total (σ : ℝ) (g : ι → Bool) (i : ι) :
    ∑ j ∈ univ.erase i, crossWeight σ g i j
      = ((univ.filter (fun j => g j = g i)).card - 1 : ℝ)
        + (1 - σ) * (univ.filter (fun j => g j ≠ g i)).card := by
  rw [sum_erase_eq_sub (mem_univ i)]
  unfold crossWeight
  rw [sum_ite, sum_const, sum_const, nsmul_eq_mul, nsmul_eq_mul, if_pos rfl]
  ring

theorem closure_column_sum (σ : ℝ) (g : ι → Bool) (k : ι) :
    ∑ i, closureChance σ g i k
      = (((univ.filter (fun j => g j = g k)).card : ℝ) - 1)
          / ((((univ.filter (fun j => g j = g k)).card : ℝ) - 1)
            + (1 - σ) * (univ.filter (fun j => g j ≠ g k)).card)
        + (1 - σ) * (univ.filter (fun j => g j ≠ g k)).card
          / ((((univ.filter (fun j => g j ≠ g k)).card : ℝ) - 1)
            + (1 - σ) * (univ.filter (fun j => g j = g k)).card) := by
  set nk := (univ.filter (fun j => g j = g k)).card with hnk
  set no := (univ.filter (fun j => g j ≠ g k)).card with hno
  have same : ∀ i, g i = g k →
      ∑ j ∈ univ.erase i, crossWeight σ g i j = ((nk : ℝ) - 1) + (1 - σ) * no := by
    intro i hi
    rw [closure_weight_total]
    have e1 : univ.filter (fun j => g j = g i) = univ.filter (fun j => g j = g k) := by
      rw [hi]
    have e2 : univ.filter (fun j => g j ≠ g i) = univ.filter (fun j => g j ≠ g k) := by
      rw [hi]
    rw [e1, e2]
  have other : ∀ i, g i ≠ g k →
      ∑ j ∈ univ.erase i, crossWeight σ g i j = ((no : ℝ) - 1) + (1 - σ) * nk := by
    intro i hi
    rw [closure_weight_total]
    have e1 : univ.filter (fun j => g j = g i) = univ.filter (fun j => g j ≠ g k) := by
      apply filter_congr
      intro j _
      cases hgj : g j <;> cases hgi : g i <;> cases hgk : g k <;> simp_all
    have e2 : univ.filter (fun j => g j ≠ g i) = univ.filter (fun j => g j = g k) := by
      apply filter_congr
      intro j _
      cases hgj : g j <;> cases hgi : g i <;> cases hgk : g k <;> simp_all
    rw [e1, e2]
  have split : ∑ i, closureChance σ g i k
      = ∑ i ∈ univ.filter (fun i => g i = g k), closureChance σ g i k
        + ∑ i ∈ univ.filter (fun i => ¬ g i = g k), closureChance σ g i k :=
    (sum_filter_add_sum_filter_not univ (fun i => g i = g k) _).symm
  have hkmem : k ∈ univ.filter (fun i => g i = g k) := mem_filter.mpr ⟨mem_univ k, rfl⟩
  have part1 : ∑ i ∈ univ.filter (fun i => g i = g k), closureChance σ g i k
      = ((nk : ℝ) - 1) * (1 / (((nk : ℝ) - 1) + (1 - σ) * no)) := by
    rw [← add_sum_erase _ _ hkmem]
    have hk0 : closureChance σ g k k = 0 := by simp [closureChance]
    have hrest : ∀ i ∈ (univ.filter (fun i => g i = g k)).erase k,
        closureChance σ g i k = 1 / (((nk : ℝ) - 1) + (1 - σ) * no) := by
      intro i hi
      rw [mem_erase, mem_filter] at hi
      unfold closureChance
      rw [if_neg (Ne.symm hi.1), same i hi.2.2]
      unfold crossWeight
      rw [if_pos hi.2.2.symm]
    rw [hk0, zero_add, sum_congr rfl hrest, sum_const, card_erase_of_mem hkmem, nsmul_eq_mul]
    have hpos : 1 ≤ nk := card_pos.mpr ⟨k, hkmem⟩
    rw [Nat.cast_sub hpos]
    simp
  have part2 : ∑ i ∈ univ.filter (fun i => ¬ g i = g k), closureChance σ g i k
      = (no : ℝ) * ((1 - σ) / (((no : ℝ) - 1) + (1 - σ) * nk)) := by
    have hrest : ∀ i ∈ univ.filter (fun i => ¬ g i = g k),
        closureChance σ g i k = (1 - σ) / (((no : ℝ) - 1) + (1 - σ) * nk) := by
      intro i hi
      rw [mem_filter] at hi
      have hik : k ≠ i := fun h => hi.2 (h ▸ rfl)
      unfold closureChance
      rw [if_neg hik, other i hi.2]
      unfold crossWeight
      rw [if_neg (fun h => hi.2 h.symm)]
    rw [sum_congr rfl hrest, sum_const, nsmul_eq_mul]
  rw [split, part1, part2]
  ring

theorem closure_chances_infeasible (σ : ℝ) (g : ι → Bool) (hσ0 : 0 < σ) (hσ1 : σ < 1) {k : ι}
    (hother : 1 ≤ (univ.filter (fun j => g j ≠ g k)).card)
    (hsizes : (univ.filter (fun j => g j = g k)).card ≠ (univ.filter (fun j => g j ≠ g k)).card)
    (S : Finset (Equiv.Perm ι)) (p : Equiv.Perm ι → ℝ) (hp : ∑ π ∈ S, p π = 1) :
    ¬ ∀ i, ∑ π ∈ S, (if π i = k then p π else 0) = closureChance σ g i k := by
  intro h
  have hcol := reassignment_column_sum S p hp k
  rw [sum_congr rfl (fun i _ => h i), closure_column_sum] at hcol
  have hkmem : k ∈ univ.filter (fun j => g j = g k) := mem_filter.mpr ⟨mem_univ k, rfl⟩
  have h1 : (1 : ℝ) ≤ (univ.filter (fun j => g j = g k)).card := by
    exact_mod_cast card_pos.mpr ⟨k, hkmem⟩
  have h2 : (1 : ℝ) ≤ (univ.filter (fun j => g j ≠ g k)).card := by exact_mod_cast hother
  rcases (closure_column_sum_eq_one_iff h1 h2 hσ0.le hσ1).mp hcol with h3 | h3
  · linarith
  · exact hsizes (by exact_mod_cast h3)

theorem control_closure_feasible_with_equal_groups :
    (2 - 1 : ℝ) / ((2 - 1) + (1 - 1 / 2) * 2) + (1 - 1 / 2) * 2 / ((2 - 1) + (1 - 1 / 2) * 2) = 1 := by
  norm_num

theorem control_closure_infeasible_example :
    (1 - 1 : ℝ) / ((1 - 1) + (1 - 1 / 2) * 2) + (1 - 1 / 2) * 2 / ((2 - 1) + (1 - 1 / 2) * 1) = 2 / 3 := by
  norm_num

end Isc
