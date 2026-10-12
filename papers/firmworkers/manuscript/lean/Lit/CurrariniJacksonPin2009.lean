import Mathlib.Analysis.SpecialFunctions.Pow.Real
import Mathlib.Analysis.SpecialFunctions.Sqrt
import Mathlib.Tactic

open Finset

namespace CurrariniJacksonPin2009

noncomputable def homophily (s d : ℝ) : ℝ := s / (s + d)

noncomputable def inbreeding (H w : ℝ) : ℝ := (H - w) / (1 - w)

section Indices

theorem inbreeding_pos_iff {H w : ℝ} (hw : w < 1) : 0 < inbreeding H w ↔ w < H := by
  unfold inbreeding
  constructor
  · intro h
    have := (div_pos_iff.mp h)
    rcases this with ⟨h1, _⟩ | ⟨_, h2⟩
    · linarith
    · linarith
  · intro h
    exact div_pos (by linarith) (by linarith)

theorem inbreeding_at_baseline (w : ℝ) : inbreeding w w = 0 := by
  simp [inbreeding]

theorem inbreeding_full {w : ℝ} (hw : w < 1) : inbreeding 1 w = 1 := by
  unfold inbreeding
  exact div_self (by linarith)

theorem two_types_opposite {q₁ w₁ : ℝ} (hw0 : 0 < w₁) (hw1 : w₁ < 1) :
    inbreeding (1 - q₁) (1 - w₁) = (w₁ - q₁) / w₁ ∧
      (inbreeding (1 - q₁) (1 - w₁) < 0 ↔ 0 < inbreeding q₁ w₁) := by
  have e : inbreeding (1 - q₁) (1 - w₁) = (w₁ - q₁) / w₁ := by
    unfold inbreeding
    congr 1 <;> ring
  refine ⟨e, ?_⟩
  rw [e, inbreeding_pos_iff hw1, div_neg_iff]
  constructor
  · rintro (⟨h1, h2⟩ | ⟨h1, h2⟩)
    · linarith
    · linarith
  · intro h
    exact Or.inr ⟨by linarith, hw0⟩

theorem control_opposite_needs_two_types :
    0 < inbreeding (1 / 2) (1 / 3) ∧ 0 < inbreeding (1 / 2) (1 / 3) ∧ inbreeding 0 (1 / 3) < 0 ∧
      (1 / 2 : ℝ) + 1 / 2 + 0 = 1 / 3 + 1 / 3 + 1 / 3 := by
  unfold inbreeding
  norm_num

end Indices

section Balance

variable {ι : Type*}

theorem unbiased_some_heterophilous {s : Finset ι} {q w : ι → ℝ}
    (hsum : ∑ i ∈ s, q i = ∑ i ∈ s, w i) {i : ι} (hi : i ∈ s) (hq : w i < q i) :
    ∃ j ∈ s, q j < w j := by
  by_contra h
  push_neg at h
  have : ∑ j ∈ s, w j < ∑ j ∈ s, q j := sum_lt_sum h ⟨i, hi, hq⟩
  linarith

end Balance

section Example

theorem optimal_time_foc {α γ c q : ℝ} (hα0 : 0 < α) (hα1 : α < 1) (hc : 0 < c)
    (hK : 0 < γ + (1 - γ) * q) :
    let K := γ + (1 - γ) * q
    let t := (α / c) ^ (1 / (1 - α)) * K ^ (α / (1 - α))
    α * t ^ (α - 1) * K ^ α = c := by
  intro K t
  have h1a : (1 : ℝ) - α ≠ 0 := by linarith
  have hb : 0 < α / c := div_pos hα0 hc
  have hKp : 0 < K := hK
  have ht : t ^ (α - 1) = (α / c) ^ (-1 : ℝ) * K ^ (-α) := by
    show ((α / c) ^ (1 / (1 - α)) * K ^ (α / (1 - α))) ^ (α - 1) = _
    rw [Real.mul_rpow (Real.rpow_nonneg hb.le _) (Real.rpow_nonneg hKp.le _), ← Real.rpow_mul hb.le,
      ← Real.rpow_mul hKp.le]
    congr 2
    all_goals field_simp
    all_goals ring
  rw [ht, Real.rpow_neg_one, mul_assoc, mul_assoc, ← Real.rpow_add hKp, neg_add_cancel,
    Real.rpow_zero, mul_one]
  field_simp

theorem example_solution :
    2 * Real.sqrt 3 / (2 * Real.sqrt 3 + (3 - Real.sqrt 3)) = Real.sqrt 3 - 1 ∧
      (Real.sqrt 3 - 1) + (2 - Real.sqrt 3) = 1 := by
  have h3 : Real.sqrt 3 ^ 2 = 3 := Real.sq_sqrt (by norm_num)
  have hpos : 0 < Real.sqrt 3 := Real.sqrt_pos.mpr (by norm_num)
  constructor
  · rw [div_eq_iff (by nlinarith)]
    nlinarith
  · ring

theorem welfare_proportional_to_friends {α c t K : ℝ} (ht : 0 < t)
    (hfoc : α * t ^ (α - 1) * K = c) : t ^ α * K = c * t / α ∨ α = 0 := by
  by_cases hα : α = 0
  · right
    exact hα
  · left
    have e : t ^ (α - 1) = t ^ α / t := Real.rpow_sub_one ht.ne' α
    rw [e] at hfoc
    field_simp
    field_simp at hfoc
    linarith

end Example

section Closure

theorem closure_is_inbreeding_over_homophily {n m σ : ℝ} (hn : 0 < n) (hm : 0 < m) (hσ : σ ≤ 1) :
    let H := n / (n + (1 - σ) * m)
    let w := n / (n + m)
    σ = inbreeding H w / H := by
  intro H w
  have hd : 0 < n + (1 - σ) * m := by nlinarith
  have hnm : 0 < n + m := by linarith
  unfold inbreeding
  show σ = (n / (n + (1 - σ) * m) - n / (n + m)) / (1 - n / (n + m)) / (n / (n + (1 - σ) * m))
  have e1 : 1 - n / (n + m) = m / (n + m) := by
    field_simp
  rw [e1]
  field_simp
  ring

theorem control_closure_needs_outgroup :
    inbreeding (1 / (1 + (1 - 1 / 2) * 0)) (1 / (1 + 0)) = 0 := by
  unfold inbreeding
  norm_num

end Closure

end CurrariniJacksonPin2009
