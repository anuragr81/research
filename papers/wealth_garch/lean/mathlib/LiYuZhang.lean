/-
================================================================================
  Li, Yu and Zhang, "Optimal Consumption with Loss Aversion and Reference to
  Past Spending Maximum", arXiv:2108.02648v4 (28 Feb 2023). What
  lit/li_yu_zhang_2023/ attributes to the paper, in the paper's notation
  (β1, β2 the curvature on gains and losses, k the loss aversion degree,
  λ the reference degree, r1 > 1 > 0 > r2 the roots of η² − η − 2r/κ² = 0,
  γ1 = β1/(β1 − 1), κ the Sharpe ratio).
================================================================================
-/

import Mathlib

open Real

namespace LiYuZhang

noncomputable def U (β1 β2 k x : ℝ) : ℝ :=
  if 0 ≤ x then x ^ β1 / β1 else -(k * (-x) ^ β2 / β2)

noncomputable def limitTerm (β1 β2 k lam : ℝ) : ℝ :=
  k / β2 * lam ^ β2 * (if β2 = β1 then 1 else 0) + (1 - lam) ^ β1 / β1

theorem roots_sum_prod {a b c : ℝ} (hab : a ≠ b) (ha : a ^ 2 - a - c = 0) (hb : b ^ 2 - b - c = 0) :
    a + b = 1 ∧ a * b = -c := by
  have h : (a - b) * (a + b - 1) = 0 := by linear_combination ha - hb
  have hs : a + b = 1 := by
    rcases mul_eq_zero.mp h with h1 | h1
    · exact absurd (sub_eq_zero.mp h1) hab
    · linarith
  refine ⟨hs, ?_⟩
  have : b = 1 - a := by linarith
  subst this
  linear_combination -ha

theorem merton_portfolio_limit {μ r σ β1 r1 r2 : ℝ} (hσ : σ ≠ 0) (hμ : μ - r ≠ 0) (hr : r ≠ 0)
    (hβ : β1 - 1 ≠ 0) (hprod : r1 * r2 = -(2 * r / ((μ - r) / σ) ^ 2)) :
    2 * r * (β1 / (β1 - 1) - 1) / ((μ - r) * (r1 * r2)) = (μ - r) / (σ ^ 2 * (1 - β1)) := by
  rw [hprod]
  have h1 : (1 - β1) ≠ 0 := by intro h; apply hβ; linarith
  field_simp
  ring

theorem limitTerm_free_of_k {β1 β2 lam k₁ k₂ : ℝ} (h : β2 ≠ β1) :
    limitTerm β1 β2 k₁ lam = limitTerm β1 β2 k₂ lam := by
  unfold limitTerm
  rw [if_neg h]
  ring

theorem U_homogeneous {β k t x : ℝ} (ht : 0 < t) : U β β k (t * x) = t ^ β * U β β k x := by
  unfold U
  by_cases hx : 0 ≤ x
  · rw [if_pos (mul_nonneg ht.le hx), if_pos hx, Real.mul_rpow ht.le hx]
    ring
  · have hneg : ¬ 0 ≤ t * x := not_le.mpr (mul_neg_of_pos_of_neg ht (not_le.mp hx))
    rw [if_neg hneg, if_neg hx, show -(t * x) = t * -x by ring,
      Real.mul_rpow ht.le (by linarith [not_le.mp hx])]
    ring

theorem control_limitTerm_depends_on_k_when_equal :
    limitTerm (1 / 2) (1 / 2) 1 (1 / 2) ≠ limitTerm (1 / 2) (1 / 2) 2 (1 / 2) := by
  unfold limitTerm
  rw [if_pos rfl]
  intro h
  have hpos : (0 : ℝ) < (1 / 2 : ℝ) ^ (1 / 2 : ℝ) := by positivity
  nlinarith

theorem control_roots_need_distinct :
    ∃ a b c : ℝ, a ^ 2 - a - c = 0 ∧ b ^ 2 - b - c = 0 ∧ a + b ≠ 1 :=
  ⟨0, 0, 0, by norm_num, by norm_num, by norm_num⟩

theorem control_U_not_concave : ¬ ConcaveOn ℝ Set.univ (U (1 / 2) (1 / 2) 1) := by
  intro h
  have key := h.2 (Set.mem_univ (-1)) (Set.mem_univ 0) (by norm_num : (0:ℝ) ≤ 1 / 2)
    (by norm_num : (0:ℝ) ≤ 1 / 2) (by norm_num)
  simp only [smul_eq_mul] at key
  unfold U at key
  have e1 : (1 / 2 : ℝ) * -1 + 1 / 2 * 0 = -(1 / 2) := by norm_num
  rw [e1] at key
  norm_num at key
  have hsq : ((1:ℝ) / 2) ^ ((1:ℝ) / 2) = Real.sqrt (1 / 2) := (Real.sqrt_eq_rpow _).symm
  rw [hsq] at key
  have hlt : (1:ℝ) / 2 < Real.sqrt (1 / 2) := by
    rw [Real.lt_sqrt (by norm_num)]
    norm_num
  have : Real.sqrt (1 / 2) / (1 / 2) = 2 * Real.sqrt (1 / 2) := by ring
  linarith

end LiYuZhang
