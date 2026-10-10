import Mathlib.Analysis.MeanInequalitiesPow

namespace Isc

noncomputable def pareto (x0 a q : ℝ) : ℝ := x0 * (1 - q) ^ (-(1 / a))

theorem pareto_strictMono_quantile {x0 a q₁ q₂ : ℝ} (hx0 : 0 < x0) (ha : 0 < a)
    (h12 : q₁ < q₂) (hq₂ : q₂ < 1) :
    pareto x0 a q₁ < pareto x0 a q₂ := by
  unfold pareto
  have hneg : -(1 / a) < 0 := by
    have : 0 < 1 / a := by positivity
    linarith
  exact mul_lt_mul_of_pos_left
    (Real.rpow_lt_rpow_of_exponent_neg (by linarith) (by linarith) hneg) hx0

theorem pareto_strictAnti_tail {x0 a₁ a₂ q : ℝ} (hx0 : 0 < x0) (ha₁ : 0 < a₁)
    (h12 : a₁ < a₂) (hq0 : 0 < q) (hq1 : q < 1) :
    pareto x0 a₂ q < pareto x0 a₁ q := by
  unfold pareto
  have hinv : 1 / a₂ < 1 / a₁ := one_div_lt_one_div_of_lt ha₁ h12
  exact mul_lt_mul_of_pos_left
    (Real.rpow_lt_rpow_of_exponent_gt (by linarith) (by linarith) (by linarith)) hx0

noncomputable def upCost (b mi mk : ℝ) : ℝ := if mi < mk then (mk - mi) ^ b else 0

theorem upCost_down_free {b mi mk : ℝ} (h : mk ≤ mi) : upCost b mi mk = 0 := by
  simp [upCost, not_lt.mpr h]

theorem upCost_steps_le_jump {b m₁ m₂ m₃ : ℝ} (hb : 1 ≤ b) (h12 : m₁ < m₂) (h23 : m₂ < m₃) :
    upCost b m₁ m₂ + upCost b m₂ m₃ ≤ upCost b m₁ m₃ := by
  have h13 : m₁ < m₃ := h12.trans h23
  simp only [upCost, if_pos h12, if_pos h23, if_pos h13]
  have hsum : m₃ - m₁ = (m₂ - m₁) + (m₃ - m₂) := by ring
  rw [hsum]
  exact Real.add_rpow_le_rpow_add (by linarith) (by linarith) hb

theorem upCost_jump_le_steps {b m₁ m₂ m₃ : ℝ} (hb0 : 0 ≤ b) (hb1 : b ≤ 1) (h12 : m₁ < m₂)
    (h23 : m₂ < m₃) :
    upCost b m₁ m₃ ≤ upCost b m₁ m₂ + upCost b m₂ m₃ := by
  have h13 : m₁ < m₃ := h12.trans h23
  simp only [upCost, if_pos h12, if_pos h23, if_pos h13]
  have hsum : m₃ - m₁ = (m₂ - m₁) + (m₃ - m₂) := by ring
  rw [hsum]
  exact Real.rpow_add_le_add_rpow (by linarith) (by linarith) hb0 hb1

theorem printed_index_base_nonpos {i : ℕ} (hi : 1 ≤ i) : (1 : ℝ) - i ≤ 0 := by
  have : (1 : ℝ) ≤ (i : ℝ) := by exact_mod_cast hi
  linarith

theorem printed_charged_branch_base_neg {b mi mk : ℝ} (h : mk < mi) :
    mk - mi < 0 ∧ upCost b mi mk = 0 ∧ ¬ ∃ y : ℝ, y ^ 2 = mk - mi := by
  refine ⟨by linarith, upCost_down_free h.le, ?_⟩
  rintro ⟨y, hy⟩
  nlinarith [sq_nonneg y]

theorem pareto_top_decile (x0 a : ℝ) : pareto x0 a (9 / 10) = x0 * (10 : ℝ) ^ (1 / a) := by
  unfold pareto
  have h : (1 : ℝ) - 9 / 10 = (10 : ℝ)⁻¹ := by norm_num
  rw [h, Real.inv_rpow (by norm_num), Real.rpow_neg (by norm_num), inv_inv]

theorem top_decile_witness : pareto 1 3 (9 / 10) < pareto 1 2 (9 / 10) :=
  pareto_strictAnti_tail one_pos two_pos (by norm_num) (by norm_num) (by norm_num)

end Isc
