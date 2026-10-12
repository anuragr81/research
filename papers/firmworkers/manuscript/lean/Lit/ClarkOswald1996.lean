import Mathlib.Analysis.SpecialFunctions.Log.Basic
import Mathlib.Tactic

namespace ClarkOswald1996

theorem relative_income_form (b : ℝ) {y ystar : ℝ} (hy : 0 < y) (hs : 0 < ystar) :
    b * Real.log y - b * Real.log ystar = b * Real.log (y / ystar) := by
  rw [Real.log_div hy.ne' hs.ne']
  ring

theorem hourly_income_form (b : ℝ) {y h : ℝ} (hy : 0 < y) (hh : 0 < h) :
    b * Real.log y + (-b) * Real.log h = b * Real.log (y / h) := by
  rw [Real.log_div hy.ne' hh.ne']
  ring

theorem common_rise_changes_satisfaction (by_ bs : ℝ) {y ystar k : ℝ} (hy : 0 < y) (hs : 0 < ystar)
    (hk : 0 < k) :
    (by_ * Real.log (k * y) + bs * Real.log (k * ystar)) - (by_ * Real.log y + bs * Real.log ystar)
      = (by_ + bs) * Real.log k := by
  rw [Real.log_mul hk.ne' hy.ne', Real.log_mul hk.ne' hs.ne']
  ring

theorem equal_rise_worse_off : (0.11 + (-0.20) : ℝ) * Real.log 1.1 < 0 := by
  have h : 0 < Real.log 1.1 := Real.log_pos (by norm_num)
  nlinarith

theorem magnitude_of_comparison_effect :
    ((6.5 + 0.8) - (6.5 - 0.8)) * (-0.2 : ℝ) = -0.32 ∧ |(-0.32 : ℝ) - (-0.3)| < 0.05 := by
  constructor
  · norm_num
  · rw [abs_lt]
    constructor <;> norm_num

theorem satisfaction_distribution_total :
    (1645 + 1396 + 995 + 654 + 237 + 90 + 178 : ℕ) = 5195 := by
  norm_num

theorem top_category_share : |(1645 : ℝ) / 5195 - 0.317| < 0.0005 := by
  rw [abs_lt]
  constructor <;> norm_num

theorem control_ratio_form_needs_equal_and_opposite : (0.11 + (-0.26) : ℝ) * Real.log 2 ≠ 0 := by
  have h : 0 < Real.log 2 := Real.log_pos (by norm_num)
  intro h0
  rcases mul_eq_zero.mp h0 with h1 | h1
  · norm_num at h1
  · linarith

end ClarkOswald1996
