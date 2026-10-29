import Mathlib

namespace CorbaeDErasmo

theorem size_gap : (12.10 : ℝ) - 8.46 = 3.64 ∧ (11.28 : ℝ) - 10.51 = 0.77 ∧ (0.77 : ℝ) / 3.64 < 0.22 := by
  refine ⟨by norm_num, by norm_num, by norm_num⟩

theorem big_overpredicted : (2 : ℝ) < 10.51 - 8.46 := by norm_num

theorem control_gap_not_closed : (0.77 : ℝ) < 3.64 := by norm_num

theorem control_big_overpredicted : (8.46 : ℝ) < 10.51 := by norm_num

end CorbaeDErasmo
