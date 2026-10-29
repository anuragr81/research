import Mathlib

namespace SenguptaHogue

theorem gap_leverage : (10.70 : ℝ) - 9.65 = 1.05 := by norm_num

theorem gap_riskbased : (17.00 : ℝ) - 14.80 = 2.20 := by norm_num

theorem control_rounding : (10.70 : ℝ) - 9.65 ≠ 1.06 := by norm_num

theorem control_riskbased_not_leverage : (2.20 : ℝ) ≠ 1.05 := by norm_num

end SenguptaHogue
