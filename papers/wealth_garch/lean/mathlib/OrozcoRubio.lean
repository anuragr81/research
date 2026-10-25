import Mathlib

namespace OrozcoRubio

theorem wellcap_margin : (10 : ℚ) - 8 = 2 := by norm_num

theorem basel3_floor_eq_R : (8 : ℚ) + 5 / 2 = 105 / 10 := by norm_num

theorem basel3_floor_above_pca : (10 : ℚ) < 8 + 5 / 2 := by norm_num

theorem control_floor_needs_buffer : ¬ ((10 : ℚ) < 8 + 0) := by norm_num

theorem control_margin_not_buffer : ¬ ((10 : ℚ) - 8 = 5 / 2) := by norm_num

end OrozcoRubio
