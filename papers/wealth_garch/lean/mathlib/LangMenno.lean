import Mathlib

namespace LangMenno

theorem cbr_is_min_plus_ccob : (10 : ℚ) / 100 + 25 / 1000 = 125 / 1000 := by norm_num

theorem us_floor_same_construction : (8 : ℚ) / 100 + 25 / 1000 = 105 / 1000 := by norm_num

theorem control_cbr_needs_buffer : ¬ ((10 : ℚ) / 100 + 0 = 125 / 1000) := by norm_num

theorem control_us_floor_not_ecb : ¬ ((105 : ℚ) / 1000 = 125 / 1000) := by norm_num

end LangMenno
