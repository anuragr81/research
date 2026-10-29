import Mathlib

namespace JuseliusEtAl

theorem pre_multiple : (7.6 : ℝ) < 4.59 / 0.60 ∧ (4.59 : ℝ) / 0.60 < 7.7 := by
  constructor <;> norm_num

theorem post_multiple : (3.8 : ℝ) < 2.59 / 0.68 ∧ (2.59 : ℝ) / 0.68 < 3.82 := by
  constructor <;> norm_num

theorem us_multiple : (4.68 : ℝ) < 2.39 / 0.51 ∧ (2.39 : ℝ) / 0.51 < 4.7 := by
  constructor <;> norm_num

theorem control_post_multiple_not_three : (3 : ℝ) < 2.59 / 0.68 := by norm_num

theorem control_us_lower_volatility : (0.51 : ℝ) < 0.82 := by norm_num

end JuseliusEtAl
