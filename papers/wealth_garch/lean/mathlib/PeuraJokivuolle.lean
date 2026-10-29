import Mathlib

namespace PeuraJokivuolle

theorem breach_probabilities : (1 : ℝ) - 0.993 = 0.007 ∧ (1 : ℝ) - 0.984 = 0.016 ∧ (1 : ℝ) - 0.972 = 0.028 := by
  refine ⟨by norm_num, by norm_num, by norm_num⟩

theorem fat_tail : (3.4 : ℝ) < 2.41 / 0.69 ∧ (2.41 : ℝ) / 0.69 < 3.5 ∧ (3.6 : ℝ) < 1.16 / 0.32 := by
  refine ⟨by norm_num, by norm_num, by norm_num⟩

theorem control_not_normal_tail : (2.326 : ℝ) < 2.41 / 0.69 := by norm_num

theorem control_breach_order : (0.007 : ℝ) < 0.028 := by norm_num

end PeuraJokivuolle
