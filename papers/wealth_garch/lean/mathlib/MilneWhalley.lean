import Mathlib

namespace MilneWhalley

theorem buffer_eq_u0 {p m : ℝ} (hp : 0 < p) (hm : m < 0) :
    1 / (p - m) * Real.log (m ^ 2 / p ^ 2) = 2 / (p - m) * Real.log (-m / p) := by
  have h : m ^ 2 / p ^ 2 = (-m / p) ^ 2 := by
    field_simp
  rw [h, Real.log_pow]
  push_cast
  ring

theorem buffer_shift_invariant {Ct C0 d : ℝ} : (Ct + d) - (C0 + d) = Ct - C0 := by
  ring

theorem control_buffer_needs_square : ¬ ((2 : ℝ) * Real.log 2 = Real.log 2) := by
  have h : 0 < Real.log 2 := Real.log_pos (by norm_num)
  intro e
  linarith

theorem control_shift_needs_same_d : ¬ ((1 + 1 : ℝ) - (0 + 0) = 1 - 0) := by
  norm_num

end MilneWhalley
