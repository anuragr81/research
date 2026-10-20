import Mathlib

namespace StressTests

theorem sqrt_horizon : Real.sqrt (9 / 4) = 3 / 2 := by
  rw [show (9 / 4 : ℝ) = (3 / 2) ^ 2 by norm_num]
  exact Real.sqrt_sq (by norm_num)

theorem judged_sigma {D z : ℝ} : D / (z * Real.sqrt (9 / 4)) = D / (3 / 2 * z) := by
  rw [sqrt_horizon, mul_comm]

theorem decline_of_rows {a m : ℝ} (h : m ≤ a) : 0 ≤ a - m := by
  linarith

theorem control_horizon_not_quarters : ¬ (Real.sqrt (9 / 4) = 9 / 4) := by
  rw [sqrt_horizon]
  norm_num

theorem control_decline_needs_order : ¬ ((0 : ℝ) ≤ 13.2 - 15.9) := by
  norm_num

end StressTests
