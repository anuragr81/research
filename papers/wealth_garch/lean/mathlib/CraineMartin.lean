import Mathlib

namespace CraineMartin

theorem scr_ge_k {k L : ℝ} (hk0 : 0 < k) (hk1 : k < 1) (hL0 : 0 ≤ L) (hL1 : L < 1) :
    k ≤ k / (1 - (1 - k) * L) := by
  have hpos : 0 < 1 - (1 - k) * L := by nlinarith
  have hle : 1 - (1 - k) * L ≤ 1 := by nlinarith
  rw [le_div_iff₀ hpos]
  nlinarith

theorem ratio_above_four : (4 : ℚ) < 739 / 175 := by norm_num

theorem control_scr_needs_loss_below_one : 1 - (1 - (0 : ℝ)) * 1 = 0 := by norm_num

theorem control_ratio_not_eight : (739 : ℚ) / 175 < 8 := by norm_num

end CraineMartin
