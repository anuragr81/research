import Mathlib

namespace Vo

/-- The two slopes of the insurer's value that set the thresholds: dividends where F' = -1,
    recapitalisation where F' = -1/(1 + α). -/
theorem slopes_ordered {α : ℝ} (hα : 0 < α) : (-1 : ℝ) < -1 / (1 + α) := by
  rw [neg_div, neg_lt_neg_iff, div_lt_one (by linarith)]
  linarith

/-- As α → 0 the two slopes coincide, so the two thresholds merge (Corollary 1). -/
theorem control_slopes_merge_at_zero : (-1 : ℝ) / (1 + 0) = -1 := by norm_num

end Vo
