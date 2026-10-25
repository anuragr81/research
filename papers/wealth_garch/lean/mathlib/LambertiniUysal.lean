import Mathlib

namespace LambertiniUysal

/-- The log term of the penalty, log(k / (γ a)). -/
noncomputable def logTerm (γ a k : ℝ) : ℝ := Real.log (k / (γ * a))

theorem logTerm_neg_below {γ a k : ℝ} (hk : 0 < k) (hγa : 0 < γ * a) (h : k < γ * a) :
    logTerm γ a k < 0 :=
  Real.log_neg (div_pos hk hγa) ((div_lt_one hγa).mpr h)

/-- Above the requirement the log term is positive: a reward, not zero. -/
theorem logTerm_pos_above {γ a k : ℝ} (hγa : 0 < γ * a) (h : γ * a < k) : 0 < logTerm γ a k :=
  Real.log_pos ((one_lt_div hγa).mpr h)

theorem control_logTerm_zero_at_requirement {γ a : ℝ} (hγa : 0 < γ * a) : logTerm γ a (γ * a) = 0 := by
  simp [logTerm, div_self hγa.ne']

end LambertiniUysal
