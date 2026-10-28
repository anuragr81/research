import Mathlib

namespace BonaparteCooper

/-- The time cost of an adjustment: income y times (1 - ψ), whatever the amount traded. -/
def timeCost (y ψ _a : ℝ) : ℝ := y * (1 - ψ)

/-- It is fixed per adjustment: the same at every trade size. -/
theorem timeCost_fixed_in_size {y ψ a₁ a₂ : ℝ} : timeCost y ψ a₁ = timeCost y ψ a₂ := rfl

/-- It scales with income, so a richer household pays more per adjustment. -/
theorem timeCost_rises_in_income {y₁ y₂ ψ : ℝ} (hψ : ψ < 1) (h : y₁ < y₂) :
    timeCost y₁ ψ 0 < timeCost y₂ ψ 0 := by
  unfold timeCost
  nlinarith

/-- At ψ = 1 the time cost is zero. -/
theorem control_no_cost_at_one {y a : ℝ} : timeCost y 1 a = 0 := by
  simp [timeCost]

end BonaparteCooper
