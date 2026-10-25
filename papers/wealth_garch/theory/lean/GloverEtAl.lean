import Mathlib

namespace GloverEtAl

/-- The cost of the forced issue that restores E/L = ω, ψ₀ · max(ωL - E, 0). -/
noncomputable def forcedCost (ψ₀ ω L E : ℝ) : ℝ := ψ₀ * max (ω * L - E) 0

theorem forcedCost_below {ψ₀ ω L E : ℝ} (h : E < ω * L) : forcedCost ψ₀ ω L E = ψ₀ * (ω * L - E) := by
  unfold forcedCost; rw [max_eq_left (by linarith)]

theorem forcedCost_above {ψ₀ ω L E : ℝ} (h : ω * L ≤ E) : forcedCost ψ₀ ω L E = 0 := by
  unfold forcedCost; rw [max_eq_right (by linarith)]; ring

/-- The breach is priced at the slope of voluntary issuance: one parameter for both. -/
theorem breach_slope_is_issuance_cost {ψ₀ ω L E₁ E₂ : ℝ} (h₁ : E₁ < ω * L) (h₂ : E₂ < ω * L) :
    forcedCost ψ₀ ω L E₁ - forcedCost ψ₀ ω L E₂ = ψ₀ * (E₂ - E₁) := by
  rw [forcedCost_below h₁, forcedCost_below h₂]; ring

theorem control_free_issuance_no_breach_cost {ω L E : ℝ} : forcedCost 0 ω L E = 0 := by
  simp [forcedCost]

end GloverEtAl
