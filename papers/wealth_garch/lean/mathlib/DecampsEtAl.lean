import Mathlib

namespace DecampsEtAl

/-- Cost per unit raised when an issue of size m costs p(m + f). -/
noncomputable def unitCost (p f m : ℝ) : ℝ := p * (m + f) / m

/-- With a fixed cost the cost per unit raised falls with the size of the issue, so issues are lumpy. -/
theorem unitCost_falls {p f m₁ m₂ : ℝ} (hp : 0 < p) (hf : 0 < f) (h₁ : 0 < m₁) (h₁₂ : m₁ < m₂) :
    unitCost p f m₂ < unitCost p f m₁ := by
  unfold unitCost
  have h₂ : 0 < m₂ := lt_trans h₁ h₁₂
  rw [div_lt_div_iff₀ h₂ h₁]
  nlinarith [mul_pos hp hf]

/-- Without the fixed cost the cost per unit is p whatever the size. -/
theorem control_no_fixed_cost_flat {p m₁ m₂ : ℝ} (h₁ : m₁ ≠ 0) (h₂ : m₂ ≠ 0) :
    unitCost p 0 m₁ = unitCost p 0 m₂ := by
  simp [unitCost, h₁, h₂]

end DecampsEtAl
