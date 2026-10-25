import Mathlib

namespace BehnEtAl

/-- The breach cost Ω · 1(CR < θ). -/
noncomputable def breach (Ω θ cr : ℝ) : ℝ := if cr < θ then Ω else 0

/-- The issuance cost Φ₁ · a^Φ₂ on an amount a raised. -/
noncomputable def issueCost (Φ₁ Φ₂ a : ℝ) : ℝ := Φ₁ * a ^ Φ₂

theorem breach_flat_below {Ω θ x y : ℝ} (hx : x < θ) (hy : y < θ) : breach Ω θ x = breach Ω θ y := by
  simp [breach, hx, hy]

/-- With Φ₂ > 0 nothing is paid on a zero issue: the cost has no fixed part. -/
theorem issueCost_no_fixed_part {Φ₁ Φ₂ : ℝ} (h : 0 < Φ₂) : issueCost Φ₁ Φ₂ 0 = 0 := by
  simp [issueCost, Real.zero_rpow h.ne']

/-- At Φ₂ = 0 the same form would be a pure fixed cost Φ₁. -/
theorem control_zero_exponent_is_fixed {Φ₁ a : ℝ} : issueCost Φ₁ 0 a = Φ₁ := by
  simp [issueCost]

end BehnEtAl
