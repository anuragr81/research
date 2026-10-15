import ClosureMap

namespace MeasurementMap

open ClosureMap

/-! The recovery formulas of MEASUREMENT_MAP.tex. Each reads a parameter off
recorded standings, given the law of motion. -/

/-- Recovery 1. With no exclusion, two pairs of consecutive standings give
`γ` and `μ`. -/
theorem recovery_mu_gamma {μ γ c₁ c₂ : ℝ} (h1 : c₁ ≠ 1) (h2 : c₂ ≠ 1) (h12 : c₁ ≠ c₂) :
    let y₁ := (H μ γ 0 c₁ - c₁) / (1 - c₁)
    let y₂ := (H μ γ 0 c₂ - c₂) / (1 - c₂)
    (y₁ - y₂) / (c₁ - c₂) = γ ∧ y₁ - γ * c₁ = μ := by
  have e1 : H μ γ 0 c₁ - c₁ = (1 - c₁) * (μ + γ * c₁) := by unfold H; ring
  have e2 : H μ γ 0 c₂ - c₂ = (1 - c₂) * (μ + γ * c₂) := by unfold H; ring
  have n1 : (1 - c₁) ≠ 0 := sub_ne_zero.2 (Ne.symm h1)
  have n2 : (1 - c₂) ≠ 0 := sub_ne_zero.2 (Ne.symm h2)
  have n12 : c₁ - c₂ ≠ 0 := sub_ne_zero.2 h12
  simp only
  rw [e1, e2, mul_div_cancel_left₀ _ n1, mul_div_cancel_left₀ _ n2]
  constructor
  · field_simp; ring
  · ring

/-- Recovery 2. Under the canonical `F`, one pair of consecutive standings
inside `(0, 1)` gives `b`. -/
theorem recovery_b {μ γ b c : ℝ} (hc0 : c ≠ 0) (hc1 : c ≠ 1) :
    ((1 - c) * (μ + γ * c) - (H μ γ b c - c)) / (c * (1 - c) ^ 2) = b := by
  have e : (1 - c) * (μ + γ * c) - (H μ γ b c - c) = b * (c * (1 - c) ^ 2) := by
    unfold H; ring
  have n : c * (1 - c) ^ 2 ≠ 0 := by
    have : (1 - c) ≠ 0 := sub_ne_zero.2 (Ne.symm hc1)
    positivity
  rw [e, mul_div_cancel_right₀ _ n]

/-- Control. At standing `0` the formula of Recovery 2 returns `0` whatever `b`
is. -/
theorem control_recovery_b_interior (μ γ b : ℝ) :
    ((1 - 0) * (μ + γ * 0) - (H μ γ b 0 - 0)) / (0 * (1 - 0) ^ 2) = 0 := by
  simp

/-- Recovery 3. With the discrimination rate `φ = s F c` recorded, one pair
gives `δ`. -/
theorem recovery_delta {F : ℝ → ℝ} {μ γ δ s c : ℝ} (h : s * F c * c ≠ 0) :
    ((1 - c) * (μ + γ * c) - (Hf F μ γ δ s c - c)) / (s * F c * c) = δ := by
  have e : (1 - c) * (μ + γ * c) - (Hf F μ γ δ s c - c) = δ * (s * F c * c) := by
    unfold Hf; ring
  rw [e, mul_div_cancel_right₀ _ h]

/-- Recovery 4. With salience elicited and the rate recorded, `F` is read off
pointwise. -/
theorem recovery_F {F : ℝ → ℝ} {s c : ℝ} (hs : s ≠ 0) :
    (s * F c) / s = F c := by
  rw [mul_comm, mul_div_assoc, div_self hs, mul_one]

end MeasurementMap
