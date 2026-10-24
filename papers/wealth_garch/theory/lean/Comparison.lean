/-
================================================================================
  The three approaches of the theory paper's Section 3 in one formulation
================================================================================

The limits of buffer variance of `CapGeometry.nu2`, evaluated at the baseline of
Bayraktar, Chevalier, Ly Vath and Wang (2026), and the nesting of the
volatility levels of Black and Perold (1992): with no liability shock the limit
for slope `a` is the log-cushion variance of capped portfolio insurance with
multiple `1/a`. The control shows the liability shock moves the limits.
-/

import CapGeometry
import BlackPerold

namespace Comparison

/-- The deficit limit at the baseline of Bayraktar et al.,
σ = 0.08, σ_L = 0.03, c = 0.20, a₁ = 0.045. -/
theorem nu1_baseline :
    CapGeometry.nu2 (8 / 100) (3 / 100) (20 / 100) (45 / 1000) = 2543449 / 810000 := by
  unfold CapGeometry.nu2
  norm_num

/-- The surplus limit at the same baseline, a₃ = 0.30. -/
theorem nu3_baseline :
    CapGeometry.nu2 (8 / 100) (3 / 100) (20 / 100) (30 / 100) = 6193 / 90000 := by
  unfold CapGeometry.nu2
  norm_num

/-- The deficit limit over the surplus limit at the baseline. -/
theorem ratio_baseline :
    CapGeometry.nu2 (8 / 100) (3 / 100) (20 / 100) (45 / 1000)
      / CapGeometry.nu2 (8 / 100) (3 / 100) (20 / 100) (30 / 100) = 2543449 / 55737 := by
  rw [nu1_baseline, nu3_baseline]
  norm_num

/-- With no liability shock the limit for slope `a` is (1/a)²σ². -/
theorem nu2_hedged (σ c a : ℝ) : CapGeometry.nu2 σ 0 c a = (1 / a) ^ 2 * σ ^ 2 := by
  unfold CapGeometry.nu2
  ring

/-- With no liability shock the limit equals the log-cushion variance of capped
portfolio insurance on its multiplier branch, with multiple 1/a. -/
theorem hedged_is_cushion_vol {σ c a b W F : ℝ}
    (h : 1 / a * (W - F) ≤ b * W) (hC : 0 < W - F) :
    CapGeometry.nu2 σ 0 c a = BlackPerold.cushionVol2 σ (BlackPerold.exposure (1 / a) b W F) (W - F) := by
  rw [nu2_hedged, BlackPerold.multiplier_branch_vol h hC]

/-- The hedged limits at the same baseline, 256/81 and 16/225. -/
theorem hedged_baseline :
    CapGeometry.nu2 (8 / 100) 0 (20 / 100) (45 / 1000) = 256 / 81 ∧
      CapGeometry.nu2 (8 / 100) 0 (20 / 100) (30 / 100) = 16 / 225 := by
  constructor <;> (unfold CapGeometry.nu2; norm_num)

theorem control_liability_shock_moves_nu : CapGeometry.nu2 1 1 0 1 ≠ CapGeometry.nu2 1 0 0 1 := by
  unfold CapGeometry.nu2
  norm_num

end Comparison
