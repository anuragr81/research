/-
================================================================================
  When the optimal exposure binds the regulatory cap (row M16)
  Written for the theory paper on 30 October 2026.
================================================================================

In units of exposure U = π x, the first term of the quasi-variational inequality is,
up to terms free of U,

    h(U) = (A/2) (U² S - 2 U D) + B m U,

with A = V'', B = V', S = σ², D = c σ σ_L (x - 1) and m = μ_s - r. The cap is U = u(x).
For 0 ≤ u ≤ U,

    h(U) - h(u) = (U - u) ((A/2) ((U + u) S - 2 D) + B m),

so the cap is optimal over [0, U]
  * when A ≤ 0 and A (U S - D) + B m ≥ 0 (the concave case), and
  * when A ≥ 0, U S ≥ 2 D and B m ≥ 0 (the convex case).
On the solvency branch U = (x - 1)/a1, and U S - 2 D = (x - 1) σ (σ/a1 - 2 c σ_L), so
U S ≥ 2 D when σ ≥ 2 a1 c σ_L. Near a point where A (U S - D) + B m > 0 and the data are
continuous, the cap is optimal on a neighbourhood.
-/

import Mathlib

open Filter Topology Set

namespace CapBinding

/-- The part of the first term of the equation that depends on the exposure U = π x. -/
noncomputable def h (A B m S D U : ℝ) : ℝ := A / 2 * (U ^ 2 * S - 2 * U * D) + B * m * U

theorem h_diff (A B m S D U u : ℝ) :
    h A B m S D U - h A B m S D u = (U - u) * (A / 2 * ((U + u) * S - 2 * D) + B * m) := by
  unfold h
  ring

/-- Concave case: with A ≤ 0 and the slope at the cap nonnegative, the cap is optimal. -/
theorem cap_optimal_concave {A B m S D U : ℝ} (hA : A ≤ 0) (hS : 0 ≤ S)
    (hslope : 0 ≤ A * (U * S - D) + B * m) :
    ∀ u ∈ Icc (0 : ℝ) U, h A B m S D u ≤ h A B m S D U := by
  intro u hu
  have hdiff := h_diff A B m S D U u
  have h1 : 0 ≤ U - u := by linarith [hu.2]
  have h2 : (U + u) * S ≤ 2 * U * S := by nlinarith [hu.2]
  have h3 : A * (U * S - D) ≤ A / 2 * ((U + u) * S - 2 * D) := by nlinarith
  have h4 : 0 ≤ A / 2 * ((U + u) * S - 2 * D) + B * m := by linarith
  nlinarith [mul_nonneg h1 h4]

/-- Convex case: with A ≥ 0, U S ≥ 2 D and B m ≥ 0, the cap is optimal. -/
theorem cap_optimal_convex {A B m S D U : ℝ} (hA : 0 ≤ A) (hS : 0 ≤ S)
    (hC : 2 * D ≤ U * S) (hBm : 0 ≤ B * m) :
    ∀ u ∈ Icc (0 : ℝ) U, h A B m S D u ≤ h A B m S D U := by
  intro u hu
  have hdiff := h_diff A B m S D U u
  have h1 : 0 ≤ U - u := by linarith [hu.2]
  have h2 : 0 ≤ (U + u) * S - 2 * D := by nlinarith [hu.1]
  have h4 : 0 ≤ A / 2 * ((U + u) * S - 2 * D) + B * m := by
    have := mul_nonneg hA h2
    linarith
  nlinarith [mul_nonneg h1 h4]

/-- Where V is affine (A = 0), below the trigger and above the barrier, a positive premium
and V' > 0 make the cap optimal. -/
theorem cap_optimal_affine {B m S D U : ℝ} (hBm : 0 ≤ B * m) :
    ∀ u ∈ Icc (0 : ℝ) U, h 0 B m S D u ≤ h 0 B m S D U := by
  intro u hu
  unfold h
  have : 0 ≤ U - u := by linarith [hu.2]
  nlinarith [mul_nonneg hBm this]

/-- On the solvency branch, U S - 2 D = (x - 1)(σ²/a1 - 2 c σ σ_L). -/
theorem solvency_branch_gap {x a1 σ σL c : ℝ} (ha1 : a1 ≠ 0) :
    (x - 1) / a1 * σ ^ 2 - 2 * (c * σ * σL * (x - 1)) = (x - 1) * (σ ^ 2 / a1 - 2 * c * σ * σL) := by
  field_simp

/-- The condition σ ≥ 2 a1 c σ_L gives U S ≥ 2 D on the solvency branch, for x ≥ 1. -/
theorem solvency_branch_condition {x a1 σ σL c : ℝ} (ha1 : 0 < a1) (hσ : 0 ≤ σ) (hx : 1 ≤ x)
    (hcond : 2 * a1 * c * σL ≤ σ) :
    2 * (c * σ * σL * (x - 1)) ≤ (x - 1) / a1 * σ ^ 2 := by
  have hgap := solvency_branch_gap (x := x) (σ := σ) (σL := σL) (c := c) ha1.ne'
  have h1 : 0 ≤ σ ^ 2 / a1 - 2 * c * σ * σL := by
    rw [sub_nonneg, le_div_iff₀ ha1]
    nlinarith
  nlinarith [mul_nonneg (by linarith : (0:ℝ) ≤ x - 1) h1]

/-- On the liquidity branch U = (x - a2)/a3 with a2 ≤ 1, and σ ≥ 2 a3 c σ_L gives
U S ≥ 2 D for x ≥ 1. -/
theorem liquidity_branch_condition {x a2 a3 σ σL c : ℝ} (ha3 : 0 < a3) (ha2 : a2 ≤ 1) (hσ : 0 ≤ σ)
    (hx : 1 ≤ x) (hcond : 2 * a3 * c * σL ≤ σ) :
    2 * (c * σ * σL * (x - 1)) ≤ (x - a2) / a3 * σ ^ 2 := by
  have h1 := solvency_branch_condition (x := x) (σ := σ) (σL := σL) (c := c) ha3 hσ hx hcond
  have h2 : (x - 1) / a3 * σ ^ 2 ≤ (x - a2) / a3 * σ ^ 2 := by
    apply mul_le_mul_of_nonneg_right _ (sq_nonneg σ)
    exact div_le_div_of_nonneg_right (by linarith) ha3.le
  linarith

/-- Near a point where the slope at the cap is positive, it stays positive: with the
data continuous, the cap is optimal on a neighbourhood (concave or convex side). -/
theorem slope_positive_near {G : ℝ → ℝ} {x0 : ℝ} (hG : ContinuousAt G x0) (h0 : 0 < G x0) :
    ∀ᶠ x in 𝓝 x0, 0 < G x :=
  hG.eventually (lt_mem_nhds h0)

/-- Control. With V'' strongly negative the cap is not optimal: A = -10, B = m = S = U = 1,
D = 0 gives h(1/10) = 1/20 > -4 = h(1). -/
theorem control_cap_not_optimal_when_very_concave :
    h (-10) 1 1 1 0 1 < h (-10) 1 1 1 0 (1 / 10) := by
  unfold h
  norm_num

/-- Control. Without a premium (m < 0) and with V affine, no exposure beats the cap. -/
theorem control_cap_needs_premium : h 0 1 (-1) 1 0 0 > h 0 1 (-1) 1 0 1 := by
  unfold h
  norm_num

end CapBinding
