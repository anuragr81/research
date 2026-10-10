/-
================================================================================
  Bayraktar, Chevalier, Ly Vath and Wang, arXiv:2603.14557v2
  "Tractable bank capital structure: optimal control under Basel III
  constraints". What lit/bayraktar_2026/ attributes to the paper, in the
  paper's notation (y the asset-to-deposit ratio, π the risky fraction).
================================================================================
-/

import CapGeometry

open Real Filter Topology Set

namespace BCVW

noncomputable def piBar (a1 a2 a3 y : ℝ) : ℝ := min (1 / a1 * (1 - 1 / y)) (1 / a3 * (1 - a2 / y))

noncomputable def drift (r μ μL γ y p : ℝ) : ℝ := y * ((1 - p) * r + p * μ - μL) + γ

theorem piBar_mul_y {a1 a2 a3 y : ℝ} (ha1 : 0 < a1) (ha3 : 0 < a3) (hy : 0 < y) :
    piBar a1 a2 a3 y * y = CapGeometry.capU a1 a2 a3 y := by
  unfold piBar CapGeometry.capU
  rw [min_mul_of_nonneg _ _ hy.le]
  congr 1
  · field_simp
  · field_simp

theorem piBar_at_one {a1 a2 a3 : ℝ} (ha3 : 0 < a3) (ha2 : a2 < 1) : piBar a1 a2 a3 1 = 0 := by
  unfold piBar
  simp only [div_one, sub_self, mul_zero]
  exact min_eq_left (mul_nonneg (by positivity) (by linarith))

theorem diffusion_vanishes_at_one (σ σL c : ℝ) : CapGeometry.sig2 σ σL c 1 0 = 0 := by
  unfold CapGeometry.sig2
  ring

theorem drift_at_one (r μ μL γ rL : ℝ) (hμL : μL = γ + rL) : drift r μ μL γ 1 0 = r - rL := by
  unfold drift
  rw [hμL]
  ring

theorem sig2_decomp (σ σL c y p : ℝ) :
    CapGeometry.sig2 σ σL c y p = (p * σ * y + c * σL * (1 - y)) ^ 2 + (1 - c ^ 2) * σL ^ 2 * (1 - y) ^ 2 := by
  unfold CapGeometry.sig2
  ring

theorem sig2_nondegenerate {σ σL c y p : ℝ} (hc : c ^ 2 < 1) (hσL : σL ≠ 0) (hy : y ≠ 1) :
    0 < CapGeometry.sig2 σ σL c y p := by
  rw [sig2_decomp]
  have h1 : 0 < 1 - c ^ 2 := by linarith
  have h2 : 0 < σL ^ 2 := by positivity
  have h3 : 0 < (1 - y) ^ 2 := by
    have : 1 - y ≠ 0 := sub_ne_zero.mpr (Ne.symm hy)
    positivity
  have := mul_pos (mul_pos h1 h2) h3
  nlinarith [sq_nonneg (p * σ * y + c * σL * (1 - y))]

theorem switching_point_baseline :
    |CapGeometry.xbar (45 / 1000) (5 / 100) (30 / 100) - 1168 / 1000| < 5 / 10000 := by
  unfold CapGeometry.xbar
  rw [abs_lt]
  constructor <;> norm_num

theorem piBar_tendsto {a1 a2 a3 : ℝ} (ha1 : 0 < a1) (ha3 : 0 < a3) :
    Tendsto (piBar a1 a2 a3) atTop (𝓝 (1 / max a1 a3)) := by
  have hinv : Tendsto (fun y : ℝ => 1 / y) atTop (𝓝 0) := by
    simpa using tendsto_inv_atTop_zero
  have h1 : Tendsto (fun y : ℝ => 1 / a1 * (1 - 1 / y)) atTop (𝓝 (1 / a1 * (1 - 0))) :=
    (tendsto_const_nhds.sub hinv).const_mul _
  have h3 : Tendsto (fun y : ℝ => 1 / a3 * (1 - a2 / y)) atTop (𝓝 (1 / a3 * (1 - a2 * 0))) := by
    have : Tendsto (fun y : ℝ => a2 / y) atTop (𝓝 (a2 * 0)) := by
      simpa [div_eq_mul_inv] using hinv.const_mul a2
    exact (tendsto_const_nhds.sub this).const_mul _
  have hm := h1.min h3
  simp only [sub_zero, mul_zero, mul_one] at hm
  have hmin : min (1 / a1) (1 / a3) = 1 / max a1 a3 := by
    rcases le_total a1 a3 with h | h
    · rw [max_eq_right h, min_eq_right (one_div_le_one_div_of_le ha1 h)]
    · rw [max_eq_left h, min_eq_left (one_div_le_one_div_of_le ha3 h)]
  rw [hmin] at hm
  exact hm

theorem u_concave (a1 a2 a3 : ℝ) :
    ConcaveOn ℝ univ (CapGeometry.capU a1 a2 a3) := by
  have h1 : ConcaveOn ℝ univ (fun y : ℝ => (y - 1) / a1) :=
    (LinearMap.concaveOn (LinearMap.id.smulRight (1 / a1) : ℝ →ₗ[ℝ] ℝ) convex_univ).add_const (-1 / a1)
      |>.congr fun y _ => by simp; ring
  have h3 : ConcaveOn ℝ univ (fun y : ℝ => (y - a2) / a3) :=
    (LinearMap.concaveOn (LinearMap.id.smulRight (1 / a3) : ℝ →ₗ[ℝ] ℝ) convex_univ).add_const (-a2 / a3)
      |>.congr fun y _ => by simp; ring
  have := h1.inf h3
  exact this.congr fun y _ => rfl

theorem control_nondegenerate_needs_abs_c_lt_one : CapGeometry.sig2 1 1 1 2 (1 / 2) = 0 := by
  unfold CapGeometry.sig2
  norm_num

theorem control_limit_needs_a1_le_a3 : 1 / max (1 / 2 : ℝ) (1 / 4) ≠ 1 / (1 / 4 : ℝ) := by
  norm_num

end BCVW
