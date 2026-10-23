/-
================================================================================
  Black and Perold (1992), Theory of constant proportion portfolio insurance
  Companion to lit/black_perold_1992.
================================================================================

The rule (2.1), E = min{m·C, b·W} with cushion C = W − F, in the paper's
notation and numeraire (shares of the reserve asset). The file proves the
exposure-to-cushion ratio on each branch, the limit b of that ratio at the
borrowing limit as wealth grows, the wealth at which the limit starts to bind
(p. 421, W* = F·m/(m − b)), and that the regulatory cap of `CapGeometry` with
a₂ = 0 is this rule with floor one, multiple 1/a₁ and leverage limit 1/a₃.
The log-cushion variance is (E/C)²σ², the Itô form behind Prop. 2, taken as a
definition as `CapGeometry.zvol2` takes its own. The utility foundation of
Prop. 11 is read, not formalised.
-/

import CapGeometry

open Real Filter Topology

namespace BlackPerold

noncomputable def exposure (m b W F : ℝ) : ℝ := min (m * (W - F)) (b * W)

noncomputable def cushionVol2 (σ E C : ℝ) : ℝ := (E / C) ^ 2 * σ ^ 2

theorem multiplier_branch {m b W F : ℝ} (h : m * (W - F) ≤ b * W) (hC : 0 < W - F) :
    exposure m b W F / (W - F) = m := by
  unfold exposure
  rw [min_eq_left h]
  field_simp

theorem multiplier_branch_vol {σ m b W F : ℝ} (h : m * (W - F) ≤ b * W) (hC : 0 < W - F) :
    cushionVol2 σ (exposure m b W F) (W - F) = m ^ 2 * σ ^ 2 := by
  unfold cushionVol2
  rw [multiplier_branch h hC]

theorem limit_branch {m b W F : ℝ} (h : b * W ≤ m * (W - F)) : exposure m b W F = b * W := by
  unfold exposure
  exact min_eq_right h

theorem limit_ratio_tendsto (b F : ℝ) :
    Tendsto (fun W : ℝ => b * W / (W - F)) atTop (𝓝 b) := by
  have hinv : Tendsto (fun W : ℝ => (W - F)⁻¹) atTop (𝓝 0) :=
    tendsto_inv_atTop_zero.comp (tendsto_atTop_add_const_right _ (-F) tendsto_id |>.congr
      (fun W => by simp [sub_eq_add_neg]))
  have h2 := (hinv.const_mul (b * F)).const_add b
  simp only [mul_zero, add_zero] at h2
  refine h2.congr' ?_
  filter_upwards [eventually_gt_atTop F] with W hW
  have hne : W - F ≠ 0 := by linarith
  field_simp
  ring

theorem limit_vol_tendsto (σ b F : ℝ) :
    Tendsto (fun W : ℝ => cushionVol2 σ (b * W) (W - F)) atTop (𝓝 (b ^ 2 * σ ^ 2)) := by
  unfold cushionVol2
  exact ((limit_ratio_tendsto b F).pow 2).mul_const _

theorem binding_wealth {m b W F : ℝ} (hmb : m ≠ b) :
    m * (W - F) = b * W ↔ W = F * m / (m - b) := by
  have hne : m - b ≠ 0 := sub_ne_zero.mpr hmb
  constructor
  · intro h
    rw [eq_div_iff hne]
    linear_combination h
  · intro h
    rw [h]
    field_simp
    ring

theorem cap_is_cppi (a1 a3 x : ℝ) :
    CapGeometry.capU a1 0 a3 x = exposure (1 / a1) (1 / a3) x 1 := by
  unfold CapGeometry.capU exposure
  congr 1 <;> ring

theorem control_ratio_needs_multiplier_branch : exposure 4 1 2 1 / (2 - 1) ≠ 4 := by
  unfold exposure
  norm_num

theorem control_limit_needs_fixed_floor : (1 : ℝ) * 2 / (2 - 2 / 2) ≠ 1 := by
  norm_num

end BlackPerold
