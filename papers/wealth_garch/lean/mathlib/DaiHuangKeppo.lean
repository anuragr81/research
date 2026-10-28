/-
================================================================================
  Dai, Huang and Keppo (2019), Opaque bank assets and optimal equity capital
  Companion to lit/dai_huang_keppo_2019, read as chapter 2 of Huang (2021),
  arXiv:2107.02242v1.
================================================================================

Table 2.1 (PDF p. 76) prints elasticities of the dividend barrier u₂ and the
recapitalisation barrier u₁ in seven parameters. An elasticity is a derivative
scaled by θ/u, so for non-zero barriers and parameters the 2×2 elasticity
determinant is the Jacobian determinant times a non-zero factor
(`elasticity_det`, `jacobian_ne_zero_of_elasticity`). The file evaluates the
printed block in the issue delay Δ and the fixed cost K. Both parameters raise
u₂ and lower u₁, so the determinant is non-zero by magnitude, not by the
opposite-direction sign pattern of `Identification.det_neg_of_opposite`.
-/

import Mathlib

namespace DaiHuangKeppo

theorem elasticity_det {J11 J12 J21 J22 t1 t2 y1 y2 : ℝ} (hy1 : y1 ≠ 0) (hy2 : y2 ≠ 0) :
    (J11 * t1 / y1) * (J22 * t2 / y2) - (J12 * t2 / y1) * (J21 * t1 / y2) =
      (J11 * J22 - J12 * J21) * (t1 * t2) / (y1 * y2) := by
  field_simp

theorem jacobian_ne_zero_of_elasticity {J11 J12 J21 J22 t1 t2 y1 y2 : ℝ}
    (hy1 : y1 ≠ 0) (hy2 : y2 ≠ 0)
    (hE : (J11 * t1 / y1) * (J22 * t2 / y2) - (J12 * t2 / y1) * (J21 * t1 / y2) ≠ 0) :
    J11 * J22 - J12 * J21 ≠ 0 := by
  rw [elasticity_det hy1 hy2] at hE
  intro h
  apply hE
  rw [h]
  ring

theorem table21_K_signs : (0 : ℝ) < 0.0017 ∧ (-0.0543 : ℝ) < 0 := by
  norm_num

theorem table21_delay_signs : (0 : ℝ) < 0.0808 ∧ (-0.1444 : ℝ) < 0 := by
  norm_num

theorem table21_det_neg : (0.0808 : ℝ) * (-0.0543) - 0.0017 * (-0.1444) < 0 := by
  norm_num

/-- The elasticity determinant of Table 2.1 in $(\Delta,K)$, exactly. -/
theorem table21_det_value : (0.0808 : ℝ) * (-0.0543) - 0.0017 * (-0.1444) = -103549 / 25000000 := by
  norm_num

theorem control_proportional_columns_singular :
    (0.0808 : ℝ) * (2 * -0.1444) - (2 * 0.0808) * (-0.1444) = 0 := by
  ring

theorem hidden_share : (1 : ℝ) / 3 < (5.21 - 3.11) / 5.21 ∧ (5.21 : ℝ) / 3.11 < 1.7 := by
  constructor <;> norm_num

theorem control_hidden_not_half : ((5.21 : ℝ) - 3.11) / 5.21 < 1 / 2 := by norm_num

end DaiHuangKeppo
