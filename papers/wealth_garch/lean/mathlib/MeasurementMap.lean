/-
================================================================================
  The measurement map: regulatory ratios and the capped exposure
  Companion to MEASUREMENT_MAP.tex.
================================================================================

With equity X − L, risk-weighted assets πX, high-quality liquid assets
X − a₃πX and 30-day net outflows a₂L, the solvency and liquidity ratios
rearrange into the two branches of the cap in the state x = X/L, and a
leverage floor l₀ on (X − L)/X into the floor l₀/(1 − l₀) on x − 1. The
identification of risk-weighted assets with πX (weight 0 on the riskless
asset and 1 on the risky asset) is a modelling choice, shown here to be
needed by its control. That regulatory capital equals book equity X − L is
an assumption and is not proved.
-/

import CapGeometry

open Real

namespace MeasurementMap

noncomputable def rwa (w0 w1 p X : ℝ) : ℝ := w0 * ((1 - p) * X) + w1 * (p * X)

noncomputable def hqla (h p X : ℝ) : ℝ := (1 - p) * X + (1 - h) * (p * X)

theorem rwa_two_assets (p X : ℝ) : rwa 0 1 p X = p * X := by
  unfold rwa
  ring

theorem hqla_haircut (a3 p X : ℝ) : hqla a3 p X = X - a3 * p * X := by
  unfold hqla
  ring

theorem solvency_iff {a1 p X L : ℝ} (ha1 : 0 < a1) (hp : 0 < p) (hX : 0 < X) (hL : 0 < L) :
    a1 ≤ (X - L) / (p * X) ↔ p * (X / L) ≤ (X / L - 1) / a1 := by
  have hpX : 0 < p * X := mul_pos hp hX
  rw [le_div_iff₀ hpX, le_div_iff₀ ha1]
  rw [show p * (X / L) * a1 = (a1 * (p * X)) / L by field_simp,
      show X / L - 1 = (X - L) / L by field_simp]
  exact (div_le_div_iff_of_pos_right hL).symm

theorem lcr_iff {a2 a3 p X L : ℝ} (ha2 : 0 < a2) (ha3 : 0 < a3) (hL : 0 < L) :
    1 ≤ hqla a3 p X / (a2 * L) ↔ p * (X / L) ≤ (X / L - a2) / a3 := by
  have ha2L : 0 < a2 * L := mul_pos ha2 hL
  rw [hqla_haircut, le_div_iff₀ ha2L, le_div_iff₀ ha3]
  rw [show p * (X / L) * a3 = (a3 * p * X) / L by field_simp,
      show X / L - a2 = (X - a2 * L) / L by field_simp]
  rw [div_le_div_iff_of_pos_right hL]
  constructor <;> intro h <;> linarith

theorem cap_iff {a1 a2 a3 p X L : ℝ} (ha1 : 0 < a1) (ha2 : 0 < a2) (ha3 : 0 < a3)
    (hp : 0 < p) (hX : 0 < X) (hL : 0 < L) :
    (a1 ≤ (X - L) / (p * X) ∧ 1 ≤ hqla a3 p X / (a2 * L)) ↔
      p * (X / L) ≤ CapGeometry.capU a1 a2 a3 (X / L) := by
  rw [solvency_iff ha1 hp hX hL, lcr_iff ha2 ha3 hL, CapGeometry.capU, le_min_iff]

theorem leverage_of_state {X L : ℝ} (hX : 0 < X) (hL : 0 < L) :
    (X - L) / X = (X / L - 1) / (X / L) := by
  field_simp

theorem leverage_req_iff {l0 X L : ℝ} (h1 : l0 < 1) (hX : 0 < X) (hL : 0 < L) :
    l0 ≤ (X - L) / X ↔ l0 / (1 - l0) ≤ X / L - 1 := by
  have h1' : 0 < 1 - l0 := by linarith
  rw [le_div_iff₀ hX, div_le_iff₀ h1', show X / L - 1 = (X - L) / L by field_simp,
    div_mul_eq_mul_div, le_div_iff₀ hL]
  constructor <;> intro h <;> nlinarith

theorem zvol2_free_of_q {σ σL c q q' x : ℝ} {pol : ℝ → ℝ} (hq : 0 < q) (hq' : 0 < q')
    (hx : 1 < x) : CapGeometry.zvol2 σ σL c q pol x = CapGeometry.zvol2 σ σL c q' pol x := by
  rw [CapGeometry.zvol2_eq hq hx, CapGeometry.zvol2_eq hq' hx]

theorem control_rwa_needs_zero_weight_on_riskless : rwa (1 / 5) 1 (1 / 2) 1 ≠ (1 / 2) * 1 := by
  unfold rwa
  norm_num

theorem control_solvency_needs_positive_exposure :
    ¬ ((1 / 2 : ℝ) ≤ (2 - 1) / (0 * 2)) ∧ 0 * ((2 : ℝ) / 1) ≤ ((2 : ℝ) / 1 - 1) / (1 / 2) := by
  norm_num

theorem control_lcr_needs_positive_runoff :
    ¬ ((1 : ℝ) ≤ hqla (1 / 2) (1 / 2) 2 / (0 * 1)) ∧
      (1 / 2 : ℝ) * ((2 : ℝ) / 1) ≤ ((2 : ℝ) / 1 - 0) / (1 / 2) := by
  unfold hqla
  norm_num

theorem control_leverage_needs_l0_lt_one :
    ¬ ((1 : ℝ) ≤ (2 - 1) / 2) ∧ (1 : ℝ) / (1 - 1) ≤ (2 : ℝ) / 1 - 1 := by
  norm_num

end MeasurementMap
