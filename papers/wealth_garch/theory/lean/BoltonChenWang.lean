/-
================================================================================
  Bolton, Chen and Wang (2011), A Unified Theory of Tobin's q, Corporate
  Investment, Financing, and Risk Management
  Companion to lit/bolton_chen_wang_2011.
================================================================================

The first-best investment rate (7) solves the Modigliani-Miller valuation
with q = 1 + θi (8), and at the benchmark parameters gives i ≈ 15.1% and
q ≈ 1.23. The investment-cash sensitivity (21) is the derivative of (14),
and marginal q (23) differs from average q by -(p' - 1)w, with derivative
-p''w. The ODE (13) and the boundary conditions (16)-(20) are not
formalised.
-/

import Mathlib

open Real

namespace BoltonChenWang

noncomputable def ifb (r δ μ θ : ℝ) : ℝ := r + δ - sqrt ((r + δ) ^ 2 - 2 * (μ - (r + δ)) / θ)

theorem ifb_mm_value {r δ μ θ : ℝ} (hθ : θ ≠ 0) (hd : 0 ≤ (r + δ) ^ 2 - 2 * (μ - (r + δ)) / θ) :
    (r + δ - ifb r δ μ θ) * (1 + θ * ifb r δ μ θ) = μ - ifb r δ μ θ - θ * ifb r δ μ θ ^ 2 / 2 := by
  have hs := Real.sq_sqrt hd
  set s := sqrt ((r + δ) ^ 2 - 2 * (μ - (r + δ)) / θ)
  unfold ifb
  have key : s ^ 2 * θ = (r + δ) ^ 2 * θ - 2 * (μ - (r + δ)) := by
    rw [hs]; field_simp
  nlinarith [key]

theorem ifb_baseline :
    0.1511 < ifb 0.06 0.1007 0.18 1.5 ∧ ifb 0.06 0.1007 0.18 1.5 < 0.1512 ∧
      1.2266 < 1 + 1.5 * ifb 0.06 0.1007 0.18 1.5 ∧ 1 + 1.5 * ifb 0.06 0.1007 0.18 1.5 < 1.2268 := by
  have hlo : (0.0095 : ℝ) < sqrt ((0.06 + 0.1007) ^ 2 - 2 * (0.18 - (0.06 + 0.1007)) / 1.5) := by
    rw [Real.lt_sqrt (by norm_num)]; norm_num
  have hhi : sqrt ((0.06 + 0.1007) ^ 2 - 2 * (0.18 - (0.06 + 0.1007)) / 1.5) < (0.0096 : ℝ) := by
    rw [Real.sqrt_lt' (by norm_num)]; norm_num
  unfold ifb
  refine ⟨by linarith, by linarith, by linarith, by linarith⟩

theorem investment_cash_sensitivity {P D : ℝ → ℝ} {w D2 θ : ℝ} (hθ : θ ≠ 0)
    (hP : HasDerivAt P (D w) w) (hD : HasDerivAt D D2 w) (hD0 : D w ≠ 0) :
    HasDerivAt (fun v => (P v / D v - v - 1) / θ) (-(P w * D2) / (θ * D w ^ 2)) w := by
  have h := (((hP.div hD hD0).sub (hasDerivAt_id w)).sub_const 1).div_const θ
  simp only [Pi.sub_apply, Pi.div_apply, id_eq] at h
  exact h.congr_deriv (by field_simp; ring)

theorem sensitivity_pos {p p1 p2 θ : ℝ} (hθ : 0 < θ) (hp : 0 < p) (hp1 : p1 ≠ 0) (hp2 : p2 < 0) :
    0 < -(p * p2) / (θ * p1 ^ 2) :=
  div_pos (by nlinarith) (by positivity)

theorem marginal_q_wedge (p p1 w : ℝ) : p - w * p1 = (p - w) - (p1 - 1) * w := by ring

theorem marginal_q_deriv {P D : ℝ → ℝ} {w D2 : ℝ} (hP : HasDerivAt P (D w) w) (hD : HasDerivAt D D2 w) :
    HasDerivAt (fun v => P v - v * D v) (-(w * D2)) w := by
  have h := hP.sub ((hasDerivAt_id w).mul hD)
  simp only [id_eq] at h
  exact h.congr_deriv (by ring)

theorem control_sensitivity_needs_concavity : ¬ (0 < -((1 : ℝ) * 1) / (1 * 1 ^ 2)) := by
  norm_num

theorem control_ifb_needs_root_condition :
    (2 - ifb 1 1 10 1) * (1 + 1 * ifb 1 1 10 1) ≠ 10 - ifb 1 1 10 1 - 1 * ifb 1 1 10 1 ^ 2 / 2 := by
  have : sqrt ((1 + 1) ^ 2 - 2 * (10 - (1 + 1)) / 1 : ℝ) = 0 := by
    rw [Real.sqrt_eq_zero']; norm_num
  unfold ifb
  rw [this]
  norm_num

end BoltonChenWang
