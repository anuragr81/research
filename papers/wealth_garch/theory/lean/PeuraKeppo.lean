/-
================================================================================
  Peura and Keppo (2006), Optimal Bank Capital with Costly Recapitalization
  Companion to lit/peura_keppo_2006.
================================================================================

The closed forms of the general case: the roots d₁± of the characteristic
equation of (A1), the coefficients a₁, a₂ of (A4) from smooth pasting at the
dividend barrier, the value μ/ρ at that barrier (A5), and the dividend
barrier u₀ of (17) without the capital issue option. Notation: p = d₁₊,
m = d₁₋. The impulse value M of (14) and the existence argument of Lemma 4
are not formalised.
-/

import Mathlib
import Identification

open Real

namespace PeuraKeppo

noncomputable def dplus (μ σ ρ : ℝ) : ℝ := (-μ + sqrt (μ ^ 2 + 2 * ρ * σ ^ 2)) / σ ^ 2

noncomputable def dminus (μ σ ρ : ℝ) : ℝ := (-μ - sqrt (μ ^ 2 + 2 * ρ * σ ^ 2)) / σ ^ 2

noncomputable def a1 (p m : ℝ) : ℝ := m / (p * m - p ^ 2)

noncomputable def a2 (p m : ℝ) : ℝ := p / (p * m - m ^ 2)

noncomputable def u0 (p m : ℝ) : ℝ := 2 / (p - m) * log (-m / p)

theorem char_eq {μ σ ρ d : ℝ} (hσ : σ ≠ 0) (hρ : 0 ≤ ρ) (hd : d = dplus μ σ ρ ∨ d = dminus μ σ ρ) :
    σ ^ 2 / 2 * d ^ 2 + μ * d - ρ = 0 := by
  have hs := Real.sq_sqrt (by positivity : (0 : ℝ) ≤ μ ^ 2 + 2 * ρ * σ ^ 2)
  set s := sqrt (μ ^ 2 + 2 * ρ * σ ^ 2) with hsdef
  have hσ2 : σ ^ 2 ≠ 0 := pow_ne_zero 2 hσ
  rcases hd with rfl | rfl
  · have key : σ ^ 2 / 2 * ((-μ + s) / σ ^ 2) ^ 2 + μ * ((-μ + s) / σ ^ 2) - ρ
        = (s ^ 2 - (μ ^ 2 + 2 * ρ * σ ^ 2)) / (2 * σ ^ 2) := by
      field_simp
      ring
    unfold dplus
    rw [← hsdef, key, hs, sub_self, zero_div]
  · have key : σ ^ 2 / 2 * ((-μ - s) / σ ^ 2) ^ 2 + μ * ((-μ - s) / σ ^ 2) - ρ
        = (s ^ 2 - (μ ^ 2 + 2 * ρ * σ ^ 2)) / (2 * σ ^ 2) := by
      field_simp
      ring
    unfold dminus
    rw [← hsdef, key, hs, sub_self, zero_div]

theorem dplus_pos {μ σ ρ : ℝ} (hσ : σ ≠ 0) (hρ : 0 < ρ) : 0 < dplus μ σ ρ := by
  unfold dplus
  have h1 : μ ≤ sqrt (μ ^ 2) := by rw [Real.sqrt_sq_eq_abs]; exact le_abs_self μ
  have h2 : sqrt (μ ^ 2) < sqrt (μ ^ 2 + 2 * ρ * σ ^ 2) :=
    Real.sqrt_lt_sqrt (sq_nonneg μ) (by have := pow_pos (pow_pos (abs_pos.mpr hσ) 1) 2; nlinarith [sq_abs σ])
  exact div_pos (by linarith) (by positivity)

theorem dminus_neg {μ σ ρ : ℝ} (hσ : σ ≠ 0) (hρ : 0 < ρ) : dminus μ σ ρ < 0 := by
  unfold dminus
  have h1 : -μ ≤ sqrt (μ ^ 2) := by rw [Real.sqrt_sq_eq_abs]; exact neg_le_abs μ
  have h2 : sqrt (μ ^ 2) < sqrt (μ ^ 2 + 2 * ρ * σ ^ 2) :=
    Real.sqrt_lt_sqrt (sq_nonneg μ) (by have := pow_pos (pow_pos (abs_pos.mpr hσ) 1) 2; nlinarith [sq_abs σ])
  exact div_neg_of_neg_of_pos (by linarith) (by positivity)

theorem smooth_paste {p m : ℝ} (hp : p ≠ 0) (hm : m ≠ 0) (hpm : p ≠ m) :
    a1 p m * p + a2 p m * m = 1 ∧ a1 p m * p ^ 2 + a2 p m * m ^ 2 = 0 := by
  have h1 : p * m - p ^ 2 ≠ 0 := by
    rw [show p * m - p ^ 2 = p * (m - p) by ring]; exact mul_ne_zero hp (sub_ne_zero.mpr (Ne.symm hpm))
  have h2 : p * m - m ^ 2 ≠ 0 := by
    rw [show p * m - m ^ 2 = m * (p - m) by ring]; exact mul_ne_zero hm (sub_ne_zero.mpr hpm)
  unfold a1 a2
  constructor <;> field_simp <;> ring

theorem coeff_signs {p m : ℝ} (hp : 0 < p) (hm : m < 0) : 0 < a1 p m ∧ a2 p m < 0 := by
  unfold a1 a2
  constructor
  · exact div_pos_of_neg_of_neg hm (by nlinarith)
  · exact div_neg_of_pos_of_neg hp (by nlinarith)

theorem value_at_barrier {μ σ ρ p m b1 b2 : ℝ} (hρ : ρ ≠ 0)
    (hp : σ ^ 2 / 2 * p ^ 2 + μ * p - ρ = 0) (hm : σ ^ 2 / 2 * m ^ 2 + μ * m - ρ = 0)
    (h1 : b1 * p + b2 * m = 1) (h2 : b1 * p ^ 2 + b2 * m ^ 2 = 0) :
    b1 + b2 = μ / ρ := by
  rw [eq_div_iff hρ]
  linear_combination (-b1) * hp + (-b2) * hm + (σ ^ 2 / 2) * h2 + μ * h1

theorem no_issue_barrier {p m : ℝ} (hp : 0 < p) (hm : m < 0) :
    a1 p m * exp (-(p * u0 p m)) + a2 p m * exp (-(m * u0 p m)) = 0 := by
  have hpm : 0 < p - m := by linarith
  have hy : 0 < -m / p := div_pos (by linarith) hp
  have hexp : exp ((p - m) * u0 p m) = (-m / p) ^ 2 := by
    have : (p - m) * u0 p m = 2 * log (-m / p) := by
      unfold u0; field_simp
    rw [this, show 2 * log (-m / p) = log (-m / p) + log (-m / p) by ring, exp_add, exp_log hy]
    ring
  have hsplit : exp (-(m * u0 p m)) = exp (-(p * u0 p m)) * exp ((p - m) * u0 p m) := by
    rw [← exp_add]; ring_nf
  rw [hsplit, hexp]
  have hd1 : p * m - p ^ 2 ≠ 0 := by nlinarith
  have hd2 : p * m - m ^ 2 ≠ 0 := by nlinarith
  have hmp : m - p ≠ 0 := by linarith
  unfold a1 a2
  field_simp
  ring

theorem control_dplus_needs_positive_rho : dplus 1 1 0 = 0 := by
  unfold dplus
  norm_num

theorem control_paste_needs_distinct_roots : a1 1 1 * 1 + a2 1 1 * 1 ≠ 1 := by
  unfold a1 a2
  norm_num

/-! Table 2 (p. 2184), printed values. Finite differences of the dividend barrier u₂
and the issue barrier u₁ from (Δ, K) = (0.08, 0.10): Δ to 0.5 at K = 0.10, K to 0.25
at Δ = 0.08. -/

theorem table2_signs :
    (0 : ℝ) < 3.2 - 2.4 ∧ (0 : ℝ) < 2.7 - 2.4 ∧ (0 : ℝ) < 1.6 - 0.9 ∧ (0.8 : ℝ) - 0.9 < 0 := by
  norm_num

theorem table2_det_neg : ((3.2 : ℝ) - 2.4) * (0.8 - 0.9) - (2.7 - 2.4) * (1.6 - 0.9) < 0 :=
  Identification.det_neg_of_opposite (by norm_num) (by norm_num) (by norm_num) (by norm_num)

theorem control_delay_lowers_trigger_at_high_K : (0.6 : ℝ) - 0.9 < 0 := by
  norm_num

end PeuraKeppo
