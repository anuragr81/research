import Mathlib

namespace EnhancementMap

open Real

theorem capital_ratio_units {K A c : ℝ} (hA : A ≠ 0) (hc : c ≠ 0) : (c * K) / (c * A) = K / A := by
  field_simp

theorem buffer_of_ratio (k f : ℝ) : (k - f) + f = k := by ring

theorem change_free_of_floor (a b f : ℝ) : (b - f) - (a - f) = b - a := by ring

theorem net_earnings {NI R r X : ℝ} (hR : R ≠ 0) : NI / R - r * X = (NI - r * X * R) / R := by
  field_simp

theorem mean_between (a b : ℝ) : min a b ≤ (a + b) / 2 ∧ (a + b) / 2 ≤ max a b := by
  constructor
  · rcases le_total a b with h | h
    · rw [min_eq_left h]; linarith
    · rw [min_eq_right h]; linarith
  · rcases le_total a b with h | h
    · rw [max_eq_right h]; linarith
    · rw [max_eq_left h]; linarith

theorem annualise (v : ℝ) : sqrt (252 * v) = sqrt 252 * sqrt v := by
  rw [sqrt_mul (by norm_num)]

theorem market_scale {X v : ℝ} (hX : 0 ≤ X) : sqrt (X ^ 2 * v) = X * sqrt v := by
  rw [sqrt_mul (sq_nonneg X), sqrt_sq hX]

theorem do_nothing_split {K NI DIV A A' : ℝ} (hA : A ≠ 0) (hA' : A' ≠ 0) :
    (K + NI - DIV) / A' - K / A = (NI - DIV) / A' + K * (1 / A' - 1 / A) := by
  field_simp
  ring

theorem share_unit {h n : ℝ} (h0 : 0 ≤ h) (hn : h ≤ n) (n0 : 0 < n) : 0 ≤ h / n ∧ h / n ≤ 1 := by
  constructor
  · positivity
  · rw [div_le_one n0]; exact hn

theorem size_monotone {a b : ℝ} (ha : 0 < a) (hab : a ≤ b) : log a ≤ log b :=
  log_le_log ha hab

theorem control_market_scale_needs_nonneg : sqrt ((-1 : ℝ) ^ 2 * 1) ≠ (-1) * sqrt 1 := by
  norm_num

theorem control_share_needs_bound : ¬ ((2 : ℝ) / 1 ≤ 1) := by norm_num

theorem control_do_nothing_needs_same_assets : (1 + 1 - 0 : ℝ) / 2 - 1 / 1 ≠ (1 - 0) / 2 := by
  norm_num

end EnhancementMap
