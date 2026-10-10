/-
================================================================================
  Engle and Siriwardane, "Structural GARCH: The Volatility-Leverage
  Connection", Review of Financial Studies 31(2):449-492, 2018. What
  lit/engle_siriwardane_2018/ attributes to the paper, in the paper's
  notation (h variance, r returns, LM the leverage multiplier, φ its exponent,
  γ the GJR asymmetry parameter, ρ the correlation measure of asymmetry).
================================================================================
-/

import Mathlib

open Real

namespace EngleSiriwardane

noncomputable def gjr (ω α γ β r h : ℝ) : ℝ := ω + α * r ^ 2 + γ * r ^ 2 * (if r < 0 then 1 else 0) + β * h

noncomputable def lm (lmBSM φ : ℝ) : ℝ := lmBSM ^ φ

def leverageShare (ratio : ℚ) : ℚ := 1 - ratio

theorem gjr_news_asymmetry {ω α γ β r h : ℝ} (hr : 0 < r) :
    gjr ω α γ β (-r) h - gjr ω α γ β r h = γ * r ^ 2 := by
  unfold gjr
  rw [if_pos (by linarith : -r < 0), if_neg (not_lt.mpr hr.le)]
  ring

theorem equity_variance (lmv rA : ℝ) : (lmv * rA) ^ 2 = lmv ^ 2 * rA ^ 2 := by
  ring

theorem phi_zero_nests_gjr (lmBSM : ℝ) : lm lmBSM 0 = 1 := by
  unfold lm
  exact Real.rpow_zero lmBSM

theorem phi_zero_equity_is_asset (lmBSM hA : ℝ) : lm lmBSM 0 ^ 2 * hA = hA := by
  rw [phi_zero_nests_gjr]
  ring

theorem share_corr : leverageShare (97 / 100) = 3 / 100 := by
  unfold leverageShare
  norm_num

theorem share_gjr : leverageShare (86 / 100) = 14 / 100 := by
  unfold leverageShare
  norm_num

theorem share_market : leverageShare (17 / 100) = 83 / 100 := by
  unfold leverageShare
  norm_num

theorem control_symmetric_without_gamma {ω α β r h : ℝ} (hr : 0 < r) :
    gjr ω α 0 β (-r) h - gjr ω α 0 β r h = 0 := by
  rw [gjr_news_asymmetry hr]
  ring

theorem control_market_share_is_not_eighty : leverageShare (17 / 100) ≠ 80 / 100 := by
  rw [share_market]
  norm_num

end EngleSiriwardane
