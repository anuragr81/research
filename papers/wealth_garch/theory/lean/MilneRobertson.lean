/-
================================================================================
  Milne and Robertson (1996): the facts the theory paper's rows use
  Written for the theory paper on 30 October 2026, with the record
  theory/lit/milne_robertson_1996/ (read as the February 1994 draft).
================================================================================

1. Their HJB (3) is linear in the dividend rate d, so the dividend term d(1 - V')
   is maximised at d = 0 wherever V' > 1: no dividend below the barrier.
2. Their Theorem 2: W = -V''/V' falls in x because
   W'/W = (σ²/2)⁻¹ ((ρ - r) V'/V'' - ρ V/V'), negative when ρ > r, V'/V'' < 0 and
   V/V' > 0. Their text writes V/V' < 0; with V > 0 and V' > 0 the sign is positive,
   which is what the inequality needs (record MR-D3).
-/

import Mathlib

namespace MilneRobertson

/-- Below the barrier, where V' > 1, no dividend rate beats paying nothing. -/
theorem no_dividend_below_barrier {v d : ℝ} (hv : 1 < v) (hd : 0 ≤ d) :
    d * (1 - v) ≤ 0 * (1 - v) := by
  nlinarith

/-- Theorem 2's sign: with ρ > r, V'/V'' < 0 and V/V' > 0, W'/W < 0. -/
theorem prudence_sign {s ρ r p q : ℝ} (hs : 0 < s) (hρr : r < ρ) (hρ : 0 < ρ) (hp : p < 0) (hq : 0 < q) :
    (1 / s) * ((ρ - r) * p - ρ * q) < 0 := by
  have h1 : (ρ - r) * p < 0 := mul_neg_of_pos_of_neg (by linarith) hp
  have h2 : 0 < ρ * q := mul_pos hρ hq
  have h3 : (ρ - r) * p - ρ * q < 0 := by linarith
  exact mul_neg_of_pos_of_neg (by positivity) h3

/-- Control. With the sign their text states, V/V' < 0, the inequality can fail:
s = 1, ρ = 1, r = 0, V'/V'' = -1, V/V' = -2 gives 1 > 0. -/
theorem control_sign_needs_positive_ratio :
    (1 / (1 : ℝ)) * ((1 - 0) * (-1) - 1 * (-2)) = 1 := by
  norm_num

/-- Control. Above the barrier, where V' < 1, a positive dividend beats none. -/
theorem control_dividend_above_barrier : (0 : ℝ) * (1 - 1 / 2) < 1 * (1 - 1 / 2) := by
  norm_num

end MilneRobertson
