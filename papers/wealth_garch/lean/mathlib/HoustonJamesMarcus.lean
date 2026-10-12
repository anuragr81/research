/-
================================================================================
  Houston, James and Marcus (1997), Capital market frictions and the role of
  internal capital markets in banking
  Companion to lit/houston_james_marcus_1997.
================================================================================

The cash-flow sensitivity of loan growth at surplus capital s is a + b s,
with a = 4.53 and b = -34.95 in the first column of Table 3 (p. 148). The
file checks the sensitivity 3.83 at s = 0.02, the surplus of nearly 13
percentage points at which it reaches zero, and the share of holding
companies below the minimum (31 of 954, p. 143). The regressions and the
Heckman fee model are not formalised.
-/

import Mathlib

namespace HoustonJamesMarcus

def sensitivity (a b s : ℝ) : ℝ := a + b * s

theorem sensitivity_anti {a b s t : ℝ} (hb : b < 0) (hst : s < t) :
    sensitivity a b t < sensitivity a b s := by
  unfold sensitivity
  nlinarith

theorem sensitivity_zero {a b : ℝ} (hb : b ≠ 0) : sensitivity a b (-a / b) = 0 := by
  unfold sensitivity
  field_simp
  ring

theorem median_sensitivity :
    sensitivity 4.53 (-34.95) 0.02 = 3.831 ∧ |sensitivity 4.53 (-34.95) 0.02 - 3.83| < 0.005 := by
  unfold sensitivity
  norm_num [abs_lt]

theorem zero_near_13_points :
    sensitivity 4.53 (-34.95) (4.53 / 34.95) = 0 ∧ (0.129 : ℝ) < 4.53 / 34.95 ∧ (4.53 : ℝ) / 34.95 < 0.13 := by
  unfold sensitivity
  norm_num

theorem below_minimum_share : (0.03 : ℝ) < 31 / 954 ∧ (31 : ℝ) / 954 < 0.035 := by
  norm_num

theorem control_sensitivity_rises_if_b_positive :
    sensitivity 4.53 34.95 0 < sensitivity 4.53 34.95 0.02 := by
  unfold sensitivity
  norm_num

theorem control_zero_not_at_13_with_other_sign : sensitivity 4.53 34.95 (4.53 / 34.95) ≠ 0 := by
  unfold sensitivity
  norm_num

end HoustonJamesMarcus
