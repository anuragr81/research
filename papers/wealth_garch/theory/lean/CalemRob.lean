/-
================================================================================
  Calem and Rob (1996), FEDS 96-12: the facts the theory paper's rows use
  Written for the theory paper on 30 October 2026, with the record
  theory/lit/calem_rob_1999/.
================================================================================

1. Their risk-based standard C* = 0.06 + n (R - R0) bounds the risky share by
   capital: a bank with capital C meets it exactly when R ≤ R0 + (C - 0.06)/n, a bound
   linear in capital with slope 1/n, the form of the solvency branch of the theory
   paper's regulatory cap (A2) with an intercept.
2. Their printed solutions: in Table 3a, column (i), the risky share is 1.00 at the
   lowest capital, 0.05 in between and 0.55 at the requirement, so it falls and then
   rises in capital; Table 4 against Table 3a, column (i), puts the well-capitalized
   bank at 0.60 to 0.65 under a requirement of 0.07, above its 0.55 under 0.06.
-/

import Mathlib

namespace CalemRob

/-- Their rule (13): capital C meets the risk-based standard exactly when the risky
share is at most R0 + (C - 0.06)/n. -/
theorem riskbased_is_linear_bound {n R R0 C : ℝ} (hn : 0 < n) :
    0.06 + n * (R - R0) ≤ C ↔ R ≤ R0 + (C - 0.06) / n := by
  rw [← sub_le_iff_le_add', le_div_iff₀ hn]
  constructor <;> intro h <;> linarith [mul_comm n (R - R0)]

/-- The bound rises in capital with slope 1/n. -/
theorem bound_slope {n R0 C : ℝ} (hn : 0 < n) :
    (R0 + ((C + 1) - 0.06) / n) - (R0 + (C - 0.06) / n) = 1 / n := by
  field_simp
  ring

/-- Table 3a, column (i): 1.00 at C ≤ 0.006, 0.05 on (0.008, 0.028], 0.55 on
(0.056, 0.060]. The risky share falls and then rises in capital. -/
theorem table3a_u_shape : (0.05 : ℝ) < 1.00 ∧ (0.05 : ℝ) < 0.55 := by
  norm_num

/-- Table 4, column (i), against Table 3a, column (i): at the requirement the
well-capitalized bank holds 0.60 to 0.65 under C* = 0.07 and 0.55 under C* = 0.06. -/
theorem higher_requirement_more_risk : (0.55 : ℝ) < 0.60 ∧ (0.55 : ℝ) < 0.65 := by
  norm_num

/-- Control. With n = 0 the standard does not depend on the risky share: capital of
0.06 meets it at R = 1 with R0 = 0, so no bound on R follows. -/
theorem control_bound_needs_positive_slope : 0.06 + (0 : ℝ) * (1 - 0) ≤ 0.06 := by
  norm_num

/-- Control. A monotone schedule has no interior minimum: 1.00, 0.55, 0.05 falls
throughout, unlike Table 3a. -/
theorem control_monotone_is_not_u : ¬ ((0.55 : ℝ) < 0.05) := by
  norm_num

end CalemRob
