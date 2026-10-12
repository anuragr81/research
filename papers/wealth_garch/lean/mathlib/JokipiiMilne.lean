/-
================================================================================
  Jokipii and Milne (2008), The cyclical behaviour of European bank capital
  buffers
  Companion to lit/jokipii_milne_2008.
================================================================================

The partial adjustment (1a): ΔBUF = d (BUF* - BUF_{t-1}), so the coefficient
on the lagged buffer is 1 - d, and an adjustment rate of 66% per annum is a
lag coefficient of 0.34. The file checks the averages of Table 1: the EU15
average 2.93 is the mean of the fifteen country averages and of the yearly
EU15 values; the RAM10 average 5.14 is the mean of the yearly values, not of
the ten country averages. It also proves that the smallest EU15 average is
the UK's and the largest RAM10 average Slovakia's (JM-D2, JM-D3). The GMM
estimates are not formalised.
-/

import Mathlib

namespace JokipiiMilne

theorem partial_adjustment {b bstar b0 d : ℝ} (h : b - b0 = d * (bstar - b0)) :
    b = d * bstar + (1 - d) * b0 := by
  linarith

theorem rate_66_is_lag_034 : (1 : ℝ) - 0.66 = 0.34 := by
  norm_num

def eu15 : List ℚ :=
  [2.32, 4.56, 4.79, 1.91, 2.97, 3.40, 4.15, 1.92, 4.04, 3.04, 1.87, 2.59, 2.32, 2.65, 1.46]

def ram10 : List ℚ := [2.64, 4.00, 6.56, 4.45, 3.64, 4.76, 6.99, 5.02, 9.18, 6.60]

def eu15Years : List ℚ := [2.51, 2.48, 3.03, 2.66, 2.62, 2.76, 3.80, 3.60]

def ram10Years : List ℚ := [4.36, 5.80, 5.88, 5.81, 4.85, 5.02, 4.75, 4.67]

theorem eu15_average :
    |eu15.sum / 15 - 2.93| ≤ 1 / 200 ∧ |eu15Years.sum / 8 - 2.93| ≤ 1 / 200 := by
  simp only [eu15, eu15Years, List.sum_cons, List.sum_nil]
  norm_num [abs_le]

theorem ram10_average_is_over_years :
    |ram10Years.sum / 8 - 5.14| ≤ 1 / 200 ∧ ¬ |ram10.sum / 10 - 5.14| ≤ 1 / 200 := by
  simp only [ram10, ram10Years, List.sum_cons, List.sum_nil]
  norm_num [abs_le]

theorem eu15_extremes : (∀ x ∈ eu15, (1.46 : ℚ) ≤ x ∧ x ≤ 4.79) ∧ (1.46 : ℚ) < 1.87 := by
  refine ⟨?_, by norm_num⟩
  intro x hx
  simp only [eu15, List.mem_cons, List.not_mem_nil, or_false] at hx
  rcases hx with h | h | h | h | h | h | h | h | h | h | h | h | h | h | h <;> subst h <;> norm_num

theorem ram10_extremes : (∀ x ∈ ram10, (2.64 : ℚ) ≤ x ∧ x ≤ 9.18) ∧ (6.99 : ℚ) < 9.18 := by
  refine ⟨?_, by norm_num⟩
  intro x hx
  simp only [ram10, List.mem_cons, List.not_mem_nil, or_false] at hx
  rcases hx with h | h | h | h | h | h | h | h | h | h <;> subst h <;> norm_num

theorem control_rate_is_not_lag : (1 : ℝ) - 0.66 ≠ 0.66 := by
  norm_num

theorem control_eu15_average_not_303 : ¬ |eu15.sum / 15 - 3.03| ≤ 1 / 200 := by
  simp only [eu15, List.sum_cons, List.sum_nil]
  norm_num [abs_le]

end JokipiiMilne
