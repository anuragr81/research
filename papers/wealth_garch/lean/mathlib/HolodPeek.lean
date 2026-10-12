/-
================================================================================
  Holod and Peek (2010), Capital Constraints, Asymmetric Information, and
  Internal Capital Markets in Banking: New Evidence
  Companion to lit/holod_peek_2010 (working paper of 5 December 2006).
================================================================================

CapTrans (p. 14) is dividends with a negative sign plus capital sales and
acquisitions plus other transactions with the parent. Net loan sales (p. 15)
are loan sales less loan purchases. The file checks the components of
Tables 1 and 2 against their totals, the differences quoted on p. 17, and
the ratios of coefficients described on pp. 29 to 30. The regressions are
not formalised.
-/

import Mathlib

namespace HolodPeek

def capTrans (div capSale other : ℝ) : ℝ := -div + capSale + other

def netLoanSales (sales purchases : ℝ) : ℝ := sales - purchases

theorem capTrans_upstream_iff {div capSale other : ℝ} :
    capTrans div capSale other < 0 ↔ capSale + other < div := by
  unfold capTrans
  constructor <;> intro h <;> linarith

theorem table1_captrans :
    capTrans 0.51 0.04 0.22 = -0.25 ∧ capTrans 0.67 0.02 0.09 = -0.56 ∧
      capTrans 0.74 0.02 0.05 = -0.67 ∧ capTrans 0.85 0.01 0.03 = -0.81 ∧
      capTrans 0.93 0.00 0.03 = -0.90 := by
  unfold capTrans
  norm_num

theorem table1_net_loan_sales :
    netLoanSales 0.94 0.66 = 0.28 ∧ netLoanSales 0.80 0.66 = 0.14 ∧
      netLoanSales 0.68 0.70 = -0.02 ∧ netLoanSales 0.58 0.70 = -0.12 ∧
      netLoanSales 0.62 0.74 = -0.12 := by
  unfold netLoanSales
  norm_num

theorem table1_dividends_rise :
    (0.51 : ℝ) < 0.67 ∧ (0.67 : ℝ) < 0.74 ∧ (0.74 : ℝ) < 0.85 ∧ (0.85 : ℝ) < 0.93 := by
  norm_num

theorem table2_captrans_within_rounding :
    |capTrans 0.52 0.06 0.18 - (-0.28)| ≤ 0.01 ∧ |capTrans 0.55 0.04 0.14 - (-0.37)| ≤ 0.01 ∧
      |capTrans 0.60 0.02 0.12 - (-0.45)| ≤ 0.01 ∧ |capTrans 0.73 0.01 0.08 - (-0.64)| ≤ 0.01 ∧
      |capTrans 0.85 0.00 0.04 - (-0.81)| ≤ 0.01 ∧ |capTrans 0.92 0.00 0.04 - (-0.87)| ≤ 0.01 ∧
      |capTrans 0.98 0.00 0.05 - (-0.93)| ≤ 0.01 := by
  unfold capTrans
  norm_num [abs_le]

theorem table2_two_cells_rounded :
    capTrans 0.60 0.02 0.12 ≠ -0.45 ∧ capTrans 0.92 0.00 0.04 ≠ -0.87 := by
  unfold capTrans
  norm_num

theorem p17_differences :
    -0.28 - (-0.93 : ℝ) = 0.65 ∧ 0.34 - (-0.19 : ℝ) = 0.53 := by
  norm_num

theorem table8_almost_three_times :
    (2.8 : ℝ) < 0.0440 / 0.0153 ∧ (0.0440 : ℝ) / 0.0153 < 3 ∧ -0.0440 - (-0.0153 : ℝ) = -0.0287 := by
  norm_num

theorem table10_more_than_twice :
    (2 : ℝ) < 0.0295 / 0.0136 ∧ -0.0295 - (-0.0136 : ℝ) = -0.0159 := by
  norm_num

theorem table9_slightly_more_than_half :
    (1 / 2 : ℝ) < 0.0246 / 0.0440 ∧ (0.0246 : ℝ) / 0.0440 < 0.6 := by
  norm_num

theorem control_dividends_enter_negatively : 0.51 + 0.04 + 0.22 ≠ (-0.25 : ℝ) := by
  norm_num

theorem control_not_three_times : ¬ ((3 : ℝ) ≤ 0.0440 / 0.0153) := by
  norm_num

end HolodPeek
