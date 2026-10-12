/-
================================================================================
  Gordon and Hirtle (2026), How Do Banks Build Equity Capital?
  Companion to lit/gordon_hirtle_2026.
================================================================================

EMP is earnings (net income plus OCI) minus common stock payouts (dividends
plus repurchases) (Chart 10 notes). The file proves that a negative EMP is a
payout ratio above one when earnings are positive, and is forced when
earnings are not positive; that with no issuance and no other change a
negative EMP lowers equity capital under Equation 1; and checks the
magnitudes stated on pp. 28-29 against Tables 1 to 3. The regressions are
not formalised.
-/

import Mathlib

namespace GordonHirtle

def emp (earn div rep : ℝ) : ℝ := earn - (div + rep)

def equityNext (e0 ni oci cIss cDiv cRep pIss pDiv pRep other : ℝ) : ℝ :=
  e0 + ni + oci + cIss - cDiv - cRep + pIss - pDiv - pRep + other

theorem negEMP_iff_payout_ratio {earn div rep : ℝ} (h : 0 < earn) :
    emp earn div rep < 0 ↔ 1 < (div + rep) / earn := by
  unfold emp
  rw [one_lt_div h]
  constructor <;> intro k <;> linarith

theorem negEMP_of_nonpos_earnings {earn div rep : ℝ} (h : earn ≤ 0) (hp : 0 < div + rep) :
    emp earn div rep < 0 := by
  unfold emp
  linarith

theorem equity_falls_of_negEMP {e0 ni oci cDiv cRep : ℝ} (h : emp (ni + oci) cDiv cRep < 0) :
    equityNext e0 ni oci 0 cDiv cRep 0 0 0 0 < e0 := by
  unfold emp at h
  unfold equityNext
  linarith

theorem persistence_magnitudes :
    (0.45 : ℝ) < 0.097 / 0.21 ∧ (0.097 : ℝ) / 0.21 < 0.47 ∧ (0.99 : ℝ) < 0.208 / 0.21 ∧ (0.208 : ℝ) / 0.21 < 1 := by
  norm_num

theorem equity_sd_magnitudes :
    (0.02 : ℝ) < 0.0265 * 0.910 ∧ (0.0265 : ℝ) * 2.331 < 0.065 ∧
      (0.10 : ℝ) < 0.0265 * 0.910 / 0.21 ∧ (0.0265 : ℝ) * 2.331 / 0.21 < 0.30 := by
  norm_num

theorem shock_sd_magnitudes :
    (0.12 : ℝ) < 0.0092 * 13.313 ∧ (0.0092 : ℝ) * 22.158 < 0.21 ∧
      (0.58 : ℝ) < 0.0092 * 13.313 / 0.21 ∧ (0.0092 : ℝ) * 22.158 / 0.21 < 1 := by
  norm_num

theorem control_ratio_needs_positive_earnings :
    emp (-1) 1 0 < 0 ∧ ¬ (1 < ((1 : ℝ) + 0) / (-1)) := by
  unfold emp
  norm_num

theorem control_lower_persistence_below_half : (0.097 : ℝ) / 0.21 < 0.5 := by
  norm_num

end GordonHirtle
