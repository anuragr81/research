/-
================================================================================
  Hirtle (2016), Bank Holding Company Dividends and Repurchases during the
  Financial Crisis, FRBNY Staff Report 666 (revised April 2016)
  Companion to lit/hirtle_2016.
================================================================================

`Rounds n d r` says that n out of d, as a percentage, rounds to r. The file
checks every percentage of Tables 2 and 3 against its counts, the cell sums
of Table 1, and the shares stated in the text (pp. 9-10, 18). The Cox and
Heckman estimations are not formalised.
-/

import Mathlib

namespace Hirtle

def Rounds (n d r : ℚ) : Prop := |100 * n / d - r| ≤ 1 / 2

theorem table1_sums :
    (63 : ℕ) + 10 = 73 ∧ (2 : ℕ) + 3 = 5 ∧ (63 : ℕ) + 2 = 65 ∧ (10 : ℕ) + 3 = 13 ∧ (73 : ℕ) + 5 = 78 ∧
      Rounds 63 78 81 := by
  unfold Rounds
  norm_num [abs_le]

theorem table2_percentages :
    Rounds 48 66 73 ∧ Rounds 30 66 45 ∧ Rounds 30 48 63 ∧
      Rounds 26 42 62 ∧ Rounds 17 42 40 ∧ Rounds 17 26 65 ∧
      Rounds 22 24 92 ∧ Rounds 13 24 54 ∧ Rounds 13 22 59 ∧
      (42 : ℕ) + 24 = 66 ∧ (26 : ℕ) + 22 = 48 ∧ (17 : ℕ) + 13 = 30 := by
  unfold Rounds
  norm_num [abs_le]

theorem table3_percentages :
    Rounds 23 33 70 ∧ Rounds 19 33 58 ∧ Rounds 25 33 76 ∧ Rounds 11 33 33 ∧
      Rounds 17 25 68 ∧ Rounds 15 25 60 ∧ Rounds 9 17 53 ∧ Rounds 2 17 12 ∧
      Rounds 6 8 75 ∧ Rounds 4 8 50 ∧ Rounds 16 16 100 ∧ Rounds 9 16 56 := by
  unfold Rounds
  norm_num [abs_le]

theorem text_shares :
    (26 : ℚ) / 42 < 2 / 3 ∧ (30 : ℚ) / 66 < 1 / 2 ∧ (9 / 10 : ℚ) < 22 / 24 ∧ (24 : ℕ) - 22 = 2 := by
  norm_num

theorem control_rounding_rejects_wrong_percentage : ¬ Rounds 30 66 46 := by
  unfold Rounds
  norm_num [abs_le]

theorem control_rounding_rejects_wrong_count : ¬ Rounds 9 16 57 := by
  unfold Rounds
  norm_num [abs_le]

end Hirtle
