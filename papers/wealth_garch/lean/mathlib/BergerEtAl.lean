/-
================================================================================
  Berger, DeYoung, Flannery, Lee and Öztekin (2008), How Do Large Banking
  Organizations Manage Their Capital Ratios?
  Companion to lit/berger_2008 (Kansas City Fed RWP 08-01).
================================================================================

The partial adjustment model (2): k - DNK = λ (k* - DNK). The file proves
that k is the λ-weighted average of the target and the do-nothing ratio, that
the remaining gap is (1 - λ) times the initial one, and that k lies between
them for λ in [0, 1]. It checks the rows of Table 1 against 100%, footnotes
28 and 29, and the Step 1 to Step 3 differences of Table 3. The GMM
estimation is not formalised.
-/

import Mathlib

namespace BergerEtAl

theorem partial_adjustment_solution {k d t l : ℝ} (h : k - d = l * (t - d)) :
    k = l * t + (1 - l) * d := by
  linarith

theorem gap_closes {k d t l : ℝ} (h : k - d = l * (t - d)) : t - k = (1 - l) * (t - d) := by
  linarith

theorem between {k d t l : ℝ} (h : k - d = l * (t - d)) (h0 : 0 ≤ l) (h1 : l ≤ 1) (hdt : d ≤ t) :
    d ≤ k ∧ k ≤ t := by
  constructor <;> nlinarith

theorem table1_rows_within_rounding :
    ∀ r ∈ ([(38, 35, 26), (62, 18, 20), (143, 37, -80), (64, 70, -33), (67, 101, -68), (104, 108, -112),
      (34, 189, -124), (302, 220, -422), (60, 61, -21), (57, 100, -57), (120, 5, -26), (140, 11, -51),
      (30, 78, -8), (184, 5, -89), (66, 2, 32)] : List (ℤ × ℤ × ℤ)),
      |r.1 + r.2.1 + r.2.2 - 100| ≤ 1 := by
  decide

theorem table1_means : (98 : ℤ) + 69 = 167 ∧ (98 : ℤ) + 69 - 67 = 100 ∧
    (143 : ℤ) + 82 - 125 = 100 ∧ (169 : ℤ) + 88 - 157 = 100 := by
  norm_num

theorem footnote28 : -0.1985 + 0.0109 * 16.87 = (-0.014617 : ℝ) := by
  norm_num

theorem footnote29 :
    |-0.0053 * (1 / 4.1) * 10 - (-0.0129 : ℝ)| < 0.00005 ∧ |(-0.0053 + 0.0109) * (1 / 213.2) * 10 - (0.0003 : ℝ)| < 0.00005 := by
  norm_num [abs_lt]

theorem step3_minus_step1 :
    (0.0864 : ℝ) - 0.0830 = 0.0034 ∧ (0.1440 : ℝ) - 0.1302 = 0.0138 ∧ (0.1694 : ℝ) - 0.1500 = 0.0194 := by
  norm_num

theorem step1_speed_range : (0.48 : ℝ) < 0.4864 ∧ (0.6024 : ℝ) < 0.61 := by
  norm_num

theorem control_overshoot_without_speed_bound :
    ∃ k d t l : ℝ, k - d = l * (t - d) ∧ d ≤ t ∧ t < k := by
  exact ⟨2, 0, 1, 2, by norm_num, by norm_num, by norm_num⟩

theorem control_step1_lower_below_half : (0.4864 : ℝ) < 0.5 := by
  norm_num

end BergerEtAl
