/-
================================================================================
  Baron (2020), Countercyclical Bank Equity Issuance
  Companion to lit/baron_2020 (accepted manuscript of 13 January 2020).
================================================================================

The first-order condition (A8) of the Online Appendix, with k = Φ/(1 - Φ),
written as a8, is linear in the debt-to-expected-assets ratio L̄. The file
proves that it holds exactly at (A9), L̄ = (γ(1 - σ) + 2σδ - V/Ā)/(2ck² + γ),
that L̄ rises with the funding benefit δ and falls with franchise value V,
and that a zero denominator leaves (A8) unsolved. It checks the Landesbank
magnitudes of p. 5 against Table 8 and the size pattern of Table 4. The
empirical estimates are not formalised.
-/

import Mathlib

namespace Baron

noncomputable def a8 (g s d V Ab c k Lb : ℝ) : ℝ :=
  -(g / (2 * s)) * Ab * (Lb - (1 - s)) + d * Ab - V / (2 * s) - c / s * k ^ 2 * Lb * Ab

noncomputable def a9 (g s d V Ab c k : ℝ) : ℝ :=
  (g * (1 - s) + d * (2 * s) - V / Ab) / (2 * c * k ^ 2 + g)

theorem a8_factor {g s d V Ab c k Lb : ℝ} (hs : 0 < s) (hA : 0 < Ab) :
    a8 g s d V Ab c k Lb = Ab / (2 * s) * ((g * (1 - s) + d * (2 * s) - V / Ab) - Lb * (2 * c * k ^ 2 + g)) := by
  unfold a8
  field_simp
  ring

theorem a8_iff_a9 {g s d V Ab c k Lb : ℝ} (hs : 0 < s) (hA : 0 < Ab) (hD : 0 < 2 * c * k ^ 2 + g) :
    a8 g s d V Ab c k Lb = 0 ↔ Lb = a9 g s d V Ab c k := by
  rw [a8_factor hs hA]
  have hpos : 0 < Ab / (2 * s) := by positivity
  unfold a9
  rw [mul_eq_zero, eq_div_iff hD.ne']
  constructor
  · rintro (h | h)
    · exact absurd h hpos.ne'
    · linarith
  · intro h
    right
    linarith

theorem a9_increasing_in_delta {g s d d' V Ab c k : ℝ} (hs : 0 < s) (hD : 0 < 2 * c * k ^ 2 + g) (hd : d < d') :
    a9 g s d V Ab c k < a9 g s d' V Ab c k := by
  unfold a9
  apply div_lt_div_of_pos_right _ hD
  nlinarith

theorem a9_decreasing_in_V {g s d V V' Ab c k : ℝ} (hA : 0 < Ab) (hD : 0 < 2 * c * k ^ 2 + g) (hV : V < V') :
    a9 g s d V' Ab c k < a9 g s d V Ab c k := by
  unfold a9
  apply div_lt_div_of_pos_right _ hD
  have : V / Ab < V' / Ab := div_lt_div_of_pos_right hV hA
  linarith

theorem landesbank_magnitudes : (2.336 : ℝ) * 0.10 = 0.2336 ∧ (1.357 : ℝ) * 0.10 = 0.1357 := by
  norm_num

theorem table4_size_pattern :
    (1.9 : ℝ) < 4.7 ∧ (4.7 : ℝ) < 7.6 ∧ (7.1 : ℝ) < 7.6 ∧ (7.1 : ℝ) < 10.4 ∧ (6.8 : ℝ) < 10.4 ∧
      (10.2 : ℝ) < 10.6 ∧ (23.3 : ℝ) < 24.1 ∧ (22.9 : ℝ) < 24.1 := by
  norm_num

theorem control_zero_denominator_leaves_a8_unsolved :
    a8 0 1 1 0 1 0 0 (a9 0 1 1 0 1 0 0) ≠ 0 := by
  unfold a8 a9
  norm_num

theorem control_landesbank_coefficients_not_swapped : (1.357 : ℝ) * 0.10 ≠ 0.2336 := by
  norm_num

end Baron
