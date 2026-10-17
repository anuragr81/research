import Mathlib
import Budget
import Platform

/-! # Row M20. Signed comparative statics for the implications table

The sensitivity of M19, the engagement threshold of M18 and the admission
threshold of M15 are closed forms. This file signs their derivatives, so
that each lever in the implications table rests on a theorem. -/

namespace Implications

noncomputable section

/-! ### The sensitivity and the engagement threshold -/

/-- (i) The sensitivity does not rise in the activity curvature. -/
theorem sensitivity_antitone_h (a β h h' : ℝ) (hah : 0 < a + h) (hh : h ≤ h') :
    Budget.sensitivity a h' β ≤ Budget.sensitivity a h β := by
  unfold Budget.sensitivity
  have h1 : 0 < a + h' := by linarith
  gcongr

/-- (ii) The sensitivity does not fall in the budget slope. -/
theorem sensitivity_mono_beta (a h β β' : ℝ) (hah : 0 < a + h) (hβ : 0 ≤ β) (hββ : β ≤ β') :
    Budget.sensitivity a h β ≤ Budget.sensitivity a h β' := by
  unfold Budget.sensitivity
  gcongr

/-- (iii) The engagement threshold `(v − u)/κ` does not fall as the
sensitivity falls, when the baseline value is at least the outside option. -/
theorem threshold_antitone (v u κ κ' : ℝ) (hvu : u ≤ v) (hκ : 0 < κ) (hκκ : κ ≤ κ') :
    (v - u) / κ' ≤ (v - u) / κ := by
  have : 0 ≤ v - u := by linarith
  gcongr

/-- Control for (iii). With the outside option above the baseline value the
threshold is negative and the order reverses. -/
theorem control_threshold_reverses (v u κ κ' : ℝ) (hvu : v < u) (hκ : 0 < κ) (hκκ : κ < κ') :
    (v - u) / κ < (v - u) / κ' := by
  have hneg : v - u < 0 := by linarith
  rw [div_eq_mul_one_div (v - u) κ, div_eq_mul_one_div (v - u) κ']
  exact mul_lt_mul_of_neg_left (one_div_lt_one_div_of_lt hκ hκκ) hneg

/-! ### The admission threshold -/

/-- (iv) The admission threshold does not rise in the willingness to pay. -/
theorem cAdm_antitone_W (W W' n α cI A : ℝ) (hA : 0 ≤ A) (hd : 0 < n * (1 - α))
    (hW : W ≤ W') :
    Platform.cAdm W' n α cI A ≤ Platform.cAdm W n α cI A := by
  unfold Platform.cAdm
  gcongr

/-- (iv) The admission threshold does not rise in the room size. -/
theorem cAdm_antitone_A (W n α cI A A' : ℝ) (hW : 0 ≤ W) (hd : 0 < n * (1 - α))
    (hA : A ≤ A') :
    Platform.cAdm W n α cI A' ≤ Platform.cAdm W n α cI A := by
  unfold Platform.cAdm
  gcongr

/-- (iv) The admission threshold does not fall in the number of host clients. -/
theorem cAdm_mono_n (W n n' α cI A : ℝ) (hW : 0 ≤ W) (hA : 0 ≤ A) (hn : 0 < n)
    (hnn : n ≤ n') (hα : α < 1) :
    Platform.cAdm W n α cI A ≤ Platform.cAdm W n' α cI A := by
  unfold Platform.cAdm
  have h1 : 0 < 1 - α := by linarith
  have hd : 0 < n * (1 - α) := by positivity
  have hle : n * (1 - α) ≤ n' * (1 - α) := by nlinarith
  have : W * A / (n' * (1 - α)) ≤ W * A / (n * (1 - α)) := by gcongr
  linarith

/-- (iv) The admission threshold does not rise in the objective weight. -/
theorem cAdm_antitone_alpha (W n α α' cI A : ℝ) (hW : 0 ≤ W) (hA : 0 ≤ A) (hn : 0 < n)
    (hαα : α ≤ α') (hα' : α' < 1) :
    Platform.cAdm W n α' cI A ≤ Platform.cAdm W n α cI A := by
  unfold Platform.cAdm
  have h1 : 0 < 1 - α' := by linarith
  have hd : 0 < n * (1 - α') := by positivity
  have hle : n * (1 - α') ≤ n * (1 - α) := by nlinarith
  have : W * A / (n * (1 - α)) ≤ W * A / (n * (1 - α')) := by gcongr
  linarith

/-- Control for (iv). With a negative willingness to pay the room size works
the other way. -/
theorem control_cAdm_negative_W (W n α cI A A' : ℝ) (hW : W < 0) (hd : 0 < n * (1 - α))
    (hA : A ≤ A') :
    Platform.cAdm W n α cI A ≤ Platform.cAdm W n α cI A' := by
  unfold Platform.cAdm
  have h1 : W * A' ≤ W * A := mul_le_mul_of_nonpos_left hA hW.le
  have h2 : W * A' / (n * (1 - α)) ≤ W * A / (n * (1 - α)) := by
    rw [div_eq_mul_inv, div_eq_mul_inv]
    exact mul_le_mul_of_nonneg_right h1 (inv_nonneg.2 hd.le)
  linarith

end

end Implications
