import Mathlib

/-! Kraus, Torrez, Park and Ghayebi (2019), "Evidence for the reproduction of
social class in brief speech", PNAS 116(46), in the paper's notation for its
study 1. A perceiver answers "Yes" or "No" to "Is this person a college
graduate?" and accuracy is the proportion of correct responses, benchmarked
against "chance (50%)". `π` is the share of college graduates among the
speakers heard, `H` the hit rate (Yes to a graduate) and `CR` the correct
rejection rate (No to a non-graduate).

The paper states the accuracy measure and the 50% benchmark. The two facts
below that bear on Paper A1 are our reading of that measure, not results of the
paper. A stimulus-blind responder scores 50% only under balance, and a
perceiver who answers from the speaker's race alone scores above 50% whenever
graduation rates differ by race. -/

namespace KrausEtAl2019

/-- Proportion correct, the study 1 accuracy metric, for base rate `π`, hit
rate `H` and correct rejection rate `CR`. -/
def pc (π H CR : ℝ) : ℝ := π * H + (1 - π) * CR

/-- A stimulus-blind responder says "Yes" with probability `g` whatever is
heard, so `H = g` and `CR = 1 − g`. -/
def blindPc (π g : ℝ) : ℝ := pc π g (1 - g)

/-- With a balanced set of speakers, `π = 1/2`, every stimulus-blind responder
scores exactly 1/2. -/
theorem chance_half_balanced (g : ℝ) : blindPc (1 / 2) g = 1 / 2 := by
  unfold blindPc pc; ring

/-- A stimulus-blind responder who says "Yes" half the time scores 1/2 at
every base rate. -/
theorem chance_half_unbiased (π : ℝ) : blindPc π (1 / 2) = 1 / 2 := by
  unfold blindPc pc; ring

/-- Control. Drop both balance conditions and 50% is no longer chance. With
more graduates than not, `π > 1/2`, a responder who always says "Yes" scores
`π`, above 50%, without hearing anything. -/
theorem control_majority_beats_half {π : ℝ} (h : 1 / 2 < π) : 1 / 2 < blindPc π 1 := by
  unfold blindPc pc; linarith

/-! ### A perceiver who answers from race alone

Two groups of speakers, shares `w` and `1 − w`, with graduation rates `p₁` and
`p₂`. The perceiver says "Yes" exactly when the speaker is heard as a member of
group 1, so the hit rate within group 1 is 1 and the correct rejection rate
within group 2 is 1. This is class read from the group alone, the inference
Paper A1 assumes. -/

/-- Proportion correct of the race-only perceiver. -/
def raceJudgePc (w p₁ p₂ : ℝ) : ℝ := w * p₁ + (1 - w) * (1 - p₂)

/-- The race-only perceiver is a special case of `pc`. Its hit rate is the
group-1 share among graduates and its correct rejection rate the group-2 share
among non-graduates. -/
theorem raceJudgePc_eq_pc {w p₁ p₂ : ℝ} (hπ0 : w * p₁ + (1 - w) * p₂ ≠ 0)
    (hπ1 : 1 - (w * p₁ + (1 - w) * p₂) ≠ 0) :
    raceJudgePc w p₁ p₂ =
      pc (w * p₁ + (1 - w) * p₂) (w * p₁ / (w * p₁ + (1 - w) * p₂))
        ((1 - w) * (1 - p₂) / (1 - (w * p₁ + (1 - w) * p₂))) := by
  unfold raceJudgePc pc
  field_simp

/-- If graduation is more common than not in group 1 and less common in
group 2, the race-only perceiver is above 50% at every group share. -/
theorem race_judge_above_chance {w p₁ p₂ : ℝ} (hw0 : 0 ≤ w) (hw1 : w ≤ 1)
    (h1 : 1 / 2 < p₁) (h2 : p₂ < 1 / 2) : 1 / 2 < raceJudgePc w p₁ p₂ := by
  unfold raceJudgePc
  have ha : 0 < p₁ - 1 / 2 := by linarith
  have hb : 0 < 1 / 2 - p₂ := by linarith
  have hw1' : 0 ≤ 1 - w := by linarith
  rcases le_total (p₁ - 1 / 2) (1 / 2 - p₂) with hab | hab
  · nlinarith [mul_le_mul_of_nonneg_left hab hw1']
  · nlinarith [mul_le_mul_of_nonneg_left hab hw0]

/-- With a balanced set of speakers, the race-only perceiver's excess over 50%
is `2 w (1 − w) (p₁ − p₂)`, proportional to the gap in graduation rates. -/
theorem race_judge_gain {w p₁ p₂ : ℝ} (hbal : w * p₁ + (1 - w) * p₂ = 1 / 2) :
    raceJudgePc w p₁ p₂ - 1 / 2 = 2 * w * (1 - w) * (p₁ - p₂) := by
  unfold raceJudgePc
  linear_combination (-1 + 2 * w) * hbal

/-- Control. Drop the gap in graduation rates and the race-only perceiver is at
chance. With `p₁ = p₂` and a balanced set of speakers, the score is exactly
1/2. -/
theorem control_no_gap_chance {w p : ℝ} (hbal : w * p + (1 - w) * p = 1 / 2) :
    raceJudgePc w p p = 1 / 2 := by
  have := race_judge_gain hbal
  linarith [show 2 * w * (1 - w) * (p - p) = 0 by ring]

end KrausEtAl2019
