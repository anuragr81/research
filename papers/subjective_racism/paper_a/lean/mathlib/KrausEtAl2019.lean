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

/-! ### The composite measure of class (Materials and Methods)

"a composite of available indicators of social class measured in each dataset
that is standardized using z scores and then averaged". The sample is
`Fin n`, and `x j` is indicator `j` of `k` indicators. -/

open Finset

/-- The sample mean. -/
noncomputable def mean {n : ℕ} (x : Fin n → ℝ) : ℝ := (∑ i, x i) / n

/-- The z score of an indicator, for a given scale `sd` (any positive
normalisation; the theorems do not depend on which). -/
noncomputable def zscore {n : ℕ} (x : Fin n → ℝ) (sd : ℝ) (i : Fin n) : ℝ :=
  (x i - mean x) / sd

/-- The composite, the average of the `k` z scores. -/
noncomputable def composite {n k : ℕ} (x : Fin k → Fin n → ℝ) (sd : Fin k → ℝ) (i : Fin n) : ℝ :=
  (∑ j, zscore (x j) (sd j) i) / k

/-- Each z score has sample mean zero. -/
theorem mean_zscore {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (sd : ℝ) :
    mean (zscore x sd) = 0 := by
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  unfold mean zscore
  rw [← Finset.sum_div, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul, mean, mul_div_cancel₀ _ hn']
  simp

/-- The composite has sample mean zero. -/
theorem mean_composite {n k : ℕ} (hn : 0 < n) (x : Fin k → Fin n → ℝ) (sd : Fin k → ℝ) :
    mean (composite x sd) = 0 := by
  have h : ∀ j, ∑ i, zscore (x j) (sd j) i = 0 := by
    intro j
    have := mean_zscore hn (x j) (sd j)
    have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
    unfold mean at this
    rcases div_eq_zero_iff.mp this with h | h
    · exact h
    · exact absurd h hn'
  unfold mean composite
  rw [← Finset.sum_div, Finset.sum_comm]
  simp [h]

/-- Control. Drop the centring and the average of raw indicators is not mean
zero. Education coded 1 to 4 and an occupation score, averaged raw over two
speakers, has mean 2.5. -/
theorem control_raw_average_not_centered :
    mean (fun i : Fin 2 => ((![1, 3] : Fin 2 → ℝ) i + (![2, 4] : Fin 2 → ℝ) i) / 2) ≠ 0 := by
  simp [mean, Fin.sum_univ_two]
  norm_num

/-! ### The profile correlation of study 4

Accuracy is "average correlations between perceiver ratings of social class
and actual speaker social class". The Pearson correlation of ratings `y` with
class `x`. -/

/-- Covariance sum, `∑ (x − x̄)(y − ȳ)`. -/
noncomputable def cov {n : ℕ} (x y : Fin n → ℝ) : ℝ := ∑ i, (x i - mean x) * (y i - mean y)

/-- The Pearson correlation. -/
noncomputable def corr {n : ℕ} (x y : Fin n → ℝ) : ℝ := cov x y / Real.sqrt (cov x x * cov y y)

theorem mean_affine {n : ℕ} (hn : 0 < n) (a b : ℝ) (y : Fin n → ℝ) :
    mean (fun i => a * y i + b) = a * mean y + b := by
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  unfold mean
  rw [Finset.sum_add_distrib, ← Finset.mul_sum, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]
  field_simp

theorem cov_affine {n : ℕ} (hn : 0 < n) (a b : ℝ) (x y : Fin n → ℝ) :
    cov x (fun i => a * y i + b) = a * cov x y := by
  unfold cov
  rw [mean_affine hn, Finset.mul_sum]
  refine Finset.sum_congr rfl (fun i _ => ?_)
  ring

theorem cov_affine_self {n : ℕ} (hn : 0 < n) (a b : ℝ) (y : Fin n → ℝ) :
    cov (fun i => a * y i + b) (fun i => a * y i + b) = a ^ 2 * cov y y := by
  unfold cov
  rw [mean_affine hn, Finset.mul_sum]
  refine Finset.sum_congr rfl (fun i _ => ?_)
  ring

/-- The rating scale does not matter. Any increasing affine recoding of the
ratings, such as a different ladder, leaves the correlation unchanged. -/
theorem corr_affine_pos {n : ℕ} (hn : 0 < n) {a : ℝ} (ha : 0 < a) (b : ℝ) (x y : Fin n → ℝ) :
    corr x (fun i => a * y i + b) = corr x y := by
  unfold corr
  rw [cov_affine hn, cov_affine_self hn]
  have : cov x x * (a ^ 2 * cov y y) = a ^ 2 * (cov x x * cov y y) := by ring
  rw [this, Real.sqrt_mul (sq_nonneg a), Real.sqrt_sq ha.le]
  rcases eq_or_ne (Real.sqrt (cov x x * cov y y)) 0 with h | h
  · simp [h]
  · field_simp

/-- Control. Drop the sign condition and the correlation flips. A decreasing
recoding of the ratings turns the speech condition's positive accuracy into the
transcript condition's sign. -/
theorem control_corr_neg_scale {n : ℕ} (hn : 0 < n) {a : ℝ} (ha : a < 0) (b : ℝ)
    (x y : Fin n → ℝ) : corr x (fun i => a * y i + b) = - corr x y := by
  unfold corr
  rw [cov_affine hn, cov_affine_self hn]
  have : cov x x * (a ^ 2 * cov y y) = a ^ 2 * (cov x x * cov y y) := by ring
  rw [this, Real.sqrt_mul (sq_nonneg a), Real.sqrt_sq_eq_abs, abs_of_neg ha]
  rcases eq_or_ne (Real.sqrt (cov x x * cov y y)) 0 with h | h
  · simp [h]
  · have ha' : a ≠ 0 := ha.ne
    field_simp

/-! ### Printed numbers that follow from other printed numbers

Bounds on `t / √df` are proved through the squares. -/

theorem lt_div_sqrt {a t d : ℝ} (ha : 0 < a) (hd : 0 < d) (ht : 0 < t) (h : a ^ 2 * d < t ^ 2) :
    a < t / Real.sqrt d := by
  have hs : 0 < Real.sqrt d := Real.sqrt_pos.mpr hd
  rw [lt_div_iff₀ hs]
  have : a * Real.sqrt d = Real.sqrt (a ^ 2 * d) := by
    rw [Real.sqrt_mul (sq_nonneg a), Real.sqrt_sq ha.le]
  rw [this, Real.sqrt_lt' ht]
  exact h

theorem div_sqrt_lt {b t d : ℝ} (hb : 0 < b) (hd : 0 < d) (ht : 0 ≤ t) (h : t ^ 2 < b ^ 2 * d) :
    t / Real.sqrt d < b := by
  have hs : 0 < Real.sqrt d := Real.sqrt_pos.mpr hd
  rw [div_lt_iff₀ hs]
  have : b * Real.sqrt d = Real.sqrt (b ^ 2 * d) := by
    rw [Real.sqrt_mul (sq_nonneg b), Real.sqrt_sq hb.le]
  rw [this, Real.lt_sqrt ht]
  exact h

/-- Study 1. The printed `D = 0.97` with `T(227) = 7.33` is `2t/√df`, the
conversion for two independent groups. -/
theorem study1_D : (965 / 1000 : ℝ) < 2 * (733 / 100) / Real.sqrt 227 ∧
    2 * (733 / 100) / Real.sqrt 227 < 975 / 1000 :=
  ⟨lt_div_sqrt (by norm_num) (by norm_num) (by norm_num) (by norm_num),
    div_sqrt_lt (by norm_num) (by norm_num) (by norm_num) (by norm_num)⟩

/-- Study 1. The one-sample convention `t/√n` over 228 perceivers gives under
0.49, half the printed value (KT-D8). -/
theorem study1_d_one_sample : (733 / 100 : ℝ) / Real.sqrt 228 < 49 / 100 :=
  div_sqrt_lt (by norm_num) (by norm_num) (by norm_num) (by norm_num)

/-- Study 4. `D = 0.44` with `T(282.51) = 3.69` is `2t/√df`. -/
theorem study4_D : (435 / 1000 : ℝ) < 2 * (369 / 100) / Real.sqrt (28251 / 100) ∧
    2 * (369 / 100) / Real.sqrt (28251 / 100) < 445 / 1000 :=
  ⟨lt_div_sqrt (by norm_num) (by norm_num) (by norm_num) (by norm_num),
    div_sqrt_lt (by norm_num) (by norm_num) (by norm_num) (by norm_num)⟩

/-- Study 5. Competence, `T(273) = 11.32`, `D = 0.69`, is `t/√df`. -/
theorem study5_D_competence : (685 / 1000 : ℝ) < (1132 / 100) / Real.sqrt 273 ∧
    (1132 / 100) / Real.sqrt 273 < 695 / 1000 :=
  ⟨lt_div_sqrt (by norm_num) (by norm_num) (by norm_num) (by norm_num),
    div_sqrt_lt (by norm_num) (by norm_num) (by norm_num) (by norm_num)⟩

/-- Study 5. Fit, `T(273) = 10.9`, `D = 0.66`. -/
theorem study5_D_fit : (655 / 1000 : ℝ) < (109 / 10) / Real.sqrt 273 ∧
    (109 / 10) / Real.sqrt 273 < 665 / 1000 :=
  ⟨lt_div_sqrt (by norm_num) (by norm_num) (by norm_num) (by norm_num),
    div_sqrt_lt (by norm_num) (by norm_num) (by norm_num) (by norm_num)⟩

/-- Study 5. Similarity, `T(273) = 11.43`, `D = 0.69`. -/
theorem study5_D_similar : (685 / 1000 : ℝ) < (1143 / 100) / Real.sqrt 273 ∧
    (1143 / 100) / Real.sqrt 273 < 695 / 1000 :=
  ⟨lt_div_sqrt (by norm_num) (by norm_num) (by norm_num) (by norm_num),
    div_sqrt_lt (by norm_num) (by norm_num) (by norm_num) (by norm_num)⟩

/-- Study 5. Hiring, `T(273) = 10.16`, printed `D = 0.62`. The same
conversion gives under 0.615, which rounds to 0.61 (KT-D9). -/
theorem study5_D_hire : (1016 / 100 : ℝ) / Real.sqrt 273 < 615 / 1000 :=
  div_sqrt_lt (by norm_num) (by norm_num) (by norm_num) (by norm_num)

/-- Study 5. Salary, `T(273) = 9.34`, `D = 0.57`. -/
theorem study5_D_salary : (565 / 1000 : ℝ) < (934 / 100) / Real.sqrt 273 ∧
    (934 / 100) / Real.sqrt 273 < 575 / 1000 :=
  ⟨lt_div_sqrt (by norm_num) (by norm_num) (by norm_num) (by norm_num),
    div_sqrt_lt (by norm_num) (by norm_num) (by norm_num) (by norm_num)⟩

/-- Study 5. Bonus, `T(273) = 7.97`, `D = 0.48`. -/
theorem study5_D_bonus : (475 / 1000 : ℝ) < (797 / 100) / Real.sqrt 273 ∧
    (797 / 100) / Real.sqrt 273 < 485 / 1000 :=
  ⟨lt_div_sqrt (by norm_num) (by norm_num) (by norm_num) (by norm_num),
    div_sqrt_lt (by norm_num) (by norm_num) (by norm_num) (by norm_num)⟩

/-- The `t` statistic of a correlation `r` on `df` degrees of freedom,
squared, `t² = r² df / (1 − r²)`. -/
noncomputable def tSq (r df : ℝ) : ℝ := r ^ 2 * df / (1 - r ^ 2)

/-- Study 4, speech. `R = 0.21` on 148 degrees of freedom gives `t` between
2.605 and 2.615, the printed `T(148) = 2.61`. -/
theorem study4_speech_t : (2605 / 1000 : ℝ) ^ 2 < tSq (21 / 100) 148 ∧
    tSq (21 / 100) 148 < (2615 / 1000) ^ 2 := by
  unfold tSq; norm_num

/-- Study 4, transcript. No `|R|` that rounds to 0.22 gives `|t|` below 2.645,
so the printed `R = −0.22` and `T(148) = −2.64` do not agree (KT-D10). -/
theorem study4_transcript_t_mismatch {r : ℝ} (h0 : 215 / 1000 ≤ r) (h1 : r < 225 / 1000) :
    (2645 / 1000 : ℝ) ^ 2 < tSq r 148 := by
  unfold tSq
  have hr : 0 < 1 - r ^ 2 := by nlinarith
  rw [lt_div_iff₀ hr]
  nlinarith

/-- Study 4, 30 s. `R = 0.22` and `T(144) = 2.66` agree within rounding. At
`R = 0.2165`, which rounds to 0.22, `t` lies between 2.655 and 2.665. -/
theorem study4_30s_t : (2655 / 1000 : ℝ) ^ 2 < tSq (2165 / 10000) 144 ∧
    tSq (2165 / 10000) 144 < (2665 / 1000) ^ 2 := by
  unfold tSq; norm_num

/-- Study 1. The perceiver-education correlation `R(227) = 0.161` gives `t`
between 2.45 and 2.47. The printed `P = 0.015` is not checked here. -/
theorem study1_education_t : (245 / 100 : ℝ) ^ 2 < tSq (161 / 1000) 227 ∧
    tSq (161 / 1000) 227 < (247 / 100) ^ 2 := by
  unfold tSq; norm_num

/-- Study 5. The speech gap in perceived class (5.50 − 4.55) exceeds the
transcript gap (4.97 − 4.36) by 0.34, the "especially true when the perceivers
listened" of KT-Q8. -/
theorem study5_class_interaction :
    ((550 : ℝ) / 100 - 455 / 100) - (497 / 100 - 436 / 100) = 34 / 100 := by norm_num

/-- Methods. With the usual normal quantiles 1.96 and 0.8416 (not printed in
the paper), `2 (z₁ + z₂)² / D²` at `D = 0.40` is about 98, under the goal of
100 per condition. -/
theorem power_goal : 2 * ((196 : ℝ) / 100 + 8416 / 10000) ^ 2 / (40 / 100) ^ 2 < 100 := by
  norm_num

/-! ### Hypothesis 3, class as a proxy for merit

"We argue that class cues are used as a proxy for merit." A judgement of
competence is a function `f` of perceived class. -/

/-- If competence is judged by an increasing function of perceived class, the
applicant perceived as higher in class is judged more competent, with no
information on qualifications. -/
theorem proxy_bias {f : ℝ → ℝ} (hf : StrictMono f) {ĉL ĉH : ℝ} (h : ĉL < ĉH) :
    f ĉL < f ĉH := hf h

/-- Control. Drop the dependence on perceived class and the bias goes. A
judgement that ignores class gives both applicants the same competence. -/
theorem control_no_proxy (k ĉL ĉH : ℝ) : (fun _ : ℝ => k) ĉL = (fun _ : ℝ => k) ĉH := rfl

end KrausEtAl2019
