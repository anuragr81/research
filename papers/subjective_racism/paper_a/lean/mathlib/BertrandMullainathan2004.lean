import Mathlib

/-! Bertrand and Mullainathan (2004), "Are Emily and Greg More Employable Than
Lakisha and Jamal? A Field Experiment on Labor Market Discrimination", *American
Economic Review* 94(4), pp. 991–1013.

What is formalised.
* The arithmetic of Tables 1 and 4 and the Spearman correlations of Table 8
  for the two groups where the printed table has no tie in callback rates once
  the listed order is used (a reading recorded as BM-D2).
* The logic of the Table 8 test, our reading: the social background hypothesis,
  that callback rises with the class a name conveys, predicts a concordant
  ranking of callback and mother's education across names. Under group
  sufficiency, where an employer's inference depends on the group only, the
  prediction is no association at all rather than a positive one.
* The logic of the Table 6 test, our reading, in a linear predictor of class from
  group and address: with a common weight on the address, the group gap in
  predicted class does not depend on the address. The paper states the
  prediction that the gap should be smaller at better addresses; it states no
  model, and the predictor here is ours. -/

namespace BertrandMullainathan2004

/-! ### Tables 1 and 4 -/

/-- Table 1, row 1. Callback rates 9.65 and 6.45 percent: a gap of 3.20 points,
a ratio between 1.49 and 1.50, and, at 0.4 points per year of experience
(p. 998), eight years. -/
theorem table1_gap :
    (965 : ℚ) / 100 - 645 / 100 = 32 / 10 ∧
      (149 : ℚ) / 100 < (965 / 100) / (645 / 100) ∧ (965 / 100) / (645 / 100) < (150 : ℚ) / 100 ∧
      (32 / 10 : ℚ) / (4 / 10) = 8 := by
  norm_num

/-- Table 4, Panel A. White names gain 2.29 points from a higher-quality resume,
African-American names 0.51 points. -/
theorem table4_returns :
    (1079 : ℚ) / 100 - 850 / 100 = 229 / 100 ∧ (670 : ℚ) / 100 - 619 / 100 = 51 / 100 := by
  norm_num

/-! ### Table 8, Spearman correlations -/

/-- The Spearman coefficient from rank differences, `1 − 6 Σ d² / (n (n² − 1))`,
valid when neither ranking has ties. -/
def spearman {n : ℕ} (r s : Fin n → ℚ) : ℚ :=
  1 - 6 * (∑ i, (r i - s i) ^ 2) / (n * ((n : ℚ) ^ 2 - 1))

/-- African-American male names in Table 8 order (Rasheed, Kareem, Darnell,
Tyrone, Hakim, Jamal, Leroy, Jermaine; Tremayne has no education data). Ranks
by callback rate and by mother's education. -/
def aaMaleCallbackRank : Fin 8 → ℚ := ![1, 2, 3, 4, 5, 6, 7, 8]
def aaMaleEducRank : Fin 8 → ℚ := ![8, 5, 4, 3, 6, 7, 1, 2]

/-- Table 8: the correlation for African-American males is `−25/42 ≈ −0.595`. -/
theorem table8_aa_male : spearman aaMaleCallbackRank aaMaleEducRank = -25 / 42 := by
  unfold spearman aaMaleCallbackRank aaMaleEducRank
  simp [Fin.sum_univ_eight]
  norm_num

/-- African-American female names in Table 8 order (Aisha, Keisha, Tamika,
Lakisha, Tanisha, Latoya, Kenya, Latonya, Ebony), callback ranks in listed order
(BM-D2), and ranks of mother's education. -/
def aaFemaleCallbackRank : Fin 9 → ℚ := ![1, 2, 3, 4, 5, 6, 7, 8, 9]
def aaFemaleEducRank : Fin 9 → ℚ := ![9, 7, 4, 3, 5, 2, 8, 1, 6]

/-- Table 8: the correlation for African-American females is `−23/60 ≈ −0.383`. -/
theorem table8_aa_female : spearman aaFemaleCallbackRank aaFemaleEducRank = -23 / 60 := by
  unfold spearman aaFemaleCallbackRank aaFemaleEducRank
  simp [Fin.sum_univ_succ]
  norm_num

/-! ### The logic of the Table 8 test -/

/-- The sign of a real number. -/
noncomputable def sgn (t : ℝ) : ℝ := if 0 < t then 1 else if t < 0 then -1 else 0

/-- The concordance of two rankings over names, `Σ_i Σ_j sgn(x_i − x_j) sgn(y_i − y_j)`,
twice Kendall's numerator. -/
noncomputable def concord {n : ℕ} (x y : Fin n → ℝ) : ℝ :=
  ∑ i, ∑ j, sgn (x i - x j) * sgn (y i - y j)

lemma sgn_strictMono {f : ℝ → ℝ} (hf : StrictMono f) (a b : ℝ) :
    sgn (f a - f b) = sgn (a - b) := by
  unfold sgn
  rcases lt_trichotomy a b with h | h | h
  · have := hf h
    simp [show ¬ (0 < f a - f b) by linarith, show f a - f b < 0 by linarith,
      show ¬ (0 < a - b) by linarith, show a - b < 0 by linarith]
  · subst h; simp
  · have := hf h
    simp [show 0 < f a - f b by linarith, show 0 < a - b by linarith]

lemma sgn_mul_self_nonneg (t : ℝ) : 0 ≤ sgn t * sgn t := mul_self_nonneg _

lemma sgn_mul_self_pos {t : ℝ} (ht : t ≠ 0) : 0 < sgn t * sgn t := by
  unfold sgn
  rcases lt_or_gt_of_ne ht with h | h
  · simp [h, show ¬ (0 < t) by linarith]
  · simp [h]

/-- The social background hypothesis, our reading. If callback is a strictly
increasing function of the class a name conveys, and mother's education is that
class, the two rankings are concordant: the concordance is positive whenever two
names differ in mother's education. -/
theorem background_concordant {n : ℕ} {f : ℝ → ℝ} (hf : StrictMono f) (x : Fin n → ℝ)
    {i j : Fin n} (hij : x i ≠ x j) : 0 < concord x (fun k => f (x k)) := by
  unfold concord
  simp only [sgn_strictMono hf]
  have hpos : 0 < sgn (x i - x j) * sgn (x i - x j) := sgn_mul_self_pos (sub_ne_zero.mpr hij)
  have hrow : ∀ k, 0 ≤ ∑ l, sgn (x k - x l) * sgn (x k - x l) :=
    fun k => Finset.sum_nonneg (fun l _ => sgn_mul_self_nonneg _)
  have hi : 0 < ∑ l, sgn (x i - x l) * sgn (x i - x l) :=
    lt_of_lt_of_le hpos (Finset.single_le_sum (f := fun l => sgn (x i - x l) * sgn (x i - x l))
      (fun l _ => sgn_mul_self_nonneg _) (Finset.mem_univ j))
  exact lt_of_lt_of_le hi (Finset.single_le_sum
    (f := fun k => ∑ l, sgn (x k - x l) * sgn (x k - x l)) (fun k _ => hrow k) (Finset.mem_univ i))

/-- Control: under group sufficiency the prediction is not a positive association.
If an employer's callback depends on the group only, it is constant across the
names of one group, and the concordance with mother's education is zero. -/
theorem control_group_sufficiency_zero {n : ℕ} (x : Fin n → ℝ) (c : ℝ) :
    concord x (fun _ => c) = 0 := by
  unfold concord sgn
  simp

/-- Table 8 rejects a strictly increasing map from mother's education to callback
for African-American males: Rasheed (77.3, 3.0) and Leroy (53.3, 9.4) are
discordant. -/
theorem table8_rejects_monotone :
    ¬ ∃ f : ℝ → ℝ, StrictMono f ∧ f (773 / 10) = 3 ∧ f (533 / 10) = 94 / 10 := by
  rintro ⟨f, hf, h1, h2⟩
  have := hf (show (533 / 10 : ℝ) < 773 / 10 by norm_num)
  rw [h1, h2] at this
  norm_num at this

/-! ### The logic of the Table 6 test, our reading

A linear predictor of class from group `g` and address quality `a`:
`(1 − w_g) μ_g + w_g a`, with `w_g` the weight on the address for group `g`. -/

/-- The predicted class of a member of a group with mean `μ` and address weight
`w`, at address quality `a`. -/
def pred (μ w a : ℝ) : ℝ := (1 - w) * μ + w * a

/-- With a common weight on the address, the group gap in predicted class is
`(1 − w)(μ_W − μ_B)` at every address, so a better address does not narrow it. -/
theorem gap_indep_address (μW μB w a a' : ℝ) :
    pred μW w a - pred μB w a = (1 - w) * (μW - μB) ∧
      pred μW w a - pred μB w a = pred μW w a' - pred μB w a' := by
  unfold pred; constructor <;> ring

/-- When the address shows class fully (`w = 1`), the gap is zero. -/
theorem gap_vanishes_class_shown (μW μB a : ℝ) : pred μW 1 a - pred μB 1 a = 0 := by
  unfold pred; ring

/-- Control: with different weights on the address by group, as when the group
variances differ, the gap does move with the address. -/
theorem control_unequal_weights_gap_moves {μW μB wW wB a a' : ℝ} (hw : wW ≠ wB) (ha : a ≠ a') :
    pred μW wW a - pred μB wB a ≠ pred μW wW a' - pred μB wB a' := by
  unfold pred
  intro h
  have : (wW - wB) * (a - a') = 0 := by linarith
  rcases mul_eq_zero.mp this with h1 | h1
  · exact hw (by linarith)
  · exact ha (by linarith)

/-! ### Table 1, every row

Callback rates in percent for White and African-American names, the printed ratio
and the printed difference. -/

/-- Rows 2, 4, 5 and 7 (Chicago, females, females in administrative jobs, males):
the differences are exact and the ratios round to the printed 1.49, 1.49, 1.60, 1.52. -/
theorem table1_rows :
    (806 : ℚ) / 100 - 540 / 100 = 266 / 100 ∧ (1485 : ℚ) / 1000 ≤ 806 / 540 ∧ (806 : ℚ) / 540 < 1495 / 1000 ∧
    (989 : ℚ) / 100 - 663 / 100 = 326 / 100 ∧ (1485 : ℚ) / 1000 ≤ 989 / 663 ∧ (989 : ℚ) / 663 < 1495 / 1000 ∧
    (1046 : ℚ) / 100 - 655 / 100 = 391 / 100 ∧ (1595 : ℚ) / 1000 ≤ 1046 / 655 ∧ (1046 : ℚ) / 655 < 1605 / 1000 ∧
    (887 : ℚ) / 100 - 583 / 100 = 304 / 100 ∧ (1515 : ℚ) / 1000 ≤ 887 / 583 ∧ (887 : ℚ) / 583 < 1525 / 1000 := by
  norm_num

/-- BM-D9: the Boston row prints a difference of 4.05 points, but 11.63 − 7.76 = 3.87.
The ratio 1.50 is consistent with the rates. -/
theorem table1_boston_discrepancy :
    (1163 : ℚ) / 100 - 776 / 100 = 387 / 100 ∧ (1163 : ℚ) / 100 - 776 / 100 ≠ 405 / 100 ∧
      (1495 : ℚ) / 1000 ≤ 1163 / 776 ∧ (1163 : ℚ) / 776 < 1505 / 1000 := by
  norm_num

/-- BM-D9: the row for females in sales prints a ratio of 1.22, while 8.37/6.83 lies in
`[1.225, 1.226)`, which rounds to 1.23 (or 1.22 only from unrounded rates). -/
theorem table1_sales_ratio : (1225 : ℚ) / 1000 ≤ 837 / 683 ∧ (837 : ℚ) / 683 < 1226 / 1000 := by
  norm_num

/-- Table 1, the numbers of resumes: each column's cells add up. -/
theorem table1_counts :
    1352 + 1083 = 2435 ∧ 1860 + 575 = 2435 ∧ 1358 + 502 = 1860 ∧
      1886 + 549 = 2435 ∧ 1359 + 527 = 1886 ∧ 2435 + 2435 = 4870 ∧ 2446 + 2424 = 4870 := by
  norm_num

/-- PDF p. 998: one callback per about 10 ads for White names and per about 15 for
African-American names. -/
theorem ads_per_callback :
    (10 : ℚ) < 100 / 9.65 ∧ (100 : ℚ) / 9.65 < 10.5 ∧ (15 : ℚ) < 100 / 6.45 ∧ (100 : ℚ) / 6.45 < 15.6 := by
  norm_num

/-! ### Table 2, the classification of ads -/

/-- An ad's outcome by the numbers of White and African-American callbacks. -/
inductive Treatment | equal | whitesFavored | blacksFavored
  deriving DecidableEq

def classify (w b : ℕ) : Treatment :=
  if w = b then .equal else if b < w then .whitesFavored else .blacksFavored

/-- The note to Table 2: the nine outcomes fall into the three classes as stated. -/
theorem table2_classes :
    classify 0 0 = .equal ∧ classify 1 1 = .equal ∧ classify 2 2 = .equal ∧
    classify 1 0 = .whitesFavored ∧ classify 2 0 = .whitesFavored ∧ classify 2 1 = .whitesFavored ∧
    classify 0 1 = .blacksFavored ∧ classify 0 2 = .blacksFavored ∧ classify 1 2 = .blacksFavored := by
  decide

/-- Table 2: the counts add to the class totals and to 1,323 ads, and each printed
percentage is its count over 1,323, rounded. -/
theorem table2_counts :
    1103 + 46 + 17 = 1166 ∧ 74 + 19 + 18 = 111 ∧ 33 + 6 + 7 = 46 ∧ 1166 + 111 + 46 = 1323 ∧
    |(1166 : ℚ) / 1323 * 100 - 88.13| < 0.005 ∧ |(111 : ℚ) / 1323 * 100 - 8.39| < 0.005 ∧
    |(46 : ℚ) / 1323 * 100 - 3.48| < 0.005 ∧ |(1103 : ℚ) / 1323 * 100 - 83.37| < 0.005 ∧
    |(74 : ℚ) / 1323 * 100 - 5.59| < 0.005 ∧ |(33 : ℚ) / 1323 * 100 - 2.49| < 0.005 ∧
    |(19 : ℚ) / 1323 * 100 - 1.44| < 0.005 ∧ |(6 : ℚ) / 1323 * 100 - 0.45| < 0.005 ∧
    |(17 : ℚ) / 1323 * 100 - 1.28| < 0.005 ∧ |(18 : ℚ) / 1323 * 100 - 1.36| < 0.005 ∧
    |(7 : ℚ) / 1323 * 100 - 0.53| < 0.005 := by
  norm_num [abs_lt]

/-- BM-D3: the parts of "African-Americans Favored" as printed sum to 3.47, not 3.48. -/
theorem table2_rounding : (2.49 : ℚ) + 0.45 + 0.53 = 3.47 := by norm_num

/-! ### Table 4, every cell, and the text on PDF pp. 1000–1001 -/

theorem table4_all :
    (1079 : ℚ) / 850 ≥ 1.265 ∧ (1079 : ℚ) / 850 < 1.275 ∧
    (670 : ℚ) / 619 ≥ 1.075 ∧ (670 : ℚ) / 619 < 1.085 ∧
    (1360 : ℚ) / 100 - 718 / 100 = 642 / 100 ∧ (1360 : ℚ) / 718 ≥ 1.885 ∧ (1360 : ℚ) / 718 < 1.895 ∧
    (860 : ℚ) / 100 - 537 / 100 = 323 / 100 ∧ (860 : ℚ) / 537 ≥ 1.595 ∧ (860 : ℚ) / 537 < 1.605 ∧
    (26 : ℚ) < 229 / 850 * 100 ∧ (229 : ℚ) / 850 * 100 < 28 ∧
    (8 : ℚ) < 51 / 619 * 100 ∧ (51 : ℚ) / 619 * 100 < 9 ∧
    1212 + 1223 = 2435 := by
  norm_num

/-- PDF p. 1003: at 0.054 per unit of the college fraction, ten points more raise the
callback rate by 0.54 points. -/
theorem table6_college : (0.054 : ℚ) * 0.10 * 100 = 0.54 := by norm_num

/-! ### Section IV.A. A race-blind rule gives equal treatment

Callbacks depend on resume quality `q` only, and names are assigned at random, so each
name group gets the same distribution `π` of resumes. -/

noncomputable def expectedCallback {Q : Type*} [Fintype Q] (π : Q → ℝ) (cb : Q → ℝ) : ℝ :=
  ∑ q, π q * cb q

/-- With a race-blind callback rule, randomised names give both groups the same
distribution of resumes, `π_W = π_B`, and so the same expected callback rate. -/
theorem race_blind_equal {Q : Type*} [Fintype Q] {πW πB : Q → ℝ} (cb : Q → ℝ)
    (hπ : πW = πB) : expectedCallback πW cb = expectedCallback πB cb := by
  rw [hπ]

/-- Control: if the rule uses race, here a callback lowered by `d > 0` for one group at
every quality, expected callbacks differ although the resumes are the same. -/
theorem control_race_used {Q : Type*} [Fintype Q] (π : Q → ℝ) (cb : Q → ℝ) {d : ℝ}
    (hd : 0 < d) (hπ : ∑ q, π q = 1) :
    expectedCallback π (fun q => cb q - d) < expectedCallback π cb := by
  unfold expectedCallback
  have : ∑ q, π q * (cb q - d) = ∑ q, π q * cb q - d := by
    simp only [mul_sub, Finset.sum_sub_distrib, ← Finset.sum_mul, hπ, one_mul]
  rw [this]; linarith

/-! ### Section IV.C. The theories against the returns to credentials

In a normal signal-extraction model the weight on a credential is
`a = v/(v + e)`, with `v` the variance of productivity in the group and `e` the noise
in the credential. -/

noncomputable def credWeight (v e : ℝ) : ℝ := v / (v + e)

/-- Phelps and Arrow, as the paper reads them (BM-Q13, BM-Q14): if productivity is
more uncertain for African-Americans, `v_B > v_W`, the same credential moves the
estimate more for them. -/
theorem phelps_larger_update {vW vB e : ℝ} (hW : 0 < vW) (he : 0 < e) (h : vW < vB) :
    credWeight vW e < credWeight vB e := by
  unfold credWeight
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]; nlinarith

/-- Aigner and Cain, as the paper reads them: if the same credential is noisier for
African-Americans, `e_B > e_W`, it moves the estimate less for them. -/
theorem aigner_cain_smaller_update {v eW eB : ℝ} (hv : 0 < v) (hW : 0 < eW) (h : eW < eB) :
    credWeight v eB < credWeight v eW := by
  unfold credWeight
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]; nlinarith

/-- Control: with equal variances and equal noise neither group's credential moves the
estimate more, so neither theory predicts different returns. -/
theorem control_equal_precision (v e : ℝ) : ¬ credWeight v e < credWeight v e := lt_irrefl _

/-- Taste-based employer discrimination (p. 1010): the employer calls back a White
applicant when productivity `y` clears `θ`, and an African-American one when `y − d`
does. The two are treated differently exactly on `θ ≤ y < θ + d`, so the gap closes at
high credentials, and the profit forgone by a rejection, `y − θ`, rises in `y`. -/
theorem taste_gap_region {y θ d : ℝ} :
    ((θ ≤ y) ∧ ¬ (θ ≤ y - d)) ↔ (θ ≤ y ∧ y < θ + d) := by
  constructor
  · rintro ⟨h1, h2⟩; exact ⟨h1, by push_neg at h2; linarith⟩
  · rintro ⟨h1, h2⟩; exact ⟨h1, by push_neg; linarith⟩

/-- Control: without a taste, `d = 0`, no applicant is treated differently. -/
theorem control_no_taste {y θ : ℝ} : ¬ ((θ ≤ y) ∧ ¬ (θ ≤ y - 0)) := by
  rintro ⟨h1, h2⟩; simp at h2; linarith

/-- Lexicographic search (BM-Q15): if the employer stops reading at the name, the
callback for that group is the same function of every credential, so a better resume
earns nothing. -/
theorem lexicographic_zero_return {Q : Type*} (cbB : Q → ℝ) (hlex : ∀ q q', cbB q = cbB q')
    (q q' : Q) : cbB q - cbB q' = 0 := by rw [hlex q q', sub_self]

end BertrandMullainathan2004
