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

end BertrandMullainathan2004
