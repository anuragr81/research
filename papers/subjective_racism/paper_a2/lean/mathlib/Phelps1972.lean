import Mathlib

/-! Phelps (1972), the structure Paper A relies on, in the paper's notation.
Qualification `q = α + z + λ` with `z = −β c`, test score `y = q + μ`. `vl` is
`var λ` and `m` is `var μ`. The least-squares prediction of `q` from `y`
within a group is `α + z + a (y − α − z)` with `a = vl/(vl + m)`, eq. (5′) in
levels. -/

namespace Phelps1972

/-- The weight on the test score, eqs. (2) and (5). -/
noncomputable def a (vl m : ℝ) : ℝ := vl / (vl + m)

/-- The predicted qualification of an applicant with score `y`. -/
noncomputable def pred (α z vl m y : ℝ) : ℝ := α + z + a vl m * (y - α - z)

/-- Eqs. (2) and (5). The weight lies strictly between 0 and 1. -/
theorem a_mem {vl m : ℝ} (hv : 0 < vl) (hm : 0 < m) : 0 < a vl m ∧ a vl m < 1 := by
  unfold a
  have h : 0 < vl + m := by linarith
  exact ⟨div_pos hv h, (div_lt_one h).2 (by linarith)⟩

/-- Eq. (5′). The weights on the test and on the group are `vl/(vl + m)` and
`m/(vl + m)`, and they sum to 1. -/
theorem weights_sum {vl m : ℝ} (h : vl + m ≠ 0) : a vl m + m / (vl + m) = 1 := by
  unfold a; field_simp

/-- Case 2. A larger postulated variance gives a larger weight on the test. -/
theorem a_strictMono {v₁ v₂ m : ℝ} (hm : 0 < m) (h1 : 0 ≤ v₁) (h12 : v₁ < v₂) :
    a v₁ m < a v₂ m := by
  unfold a
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith

/-- Case 1. With equal variances the prediction for blacks lies parallel to and
below that for whites, by `β (1 − a)`. -/
theorem case1_parallel {α β vl m : ℝ} (hβ : 0 < β) (hv : 0 < vl) (hm : 0 < m) (y : ℝ) :
    pred α (-β) vl m y - pred α 0 vl m y = -β * (1 - a vl m) ∧
      pred α (-β) vl m y < pred α 0 vl m y := by
  have ha := a_mem hv hm
  constructor
  · unfold pred; ring
  · have : pred α (-β) vl m y - pred α 0 vl m y = -β * (1 - a vl m) := by unfold pred; ring
    nlinarith

/-- Case 2. With a larger variance for blacks, at some high score and every
higher one a black applicant is predicted to exceed a white applicant with the
same score. The compiler showed the sign of `β` is not needed. -/
theorem case2_crossing {α β vW vB m : ℝ} (hW : 0 < vW) (hm : 0 < m)
    (hWB : vW < vB) :
    ∃ Y, ∀ y, Y < y → pred α 0 vW m y < pred α (-β) vB m y := by
  have hgap : 0 < a vB m - a vW m := by linarith [a_strictMono hm hW.le hWB]
  refine ⟨α + β * (1 - a vB m) / (a vB m - a vW m), fun y hy => ?_⟩
  have hd : pred α (-β) vB m y - pred α 0 vW m y
      = -β * (1 - a vB m) + (a vB m - a vW m) * (y - α) := by
    unfold pred; ring
  have : β * (1 - a vB m) < (a vB m - a vW m) * (y - α) := by
    rw [← div_lt_iff₀' hgap]; linarith
  linarith

/-- Control: without a variance difference there is no crossing. With equal
variances the black prediction is below the white one at every score. -/
theorem control_no_crossing_equal_variance {α β v m : ℝ} (hβ : 0 < β) (hv : 0 < v)
    (hm : 0 < m) : ¬ ∃ y, pred α 0 v m y < pred α (-β) v m y := by
  rintro ⟨y, hy⟩
  have := (case1_parallel (α := α) hβ hv hm y).2
  linarith

/-- Control: the weight is not 1. With `m > 0` a high test score never moves
the prediction one for one. -/
theorem control_weight_below_one {vl m : ℝ} (hv : 0 < vl) (hm : 0 < m) : a vl m ≠ 1 :=
  ne_of_lt (a_mem hv hm).2

end Phelps1972
