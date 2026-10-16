import Mathlib

/-! Rothschild and White, "The University in the Marketplace: Some Insights and
Some Puzzles", in Clotfelter and Rothschild (eds.), *Studies of Supply and
Demand in Higher Education*, NBER and University of Chicago Press, 1993,
pp. 11–42, section 1.3, in the chapter's notation. Pages are PDF pages; PDF
page `n` is book page `n + 9`.

Section 1.3 is verbal with one displayed price system. What is formalised is
our reading of its structure. (i) The personalised price system `p_A(c, s)` of
eq. (1), which implements any allocation `A` by charging `∞` at every college
but `A(s)` (PDF p. 9). (ii) Section 1.3.2, where the benefit a college confers
rises with the mean ability of its students and falls with their variance (PDF
pp. 9–10). We take the benefit per student to be `mean − k · variance`, a
tractable special case of the chapter's monotone function, and show that with
four students in two colleges of two the efficient allocation groups similar
abilities. (iii) Section 1.3.3, the four-student diversity example, where a
student's share of the college's output has variance `σ²` when both students
bring the same random input and `σ²/2` when they bring independent ones (PDF
pp. 10–11). (iv) The chapter's remark that an anonymous price system cannot
separate two students who look alike (PDF p. 11). -/

namespace RothschildWhite1993

/-! ### Eq. (1), PDF p. 9 -/

/-- Eq. (1). The price college `c` charges student `s` under the allocation
`A`, with `⊤` for `∞`, a denial of admission. -/
def pA {S C : Type*} [DecidableEq C] (A : S → C) (x : S → ℝ) (c : C) (s : S) : WithTop ℝ :=
  if A s = c then (x s : WithTop ℝ) else ⊤

/-- `p_A` implements `A`. The only college at which student `s` faces a finite
price is `A(s)`. -/
theorem pA_finite_iff {S C : Type*} [DecidableEq C] (A : S → C) (x : S → ℝ) (c : C) (s : S) :
    pA A x c s ≠ ⊤ ↔ c = A s := by
  unfold pA
  by_cases h : A s = c
  · simp [h]
  · simp only [h, if_false, ne_eq, not_true_eq_false, false_iff]
    exact fun h' => h h'.symm

/-- PDF p. 11, our reading. Under an anonymous price system, one that depends
only on the college, two students with the same benefit function have the same
net benefit at every college, so neither can strictly prefer a college the
other strictly ranks below another. -/
theorem anonymous_same_ranking {C : Type*} (p : C → ℝ) (W₁ W₂ : C → ℝ) (hW : W₁ = W₂)
    (c c' : C) (h : W₁ c - p c > W₁ c' - p c') : W₂ c - p c > W₂ c' - p c' := by
  rw [← hW]; exact h

/-- Control: a personalised price system separates the same two students.
With equal benefits `W` at two colleges, prices that charge student 1 less at
college 0 and student 2 less at college 1 give them opposite strict rankings. -/
theorem control_personalised_separates :
    let W : Fin 2 → ℝ := fun _ => 1
    let p : Fin 2 → Fin 2 → ℝ := fun c s => if c = s then 0 else 1
    (W 0 - p 0 0 > W 1 - p 1 0) ∧ (W 1 - p 1 1 > W 0 - p 0 1) := by
  simp

/-! ### Section 1.3.2, benefits of homogeneity

Four students of abilities `a ≤ b ≤ c ≤ d` go to two colleges of two. The
benefit per student is the college's mean ability less `k` times the variance
of ability within it. For a pair `{u, v}` the variance is `((u − v)/2)²`. -/

/-- The variance of ability in a two-student college. -/
noncomputable def pairVar (u v : ℝ) : ℝ := ((u - (u + v) / 2) ^ 2 + (v - (u + v) / 2) ^ 2) / 2

theorem pairVar_eq (u v : ℝ) : pairVar u v = (u - v) ^ 2 / 4 := by
  unfold pairVar; ring

/-- The total benefit of a college with students `u, v`, two students each
receiving `mean − k · variance`. -/
noncomputable def collegeBenefit (k u v : ℝ) : ℝ := 2 * ((u + v) / 2 - k * pairVar u v)

/-- `B(A)` for the allocation pairing `{u, v}` and `{w, z}`. -/
noncomputable def totalBenefit (k u v w z : ℝ) : ℝ := collegeBenefit k u v + collegeBenefit k w z

/-- The total benefit is total ability less `k/2` times the sum of the squared
gaps within the colleges. -/
theorem totalBenefit_eq (k u v w z : ℝ) :
    totalBenefit k u v w z = (u + v + w + z) - k / 2 * ((u - v) ^ 2 + (w - z) ^ 2) := by
  unfold totalBenefit collegeBenefit; rw [pairVar_eq, pairVar_eq]; ring

/-- PDF p. 10. "An efficient price system would necessarily group students of
similar ability together." With a penalty on variance, grouping the two lowest
and the two highest abilities gives at least the benefit of either other
pairing. -/
theorem sorted_pairing_efficient {k a b c d : ℝ} (hk : 0 ≤ k) (hab : a ≤ b) (hbc : b ≤ c)
    (hcd : c ≤ d) :
    totalBenefit k a c b d ≤ totalBenefit k a b c d ∧
      totalBenefit k a d b c ≤ totalBenefit k a b c d := by
  rw [totalBenefit_eq, totalBenefit_eq, totalBenefit_eq]
  have h1 : (a - b) ^ 2 + (c - d) ^ 2 ≤ (a - c) ^ 2 + (b - d) ^ 2 := by nlinarith
  have h2 : (a - b) ^ 2 + (c - d) ^ 2 ≤ (a - d) ^ 2 + (b - c) ^ 2 := by nlinarith
  constructor
  · have := mul_le_mul_of_nonneg_left h1 (by linarith : 0 ≤ k / 2)
    linarith
  · have := mul_le_mul_of_nonneg_left h2 (by linarith : 0 ≤ k / 2)
    linarith

/-- With `b < c` and `k > 0`, sorting is strictly better than the interleaved
pairing. The difference in squared gaps is `2 (c − b)(d − a)`. -/
theorem sorted_pairing_strict {k a b c d : ℝ} (hk : 0 < k) (hab : a ≤ b) (hbc : b < c)
    (hcd : c ≤ d) : totalBenefit k a c b d < totalBenefit k a b c d := by
  rw [totalBenefit_eq, totalBenefit_eq]
  have h1 : (a - b) ^ 2 + (c - d) ^ 2 < (a - c) ^ 2 + (b - d) ^ 2 := by
    nlinarith [mul_pos (show 0 < c - b by linarith) (show 0 < d - a by linarith)]
  have := mul_lt_mul_of_pos_left h1 (by linarith : 0 < k / 2)
  linarith

/-- Control: PDF p. 8, "Allocations differ in efficiency only if there is some
synergy." With no variance penalty every pairing gives the same benefit. -/
theorem control_no_synergy (a b c d : ℝ) :
    totalBenefit 0 a c b d = totalBenefit 0 a b c d ∧
      totalBenefit 0 a d b c = totalBenefit 0 a b c d := by
  rw [totalBenefit_eq, totalBenefit_eq, totalBenefit_eq]; constructor <;> ring

/-! ### Section 1.3.3, benefits of diversity

Students `x₁, x₂` both bring the random input `x̃`, students `y₁, y₂` bring
`ỹ`, and `x̃`, `ỹ` are identically distributed with variance `σ²` and
covariance `γ` (`γ = 0` for the chapter's independence). Students of a college
share its output equally, so a student's share is half the sum of the two
inputs. The variance of `(α x̃ + β ỹ)` is `α² σ² + 2αβγ + β² σ²`. -/

/-- The variance of `α x̃ + β ỹ`. -/
def varComb (σ2 γ α β : ℝ) : ℝ := α ^ 2 * σ2 + 2 * α * β * γ + β ^ 2 * σ2

/-- A student in a college `{x₁, x₂}` receives `(x̃ + x̃)/2 = x̃`, with variance
`σ²`. -/
theorem homogeneous_share_var (σ2 γ : ℝ) : varComb σ2 γ 1 0 = σ2 := by
  unfold varComb; ring

/-- A student in a college `{xᵢ, yⱼ}` receives `(x̃ + ỹ)/2`, with variance
`σ²/2` under independence. -/
theorem mixed_share_var (σ2 : ℝ) : varComb σ2 0 (1 / 2) (1 / 2) = σ2 / 2 := by
  unfold varComb; ring

/-- PDF p. 11. "Clearly, the optimal allocation sends one xᵢ and one yᵢ to each
college." With `σ² > 0`, risk-averse students who care about the variance of
their share are better off in mixed colleges. -/
theorem mixed_better {σ2 : ℝ} (hσ : 0 < σ2) :
    varComb σ2 0 (1 / 2) (1 / 2) < varComb σ2 0 1 0 := by
  rw [mixed_share_var, homogeneous_share_var]; linarith

/-- Control: the gain from mixing needs the inputs not to be perfectly
correlated. With `γ = σ²` the mixed share has variance `σ²`, the same as the
homogeneous one. -/
theorem control_perfect_correlation (σ2 : ℝ) :
    varComb σ2 σ2 (1 / 2) (1 / 2) = varComb σ2 σ2 1 0 := by
  unfold varComb; ring

/-- The two sections pull in opposite directions. Under section 1.3.2 the
variance of ability inside a college is a cost, so similar students are
grouped (`sorted_pairing_efficient`). Under section 1.3.3 the variance of the
shared output falls when unlike inputs are pooled, so unlike students are
mixed (`mixed_better`). Our reading, stated as one fact: a pair of equal
abilities has zero within-college variance. -/
theorem equal_pair_zero_var (u : ℝ) : pairVar u u = 0 := by
  rw [pairVar_eq]; ring

end RothschildWhite1993
