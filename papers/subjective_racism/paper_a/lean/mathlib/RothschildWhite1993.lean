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

/-! ### Section 1.2, the stand-alone test (PDF pp. 5–6)

Products `1, …, N` serve `N` classes of customers. For an output vector `y`,
`y^T` keeps the outputs in `T` and sets the others to zero. There is no
cross-subsidisation if `C(y^T) ≥ r(y^T)` for every subset `T`. -/

/-- `y^T`. -/
def restrict {N : ℕ} (y : Fin N → ℝ) (T : Finset (Fin N)) : Fin N → ℝ :=
  fun i => if i ∈ T then y i else 0

/-- The chapter's definition of no cross-subsidisation. -/
def noCrossSubsidy {N : ℕ} (C r : (Fin N → ℝ) → ℝ) (y : Fin N → ℝ) : Prop :=
  ∀ T : Finset (Fin N), r (restrict y T) ≤ C (restrict y T)

/-- PDF p. 6. Prices carry a cross-subsidy exactly when an entrant could serve
some subset of the customers at the incumbent's revenue and make a profit. -/
theorem noCrossSubsidy_iff_no_profitable_entry {N : ℕ} (C r : (Fin N → ℝ) → ℝ) (y : Fin N → ℝ) :
    noCrossSubsidy C r y ↔ ¬ ∃ T : Finset (Fin N), C (restrict y T) < r (restrict y T) := by
  unfold noCrossSubsidy
  push_neg
  rfl

/-- Control: the chapter's application checks survival against firms that
serve one class only. That is the test on singletons, and it does not imply the
test on every subset. Three products with revenues 1, 1, 0, stand-alone costs 1
for each single product, and 3/2 for the first two together, pass on
singletons and fail on the pair. -/
theorem control_singleton_tests_insufficient :
    let r : (Fin 3 → ℝ) → ℝ := fun z => z 0 + z 1
    let C : (Fin 3 → ℝ) → ℝ := fun z => if z 0 ≠ 0 ∧ z 1 ≠ 0 then 3 / 2 else 1
    let y : Fin 3 → ℝ := fun _ => 1
    (∀ i : Fin 3, r (restrict y {i}) ≤ C (restrict y {i})) ∧ ¬ noCrossSubsidy C r y := by
  intro r C y
  refine ⟨?_, ?_⟩
  · intro i
    fin_cases i <;> simp [r, C, y, restrict]
  · intro h
    have := h {0, 1}
    simp [r, C, y, restrict] at this
    norm_num at this

/-! ### Section 1.3.1, the general problem (PDF pp. 7–8)

Students `S`, colleges `C`, an allocation `A : S → C`, net benefits
`W_A[A(s)]` that may depend on the whole allocation, and `B(A) = Σ_s W_A[A(s)]`.
An allocation is efficient if it maximises `B`. -/

/-- `B(A)`. The benefit of student `s` under allocation `A` is `W A s`. -/
def totalSurplus {S C : Type*} [Fintype S] (W : (S → C) → S → ℝ) (A : S → C) : ℝ :=
  ∑ s, W A s

/-- An efficient allocation. -/
def Efficient {S C : Type*} [Fintype S] (W : (S → C) → S → ℝ) (A : S → C) : Prop :=
  ∀ A' : S → C, totalSurplus W A' ≤ totalSurplus W A

/-- PDF p. 8. If a student's benefit depends neither on the college nor on who
else attends, the identities of the students are irrelevant and every
allocation is efficient. -/
theorem all_efficient_without_synergy {S C : Type*} [Fintype S] (w : S → ℝ) (A : S → C) :
    Efficient (fun _ s => w s) A := by
  intro A'
  simp [totalSurplus]

/-! ### Section 1.3.2, the signalling argument (PDF p. 10)

A person of type `t` choosing `g` gets `w(g, t) = W(g, t) − p(g, t)`. "Under
mild conditions on `w(g, t)`, `g(t)` will be an increasing function of `t`."
The condition formalised is strictly increasing differences. The result is
general for linear orders. -/

/-- Strictly increasing differences of `w` in `(g, t)`. -/
def StrictIncDiff {G T : Type*} [Preorder G] [Preorder T] (w : G → T → ℝ) : Prop :=
  ∀ g g' t t', g < g' → t < t' → w g' t - w g t < w g' t' - w g t'

/-- PDF p. 10. With strictly increasing differences, any choice that is optimal
for type `t` is no larger than any choice optimal for a higher type `t'`. -/
theorem choice_monotone {G T : Type*} [LinearOrder G] [Preorder T] {w : G → T → ℝ}
    (hw : StrictIncDiff w) {t t' : T} (htt : t < t') {g g' : G}
    (hg : ∀ x, w x t ≤ w g t) (hg' : ∀ x, w x t' ≤ w g' t') : g ≤ g' := by
  by_contra hlt
  push_neg at hlt
  have h1 := hg g'
  have h2 := hg' g
  have h3 := hw g' g t t' hlt htt
  linarith

/-- Control: without increasing differences the choice can fall with the type.
With `w(g, t) = −(g + t − 1)²` on `g, t ∈ {0, 1}`, type 0 chooses 1 and type 1
chooses 0. -/
theorem control_choice_not_monotone :
    let w : ℝ → ℝ → ℝ := fun g t => -(g + t - 1) ^ 2
    (∀ x ∈ ({0, 1} : Set ℝ), w x 0 ≤ w 1 0) ∧ (∀ x ∈ ({0, 1} : Set ℝ), w x 1 ≤ w 0 1) ∧
      ¬ ((1 : ℝ) ≤ 0) := by
  intro w
  refine ⟨?_, ?_, by norm_num⟩ <;> intro x hx <;> rcases hx with rfl | rfl <;> simp [w]

/-! ### Section 1.4, tuition and scholarships (PDF pp. 16–17) -/

/-- Net revenue from students paying tuition `X` less scholarships `Y_i`. -/
def netRevenue {ι : Type*} (s : Finset ι) (X : ℝ) (Y : ι → ℝ) : ℝ := ∑ i ∈ s, (X - Y i)

/-- PDF p. 16. Raising tuition by `Δ` and every scholarship by `Δ` changes
nothing. -/
theorem tuition_scholarship_neutral {ι : Type*} (s : Finset ι) (X Δ : ℝ) (Y : ι → ℝ) :
    netRevenue s (X + Δ) (fun i => Y i + Δ) = netRevenue s X Y := by
  unfold netRevenue
  refine Finset.sum_congr rfl (fun i _ => ?_)
  ring

/-- PDF p. 16. Raising tuition by `Δ` and scholarships selectively by `δ_i ≤ Δ`,
with some `δ_i < Δ`, raises net revenue, if the students stay. -/
theorem selective_increments_raise_revenue {ι : Type*} (s : Finset ι) (X Δ : ℝ) (Y δ : ι → ℝ)
    (hle : ∀ i ∈ s, δ i ≤ Δ) (hlt : ∃ i ∈ s, δ i < Δ) :
    netRevenue s X Y < netRevenue s (X + Δ) (fun i => Y i + δ i) := by
  unfold netRevenue
  apply Finset.sum_lt_sum
  · intro i hi; have := hle i hi; linarith
  · obtain ⟨i, hi, h⟩ := hlt; exact ⟨i, hi, by linarith⟩

/-! ### Numbers that follow from printed numbers -/

/-- Footnote 9 (PDF p. 12). 220 of 3,340 institutions is 6.6 percent, and 190
of the 220 is over 86 percent. Footnote 10. The revenue shares of public and of
private universities each sum to 100 percent. -/
theorem footnotes_9_10 :
    (655 / 10000 : ℚ) < 220 / 3340 ∧ (220 / 3340 : ℚ) < 665 / 10000 ∧ (86 / 100 : ℚ) < 190 / 220 ∧
      (147 / 10 + 212 / 10 + 641 / 10 : ℚ) = 100 ∧ (396 / 10 + 217 / 10 + 387 / 10 : ℚ) = 100 := by
  norm_num

/-- Table 1.4 (PDF p. 22, read from the image) and PDF p. 23. The four 1949–50
counts sum to the 1,851 institutions of the text. Over 50 percent growth in
private two-year institutions holds on the series excluding branches
(227 to 367 by 1984–85). For private four-year institutions it holds only on the
1989–90 count including branches (983 to 1,532), and not on the series
excluding them (983 to 1,450 by 1984–85) (RW-D9). -/
theorem table14_numbers :
    (344 + 297 + 983 + 227 : ℚ) = 1851 ∧ (3 / 2 : ℚ) < 367 / 227 ∧
      (3 / 2 : ℚ) < 1532 / 983 ∧ (1450 / 983 : ℚ) < 3 / 2 := by
  norm_num

/-- PDF p. 19. The slopes 0.085 and 0.095 of tuition on expected salary are
below 0.10, "less than 10 percent of the higher expected annual starting
salary". -/
theorem tuition_salary_slopes : (85 / 1000 : ℚ) < 1 / 10 ∧ (95 / 1000 : ℚ) < 1 / 10 := by
  norm_num

end RothschildWhite1993
