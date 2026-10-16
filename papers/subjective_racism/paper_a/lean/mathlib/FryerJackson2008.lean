import Mathlib

/-! Fryer and Jackson (2008), the structure Paper A1 relies on or contrasts with,
in the paper's notation.

Objects carry binary attributes `θ_k(o) ∈ {0, 1}`. The variation of a group of
objects is eq. (4), `Var(O) = ∑_{o ∈ O} d(θ(o), θ̄(O))`, with `d` the city-block
metric, so it is the sum over attributes of the variation in each attribute.

Tractable special case. One attribute at a time: a group with `np` objects at 1
and `nm` objects at 0 has mean `np/(np + nm)` in that attribute, and `varAttr np
nm` is eq. (4) restricted to that attribute. A category of the four types of
Example 1, with counts `a, b, c, d` of `(0,0), (0,1), (1,0), (1,1)` (first
coordinate race, 1 white; second human capital, 1 high), has variation
`catVar a b c d`, the sum over the two attributes. The general Theorem 1 and
Corollary 1 are not formalised. -/

namespace FryerJackson2008

/-- Eq. (4) in one binary attribute. `np` objects have the attribute, `nm` do
not, and each is at city-block distance `|θ_k(o) − θ̄_k|` from the mean. -/
noncomputable def varAttr (np nm : ℝ) : ℝ :=
  np * |1 - np / (np + nm)| + nm * |0 - np / (np + nm)|

/-- Lemma 4 for one attribute. `Var(O) = 2 n⁺ n⁻ / n`. -/
theorem lemma4_one {np nm : ℝ} (hp : 0 ≤ np) (hm : 0 ≤ nm) (hn : 0 < np + nm) :
    varAttr np nm = 2 * np * nm / (np + nm) := by
  unfold varAttr
  have h1 : 1 - np / (np + nm) = nm / (np + nm) := by field_simp; ring
  rw [h1, zero_sub, abs_neg, abs_of_nonneg (div_nonneg hm hn.le),
    abs_of_nonneg (div_nonneg hp hn.le)]
  field_simp
  ring

/-- The variation of a category holding the four types of Example 1 in counts
`a, b, c, d` of `(0,0), (0,1), (1,0), (1,1)`. The city-block metric adds the
race attribute (`c + d` ones) and the human capital attribute (`b + d` ones). -/
noncomputable def catVar (a b c d : ℝ) : ℝ :=
  varAttr (c + d) (a + b) + varAttr (b + d) (a + c)

/-- The variation of the union of two homogeneous groups of sizes `ni, nj` that
differ on `h` attributes. Each differing attribute contributes `varAttr ni nj`
and each common attribute contributes nothing. -/
noncomputable def pairVar (h ni nj : ℝ) : ℝ := h * varAttr ni nj

/-- Proposition 1. For homogeneous groups, `Var(O₃ ∪ O₄) > Var(O₁ ∪ O₂)` if and
only if `h₃₄ (1/n₁ + 1/n₂) > h₁₂ (1/n₃ + 1/n₄)`. -/
theorem prop1 {h12 h34 n1 n2 n3 n4 : ℝ} (h1 : 0 < n1) (h2 : 0 < n2) (h3 : 0 < n3)
    (h4 : 0 < n4) :
    pairVar h12 n1 n2 < pairVar h34 n3 n4 ↔
      h12 * (1 / n3 + 1 / n4) < h34 * (1 / n1 + 1 / n2) := by
  unfold pairVar
  rw [lemma4_one h1.le h2.le (by linarith), lemma4_one h3.le h4.le (by linarith)]
  have e12 : 2 * n1 * n2 / (n1 + n2) = 2 / (1 / n1 + 1 / n2) := by
    field_simp; ring
  have e34 : 2 * n3 * n4 / (n3 + n4) = 2 / (1 / n3 + 1 / n4) := by
    field_simp; ring
  have p12 : 0 < 1 / n1 + 1 / n2 := by positivity
  have p34 : 0 < 1 / n3 + 1 / n4 := by positivity
  rw [e12, e34, mul_div_assoc', mul_div_assoc', div_lt_div_iff₀ p12 p34]
  constructor <;> intro h <;> nlinarith

/-- Example 1 and Table 1, three categories, with 5, 5, 45, 45 objects of types
`(0,0), (0,1), (1,0), (1,1)`. The race-based categorization, which pools the two
black types, has total variation 5. Pooling the two low types gives 9, and
pooling a black type with the white type of the other human capital level gives
18. -/
theorem example1_table :
    catVar 5 5 0 0 = 5 ∧ catVar 5 0 45 0 = 9 ∧ catVar 0 5 0 45 = 9 ∧
      catVar 5 0 0 45 = 18 ∧ catVar 0 5 45 0 = 18 ∧ catVar 0 0 45 45 = 45 := by
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_⟩ <;>
    norm_num [catVar, varAttr, abs_of_nonneg, abs_of_nonpos]

/-- Example 1. With three categories the race-based categorization, in which the
black types share a category and the white types are perfectly sorted, has a
strictly smaller variation than each of the five other ways to merge two of the
four types. -/
theorem example1_race_optimal :
    catVar 5 5 0 0 < catVar 0 0 45 45 ∧ catVar 5 5 0 0 < catVar 5 0 45 0 ∧
      catVar 5 5 0 0 < catVar 0 5 0 45 ∧ catVar 5 5 0 0 < catVar 5 0 0 45 ∧
      catVar 5 5 0 0 < catVar 0 5 45 0 := by
  obtain ⟨a, b, c, d, e, f⟩ := example1_table
  rw [a, b, c, d, e, f]
  norm_num

/-- The prototype of the category holding both black types is `(0, 1/2)`. A
black worker of high human capital is predicted at `1/2` in human capital, an
error of `1/2`, while a white worker of high human capital, alone in a category,
is predicted without error. -/
theorem example1_prototype_error :
    |(1 : ℝ) - 5 / (5 + 5)| = 1 / 2 ∧ |(1 : ℝ) - 45 / (45 + 0)| = 0 := by
  constructor <;> norm_num

/-- The expected prediction error per object of a category, the payoff loss in
the expected-utility problem of Section 3, is the variation divided by the size.
In one binary attribute it is `2 p (1 − p)` with `p` the share of the category
having the attribute, twice the variance of that share. -/
theorem meanDev_eq {np nm : ℝ} (hp : 0 ≤ np) (hm : 0 ≤ nm) (hn : 0 < np + nm) :
    varAttr np nm / (np + nm) = 2 * (np / (np + nm)) * (1 - np / (np + nm)) := by
  rw [lemma4_one hp hm hn]
  field_simp
  ring

/-- The proof of Proposition 2. In the equidistant world (distance 1 between
different types), adding `nn` identical new objects to a category of `n` objects
with variation `V` raises its variation by `nn (2 n − V)/(n + nn)`, which is `nn`
times `2 − (V + 2 nn)/(n + nn)`. So the variation rises least where
`(V + 2 nn)/(n + nn)` is largest, as the proposition states. The printed proof
omits the factor `nn` (FJ-D2). -/
theorem prop2_change {n nn V : ℝ} (hn : 0 < n) (hnn : 0 < nn) :
    (n * V + 2 * n * nn) / (n + nn) - V = nn * (2 - (V + 2 * nn) / (n + nn)) := by
  have : n + nn ≠ 0 := by linarith
  field_simp
  ring

/-- Example 4 at the stated counts (70, 160, 80, 28 objects of types
`(0,0,0,0), (1,0,0,0), (0,1,0,0), (0,1,1,1)`), computed from Lemma 4 attribute by
attribute. Pooling types 1, 3 and 4 and leaving type 2 alone (`V₁₃₄,₂`) has a
strictly smaller variation than pooling 1 with 2 and 3 with 4 (`V₁₂,₃₄`), which
the paper reports as optimal. Footnote 22's numbers are not reproduced (FJ-D3).
The left side is `V₁₃₄,₂` (attribute 2 has 108 ones and 70 zeros, attributes 3
and 4 have 28 ones and 150 zeros), the right side `V₁₂,₃₄`. -/
theorem example4_not_reproduced :
    varAttr 108 70 + 2 * varAttr 28 150 < varAttr 160 70 + 2 * varAttr 28 80 := by
  norm_num [varAttr, abs_of_nonneg, abs_of_nonpos]

/-- Control: the race-based categorization is strictly optimal in Example 1
only because the black group is a minority. With 25 objects of each of the four
types, pooling the two black types and pooling the two low types give the same
variation, 25, so the strict inequality of `example1_race_optimal` fails. -/
theorem control_no_minority :
    catVar 25 25 0 0 = 25 ∧ catVar 25 0 25 0 = 25 ∧
      ¬ catVar 25 25 0 0 < catVar 25 0 25 0 := by
  have h1 : catVar 25 25 0 0 = 25 := by
    norm_num [catVar, varAttr, abs_of_nonneg, abs_of_nonpos]
  have h2 : catVar 25 0 25 0 = 25 := by
    norm_num [catVar, varAttr, abs_of_nonneg, abs_of_nonpos]
  exact ⟨h1, h2, by rw [h1, h2]; exact lt_irrefl _⟩

/-- Control: the coarse sorting of the minority needs the number of categories
of Example 1. With two categories (Section 7, "Endogeneity of Categories") the
split by human capital, variation 18, is strictly better than the split by
race, variation 50, and than each of the other five two-category partitions of
the four types. -/
theorem control_two_categories :
    catVar 5 0 45 0 + catVar 0 5 0 45 = 18 ∧
      catVar 5 5 0 0 + catVar 0 0 45 45 = 50 ∧
      18 < catVar 5 5 0 0 + catVar 0 0 45 45 ∧
      18 < catVar 5 0 0 45 + catVar 0 5 45 0 ∧
      18 < catVar 5 0 0 0 + catVar 0 5 45 45 ∧
      18 < catVar 0 5 0 0 + catVar 5 0 45 45 ∧
      18 < catVar 0 0 45 0 + catVar 5 5 0 45 ∧
      18 < catVar 0 0 0 45 + catVar 5 5 45 0 := by
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩ <;>
    norm_num [catVar, varAttr, abs_of_nonneg, abs_of_nonpos]

end FryerJackson2008
