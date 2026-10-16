import Mathlib

/-! Mill and Stein, "Race, Skin Color, and Economic Outcomes in Early
Twentieth-Century America", working paper of December 2012 (version updated
30 December 2012), in the paper's notation.

The paper is empirical. What is formalised is the structure it states.

1. Eq. (5.2). The measured difference between passers and non-passers is the
   return to passing plus a selection term. `ywp` is `E[y_i^w | p]`, `ybp` the
   counterfactual `E[y_i^b | p]` and `ybnp` is `E[y_j^b | np]`.
2. Eqs. (4.2) and (5.4). With a family effect `φ_f`, `y_if = α + β x_if + φ_f +
   ε_if`, a difference between brothers removes `φ_f`, and a difference across
   families does not.
3. The Roy argument of p. 33, our reading. If the log wage among Whites rises
   faster in earning potential `s` than among Blacks, the gain from passing
   rises in `s`, and with a cost of passing the passers are those above a
   threshold. Table 10, column (3), reports the steeper slope as a positive
   interaction of White in 1940 with years of schooling.
4. The arithmetic of Section 5. Log points in Table 10 are read as the
   percentages of the text through `exp`, and the share of the population gap
   that the sibling comparison removes is computed. -/

namespace MillStein2012

open Real

/-! ### Eq. (5.2) -/

/-- Eq. (5.2). The measured difference is the return to passing plus the
selection of passers. -/
theorem eq_5_2 (ywp ybp ybnp : ℝ) :
    ywp - ybnp = (ywp - ybp) + (ybp - ybnp) := by ring

/-- The measured difference equals the return exactly when there is no
selection, `E[y_i^b | p] = E[y_j^b | np]`. -/
theorem measured_eq_return_iff (ywp ybp ybnp : ℝ) :
    ywp - ybnp = ywp - ybp ↔ ybp = ybnp := by
  constructor <;> intro h <;> linarith

/-! ### Family fixed effects, eqs. (4.2) and (5.4) -/

/-- The outcome of a son with regressor `x` in a family with effect `φ`. -/
def y (α β x φ ε : ℝ) : ℝ := α + β * x + φ + ε

/-- Eq. (5.4). Between brothers the family effect cancels. -/
theorem within_family (α β φ x₁ x₂ ε₁ ε₂ : ℝ) :
    y α β x₁ φ ε₁ - y α β x₂ φ ε₂ = β * (x₁ - x₂) + (ε₁ - ε₂) := by
  unfold y; ring

/-- Across families the difference keeps `φ_f − φ_g`. -/
theorem across_families (α β φf φg x₁ x₂ ε₁ ε₂ : ℝ) :
    y α β x₁ φf ε₁ - y α β x₂ φg ε₂ = β * (x₁ - x₂) + (φf - φg) + (ε₁ - ε₂) := by
  unfold y; ring

/-- Control. With a passer (`x = 1`) and a non-passer (`x = 0`) from different
families and no idiosyncratic terms, the comparison misses `β` by the
difference in family effects. The sibling design is needed. -/
theorem control_family_effect {α β φf φg : ℝ} (h : φf ≠ φg) :
    y α β 1 φf 0 - y α β 0 φg 0 ≠ β := by
  unfold y
  intro hc
  apply h
  linarith

/-! ### Selection into passing, the Roy argument (our reading) -/

/-- The gain in log income from passing for a man of earning potential `s`,
when the log income of a White is `aW + bW s` and of a Black `aB + bB s`. -/
def gain (aW bW aB bB s : ℝ) : ℝ := (aW - aB) + (bW - bB) * s

/-- A man passes when the gain covers the cost `c`. -/
def passes (aW bW aB bB c s : ℝ) : Prop := c ≤ gain aW bW aB bB s

/-- With a steeper slope among Whites, the gain rises in earning potential. -/
theorem gain_strictMono {aW bW aB bB : ℝ} (h : bB < bW) :
    StrictMono (gain aW bW aB bB) := by
  intro s s' hs
  unfold gain
  have : (bW - bB) * s < (bW - bB) * s' := mul_lt_mul_of_pos_left hs (by linarith)
  linarith

/-- Passers form an upper set in earning potential. If a man passes, so does
every man of higher potential with the same cost. -/
theorem passers_upper {aW bW aB bB c s s' : ℝ} (h : bB < bW) (hs : s ≤ s')
    (hp : passes aW bW aB bB c s) : passes aW bW aB bB c s' := by
  unfold passes at *
  exact le_trans hp ((gain_strictMono h).monotone hs)

/-- Control. With equal slopes the gain does not depend on earning potential,
so passing is not selected on it. -/
theorem control_equal_slopes {aW aB b c s s' : ℝ} :
    passes aW b aB b c s ↔ passes aW b aB b c s' := by
  unfold passes gain
  constructor <;> intro h <;> linarith

/-! ### The arithmetic of Section 5 -/

/-- For `0 ≤ x ≤ 1`, `1 + x + x²/2 + x³/6 ≤ exp x ≤ 1 + x + x²/2 + (2/9) x³`,
from Mathlib's `Real.sum_le_exp_of_nonneg` with four terms and `Real.exp_bound'`
with three. -/
theorem exp_sandwich {x : ℝ} (h0 : 0 ≤ x) (h1 : x ≤ 1) :
    1 + x + x ^ 2 / 2 + x ^ 3 / 6 ≤ exp x ∧ exp x ≤ 1 + x + x ^ 2 / 2 + 2 / 9 * x ^ 3 := by
  constructor
  · have h := Real.sum_le_exp_of_nonneg h0 4
    simp [Finset.sum_range_succ, Nat.factorial] at h
    linarith
  · have h := Real.exp_bound' h0 h1 (n := 3) (by norm_num)
    simp [Finset.sum_range_succ, Nat.factorial] at h
    linarith

/-- Table 10, column (1). `0.273` log points is the "31%" of p. 34 and `0.351`
the "42%" of p. 35. -/
theorem col1_percent :
    (131 / 100 : ℝ) < exp (273 / 1000) ∧ exp (273 / 1000) < 132 / 100 ∧
      (141 / 100 : ℝ) < exp (351 / 1000) ∧ exp (351 / 1000) < 143 / 100 := by
  obtain ⟨a1, a2⟩ := exp_sandwich (x := 273 / 1000) (by norm_num) (by norm_num)
  obtain ⟨b1, b2⟩ := exp_sandwich (x := 351 / 1000) (by norm_num) (by norm_num)
  refine ⟨?_, ?_, ?_, ?_⟩ <;> norm_num at * <;> linarith

/-- Table 10, column (2). `0.185` and `0.269` are the "20% and 31%" of p. 34. -/
theorem col2_percent :
    (120 / 100 : ℝ) < exp (185 / 1000) ∧ exp (185 / 1000) < 121 / 100 ∧
      (130 / 100 : ℝ) < exp (269 / 1000) ∧ exp (269 / 1000) < 131 / 100 := by
  obtain ⟨a1, a2⟩ := exp_sandwich (x := 185 / 1000) (by norm_num) (by norm_num)
  obtain ⟨b1, b2⟩ := exp_sandwich (x := 269 / 1000) (by norm_num) (by norm_num)
  refine ⟨?_, ?_, ?_, ?_⟩ <;> norm_num at * <;> linarith

/-- Table 10, column (4). `0.191` and `0.130` are the "21%" and "14%" between
brothers of p. 34. -/
theorem col4_percent :
    (121 / 100 : ℝ) < exp (191 / 1000) ∧ exp (191 / 1000) < 122 / 100 ∧
      (113 / 100 : ℝ) < exp (130 / 1000) ∧ exp (130 / 1000) < 114 / 100 := by
  obtain ⟨a1, a2⟩ := exp_sandwich (x := 191 / 1000) (by norm_num) (by norm_num)
  obtain ⟨b1, b2⟩ := exp_sandwich (x := 130 / 1000) (by norm_num) (by norm_num)
  refine ⟨?_, ?_, ?_, ?_⟩ <;> norm_num at * <;> linarith

/-- P. 35. The share of the population gap removed by comparing brothers is
`(0.273 − 0.191)/0.273`, about 0.30, for total income and
`(0.351 − 0.130)/0.351`, about 0.63, for labor income. The first lies just
below the text's "1/3". -/
theorem selection_share :
    (273 - 191 : ℝ) / 273 < 1 / 3 ∧ 3 / 10 < (273 - 191 : ℝ) / 273 ∧
      (351 - 130 : ℝ) / 351 < 2 / 3 ∧ 6 / 10 < (351 - 130 : ℝ) / 351 := by
  norm_num

/-- Control. The log-point coefficient is not the percentage. `0.351` log
points exceed 35.1 percent by more than six points. -/
theorem control_log_not_percent : (1351 / 1000 : ℝ) + 6 / 100 < exp (351 / 1000) := by
  obtain ⟨b1, -⟩ := exp_sandwich (x := 351 / 1000) (by norm_num) (by norm_num)
  norm_num at b1
  linarith

end MillStein2012
