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

/-! ### Eqs. (4.1), (5.1) to (5.3) over a finite population

The expectations of eqs. (5.1) and (5.2) are averages over finite sets of men.
`P` is the set of passers and `NP` of non-passers, `yw` and `yb` give each
man's income as White and as Black, and `avg` is the mean over a set. -/

section Averages

variable {ι : Type*}

/-- The mean of `f` over a finite set `s`. -/
noncomputable def avg (s : Finset ι) (f : ι → ℝ) : ℝ := (∑ i ∈ s, f i) / s.card

theorem avg_sub (s : Finset ι) (f g : ι → ℝ) :
    avg s (fun i => f i - g i) = avg s f - avg s g := by
  unfold avg
  rw [Finset.sum_sub_distrib, sub_div]

/-- Eq. (5.2) over finite populations. The measured difference of means is
the mean return to passing among passers, eq. (5.1) on `P`, plus selection. -/
theorem eq_5_2_avg (P NP : Finset ι) (yw yb : ι → ℝ) :
    avg P yw - avg NP yb = avg P (fun i => yw i - yb i) + (avg P yb - avg NP yb) := by
  rw [avg_sub]; ring

/-- Control. With a constant return `ρ` for every man, the measured difference
exceeds `ρ` by exactly the selection term, which is nonzero when passers would
have earned more as Black than non-passers do. -/
theorem control_selection_biases {P NP : Finset ι} {yw yb : ι → ℝ} {ρ : ℝ}
    (hP : P.Nonempty) (hρ : ∀ i, yw i = yb i + ρ) (hsel : avg NP yb < avg P yb) :
    ρ < avg P yw - avg NP yb := by
  have hc : (P.card : ℝ) ≠ 0 := by exact_mod_cast hP.card_pos.ne'
  have h : avg P (fun i => yw i - yb i) = ρ := by
    unfold avg
    simp only [hρ, add_sub_cancel_left, Finset.sum_const, nsmul_eq_mul]
    field_simp
  rw [eq_5_2_avg, h]
  linarith

end Averages

/-- Eqs. (4.1) and (5.3). A cross-section `y = α + β d + x γ + u`. Two men with
the same `x` differ by `β (d₁ − d₂)` plus the difference of their `u`, so the
cross-section identifies `β` only when `u` does not differ with `d`. -/
theorem eq_4_1 (α β γ x d₁ d₂ u₁ u₂ : ℝ) :
    (α + β * d₁ + x * γ + u₁) - (α + β * d₂ + x * γ + u₂) = β * (d₁ - d₂) + (u₁ - u₂) := by
  ring

/-- Eq. (4.3). Enumeration-district fixed effects remove the district term in
the same way as the family effect of eq. (4.2), `within_family` with `φ_d`. -/
theorem eq_4_3 (α β φd x₁ x₂ ε₁ ε₂ : ℝ) :
    y α β x₁ φd ε₁ - y α β x₂ φd ε₂ = β * (x₁ - x₂) + (ε₁ - ε₂) :=
  within_family α β φd x₁ x₂ ε₁ ε₂

/-- Eq. (4.2), the within-family estimator for families of two brothers.
`Δy f = β Δx f + Δε f`. If `Δε` is orthogonal to `Δx` across families, the
least-squares slope `Σ Δx Δy / Σ Δx²` is `β`. -/
theorem within_estimator {F : Type*} [Fintype F] (β : ℝ) (dx de : F → ℝ)
    (horth : ∑ f, dx f * de f = 0) (hvar : ∑ f, dx f ^ 2 ≠ 0) :
    (∑ f, dx f * (β * dx f + de f)) / ∑ f, dx f ^ 2 = β := by
  have : ∑ f, dx f * (β * dx f + de f) = β * ∑ f, dx f ^ 2 + ∑ f, dx f * de f := by
    rw [Finset.mul_sum, ← Finset.sum_add_distrib]
    exact Finset.sum_congr rfl (fun f _ => by ring)
  rw [this, horth, add_zero, mul_div_assoc, div_self hvar, mul_one]

/-- Control. If the brothers' idiosyncratic terms move with their colour
difference, the within-family slope is off by `Σ Δx Δε / Σ Δx²`. -/
theorem control_within_orthogonality {F : Type*} [Fintype F] (β : ℝ) (dx de : F → ℝ)
    (hvar : ∑ f, dx f ^ 2 ≠ 0) :
    (∑ f, dx f * (β * dx f + de f)) / ∑ f, dx f ^ 2 = β + (∑ f, dx f * de f) / ∑ f, dx f ^ 2 := by
  have : ∑ f, dx f * (β * dx f + de f) = β * ∑ f, dx f ^ 2 + ∑ f, dx f * de f := by
    rw [Finset.mul_sum, ← Finset.sum_add_distrib]
    exact Finset.sum_congr rfl (fun f _ => by ring)
  rw [this, add_div, mul_div_assoc, div_self hvar, mul_one]

/-! ### Section 4.3. Colour distance between siblings (PDF p. 29) -/

/-- If discrimination is linear in true colour with slope `δ`, the population
gap is `δ D` and the sibling gap `δ r D` when siblings are a fraction `r` of
the population colour distance `D`. Dividing the sibling gap by `r`, "multiply
`β_f` by 2" at `r = 1/2`, recovers the population gap. -/
theorem sibling_inflation {δ D r : ℝ} (hr : r ≠ 0) : δ * r * D / r = δ * D := by
  field_simp

/-- Control. Without the inflation, a sibling gap at `r = 1/2` is half the
population gap whenever the latter is nonzero. -/
theorem control_no_inflation {δ D : ℝ} (h : δ * D ≠ 0) : δ * (1 / 2) * D ≠ δ * D := by
  intro hc; apply h; linarith

/-! ### The Roy argument in variances (PDF p. 34) -/

/-- The paper's form. With log income `a + b s` and `s` of variance `V > 0`,
the variance of log income is `b² V`. A higher White variance with a
nonnegative White slope gives a steeper White slope, hence `passers_upper`. -/
theorem roy_variance {bW bB V : ℝ} (hV : 0 < V) (hW : 0 ≤ bW)
    (hvar : bB ^ 2 * V < bW ^ 2 * V) : bB < bW := by
  have h2 : bB ^ 2 < bW ^ 2 := lt_of_mul_lt_mul_right hvar hV.le
  nlinarith [sq_nonneg (bW - bB), sq_nonneg (bW + bB)]

/-- Control. Without a nonnegative White slope the variance ordering does not order
the slopes. `bW = −2`, `bB = 1` gives the higher White variance and a gain from
passing that falls in earning potential. -/
theorem control_roy_sign : (1 : ℝ) ^ 2 * 1 < (-2) ^ 2 * 1 ∧
    StrictAnti (gain 0 (-2) 0 1) := by
  refine ⟨by norm_num, fun s s' h => ?_⟩
  unfold gain; nlinarith

/-! ### Further numbers in the text -/

/-- Table 1b, read from the image. Households with at least one parent coded
Mulatto: father Mulatto 29.47% plus mother Mulatto 48.01% less both 9.1%,
68.38%, the text's "slightly over two thirds" (PDF p. 15). -/
theorem table1b_mulatto_parent :
    (2947 + 4801 - 910 : ℝ) / 10000 = 6838 / 10000 ∧ (2 / 3 : ℝ) < 6838 / 10000 := by
  norm_num

/-- Table 3, read from the image. The match rates are 20,574 and 6,613 of
61,055, between 33% and 34% and between 10% and 11% (MLS-D3). -/
theorem match_rates :
    (33 / 100 : ℝ) < 20574 / 61055 ∧ (20574 / 61055 : ℝ) < 34 / 100 ∧
      (10 / 100 : ℝ) < 6613 / 61055 ∧ (6613 / 61055 : ℝ) < 11 / 100 := by
  norm_num

/-- PDF p. 22 and Table 4. Illiteracy is one minus the mean literacy,
37.8% and 28.2% for boys, 29.8% and 16.8% for girls. -/
theorem illiteracy :
    1 - (622 / 1000 : ℝ) = 378 / 1000 ∧ 1 - (718 / 1000 : ℝ) = 282 / 1000 ∧
      1 - (702 / 1000 : ℝ) = 298 / 1000 ∧ 1 - (832 / 1000 : ℝ) = 168 / 1000 := by
  norm_num

/-- PDF p. 22 against Table 4, column 2 (MLS-D12). The boys' colour gap with
family controls is printed as 0.039, not the text's 4.8 points. The girls'
0.079 matches the text's 7.9. -/
theorem table4_col2_gap : (39 / 1000 : ℝ) ≠ 48 / 1000 ∧ (79 / 1000 : ℝ) = 79 / 1000 := by
  norm_num

/-- PDF p. 25 and Table 6. `exp(−0.116) − 1` and `exp(−0.168) − 1` are the
"10.9%" and "15.5%" less, and `exp(0.035) − 1` the "3.5%" more. -/
theorem table6_percent :
    exp (116 / 1000) ∈ Set.Ioo (11229 / 10000 : ℝ) (11231 / 10000) ∧
      exp (168 / 1000) ∈ Set.Ioo (11828 / 10000 : ℝ) (11832 / 10000) ∧
      exp (35 / 1000) ∈ Set.Ioo (1035 / 1000 : ℝ) (1036 / 1000) := by
  obtain ⟨a1, a2⟩ := exp_sandwich (x := 116 / 1000) (by norm_num) (by norm_num)
  obtain ⟨b1, b2⟩ := exp_sandwich (x := 168 / 1000) (by norm_num) (by norm_num)
  obtain ⟨c1, c2⟩ := exp_sandwich (x := 35 / 1000) (by norm_num) (by norm_num)
  refine ⟨⟨?_, ?_⟩, ⟨?_, ?_⟩, ⟨?_, ?_⟩⟩ <;> norm_num at * <;> linarith

/-- The reading of `table6_percent`. `1 − 1/exp(0.116)` lies in (0.108, 0.110)
and `1 − 1/exp(0.168)` in (0.154, 0.155), the text's 10.9% and 15.5%. Neither
is footnote 5's 11.4% (MLS-D2). -/
theorem table6_less :
    (108 / 1000 : ℝ) < 1 - 1 / exp (116 / 1000) ∧ 1 - 1 / exp (116 / 1000) < 110 / 1000 ∧
      (154 / 1000 : ℝ) < 1 - 1 / exp (168 / 1000) ∧ 1 - 1 / exp (168 / 1000) < 155 / 1000 := by
  obtain ⟨⟨a1, a2⟩, ⟨b1, b2⟩, -⟩ := table6_percent
  have ha : 0 < exp (116 / 1000 : ℝ) := exp_pos _
  have hb : 0 < exp (168 / 1000 : ℝ) := exp_pos _
  refine ⟨?_, ?_, ?_, ?_⟩
  · rw [lt_sub_iff_add_lt, ← lt_sub_iff_add_lt', div_lt_iff₀ ha]; nlinarith
  · rw [sub_lt_iff_lt_add, ← sub_lt_iff_lt_add', lt_div_iff₀ ha]; nlinarith
  · rw [lt_sub_iff_add_lt, ← lt_sub_iff_add_lt', div_lt_iff₀ hb]; nlinarith
  · rw [sub_lt_iff_lt_add, ← sub_lt_iff_lt_add', lt_div_iff₀ hb]; nlinarith

/-- PDF p. 35. Schooling "accounts for about 10 percentage points" of the gap,
`exp 0.273 − exp 0.185` and `exp 0.351 − exp 0.269`, each between 0.09 and 0.13. -/
theorem schooling_accounts :
    (9 / 100 : ℝ) < exp (273 / 1000) - exp (185 / 1000) ∧
      exp (273 / 1000) - exp (185 / 1000) < 13 / 100 ∧
      (9 / 100 : ℝ) < exp (351 / 1000) - exp (269 / 1000) ∧
      exp (351 / 1000) - exp (269 / 1000) < 13 / 100 := by
  obtain ⟨a1, a2, b1, b2⟩ := col1_percent
  obtain ⟨c1, c2, d1, d2⟩ := col2_percent
  refine ⟨?_, ?_, ?_, ?_⟩ <;> linarith

/-- PDF p. 36 (MLS-D13). "Higher by more than 50% for Whites": the White
interaction over the base slope is `0.029/0.064`, below one half, for total
income and `0.042/0.061`, above it, for labour income. -/
theorem schooling_return_ratio :
    (29 / 1000 : ℝ) / (64 / 1000) < 1 / 2 ∧ (1 / 2 : ℝ) < (42 / 1000) / (61 / 1000) := by
  norm_num

/-- PDF p. 2. The Spencer brothers' $1,800 and $480 scaled by 40 are $72,000
and $19,200, against the text's $72,000 and $19,000. -/
theorem spencer_dollars : (1800 : ℕ) * 40 = 72000 ∧ (480 : ℕ) * 40 = 19200 := by norm_num

end MillStein2012
