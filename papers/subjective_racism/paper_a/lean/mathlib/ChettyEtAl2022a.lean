import Mathlib

/-! Chetty et al. (2022), "Social capital I: measurement and associations with
economic mobility", *Nature* 608, 108–121, in the paper's notation.

`f_{Q,i}` is individual `i`'s share of friends in SES quantile `Q` (eq. (1)),
`w_Q` the population share of the quantile, `IEC_{Q,i} = f_{Q,i}/w_Q` (eq. (2)),
and `EC_c` the mean of `IEC` over the low-SES members of community `c` (eq. (3)).
With the median split `w_H = 1/2`.

The no-homophily benchmark of p. 110 (PDF p. 3) is printed as `1.254/(1 + 1.254)`.
We read it as the high-SES friend share of a low-SES person when friending does
not depend on SES and high-SES people have `x` times as many friends,
`x/(1 + x)` at `w_H = 1/2`. That reading is ours.

The paper's results are correlations and regressions on data we do not hold.
What is formalised is the measure, the benchmark, and the arithmetic of the
numbers printed from them. -/

namespace ChettyEtAl2022a

open Finset

/-- Eq. (1). The share of `i`'s friends in quantile `Q`. -/
noncomputable def f (nQ nTotal : ℝ) : ℝ := nQ / nTotal

/-- Eq. (2). Individual economic connectedness to quantile `Q`. -/
noncomputable def IEC (fQi wQ : ℝ) : ℝ := fQi / wQ

/-- Eq. (3). Community EC, the mean of individual EC over the low-SES members
`L` of the community. -/
noncomputable def EC {ι : Type*} (L : Finset ι) (iec : ι → ℝ) : ℝ :=
  (∑ i ∈ L, iec i) / L.card

/-- The high-SES friend share of a low-SES person under no homophily, when
high-SES people have `x` times as many friends and `w_H = 1/2`. -/
noncomputable def benchmark (x : ℝ) : ℝ := x / (1 + x)

/-- PDF p. 3. The printed shares 38.8% and 70.6% give under-representation of
22.4% for low-SES people, over-representation of 41.2% for high-SES people, an
average of 54.7%, and EC `0.388/0.5 = 0.776`. The gap in EC between the two
groups is `0.636`, the figure used on PDF p. 9. -/
theorem shares_p3 :
    1 - IEC 0.388 0.5 = (0.224 : ℝ) ∧ IEC 0.706 0.5 - 1 = (0.412 : ℝ) ∧
      ((0.388 : ℝ) + 0.706) / 2 = 0.547 ∧ IEC 0.388 0.5 = (0.776 : ℝ) ∧
      IEC 0.706 0.5 - IEC 0.388 0.5 = (0.636 : ℝ) := by
  unfold IEC; norm_num

/-- PDF p. 3. Top-decile friends are under-represented among bottom-decile people
by `1 − 0.025/0.1 = 0.75`. -/
theorem decile_p3 : 1 - IEC 0.025 0.1 = (0.75 : ℝ) := by
  unfold IEC; norm_num

/-- PDF p. 3. The benchmark `1.254/(1 + 1.254)` is 55.6% to three figures. -/
theorem benchmark_value : 0.556 < benchmark 1.254 ∧ benchmark 1.254 < 0.5565 := by
  unfold benchmark; norm_num

/-- PDF p. 3. "30.2% fewer" follows from the benchmark rounded to 55.6%. With the
unrounded benchmark the shortfall is between 30.25% and 30.26%. -/
theorem fewer_p3 :
    (0.3015 < 1 - 0.388 / (0.556 : ℝ) ∧ 1 - 0.388 / (0.556 : ℝ) < 0.3025) ∧
      (0.3025 < 1 - 0.388 / benchmark 1.254 ∧ 1 - 0.388 / benchmark 1.254 < 0.3026) := by
  unfold benchmark; norm_num

/-- Control for `benchmark_value`. With equal numbers of friends, `x = 1`, the
no-homophily benchmark is exactly one half, and the shortfall is the 22.4% of
`shares_p3`. The 55.6% benchmark and the 30.2% figure rest on the 25.4%
difference in friend counts. -/
theorem control_equal_friends : benchmark 1 = 1 / 2 ∧ 1 - 0.388 / benchmark 1 = (0.224 : ℝ) := by
  unfold benchmark; norm_num

/-- `benchmark` rises in `x`, so a positive difference in friend counts raises
the share a low-SES person would have under no homophily. -/
theorem benchmark_strictMono {x y : ℝ} (hx : 0 ≤ x) (hxy : x < y) :
    benchmark x < benchmark y := by
  unfold benchmark
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith

/-- Eq. (3) weights people, not friendships. When every low-SES member has the
same number of friends `n > 0`, community EC equals the pooled share of high-SES
friends divided by `w_Q`. -/
theorem EC_pooled_of_equal_degree {ι : Type*} (L : Finset ι) (h : ι → ℝ) {n wQ : ℝ}
    (hn : n ≠ 0) (hwQ : wQ ≠ 0) (hL : L.Nonempty) :
    EC L (fun i => IEC (f (h i) n) wQ) = (∑ i ∈ L, h i) / (L.card * n) / wQ := by
  unfold EC IEC f
  have hc : (L.card : ℝ) ≠ 0 := by
    have := hL.card_pos; positivity
  rw [← Finset.sum_div, ← Finset.sum_div]
  field_simp

/-- Control for `EC_pooled_of_equal_degree`. With unequal friend counts the
mean of individual ratios differs from the pooled ratio. Two low-SES people, one
with 1 high-SES friend of 2 and one with none of 8: eq. (3) gives EC 0.5, the
pooled share gives 0.2. -/
theorem control_EC_mean_of_ratios :
    EC (Finset.univ : Finset (Fin 2))
        (fun i => IEC (f (if i = 0 then 1 else 0) (if i = 0 then 2 else 8)) 0.5) = (0.5 : ℝ) ∧
      ((1 : ℝ) + 0) / (2 + 8) / 0.5 = 0.2 := by
  unfold EC IEC f
  simp [Fin.sum_univ_two]
  norm_num

/-- PDF p. 9. Growing up in a county with EC higher by the 0.636 gap raises
adult income by `0.636 × 30.7 = 19.5%`, or `9.8 × 0.636 = 6.23` percentiles,
about 37% of a 17-percentile gap. A move raising EC by 0.57 raises income by
17.5%. -/
theorem income_p9 :
    (0.636 : ℝ) * 30.7 = 19.5252 ∧ (9.8 : ℝ) * 0.636 = 6.2328 ∧
      0.36 < (6.23 : ℝ) / 17 ∧ (6.23 : ℝ) / 17 < 0.37 ∧ (0.57 : ℝ) * 30.7 = 17.499 := by
  norm_num

/-- Extended Data Table 1, panel A, row 1 (own decile 1). The share of friends
in each decile, in per cent, falls with the distance of the decile from the
bottom. -/
def row1 : Fin 10 → ℚ :=
  ![23.4, 17.5, 13.7, 11.0, 9.2, 7.7, 6.2, 4.9, 3.8, 2.5]

/-- Extended Data Table 1, panel A, row 5 (own decile 5). -/
def row5 : Fin 10 → ℚ :=
  ![7.0, 8.7, 10.0, 10.8, 11.6, 11.9, 11.6, 11.0, 9.8, 7.5]

/-- PDF p. 25. Row 1 strictly decreases across deciles: friendship falls with
SES distance for the poorest decile. -/
theorem row1_strictAnti : StrictAnti row1 := by
  refine Fin.strictAnti_iff_succ_lt.2 ?_
  intro i; fin_cases i <;> simp [row1] <;> norm_num

/-- PDF p. 25. Row 5 peaks at decile 6, next to its own, and is lower at both
ends. A middle-decile person does not have the most friends in the top decile.
Friend shares by SES are not ordered the same way for every own decile. -/
theorem row5_interior_peak :
    (∀ k, row5 k ≤ row5 5) ∧ row5 9 < row5 5 ∧ row5 0 < row5 5 ∧ row1 9 < row1 0 := by
  refine ⟨fun k => ?_, by simp [row5]; norm_num, by simp [row5]; norm_num,
    by simp [row1]; norm_num⟩
  fin_cases k <;> simp [row5] <;> norm_num

end ChettyEtAl2022a
