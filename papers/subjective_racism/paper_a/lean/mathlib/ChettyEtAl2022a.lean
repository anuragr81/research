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

/-! ### Both sides of a cross-SES friendship -/

/-- Friendship is mutual (PDF p. 15, links "need to be confirmed by both
parties"). With a symmetric adjacency `A` and disjoint sets `L` and `H`, the
number of friendships counted from the low-SES side equals the number counted
from the high-SES side. In pooled counts, low-SES people having few high-SES
friends and high-SES people having few low-SES friends are one fact. -/
theorem cross_links_balance {ι : Type*} (A : ι → ι → ℝ) (hA : ∀ i j, A i j = A j i)
    (L H : Finset ι) :
    ∑ i ∈ L, ∑ j ∈ H, A i j = ∑ j ∈ H, ∑ i ∈ L, A j i := by
  rw [Finset.sum_comm]
  exact Finset.sum_congr rfl (fun j _ => Finset.sum_congr rfl (fun i _ => hA i j))

/-- In shares, with equal numbers of low- and high-SES people and friend counts
`d_L` and `d_H`, pooled balance is `d_L f_{LH} = d_H (1 − f_{HH})`. -/
theorem balance_shares {NL NH dL dH fLH fHH : ℝ} (hN : NL = NH)
    (hbal : NL * dL * fLH = NH * dH * (1 - fHH)) (hN0 : NL ≠ 0) :
    dL * fLH = dH * (1 - fHH) := by
  subst hN
  have := hbal
  have h' : NL * (dL * fLH) = NL * (dH * (1 - fHH)) := by linarith
  exact mul_left_cancel₀ hN0 h'

/-- PDF p. 3. The printed shares are means over people, not pooled shares. With
the 25.4% more friends of high-SES people, pooled balance would give a low-SES
high-SES share of `1.254 × 0.294 = 0.3687`, against the printed 0.388. -/
theorem printed_shares_not_pooled :
    (1.254 : ℝ) * (1 - 0.706) = 0.368676 ∧ (0.368676 : ℝ) ≠ 0.388 := by
  norm_num

/-! ### Cohesiveness, eqs. (4) to (6), and spectral homophily -/

/-- Eqs. (4) and (6) share one form, the fraction of a finite set of pairs that
satisfy a predicate (friends of `i` who are friends, or friendships with a common
friend). -/
noncomputable def shareOf {α : Type*} (S : Finset α) (P : α → Prop) [DecidablePred P] : ℝ :=
  ((S.filter P).card : ℝ) / S.card

/-- PDF p. 5. Clustering and the support ratio range from 0 to 1. -/
theorem shareOf_mem {α : Type*} (S : Finset α) (P : α → Prop) [DecidablePred P] :
    0 ≤ shareOf S P ∧ shareOf S P ≤ 1 := by
  unfold shareOf
  refine ⟨by positivity, ?_⟩
  rcases Nat.eq_zero_or_pos S.card with h | h
  · simp [h]
  · rw [div_le_one (by exact_mod_cast h)]
    exact_mod_cast Finset.card_filter_le S P

/-- Spectral homophily is an eigenvalue of the row-stochastic adjacency matrix
(PDF p. 17). Every real eigenvalue of a nonnegative row-stochastic matrix has
absolute value at most 1. -/
theorem stochastic_eig_le_one {n : ℕ} (A : Fin n → Fin n → ℝ) (hA : ∀ i j, 0 ≤ A i j)
    (hrow : ∀ i, ∑ j, A i j = 1) (v : Fin n → ℝ) (hv : v ≠ 0) (lam : ℝ)
    (h : ∀ i, ∑ j, A i j * v j = lam * v i) : |lam| ≤ 1 := by
  obtain ⟨k, hk⟩ : ∃ k, v k ≠ 0 := by
    by_contra hc; push_neg at hc; exact hv (funext hc)
  obtain ⟨i, -, hi⟩ := Finset.exists_max_image Finset.univ (fun j => |v j|) ⟨k, Finset.mem_univ k⟩
  have hpos : 0 < |v i| := lt_of_lt_of_le (abs_pos.2 hk) (hi k (Finset.mem_univ k))
  have key : |lam| * |v i| ≤ 1 * |v i| := by
    rw [← abs_mul, ← h i]
    calc |∑ j, A i j * v j| ≤ ∑ j, |A i j * v j| := Finset.abs_sum_le_sum_abs _ _
      _ = ∑ j, A i j * |v j| := by
          refine Finset.sum_congr rfl (fun j _ => ?_); rw [abs_mul, abs_of_nonneg (hA i j)]
      _ ≤ ∑ j, A i j * |v i| := Finset.sum_le_sum (fun j _ =>
          mul_le_mul_of_nonneg_left (hi j (Finset.mem_univ j)) (hA i j))
      _ = 1 * |v i| := by rw [← Finset.sum_mul, hrow i]
  exact le_of_mul_le_mul_right key hpos

/-! ### Further printed arithmetic -/

/-- PDF p. 3. Bottom-decile counties with EC below 0.58 have 42% fewer high-SES
friends than the no-homophily benchmark with equal friend counts. -/
theorem bottom_decile_p3 : 1 - (0.58 : ℝ) = 0.42 := by norm_num

/-- PDF p. 6. Minneapolis, 49% high-SES friends, has EC `0.98`. Indianapolis,
32%, is printed with EC 0.65, while `2 × 0.32 = 0.64`, so the 32% is rounded. An
EC rise of 0.5 is a rise of the share from 25% to 50%, and at the slope 16.4 of
Fig. 4 it is 8.2 percentiles. -/
theorem mobility_p6 :
    2 * (0.49 : ℝ) = 0.98 ∧ 2 * (0.32 : ℝ) = 0.64 ∧ 2 * (0.50 : ℝ) - 2 * 0.25 = 0.5 ∧
      (16.4 : ℝ) * 0.5 = 8.2 := by
  norm_num

/-- PDF p. 3. The sample of 72.2 million is about 3,500 times the 20,000 of Add
Health (3,610 times). PDF p. 9. 9.8 percentiles is 30.7% of a mean rank of
about 31.9. -/
theorem scale_numbers :
    (72200000 : ℝ) / 20000 = 3610 ∧ 31.9 < (9.8 : ℝ) / 0.307 ∧ (9.8 : ℝ) / 0.307 < 32 := by
  norm_num

/-- Methods, "Correlations". The signal correlation divides the raw correlation
by the square root of the reliability of the noisier variable. -/
noncomputable def signalCorr (r rel : ℝ) : ℝ := r / Real.sqrt rel

/-- With perfect reliability the signal correlation is the raw one. -/
theorem signalCorr_one (r : ℝ) : signalCorr r 1 = r := by simp [signalCorr]

/-- Control for `cross_links_balance`. Without mutual consent, a directed
"follow" relation (as on Instagram before the paper restricts to reciprocal
followers, PDF p. 17) need not balance. One low-SES person follows one high-SES
person who does not follow back. -/
theorem control_balance_needs_symmetry :
    let A : Fin 2 → Fin 2 → ℝ := fun i j => if i = 0 ∧ j = 1 then 1 else 0
    (∑ i ∈ ({0} : Finset (Fin 2)), ∑ j ∈ ({1} : Finset (Fin 2)), A i j)
      ≠ ∑ j ∈ ({1} : Finset (Fin 2)), ∑ i ∈ ({0} : Finset (Fin 2)), A j i := by
  simp

/-- Control for `stochastic_eig_le_one`. Rows summing to one without
nonnegativity allow an eigenvalue of 3. -/
theorem control_eig_needs_nonneg :
    let A : Fin 2 → Fin 2 → ℝ := ![![2, -1], ![-1, 2]]
    let v : Fin 2 → ℝ := ![1, -1]
    (∀ i, ∑ j, A i j = 1) ∧ (∀ i, ∑ j, A i j * v j = 3 * v i) ∧ 1 < |(3 : ℝ)| := by
  refine ⟨?_, ?_, by norm_num⟩ <;> intro i <;> fin_cases i <;> simp [Fin.sum_univ_two] <;> norm_num

end ChettyEtAl2022a
