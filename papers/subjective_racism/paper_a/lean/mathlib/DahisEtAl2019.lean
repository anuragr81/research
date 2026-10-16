import Mathlib

/-! Dahis, Nix and Qian, "Choosing Racial Identity in the United States,
1880–1940", NBER Working Paper 26465 (November 2019, revised June 2020), in the
paper's own terms where it has them.

The paper is empirical. What is formalised here is the structure it states and
the identities it computes, not its regressions.

1. The 2-Sided Unique Perfect (2SUP) link of Section 4 and Appendix C.1. A
   record `i` of year `t` and a record `j` of year `t + 10` are candidates,
   `cand i j`, when they agree on perfect spelling, birth place and the age
   window. A one-sided Unique Perfect link needs `j` to be the only candidate of
   `i`. A 2SUP link also needs `i` to be the only candidate of `j`. Figure A.4
   is checked as a finite example.
2. The extrapolation of Section 5.1.1. With a share `l` of black men linked, a
   rate `r` of passing among the linked and a rate `u` among the unlinked, the
   population rate is `l r + (1 − l) u`. The paper's lower bound sets `u = 0`.
   The adjusted weighted rate of Appendix F is checked as arithmetic.
3. Our reading, not the paper's statement. Section 5.3.7 reports that passing
   rises with the base-year occupational income score. If those who pass have
   a mean above the group's, the mean of those who remain falls. The paper does
   not draw this conclusion. It is the step Paper A1 would take from it. -/

namespace DahisEtAl2019

/-! ### The 2SUP link -/

section Links

variable {α β : Type*}

/-- The one-sided Unique Perfect link (Appendix C.1). `j` is the only
candidate of `i` in year `t + 10`. -/
def oneSided (cand : α → β → Prop) (i : α) (j : β) : Prop :=
  cand i j ∧ ∀ j', cand i j' → j' = j

/-- The 2-Sided Unique Perfect link (Section 4). The link is unique going
forwards and, from `j`, unique going backwards. -/
def twoSided (cand : α → β → Prop) (i : α) (j : β) : Prop :=
  oneSided cand i j ∧ ∀ i', cand i' j → i' = i

/-- Every 2SUP link is a one-sided link, so 2SUP links no more records. -/
theorem twoSided_oneSided {cand : α → β → Prop} {i : α} {j : β}
    (h : twoSided cand i j) : oneSided cand i j :=
  h.1

/-- A record of year `t + 10` is the 2SUP link of at most one record of year
`t`. The backward condition is what rules out the false link of Figure A.4. -/
theorem twoSided_unique_back {cand : α → β → Prop} {i i' : α} {j : β}
    (h : twoSided cand i j) (h' : twoSided cand i' j) : i' = i :=
  h.2 i' h'.1.1

instance decOneSided [Fintype β] [DecidableEq β] (cand : α → β → Prop)
    [∀ i j, Decidable (cand i j)] (i : α) (j : β) : Decidable (oneSided cand i j) := by
  unfold oneSided; infer_instance

instance decTwoSided [Fintype α] [Fintype β] [DecidableEq α] [DecidableEq β]
    (cand : α → β → Prop) [∀ i j, Decidable (cand i j)] (i : α) (j : β) :
    Decidable (twoSided cand i j) := by
  unfold twoSided; infer_instance

end Links

/-! ### Figure A.4

Year `t` has seven men. Records 0, 1, 2 are the black Samuel, Elijah and Abe,
and records 3 to 6 are four white Abes. Year `t + 10` has three men, record 0
the black Samuel, record 1 a white Elijah and record 2 a white Abe. Names are
coded 0 (Samuel), 1 (Elijah), 2 (Abe), and two records are candidates when the
names agree. -/

/-- The names in year `t`. -/
def nameT : Fin 7 → Fin 3 := ![0, 1, 2, 2, 2, 2, 2]

/-- The names in year `t + 10`. -/
def nameT10 : Fin 3 → Fin 3 := ![0, 1, 2]

/-- Candidates agree on the name. -/
def exCand (i : Fin 7) (j : Fin 3) : Prop := nameT i = nameT10 j

instance (i : Fin 7) (j : Fin 3) : Decidable (exCand i j) := by
  unfold exCand; infer_instance

/-- Race in year `t + 10`. Only record 0 is black. -/
def blackT10 (j : Fin 3) : Prop := j = 0

instance (j : Fin 3) : Decidable (blackT10 j) := by
  unfold blackT10; infer_instance

/-- Figure A.4a. The one-sided link joins the black Abe to the white Abe. -/
theorem figA4a_abe_linked : oneSided exCand 2 2 := by
  unfold oneSided exCand nameT nameT10
  decide

/-- Figure A.4b. The 2SUP link drops the black Abe, because the white Abe of
year `t + 10` has five candidates in year `t`. -/
theorem figA4b_abe_dropped : ∀ j, ¬ twoSided exCand 2 j := by
  unfold twoSided oneSided exCand nameT nameT10
  decide

/-- Figure A.4b. The 2SUP link keeps the black Elijah's link to the white
Elijah, who passed. -/
theorem figA4b_elijah_kept : twoSided exCand 1 1 := by
  unfold twoSided oneSided exCand nameT nameT10
  decide

/-- Figure A.4b. The 2SUP link keeps Samuel, who stays black. -/
theorem figA4b_samuel_kept : twoSided exCand 0 0 := by
  unfold twoSided oneSided exCand nameT nameT10
  decide

/-- Appendix C.1. Among the three black men of year `t`, the one-sided link
records two as passing, Elijah and Abe. -/
theorem figA4a_two_pass :
    ((Finset.univ : Finset (Fin 3)).filter
      (fun i => ∃ j, oneSided exCand (Fin.castLE (by norm_num) i) j ∧ ¬ blackT10 j)).card = 2 := by
  unfold oneSided exCand blackT10 nameT nameT10
  decide

/-- Appendix C.1. The 2SUP link records one, Elijah. -/
theorem figA4b_one_pass :
    ((Finset.univ : Finset (Fin 3)).filter
      (fun i => ∃ j, twoSided exCand (Fin.castLE (by norm_num) i) j ∧ ¬ blackT10 j)).card = 1 := by
  unfold twoSided oneSided exCand blackT10 nameT nameT10
  decide

/-- Control. Without the backward condition the black Abe is counted as
passing, although his only candidate is the future self of a white Abe. -/
theorem control_backward_needed : oneSided exCand 2 2 ∧ ¬ twoSided exCand 2 2 :=
  ⟨figA4a_abe_linked, figA4b_abe_dropped 2⟩

/-! ### The extrapolation to the population -/

/-- The population rate of passing when a share `l` is linked, the linked pass
at rate `r` and the unlinked at rate `u`. -/
def popRate (l r u : ℝ) : ℝ := l * r + (1 - l) * u

/-- Section 5.1.1. Assuming that the unlinked never pass gives a lower bound. -/
theorem lower_bound {l r u : ℝ} (hl : l ≤ 1) (hu : 0 ≤ u) : l * r ≤ popRate l r u := by
  unfold popRate
  have : 0 ≤ (1 - l) * u := mul_nonneg (by linarith) hu
  linarith

/-- Section 5.1.1. `0.086 × 0.166 = 0.014276`, the "1.4%" of the text, and
5.6 times Eckard's 0.25%. -/
theorem lower_bound_value :
    (86 / 1000 : ℝ) * (166 / 1000) = 14276 / 1000000 ∧
      (14 / 1000 : ℝ) / (25 / 10000) = 56 / 10 := by
  norm_num

/-- Section 5.1.1. 30,239 passers in the 10% sample are about 302,390 in the
population. -/
theorem scale_up : (30239 : ℕ) * 10 = 302390 := by norm_num

/-- Appendix F. Adding back the 38% of the population with no linked
counterpart, assumed never to pass, gives `0.16 × (1 − 0.38) = 0.0992`. -/
theorem adjusted_weighted : (16 / 100 : ℝ) * (1 - 38 / 100) = 992 / 10000 := by norm_num

/-- Control. If some unlinked men pass and not everyone is linked, the bound is
strict, so 1.4% is a bound and not an estimate. -/
theorem control_bound_strict {l r u : ℝ} (hl : l < 1) (hu : 0 < u) :
    l * r < popRate l r u := by
  unfold popRate
  have : 0 < (1 - l) * u := mul_pos (by linarith) hu
  linarith

/-- Table 8, column 4, as the text sums it. The passing coefficient, the
interaction with mulatto and the mulatto coefficient sum to 2.331 > 0. -/
theorem table8_col4_sum : (3178 / 1000 : ℝ) - 1301 / 1000 + 454 / 1000 = 2331 / 1000 := by
  norm_num

/-! ### The group left behind (our reading)

A group of mean `m` splits into a share `s` that passes, with mean `mp`, and a
share `1 − s` that remains, with mean `ms`, so `m = s mp + (1 − s) ms`. -/

/-- If those who pass are above the group's mean, those who remain are below
it. This is the step from the selection reported in Table 7 to the composition
of the group left behind. -/
theorem left_behind_lower {s m mp ms : ℝ} (hs0 : 0 < s) (hs1 : s < 1)
    (hm : m = s * mp + (1 - s) * ms) (hp : m < mp) : ms < m := by
  by_contra h
  push_neg at h
  have h1 : s * m < s * mp := mul_lt_mul_of_pos_left hp hs0
  have h2 : (1 - s) * m ≤ (1 - s) * ms := mul_le_mul_of_nonneg_left h (by linarith)
  nlinarith

/-- The mean of those who remain, `ms = (m − s mp)/(1 − s)`. -/
theorem left_behind_mean {s m mp ms : ℝ} (hs1 : s < 1)
    (hm : m = s * mp + (1 - s) * ms) : ms = (m - s * mp) / (1 - s) := by
  have : (1 - s) ≠ 0 := by linarith
  field_simp
  linarith

/-- Control. Without selection, when those who pass have the group's mean,
the mean of those who remain is unchanged. -/
theorem control_no_selection {s m ms : ℝ} (hs1 : s < 1)
    (hm : m = s * m + (1 - s) * ms) : ms = m := by
  have h : (1 - s) * ms = (1 - s) * m := by linarith
  have : (1 - s) ≠ 0 := by linarith
  exact mul_left_cancel₀ this h

/-! ### The conditions for a false pass (Section 4, PDF p. 16)

The paper says the 2SUP link overstates passing only if three conditions hold
for the black man `i` of year `t`: (1) no black man of `t + 10` can be linked to
him, (2) exactly one white man of `t + 10` can, and (3) that white man links
back only to `i`. A recorded 2SUP pass to a white record implies all three. -/

/-- Section 4. A 2SUP link from `i` to a white record `j` has no black
candidate (condition 1), `j` as its only candidate (condition 2), and `i` as
the only backward candidate of `j` (condition 3). -/
theorem false_pass_conditions {α β : Type*} {cand : α → β → Prop} {black : β → Prop}
    {i : α} {j : β} (h : twoSided cand i j) (hw : ¬ black j) :
    (∀ j', cand i j' → ¬ black j') ∧ (∀ j', cand i j' → j' = j) ∧
      (∀ i', cand i' j → i' = i) := by
  refine ⟨fun j' hj' hb => hw ?_, h.1.2, h.2⟩
  rwa [← h.1.2 j' hj']

/-- Control for condition 2. A record with two candidates in `t + 10` has no
one-sided link at all, so it cannot be recorded as passing. -/
theorem control_two_candidates {α β : Type*} {cand : α → β → Prop} {i : α} {j₁ j₂ : β}
    (h₁ : cand i j₁) (h₂ : cand i j₂) (hne : j₁ ≠ j₂) : ∀ j, ¬ oneSided cand i j := by
  intro j hj
  exact hne ((hj.2 j₁ h₁).trans (hj.2 j₂ h₂).symm)

/-! ### The estimating models of Tables 7 and 8

Both are linear in the regressor of interest, a linear probability model for
passing (Table 7) and a linear model of the occupational score (Table 8). With
the other regressors fixed, the prediction moves by the coefficient times the
change in the regressor. -/

/-- A linear specification with coefficient `b` on the regressor `x` and the
rest of the index `r`. -/
def lin (b x r : ℝ) : ℝ := b * x + r

/-- The change in the prediction when the regressor moves from `x` to `x'`. -/
theorem lin_change (b x x' r : ℝ) : lin b x' r - lin b x r = b * (x' - x) := by
  unfold lin; ring

/-- Table 7, column 5, read as in DNQ-D3. Doubling a share `x` moves the
linear probability by `0.345 x`, which is `0.345` only at `x = 1`. -/
theorem doubling_share (x r : ℝ) :
    lin (345 / 1000) (2 * x) r - lin (345 / 1000) x r = 345 / 1000 * x := by
  unfold lin; ring

/-- Control for DNQ-D3. At an immigrant share of 0.1, doubling moves the
probability by 3.45 points, not 34.5. -/
theorem control_doubling_not_unit (r : ℝ) :
    lin (345 / 1000) (2 * (1 / 10)) r - lin (345 / 1000) (1 / 10) r ≠ 345 / 1000 := by
  rw [doubling_share]; norm_num

/-- Table 7, column 1. Legal intermarriage goes with 31.9 percentage points
less passing, the coefficient on a dummy in a linear probability model. -/
theorem miscegenation_points (r : ℝ) : lin (-319 / 1000) 1 r - lin (-319 / 1000) 0 r = -319 / 1000 := by
  unfold lin; ring

/-! ### Appendix F. Post-stratification and probit weights -/

section Weights

variable {K : Type*} [Fintype K]

/-- Footnote 95. With population shares `pop k` and linked-sample shares
`smp k` over cells `k`, the post-stratified rate weights each linked man by
`pop k / smp k`. Summed over a cell of linked share `smp k` and pass rate
`r k`, it is the population-share-weighted mean of the cell rates. -/
theorem post_stratified (pop smp r : K → ℝ) (h : ∀ k, smp k ≠ 0) :
    ∑ k, smp k * (pop k / smp k) * r k = ∑ k, pop k * r k := by
  refine Finset.sum_congr rfl (fun k _ => ?_)
  field_simp [h k]

/-- Appendix F, selection on observables (DNQ-Q13). If the unlinked of each
cell pass at the linked rate, the post-stratified rate is the population rate.
`ℓ k` is the linked fraction of cell `k` and `u k` the unlinked rate. -/
theorem post_stratified_eq_pop (pop ℓ r u : K → ℝ) (hu : ∀ k, u k = r k) :
    ∑ k, pop k * r k = ∑ k, pop k * (ℓ k * r k + (1 - ℓ k) * u k) := by
  refine Finset.sum_congr rfl (fun k _ => ?_)
  rw [hu k]; ring

/-- Control. If the unlinked of a cell pass at a different rate, the cell's
population rate differs from its linked rate. -/
theorem control_selection_unobservables {ℓ r u : ℝ} (hℓ : ℓ < 1) (hu : u ≠ r) :
    ℓ * r + (1 - ℓ) * u ≠ r := by
  intro h
  apply hu
  have h1 : (1 - ℓ) * (u - r) = 0 := by linarith
  rcases mul_eq_zero.1 h1 with h2 | h2
  · exact absurd h2 (by linarith)
  · linarith

/-- Footnote 97. The probit weight of a linked man with link propensity `P`,
when a share `q` of records is linked. -/
noncomputable def probitWeight (P q : ℝ) : ℝ := (1 - P) / P * (q / (1 - q))

/-- Footnote 97, read. In a cell of mass `N` and propensity `P`, the weighted
mass of the linked, `N P` times the weight, is the unlinked mass `N (1 − P)`
times the constant `q/(1 − q)`. The weights give the linked sample the
distribution of the unlinked over cells (DNQ-D13). -/
theorem probit_weighted_mass {N P q : ℝ} (hP : P ≠ 0) :
    N * P * probitWeight P q = N * (1 - P) * (q / (1 - q)) := by
  unfold probitWeight
  field_simp

end Weights

/-! ### Numbers in the text that follow from printed numbers -/

/-- Section 2.3, the genetic exercise. With European-Americans 72.4% and
African-Americans 12.6% of the population, a share `p` of European-Americans
with African ancestry is `p × 72.4/12.6` of today's black population. The
paper's 20%, 57.4%, 4.6% and 49.6% follow from `p` = 3.5%, 10%, 0.8% and 8.63%. -/
noncomputable def geneticRate (p : ℝ) : ℝ := p * (724 / 10) / (126 / 10)

theorem genetic_rates :
    (20 / 100 : ℝ) < geneticRate (35 / 1000) ∧ geneticRate (35 / 1000) < 21 / 100 ∧
      (574 / 1000 : ℝ) < geneticRate (10 / 100) ∧ geneticRate (10 / 100) < 575 / 1000 ∧
      (45 / 1000 : ℝ) < geneticRate (8 / 1000) ∧ geneticRate (8 / 1000) < 47 / 1000 ∧
      (495 / 1000 : ℝ) < geneticRate (863 / 10000) ∧ geneticRate (863 / 10000) < 497 / 1000 := by
  unfold geneticRate; norm_num

/-- Section 1, DNQ-D14. Johnson's 2.5% a year does not give "around 21%" per
ten years. Compounded it gives between 22% and 23%, and summed it gives 25%. -/
theorem johnson_intercensal :
    (22 / 100 : ℝ) < 1 - (1 - 25 / 1000) ^ 10 ∧ 1 - (1 - 25 / 1000 : ℝ) ^ 10 < 23 / 100 ∧
      (10 : ℝ) * (25 / 1000) = 25 / 100 := by
  norm_num

/-- Table 6, panel I. The "move county or state" column is the sum of the two
before it, for passers and non-passers in every row. -/
theorem table6_sums :
    (497 + 387 : ℕ) = 884 ∧ (513 + 408 : ℕ) = 921 ∧ (550 + 353 : ℕ) = 903 ∧
      (496 + 361 : ℕ) = 857 ∧ (500 + 417 : ℕ) = 917 ∧ (447 + 399 : ℕ) = 846 ∧
      (227 + 165 : ℕ) = 392 ∧ (324 + 189 : ℕ) = 513 ∧ (267 + 134 : ℕ) = 401 ∧
      (239 + 164 : ℕ) = 403 ∧ (214 + 205 : ℕ) = 419 ∧ (176 + 143 : ℕ) = 319 := by
  norm_num

/-- PDF p. 29. Non-passers move at "approximately half" the passers' rate,
`39.2/88.4`, between 0.44 and 0.45. -/
theorem migration_half : (44 / 100 : ℝ) < 392 / 884 ∧ (392 / 884 : ℝ) < 45 / 100 := by norm_num

/-- Appendix C.3. `0.01/0.08 = 0.125`. -/
theorem parents_birthstate : (1 / 100 : ℝ) / (8 / 100) = 125 / 1000 := by norm_num

/-- Table 2, row A. The shares of black men by race ten years later sum to
100% up to the cell printed as "<0.01%". -/
theorem table2_rowA : (848 + 150 + 1 + 1 : ℕ) = 1000 := by norm_num

/-- Figure A.5. The 2SUP share 8.6% of the base population is the 14.1%
unique-perfect share thinned by the backward condition to `0.086/0.141`, about
61% of the one-sided links. -/
theorem backward_retention : (60 / 100 : ℝ) < 86 / 141 ∧ (86 / 141 : ℝ) < 62 / 100 := by norm_num

/-- Table 4, columns 3 and 4. White men recorded as black ten years later, by
counts times pass rates. Those who become married to a black woman (rows B, E,
H) are `M`, and all are `M` plus rows A, D, G (rows C, F, I pass at 0%). `M`
is between 50% and 55% of the total. -/
theorem table4_white_to_black :
    let M : ℝ := 1151 * (988 / 1000) + 15 * (867 / 1000) + 1098 * (994 / 1000)
    let T : ℝ := M + 267111 * (7 / 1000) + 3 * (667 / 1000) + 3391 * (48 / 1000)
    1 / 2 < M / T ∧ M / T < 55 / 100 := by
  norm_num

end DahisEtAl2019
