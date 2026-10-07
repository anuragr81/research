import Mathlib
import Refutations
import FallWitness
import KappaSpread

open MeasureTheory ProbabilityTheory Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestMM

open EntryContestModel EntryContestP1P2 EntryContestP6P7 EntryContestPMU EntryContestPMUWitness
  EntryContestRefute EntryContestFall EntryContestKappa

/-! **The measurement-map checks MM1 to MM10 in Lean.** Each section replaces the SymPy block of
the same name in `checks/verify_measurement_map.py`, stated over the reals and, where the SymPy
fixed a size or a family, in general. -/

section MM1

/-! MM1. The linear family `T(w) = x0 + λ(w − x0)`: recovering `λ` and `x0` from the
displacements `d = T(w) − w` at two ranks. -/

theorem mm1c (x0 lam wa wb : ℝ) :
    (spread x0 lam wb - wb) - (spread x0 lam wa - wa) = (lam - 1) * (wb - wa) := by
  unfold spread
  ring

theorem mm1a (x0 lam wa wb : ℝ) (h : wa ≠ wb) :
    lam = 1 + ((spread x0 lam wa - wa) - (spread x0 lam wb - wb)) / (wa - wb) := by
  have hne : wa - wb ≠ 0 := sub_ne_zero.mpr h
  have e : (spread x0 lam wa - wa) - (spread x0 lam wb - wb) = (lam - 1) * (wa - wb) := by
    unfold spread
    ring
  rw [e, mul_div_cancel_right₀ _ hne]
  ring

theorem mm1b (x0 lam wa wb : ℝ) (h : wa ≠ wb) (hl : lam ≠ 1) :
    x0 = ((spread x0 lam wb - wb) * wa - (spread x0 lam wa - wa) * wb)
      / ((spread x0 lam wb - wb) - (spread x0 lam wa - wa)) := by
  have hd : (spread x0 lam wb - wb) - (spread x0 lam wa - wa) ≠ 0 := by
    rw [mm1c]
    exact mul_ne_zero (sub_ne_zero.mpr hl) (sub_ne_zero.mpr (Ne.symm h))
  rw [eq_div_iff hd]
  unfold spread
  ring

/-- Control. The swapped numerator does not recover `x0`. -/
theorem mm1d :
    ((spread 0 2 1 - 1) * 1 - (spread 0 2 2 - 2) * 2) / ((spread 0 2 2 - 2) - (spread 0 2 1 - 1))
      ≠ 0 := by
  unfold spread
  norm_num

end MM1

section MM2

/-! MM2. The linear spread about the profile mean, for any number of challengers. -/

/-- The mean of a wealth profile. -/
noncomputable def pmean {n : ℕ} (w : Fin n → ℝ) : ℝ := (∑ i, w i) / n

theorem sum_spread {n : ℕ} (w : Fin n → ℝ) (x lam : ℝ) :
    ∑ i, spread x lam (w i) = n * x + lam * ((∑ i, w i) - n * x) := by
  simp only [spread, Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, ← Finset.mul_sum, Finset.sum_sub_distrib]

theorem pmean_spread {n : ℕ} (hn : 0 < n) (w : Fin n → ℝ) (x lam : ℝ) :
    pmean (fun i => spread x lam (w i)) = pmean w + (1 - lam) * (x - pmean w) := by
  have hn' : (n : ℝ) ≠ 0 := Nat.cast_ne_zero.mpr hn.ne'
  unfold pmean
  rw [sum_spread]
  field_simp
  ring

/-- MM2a. A spread about the profile mean preserves the profile mean. -/
theorem mm2a {n : ℕ} (hn : 0 < n) (w : Fin n → ℝ) (lam : ℝ) :
    pmean (fun i => spread (pmean w) lam (w i)) = pmean w := by
  rw [pmean_spread hn]
  ring

/-- MM2b. The spread factor is read off any challenger away from the mean. -/
theorem mm2b (m lam wj : ℝ) (h : wj ≠ m) : (spread m lam wj - m) / (wj - m) = lam := by
  unfold spread
  rw [show m + lam * (wj - m) - m = lam * (wj - m) by ring, mul_div_assoc,
    div_self (sub_ne_zero.mpr h), mul_one]

/-- MM2c. Control. A spread about another point, with `λ ≠ 1`, moves the profile mean. -/
theorem mm2c {n : ℕ} (hn : 0 < n) (w : Fin n → ℝ) (x lam : ℝ) (hx : x ≠ pmean w)
    (hl : lam ≠ 1) : pmean (fun i => spread x lam (w i)) ≠ pmean w := by
  rw [pmean_spread hn]
  intro h
  have : (1 - lam) * (x - pmean w) = 0 := by linarith
  rcases mul_eq_zero.mp this with h1 | h1
  · exact hl (by linarith)
  · exact hx (by linarith)

end MM2

section MM345

/-! MM3 to MM5 are earlier theorems, stated here with the factor `V` as the measurement map uses
them. -/

/-- MM3. P6 with the factor `V`. At `μ = 0`, with the incumbent investing and `G` the point mass
    at 0, `Δ(m) = V/(m + 2)` for every atomless investor law with no mass at or below 0. -/
theorem mm3 (α : Measure ℝ) [IsProbabilityMeasure α] [NoAtoms α] (V : ℝ) (Q m : ℕ)
    (h0 : α (Iic 0) = 0) : Delta V α (Measure.dirac 0) α Q m = V / (m + 2) :=
  p6_evaluation α V Q m h0

/-- MM4. The P7 base integral with the factor `V`. -/
theorem mm4 (α : Measure ℝ) [IsProbabilityMeasure α] [NoAtoms α] (V : ℝ) (m : ℕ) :
    V * ∫ x, cdf α x ^ m ∂α = V / (m + 1) := by
  rw [integral_cdf_pow α m]
  ring

/-- MM5a. The pointwise identity behind P-MU. -/
theorem mm5a (V F G : ℝ) (n : ℕ) : V * (G ^ (n + 1) - G ^ n) * F = -V * (G ^ n * (1 - G) * F) := by
  ring

/-- MM5. The P-MU identity with the factor `V`, for every incumbent law. -/
theorem mm5 (V : ℝ) (α β C : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]
    [IsProbabilityMeasure C] (Q : ℕ) (hQ : 1 ≤ Q) :
    stepQ V α β C Q = -V * ((∫ x, weight β C Q x ∂α) - ∫ x, weight β C Q x ∂β) :=
  pmu_identity V α β C Q hQ

/-- MM5b, the first family. With `F = x²`, `G = x` and `Q = 3`, the step is `−V/70`. -/
theorem mm5b (V : ℝ) : stepQ V lawB unif lawB 3 = -V / 70 := by
  rw [show (3 : ℕ) = 2 + 1 from rfl, pairB_step V 2]
  norm_num
  ring

end MM345

section MM6

/-! MM6. The entry condition is invariant to `(u, V) ↦ (a u + b, a V)` with `a > 0`. -/

/-- MM6a. The cost of entry scales by `a`, and `b` cancels. -/
theorem mm6a (u : ℝ → ℝ) (a b c w : ℝ) :
    kappa (fun x => a * u x + b) c w = a * kappa u c w := by
  unfold kappa
  ring

/-- The gain scales by `a` when `V` does. -/
theorem Delta_scale (a V : ℝ) (α β C : Measure ℝ) (Q m : ℕ) :
    Delta (a * V) α β C Q m = a * Delta V α β C Q m := by
  unfold Delta
  ring

/-- MM6b. Every entry condition is unchanged under the rescaling. -/
theorem mm6b (u : ℝ → ℝ) (a b c w V : ℝ) (ha : 0 < a) (α β C : Measure ℝ) (Q m : ℕ) :
    kappa (fun x => a * u x + b) c w ≤ Delta (a * V) α β C Q m
      ↔ kappa u c w ≤ Delta V α β C Q m := by
  rw [mm6a, Delta_scale]
  exact mul_le_mul_iff_of_pos_left ha

/-- MM6c. Control. Rescaling `V` alone changes the gap unless `a = 1` or the cost is 0. -/
theorem mm6c (a V e κ : ℝ) : (a * V) * e - κ = a * (V * e - κ) ↔ κ = 0 ∨ a = 1 := by
  constructor
  · intro h
    have : κ * (a - 1) = 0 := by linarith
    rcases mul_eq_zero.mp this with h1 | h1
    · exact Or.inl h1
    · exact Or.inr (by linarith)
  · rintro (h | h) <;> subst h <;> ring

/-- MM6d. A statement invariant under the rescaling depends on `κ` and `V` only through `κ/V`. -/
theorem mm6d (f : ℝ → ℝ → Prop) (hf : ∀ a, 0 < a → ∀ κ V, f (a * κ) (a * V) ↔ f κ V)
    (κ V : ℝ) (hV : 0 < V) : f κ V ↔ f (κ / V) 1 := by
  have h := hf (1 / V) (by positivity) κ V
  rw [show 1 / V * κ = κ / V by ring, show 1 / V * V = 1 by field_simp] at h
  exact h.symm

/-- MM6e. The difference in total cost between two entrant sets scales by `a`. -/
theorem mm6e {ι : Type*} (κ : ι → ℝ) (S T : Finset ι) (a : ℝ) :
    (∑ i ∈ T, a * κ i) - ∑ i ∈ S, a * κ i = a * ((∑ i ∈ T, κ i) - ∑ i ∈ S, κ i) := by
  rw [← Finset.mul_sum, ← Finset.mul_sum]
  ring

/-- MM6f. A sum of payoffs moves with `b` in level, and its difference across two sets of the
    same size scales by `a`. -/
theorem mm6f {ι : Type*} (U : ι → ℝ) (S T : Finset ι) (a b : ℝ) (hST : S.card = T.card) :
    (∑ i ∈ S, (a * U i + b)) = a * (∑ i ∈ S, U i) + S.card * b
      ∧ (∑ i ∈ T, (a * U i + b)) - (∑ i ∈ S, (a * U i + b))
        = a * ((∑ i ∈ T, U i) - ∑ i ∈ S, U i) := by
  have e : ∀ R : Finset ι, (∑ i ∈ R, (a * U i + b)) = a * (∑ i ∈ R, U i) + R.card * b := by
    intro R
    rw [Finset.sum_add_distrib, ← Finset.mul_sum, Finset.sum_const, nsmul_eq_mul]
  refine ⟨e S, ?_⟩
  rw [e S, e T, hST]
  ring

end MM6

section MM7

/-! MM7. The example of `PROOFS.tex`, with gains `Δ = (12, 10, 1)` and costs `κ = (2, 5, 8)` in
increasing cost order. A member of a size-`k` set compares its cost with `Δ(k − 1)` and an
outsider with `Δ(k)`. -/

/-- The gain at count `k`. -/
def gain7 (k : ℕ) : ℕ := if k = 0 then 12 else if k = 1 then 10 else if k = 2 then 1 else 0

/-- The costs, cheapest first. -/
def cost7 (i : Fin 3) : ℕ := if i.val = 0 then 2 else if i.val = 1 then 5 else 8

/-- A pure-strategy equilibrium entrant set. -/
def IsEq7 (S : Finset (Fin 3)) : Prop :=
  (∀ i ∈ S, cost7 i ≤ gain7 (S.card - 1)) ∧ (∀ i ∉ S, gain7 S.card < cost7 i)

instance : DecidablePred IsEq7 := fun S => by
  unfold IsEq7
  infer_instance

/-- The equilibria are exactly the three pairs. -/
theorem mm7_equilibria :
    ∀ S : Finset (Fin 3), IsEq7 S ↔ S = {0, 1} ∨ S = {0, 2} ∨ S = {1, 2} := by
  decide

/-- MM7a. Every equilibrium has the count `k* = 2`. -/
theorem mm7a : ∀ S : Finset (Fin 3), IsEq7 S → S.card = 2 := by
  decide

/-- MM7b. Some equilibrium omits the challenger at rank `k*`. -/
theorem mm7b : IsEq7 {0, 2} ∧ (1 : Fin 3) ∉ ({0, 2} : Finset (Fin 3)) := by
  decide

end MM7

section MM8

/-! MM8. The upper end of the non-investor's score support. -/

/-- With `r` uniform on `[0, 1]`, the non-investor's CDF reaches 1 exactly from `c` on. -/
theorem cdf_nonInvestor_unif_eq_one (c : ℝ) (hc : 0 < c) (x : ℝ) :
    cdf (nonInvestorLaw c unif) x = 1 ↔ c ≤ x := by
  constructor
  · intro h
    by_contra hx
    push_neg at hx
    have e : cdf (nonInvestorLaw c unif) x = cdf unif (x / c) := cdf_map_mul unif c hc x
    rw [e] at h
    rcases lt_or_ge (x / c) 0 with h0 | h0
    · rw [cdf_unif_of_neg _ h0] at h
      norm_num at h
    · have h1 : x / c < 1 := by rw [div_lt_one hc]; exact hx
      rw [cdf_unif_of_mem _ h0 h1.le] at h
      linarith
  · exact nonInvestor_unif_Iic c hc x

/-- `r` uniform on `[0, b]`. -/
noncomputable def unifB (b : ℝ) : Measure ℝ := unif.map (fun r => b * r)

instance unifB_isProb (b : ℝ) : IsProbabilityMeasure (unifB b) := by
  unfold unifB
  infer_instance

theorem nonInvestor_unifB (μ b : ℝ) : nonInvestorLaw μ (unifB b) = nonInvestorLaw (μ * b) unif := by
  unfold nonInvestorLaw unifB
  rw [Measure.map_map (by fun_prop) (by fun_prop)]
  congr 1
  funext r
  simp only [Function.comp_apply]
  ring

/-- MM8a. With `r` uniform on `[0, b]`, the top of the non-investor's support is `μ b`. -/
theorem mm8a (μ b : ℝ) (hμ : 0 < μ) (hb : 0 < b) (x : ℝ) :
    cdf (nonInvestorLaw μ (unifB b)) x = 1 ↔ μ * b ≤ x := by
  rw [nonInvestor_unifB]
  exact cdf_nonInvestor_unif_eq_one (μ * b) (mul_pos hμ hb) x

/-- MM8b. The top of the support is `μ` exactly when `b = 1`. -/
theorem mm8b (μ b : ℝ) (hμ : 0 < μ) (hb : 0 < b) :
    (∀ x, cdf (nonInvestorLaw μ (unifB b)) x = 1 ↔ μ ≤ x) ↔ b = 1 := by
  constructor
  · intro h
    have h1 := (mm8a μ b hμ hb μ).mp ((h μ).mpr le_rfl)
    have h2 := (h (μ * b)).mp ((mm8a μ b hμ hb (μ * b)).mpr le_rfl)
    have hb1 : b ≤ 1 := by nlinarith
    have hb2 : 1 ≤ b := by nlinarith
    linarith
  · intro h x
    subst h
    rw [mm8a μ 1 hμ one_pos x, mul_one]

end MM8

section MM9

/-! MM9. The entry condition is a comparison of expected payoffs. A challenger who enters pays
`c` whether or not it wins, and the winner receives `V`. -/

/-- MM9a. Expected payoff with entry minus expected payoff without is `V(P1 − P0) − κ(w)`. -/
theorem mm9a (u : ℝ → ℝ) (w c V P1 P0 : ℝ) :
    ((1 - P1) * u (w - c) + P1 * (u (w - c) + V)) - ((1 - P0) * u w + P0 * (u w + V))
      = V * (P1 - P0) - kappa u c w := by
  unfold kappa
  ring

/-- MM9b. Control. Paying `c` only when losing adds `P1 κ(w)`, which breaks the identity. -/
theorem mm9b (u : ℝ → ℝ) (w c V P1 P0 : ℝ) :
    ((1 - P1) * u (w - c) + P1 * (u w + V)) - ((1 - P0) * u w + P0 * (u w + V))
      - (V * (P1 - P0) - kappa u c w) = P1 * kappa u c w := by
  unfold kappa
  ring

/-- In the model the win probabilities are the two integrals of `Δ`, so entering is weakly better
    exactly when the cost is within the gain. -/
theorem mm9_model (u : ℝ → ℝ) (w c V : ℝ) (α β C : Measure ℝ) (Q m : ℕ) :
    let P1 := ∫ x, cdf (rivals C α β m (Q - 1 - m)) x ∂α
    let P0 := ∫ x, cdf (rivals C α β m (Q - 1 - m)) x ∂β
    ((1 - P0) * u w + P0 * (u w + V) ≤ (1 - P1) * u (w - c) + P1 * (u (w - c) + V))
      ↔ kappa u c w ≤ Delta V α β C Q m := by
  intro P1 P0
  have h := mm9a u w c V P1 P0
  unfold Delta
  constructor <;> intro h' <;> linarith

end MM9

section MM10

/-! MM10. The ranking by expected payoff is invariant under `a U + b` with `a > 0`, and not under
a monotone nonlinear map. -/

/-- Expected payoff of a finite lottery with probabilities `p` over prizes `x`. -/
noncomputable def eu {k : ℕ} (p x : Fin k → ℝ) (U : ℝ → ℝ) : ℝ := ∑ i, p i * U (x i)

theorem eu_affine {k : ℕ} (p x : Fin k → ℝ) (hp : ∑ i, p i = 1) (U : ℝ → ℝ) (a b : ℝ) :
    eu p x (fun t => a * U t + b) = a * eu p x U + b := by
  unfold eu
  simp only [mul_add, Finset.sum_add_distrib, ← Finset.sum_mul, hp, one_mul]
  rw [Finset.mul_sum]
  congr 1
  refine Finset.sum_congr rfl (fun i _ => ?_)
  ring

/-- MM10a. With `a > 0`, the ranking of any two lotteries is unchanged. -/
theorem mm10a {k l : ℕ} (p x : Fin k → ℝ) (q y : Fin l → ℝ) (hp : ∑ i, p i = 1)
    (hq : ∑ i, q i = 1) (U : ℝ → ℝ) (a b : ℝ) (ha : 0 < a) :
    eu p x (fun t => a * U t + b) ≤ eu q y (fun t => a * U t + b) ↔ eu p x U ≤ eu q y U := by
  rw [eu_affine p x hp, eu_affine q y hq]
  constructor
  · intro h
    exact le_of_mul_le_mul_left (by linarith) ha
  · intro h
    nlinarith

/-- The two lotteries of the check. `A` pays 0 or 10 with probability 1/2 each, and `B` pays 4. -/
noncomputable def pA : Fin 2 → ℝ := ![1 / 2, 1 / 2]
noncomputable def xA : Fin 2 → ℝ := ![0, 10]
noncomputable def pB : Fin 1 → ℝ := ![1]
noncomputable def xB : Fin 1 → ℝ := ![4]

/-- Under `U = √`, lottery `B` is preferred. -/
theorem mm10_sqrt : eu pA xA Real.sqrt < eu pB xB Real.sqrt := by
  simp only [eu, pA, xA, pB, xB, Fin.sum_univ_two, Fin.sum_univ_one, Matrix.cons_val_zero,
    Matrix.cons_val_one]
  have h4 : Real.sqrt 4 = 2 := by
    rw [show (4 : ℝ) = 2 ^ 2 by norm_num, Real.sqrt_sq (by norm_num)]
  have h10 : Real.sqrt 10 < 4 := by
    rw [Real.sqrt_lt' (by norm_num)]
    norm_num
  rw [h4, Real.sqrt_zero]
  linarith

/-- MM10a on the example. Under `3√ + 7`, lottery `B` is still preferred. -/
theorem mm10a_example :
    eu pA xA (fun t => 3 * Real.sqrt t + 7) < eu pB xB (fun t => 3 * Real.sqrt t + 7) := by
  have hpA : ∑ i, pA i = 1 := by simp [pA, Fin.sum_univ_two]; norm_num
  have hpB : ∑ i, pB i = 1 := by simp [pB]
  rw [eu_affine pA xA hpA, eu_affine pB xB hpB]
  linarith [mm10_sqrt]

/-- MM10b. Control. Under `(√)⁴`, monotone on the nonnegative reals, the ranking reverses. -/
theorem mm10b :
    eu pB xB (fun t => Real.sqrt t ^ 4) < eu pA xA (fun t => Real.sqrt t ^ 4) := by
  have e : ∀ t : ℝ, 0 ≤ t → Real.sqrt t ^ 4 = t ^ 2 := by
    intro t ht
    rw [show Real.sqrt t ^ 4 = (Real.sqrt t ^ 2) ^ 2 by ring, Real.sq_sqrt ht]
  simp only [eu, pA, xA, pB, xB, Fin.sum_univ_two, Fin.sum_univ_one, Matrix.cons_val_zero,
    Matrix.cons_val_one]
  rw [e 4 (by norm_num), e 0 le_rfl, e 10 (by norm_num)]
  norm_num

end MM10

section CountInvariance

/-! **P5-inv without the counting hypotheses.** `lean/EntryContest.lean` proves that every
equilibrium has size `k*` given two counting facts about a size-`k` set of indices, which it left
to enumeration in `checks/verify_equilibria.py`. Both are proved here, so the count invariance
holds with no hypothesis beyond the model's. -/

/-- Some member of a nonempty set of `k` indices sits at index `k − 1` or above. -/
theorem exists_mem_ge (S : Finset ℕ) (hk : 1 ≤ S.card) : ∃ i ∈ S, S.card - 1 ≤ i := by
  by_contra h
  push_neg at h
  have hsub : S ⊆ Finset.range (S.card - 1) := fun i hi => Finset.mem_range.mpr (h i hi)
  have := Finset.card_le_card hsub
  rw [Finset.card_range] at this
  omega

/-- Some index `k` or below lies outside a set of `k` indices. -/
theorem exists_not_mem_le (S : Finset ℕ) : ∃ j, j ≤ S.card ∧ j ∉ S := by
  by_contra h
  push_neg at h
  have hsub : Finset.range (S.card + 1) ⊆ S := fun j hj =>
    h j (Nat.lt_succ_iff.mp (Finset.mem_range.mp hj))
  have := Finset.card_le_card hsub
  rw [Finset.card_range] at this
  omega

/-- **P5-inv, closed.** With costs in increasing order, every pure-strategy equilibrium entrant set
    has size `k*`. -/
theorem count_invariance (cost gain : ℕ → ℝ) (hcost : Monotone cost) (kstar : ℕ)
    (hmax : ∀ m, cost (m - 1) ≤ gain (m - 1) → m ≤ kstar)
    (hprefix : ∀ j, 1 ≤ j → j ≤ kstar → cost (j - 1) ≤ gain (j - 1))
    (S : Finset ℕ) (hmem : ∀ i ∈ S, cost i ≤ gain (S.card - 1))
    (hout : ∀ j ∉ S, gain S.card < cost j) :
    S.card = kstar := by
  obtain ⟨j, hj, hjS⟩ := exists_not_mem_le S
  have hge := EntryContest.count_ge_kstar (fun a b : ℝ => a ≤ b) (fun _ _ _ => le_trans) cost gain
    (fun i j h => hcost h) S.card kstar hprefix j hj (not_le.mpr (hout j hjS))
  rcases Nat.eq_zero_or_pos S.card with h0 | hpos
  · omega
  · obtain ⟨i, hiS, hi⟩ := exists_mem_ge S hpos
    have hle := EntryContest.count_le_kstar (fun a b : ℝ => a ≤ b) (fun _ _ _ => le_trans) cost
      gain (fun i j h => hcost h) S.card kstar hmax i hi (hmem i hiS)
    omega

end CountInvariance

end EntryContestMM
