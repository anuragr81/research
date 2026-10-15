import Mathlib
import MuMonotone
import PMUWitness
import KappaSpread

/-!
# The winner's base component and the weight `μ` (W-3 of WEIGHT_ADDENDUM.tex)

W-3 conjectured that the winner's expected base component does not fall as the rule weighs the
base component more. This file settles it in two parts, for the robustness of the model.

## What is proved

1. At a fixed set of entrants it holds, pointwise and for any laws: if `i` has the highest score
   at `μ₁` and `j` at `μ₂`, with `μ₁ < μ₂ ≤ 1`, then `r_i ≤ r_j` (`winner_base_mono`), whatever
   the bought scores, so the expectation does not fall either (`winner_base_mono_expect`).
2. Once entry responds it can fail, in the model's own terms. With one challenger, the
   incumbent scored as an entrant, `r` uniform on `[0, 1]`, a bonus `s ≡ 1` and `V = 1`:
   - the gain is `1/2` at `μ = 1/2` and `3/8` at `μ = 2/3` (`gain_half`, `gain_two_thirds`),
     computed on the paper's `Delta` through `Delta_normalise`;
   - at a cost `κ = 7/16` the challenger enters at `μ = 1/2` and stays out at `μ = 2/3`
     (`enters_at_half`, `out_at_two_thirds`), as M28 allows;
   - with the challenger in, the bonus shifts both scores equally and the winner has the higher
     base component; with the challenger out, the incumbent's bonus beats a challenger with a
     better base component, and the winner's expected base component is strictly lower at
     `μ = 2/3` than at `μ = 1/2` (`selection_falls_with_exit`).
   A higher weight lowers entry (K8) and, with a common bonus, lowers the quality of the winner.

Controls: the fixed-set result needs `μ₂ ≤ 1` (`winner_base_mono_needs_le_one`).
-/

open MeasureTheory ProbabilityTheory Set

set_option linter.unusedSectionVars false

namespace WeightSelection

/-! ## 1. At a fixed set of entrants -/

/-- If `i` has the highest score at `μ₁` and `j` at `μ₂`, with `μ₁ < μ₂ ≤ 1`, then `r_i ≤ r_j`,
    for any bought scores `a` (`a_k = s_k e_k`). -/
theorem winner_base_mono {n : ℕ} (r a : Fin n → ℝ) {μ₁ μ₂ : ℝ} (h12 : μ₁ < μ₂) (h2 : μ₂ ≤ 1)
    {i j : Fin n} (hi : μ₁ * r j + (1 - μ₁) * a j ≤ μ₁ * r i + (1 - μ₁) * a i)
    (hj : μ₂ * r i + (1 - μ₂) * a i ≤ μ₂ * r j + (1 - μ₂) * a j) : r i ≤ r j := by
  have h1 : 0 ≤ 1 - μ₁ := by linarith
  have h2' : 0 ≤ 1 - μ₂ := by linarith
  have k1 := mul_le_mul_of_nonneg_left hi h2'
  have k2 := mul_le_mul_of_nonneg_left hj h1
  by_contra hcon
  push_neg at hcon
  nlinarith [mul_pos (by linarith : (0:ℝ) < μ₂ - μ₁) (by linarith : (0:ℝ) < r i - r j)]

/-- The expectation of the winner's base component does not fall in `μ` at a fixed set of
    entrants: pointwise monotonicity integrated. -/
theorem winner_base_mono_expect {Ω : Type*} [MeasurableSpace Ω] {P : Measure Ω} {n : ℕ}
    (r a : Fin n → Ω → ℝ) {μ₁ μ₂ : ℝ} (h12 : μ₁ < μ₂) (h2 : μ₂ ≤ 1) (W₁ W₂ : Ω → Fin n)
    (hW₁ : ∀ ω k, μ₁ * r k ω + (1 - μ₁) * a k ω ≤ μ₁ * r (W₁ ω) ω + (1 - μ₁) * a (W₁ ω) ω)
    (hW₂ : ∀ ω k, μ₂ * r k ω + (1 - μ₂) * a k ω ≤ μ₂ * r (W₂ ω) ω + (1 - μ₂) * a (W₂ ω) ω)
    (hi₁ : Integrable (fun ω => r (W₁ ω) ω) P) (hi₂ : Integrable (fun ω => r (W₂ ω) ω) P) :
    ∫ ω, r (W₁ ω) ω ∂P ≤ ∫ ω, r (W₂ ω) ω ∂P :=
  integral_mono hi₁ hi₂ (fun ω =>
    winner_base_mono (fun k => r k ω) (fun k => a k ω) h12 h2 (hW₁ ω (W₂ ω)) (hW₂ ω (W₁ ω)))

/-- Control: above `μ = 1` the order reverses (the bought score enters with a negative weight). -/
theorem winner_base_mono_needs_le_one :
    let r : Fin 2 → ℝ := ![1, 0]
    let a : Fin 2 → ℝ := ![3, 0]
    (1 * r 1 + (1 - 1) * a 1 ≤ 1 * r 0 + (1 - 1) * a 0) ∧
      (3 * r 0 + (1 - 3) * a 0 ≤ 3 * r 1 + (1 - 3) * a 1) ∧ r 1 < r 0 := by
  norm_num

/-! ## 2. With entry responding: a counterexample in the model's terms -/

section Counterexample

open EntryContestModel EntryContestP1P2 EntryContestMu EntryContestPMU EntryContestPMUWitness

/-- The bought component is a fixed bonus, `s ≡ 1`, as Chiba City's four points are. -/
noncomputable def bonus : Measure ℝ := Measure.dirac 1

instance : IsProbabilityMeasure bonus := by unfold bonus; infer_instance

/-- With a fixed bonus, the normalised entrant law is the uniform law shifted by `t`. -/
theorem payerLaw_bonus (t : ℝ) : payerLaw t unif bonus = unif.map (fun r => r + t) := by
  unfold payerLaw bonus
  rw [Measure.prod_dirac, Measure.map_map (measurable_payerScore t) (by fun_prop)]
  congr 1
  funext r
  simp

theorem cdf_payerLaw_bonus (t y : ℝ) : cdf (payerLaw t unif bonus) y = cdf unif (y - t) := by
  rw [payerLaw_bonus]
  haveI : IsProbabilityMeasure (unif.map (fun r => r + t)) :=
    Measure.isProbabilityMeasure_map (by fun_prop)
  rw [cdf_eq_real, cdf_eq_real, Measure.real, Measure.real,
    Measure.map_apply (by fun_prop) measurableSet_Iic]
  congr 2
  ext r
  simp only [mem_preimage, mem_Iic]
  constructor <;> intro h <;> linarith

/-- With one challenger (`Q = 1`, no other entrants), the gain is
    `V (∫ F dF − ∫ F dG)` for the incumbent scored as an entrant. -/
theorem Delta_one_zero (V : ℝ) (α β : Measure ℝ) [IsProbabilityMeasure α]
    [IsProbabilityMeasure β] :
    Delta V α β α 1 0 = V * ((∫ x, cdf α x ∂α) - (∫ x, cdf α x ∂β)) := by
  unfold Delta
  simp only [show (1 : ℕ) - 1 - 0 = 0 from rfl, cdf_rivals, pow_zero, mul_one]

theorem self_integral_half (α : Measure ℝ) [IsProbabilityMeasure α] [NoAtoms α] :
    ∫ x, cdf α x ∂α = 1 / 2 := by
  have := two_max_sum α α
  linarith

theorem integral_shift_one : ∫ y, cdf unif (y - 1) ∂unif = 0 := by
  unfold unif
  refine setIntegral_eq_zero_of_forall_eq_zero (fun y hy => ?_)
  rcases eq_or_lt_of_le hy.2 with h | h
  · rw [h, sub_self]; exact cdf_unif_of_mem 0 le_rfl zero_le_one
  · exact cdf_unif_of_neg _ (by linarith)

theorem integral_shift_half : ∫ y, cdf unif (y - 1/2) ∂unif = 1 / 8 := by
  have hmono : Monotone (fun y => cdf unif (y - 1/2)) :=
    fun a b hab => (cdf unif).mono (by linarith)
  rw [show (∫ y, cdf unif (y - 1/2) ∂unif) = ∫ y in Icc (0:ℝ) 1, cdf unif (y - 1/2) from rfl,
    integral_Icc_eq_integral_Ioc, ← intervalIntegral.integral_of_le zero_le_one,
    ← intervalIntegral.integral_add_adjacent_intervals (b := 1/2)
      (hmono.intervalIntegrable) (hmono.intervalIntegrable)]
  have h1 : ∫ y in (0:ℝ)..1/2, cdf unif (y - 1/2) = 0 := by
    rw [intervalIntegral.integral_congr (g := fun _ => (0:ℝ))]
    · simp
    · intro y hy
      rw [uIcc_of_le (by norm_num)] at hy
      rcases eq_or_lt_of_le hy.2 with h | h
      · simp only; rw [h, sub_self]; exact cdf_unif_of_mem 0 le_rfl zero_le_one
      · exact cdf_unif_of_neg _ (by linarith)
  have h2 : ∫ y in (1/2:ℝ)..1, cdf unif (y - 1/2) = 1 / 8 := by
    rw [intervalIntegral.integral_congr (g := fun y => y - 1/2)]
    · simp [intervalIntegral.integral_sub, integral_id]; norm_num
    · intro y hy
      rw [uIcc_of_le (by norm_num)] at hy
      exact cdf_unif_of_mem _ (by linarith [hy.1]) (by linarith [hy.2])
  rw [h1, h2]; norm_num

/-- The gain at `μ = 1/2`, on the paper's laws, with `V = 1`: `1/2`. -/
theorem gain_half :
    Delta 1 (investorLaw (1/2) unif bonus) (nonInvestorLaw (1/2) unif)
      (investorLaw (1/2) unif bonus) 1 0 = 1 / 2 := by
  rw [Delta_normalise unif bonus 1 (1/2) (by norm_num) 1 0,
    show ((1:ℝ) - 1/2) / (1/2) = 1 by norm_num, Delta_one_zero, self_integral_half]
  have : ∫ x, cdf (payerLaw 1 unif bonus) x ∂unif = 0 := by
    simp_rw [cdf_payerLaw_bonus]; exact integral_shift_one
  rw [this]; norm_num

/-- The gain at `μ = 2/3`, on the paper's laws, with `V = 1`: `3/8`. -/
theorem gain_two_thirds :
    Delta 1 (investorLaw (2/3) unif bonus) (nonInvestorLaw (2/3) unif)
      (investorLaw (2/3) unif bonus) 1 0 = 3 / 8 := by
  rw [Delta_normalise unif bonus 1 (2/3) (by norm_num) 1 0,
    show ((1:ℝ) - 2/3) / (2/3) = 1/2 by norm_num, Delta_one_zero, self_integral_half]
  have : ∫ x, cdf (payerLaw (1/2) unif bonus) x ∂unif = 1 / 8 := by
    simp_rw [cdf_payerLaw_bonus]; exact integral_shift_half
  rw [this]; norm_num

/-- A challenger whose fee costs `7/16` in utility (linear utility, fee `7/16`) enters at
    `μ = 1/2`. -/
theorem enters_at_half (w : ℝ) :
    EntryContestKappa.kappa (fun x => x) (7/16) w ≤
      Delta 1 (investorLaw (1/2) unif bonus) (nonInvestorLaw (1/2) unif)
        (investorLaw (1/2) unif bonus) 1 0 := by
  rw [gain_half]; unfold EntryContestKappa.kappa; norm_num

/-- The same challenger stays out at `μ = 2/3`. -/
theorem out_at_two_thirds (w : ℝ) :
    Delta 1 (investorLaw (2/3) unif bonus) (nonInvestorLaw (2/3) unif)
      (investorLaw (2/3) unif bonus) 1 0 < EntryContestKappa.kappa (fun x => x) (7/16) w := by
  rw [gain_two_thirds]; unfold EntryContestKappa.kappa; norm_num

/-- The winner's base component when the incumbent (base `p.1`) holds the bonus and the
    challenger (base `p.2`) holds it if `e₁ = 1`; ties go to the incumbent, a null event. -/
noncomputable def winnerBase (μ e₁ : ℝ) (p : ℝ × ℝ) : ℝ :=
  if μ * p.1 + (1 - μ) * 1 < μ * p.2 + (1 - μ) * e₁ then p.2 else p.1

theorem winnerBase_measurable (μ e₁ : ℝ) : Measurable (winnerBase μ e₁) := by
  unfold winnerBase
  exact Measurable.ite (measurableSet_lt (by fun_prop) (by fun_prop)) measurable_snd measurable_fst

theorem winnerBase_in (p : ℝ × ℝ) : winnerBase (1/2) 1 p = max p.1 p.2 := by
  unfold winnerBase
  split_ifs with h
  · rw [max_eq_right (by linarith)]
  · rw [max_eq_left (by linarith)]

theorem winnerBase_le_max (μ e₁ : ℝ) (p : ℝ × ℝ) : winnerBase μ e₁ p ≤ max p.1 p.2 := by
  unfold winnerBase
  split_ifs
  · exact le_max_right _ _
  · exact le_max_left _ _

theorem winnerBase_integrable (μ e₁ : ℝ) : Integrable (winnerBase μ e₁) (unif.prod unif) := by
  have hb : Integrable (fun p : ℝ × ℝ => |p.1| + |p.2|) (unif.prod unif) := by
    have hid : Integrable (fun x : ℝ => |x|) unif := by
      unfold unif
      exact (continuous_abs.integrableOn_Icc)
    exact (hid.comp_fst unif).add (hid.comp_snd unif)
  refine hb.mono' (winnerBase_measurable μ e₁).aestronglyMeasurable (ae_of_all _ (fun p => ?_))
  unfold winnerBase
  split_ifs
  · rw [Real.norm_eq_abs]; linarith [abs_nonneg p.1]
  · rw [Real.norm_eq_abs]; linarith [abs_nonneg p.2]

/-- **Selection falls with exit.** The winner's expected base component is strictly lower at
    `μ = 2/3`, where the challenger stays out (`out_at_two_thirds`), than at `μ = 1/2`, where she
    enters (`enters_at_half`). -/
theorem selection_falls_with_exit :
    ∫ p, winnerBase (2/3) 0 p ∂(unif.prod unif) < ∫ p, winnerBase (1/2) 1 p ∂(unif.prod unif) := by
  set g : ℝ × ℝ → ℝ := fun p => winnerBase (1/2) 1 p - winnerBase (2/3) 0 p with hg
  have hg0 : 0 ≤ g := fun p => by
    simp only [hg, Pi.zero_apply, sub_nonneg]
    rw [winnerBase_in]; exact winnerBase_le_max _ _ p
  have hgi : Integrable g (unif.prod unif) :=
    (winnerBase_integrable _ _).sub (winnerBase_integrable _ _)
  have hbox : Ioo (1/10 : ℝ) (2/10) ×ˢ Ioo (25/100 : ℝ) (30/100) ⊆ Function.support g := by
    intro p hp
    simp only [mem_prod, mem_Ioo] at hp
    simp only [Function.mem_support, hg, winnerBase_in]
    unfold winnerBase
    rw [if_neg (by nlinarith), max_eq_right (by linarith)]
    linarith
  have hunif : ∀ a b : ℝ, 0 ≤ a → a < b → b ≤ 1 → unif (Ioo a b) = ENNReal.ofReal (b - a) := by
    intro a b ha hab hb
    unfold unif
    rw [Measure.restrict_apply measurableSet_Ioo,
      inter_eq_left.mpr (Ioo_subset_Icc_self.trans (Icc_subset_Icc ha hb)), Real.volume_Ioo]
  have hpos : 0 < (unif.prod unif) (Function.support g) := by
    refine lt_of_lt_of_le ?_ (measure_mono hbox)
    rw [Measure.prod_prod, hunif _ _ (by norm_num) (by norm_num) (by norm_num),
      hunif _ _ (by norm_num) (by norm_num) (by norm_num)]
    norm_num
  have := (integral_pos_iff_support_of_nonneg hg0 hgi).mpr hpos
  rw [hg, integral_sub (winnerBase_integrable _ _) (winnerBase_integrable _ _)] at this
  linarith

end Counterexample

end WeightSelection
