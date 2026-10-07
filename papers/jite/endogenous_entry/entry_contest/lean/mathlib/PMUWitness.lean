import Mathlib
import PMU
import SaturationBenchmark

open MeasureTheory ProbabilityTheory Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestPMUWitness

open EntryContestModel EntryContestP1P2 EntryContestP6P7 EntryContestPMU

/-! **First-order stochastic dominance does not sign the P-MU comparison.** Two admissible
pairs of score laws, each with `F ≤ G`, no atoms and the incumbent investing (`C = F`), give
opposite signs of `D(Q) = Δ(0, Q+1) − Δ(0, Q)` at `Q = 1` and at `Q = 2`. Both pairs are
built from uniform draws, so every integral reduces to `∫ u^k du = 1/(k+1)`. -/

section Uniform

instance unif_noAtoms : NoAtoms unif := ⟨fun x => by
  unfold unif
  rw [Measure.restrict_apply' measurableSet_Icc]
  exact measure_mono_null inter_subset_left (measure_singleton x)⟩

theorem cdf_unif_of_mem (x : ℝ) (h0 : 0 ≤ x) (h1 : x ≤ 1) : cdf unif x = x := by
  rw [cdf_eq_real, measureReal_def]
  unfold unif
  rw [Measure.restrict_apply' measurableSet_Icc]
  have e : Iic x ∩ Icc 0 1 = Icc 0 x := by
    ext y
    simp only [mem_inter_iff, mem_Iic, mem_Icc]
    constructor
    · rintro ⟨h, h', _⟩
      exact ⟨h', h⟩
    · rintro ⟨h', h⟩
      exact ⟨h, h', by linarith⟩
  rw [e, Real.volume_Icc, sub_zero, ENNReal.toReal_ofReal h0]

theorem cdf_unif_of_neg (x : ℝ) (h : x < 0) : cdf unif x = 0 := by
  rw [cdf_eq_real, measureReal_def]
  unfold unif
  rw [Measure.restrict_apply' measurableSet_Icc]
  have e : Iic x ∩ Icc 0 1 = ∅ := by
    ext y
    simp only [mem_inter_iff, mem_Iic, mem_Icc, mem_empty_iff_false, iff_false, not_and]
    intro h1 h2
    linarith
  rw [e, measure_empty]
  simp

theorem cdf_unif_of_ge (x : ℝ) (h : 1 ≤ x) : cdf unif x = 1 := by
  rw [cdf_eq_real, measureReal_def]
  unfold unif
  rw [Measure.restrict_apply' measurableSet_Icc]
  have e : Iic x ∩ Icc 0 1 = Icc 0 1 := by
    ext y
    simp only [mem_inter_iff, mem_Iic, mem_Icc]
    constructor
    · rintro ⟨_, h'⟩
      exact h'
    · rintro ⟨h0, h1⟩
      exact ⟨by linarith, h0, h1⟩
  rw [e, Real.volume_Icc]
  simp

theorem unif_ae_mem : ∀ᵐ x ∂unif, x ∈ Icc (0 : ℝ) 1 := by
  unfold unif
  exact ae_restrict_mem measurableSet_Icc

theorem integrable_upow (k : ℕ) : Integrable (fun x => cdf unif x ^ k) unif :=
  integrable_of_bounded unif _ ((cdf unif).mono.measurable.pow_const k) 1 (fun x => by
    rw [abs_of_nonneg (pow_nonneg (cdf_nonneg unif x) k)]
    exact pow_le_one₀ (cdf_nonneg unif x) (cdf_le_one unif x))

/-- `∫ (u^k − u^(k+1)) du = 1/(k+1) − 1/(k+2)`. -/
theorem integral_udiff (k : ℕ) :
    ∫ x, (cdf unif x ^ k - cdf unif x ^ (k + 1)) ∂unif = 1 / ((k : ℝ) + 1) - 1 / ((k : ℝ) + 2) := by
  rw [integral_sub (integrable_upow k) (integrable_upow (k + 1)), integral_cdf_pow unif k,
    integral_cdf_pow unif (k + 1)]
  push_cast
  ring

theorem integrable_udiff (k : ℕ) :
    Integrable (fun x => cdf unif x ^ k - cdf unif x ^ (k + 1)) unif :=
  (integrable_upow k).sub (integrable_upow (k + 1))

theorem weight_bound (β C : Measure ℝ) [IsProbabilityMeasure β] [IsProbabilityMeasure C]
    (Q : ℕ) (x : ℝ) : |weight β C Q x| ≤ 1 := by
  rw [abs_of_nonneg (weight_nonneg β C Q x)]
  exact weight_le_one β C Q x

end Uniform

section PairB

/-- Pair B. The investor's score is the larger of two uniform draws, `F = x²`, and the
    non-investor's is uniform, `G = x`. -/
noncomputable abbrev lawB : Measure ℝ := powLaw unif unif 1

theorem pairB_fosd (x : ℝ) : cdf lawB x ≤ cdf unif x := by
  rw [cdf_powLaw, pow_one]
  exact mul_le_of_le_one_right (cdf_nonneg unif x) (cdf_le_one unif x)

/-- **Pair B, every `Q`.** `D(Q) = −V Q / ((Q+2)(Q+3)(Q+4))`, negative for `V > 0`. -/
theorem pairB_step (V : ℝ) (n : ℕ) :
    stepQ V lawB unif lawB (n + 1)
      = -V * (((n : ℝ) + 1) / (((n : ℝ) + 3) * ((n : ℝ) + 4) * ((n : ℝ) + 5))) := by
  rw [pmu_identity V lawB unif lawB (n + 1) (by omega)]
  have hW : ∀ x, weight unif lawB (n + 1) x = cdf unif x ^ (n + 2) - cdf unif x ^ (n + 3) := by
    intro x
    unfold weight
    rw [cdf_powLaw, Nat.add_sub_cancel]
    ring
  have hA : ∫ x, weight unif lawB (n + 1) x ∂lawB
      = 2 * (1 / ((n : ℝ) + 4) - 1 / ((n : ℝ) + 5)) := by
    rw [integral_iid unif 1 _ (measurable_weight unif lawB (n + 1)) 1
      (weight_bound unif lawB (n + 1))]
    have e : ∀ x, weight unif lawB (n + 1) x * cdf unif x ^ 1
        = cdf unif x ^ (n + 3) - cdf unif x ^ (n + 3 + 1) := by
      intro x
      rw [hW]
      ring
    simp_rw [e]
    rw [integral_udiff (n + 3)]
    push_cast
    ring
  have hB : ∫ x, weight unif lawB (n + 1) x ∂unif = 1 / ((n : ℝ) + 3) - 1 / ((n : ℝ) + 4) := by
    have e : ∀ x, weight unif lawB (n + 1) x = cdf unif x ^ (n + 2) - cdf unif x ^ (n + 2 + 1) :=
      fun x => hW x
    simp_rw [e]
    rw [integral_udiff (n + 2)]
    push_cast
    ring
  rw [hA, hB]
  field_simp
  ring

theorem pairB_step_neg (V : ℝ) (hV : 0 < V) (n : ℕ) : stepQ V lawB unif lawB (n + 1) < 0 := by
  rw [pairB_step]
  have : 0 < ((n : ℝ) + 1) / (((n : ℝ) + 3) * ((n : ℝ) + 4) * ((n : ℝ) + 5)) := by positivity
  nlinarith

end PairB

section PairA

/-- The larger of two uniform draws, `x²` on `[0, 1]`. -/
noncomputable abbrev maxTwo : Measure ℝ := powLaw unif unif 1

/-- Pair A. The non-investor's score is the smaller of two uniform draws, the reflection of
    `maxTwo`, with `G = 1 − (1 − x)²`. The investor's score is uniform, `F = x`. -/
noncomputable def lawA : Measure ℝ := maxTwo.map (fun y => 1 - y)

theorem measurable_refl : Measurable (fun y : ℝ => 1 - y) := measurable_const.sub measurable_id

instance lawA_isProb : IsProbabilityMeasure lawA :=
  Measure.isProbabilityMeasure_map measurable_refl.aemeasurable

instance lawA_noAtoms : NoAtoms lawA := ⟨fun x => by
  unfold lawA
  rw [Measure.map_apply measurable_refl (measurableSet_singleton x)]
  have e : (fun y : ℝ => 1 - y) ⁻¹' {x} = {1 - x} := by
    ext y
    simp only [mem_preimage, mem_singleton_iff]
    constructor <;> intro h <;> linarith
  rw [e]
  exact measure_singleton _⟩

theorem cdf_lawA (x : ℝ) : cdf lawA x = 1 - cdf maxTwo (1 - x) := by
  have hpre : (fun y : ℝ => 1 - y) ⁻¹' Iic x = (Iio (1 - x))ᶜ := by
    ext y
    simp only [mem_preimage, mem_Iic, mem_compl_iff, mem_Iio, not_lt]
    constructor <;> intro h <;> linarith
  rw [cdf_eq_real, measureReal_def, lawA, Measure.map_apply measurable_refl measurableSet_Iic,
    hpre, prob_compl_eq_one_sub measurableSet_Iio, measure_congr (Iio_ae_eq_Iic (μ := maxTwo)),
    ENNReal.toReal_sub_of_le prob_le_one ENNReal.one_ne_top, ENNReal.toReal_one,
    cdf_eq_real, measureReal_def]

theorem cdf_maxTwo (y : ℝ) : cdf maxTwo y = cdf unif y * cdf unif y := by
  rw [cdf_powLaw, pow_one]

theorem pairA_fosd (x : ℝ) : cdf unif x ≤ cdf lawA x := by
  rw [cdf_lawA, cdf_maxTwo]
  rcases lt_or_ge x 0 with h | h
  · rw [cdf_unif_of_neg x h]
    have := cdf_le_one unif (1 - x)
    have := cdf_nonneg unif (1 - x)
    nlinarith
  rcases le_or_gt x 1 with h' | h'
  · rw [cdf_unif_of_mem x h h', cdf_unif_of_mem (1 - x) (by linarith) (by linarith)]
    nlinarith
  · rw [cdf_unif_of_neg (1 - x) (by linarith)]
    have := cdf_le_one unif x
    linarith

/-- On `[0, 1]`, the reflected law has `G(1 − y) = 1 − y²`. -/
theorem cdf_lawA_refl (y : ℝ) (h0 : 0 ≤ y) (h1 : y ≤ 1) : cdf lawA (1 - y) = 1 - y * y := by
  rw [cdf_lawA, show (1 : ℝ) - (1 - y) = y by ring, cdf_maxTwo, cdf_unif_of_mem y h0 h1]

theorem cdf_lawA_mem (x : ℝ) (h0 : 0 ≤ x) (h1 : x ≤ 1) :
    cdf lawA x = 1 - (1 - x) * (1 - x) := by
  rw [cdf_lawA, cdf_maxTwo, cdf_unif_of_mem (1 - x) (by linarith) (by linarith)]

theorem integral_lawA (g : ℝ → ℝ) (hg : Measurable g) (B : ℝ) (hB : ∀ x, |g x| ≤ B) :
    ∫ x, g x ∂lawA = 2 * ∫ y, g (1 - y) * cdf unif y ∂unif := by
  rw [lawA, integral_map measurable_refl.aemeasurable hg.aestronglyMeasurable]
  rw [integral_iid unif 1 (fun y => g (1 - y)) (hg.comp measurable_refl) B (fun y => hB _)]
  simp only [pow_one]
  push_cast
  ring

/-- **Pair A, `Q = 1`.** `D(1) = V/60`, positive for `V > 0`. -/
theorem pairA_step_one (V : ℝ) : stepQ V unif lawA unif 1 = V / 60 := by
  rw [pmu_identity V unif lawA unif 1 le_rfl]
  have hA : ∫ x, weight lawA unif 1 x ∂unif = 1 / 12 := by
    have e : ∀ᵐ x ∂unif, weight lawA unif 1 x
        = (cdf unif x ^ 1 - cdf unif x ^ (1 + 1)) - (cdf unif x ^ 2 - cdf unif x ^ (2 + 1)) := by
      refine unif_ae_mem.mono (fun x hx => ?_)
      unfold weight
      rw [cdf_lawA_mem x hx.1 hx.2, cdf_unif_of_mem x hx.1 hx.2]
      ring
    rw [integral_congr_ae e, integral_sub (integrable_udiff 1) (integrable_udiff 2),
      integral_udiff 1, integral_udiff 2]
    norm_num
  have hB : ∫ x, weight lawA unif 1 x ∂lawA = 1 / 10 := by
    rw [integral_lawA _ (measurable_weight lawA unif 1) 1 (weight_bound lawA unif 1)]
    have e : ∀ᵐ y ∂unif, weight lawA unif 1 (1 - y) * cdf unif y
        = cdf unif y ^ 3 - cdf unif y ^ (3 + 1) := by
      refine unif_ae_mem.mono (fun y hy => ?_)
      unfold weight
      rw [cdf_lawA_refl y hy.1 hy.2, cdf_unif_of_mem (1 - y) (by linarith [hy.2])
        (by linarith [hy.1]), cdf_unif_of_mem y hy.1 hy.2]
      ring
    rw [integral_congr_ae e, integral_udiff 3]
    norm_num
  rw [hA, hB]
  ring

/-- **Pair A, `Q = 2`.** `D(2) = V/420`, positive for `V > 0`. -/
theorem pairA_step_two (V : ℝ) : stepQ V unif lawA unif 2 = V / 420 := by
  rw [pmu_identity V unif lawA unif 2 (by norm_num)]
  have hA : ∫ x, weight lawA unif 2 x ∂unif = 1 / 20 := by
    have e : ∀ᵐ x ∂unif, weight lawA unif 2 x
        = (2 * (cdf unif x ^ 2 - cdf unif x ^ (2 + 1))
            - 3 * (cdf unif x ^ 3 - cdf unif x ^ (3 + 1)))
          + (cdf unif x ^ 4 - cdf unif x ^ (4 + 1)) := by
      refine unif_ae_mem.mono (fun x hx => ?_)
      unfold weight
      rw [cdf_lawA_mem x hx.1 hx.2, cdf_unif_of_mem x hx.1 hx.2]
      ring
    have i2 : Integrable (fun x => 2 * (cdf unif x ^ 2 - cdf unif x ^ (2 + 1))) unif :=
      (integrable_udiff 2).const_mul 2
    have i3 : Integrable (fun x => 3 * (cdf unif x ^ 3 - cdf unif x ^ (3 + 1))) unif :=
      (integrable_udiff 3).const_mul 3
    have i23 : Integrable (fun x => 2 * (cdf unif x ^ 2 - cdf unif x ^ (2 + 1))
        - 3 * (cdf unif x ^ 3 - cdf unif x ^ (3 + 1))) unif := i2.sub i3
    rw [integral_congr_ae e, integral_add i23 (integrable_udiff 4), integral_sub i2 i3,
      integral_const_mul, integral_const_mul, integral_udiff 2, integral_udiff 3,
      integral_udiff 4]
    norm_num
  have hB : ∫ x, weight lawA unif 2 x ∂lawA = 11 / 210 := by
    rw [integral_lawA _ (measurable_weight lawA unif 2) 1 (weight_bound lawA unif 2)]
    have e : ∀ᵐ y ∂unif, weight lawA unif 2 (1 - y) * cdf unif y
        = (cdf unif y ^ 3 - cdf unif y ^ (3 + 1)) - (cdf unif y ^ 5 - cdf unif y ^ (5 + 1)) := by
      refine unif_ae_mem.mono (fun y hy => ?_)
      unfold weight
      rw [cdf_lawA_refl y hy.1 hy.2, cdf_unif_of_mem (1 - y) (by linarith [hy.2])
        (by linarith [hy.1]), cdf_unif_of_mem y hy.1 hy.2]
      ring
    rw [integral_congr_ae e, integral_sub (integrable_udiff 3) (integrable_udiff 5),
      integral_udiff 3, integral_udiff 5]
    norm_num
  rw [hA, hB]
  ring

end PairA

/-- **First-order stochastic dominance does not sign `D(Q)`.** Both pairs satisfy `F ≤ G`, have
    no atoms and let the incumbent invest. At `Q = 1` and `Q = 2`, pair A gives `D(Q) > 0` and
    pair B gives `D(Q) < 0`. -/
theorem fosd_does_not_sign (V : ℝ) (hV : 0 < V) :
    (∀ x, cdf unif x ≤ cdf lawA x) ∧ (∀ x, cdf lawB x ≤ cdf unif x)
      ∧ 0 < stepQ V unif lawA unif 1 ∧ stepQ V lawB unif lawB 1 < 0
      ∧ 0 < stepQ V unif lawA unif 2 ∧ stepQ V lawB unif lawB 2 < 0 :=
  ⟨pairA_fosd, pairB_fosd,
    by rw [pairA_step_one]; positivity, pairB_step_neg V hV 0,
    by rw [pairA_step_two]; positivity, pairB_step_neg V hV 1⟩

end EntryContestPMUWitness
