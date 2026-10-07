import Mathlib
import FallWitness

open MeasureTheory ProbabilityTheory Set Filter Topology
open scoped ENNReal

set_option linter.unusedSectionVars false

namespace EntryContestShift

open EntryContestModel EntryContestP1P2 EntryContestP6P7 EntryContestPMU EntryContestPMUWitness
  EntryContestRefute EntryContestFall

/-! **The rise-then-fall shape for a class of primitives.** When the investment lifts the score
by a single step and the base law never gains mass from a shift up, `dF − dG` changes sign once,
at the lift, so the first entrant's gain is quasi-concave in `Q`. With a density this says that
the density of the base score does not rise. The witness of `FallWitness.lean` lies in the
class. -/

section Class

/-- A base law is shift-monotone when moving a set above `c` down by `c` never loses mass. -/
def ShiftMono (ρ : Measure ℝ) : Prop :=
  ∀ c : ℝ, 0 < c → ∀ B : Set ℝ, MeasurableSet B → B ⊆ Ioi c →
    ρ B ≤ ρ ((fun r => r + c) ⁻¹' B)

variable (p μ : ℝ) [hp : Fact (0 ≤ p ∧ p ≤ 1)] (ρr : Measure ℝ) [IsProbabilityMeasure ρr]

/-- Below the lift the investor's law is `p` times the non-investor's. -/
theorem left_of_lift (hμ : 0 < μ) (h0 : ρr (Iic 0) = 0) :
    (investorLaw μ ρr (bern p)).restrict (Iic (1 - μ))
      ≤ (nonInvestorLaw μ ρr).restrict (Iic (1 - μ)) := by
  rw [Measure.le_iff]
  intro A hA
  have hB : MeasurableSet (A ∩ Iic (1 - μ)) := hA.inter measurableSet_Iic
  rw [Measure.restrict_apply hA, Measure.restrict_apply hA, investorLaw_bern p μ ρr,
    Measure.add_apply, Measure.smul_apply, Measure.smul_apply, smul_eq_mul, smul_eq_mul]
  have hshift : (ρr.map (fun r => μ * r + (1 - μ))) (A ∩ Iic (1 - μ)) = 0 := by
    rw [Measure.map_apply (by fun_prop) hB]
    refine measure_mono_null (fun r hr => ?_) h0
    simp only [mem_preimage, mem_inter_iff, mem_Iic] at hr
    show r ≤ 0
    nlinarith [hr.2]
  rw [hshift, mul_zero, add_zero]
  unfold nonInvestorLaw
  calc ENNReal.ofReal p * (ρr.map (fun r => μ * r)) (A ∩ Iic (1 - μ))
      ≤ 1 * (ρr.map (fun r => μ * r)) (A ∩ Iic (1 - μ)) :=
        mul_le_mul_right' (ENNReal.ofReal_le_one.mpr hp.out.2) _
    _ = (ρr.map (fun r => μ * r)) (A ∩ Iic (1 - μ)) := one_mul _

/-- Above the lift the investor's law dominates the non-investor's, when the base law is
    shift-monotone. -/
theorem right_of_lift (hμ : 0 < μ) (hμ1 : μ < 1) (hsm : ShiftMono ρr) :
    (nonInvestorLaw μ ρr).restrict (Ioi (1 - μ))
      ≤ (investorLaw μ ρr (bern p)).restrict (Ioi (1 - μ)) := by
  obtain ⟨hp0, hp1⟩ := hp.out
  rw [Measure.le_iff]
  intro A hA
  have hB : MeasurableSet (A ∩ Ioi (1 - μ)) := hA.inter measurableSet_Ioi
  rw [Measure.restrict_apply hA, Measure.restrict_apply hA, investorLaw_bern p μ ρr,
    Measure.add_apply, Measure.smul_apply, Measure.smul_apply, smul_eq_mul, smul_eq_mul]
  unfold nonInvestorLaw
  have hc : 0 < (1 - μ) / μ := div_pos (by linarith) hμ
  have key : (ρr.map (fun r => μ * r)) (A ∩ Ioi (1 - μ))
      ≤ (ρr.map (fun r => μ * r + (1 - μ))) (A ∩ Ioi (1 - μ)) := by
    rw [Measure.map_apply (by fun_prop) hB, Measure.map_apply (by fun_prop) hB]
    have hBm : MeasurableSet ((fun r => μ * r) ⁻¹' (A ∩ Ioi (1 - μ))) :=
      (by fun_prop : Measurable (fun r : ℝ => μ * r)) hB
    have hBsub : (fun r => μ * r) ⁻¹' (A ∩ Ioi (1 - μ)) ⊆ Ioi ((1 - μ) / μ) := by
      intro r hr
      simp only [mem_preimage, mem_inter_iff, mem_Ioi] at hr
      show (1 - μ) / μ < r
      rw [div_lt_iff₀ hμ]
      linarith [hr.2]
    have e : (fun r => μ * r + (1 - μ)) ⁻¹' (A ∩ Ioi (1 - μ))
        = (fun r => r + (1 - μ) / μ) ⁻¹' ((fun r => μ * r) ⁻¹' (A ∩ Ioi (1 - μ))) := by
      ext r
      simp only [mem_preimage]
      rw [mul_add, mul_div_cancel₀ _ hμ.ne']
    rw [e]
    exact hsm _ hc _ hBm hBsub
  calc (ρr.map (fun r => μ * r)) (A ∩ Ioi (1 - μ))
      = (ENNReal.ofReal p + ENNReal.ofReal (1 - p))
          * (ρr.map (fun r => μ * r)) (A ∩ Ioi (1 - μ)) := by
        rw [← ENNReal.ofReal_add hp0 (by linarith), show p + (1 - p) = 1 by ring,
          ENNReal.ofReal_one, one_mul]
    _ = ENNReal.ofReal p * (ρr.map (fun r => μ * r)) (A ∩ Ioi (1 - μ))
          + ENNReal.ofReal (1 - p) * (ρr.map (fun r => μ * r)) (A ∩ Ioi (1 - μ)) :=
        add_mul _ _ _
    _ ≤ ENNReal.ofReal p * (ρr.map (fun r => μ * r)) (A ∩ Ioi (1 - μ))
          + ENNReal.ofReal (1 - p) * (ρr.map (fun r => μ * r + (1 - μ))) (A ∩ Ioi (1 - μ)) :=
        add_le_add_left (mul_le_mul_left' key _) _

/-- **Quasi-concavity for the class.** For any base law with no mass at or below 0 that is
    shift-monotone, any `0 < μ < 1`, any failure probability and any incumbent, the first
    entrant's gain rises and then falls in `Q`. -/
theorem quasiconcave_of_shiftMono (V : ℝ) (hV : 0 ≤ V) (hμ : 0 < μ) (hμ1 : μ < 1)
    (h0 : ρr (Iic 0) = 0) (hsm : ShiftMono ρr) (C : Measure ℝ) [IsProbabilityMeasure C]
    (Q1 Q2 Q3 : ℕ) (h1 : 1 ≤ Q1) (h12 : Q1 ≤ Q2) (h23 : Q2 ≤ Q3) :
    min (Delta V (investorLaw μ ρr (bern p)) (nonInvestorLaw μ ρr) C Q1 0)
        (Delta V (investorLaw μ ρr (bern p)) (nonInvestorLaw μ ρr) C Q3 0)
      ≤ Delta V (investorLaw μ ρr (bern p)) (nonInvestorLaw μ ρr) C Q2 0 :=
  pmu_quasiconcave V _ _ C hV (1 - μ) (left_of_lift p μ ρr hμ h0)
    (right_of_lift p μ ρr hμ hμ1 hsm) Q1 Q2 Q3 h1 h12 h23

end Class

section Density

/-- A law with a density that does not rise on the positive reals is shift-monotone. -/
theorem shiftMono_of_density (g : ℝ → ℝ)
    (hmono : ∀ x y, 0 < x → x ≤ y → g y ≤ g x) :
    ShiftMono (volume.withDensity (fun r => ENNReal.ofReal (g r))) := by
  intro c hc B hB hBsub
  have hB' : MeasurableSet ((fun r => r + c) ⁻¹' B) := (measurable_add_const c) hB
  rw [withDensity_apply _ hB, withDensity_apply _ hB', ← lintegral_indicator hB,
    ← lintegral_indicator hB']
  have e : ∀ r, ((fun r => r + c) ⁻¹' B).indicator (fun r => ENNReal.ofReal (g r)) r
      = B.indicator (fun s => ENNReal.ofReal (g (s - c))) (r + c) := by
    intro r
    by_cases h : r + c ∈ B
    · rw [indicator_of_mem (show r ∈ (fun r => r + c) ⁻¹' B from h), indicator_of_mem h,
        add_sub_cancel_right]
    · rw [indicator_of_notMem (show r ∉ (fun r => r + c) ⁻¹' B from h), indicator_of_notMem h]
  calc ∫⁻ r, B.indicator (fun r => ENNReal.ofReal (g r)) r
      ≤ ∫⁻ r, B.indicator (fun s => ENNReal.ofReal (g (s - c))) r := by
        refine lintegral_mono (fun r => ?_)
        by_cases hr : r ∈ B
        · rw [indicator_of_mem hr, indicator_of_mem hr]
          have hrc : c < r := hBsub hr
          exact ENNReal.ofReal_le_ofReal (hmono (r - c) r (by linarith) (by linarith))
        · rw [indicator_of_notMem hr, indicator_of_notMem hr]
    _ = ∫⁻ r, B.indicator (fun s => ENNReal.ofReal (g (s - c))) (r + c) :=
        (lintegral_add_right_eq_self _ c).symm
    _ = ∫⁻ r, ((fun r => r + c) ⁻¹' B).indicator (fun r => ENNReal.ofReal (g r)) r := by
        simp_rw [e]

/-- The density of the witness's base law, `2(1 − r)` on `[0, 1]`. -/
noncomputable def dens (r : ℝ) : ℝ := (Icc (0 : ℝ) 1).indicator (fun r => 2 * (1 - r)) r

theorem dens_nonneg (r : ℝ) : 0 ≤ dens r := by
  unfold dens
  rw [indicator_apply]
  split_ifs with h
  · linarith [h.2]
  · exact le_rfl

theorem dens_measurable : Measurable dens :=
  (by fun_prop : Measurable (fun r : ℝ => 2 * (1 - r))).indicator measurableSet_Icc

theorem dens_integrable : Integrable dens volume :=
  ((by fun_prop : Continuous (fun r : ℝ => 2 * (1 - r))).integrableOn_Icc).integrable_indicator
    measurableSet_Icc

theorem dens_anti (x y : ℝ) (hx : 0 < x) (hxy : x ≤ y) : dens y ≤ dens x := by
  unfold dens
  rw [indicator_apply, indicator_apply]
  split_ifs with hy hx' hx'
  · linarith
  · exact absurd ⟨hx.le, by linarith [hy.2]⟩ hx'
  · linarith [hx'.2]
  · exact le_rfl

/-- The witness's base law written through its density. -/
noncomputable def densLaw : Measure ℝ := volume.withDensity (fun r => ENNReal.ofReal (dens r))

instance densLaw_finite : IsFiniteMeasure densLaw :=
  isFiniteMeasure_withDensity_ofReal dens_integrable.2

theorem densLaw_Iic (x : ℝ) : densLaw (Iic x) = ENNReal.ofReal (∫ r in Iic x, dens r) := by
  unfold densLaw
  rw [withDensity_apply _ measurableSet_Iic,
    ofReal_integral_eq_lintegral_ofReal dens_integrable.integrableOn (ae_of_all _ dens_nonneg)]

theorem poly_two (a b : ℝ) : ∫ y in a..b, 2 * (1 - y) = (2 * b - b ^ 2) - (2 * a - a ^ 2) := by
  have e : EqOn (fun y : ℝ => 2 * (1 - y))
      (fun y => 2 + (-2) * y + 0 * y ^ 2 + 0 * y ^ 3 + 0 * y ^ 4 + 0 * y ^ 5 + 0 * y ^ 6
        + 0 * y ^ 7) (uIcc a b) := by
    intro y _
    dsimp only
    ring
  rw [intervalIntegral.integral_congr e, integral_poly8]
  ring

theorem integral_dens_Iic (x : ℝ) : ∫ r in Iic x, dens r = cdf lawA x := by
  unfold dens
  rw [integral_indicator measurableSet_Icc, Measure.restrict_restrict measurableSet_Icc]
  rcases le_or_gt x 0 with hx | hx
  · have hnull : volume (Icc (0 : ℝ) 1 ∩ Iic x) = 0 := by
      refine measure_mono_null (fun r hr => ?_) (Real.volume_singleton (a := (0 : ℝ)))
      simp only [mem_inter_iff, mem_Icc, mem_Iic] at hr
      show r = 0
      linarith [hr.1.1, hr.2]
    rw [Measure.restrict_eq_zero.mpr hnull, integral_zero_measure, cdf_lawA_of_nonpos x hx]
  rcases le_or_gt x 1 with hx1 | hx1
  · have e : Icc (0 : ℝ) 1 ∩ Iic x = Icc 0 x := by
      ext r
      simp only [mem_inter_iff, mem_Icc, mem_Iic]
      constructor
      · rintro ⟨⟨h1, _⟩, h3⟩
        exact ⟨h1, h3⟩
      · rintro ⟨h1, h3⟩
        exact ⟨⟨h1, by linarith⟩, h3⟩
    rw [e, integral_Icc_eq_integral_Ioc, ← intervalIntegral.integral_of_le hx.le, poly_two,
      cdf_lawA_mem x hx.le hx1]
    ring
  · have e : Icc (0 : ℝ) 1 ∩ Iic x = Icc 0 1 := by
      ext r
      simp only [mem_inter_iff, mem_Icc, mem_Iic]
      constructor
      · rintro ⟨h, _⟩
        exact h
      · rintro ⟨h1, h2⟩
        exact ⟨⟨h1, h2⟩, by linarith⟩
    rw [e, integral_Icc_eq_integral_Ioc, ← intervalIntegral.integral_of_le zero_le_one, poly_two,
      cdf_lawA_of_ge x hx1.le]
    norm_num

theorem lawA_eq_densLaw : lawA = densLaw := by
  refine Measure.ext_of_Iic lawA densLaw (fun x => ?_)
  rw [densLaw_Iic, integral_dens_Iic, cdf_eq_real, measureReal_def,
    ENNReal.ofReal_toReal (measure_ne_top _ _)]

theorem lawA_shiftMono : ShiftMono lawA := by
  rw [lawA_eq_densLaw]
  exact shiftMono_of_density dens dens_anti

theorem lawA_Iic_zero : lawA (Iic 0) = 0 := by
  have h := cdf_lawA_of_nonpos 0 le_rfl
  rw [cdf_eq_real, measureReal_def] at h
  exact ((ENNReal.toReal_eq_zero_iff _).mp h).resolve_right (measure_ne_top _ _)

/-- **The witness lies in the class.** Its gain is quasi-concave in `Q` for every failure
    probability, with the fall exhibited at `p = 1/2` by `rise_then_fall`. -/
theorem witness_quasiconcave (V : ℝ) (hV : 0 ≤ V) (p : ℝ) [Fact (0 ≤ p ∧ p ≤ 1)]
    (Q1 Q2 Q3 : ℕ) (h1 : 1 ≤ Q1) (h12 : Q1 ≤ Q2) (h23 : Q2 ≤ Q3) :
    min (Delta V (αp p) β0 (αp p) Q1 0) (Delta V (αp p) β0 (αp p) Q3 0)
      ≤ Delta V (αp p) β0 (αp p) Q2 0 :=
  quasiconcave_of_shiftMono p (3 / 4) lawA V hV (by norm_num) (by norm_num) lawA_Iic_zero
    lawA_shiftMono (αp p) Q1 Q2 Q3 h1 h12 h23

end Density

end EntryContestShift
