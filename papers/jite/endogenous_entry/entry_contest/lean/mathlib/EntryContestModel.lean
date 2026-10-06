import Mathlib
import StepIdentity
import EntryContest

open MeasureTheory ProbabilityTheory Set

set_option linter.unusedSectionVars false

namespace EntryContestModel

noncomputable def maxLaw (μ ν : Measure ℝ) : Measure ℝ :=
  (μ.prod ν).map (fun p : ℝ × ℝ => max p.1 p.2)

theorem measurable_max2 : Measurable (fun p : ℝ × ℝ => max p.1 p.2) :=
  measurable_fst.max measurable_snd

instance maxLaw_isProb (μ ν : Measure ℝ) [IsProbabilityMeasure μ] [IsProbabilityMeasure ν] :
    IsProbabilityMeasure (maxLaw μ ν) :=
  Measure.isProbabilityMeasure_map measurable_max2.aemeasurable

theorem maxLaw_Iic (μ ν : Measure ℝ) [IsProbabilityMeasure ν] (x : ℝ) :
    maxLaw μ ν (Iic x) = μ (Iic x) * ν (Iic x) := by
  rw [maxLaw, Measure.map_apply measurable_max2 measurableSet_Iic]
  have h : (fun p : ℝ × ℝ => max p.1 p.2) ⁻¹' Iic x = Iic x ×ˢ Iic x := by
    ext ⟨a, b⟩; simp
  rw [h, Measure.prod_prod]

instance maxLaw_noAtoms (μ ν : Measure ℝ) [IsProbabilityMeasure μ] [IsProbabilityMeasure ν]
    [NoAtoms μ] [NoAtoms ν] : NoAtoms (maxLaw μ ν) := by
  constructor
  intro x
  rw [maxLaw, Measure.map_apply measurable_max2 (measurableSet_singleton x)]
  refine measure_mono_null (t := ({x} ×ˢ univ) ∪ (univ ×ˢ {x})) ?_ ?_
  · rintro ⟨a, b⟩ h
    simp only [mem_preimage, mem_singleton_iff] at h
    rcases le_total a b with hab | hba
    · rw [max_eq_right hab] at h
      exact Or.inr ⟨mem_univ _, h⟩
    · rw [max_eq_left hba] at h
      exact Or.inl ⟨h, mem_univ _⟩
  · refine measure_union_null ?_ ?_
    · rw [Measure.prod_prod, measure_singleton, zero_mul]
    · rw [Measure.prod_prod, measure_singleton, mul_zero]

theorem cdf_maxLaw (μ ν : Measure ℝ) [IsProbabilityMeasure μ] [IsProbabilityMeasure ν] (x : ℝ) :
    cdf (maxLaw μ ν) x = cdf μ x * cdf ν x := by
  simp only [cdf_eq_real, measureReal_def, maxLaw_Iic, ENNReal.toReal_mul]

noncomputable def powLaw (base μ : Measure ℝ) : ℕ → Measure ℝ
  | 0 => base
  | n + 1 => maxLaw (powLaw base μ n) μ

instance powLaw_isProb (base μ : Measure ℝ) [IsProbabilityMeasure base] [IsProbabilityMeasure μ] :
    ∀ n, IsProbabilityMeasure (powLaw base μ n)
  | 0 => by simp only [powLaw]; infer_instance
  | n + 1 => by
      haveI := powLaw_isProb base μ n
      simp only [powLaw]; infer_instance

instance powLaw_noAtoms (base μ : Measure ℝ) [IsProbabilityMeasure base] [IsProbabilityMeasure μ]
    [NoAtoms base] [NoAtoms μ] : ∀ n, NoAtoms (powLaw base μ n)
  | 0 => by simp only [powLaw]; infer_instance
  | n + 1 => by
      haveI := powLaw_noAtoms base μ n
      simp only [powLaw]; infer_instance

theorem cdf_powLaw (base μ : Measure ℝ) [IsProbabilityMeasure base] [IsProbabilityMeasure μ] :
    ∀ (n : ℕ) (x : ℝ), cdf (powLaw base μ n) x = cdf base x * cdf μ x ^ n
  | 0, x => by simp [powLaw]
  | n + 1, x => by
      simp only [powLaw]
      rw [cdf_maxLaw, cdf_powLaw base μ n x, pow_succ]
      ring

noncomputable def rivals (C α β : Measure ℝ) (a b : ℕ) : Measure ℝ :=
  powLaw (powLaw C α a) β b

instance rivals_isProb (C α β : Measure ℝ) [IsProbabilityMeasure C] [IsProbabilityMeasure α]
    [IsProbabilityMeasure β] (a b : ℕ) : IsProbabilityMeasure (rivals C α β a b) := by
  unfold rivals; infer_instance

instance rivals_noAtoms (C α β : Measure ℝ) [IsProbabilityMeasure C] [IsProbabilityMeasure α]
    [IsProbabilityMeasure β] [NoAtoms C] [NoAtoms α] [NoAtoms β] (a b : ℕ) :
    NoAtoms (rivals C α β a b) := by
  unfold rivals; infer_instance

theorem cdf_rivals (C α β : Measure ℝ) [IsProbabilityMeasure C] [IsProbabilityMeasure α]
    [IsProbabilityMeasure β] (a b : ℕ) (x : ℝ) :
    cdf (rivals C α β a b) x = cdf C x * cdf α x ^ a * cdf β x ^ b := by
  rw [rivals, cdf_powLaw, cdf_powLaw]

noncomputable def Delta (V : ℝ) (α β C : Measure ℝ) (Q m : ℕ) : ℝ :=
  V * ((∫ x, cdf (rivals C α β m (Q - 1 - m)) x ∂α)
    - (∫ x, cdf (rivals C α β m (Q - 1 - m)) x ∂β))

variable (V : ℝ) (α β C : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]
  [IsProbabilityMeasure C] [NoAtoms α] [NoAtoms β] [NoAtoms C]

theorem Delta_step (Q m : ℕ) (hm : m + 2 ≤ Q) :
    Delta V α β C Q (m + 1) - Delta V α β C Q m
      = -(V / 2) * ∫ x, (cdf α x - cdf β x) ^ 2 ∂(rivals C α β m (Q - 2 - m)) := by
  have h1 : ∀ x, cdf (rivals C α β (m + 1) (Q - 1 - (m + 1))) x
      = cdf (rivals C α β m (Q - 2 - m)) x * cdf α x := by
    intro x
    rw [cdf_rivals, cdf_rivals, show Q - 1 - (m + 1) = Q - 2 - m by omega, pow_succ]
    ring
  have h2 : ∀ x, cdf (rivals C α β m (Q - 1 - m)) x
      = cdf (rivals C α β m (Q - 2 - m)) x * cdf β x := by
    intro x
    rw [cdf_rivals, cdf_rivals, show Q - 1 - m = (Q - 2 - m) + 1 by omega, pow_succ]
    ring
  unfold Delta
  simp_rw [h1, h2]
  have hs := EntryContestStep.step_identity α β (rivals C α β m (Q - 2 - m))
  rw [← mul_sub, hs]
  ring

theorem Delta_step_nonpos (hV : 0 ≤ V) (Q m : ℕ) (hm : m + 2 ≤ Q) :
    Delta V α β C Q (m + 1) ≤ Delta V α β C Q m := by
  have h := Delta_step V α β C Q m hm
  have hI : 0 ≤ ∫ x, (cdf α x - cdf β x) ^ 2 ∂(rivals C α β m (Q - 2 - m)) :=
    integral_nonneg (fun x => sq_nonneg _)
  nlinarith

noncomputable def DeltaUpTo (Q n : ℕ) : ℝ := Delta V α β C Q (min n (Q - 1))

theorem DeltaUpTo_step (hV : 0 ≤ V) (Q n : ℕ) :
    DeltaUpTo V α β C Q (n + 1) ≤ DeltaUpTo V α β C Q n := by
  unfold DeltaUpTo
  by_cases hn : n + 1 ≤ Q - 1
  · rw [min_eq_left hn, min_eq_left (by omega)]
    exact Delta_step_nonpos V α β C hV Q n (by omega)
  · rw [min_eq_right (by omega), min_eq_right (by omega)]

theorem p5_threshold_from_primitives (hV : 0 ≤ V) (Q : ℕ) (kappa : ℝ) :
    ∀ i j, i ≤ j → kappa ≤ DeltaUpTo V α β C Q j → kappa ≤ DeltaUpTo V α β C Q i :=
  EntryContest.equilibrium_is_threshold (fun a b : ℝ => a ≤ b) le_refl
    (fun _ _ _ => le_trans) (DeltaUpTo V α β C Q) kappa (DeltaUpTo_step V α β C hV Q)

end EntryContestModel
