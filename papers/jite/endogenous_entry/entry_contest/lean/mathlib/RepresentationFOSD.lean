import Mathlib
import StepIdentity
import EntryContestModel

open MeasureTheory ProbabilityTheory Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestP1P2

theorem integral_cdf (μ ρ : Measure ℝ) [IsProbabilityMeasure μ] [IsProbabilityMeasure ρ] :
    ∫ x, cdf μ x ∂ρ = (∫⁻ x, μ (Iic x) ∂ρ).toReal := by
  simp_rw [cdf_eq_real, measureReal_def]
  rw [integral_toReal]
  · exact (EntryContestStep.measurable_Iic μ).aemeasurable
  · exact ae_of_all _ (fun x => measure_lt_top _ _)

theorem integrable_cdf (μ ρ : Measure ℝ) [IsProbabilityMeasure μ] [IsProbabilityMeasure ρ] :
    Integrable (fun x => cdf μ x) ρ := by
  refine (integrable_const (1 : ℝ)).mono' (cdf μ).mono.measurable.aestronglyMeasurable
    (ae_of_all _ (fun x => ?_))
  rw [Real.norm_eq_abs, abs_of_nonneg (cdf_nonneg μ x)]
  exact cdf_le_one μ x

theorem two_max_sum (A B : Measure ℝ) [IsProbabilityMeasure A] [IsProbabilityMeasure B]
    [NoAtoms B] : (∫ x, cdf B x ∂A) + (∫ x, cdf A x ∂B) = 1 := by
  set P := A.prod B
  have hS1m : MeasurableSet {p : ℝ × ℝ | p.2 ≤ p.1} := measurableSet_le measurable_snd measurable_fst
  have hS2m : MeasurableSet {p : ℝ × ℝ | p.1 ≤ p.2} := measurableSet_le measurable_fst measurable_snd
  have h1 : P {p : ℝ × ℝ | p.2 ≤ p.1} = ∫⁻ x, B (Iic x) ∂A := by
    rw [Measure.prod_apply hS1m]
    refine lintegral_congr (fun x => ?_)
    congr 1
  have h2 : P {p : ℝ × ℝ | p.1 ≤ p.2} = ∫⁻ y, A (Iic y) ∂B := by
    rw [Measure.prod_apply_symm hS2m]
    refine lintegral_congr (fun y => ?_)
    congr 1
  have hinter : P ({p : ℝ × ℝ | p.2 ≤ p.1} ∩ {p : ℝ × ℝ | p.1 ≤ p.2}) = 0 := by
    refine measure_mono_null (t := {q : ℝ × ℝ | q.1 = q.2}) ?_ (EntryContestStep.diag_null A B)
    rintro ⟨x, y⟩ ⟨hyx, hxy⟩
    exact le_antisymm hxy hyx
  have hunion : {p : ℝ × ℝ | p.2 ≤ p.1} ∪ {p : ℝ × ℝ | p.1 ≤ p.2} = univ := by
    ext ⟨x, y⟩; simp [le_total]
  have hsum := measure_union_add_inter (μ := P) {p : ℝ × ℝ | p.2 ≤ p.1} hS2m
  rw [hunion, measure_univ, hinter, add_zero, h1, h2] at hsum
  rw [integral_cdf, integral_cdf]
  have hX : (∫⁻ x, B (Iic x) ∂A) ≠ ⊤ := by intro h; rw [h] at hsum; simp at hsum
  have hY : (∫⁻ y, A (Iic y) ∂B) ≠ ⊤ := by intro h; rw [h] at hsum; simp at hsum
  rw [← ENNReal.toReal_add hX hY, ← hsum]
  simp

theorem p1_representation (α β η : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]
    [IsProbabilityMeasure η] [NoAtoms η] :
    (∫ x, cdf η x ∂α) - (∫ x, cdf η x ∂β) = ∫ x, (cdf β x - cdf α x) ∂η := by
  have ha := two_max_sum α η
  have hb := two_max_sum β η
  rw [integral_sub (integrable_cdf β η) (integrable_cdf α η)]
  linarith

variable (V : ℝ) (α β C : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]
  [IsProbabilityMeasure C] [NoAtoms α] [NoAtoms β] [NoAtoms C]

theorem Delta_eq_expectation (Q m : ℕ) :
    EntryContestModel.Delta V α β C Q m
      = V * ∫ x, (cdf β x - cdf α x) ∂(EntryContestModel.rivals C α β m (Q - 1 - m)) := by
  unfold EntryContestModel.Delta
  rw [p1_representation]

theorem Delta_nonneg (hV : 0 ≤ V) (hfosd : ∀ x, cdf α x ≤ cdf β x) (Q m : ℕ) :
    0 ≤ EntryContestModel.Delta V α β C Q m := by
  rw [Delta_eq_expectation]
  exact mul_nonneg hV (integral_nonneg (fun x => sub_nonneg.mpr (hfosd x)))

noncomputable def investorLaw (μ : ℝ) (ρr ρs : Measure ℝ) : Measure ℝ :=
  (ρr.prod ρs).map (fun p : ℝ × ℝ => μ * p.1 + (1 - μ) * p.2)

noncomputable def nonInvestorLaw (μ : ℝ) (ρr : Measure ℝ) : Measure ℝ :=
  ρr.map (fun r : ℝ => μ * r)

theorem measurable_investorScore (μ : ℝ) :
    Measurable (fun p : ℝ × ℝ => μ * p.1 + (1 - μ) * p.2) := by fun_prop

theorem measurable_nonInvestorScore (μ : ℝ) : Measurable (fun r : ℝ => μ * r) := by fun_prop

instance investorLaw_isProb (μ : ℝ) (ρr ρs : Measure ℝ) [IsProbabilityMeasure ρr]
    [IsProbabilityMeasure ρs] : IsProbabilityMeasure (investorLaw μ ρr ρs) :=
  Measure.isProbabilityMeasure_map (measurable_investorScore μ).aemeasurable

instance nonInvestorLaw_isProb (μ : ℝ) (ρr : Measure ℝ) [IsProbabilityMeasure ρr] :
    IsProbabilityMeasure (nonInvestorLaw μ ρr) :=
  Measure.isProbabilityMeasure_map (measurable_nonInvestorScore μ).aemeasurable

variable (μ : ℝ) (ρr ρs : Measure ℝ) [IsProbabilityMeasure ρr] [IsProbabilityMeasure ρs]

theorem investorLaw_Iic (x : ℝ) :
    investorLaw μ ρr ρs (Iic x) = (ρr.prod ρs) {p | μ * p.1 + (1 - μ) * p.2 ≤ x} := by
  rw [investorLaw, Measure.map_apply (measurable_investorScore μ) measurableSet_Iic]
  rfl

theorem nonInvestorLaw_Iic (x : ℝ) :
    nonInvestorLaw μ ρr (Iic x) = (ρr.prod ρs) ({r | μ * r ≤ x} ×ˢ univ) := by
  rw [nonInvestorLaw, Measure.map_apply (measurable_nonInvestorScore μ) measurableSet_Iic,
    Measure.prod_prod, measure_univ, mul_one]
  rfl

theorem p2_fosd (hμ1 : μ ≤ 1) (hs : ρs (Iio 0) = 0) (x : ℝ) :
    cdf (investorLaw μ ρr ρs) x ≤ cdf (nonInvestorLaw μ ρr) x := by
  rw [cdf_eq_real, cdf_eq_real, measureReal_def, measureReal_def]
  refine ENNReal.toReal_mono (measure_ne_top _ _) ?_
  rw [investorLaw_Iic, nonInvestorLaw_Iic μ ρr ρs]
  have hsub : {p : ℝ × ℝ | μ * p.1 + (1 - μ) * p.2 ≤ x}
      ⊆ ({r | μ * r ≤ x} ×ˢ univ) ∪ (univ ×ˢ Iio 0) := by
    rintro ⟨r, s⟩ h
    simp only [mem_setOf_eq] at h
    by_cases hs0 : s < 0
    · exact Or.inr ⟨mem_univ _, hs0⟩
    · have : 0 ≤ (1 - μ) * s := mul_nonneg (by linarith) (not_lt.mp hs0)
      exact Or.inl ⟨by simp only [mem_setOf_eq]; linarith, mem_univ _⟩
  calc (ρr.prod ρs) {p : ℝ × ℝ | μ * p.1 + (1 - μ) * p.2 ≤ x}
      ≤ (ρr.prod ρs) (({r | μ * r ≤ x} ×ˢ univ) ∪ (univ ×ˢ Iio 0)) := measure_mono hsub
    _ ≤ (ρr.prod ρs) ({r | μ * r ≤ x} ×ˢ univ) + (ρr.prod ρs) (univ ×ˢ Iio 0) :=
        measure_union_le _ _
    _ = (ρr.prod ρs) ({r | μ * r ≤ x} ×ˢ univ) := by
        rw [Measure.prod_prod (univ : Set ℝ) (Iio (0 : ℝ)), hs, mul_zero, add_zero]

theorem p2_strict_point (hμ1 : μ < 1) (hs : ρs (Iio 0) = 0)
    (hpos : 0 < ρs (Ioi 0)) :
    ∃ q : ℝ, cdf (investorLaw μ ρr ρs) q < cdf (nonInvestorLaw μ ρr) q := by
  set P := ρr.prod ρs
  set X : ℝ × ℝ → ℝ := fun p => μ * p.1 + (1 - μ) * p.2
  set Y : ℝ × ℝ → ℝ := fun p => μ * p.1
  have hXm : Measurable X := measurable_investorScore μ
  have hYm : Measurable Y := by fun_prop
  have hE : 0 < P {p | Y p < X p} := by
    have hsub : (univ : Set ℝ) ×ˢ Ioi (0 : ℝ) ⊆ {p | Y p < X p} := by
      rintro ⟨r, s⟩ ⟨_, hs'⟩
      simp only [mem_Ioi] at hs'
      simp only [mem_setOf_eq, X, Y]
      have : 0 < (1 - μ) * s := mul_pos (by linarith) hs'
      linarith
    calc (0 : ENNReal) < ρr univ * ρs (Ioi 0) := by rw [measure_univ, one_mul]; exact hpos
      _ = P ((univ : Set ℝ) ×ˢ Ioi (0 : ℝ)) := (Measure.prod_prod _ _).symm
      _ ≤ P {p | Y p < X p} := measure_mono hsub
  have hcover : {p | Y p < X p} ⊆ ⋃ q : ℚ, {p | Y p ≤ (q : ℝ) ∧ (q : ℝ) < X p} := by
    intro p hp
    obtain ⟨q, hq1, hq2⟩ := exists_rat_btwn (K := ℝ) (show Y p < X p from hp)
    exact mem_iUnion.mpr ⟨q, le_of_lt hq1, hq2⟩
  have hex : ∃ q : ℚ, 0 < P {p | Y p ≤ (q : ℝ) ∧ (q : ℝ) < X p} := by
    by_contra hno
    push_neg at hno
    have hnull : P (⋃ q : ℚ, {p | Y p ≤ (q : ℝ) ∧ (q : ℝ) < X p}) = 0 :=
      measure_iUnion_null (fun q => le_antisymm (hno q) (zero_le _))
    exact absurd (measure_mono_null hcover hnull) (ne_of_gt hE)
  obtain ⟨q, hq⟩ := hex
  refine ⟨q, ?_⟩
  rw [cdf_eq_real, cdf_eq_real, measureReal_def, measureReal_def]
  refine (ENNReal.toReal_lt_toReal (measure_ne_top _ _) (measure_ne_top _ _)).mpr ?_
  rw [investorLaw_Iic, nonInvestorLaw_Iic μ ρr ρs]
  have hA : MeasurableSet {p : ℝ × ℝ | X p ≤ q ∧ 0 ≤ p.2} :=
    (measurableSet_le hXm measurable_const).inter (measurableSet_le measurable_const measurable_snd)
  have hB : MeasurableSet {p : ℝ × ℝ | Y p ≤ (q : ℝ) ∧ (q : ℝ) < X p} :=
    (measurableSet_le hYm measurable_const).inter (measurableSet_lt measurable_const hXm)
  have hdisj : Disjoint {p : ℝ × ℝ | X p ≤ q ∧ 0 ≤ p.2} {p | Y p ≤ (q : ℝ) ∧ (q : ℝ) < X p} := by
    rw [Set.disjoint_left]
    rintro p ⟨h1, _⟩ ⟨_, h2⟩
    exact absurd h1 (not_le.mpr h2)
  have hunion_sub : {p : ℝ × ℝ | X p ≤ q ∧ 0 ≤ p.2} ∪ {p | Y p ≤ (q : ℝ) ∧ (q : ℝ) < X p}
      ⊆ {r : ℝ | μ * r ≤ q} ×ˢ univ := by
    rintro ⟨r, s⟩ hp
    rcases hp with ⟨h1, h2⟩ | ⟨h1, _⟩
    · have : 0 ≤ (1 - μ) * s := mul_nonneg (by linarith) h2
      simp only [X] at h1
      exact ⟨by simp only [mem_setOf_eq]; linarith, mem_univ _⟩
    · exact ⟨h1, mem_univ _⟩
  have hXle : P {p | X p ≤ q} ≤ P {p : ℝ × ℝ | X p ≤ q ∧ 0 ≤ p.2} := by
    have hsplit : {p : ℝ × ℝ | X p ≤ q}
        ⊆ {p : ℝ × ℝ | X p ≤ q ∧ 0 ≤ p.2} ∪ ((univ : Set ℝ) ×ˢ Iio (0 : ℝ)) := by
      rintro ⟨r, s⟩ h
      by_cases hs0 : s < 0
      · exact Or.inr ⟨mem_univ _, hs0⟩
      · exact Or.inl ⟨h, not_lt.mp hs0⟩
    calc P {p | X p ≤ q} ≤ P ({p : ℝ × ℝ | X p ≤ q ∧ 0 ≤ p.2} ∪ ((univ : Set ℝ) ×ˢ Iio (0 : ℝ))) :=
          measure_mono hsplit
      _ ≤ P {p : ℝ × ℝ | X p ≤ q ∧ 0 ≤ p.2} + P ((univ : Set ℝ) ×ˢ Iio (0 : ℝ)) :=
          measure_union_le _ _
      _ = P {p : ℝ × ℝ | X p ≤ q ∧ 0 ≤ p.2} := by
          rw [Measure.prod_prod, hs, mul_zero, add_zero]
  calc P {p | X p ≤ q} ≤ P {p : ℝ × ℝ | X p ≤ q ∧ 0 ≤ p.2} := hXle
    _ < P {p : ℝ × ℝ | X p ≤ q ∧ 0 ≤ p.2} + P {p | Y p ≤ (q : ℝ) ∧ (q : ℝ) < X p} :=
        ENNReal.lt_add_right (measure_ne_top _ _) (ne_of_gt hq)
    _ = P ({p : ℝ × ℝ | X p ≤ q ∧ 0 ≤ p.2} ∪ {p | Y p ≤ (q : ℝ) ∧ (q : ℝ) < X p}) :=
        (measure_union hdisj hB).symm
    _ ≤ P ({r : ℝ | μ * r ≤ q} ×ˢ univ) := measure_mono hunion_sub

theorem p2_strict_interval (hμ1 : μ < 1) (hs : ρs (Iio 0) = 0)
    (hpos : 0 < ρs (Ioi 0)) :
    ∃ q u : ℝ, q < u ∧ ∀ x ∈ Ico q u,
      cdf (investorLaw μ ρr ρs) x < cdf (nonInvestorLaw μ ρr) x := by
  obtain ⟨q, hq⟩ := p2_strict_point μ ρr ρs hμ1 hs hpos
  have hcont : ContinuousWithinAt
      (fun x => cdf (nonInvestorLaw μ ρr) x - cdf (investorLaw μ ρr ρs) x) (Ici q) q :=
    ((cdf (nonInvestorLaw μ ρr)).right_continuous q).sub
      ((cdf (investorLaw μ ρr ρs)).right_continuous q)
  have hev : ∀ᶠ x in 𝓝[≥] q,
      0 < cdf (nonInvestorLaw μ ρr) x - cdf (investorLaw μ ρr ρs) x :=
    hcont.eventually_const_lt (by linarith)
  obtain ⟨u, hu, hsub⟩ := (mem_nhdsGE_iff_exists_Ico_subset).mp hev
  exact ⟨q, u, hu, fun x hx => by have := hsub hx; simp only [mem_setOf_eq] at this; linarith⟩

theorem Delta_nonneg_from_primitives (hV : 0 ≤ V) (hμ1 : μ ≤ 1) (hs : ρs (Iio 0) = 0)
    [NoAtoms (investorLaw μ ρr ρs)] [NoAtoms (nonInvestorLaw μ ρr)] (Q m : ℕ) :
    0 ≤ EntryContestModel.Delta V (investorLaw μ ρr ρs) (nonInvestorLaw μ ρr) C Q m :=
  Delta_nonneg V (investorLaw μ ρr ρs) (nonInvestorLaw μ ρr) C hV (p2_fosd μ ρr ρs hμ1 hs) Q m

end EntryContestP1P2
