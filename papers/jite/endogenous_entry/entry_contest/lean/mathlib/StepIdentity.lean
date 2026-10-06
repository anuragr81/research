import Mathlib

open MeasureTheory ProbabilityTheory Set

set_option linter.unusedSectionVars false

namespace EntryContestStep

def EX : Set ((ℝ × ℝ) × ℝ) := {p | p.1.2 ≤ p.1.1 ∧ p.2 ≤ p.1.1}

def EY : Set ((ℝ × ℝ) × ℝ) := {p | p.1.1 ≤ p.1.2 ∧ p.2 ≤ p.1.2}

def EZ : Set ((ℝ × ℝ) × ℝ) := {p | p.1.1 ≤ p.2 ∧ p.1.2 ≤ p.2}

theorem measurableSet_EX : MeasurableSet EX := by
  have h1 : MeasurableSet {p : (ℝ × ℝ) × ℝ | p.1.2 ≤ p.1.1} :=
    measurableSet_le (by fun_prop) (by fun_prop)
  have h2 : MeasurableSet {p : (ℝ × ℝ) × ℝ | p.2 ≤ p.1.1} :=
    measurableSet_le (by fun_prop) (by fun_prop)
  exact h1.inter h2

theorem measurableSet_EY : MeasurableSet EY := by
  have h1 : MeasurableSet {p : (ℝ × ℝ) × ℝ | p.1.1 ≤ p.1.2} :=
    measurableSet_le (by fun_prop) (by fun_prop)
  have h2 : MeasurableSet {p : (ℝ × ℝ) × ℝ | p.2 ≤ p.1.2} :=
    measurableSet_le (by fun_prop) (by fun_prop)
  exact h1.inter h2

theorem measurableSet_EZ : MeasurableSet EZ := by
  have h1 : MeasurableSet {p : (ℝ × ℝ) × ℝ | p.1.1 ≤ p.2} :=
    measurableSet_le (by fun_prop) (by fun_prop)
  have h2 : MeasurableSet {p : (ℝ × ℝ) × ℝ | p.1.2 ≤ p.2} :=
    measurableSet_le (by fun_prop) (by fun_prop)
  exact h1.inter h2

theorem measurable_Iic (μ : Measure ℝ) : Measurable (fun x : ℝ => μ (Iic x)) :=
  Monotone.measurable (fun _ _ hxy => measure_mono (Iic_subset_Iic.mpr hxy))

theorem EX_union_EY_union_EZ : EX ∪ EY ∪ EZ = univ := by
  ext ⟨⟨x, y⟩, z⟩
  simp only [EX, EY, EZ, mem_union, mem_setOf_eq, mem_univ, iff_true]
  rcases le_total y x with hyx | hxy
  · rcases le_total z x with hzx | hxz
    · exact Or.inl (Or.inl ⟨hyx, hzx⟩)
    · exact Or.inr ⟨hxz, le_trans hyx hxz⟩
  · rcases le_total z y with hzy | hyz
    · exact Or.inl (Or.inr ⟨hxy, hzy⟩)
    · exact Or.inr ⟨le_trans hxy hyz, hyz⟩

variable (A B C : Measure ℝ) [IsProbabilityMeasure A] [IsProbabilityMeasure B]
  [IsProbabilityMeasure C]

theorem EZ_eq : ((A.prod B).prod C) EZ = ∫⁻ z, A (Iic z) * B (Iic z) ∂C := by
  rw [Measure.prod_apply_symm measurableSet_EZ]
  refine lintegral_congr (fun z => ?_)
  have h : (fun p : ℝ × ℝ => (p, z)) ⁻¹' EZ = Iic z ×ˢ Iic z := by
    ext ⟨x, y⟩; simp [EZ]
  rw [h, Measure.prod_prod]

theorem EX_eq : ((A.prod B).prod C) EX = ∫⁻ x, B (Iic x) * C (Iic x) ∂A := by
  rw [Measure.prod_apply measurableSet_EX]
  have h : ∀ p : ℝ × ℝ, C (Prod.mk p ⁻¹' EX)
      = {q : ℝ × ℝ | q.2 ≤ q.1}.indicator (fun q => C (Iic q.1)) p := by
    intro p
    by_cases hp : p.2 ≤ p.1
    · have : Prod.mk p ⁻¹' EX = Iic p.1 := by ext z; simp [EX, hp]
      rw [this, indicator_of_mem (by simpa using hp)]
    · have : Prod.mk p ⁻¹' EX = ∅ := by ext z; simp [EX, hp]
      rw [this, indicator_of_notMem (by simpa using hp)]
      simp
  simp_rw [h]
  rw [lintegral_prod]
  · refine lintegral_congr (fun x => ?_)
    have hx : (fun y => {q : ℝ × ℝ | q.2 ≤ q.1}.indicator (fun q => C (Iic q.1)) (x, y))
        = (Iic x).indicator (fun _ => C (Iic x)) := by
      funext y
      by_cases hy : y ≤ x <;> simp [indicator, hy]
    simp only at hx ⊢
    rw [hx, lintegral_indicator_const measurableSet_Iic, mul_comm]
  · exact (Measurable.indicator ((measurable_Iic C).comp measurable_fst)
      (measurableSet_le measurable_snd measurable_fst)).aemeasurable

theorem EY_eq : ((A.prod B).prod C) EY = ∫⁻ y, A (Iic y) * C (Iic y) ∂B := by
  rw [Measure.prod_apply measurableSet_EY]
  have h : ∀ p : ℝ × ℝ, C (Prod.mk p ⁻¹' EY)
      = {q : ℝ × ℝ | q.1 ≤ q.2}.indicator (fun q => C (Iic q.2)) p := by
    intro p
    by_cases hp : p.1 ≤ p.2
    · have : Prod.mk p ⁻¹' EY = Iic p.2 := by ext z; simp [EY, hp]
      rw [this, indicator_of_mem (by simpa using hp)]
    · have : Prod.mk p ⁻¹' EY = ∅ := by ext z; simp [EY, hp]
      rw [this, indicator_of_notMem (by simpa using hp)]
      simp
  simp_rw [h]
  rw [lintegral_prod_symm]
  · refine lintegral_congr (fun y => ?_)
    have hy : (fun x => {q : ℝ × ℝ | q.1 ≤ q.2}.indicator (fun q => C (Iic q.2)) (x, y))
        = (Iic y).indicator (fun _ => C (Iic y)) := by
      funext x
      by_cases hx : x ≤ y <;> simp [indicator, hx]
    simp only at hy ⊢
    rw [hy, lintegral_indicator_const measurableSet_Iic, mul_comm]
  · exact (Measurable.indicator ((measurable_Iic C).comp measurable_snd)
      (measurableSet_le measurable_fst measurable_snd)).aemeasurable

theorem diag_null [NoAtoms B] : (A.prod B) {q : ℝ × ℝ | q.1 = q.2} = 0 := by
  rw [Measure.prod_apply (measurableSet_eq_fun measurable_fst measurable_snd)]
  have h : ∀ x : ℝ, Prod.mk x ⁻¹' {q : ℝ × ℝ | q.1 = q.2} = {x} := by
    intro x; ext y; simp [eq_comm]
  simp [h]

theorem tie_xy_null [NoAtoms B] : ((A.prod B).prod C) (EX ∩ EY) = 0 := by
  refine measure_mono_null (t := {q : ℝ × ℝ | q.1 = q.2} ×ˢ univ) ?_ ?_
  · rintro ⟨⟨x, y⟩, z⟩ ⟨h1, h2⟩
    simp only [EX, EY, mem_setOf_eq] at h1 h2
    simp [le_antisymm h2.1 h1.1]
  · rw [Measure.prod_prod, diag_null A B, zero_mul]

theorem tie_xz_null [NoAtoms A] : ((A.prod B).prod C) (EX ∩ EZ) = 0 := by
  refine measure_mono_null (t := {p : (ℝ × ℝ) × ℝ | p.1.1 = p.2}) ?_ ?_
  · rintro ⟨⟨x, y⟩, z⟩ ⟨h1, h2⟩
    simp only [EX, EZ, mem_setOf_eq] at h1 h2 ⊢
    exact le_antisymm h2.1 h1.2
  · rw [Measure.prod_apply_symm (measurableSet_eq_fun (by fun_prop) (by fun_prop))]
    have h : ∀ z : ℝ, (fun q : ℝ × ℝ => (q, z)) ⁻¹' {p : (ℝ × ℝ) × ℝ | p.1.1 = p.2}
        = {z} ×ˢ univ := by
      intro z; ext ⟨x, y⟩; simp
    simp [h, Measure.prod_prod]

theorem tie_yz_null [NoAtoms B] : ((A.prod B).prod C) (EY ∩ EZ) = 0 := by
  refine measure_mono_null (t := {p : (ℝ × ℝ) × ℝ | p.1.2 = p.2}) ?_ ?_
  · rintro ⟨⟨x, y⟩, z⟩ ⟨h1, h2⟩
    simp only [EY, EZ, mem_setOf_eq] at h1 h2 ⊢
    exact le_antisymm h2.2 h1.2
  · rw [Measure.prod_apply_symm (measurableSet_eq_fun (by fun_prop) (by fun_prop))]
    have h : ∀ z : ℝ, (fun q : ℝ × ℝ => (q, z)) ⁻¹' {p : (ℝ × ℝ) × ℝ | p.1.2 = p.2}
        = univ ×ˢ {z} := by
      intro z; ext ⟨x, y⟩; simp
    simp [h, Measure.prod_prod]

theorem three_max_sum [NoAtoms A] [NoAtoms B] :
    ((A.prod B).prod C) EX + ((A.prod B).prod C) EY + ((A.prod B).prod C) EZ = 1 := by
  set P := (A.prod B).prod C
  have h1 : P (EX ∪ EY) = P EX + P EY := by
    have := measure_union_add_inter (μ := P) EX measurableSet_EY
    rw [tie_xy_null A B C, add_zero] at this
    exact this
  have hnull : P ((EX ∪ EY) ∩ EZ) = 0 := by
    rw [union_inter_distrib_right]
    exact measure_union_null (tie_xz_null A B C) (tie_yz_null A B C)
  have h2 := measure_union_add_inter (μ := P) (EX ∪ EY) measurableSet_EZ
  rw [hnull, add_zero, EX_union_EY_union_EZ, measure_univ, h1] at h2
  exact h2.symm

theorem integral_cdf_mul (μ ν ρ : Measure ℝ) [IsProbabilityMeasure μ] [IsProbabilityMeasure ν]
    [IsProbabilityMeasure ρ] :
    ∫ x, cdf μ x * cdf ν x ∂ρ = (∫⁻ x, μ (Iic x) * ν (Iic x) ∂ρ).toReal := by
  simp_rw [cdf_eq_real, measureReal_def, ← ENNReal.toReal_mul]
  rw [integral_toReal]
  · exact ((measurable_Iic μ).mul (measurable_Iic ν)).aemeasurable
  · exact ae_of_all _ (fun x => ENNReal.mul_lt_top (measure_lt_top _ _) (measure_lt_top _ _))

theorem three_max_sum_real [NoAtoms A] [NoAtoms B] :
    (∫ x, cdf B x * cdf C x ∂A) + (∫ x, cdf A x * cdf C x ∂B)
      + (∫ x, cdf A x * cdf B x ∂C) = 1 := by
  have h := three_max_sum A B C
  rw [EX_eq, EY_eq, EZ_eq] at h
  rw [integral_cdf_mul, integral_cdf_mul, integral_cdf_mul]
  have hX : (∫⁻ x, B (Iic x) * C (Iic x) ∂A) ≠ ⊤ := by
    intro hx; rw [hx] at h; simp at h
  have hY : (∫⁻ y, A (Iic y) * C (Iic y) ∂B) ≠ ⊤ := by
    intro hy; rw [hy] at h; simp at h
  have hZ : (∫⁻ z, A (Iic z) * B (Iic z) ∂C) ≠ ⊤ := by
    intro hz; rw [hz] at h; simp at h
  rw [← ENNReal.toReal_add hX hY, ← ENNReal.toReal_add (ENNReal.add_ne_top.mpr ⟨hX, hY⟩) hZ, h]
  simp

theorem integrable_cdf_mul (μ ν ρ : Measure ℝ) [IsProbabilityMeasure μ] [IsProbabilityMeasure ν]
    [IsProbabilityMeasure ρ] : Integrable (fun x => cdf μ x * cdf ν x) ρ := by
  refine (integrable_const (1 : ℝ)).mono' ?_ (ae_of_all _ (fun x => ?_))
  · exact ((cdf μ).mono.measurable.mul (cdf ν).mono.measurable).aestronglyMeasurable
  · have h1 := cdf_nonneg μ x
    have h2 := cdf_le_one μ x
    have h3 := cdf_nonneg ν x
    have h4 := cdf_le_one ν x
    rw [Real.norm_eq_abs, abs_of_nonneg (mul_nonneg h1 h3)]
    nlinarith

theorem step_identity (α β κ : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]
    [IsProbabilityMeasure κ] [NoAtoms α] [NoAtoms β] [NoAtoms κ] :
    ((∫ x, cdf κ x * cdf α x ∂α) - (∫ x, cdf κ x * cdf α x ∂β))
      - ((∫ x, cdf κ x * cdf β x ∂α) - (∫ x, cdf κ x * cdf β x ∂β))
      = -(1 / 2) * ∫ x, (cdf α x - cdf β x) ^ 2 ∂κ := by
  have tαα := three_max_sum_real κ α α
  have tββ := three_max_sum_real κ β β
  have tαβ := three_max_sum_real κ α β
  have hsq : ∫ x, (cdf α x - cdf β x) ^ 2 ∂κ
      = (∫ x, cdf α x * cdf α x ∂κ) - 2 * (∫ x, cdf α x * cdf β x ∂κ)
        + (∫ x, cdf β x * cdf β x ∂κ) := by
    have e : (fun x => (cdf α x - cdf β x) ^ 2)
        = fun x => (cdf α x * cdf α x - 2 * (cdf α x * cdf β x)) + cdf β x * cdf β x := by
      funext x; ring
    rw [e, integral_add, integral_sub, integral_const_mul]
    · exact integrable_cdf_mul α α κ
    · exact (integrable_cdf_mul α β κ).const_mul 2
    · exact (integrable_cdf_mul α α κ).sub ((integrable_cdf_mul α β κ).const_mul 2)
    · exact integrable_cdf_mul β β κ
  rw [hsq]
  linarith [tαα, tββ, tαβ]

theorem step_nonpos (α β κ : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]
    [IsProbabilityMeasure κ] [NoAtoms α] [NoAtoms β] [NoAtoms κ] (V : ℝ) (hV : 0 ≤ V) :
    V * (((∫ x, cdf κ x * cdf α x ∂α) - (∫ x, cdf κ x * cdf α x ∂β))
      - ((∫ x, cdf κ x * cdf β x ∂α) - (∫ x, cdf κ x * cdf β x ∂β))) ≤ 0 := by
  rw [step_identity α β κ]
  have : 0 ≤ ∫ x, (cdf α x - cdf β x) ^ 2 ∂κ := integral_nonneg (fun x => sq_nonneg _)
  nlinarith

end EntryContestStep
