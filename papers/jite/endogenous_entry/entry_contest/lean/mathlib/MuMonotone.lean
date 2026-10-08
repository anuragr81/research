import Mathlib
import Equilibrium

open MeasureTheory ProbabilityTheory Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestMu

open EntryContestModel EntryContestP1P2 EntryContestP6P7 EntryContestPMU EntryContestPMUWitness
  EntryContestRefute EntryContestFall EntryContestEq

/-! **The entry gain falls as the score weights talent more.** An entrant scores
`μ r + (1 − μ) s` and an outsider `μ r`, with `r ~ ρr`, `s ~ ρs` independent and `s ≥ 0`; the
incumbent is scored like an entrant. Dividing every score by `μ > 0` leaves the gain unchanged
and turns the entrant's law into that of `r + t s` with `t = (1 − μ)/μ`, while the outsider's
law becomes `ρr`. A larger `t` pushes the entrant's CDF down pointwise, which lowers the
outsider's win probability and raises the entrant's, so the gain rises in `t` and falls in `μ`.
The equilibrium count then rises as `μ` falls, by `kstar_mono`. -/

section Normalise

/-- The entrant's law after dividing by `μ`: the law of `r + t s`. -/
noncomputable def payerLaw (t : ℝ) (ρr ρs : Measure ℝ) : Measure ℝ :=
  (ρr.prod ρs).map (fun p : ℝ × ℝ => p.1 + t * p.2)

theorem measurable_payerScore (t : ℝ) : Measurable (fun p : ℝ × ℝ => p.1 + t * p.2) := by
  fun_prop

instance payerLaw_isProb (t : ℝ) (ρr ρs : Measure ℝ) [IsProbabilityMeasure ρr]
    [IsProbabilityMeasure ρs] : IsProbabilityMeasure (payerLaw t ρr ρs) :=
  Measure.isProbabilityMeasure_map (measurable_payerScore t).aemeasurable

/-- `r + t s` has no atoms when `r` has none: each fibre `{r = x − t y}` is `ρr`-null. -/
instance payerLaw_noAtoms (t : ℝ) (ρr ρs : Measure ℝ) [IsProbabilityMeasure ρr]
    [IsProbabilityMeasure ρs] [NoAtoms ρr] : NoAtoms (payerLaw t ρr ρs) := ⟨fun x => by
  rw [payerLaw, Measure.map_apply (measurable_payerScore t) (measurableSet_singleton x),
    Measure.prod_apply_symm ((measurable_payerScore t) (measurableSet_singleton x))]
  have h : ∀ y, ρr ((fun r => (r, y)) ⁻¹'
      ((fun p : ℝ × ℝ => p.1 + t * p.2) ⁻¹' {x})) = 0 := by
    intro y
    have e : (fun r => (r, y)) ⁻¹' ((fun p : ℝ × ℝ => p.1 + t * p.2) ⁻¹' {x})
        = {x - t * y} := by
      ext r
      simp only [mem_preimage, mem_singleton_iff]
      constructor <;> intro h <;> linarith
    rw [e]
    exact measure_singleton _
  simp [h]⟩

variable (ρr ρs : Measure ℝ) [IsProbabilityMeasure ρr] [IsProbabilityMeasure ρs]

/-- The entrant's law is the normalised law scaled by `μ`, for `μ ≠ 0`. -/
theorem investorLaw_eq_map (μ : ℝ) (hμ : μ ≠ 0) :
    investorLaw μ ρr ρs = (payerLaw ((1 - μ) / μ) ρr ρs).map (fun y => μ * y) := by
  have e : (fun y : ℝ => μ * y) ∘ (fun p : ℝ × ℝ => p.1 + (1 - μ) / μ * p.2)
      = fun p : ℝ × ℝ => μ * p.1 + (1 - μ) * p.2 := by
    funext p
    simp only [Function.comp]
    field_simp
  rw [investorLaw, payerLaw,
    Measure.map_map (measurable_nonInvestorScore μ) (measurable_payerScore ((1 - μ) / μ)), e]

/-- The outsider's law is `ρr` scaled by `μ`, by definition. -/
theorem nonInvestorLaw_eq_map (μ : ℝ) : nonInvestorLaw μ ρr = ρr.map (fun y => μ * y) := rfl

/-- **The gain is invariant to a common positive rescaling of all three laws.** -/
theorem Delta_map_mul (V μ : ℝ) (hμ : 0 < μ) (α β C : Measure ℝ) [IsProbabilityMeasure α]
    [IsProbabilityMeasure β] [IsProbabilityMeasure C] (Q m : ℕ) :
    Delta V (α.map (fun y => μ * y)) (β.map (fun y => μ * y)) (C.map (fun y => μ * y)) Q m
      = Delta V α β C Q m := by
  unfold Delta
  have hcdf : ∀ y, cdf (rivals (C.map (fun y => μ * y)) (α.map (fun y => μ * y))
      (β.map (fun y => μ * y)) m (Q - 1 - m)) (μ * y) = cdf (rivals C α β m (Q - 1 - m)) y := by
    intro y
    rw [cdf_rivals, cdf_rivals, cdf_map_mul C μ hμ, cdf_map_mul α μ hμ, cdf_map_mul β μ hμ,
      mul_div_cancel_left₀ y hμ.ne']
  rw [integral_map (μ := α) (measurable_nonInvestorScore μ).aemeasurable
      (cdf _).mono.measurable.aestronglyMeasurable,
    integral_map (μ := β) (measurable_nonInvestorScore μ).aemeasurable
      (cdf _).mono.measurable.aestronglyMeasurable]
  simp_rw [hcdf]

/-- **Normalisation.** For `μ > 0` the gain equals the gain of the normalised laws, with the
    entrant and the incumbent at `r + t s`, `t = (1 − μ)/μ`, and the outsider at `r`. -/
theorem Delta_normalise (V μ : ℝ) (hμ : 0 < μ) (Q m : ℕ) :
    Delta V (investorLaw μ ρr ρs) (nonInvestorLaw μ ρr) (investorLaw μ ρr ρs) Q m
      = Delta V (payerLaw ((1 - μ) / μ) ρr ρs) ρr (payerLaw ((1 - μ) / μ) ρr ρs) Q m := by
  rw [investorLaw_eq_map ρr ρs μ hμ.ne', nonInvestorLaw_eq_map ρr μ, Delta_map_mul V μ hμ]

end Normalise

section Pointwise

variable (ρr ρs : Measure ℝ) [IsProbabilityMeasure ρr] [IsProbabilityMeasure ρs]

theorem payerLaw_Iic (t x : ℝ) :
    payerLaw t ρr ρs (Iic x) = (ρr.prod ρs) {p | p.1 + t * p.2 ≤ x} := by
  rw [payerLaw, Measure.map_apply (measurable_payerScore t) measurableSet_Iic]
  rfl

/-- **A larger weight on `s ≥ 0` pushes the CDF down.** For `t ≤ t'` and `s ≥ 0` a.s., the
    event `{r + t' s ≤ x}` lies in `{r + t s ≤ x}` up to the null set `{s < 0}`. -/
theorem cdf_payerLaw_antitone (t t' : ℝ) (htt : t ≤ t') (hs : ρs (Iio 0) = 0) (x : ℝ) :
    cdf (payerLaw t' ρr ρs) x ≤ cdf (payerLaw t ρr ρs) x := by
  rw [cdf_eq_real, cdf_eq_real, measureReal_def, measureReal_def]
  refine ENNReal.toReal_mono (measure_ne_top _ _) ?_
  rw [payerLaw_Iic ρr ρs t' x, payerLaw_Iic ρr ρs t x]
  have hsub : {p : ℝ × ℝ | p.1 + t' * p.2 ≤ x}
      ⊆ {p : ℝ × ℝ | p.1 + t * p.2 ≤ x} ∪ (univ ×ˢ Iio 0) := by
    rintro ⟨r, s⟩ h
    simp only [mem_setOf_eq] at h
    by_cases hs0 : s < 0
    · exact Or.inr ⟨mem_univ _, hs0⟩
    · have : t * s ≤ t' * s := mul_le_mul_of_nonneg_right htt (not_lt.mp hs0)
      exact Or.inl (by simp only [mem_setOf_eq]; linarith)
  calc (ρr.prod ρs) {p : ℝ × ℝ | p.1 + t' * p.2 ≤ x}
      ≤ (ρr.prod ρs) ({p : ℝ × ℝ | p.1 + t * p.2 ≤ x} ∪ (univ ×ˢ Iio 0)) := measure_mono hsub
    _ ≤ (ρr.prod ρs) {p : ℝ × ℝ | p.1 + t * p.2 ≤ x} + (ρr.prod ρs) (univ ×ˢ Iio 0) :=
        measure_union_le _ _
    _ = (ρr.prod ρs) {p : ℝ × ℝ | p.1 + t * p.2 ≤ x} := by
        rw [Measure.prod_prod (univ : Set ℝ) (Iio (0 : ℝ)), hs, mul_zero, add_zero]

end Pointwise

section Integrals

theorem integrable_cdf_pow (ν ρ : Measure ℝ) [IsProbabilityMeasure ν] [IsProbabilityMeasure ρ]
    (j : ℕ) : Integrable (fun x => cdf ν x ^ j) ρ :=
  integrable_of_bounded ρ _ ((cdf ν).mono.measurable.pow_const j) 1 (fun x => by
    rw [abs_of_nonneg (pow_nonneg (cdf_nonneg ν x) j)]
    exact pow_le_one₀ (cdf_nonneg ν x) (cdf_le_one ν x))

/-- With the incumbent at the entrant's law and `n ≥ 1` outsiders, the entrant's win
    probability is `(1 − ∫ F^(m+2) dG^n) / (m+2)`, by `integral_iid` and `two_max_sum`. -/
theorem first_integral (α β : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]
    [NoAtoms α] [NoAtoms β] (m k : ℕ) :
    ∫ x, cdf (rivals α α β m (k + 1)) x ∂α
      = (1 - ∫ x, cdf α x ^ (m + 1 + 1) ∂(powLaw β β k)) / (m + 2) := by
  have h1 : ∀ x, cdf (rivals α α β m (k + 1)) x
      = cdf (powLaw β β k) x * cdf α x ^ (m + 1) := by
    intro x
    rw [cdf_rivals, cdf_iid]
    ring
  simp_rw [h1]
  have h2 := integral_iid α (m + 1) (cdf (powLaw β β k)) (cdf _).mono.measurable 1
    (abs_cdf_le_one _)
  have h3 := two_max_sum (powLaw α α (m + 1)) (powLaw β β k)
  simp_rw [cdf_iid α (m + 1)] at h3
  rw [h2] at h3
  push_cast at h3
  rw [eq_div_iff (by positivity)]
  linear_combination h3

/-- With no outsider the entrant's win probability is `1/(m+2)`, whatever the law. -/
theorem first_integral_zero (α β : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]
    [NoAtoms α] (m : ℕ) :
    ∫ x, cdf (rivals α α β m 0) x ∂α = 1 / (m + 2) := by
  have h1 : ∀ x, cdf (rivals α α β m 0) x = cdf α x ^ (m + 1) := by
    intro x
    rw [cdf_rivals, pow_zero, mul_one]
    ring
  simp_rw [h1]
  rw [integral_cdf_pow α (m + 1)]
  push_cast
  ring

variable (ρr ρs : Measure ℝ) [IsProbabilityMeasure ρr] [IsProbabilityMeasure ρs]

theorem cdf_rivals_payer_le (t t' : ℝ) (htt : t ≤ t') (hs : ρs (Iio 0) = 0) (m n : ℕ) (x : ℝ) :
    cdf (rivals (payerLaw t' ρr ρs) (payerLaw t' ρr ρs) ρr m n) x
      ≤ cdf (rivals (payerLaw t ρr ρs) (payerLaw t ρr ρs) ρr m n) x := by
  rw [cdf_rivals, cdf_rivals]
  have h := cdf_payerLaw_antitone ρr ρs t t' htt hs x
  have h0 := cdf_nonneg (payerLaw t' ρr ρs) x
  have hp : cdf (payerLaw t' ρr ρs) x ^ m ≤ cdf (payerLaw t ρr ρs) x ^ m :=
    pow_le_pow_left₀ h0 h m
  exact mul_le_mul_of_nonneg_right
    (mul_le_mul h hp (pow_nonneg h0 m) (cdf_nonneg _ _)) (pow_nonneg (cdf_nonneg ρr x) n)

/-- The outsider's win probability falls in `t`. -/
theorem second_integral_antitone (t t' : ℝ) (htt : t ≤ t') (hs : ρs (Iio 0) = 0) (m n : ℕ) :
    ∫ x, cdf (rivals (payerLaw t' ρr ρs) (payerLaw t' ρr ρs) ρr m n) x ∂ρr
      ≤ ∫ x, cdf (rivals (payerLaw t ρr ρs) (payerLaw t ρr ρs) ρr m n) x ∂ρr :=
  integral_mono (integrable_cdf _ ρr) (integrable_cdf _ ρr)
    (fun x => cdf_rivals_payer_le ρr ρs t t' htt hs m n x)

theorem pow_integral_antitone (t t' : ℝ) (htt : t ≤ t') (hs : ρs (Iio 0) = 0) (j : ℕ)
    (B : Measure ℝ) [IsProbabilityMeasure B] :
    ∫ x, cdf (payerLaw t' ρr ρs) x ^ j ∂B ≤ ∫ x, cdf (payerLaw t ρr ρs) x ^ j ∂B :=
  integral_mono (integrable_cdf_pow _ B j) (integrable_cdf_pow _ B j)
    (fun x => pow_le_pow_left₀ (cdf_nonneg _ x) (cdf_payerLaw_antitone ρr ρs t t' htt hs x) j)

/-- The entrant's win probability rises in `t`. -/
theorem first_integral_mono [NoAtoms ρr] (t t' : ℝ) (htt : t ≤ t') (hs : ρs (Iio 0) = 0)
    (m n : ℕ) :
    ∫ x, cdf (rivals (payerLaw t ρr ρs) (payerLaw t ρr ρs) ρr m n) x ∂(payerLaw t ρr ρs)
      ≤ ∫ x, cdf (rivals (payerLaw t' ρr ρs) (payerLaw t' ρr ρs) ρr m n) x
          ∂(payerLaw t' ρr ρs) := by
  cases n with
  | zero =>
      exact ((first_integral_zero (payerLaw t ρr ρs) ρr m).trans
        (first_integral_zero (payerLaw t' ρr ρs) ρr m).symm).le
  | succ k =>
      rw [first_integral (payerLaw t ρr ρs) ρr m k, first_integral (payerLaw t' ρr ρs) ρr m k]
      have h := pow_integral_antitone ρr ρs t t' htt hs (m + 1 + 1) (powLaw ρr ρr k)
      have hm2 : (0 : ℝ) ≤ (m : ℝ) + 2 := by positivity
      exact div_le_div_of_nonneg_right (by linarith) hm2

/-- **The normalised gain rises in `t`.** -/
theorem Delta_payer_mono [NoAtoms ρr] (V : ℝ) (hV : 0 ≤ V) (t t' : ℝ) (htt : t ≤ t')
    (hs : ρs (Iio 0) = 0) (Q m : ℕ) :
    Delta V (payerLaw t ρr ρs) ρr (payerLaw t ρr ρs) Q m
      ≤ Delta V (payerLaw t' ρr ρs) ρr (payerLaw t' ρr ρs) Q m := by
  unfold Delta
  have h1 := first_integral_mono ρr ρs t t' htt hs m (Q - 1 - m)
  have h2 := second_integral_antitone ρr ρs t t' htt hs m (Q - 1 - m)
  exact mul_le_mul_of_nonneg_left (by linarith) hV

end Integrals

section Main

variable (ρr ρs : Measure ℝ) [IsProbabilityMeasure ρr] [IsProbabilityMeasure ρs]

/-- **The gain is non-increasing in `μ`.** For `0 < μ' ≤ μ`, `s ≥ 0` a.s., `r` atomless and
    `V ≥ 0`, the gain at `μ` is at most the gain at `μ'`, for every `Q` and `m`. -/
theorem Delta_antitone_mu [NoAtoms ρr] (V : ℝ) (hV : 0 ≤ V) (hs : ρs (Iio 0) = 0) (μ μ' : ℝ)
    (hμ' : 0 < μ') (hμμ : μ' ≤ μ) (Q m : ℕ) :
    Delta V (investorLaw μ ρr ρs) (nonInvestorLaw μ ρr) (investorLaw μ ρr ρs) Q m
      ≤ Delta V (investorLaw μ' ρr ρs) (nonInvestorLaw μ' ρr) (investorLaw μ' ρr ρs) Q m := by
  have hμ : 0 < μ := lt_of_lt_of_le hμ' hμμ
  rw [Delta_normalise ρr ρs V μ hμ Q m, Delta_normalise ρr ρs V μ' hμ' Q m]
  refine Delta_payer_mono ρr ρs V hV ((1 - μ) / μ) ((1 - μ') / μ') ?_ hs Q m
  have e : ∀ a : ℝ, a ≠ 0 → (1 - a) / a = 1 / a - 1 := fun a ha => by
    rw [sub_div, div_self ha]
  rw [e μ hμ.ne', e μ' hμ'.ne']
  linarith [one_div_le_one_div_of_le hμ' hμμ]

/-- **The equilibrium count is non-increasing in `μ`.** With fixed costs `κ`, if the entry
    condition holds on the prefix `0, …, k − 1` at `μ` and fails at rank `k'` at `μ' ≤ μ`,
    then `k ≤ k'`. -/
theorem kstar_antitone_mu [NoAtoms ρr] (V : ℝ) (hV : 0 ≤ V) (hs : ρs (Iio 0) = 0) (μ μ' : ℝ)
    (hμ' : 0 < μ') (hμμ : μ' ≤ μ) (Q : ℕ) (κ : ℕ → ℝ) (k k' : ℕ) (hkQ : k ≤ Q)
    (hprefix : ∀ j, j < k →
      κ j ≤ Delta V (investorLaw μ ρr ρs) (nonInvestorLaw μ ρr) (investorLaw μ ρr ρs) Q j)
    (hfail' : k' < Q →
      Delta V (investorLaw μ' ρr ρs) (nonInvestorLaw μ' ρr) (investorLaw μ' ρr ρs) Q k' < κ k') :
    k ≤ k' :=
  kstar_mono κ
    (fun j => Delta V (investorLaw μ ρr ρs) (nonInvestorLaw μ ρr) (investorLaw μ ρr ρs) Q j)
    κ (fun j => Delta V (investorLaw μ' ρr ρs) (nonInvestorLaw μ' ρr) (investorLaw μ' ρr ρs) Q j)
    Q k k' hkQ (fun _ => le_rfl)
    (fun j => Delta_antitone_mu ρr ρs V hV hs μ μ' hμ' hμμ Q j) hprefix hfail'

end Main

section Controls

/-- The normalised law when `s` is a point mass at `c`: `r + t c`. -/
theorem payerLaw_dirac (t c : ℝ) (ρr : Measure ℝ) [SFinite ρr] :
    payerLaw t ρr (Measure.dirac c) = ρr.map (fun r => r + t * c) := by
  have e : ((fun p : ℝ × ℝ => p.1 + t * p.2) ∘ fun r : ℝ => (r, c)) = fun r => r + t * c := by
    funext r
    simp
  rw [payerLaw, Measure.prod_dirac,
    Measure.map_map (measurable_payerScore t) measurable_prodMk_right, e]

theorem cdf_map_add (ρ : Measure ℝ) [IsProbabilityMeasure ρ] (a x : ℝ) :
    cdf (ρ.map (fun r => r + a)) x = cdf ρ (x - a) := by
  haveI : IsProbabilityMeasure (ρ.map (fun r => r + a)) :=
    Measure.isProbabilityMeasure_map (by fun_prop : Measurable (fun r : ℝ => r + a)).aemeasurable
  rw [cdf_eq_real, cdf_eq_real, measureReal_def, measureReal_def,
    Measure.map_apply (by fun_prop : Measurable (fun r : ℝ => r + a)) measurableSet_Iic]
  congr 2
  ext r
  simp only [mem_preimage, mem_Iic]
  constructor <;> intro h <;> linarith

/-- **Control: the pointwise comparison needs `s ≥ 0`.** With `s = −1` surely (so
    `ρs (Iio 0) ≠ 0`) and `r` uniform on `[0, 1]`, the CDF at `0` *rises* from `t = 0` to
    `t = 1`: `0 < 1`. `cdf_payerLaw_antitone` would give the reverse inequality. -/
theorem control_cdf_without_nonneg :
    Measure.dirac (-1 : ℝ) (Iio 0) ≠ 0
      ∧ cdf (payerLaw 0 unif (Measure.dirac (-1))) 0
          < cdf (payerLaw 1 unif (Measure.dirac (-1))) 0 := by
  refine ⟨?_, ?_⟩
  · rw [Measure.dirac_apply_of_mem (show (-1 : ℝ) ∈ Iio 0 by norm_num)]
    exact one_ne_zero
  · rw [payerLaw_dirac, payerLaw_dirac, cdf_map_add unif (0 * (-1)) 0,
      cdf_map_add unif (1 * (-1)) 0, show (0 : ℝ) - 0 * (-1) = 0 by norm_num,
      show (0 : ℝ) - 1 * (-1) = 1 by norm_num, cdf_unif_of_mem 0 le_rfl zero_le_one,
      cdf_unif_of_ge 1 le_rfl]
    exact zero_lt_one

/-- **Control: the normalisation needs `μ > 0`.** The identity behind `Delta_map_mul`,
    `cdf (ρ.map (μ ·)) (μ y) = cdf ρ y`, fails at `μ = 0`: the scaled uniform law is a point
    mass at `0`, with CDF `1` there, while `cdf unif 0 = 0`. -/
theorem control_scale_needs_pos :
    cdf (unif.map (fun y => (0 : ℝ) * y)) (0 * 0) ≠ cdf unif 0 := by
  have e : (fun y : ℝ => (0 : ℝ) * y) = fun _ => (0 : ℝ) := by
    funext y
    ring
  rw [e, Measure.map_const, measure_univ, one_smul, mul_zero, cdf_dirac_zero 0 le_rfl,
    cdf_unif_of_mem 0 le_rfl zero_le_one]
  exact one_ne_zero

end Controls

end EntryContestMu
