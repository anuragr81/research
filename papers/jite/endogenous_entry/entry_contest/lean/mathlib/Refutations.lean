import Mathlib
import PMU
import PMUWitness
import SaturationBenchmark

open MeasureTheory ProbabilityTheory Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestRefute

open EntryContestModel EntryContestP1P2 EntryContestP6P7 EntryContestPMU EntryContestPMUWitness

section Atoms

theorem investorLaw_noAtoms' (μ : ℝ) (hμ : μ ≠ 0) (ρr ρs : Measure ℝ) [IsProbabilityMeasure ρr]
    [IsProbabilityMeasure ρs] [NoAtoms ρr] : NoAtoms (investorLaw μ ρr ρs) := ⟨fun x => by
  rw [investorLaw, Measure.map_apply (measurable_investorScore μ) (measurableSet_singleton x),
    Measure.prod_apply_symm ((measurable_investorScore μ) (measurableSet_singleton x))]
  have h : ∀ y, ρr ((fun r => (r, y)) ⁻¹'
      ((fun p : ℝ × ℝ => μ * p.1 + (1 - μ) * p.2) ⁻¹' {x})) = 0 := by
    intro y
    have e : (fun r => (r, y)) ⁻¹' ((fun p : ℝ × ℝ => μ * p.1 + (1 - μ) * p.2) ⁻¹' {x})
        = {(x - (1 - μ) * y) / μ} := by
      ext r
      simp only [mem_preimage, mem_singleton_iff]
      constructor
      · intro h
        rw [eq_div_iff hμ]
        linarith
      · intro h
        rw [h]
        field_simp
        ring
    rw [e]
    exact measure_singleton _
  simp [h]⟩

theorem nonInvestorLaw_noAtoms' (μ : ℝ) (hμ : μ ≠ 0) (ρr : Measure ℝ) [IsProbabilityMeasure ρr]
    [NoAtoms ρr] : NoAtoms (nonInvestorLaw μ ρr) := ⟨fun x => by
  rw [nonInvestorLaw, Measure.map_apply (measurable_nonInvestorScore μ) (measurableSet_singleton x)]
  have e : (fun r => μ * r) ⁻¹' {x} = {x / μ} := by
    ext r
    simp only [mem_preimage, mem_singleton_iff]
    constructor
    · intro h
      rw [eq_div_iff hμ]
      linarith
    · intro h
      rw [h]
      field_simp
  rw [e]
  exact measure_singleton _⟩

end Atoms

section R1General

/-- **R1, the mechanism.** If non-investor scores never exceed `xG`, then
    `Δ(m, Q) ≥ V (∫_{x ≥ xG} C F^m dF − 1/(Q − m))`. The first term carries no `Q`. -/
theorem r1_lower_bound (V : ℝ) (hV : 0 ≤ V) (α β C : Measure ℝ) [IsProbabilityMeasure α]
    [IsProbabilityMeasure β] [IsProbabilityMeasure C] [NoAtoms β] (xG : ℝ) (hG : cdf β xG = 1)
    (Q m : ℕ) (hm : m < Q) :
    V * ((∫ x in Ici xG, cdf C x * cdf α x ^ m ∂α) - 1 / ((Q - m : ℕ) : ℝ))
      ≤ Delta V α β C Q m := by
  unfold Delta
  set H := rivals C α β m (Q - 1 - m)
  have hGone : ∀ x ∈ Ici xG, cdf β x = 1 := fun x hx =>
    le_antisymm (cdf_le_one β x) (hG ▸ (cdf β).mono hx)
  have hA : (∫ x in Ici xG, cdf C x * cdf α x ^ m ∂α) ≤ ∫ x, cdf H x ∂α := by
    have e : (∫ x in Ici xG, cdf C x * cdf α x ^ m ∂α) = ∫ x in Ici xG, cdf H x ∂α := by
      refine setIntegral_congr_fun measurableSet_Ici (fun x hx => ?_)
      simp only [H]
      rw [cdf_rivals, hGone x hx, one_pow, mul_one]
    rw [e]
    exact setIntegral_le_integral (integrable_cdf H α) (ae_of_all _ (fun x => cdf_nonneg H x))
  have hB : ∫ x, cdf H x ∂β ≤ 1 / ((Q - m : ℕ) : ℝ) := by
    have hle : ∫ x, cdf H x ∂β ≤ ∫ x, cdf β x ^ (Q - 1 - m) ∂β := by
      refine integral_mono (integrable_cdf H β)
        (integrable_of_bounded β _ ((cdf β).mono.measurable.pow_const _) 1 (fun x => by
          rw [abs_of_nonneg (pow_nonneg (cdf_nonneg β x) _)]
          exact pow_le_one₀ (cdf_nonneg β x) (cdf_le_one β x))) (fun x => ?_)
      simp only [H]
      rw [cdf_rivals]
      have a1 := cdf_le_one C x
      have a2 : cdf α x ^ m ≤ 1 := pow_le_one₀ (cdf_nonneg α x) (cdf_le_one α x)
      have a3 := cdf_nonneg C x
      have a4 := pow_nonneg (cdf_nonneg α x) m
      have a5 := pow_nonneg (cdf_nonneg β x) (Q - 1 - m)
      calc cdf C x * cdf α x ^ m * cdf β x ^ (Q - 1 - m) ≤ 1 * 1 * cdf β x ^ (Q - 1 - m) := by
            gcongr
        _ = cdf β x ^ (Q - 1 - m) := by ring
    rw [integral_cdf_pow β (Q - 1 - m)] at hle
    have e : ((Q - 1 - m : ℕ) : ℝ) + 1 = ((Q - m : ℕ) : ℝ) := by
      rw [← Nat.cast_succ]
      congr 1
      omega
    rw [e] at hle
    exact hle
  have := mul_le_mul_of_nonneg_left (show (∫ x in Ici xG, cdf C x * cdf α x ^ m ∂α)
    - 1 / ((Q - m : ℕ) : ℝ) ≤ (∫ x, cdf H x ∂α) - ∫ x, cdf H x ∂β by linarith) hV
  exact this

end R1General

section Uniform

theorem nonInvestor_unif_Iic (μ : ℝ) (hμ : 0 < μ) (x : ℝ) (hx : μ ≤ x) :
    cdf (nonInvestorLaw μ unif) x = 1 := by
  rw [cdf_eq_real, measureReal_def, nonInvestorLaw,
    Measure.map_apply (measurable_nonInvestorScore μ) measurableSet_Iic]
  unfold unif
  rw [Measure.restrict_apply' measurableSet_Icc]
  have hsub : Icc (0 : ℝ) 1 ⊆ (fun r => μ * r) ⁻¹' Iic x := by
    intro r hr
    simp only [mem_preimage, mem_Iic]
    nlinarith [hr.1, hr.2]
  rw [inter_eq_right.mpr hsub, Real.volume_Icc]
  simp

theorem nonInvestor_unif_Ioi (μ : ℝ) (hμ : 0 < μ) : nonInvestorLaw μ unif (Ioi μ) = 0 := by
  rw [nonInvestorLaw, Measure.map_apply (measurable_nonInvestorScore μ) measurableSet_Ioi]
  unfold unif
  rw [Measure.restrict_apply' measurableSet_Icc]
  have hempty : (fun r : ℝ => μ * r) ⁻¹' Ioi μ ∩ Icc 0 1 = ∅ := by
    ext r
    simp only [mem_inter_iff, mem_preimage, mem_Ioi, mem_Icc, mem_empty_iff_false, iff_false,
      not_and]
    intro h1 _ h3
    nlinarith
  rw [hempty, measure_empty]

/-- **With a uniform base score the first entrant's gain never falls in `Q`.** The P-MU weight
    vanishes above `μ`, where the non-investor's score cannot reach, and below `μ` the investor's
    law lies under the non-investor's (`uniform_left`). This holds for every law of `s ≥ 0`, every
    `0 < μ ≤ 1` and every incumbent law. -/
theorem pmu_step_nonneg_uniform (V : ℝ) (hV : 0 ≤ V) (μ : ℝ) (hμ0 : 0 < μ) (hμ1 : μ ≤ 1)
    (ρs C : Measure ℝ) [IsProbabilityMeasure ρs] [IsProbabilityMeasure C]
    (hs : ρs (Iio 0) = 0) (Q : ℕ) (hQ : 1 ≤ Q) :
    0 ≤ stepQ V (investorLaw μ unif ρs) (nonInvestorLaw μ unif) C Q := by
  set α := investorLaw μ unif ρs
  set β := nonInvestorLaw μ unif
  have hzero : ∀ x ∈ Ioi μ, weight β C Q x = 0 := fun x hx => by
    unfold weight
    rw [nonInvestor_unif_Iic μ hμ0 x (le_of_lt hx)]
    ring
  have hint : ∀ (ρ : Measure ℝ) [IsFiniteMeasure ρ], Integrable (weight β C Q) ρ :=
    fun ρ _ => integrable_bdd ρ _ (measurable_weight β C Q) 1 (fun x => by
      rw [abs_of_nonneg (weight_nonneg β C Q x)]
      exact weight_le_one β C Q x)
  have hsplit : ∀ (ρ : Measure ℝ) [IsProbabilityMeasure ρ],
      ∫ x, weight β C Q x ∂ρ = ∫ x in Iic μ, weight β C Q x ∂ρ := by
    intro ρ _
    rw [← integral_add_compl measurableSet_Iic (hint ρ), compl_Iic,
      setIntegral_eq_zero_of_forall_eq_zero hzero, add_zero]
  have hle : ∫ x in Iic μ, weight β C Q x ∂α ≤ ∫ x in Iic μ, weight β C Q x ∂β :=
    integral_mono_measure (uniform_left μ hμ0 hμ1 ρs hs)
      (ae_of_all _ (weight_nonneg β C Q)) (hint _)
  rw [pmu_identity V α β C Q hQ, hsplit α, hsplit β]
  nlinarith

/-- With a uniform base score, `Δ(0, ·)` is non-decreasing on `Q ≥ 1`. -/
theorem Delta_mono_uniform (V : ℝ) (hV : 0 ≤ V) (μ : ℝ) (hμ0 : 0 < μ) (hμ1 : μ ≤ 1)
    (ρs C : Measure ℝ) [IsProbabilityMeasure ρs] [IsProbabilityMeasure C]
    (hs : ρs (Iio 0) = 0) (Q1 Q2 : ℕ) (h1 : 1 ≤ Q1) (h12 : Q1 ≤ Q2) :
    Delta V (investorLaw μ unif ρs) (nonInvestorLaw μ unif) C Q1 0
      ≤ Delta V (investorLaw μ unif ρs) (nonInvestorLaw μ unif) C Q2 0 := by
  induction Q2, h12 using Nat.le_induction with
  | base => exact le_refl _
  | succ k hk ih =>
      have := pmu_step_nonneg_uniform V hV μ hμ0 hμ1 ρs C hs k (le_trans h1 hk)
      unfold stepQ at this
      linarith

end Uniform

section Bernoulli

/-- An investment that fails (`s = 0`) with probability `p` and succeeds (`s = 1`) otherwise. -/
noncomputable def bern (p : ℝ) : Measure ℝ :=
  ENNReal.ofReal p • Measure.dirac 0 + ENNReal.ofReal (1 - p) • Measure.dirac 1

variable (p : ℝ) [hp : Fact (0 ≤ p ∧ p ≤ 1)]

instance bern_isProb : IsProbabilityMeasure (bern p) := ⟨by
  have h0 := hp.out.1
  have h1 := hp.out.2
  simp only [bern, Measure.add_apply, Measure.smul_apply, measure_univ, smul_eq_mul, mul_one]
  rw [← ENNReal.ofReal_add h0 (by linarith)]
  norm_num⟩

theorem bern_Iio : bern p (Iio 0) = 0 := by
  simp [bern, Measure.add_apply, Measure.smul_apply,
    Measure.dirac_apply' _ measurableSet_Iio]

/-- Below `μ`, the investor's law is exactly `p` times the non-investor's, because a successful
    investment lifts the score to at least `1 − μ ≥ μ`. -/
theorem bern_restrict (μ : ℝ) (hμ0 : 0 < μ) (hμh : μ ≤ 1 / 2) :
    (investorLaw μ unif (bern p)).restrict (Iic μ)
      = ENNReal.ofReal p • (nonInvestorLaw μ unif).restrict (Iic μ) := by
  ext A hA
  have hB : MeasurableSet (A ∩ Iic μ) := hA.inter measurableSet_Iic
  rw [Measure.restrict_apply hA, Measure.smul_apply, Measure.restrict_apply hA, smul_eq_mul,
    investorLaw, Measure.map_apply (measurable_investorScore μ) hB,
    Measure.prod_apply_symm ((measurable_investorScore μ) hB), bern, lintegral_add_measure,
    lintegral_smul_measure, lintegral_smul_measure, lintegral_dirac, lintegral_dirac,
    nonInvestorLaw, Measure.map_apply (measurable_nonInvestorScore μ) hB]
  have h1 : unif ((fun x => (x, (0 : ℝ))) ⁻¹'
      ((fun q : ℝ × ℝ => μ * q.1 + (1 - μ) * q.2) ⁻¹' (A ∩ Iic μ)))
      = unif ((fun r => μ * r) ⁻¹' (A ∩ Iic μ)) := by
    congr 1
    ext x
    simp
  have h2 : unif ((fun x => (x, (1 : ℝ))) ⁻¹'
      ((fun q : ℝ × ℝ => μ * q.1 + (1 - μ) * q.2) ⁻¹' (A ∩ Iic μ))) = 0 := by
    unfold unif
    rw [Measure.restrict_apply' measurableSet_Icc]
    refine measure_mono_null (fun r hr => ?_) (measure_singleton (0 : ℝ))
    simp only [mem_inter_iff, mem_preimage, mem_Iic, mem_Icc] at hr
    obtain ⟨⟨_, h⟩, h0, _⟩ := hr
    have : μ * r ≤ 0 := by nlinarith
    have hr0 : r = 0 := le_antisymm (nonpos_of_mul_nonpos_right this hμ0) h0
    exact hr0
  rw [h1, h2, smul_zero, add_zero, smul_eq_mul]

/-- An investor scores above every possible non-investor score exactly when the investment
    succeeds, with probability `1 − p`. -/
theorem bern_investor_above (μ : ℝ) (hμ0 : 0 < μ) (hμh : μ ≤ 1 / 2) :
    (investorLaw μ unif (bern p)).real (Ioi μ) = 1 - p := by
  have h1 : investorLaw μ unif (bern p) (Iic μ) = ENNReal.ofReal p := by
    have h := congrArg (fun ν : Measure ℝ => ν univ) (bern_restrict p μ hμ0 hμh)
    simp only [Measure.restrict_apply_univ, Measure.smul_apply, smul_eq_mul] at h
    rw [h]
    have hβ : nonInvestorLaw μ unif (Iic μ) = 1 := by
      rw [← compl_Ioi, prob_compl_eq_one_sub measurableSet_Ioi, nonInvestor_unif_Ioi μ hμ0,
        tsub_zero]
    rw [hβ, mul_one]
  rw [measureReal_def, ← compl_Iic, prob_compl_eq_one_sub measurableSet_Iic, h1,
    ENNReal.toReal_sub_of_le (ENNReal.ofReal_le_one.mpr hp.out.2) ENNReal.one_ne_top,
    ENNReal.toReal_one, ENNReal.toReal_ofReal hp.out.1]

/-- **The success-or-failure family in closed form.** For `Q ≥ 1`,
    `Δ(0, Q) = V ((1 − p²)/2 − p(1 − p)/(Q + 1))`, with the incumbent investing. -/
theorem bern_Delta (μ : ℝ) (hμ0 : 0 < μ) (hμh : μ ≤ 1 / 2) (V : ℝ) (n : ℕ) :
    Delta V (investorLaw μ unif (bern p)) (nonInvestorLaw μ unif) (investorLaw μ unif (bern p))
      (n + 1) 0
      = V * ((1 - p ^ 2) / 2 - p * (1 - p) / ((n : ℝ) + 2)) := by
  have hp0 := hp.out.1
  set α := investorLaw μ unif (bern p)
  set β := nonInvestorLaw μ unif
  haveI : NoAtoms α := investorLaw_noAtoms' μ hμ0.ne' unif (bern p)
  haveI : NoAtoms β := nonInvestorLaw_noAtoms' μ hμ0.ne' unif
  have hres := bern_restrict p μ hμ0 hμh
  have hβae : ∀ᵐ x ∂β, x ∈ Iic μ := by
    rw [ae_iff]
    have e : {x : ℝ | ¬ x ∈ Iic μ} = Ioi μ := by ext x; simp
    rw [e]
    exact nonInvestor_unif_Ioi μ hμ0
  have hβres : β.restrict (Iic μ) = β := Measure.restrict_eq_self_of_ae_mem hβae
  -- `F = p G` below `μ`
  have hF : ∀ x ∈ Iic μ, cdf α x = p * cdf β x := by
    intro x hx
    have h1 : α (Iic x) = α.restrict (Iic μ) (Iic x) := by
      rw [Measure.restrict_apply measurableSet_Iic, inter_eq_left.mpr (Iic_subset_Iic.mpr hx)]
    have h2 : β (Iic x) = β.restrict (Iic μ) (Iic x) := by
      rw [Measure.restrict_apply measurableSet_Iic, inter_eq_left.mpr (Iic_subset_Iic.mpr hx)]
    rw [cdf_eq_real, cdf_eq_real, measureReal_def, measureReal_def, h1, hres, Measure.smul_apply,
      smul_eq_mul, ← h2, ENNReal.toReal_mul, ENNReal.toReal_ofReal hp0]
  have hGone : ∀ x ∈ Ioi μ, cdf β x = 1 := fun x hx =>
    nonInvestor_unif_Iic μ hμ0 x (le_of_lt hx)
  -- integrability of the bounded integrands
  have hbd : ∀ (f : ℝ → ℝ), Measurable f → (∀ x, |f x| ≤ 1) →
      ∀ (ρ : Measure ℝ) [IsFiniteMeasure ρ], Integrable f ρ :=
    fun f hf hb ρ _ => integrable_bdd ρ f hf 1 hb
  set g : ℝ → ℝ := fun x => cdf α x * cdf β x ^ n with hg
  have hgm : Measurable g := (cdf α).mono.measurable.mul ((cdf β).mono.measurable.pow_const n)
  have hgb : ∀ x, |g x| ≤ 1 := by
    intro x
    simp only [hg]
    rw [abs_of_nonneg (mul_nonneg (cdf_nonneg α x) (pow_nonneg (cdf_nonneg β x) n))]
    exact mul_le_one₀ (cdf_le_one α x) (pow_nonneg (cdf_nonneg β x) n)
      (pow_le_one₀ (cdf_nonneg β x) (cdf_le_one β x))
  -- the β-integrals
  have hI1 : ∫ x, g x ∂β = p / ((n : ℝ) + 2) := by
    have e : ∀ᵐ x ∂β, g x = p * cdf β x ^ (n + 1) := hβae.mono (fun x hx => by
      simp only [hg]
      rw [hF x hx, pow_succ]
      ring)
    rw [integral_congr_ae e, integral_const_mul, integral_cdf_pow β (n + 1)]
    push_cast
    ring
  have hJ1 : ∫ x, cdf α x ∂β = p / 2 := by
    have e : ∀ᵐ x ∂β, cdf α x = p * cdf β x ^ 1 := hβae.mono (fun x hx => by
      rw [hF x hx, pow_one])
    rw [integral_congr_ae e, integral_const_mul, integral_cdf_pow β 1]
    norm_num
    ring
  -- splitting the α-integrals at `μ`
  have hsplitα : ∀ f : ℝ → ℝ, Integrable f α →
      ∫ x, f x ∂α = p * (∫ x, f x ∂β) + ∫ x in Ioi μ, f x ∂α := by
    intro f hf
    rw [← integral_add_compl measurableSet_Iic hf, compl_Iic, hres, integral_smul_measure,
      hβres, ENNReal.toReal_ofReal hp0, smul_eq_mul]
  have hI2 : ∫ x, g x ∂α = p * (p / ((n : ℝ) + 2)) + (1 / 2 - p * (p / 2)) := by
    rw [hsplitα g (hbd g hgm hgb α), hI1]
    congr 1
    have e1 : ∫ x in Ioi μ, g x ∂α = ∫ x in Ioi μ, cdf α x ∂α := by
      refine setIntegral_congr_fun measurableSet_Ioi (fun x hx => ?_)
      simp only [hg]
      rw [hGone x hx, one_pow, mul_one]
    have e2 := hsplitα (fun x => cdf α x) (integrable_cdf α α)
    have e3 : ∫ x, cdf α x ∂α = 1 / 2 := by
      have := integral_cdf_pow α 1
      simp only [pow_one] at this
      rw [this]
      norm_num
    rw [e1]
    rw [e3, hJ1] at e2
    linarith
  unfold Delta
  have ecdf : ∀ x, cdf (rivals α α β 0 (n + 1 - 1 - 0)) x = g x := by
    intro x
    rw [cdf_rivals, show n + 1 - 1 - 0 = n by omega]
    simp only [hg, pow_zero, mul_one]
  simp_rw [ecdf]
  rw [hI2, hI1]
  ring

/-- **R2 refuted.** In this family the first entrant's gain rises strictly with `Q`, at every
    `Q`, for every `0 < μ ≤ 1/2` and every failure probability `0 < p < 1`. -/
theorem bern_Delta_strictMono (μ : ℝ) (hμ0 : 0 < μ) (hμh : μ ≤ 1 / 2) (V : ℝ) (hV : 0 < V) (hp1 : 0 < p) (hp2 : p < 1) (n : ℕ) :
    Delta V (investorLaw μ unif (bern p)) (nonInvestorLaw μ unif) (investorLaw μ unif (bern p))
      (n + 1) 0
      < Delta V (investorLaw μ unif (bern p)) (nonInvestorLaw μ unif)
          (investorLaw μ unif (bern p)) (n + 2) 0 := by
  rw [bern_Delta p μ hμ0 hμh V n, show n + 2 = (n + 1) + 1 by ring, bern_Delta p μ hμ0 hμh V (n + 1)]
  push_cast
  have hpq : 0 < p * (1 - p) := mul_pos hp1 (by linarith)
  have hlt : p * (1 - p) / ((n : ℝ) + 1 + 2) < p * (1 - p) / ((n : ℝ) + 2) :=
    div_lt_div_of_pos_left hpq (by positivity) (by linarith)
  nlinarith

/-- **R1 refuted.** In this family the first entrant's gain never falls below `V (1 − p)/2`,
    so a challenger whose cost is at most that enters at every `Q`. -/
theorem bern_Delta_lower (μ : ℝ) (hμ0 : 0 < μ) (hμh : μ ≤ 1 / 2) (V : ℝ) (hV : 0 ≤ V) (n : ℕ) :
    V * ((1 - p) / 2) ≤ Delta V (investorLaw μ unif (bern p)) (nonInvestorLaw μ unif)
      (investorLaw μ unif (bern p)) (n + 1) 0 := by
  rw [bern_Delta p μ hμ0 hμh V n]
  have hp0 := hp.out.1
  have hp1 := hp.out.2
  have hpq : 0 ≤ p * (1 - p) := mul_nonneg hp0 (by linarith)
  have hle : p * (1 - p) / ((n : ℝ) + 2) ≤ p * (1 - p) / 2 :=
    div_le_div_of_nonneg_left hpq (by norm_num) (by linarith [Nat.cast_nonneg (α := ℝ) n])
  apply mul_le_mul_of_nonneg_left _ hV
  nlinarith

/-- **R1, the limit.** The first entrant's gain converges to `V (1 − p²)/2` as `Q → ∞`. -/
theorem bern_Delta_limit (μ : ℝ) (hμ0 : 0 < μ) (hμh : μ ≤ 1 / 2) (V : ℝ) :
    Tendsto (fun n : ℕ => Delta V (investorLaw μ unif (bern p)) (nonInvestorLaw μ unif)
      (investorLaw μ unif (bern p)) (n + 1) 0) atTop (𝓝 (V * ((1 - p ^ 2) / 2))) := by
  have h : Tendsto (fun n : ℕ => p * (1 - p) / ((n : ℝ) + 2)) atTop (𝓝 0) := by
    have := (tendsto_one_div_add_atTop_nhds_zero_nat).comp (tendsto_add_atTop_nat 1)
    have h2 : Tendsto (fun n : ℕ => 1 / ((n : ℝ) + 2)) atTop (𝓝 0) := by
      refine this.congr (fun n => ?_)
      simp only [Function.comp_apply]
      push_cast
      ring
    simpa using h2.const_mul (p * (1 - p))
  have h3 := (tendsto_const_nhds (x := (1 - p ^ 2) / 2)).sub h
  rw [sub_zero] at h3
  refine (h3.const_mul V).congr (fun n => ?_)
  rw [bern_Delta p μ hμ0 hμh V n]

end Bernoulli

end EntryContestRefute
