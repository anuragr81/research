import Mathlib
import ShiftClass

open MeasureTheory ProbabilityTheory Set Filter Topology
open scoped ENNReal

set_option linter.unusedSectionVars false

namespace EntryContestMustFall

open EntryContestModel EntryContestP1P2 EntryContestP6P7 EntryContestPMU EntryContestPMUWitness
  EntryContestRefute EntryContestFall EntryContestShift

/-! **When the first entrant's gain must fall in `Q`.** With a single-step lift `t`, the step
`D(Q)` has the sign of `∫ W_Q dG − ∫ W_Q dG_t`, where `G_t` is the non-investor's law shifted up
by `t`. Below `t` the shifted law has no mass, and `G`'s part there is at most `G(t)^(Q−1)` times
a constant. Above some `a > t` the shift's extra mass counts with weight at least `G(a)^(Q−1)`.
When `G(t) < G(a)` and the shift strictly adds mass on an interval above `a`, the ratio
`(G(t)/G(a))^(Q−1)` vanishes and `D(Q) < 0` for every `Q` past an explicit `Q0`. -/

section General

variable (β βt C : Measure ℝ) [IsProbabilityMeasure β] [IsProbabilityMeasure βt]
  [IsProbabilityMeasure C]

theorem restrict_le_sub (t : ℝ) (h : β.restrict (Ioi t) ≤ βt.restrict (Ioi t)) (s : Set ℝ)
    (hs : s ⊆ Ioi t) : β.restrict s ≤ βt.restrict s := by
  rw [← Measure.restrict_restrict_of_subset hs,
    ← Measure.restrict_restrict_of_subset (μ := βt) hs]
  exact Measure.restrict_mono subset_rfl h

theorem weight_int (ρ : Measure ℝ) [IsFiniteMeasure ρ] (Q : ℕ) : Integrable (weight β C Q) ρ :=
  integrable_bdd ρ _ (measurable_weight β C Q) 1 (fun x => by
    rw [abs_of_nonneg (weight_nonneg β C Q x)]
    exact weight_le_one β C Q x)

theorem split3 (ρ : Measure ℝ) [IsFiniteMeasure ρ] (f : ℝ → ℝ) (hf : Integrable f ρ) (t a : ℝ)
    (hta : t ≤ a) :
    ∫ x, f x ∂ρ = (∫ x in Iic t, f x ∂ρ) + (∫ x in Ioc t a, f x ∂ρ) + ∫ x in Ioi a, f x ∂ρ := by
  rw [← integral_add_compl measurableSet_Iic hf, compl_Iic, ← Ioc_union_Ioi_eq_Ioi hta,
    setIntegral_union Ioc_disjoint_Ioi_same measurableSet_Ioi hf.integrableOn hf.integrableOn]
  ring

/-- **The must-fall bound.** -/
theorem must_fall_general (t a b : ℝ) (hta : t < a) (hab : a < b) (h0 : βt (Iic t) = 0)
    (hmono : β.restrict (Ioi t) ≤ βt.restrict (Ioi t)) (hgain : β (Ioc a b) < βt (Ioc a b))
    (hC : 0 < cdf C a) (hGb : cdf β b < 1) (hGt : cdf β t < cdf β a) :
    ∃ Q0 : ℕ, 1 ≤ Q0 ∧ ∀ Q, Q0 ≤ Q → (∫ x, weight β C Q x ∂β) < ∫ x, weight β C Q x ∂βt := by
  set W1 : ℝ → ℝ := weight β C 1 with hW1
  have hW1nn : ∀ x, 0 ≤ W1 x := weight_nonneg β C 1
  have hWQ : ∀ Q, 1 ≤ Q → ∀ x, weight β C Q x = cdf β x ^ (Q - 1) * W1 x :=
    fun Q hQ x => weight_shift β C 1 Q le_rfl hQ x
  have hI : ∀ (ρ : Measure ℝ) [IsFiniteMeasure ρ] (Q : ℕ), Integrable (weight β C Q) ρ :=
    fun ρ _ Q => weight_int β C ρ Q
  -- the strict gain above `a`
  set η : ℝ := cdf C a * (1 - cdf β b) with hη
  have hηpos : 0 < η := mul_pos hC (by linarith)
  have hIoc : ∀ x ∈ Ioc a b, η ≤ W1 x := by
    intro x hx
    simp only [hW1, weight, Nat.sub_self, pow_zero, mul_one]
    have h1 : cdf C a ≤ cdf C x := (cdf C).mono hx.1.le
    have h2 : cdf β x ≤ cdf β b := (cdf β).mono hx.2
    have h3 : 0 ≤ 1 - cdf β b := by linarith
    calc η = cdf C a * (1 - cdf β b) := rfl
      _ ≤ cdf C x * (1 - cdf β x) := mul_le_mul h1 (by linarith) h3 (cdf_nonneg C x)
  have hsubIoc : Ioc a b ⊆ Ioi t := fun x hx => lt_trans hta hx.1
  have hsubIoi : ∀ c, t ≤ c → Ioi c ⊆ Ioi t := fun c hc x hx => lt_of_le_of_lt hc hx
  have hA1 : η * (βt.real (Ioc a b) - β.real (Ioc a b))
      ≤ (∫ x in Ioc a b, W1 x ∂βt) - ∫ x in Ioc a b, W1 x ∂β := by
    have hm := integral_mono_measure (restrict_le_sub β βt t hmono (Ioc a b) hsubIoc)
      (ae_restrict_of_forall_mem measurableSet_Ioc (fun x hx => by
        show (0 : ℝ) ≤ W1 x - η
        linarith [hIoc x hx]))
      (((hI βt 1).sub (integrable_const η)).integrableOn (s := Ioc a b))
    simp only [Pi.sub_apply] at hm
    rw [integral_sub ((hI β 1).integrableOn) (integrable_const η),
      integral_sub ((hI βt 1).integrableOn) (integrable_const η), setIntegral_const,
      setIntegral_const, smul_eq_mul, smul_eq_mul] at hm
    nlinarith
  have hgainR : β.real (Ioc a b) < βt.real (Ioc a b) := by
    rw [measureReal_def, measureReal_def]
    exact (ENNReal.toReal_lt_toReal (measure_ne_top _ _) (measure_ne_top _ _)).mpr hgain
  have hA2 : ∫ x in Ioi b, W1 x ∂β ≤ ∫ x in Ioi b, W1 x ∂βt :=
    integral_mono_measure (restrict_le_sub β βt t hmono (Ioi b) (hsubIoi b (by linarith)))
      (ae_of_all _ hW1nn) ((hI βt 1).integrableOn)
  set A : ℝ := (∫ x in Ioi a, W1 x ∂βt) - ∫ x in Ioi a, W1 x ∂β with hA
  have hApos : 0 < A := by
    have hs : ∀ (ρ : Measure ℝ) [IsFiniteMeasure ρ],
        ∫ x in Ioi a, W1 x ∂ρ = (∫ x in Ioc a b, W1 x ∂ρ) + ∫ x in Ioi b, W1 x ∂ρ := by
      intro ρ _
      rw [← Ioc_union_Ioi_eq_Ioi hab.le, setIntegral_union Ioc_disjoint_Ioi_same
        measurableSet_Ioi (hI ρ 1).integrableOn (hI ρ 1).integrableOn]
    rw [hA, hs βt, hs β]
    nlinarith
  set B : ℝ := ∫ x in Iic t, W1 x ∂β with hB
  have hBnn : 0 ≤ B := setIntegral_nonneg measurableSet_Iic (fun x _ => hW1nn x)
  -- the lower bound at every `Q ≥ 1`
  have hbound : ∀ Q, 1 ≤ Q →
      cdf β a ^ (Q - 1) * A - cdf β t ^ (Q - 1) * B
        ≤ (∫ x, weight β C Q x ∂βt) - ∫ x, weight β C Q x ∂β := by
    intro Q hQ
    have e : ∀ x, weight β C Q x = cdf β x ^ (Q - 1) * W1 x := hWQ Q hQ
    have hβt0 : βt.restrict (Iic t) = 0 := Measure.restrict_eq_zero.mpr h0
    have i1 : ∫ x in Iic t, weight β C Q x ∂β ≤ cdf β t ^ (Q - 1) * B := by
      rw [hB, ← integral_const_mul]
      refine integral_mono_ae (hI _ Q) ((hI β 1).integrableOn.const_mul _)
        (ae_restrict_of_forall_mem measurableSet_Iic (fun x hx => ?_))
      rw [e]
      exact mul_le_mul_of_nonneg_right
        (pow_le_pow_left₀ (cdf_nonneg β x) ((cdf β).mono hx) _) (hW1nn x)
    have i2 : ∫ x in Ioc t a, weight β C Q x ∂β ≤ ∫ x in Ioc t a, weight β C Q x ∂βt :=
      integral_mono_measure (restrict_le_sub β βt t hmono (Ioc t a) (fun x hx => hx.1))
        (ae_of_all _ (weight_nonneg β C Q)) ((hI βt Q).integrableOn)
    have i3 : cdf β a ^ (Q - 1) * A
        ≤ (∫ x in Ioi a, weight β C Q x ∂βt) - ∫ x in Ioi a, weight β C Q x ∂β := by
      set h : ℝ → ℝ := fun x => (cdf β x ^ (Q - 1) - cdf β a ^ (Q - 1)) * W1 x with hh
      have hhi : ∀ (ρ : Measure ℝ) [IsFiniteMeasure ρ], Integrable h ρ := by
        intro ρ _
        refine integrable_bdd ρ h ((((cdf β).mono.measurable.pow_const _).sub
          measurable_const).mul (measurable_weight β C 1)) 1 (fun x => ?_)
        simp only [hh]
        rw [abs_mul]
        have a1 := pow_nonneg (cdf_nonneg β x) (Q - 1)
        have a2 : cdf β x ^ (Q - 1) ≤ 1 := pow_le_one₀ (cdf_nonneg β x) (cdf_le_one β x)
        have a3 := pow_nonneg (cdf_nonneg β a) (Q - 1)
        have a4 : cdf β a ^ (Q - 1) ≤ 1 := pow_le_one₀ (cdf_nonneg β a) (cdf_le_one β a)
        have a5 : |W1 x| ≤ 1 := by
          rw [abs_of_nonneg (hW1nn x)]
          exact weight_le_one β C 1 x
        calc |cdf β x ^ (Q - 1) - cdf β a ^ (Q - 1)| * |W1 x| ≤ 1 * 1 := by
              gcongr
              rw [abs_le]
              constructor <;> linarith
          _ = 1 := by ring
      have hm := integral_mono_measure (restrict_le_sub β βt t hmono (Ioi a)
          (hsubIoi a hta.le))
        (ae_restrict_of_forall_mem measurableSet_Ioi (fun x hx => by
          show (0 : ℝ) ≤ h x
          simp only [hh]
          exact mul_nonneg (by
            have := pow_le_pow_left₀ (cdf_nonneg β a) ((cdf β).mono (le_of_lt hx)) (Q - 1)
            linarith) (hW1nn x)))
        ((hhi βt).integrableOn (s := Ioi a))
      have ex : ∀ (ρ : Measure ℝ) [IsFiniteMeasure ρ],
          ∫ x in Ioi a, h x ∂ρ
            = (∫ x in Ioi a, weight β C Q x ∂ρ) - cdf β a ^ (Q - 1) * ∫ x in Ioi a, W1 x ∂ρ := by
        intro ρ _
        rw [← integral_const_mul, ← integral_sub (hI ρ Q).integrableOn
          ((hI ρ 1).integrableOn.const_mul _)]
        refine integral_congr_ae (ae_of_all _ (fun x => ?_))
        simp only [hh]
        rw [e]
        ring
      rw [ex β, ex βt] at hm
      rw [hA]
      nlinarith
    rw [split3 βt _ (hI βt Q) t a hta.le, split3 β _ (hI β Q) t a hta.le, hβt0,
      integral_zero_measure]
    linarith
  -- choosing `Q0`
  have hGa : 0 < cdf β a := lt_of_le_of_lt (cdf_nonneg β t) hGt
  set r : ℝ := cdf β t / cdf β a with hr
  have hr0 : 0 ≤ r := div_nonneg (cdf_nonneg β t) hGa.le
  have hr1 : r < 1 := (div_lt_one hGa).mpr hGt
  obtain ⟨n, hn⟩ := exists_pow_lt_of_lt_one (div_pos hApos (by linarith : (0 : ℝ) < B + 1)) hr1
  refine ⟨n + 1, by omega, fun Q hQ => ?_⟩
  have hQ1 : 1 ≤ Q := by omega
  have hrQ : r ^ (Q - 1) ≤ r ^ n := pow_le_pow_of_le_one hr0 hr1.le (by omega)
  have hGtQ : cdf β t ^ (Q - 1) = r ^ (Q - 1) * cdf β a ^ (Q - 1) := by
    rw [hr, div_pow, div_mul_cancel₀ _ (pow_ne_zero _ hGa.ne')]
  have hnB : r ^ n * (B + 1) < A := by
    rw [lt_div_iff₀ (by linarith)] at hn
    exact hn
  have hkey : r ^ (Q - 1) * B < A := by
    have := mul_le_mul_of_nonneg_right hrQ (by linarith : (0 : ℝ) ≤ B + 1)
    nlinarith [pow_nonneg hr0 (Q - 1)]
  have hpos : 0 < cdf β a ^ (Q - 1) * A - cdf β t ^ (Q - 1) * B := by
    rw [hGtQ]
    have hGaQ := pow_pos hGa (Q - 1)
    nlinarith
  linarith [hbound Q hQ1]

end General

section Model

variable (p μ : ℝ) [hp : Fact (0 ≤ p ∧ p ≤ 1)] (ρr : Measure ℝ) [IsProbabilityMeasure ρr]

/-- Above the lift, the shifted law dominates the base law when the base law is shift-monotone. -/
theorem shift_dominates (hμ : 0 < μ) (hμ1 : μ < 1) (hsm : ShiftMono ρr) :
    (ρr.map (fun r => μ * r)).restrict (Ioi (1 - μ))
      ≤ (ρr.map (fun r => μ * r + (1 - μ))).restrict (Ioi (1 - μ)) := by
  rw [Measure.le_iff]
  intro A hA
  have hB : MeasurableSet (A ∩ Ioi (1 - μ)) := hA.inter measurableSet_Ioi
  rw [Measure.restrict_apply hA, Measure.restrict_apply hA,
    Measure.map_apply (by fun_prop) hB, Measure.map_apply (by fun_prop) hB]
  have hc : 0 < (1 - μ) / μ := div_pos (by linarith) hμ
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

/-- The step in `Q` for a single-step investment, in terms of the shifted law. -/
theorem stepQ_bern (V : ℝ) (C : Measure ℝ) [IsProbabilityMeasure C] (Q : ℕ) (hQ : 1 ≤ Q) :
    stepQ V (investorLaw μ ρr (bern p)) (nonInvestorLaw μ ρr) C Q
      = -V * (1 - p) * ((∫ x, weight (nonInvestorLaw μ ρr) C Q x
            ∂(ρr.map (fun r => μ * r + (1 - μ))))
          - ∫ x, weight (nonInvestorLaw μ ρr) C Q x ∂(nonInvestorLaw μ ρr)) := by
  set W := weight (nonInvestorLaw μ ρr) C Q with hW
  have hWm : Measurable W := measurable_weight _ C Q
  have hWb : ∀ x, |W x| ≤ 1 := fun x => by
    rw [abs_of_nonneg (weight_nonneg _ C Q x)]
    exact weight_le_one _ C Q x
  rw [pmu_identity V _ _ C Q hQ, integral_investorLaw_bern p μ ρr W hWm 1 hWb,
    integral_nonInvestorLaw ρr μ W hWm,
    integral_map (by fun_prop : Measurable (fun r : ℝ => μ * r + (1 - μ))).aemeasurable
      hWm.aestronglyMeasurable]
  ring

/-- **The gain must fall.** For a single-step investment with failure probability `p < 1`, a
    base law with no mass at or below 0 that is shift-monotone, and an interval `(a, b]` above the
    lift on which the shift strictly adds mass, the first entrant's gain falls strictly at every
    `Q` from some `Q0` on. -/
theorem must_fall (V : ℝ) (hV : 0 < V) (hp1 : p < 1) (hμ : 0 < μ) (hμ1 : μ < 1)
    (h0 : ρr (Iic 0) = 0) (hsm : ShiftMono ρr) (C : Measure ℝ) [IsProbabilityMeasure C]
    (a b : ℝ) (hta : 1 - μ < a) (hab : a < b)
    (hgain : nonInvestorLaw μ ρr (Ioc a b) < (ρr.map (fun r => μ * r + (1 - μ))) (Ioc a b))
    (hC : 0 < cdf C a) (hGb : cdf (nonInvestorLaw μ ρr) b < 1)
    (hGt : cdf (nonInvestorLaw μ ρr) (1 - μ) < cdf (nonInvestorLaw μ ρr) a) :
    ∃ Q0 : ℕ, 1 ≤ Q0 ∧ ∀ Q, Q0 ≤ Q →
      stepQ V (investorLaw μ ρr (bern p)) (nonInvestorLaw μ ρr) C Q < 0 := by
  have h0t : (ρr.map (fun r => μ * r + (1 - μ))) (Iic (1 - μ)) = 0 := by
    rw [Measure.map_apply (by fun_prop) measurableSet_Iic]
    refine measure_mono_null (fun r hr => ?_) h0
    simp only [mem_preimage, mem_Iic] at hr
    show r ≤ 0
    nlinarith
  obtain ⟨Q0, hQ0, hQ⟩ := must_fall_general (nonInvestorLaw μ ρr)
    (ρr.map (fun r => μ * r + (1 - μ))) C (1 - μ) a b hta hab h0t
    (shift_dominates μ ρr hμ hμ1 hsm) hgain hC hGb hGt
  refine ⟨Q0, hQ0, fun Q hQQ => ?_⟩
  rw [stepQ_bern p μ ρr V C Q (le_trans hQ0 hQQ)]
  have h := hQ Q hQQ
  have hpos : 0 < V * (1 - p) := mul_pos hV (by linarith)
  nlinarith

end Model

section Instances

theorem measure_Ioc_cdf (ν : Measure ℝ) [IsProbabilityMeasure ν] (a b : ℝ) :
    ν (Ioc a b) = ENNReal.ofReal (cdf ν b - cdf ν a) := by
  conv_lhs => rw [← measure_cdf (μ := ν)]
  exact StieltjesFunction.measure_Ioc _ a b

theorem lawA_vals : cdf lawA (5 / 6) = 35 / 36 ∧ cdf lawA (2 / 3) = 8 / 9
    ∧ cdf lawA (1 / 2) = 3 / 4 ∧ cdf lawA (1 / 3) = 5 / 9 := by
  refine ⟨?_, ?_, ?_, ?_⟩ <;>
  · rw [cdf_lawA_mem _ (by norm_num) (by norm_num)]
    norm_num

/-- **The witness must fall, for every failure probability.** With `r` the smaller of two uniform
    draws and `μ = 3/4`, the first entrant's gain falls strictly at every `Q` from some `Q0` on,
    for every `0 ≤ p < 1`. -/
theorem witness_must_fall (V : ℝ) (hV : 0 < V) (p : ℝ) [Fact (0 ≤ p ∧ p ≤ 1)] (hp1 : p < 1) :
    ∃ Q0 : ℕ, 1 ≤ Q0 ∧ ∀ Q, Q0 ≤ Q → stepQ V (αp p) β0 (αp p) Q < 0 := by
  obtain ⟨v56, v23, v12, v13⟩ := lawA_vals
  have hp0 : 0 ≤ p := (Fact.out : 0 ≤ p ∧ p ≤ 1).1
  have hβ : ∀ x, cdf β0 x = cdf lawA (x / (3 / 4)) := cdf_β0
  have hβt : ∀ x, cdf (lawA.map (fun r => 3 / 4 * r + (1 - 3 / 4))) x
      = cdf lawA ((x - (1 - 3 / 4)) / (3 / 4)) := cdf_map_affine lawA (3 / 4) (by norm_num)
  have hgain : nonInvestorLaw (3 / 4) lawA (Ioc (1 / 2) (5 / 8))
      < (lawA.map (fun r => 3 / 4 * r + (1 - 3 / 4))) (Ioc (1 / 2) (5 / 8)) := by
    have e : nonInvestorLaw (3 / 4) lawA = β0 := rfl
    rw [e, measure_Ioc_cdf, measure_Ioc_cdf, hβ, hβ, hβt, hβt,
      show (5 / 8 : ℝ) / (3 / 4) = 5 / 6 by norm_num, show (1 / 2 : ℝ) / (3 / 4) = 2 / 3 by norm_num,
      show ((5 / 8 : ℝ) - (1 - 3 / 4)) / (3 / 4) = 1 / 2 by norm_num,
      show ((1 / 2 : ℝ) - (1 - 3 / 4)) / (3 / 4) = 1 / 3 by norm_num, v56, v23, v12, v13]
    exact (ENNReal.ofReal_lt_ofReal_iff (by norm_num)).mpr (by norm_num)
  have hC : 0 < cdf (αp p) (1 / 2) := by
    rw [cdf_αp, show (1 / 2 : ℝ) / (3 / 4) = 2 / 3 by norm_num,
      show ((1 / 2 : ℝ) - (1 - 3 / 4)) / (3 / 4) = 1 / 3 by norm_num, v23, v13]
    nlinarith
  have hGb : cdf (nonInvestorLaw (3 / 4) lawA) (5 / 8) < 1 := by
    show cdf β0 (5 / 8) < 1
    rw [hβ, show (5 / 8 : ℝ) / (3 / 4) = 5 / 6 by norm_num, v56]
    norm_num
  have hGt : cdf (nonInvestorLaw (3 / 4) lawA) (1 - 3 / 4)
      < cdf (nonInvestorLaw (3 / 4) lawA) (1 / 2) := by
    show cdf β0 (1 - 3 / 4) < cdf β0 (1 / 2)
    rw [hβ, hβ, show (1 - 3 / 4 : ℝ) / (3 / 4) = 1 / 3 by norm_num,
      show (1 / 2 : ℝ) / (3 / 4) = 2 / 3 by norm_num, v13, v23]
    norm_num
  exact must_fall p (3 / 4) lawA V hV hp1 (by norm_num) (by norm_num) lawA_Iic_zero
    lawA_shiftMono (αp p) (1 / 2) (5 / 8) (by norm_num) (by norm_num) hgain hC hGb hGt

/-- **The condition separates the cases.** For a uniform base score no interval meets the gain
    condition together with the others, because with a uniform base score the gain never falls. -/
theorem uniform_fails_gain (V : ℝ) (hV : 0 < V) (p μ : ℝ) [Fact (0 ≤ p ∧ p ≤ 1)] (hp1 : p < 1)
    (hμ : 0 < μ) (hμ1 : μ < 1) (C : Measure ℝ) [IsProbabilityMeasure C] (a b : ℝ)
    (hta : 1 - μ < a) (hab : a < b) (hC : 0 < cdf C a)
    (hGb : cdf (nonInvestorLaw μ unif) b < 1)
    (hGt : cdf (nonInvestorLaw μ unif) (1 - μ) < cdf (nonInvestorLaw μ unif) a)
    (hsm : ShiftMono unif) :
    ¬ nonInvestorLaw μ unif (Ioc a b) < (unif.map (fun r => μ * r + (1 - μ))) (Ioc a b) := by
  intro hgain
  have h0 : unif (Iic 0) = 0 := by
    have h := cdf_unif_of_mem 0 le_rfl zero_le_one
    rw [cdf_eq_real, measureReal_def] at h
    exact ((ENNReal.toReal_eq_zero_iff _).mp h).resolve_right (measure_ne_top _ _)
  obtain ⟨Q0, hQ0, hQ⟩ := must_fall p μ unif V hV hp1 hμ hμ1 h0 hsm C a b hta hab hgain hC hGb hGt
  have hneg := hQ Q0 le_rfl
  have hnn := pmu_step_nonneg_uniform V hV.le μ hμ hμ1.le (bern p) C (bern_Iio p) Q0 hQ0
  linarith

end Instances

end EntryContestMustFall
