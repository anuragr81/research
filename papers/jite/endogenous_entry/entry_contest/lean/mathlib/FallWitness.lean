import Mathlib
import Refutations

open MeasureTheory ProbabilityTheory Set Filter Topology
open scoped ENNReal

set_option linter.unusedSectionVars false

namespace EntryContestFall

open EntryContestModel EntryContestP1P2 EntryContestP6P7 EntryContestPMU EntryContestPMUWitness
  EntryContestRefute

/-! **The first entrant's gain rises and then falls in `Q`, inside the model.** The base score
`r` is the smaller of two uniform draws, with density `2(1 − r)`. The investment lifts the score
by `1 − μ` with probability `1 − p` and by nothing otherwise, `μ = 3/4`, and the incumbent
invests. At `p = 1/2` the gain rises from `Q = 1` to `Q = 2` and falls from `Q = 2` to `Q = 3`.
At `p = 1/4` it falls from `Q = 1`. -/

section Decomposition

theorem prod_smul_right' (μ ν : Measure ℝ) [SFinite μ] [SFinite ν] (c : ℝ≥0∞) :
    μ.prod (c • ν) = c • μ.prod ν := by
  calc μ.prod (c • ν) = Measure.map Prod.swap ((c • ν).prod μ) := by rw [Measure.prod_swap]
    _ = Measure.map Prod.swap (c • ν.prod μ) := by rw [Measure.prod_smul_left]
    _ = c • Measure.map Prod.swap (ν.prod μ) := by rw [Measure.map_smul]
    _ = c • μ.prod ν := by rw [Measure.prod_swap]

instance map_mul_isProb (ρ : Measure ℝ) [IsProbabilityMeasure ρ] (μ : ℝ) :
    IsProbabilityMeasure (ρ.map (fun r => μ * r)) :=
  Measure.isProbabilityMeasure_map (by fun_prop : Measurable (fun r : ℝ => μ * r)).aemeasurable

instance map_affine_isProb (ρ : Measure ℝ) [IsProbabilityMeasure ρ] (μ : ℝ) :
    IsProbabilityMeasure (ρ.map (fun r => μ * r + (1 - μ))) :=
  Measure.isProbabilityMeasure_map
    (by fun_prop : Measurable (fun r : ℝ => μ * r + (1 - μ))).aemeasurable

theorem cdf_map_mul (ρ : Measure ℝ) [IsProbabilityMeasure ρ] (μ : ℝ) (hμ : 0 < μ) (x : ℝ) :
    cdf (ρ.map (fun r => μ * r)) x = cdf ρ (x / μ) := by
  rw [cdf_eq_real, cdf_eq_real, measureReal_def, measureReal_def,
    Measure.map_apply (by fun_prop) measurableSet_Iic]
  congr 2
  ext r
  simp only [mem_preimage, mem_Iic]
  rw [le_div_iff₀ hμ, mul_comm r μ]

theorem cdf_map_affine (ρ : Measure ℝ) [IsProbabilityMeasure ρ] (μ : ℝ) (hμ : 0 < μ) (x : ℝ) :
    cdf (ρ.map (fun r => μ * r + (1 - μ))) x = cdf ρ ((x - (1 - μ)) / μ) := by
  rw [cdf_eq_real, cdf_eq_real, measureReal_def, measureReal_def,
    Measure.map_apply (by fun_prop) measurableSet_Iic]
  congr 2
  ext r
  simp only [mem_preimage, mem_Iic]
  rw [le_div_iff₀ hμ, mul_comm r μ]
  constructor <;> intro h <;> linarith

variable (p μ : ℝ) [hp : Fact (0 ≤ p ∧ p ≤ 1)] (ρr : Measure ℝ) [IsProbabilityMeasure ρr]

/-- The investor's law is a mixture of the base law scaled by `μ` and the same law shifted by
    `1 − μ`. -/
theorem investorLaw_bern :
    investorLaw μ ρr (bern p)
      = ENNReal.ofReal p • ρr.map (fun r => μ * r)
        + ENNReal.ofReal (1 - p) • ρr.map (fun r => μ * r + (1 - μ)) := by
  have e0 : ((fun q : ℝ × ℝ => μ * q.1 + (1 - μ) * q.2) ∘ fun r : ℝ => (r, (0 : ℝ)))
      = fun r => μ * r := by funext r; simp
  have e1 : ((fun q : ℝ × ℝ => μ * q.1 + (1 - μ) * q.2) ∘ fun r : ℝ => (r, (1 : ℝ)))
      = fun r => μ * r + (1 - μ) := by funext r; simp
  unfold investorLaw bern
  rw [Measure.prod_add, prod_smul_right', prod_smul_right', Measure.prod_dirac, Measure.prod_dirac,
    Measure.map_add _ _ (measurable_investorScore μ), Measure.map_smul, Measure.map_smul,
    Measure.map_map (measurable_investorScore μ) measurable_prodMk_right,
    Measure.map_map (measurable_investorScore μ) measurable_prodMk_right, e0, e1]

theorem cdf_investorLaw_bern (hμ : 0 < μ) (x : ℝ) :
    cdf (investorLaw μ ρr (bern p)) x
      = p * cdf ρr (x / μ) + (1 - p) * cdf ρr ((x - (1 - μ)) / μ) := by
  obtain ⟨hp0, hp1⟩ := hp.out
  rw [← cdf_map_mul ρr μ hμ x, ← cdf_map_affine ρr μ hμ x]
  simp only [cdf_eq_real, measureReal_def]
  rw [investorLaw_bern p μ ρr, Measure.add_apply, Measure.smul_apply, Measure.smul_apply,
    smul_eq_mul, smul_eq_mul,
    ENNReal.toReal_add (ENNReal.mul_ne_top ENNReal.ofReal_ne_top (measure_ne_top _ _))
      (ENNReal.mul_ne_top ENNReal.ofReal_ne_top (measure_ne_top _ _)),
    ENNReal.toReal_mul, ENNReal.toReal_mul, ENNReal.toReal_ofReal hp0,
    ENNReal.toReal_ofReal (by linarith)]

theorem integral_investorLaw_bern (h : ℝ → ℝ) (hh : Measurable h) (B : ℝ)
    (hB : ∀ x, |h x| ≤ B) :
    ∫ x, h x ∂(investorLaw μ ρr (bern p))
      = p * (∫ r, h (μ * r) ∂ρr) + (1 - p) * ∫ r, h (μ * r + (1 - μ)) ∂ρr := by
  obtain ⟨hp0, hp1⟩ := hp.out
  have i1 : Integrable h (ρr.map (fun r => μ * r)) := integrable_bdd _ h hh B hB
  have i2 : Integrable h (ρr.map (fun r => μ * r + (1 - μ))) := integrable_bdd _ h hh B hB
  rw [investorLaw_bern p μ ρr,
    integral_add_measure (i1.smul_measure ENNReal.ofReal_ne_top)
      (i2.smul_measure ENNReal.ofReal_ne_top),
    integral_smul_measure, integral_smul_measure, ENNReal.toReal_ofReal hp0,
    ENNReal.toReal_ofReal (by linarith), smul_eq_mul, smul_eq_mul,
    integral_map (by fun_prop : Measurable (fun r : ℝ => μ * r)).aemeasurable
      hh.aestronglyMeasurable,
    integral_map (by fun_prop : Measurable (fun r : ℝ => μ * r + (1 - μ))).aemeasurable
      hh.aestronglyMeasurable]

end Decomposition

section LawA

theorem cdf_unif_of_nonpos (x : ℝ) (h : x ≤ 0) : cdf unif x = 0 := by
  rcases lt_or_eq_of_le h with h' | h'
  · exact cdf_unif_of_neg x h'
  · rw [h', cdf_unif_of_mem 0 le_rfl zero_le_one]

theorem cdf_lawA_of_nonpos (z : ℝ) (h : z ≤ 0) : cdf lawA z = 0 := by
  rw [cdf_lawA, cdf_maxTwo, cdf_unif_of_ge (1 - z) (by linarith)]
  ring

theorem cdf_lawA_of_ge (z : ℝ) (h : 1 ≤ z) : cdf lawA z = 1 := by
  rw [cdf_lawA, cdf_maxTwo, cdf_unif_of_nonpos (1 - z) (by linarith)]
  ring

/-- Integration against the base law is integration against the density `2y` after the
    reflection `r = 1 − y`. -/
theorem integral_lawA_interval (g : ℝ → ℝ) (hg : Measurable g) (B : ℝ) (hB : ∀ x, |g x| ≤ B) :
    ∫ r, g r ∂lawA = 2 * ∫ y in (0 : ℝ)..1, g (1 - y) * y := by
  rw [integral_lawA g hg B hB]
  congr 1
  have e : ∀ᵐ y ∂unif, g (1 - y) * cdf unif y = g (1 - y) * y :=
    unif_ae_mem.mono (fun y hy => by rw [cdf_unif_of_mem y hy.1 hy.2])
  rw [integral_congr_ae e, intervalIntegral.integral_of_le zero_le_one]
  unfold unif
  exact integral_Icc_eq_integral_Ioc

theorem intervalIntegrable_mul_id (f : ℝ → ℝ) (hf : Measurable f) (hb : ∀ x, |f x| ≤ 1)
    (a b : ℝ) : IntervalIntegrable (fun y => f y * y) volume a b :=
  (continuous_abs.intervalIntegrable a b).mono_fun' (hf.mul measurable_id).aestronglyMeasurable
    (ae_of_all _ (fun y => by
      show ‖f y * y‖ ≤ |y|
      rw [Real.norm_eq_abs, abs_mul]
      exact mul_le_of_le_one_left (abs_nonneg y) (hb y)))

/-- The integral of a polynomial of degree at most 7, by its antiderivative. -/
theorem integral_poly8 (c0 c1 c2 c3 c4 c5 c6 c7 a b : ℝ) :
    ∫ y in a..b, (c0 + c1 * y + c2 * y ^ 2 + c3 * y ^ 3 + c4 * y ^ 4 + c5 * y ^ 5 + c6 * y ^ 6
      + c7 * y ^ 7)
      = (c0 * b + c1 / 2 * b ^ 2 + c2 / 3 * b ^ 3 + c3 / 4 * b ^ 4 + c4 / 5 * b ^ 5
          + c5 / 6 * b ^ 6 + c6 / 7 * b ^ 7 + c7 / 8 * b ^ 8)
        - (c0 * a + c1 / 2 * a ^ 2 + c2 / 3 * a ^ 3 + c3 / 4 * a ^ 4 + c4 / 5 * a ^ 5
          + c5 / 6 * a ^ 6 + c6 / 7 * a ^ 7 + c7 / 8 * a ^ 8) := by
  have hd : ∀ y : ℝ, HasDerivAt (fun y : ℝ => c0 * y + c1 / 2 * y ^ 2 + c2 / 3 * y ^ 3
      + c3 / 4 * y ^ 4 + c4 / 5 * y ^ 5 + c5 / 6 * y ^ 6 + c6 / 7 * y ^ 7 + c7 / 8 * y ^ 8)
      (c0 + c1 * y + c2 * y ^ 2 + c3 * y ^ 3 + c4 * y ^ 4 + c5 * y ^ 5 + c6 * y ^ 6
        + c7 * y ^ 7) y := by
    intro y
    have h2 : HasDerivAt (fun y : ℝ => y ^ 2) (2 * y) y := by simpa using hasDerivAt_pow 2 y
    have h3 : HasDerivAt (fun y : ℝ => y ^ 3) (3 * y ^ 2) y := by simpa using hasDerivAt_pow 3 y
    have h4 : HasDerivAt (fun y : ℝ => y ^ 4) (4 * y ^ 3) y := by simpa using hasDerivAt_pow 4 y
    have h5 : HasDerivAt (fun y : ℝ => y ^ 5) (5 * y ^ 4) y := by simpa using hasDerivAt_pow 5 y
    have h6 : HasDerivAt (fun y : ℝ => y ^ 6) (6 * y ^ 5) y := by simpa using hasDerivAt_pow 6 y
    have h7 : HasDerivAt (fun y : ℝ => y ^ 7) (7 * y ^ 6) y := by simpa using hasDerivAt_pow 7 y
    have h8 : HasDerivAt (fun y : ℝ => y ^ 8) (8 * y ^ 7) y := by simpa using hasDerivAt_pow 8 y
    have h := ((((((((hasDerivAt_id' (x := y)).const_mul c0).add (h2.const_mul (c1 / 2))).add
      (h3.const_mul (c2 / 3))).add (h4.const_mul (c3 / 4))).add (h5.const_mul (c4 / 5))).add
      (h6.const_mul (c5 / 6))).add (h7.const_mul (c6 / 7))).add (h8.const_mul (c7 / 8))
    refine (h.congr_deriv (by ring)).congr_of_eventuallyEq
      (Filter.Eventually.of_forall (fun z => ?_))
    simp only [Pi.add_apply]
  rw [intervalIntegral.integral_eq_sub_of_hasDerivAt (fun y _ => hd y)
    ((by fun_prop : Continuous (fun y : ℝ => c0 + c1 * y + c2 * y ^ 2 + c3 * y ^ 3 + c4 * y ^ 4
      + c5 * y ^ 5 + c6 * y ^ 6 + c7 * y ^ 7)).intervalIntegrable a b)]

end LawA

section Witness

/-- The investor's score law at `μ = 3/4`. -/
noncomputable def αp (p : ℝ) : Measure ℝ := investorLaw (3 / 4) lawA (bern p)

/-- The non-investor's score law at `μ = 3/4`. -/
noncomputable def β0 : Measure ℝ := nonInvestorLaw (3 / 4) lawA

instance αp_isProb (p : ℝ) [Fact (0 ≤ p ∧ p ≤ 1)] : IsProbabilityMeasure (αp p) := by
  unfold αp; infer_instance

instance β0_isProb : IsProbabilityMeasure β0 := by
  unfold β0; infer_instance

instance αp_noAtoms (p : ℝ) [Fact (0 ≤ p ∧ p ≤ 1)] : NoAtoms (αp p) :=
  investorLaw_noAtoms' (3 / 4) (by norm_num) lawA (bern p)

instance β0_noAtoms : NoAtoms β0 :=
  nonInvestorLaw_noAtoms' (3 / 4) (by norm_num) lawA

variable (p : ℝ) [hp : Fact (0 ≤ p ∧ p ≤ 1)]

/-- P2 holds in the witness. -/
theorem αp_le_β0 (x : ℝ) : cdf (αp p) x ≤ cdf β0 x :=
  p2_fosd (3 / 4) lawA (bern p) (by norm_num) (bern_Iio p) x

theorem cdf_β0 (x : ℝ) : cdf β0 x = cdf lawA (x / (3 / 4)) :=
  cdf_map_mul lawA (3 / 4) (by norm_num) x

theorem cdf_αp (x : ℝ) :
    cdf (αp p) x
      = p * cdf lawA (x / (3 / 4)) + (1 - p) * cdf lawA ((x - (1 - 3 / 4)) / (3 / 4)) :=
  cdf_investorLaw_bern p (3 / 4) lawA (by norm_num) x

theorem weight_one (x : ℝ) : weight β0 (αp p) 1 x = cdf (αp p) x * (1 - cdf β0 x) := by
  unfold EntryContestPMU.weight
  simp

theorem weight_two (x : ℝ) :
    weight β0 (αp p) 2 x = cdf (αp p) x * cdf β0 x * (1 - cdf β0 x) := by
  unfold EntryContestPMU.weight
  simp

theorem integral_αp (h : ℝ → ℝ) (hh : Measurable h) (B : ℝ) (hB : ∀ x, |h x| ≤ B) :
    ∫ x, h x ∂(αp p)
      = p * (∫ r, h (3 / 4 * r) ∂lawA) + (1 - p) * ∫ r, h (3 / 4 * r + (1 - 3 / 4)) ∂lawA :=
  integral_investorLaw_bern p (3 / 4) lawA h hh B hB

theorem integral_β0 (h : ℝ → ℝ) (hh : Measurable h) :
    ∫ x, h x ∂β0 = ∫ r, h (3 / 4 * r) ∂lawA :=
  integral_nonInvestorLaw lawA (3 / 4) h hh

theorem lawA_base (Q : ℕ) :
    ∫ r, weight β0 (αp p) Q (3 / 4 * r) ∂lawA
      = 2 * ∫ y in (0 : ℝ)..1, weight β0 (αp p) Q (3 / 4 * (1 - y)) * y :=
  integral_lawA_interval (fun r => weight β0 (αp p) Q (3 / 4 * r))
    ((measurable_weight β0 (αp p) Q).comp (by fun_prop)) 1 (fun r => weight_bound β0 (αp p) Q _)

theorem lawA_shift (Q : ℕ) :
    ∫ r, weight β0 (αp p) Q (3 / 4 * r + (1 - 3 / 4)) ∂lawA
      = 2 * ∫ y in (0 : ℝ)..1, weight β0 (αp p) Q (3 / 4 * (1 - y) + (1 - 3 / 4)) * y :=
  integral_lawA_interval (fun r => weight β0 (αp p) Q (3 / 4 * r + (1 - 3 / 4)))
    ((measurable_weight β0 (αp p) Q).comp (by fun_prop)) 1 (fun r => weight_bound β0 (αp p) Q _)

theorem hi (Q : ℕ) (a b : ℝ) :
    IntervalIntegrable (fun y => weight β0 (αp p) Q (3 / 4 * (1 - y)) * y) volume a b :=
  intervalIntegrable_mul_id _ ((measurable_weight β0 (αp p) Q).comp (by fun_prop))
    (fun y => weight_bound β0 (αp p) Q _) a b

theorem hs (Q : ℕ) (a b : ℝ) :
    IntervalIntegrable (fun y => weight β0 (αp p) Q (3 / 4 * (1 - y) + (1 - 3 / 4)) * y)
      volume a b :=
  intervalIntegrable_mul_id _ ((measurable_weight β0 (αp p) Q).comp (by fun_prop))
    (fun y => weight_bound β0 (αp p) Q _) a b

/-- Below `y = 1/3` the shifted score exceeds `μ`, where the weight vanishes. -/
theorem shift0 (Q : ℕ) :
    ∫ y in (0 : ℝ)..(1 / 3), weight β0 (αp p) Q (3 / 4 * (1 - y) + (1 - 3 / 4)) * y = 0 := by
  have e : EqOn (fun y => weight β0 (αp p) Q (3 / 4 * (1 - y) + (1 - 3 / 4)) * y)
      (fun _ => (0 : ℝ)) (uIcc 0 (1 / 3)) := by
    intro y hy
    rw [uIcc_of_le (by norm_num)] at hy
    obtain ⟨h0, h1⟩ := hy
    dsimp only
    unfold EntryContestPMU.weight
    rw [cdf_β0, show (3 / 4 * (1 - y) + (1 - 3 / 4)) / (3 / 4) = 4 / 3 - y by ring,
      cdf_lawA_of_ge (4 / 3 - y) (by linarith)]
    ring
  rw [intervalIntegral.integral_congr e]
  simp

theorem base1_one :
    ∫ y in (0 : ℝ)..(2 / 3), weight β0 (αp p) 1 (3 / 4 * (1 - y)) * y
      = 28 * p / 1215 + 128 / 10935 := by
  have e : EqOn (fun y => weight β0 (αp p) 1 (3 / 4 * (1 - y)) * y)
      (fun y => 0 + 0 * y + 0 * y ^ 2 + (p / 9 + 8 / 9) * y ^ 3 + (2 * p / 3 - 2 / 3) * y ^ 4
        + (-1) * y ^ 5 + 0 * y ^ 6 + 0 * y ^ 7) (uIcc 0 (2 / 3)) := by
    intro y hy
    rw [uIcc_of_le (by norm_num)] at hy
    obtain ⟨h0, h1⟩ := hy
    dsimp only
    rw [weight_one, cdf_αp, cdf_β0, show (3 / 4 * (1 - y)) / (3 / 4) = 1 - y by ring,
      show (3 / 4 * (1 - y) - (1 - 3 / 4)) / (3 / 4) = 2 / 3 - y by ring,
      cdf_lawA_mem (1 - y) (by linarith) (by linarith),
      cdf_lawA_mem (2 / 3 - y) (by linarith) (by linarith)]
    ring
  rw [intervalIntegral.integral_congr e, integral_poly8]
  ring

theorem base2_one :
    ∫ y in (2 / 3 : ℝ)..1, weight β0 (αp p) 1 (3 / 4 * (1 - y)) * y = 425 * p / 8748 := by
  have e : EqOn (fun y => weight β0 (αp p) 1 (3 / 4 * (1 - y)) * y)
      (fun y => 0 + 0 * y + 0 * y ^ 2 + p * y ^ 3 + 0 * y ^ 4 + (-p) * y ^ 5 + 0 * y ^ 6
        + 0 * y ^ 7) (uIcc (2 / 3) 1) := by
    intro y hy
    rw [uIcc_of_le (by norm_num)] at hy
    obtain ⟨h0, h1⟩ := hy
    dsimp only
    rw [weight_one, cdf_αp, cdf_β0, show (3 / 4 * (1 - y)) / (3 / 4) = 1 - y by ring,
      show (3 / 4 * (1 - y) - (1 - 3 / 4)) / (3 / 4) = 2 / 3 - y by ring,
      cdf_lawA_mem (1 - y) (by linarith) (by linarith),
      cdf_lawA_of_nonpos (2 / 3 - y) (by linarith)]
    ring
  rw [intervalIntegral.integral_congr e, integral_poly8]
  ring

theorem shift1_one :
    ∫ y in (1 / 3 : ℝ)..1, weight β0 (αp p) 1 (3 / 4 * (1 - y) + (1 - 3 / 4)) * y
      = 412 * p / 10935 + 232 / 10935 := by
  have e : EqOn (fun y => weight β0 (αp p) 1 (3 / 4 * (1 - y) + (1 - 3 / 4)) * y)
      (fun y => 0 + (1 / 9 - p / 81) * y + (4 * p / 27 - 2 / 3) * y ^ 2
        + (8 / 9 - 5 * p / 9) * y ^ 3 + (2 * p / 3 + 2 / 3) * y ^ 4 + (-1) * y ^ 5 + 0 * y ^ 6
        + 0 * y ^ 7) (uIcc (1 / 3) 1) := by
    intro y hy
    rw [uIcc_of_le (by norm_num)] at hy
    obtain ⟨h0, h1⟩ := hy
    dsimp only
    rw [weight_one, cdf_αp, cdf_β0,
      show (3 / 4 * (1 - y) + (1 - 3 / 4)) / (3 / 4) = 4 / 3 - y by ring,
      show (3 / 4 * (1 - y) + (1 - 3 / 4) - (1 - 3 / 4)) / (3 / 4) = 1 - y by ring,
      cdf_lawA_mem (4 / 3 - y) (by linarith) (by linarith),
      cdf_lawA_mem (1 - y) (by linarith) (by linarith)]
    ring
  rw [intervalIntegral.integral_congr e, integral_poly8]
  ring

theorem base1_two :
    ∫ y in (0 : ℝ)..(2 / 3), weight β0 (αp p) 2 (3 / 4 * (1 - y)) * y
      = 10916 * p / 688905 + 6304 / 688905 := by
  have e : EqOn (fun y => weight β0 (αp p) 2 (3 / 4 * (1 - y)) * y)
      (fun y => 0 + 0 * y + 0 * y ^ 2 + (p / 9 + 8 / 9) * y ^ 3 + (2 * p / 3 - 2 / 3) * y ^ 4
        + (-p / 9 - 17 / 9) * y ^ 5 + (2 / 3 - 2 * p / 3) * y ^ 6 + 1 * y ^ 7)
      (uIcc 0 (2 / 3)) := by
    intro y hy
    rw [uIcc_of_le (by norm_num)] at hy
    obtain ⟨h0, h1⟩ := hy
    dsimp only
    rw [weight_two, cdf_αp, cdf_β0, show (3 / 4 * (1 - y)) / (3 / 4) = 1 - y by ring,
      show (3 / 4 * (1 - y) - (1 - 3 / 4)) / (3 / 4) = 2 / 3 - y by ring,
      cdf_lawA_mem (1 - y) (by linarith) (by linarith),
      cdf_lawA_mem (2 / 3 - y) (by linarith) (by linarith)]
    ring
  rw [intervalIntegral.integral_congr e, integral_poly8]
  ring

theorem base2_two :
    ∫ y in (2 / 3 : ℝ)..1, weight β0 (αp p) 2 (3 / 4 * (1 - y)) * y = 875 * p / 52488 := by
  have e : EqOn (fun y => weight β0 (αp p) 2 (3 / 4 * (1 - y)) * y)
      (fun y => 0 + 0 * y + 0 * y ^ 2 + p * y ^ 3 + 0 * y ^ 4 + (-2 * p) * y ^ 5 + 0 * y ^ 6
        + p * y ^ 7) (uIcc (2 / 3) 1) := by
    intro y hy
    rw [uIcc_of_le (by norm_num)] at hy
    obtain ⟨h0, h1⟩ := hy
    dsimp only
    rw [weight_two, cdf_αp, cdf_β0, show (3 / 4 * (1 - y)) / (3 / 4) = 1 - y by ring,
      show (3 / 4 * (1 - y) - (1 - 3 / 4)) / (3 / 4) = 2 / 3 - y by ring,
      cdf_lawA_mem (1 - y) (by linarith) (by linarith),
      cdf_lawA_of_nonpos (2 / 3 - y) (by linarith)]
    ring
  rw [intervalIntegral.integral_congr e, integral_poly8]
  ring

theorem shift1_two :
    ∫ y in (1 / 3 : ℝ)..1, weight β0 (αp p) 2 (3 / 4 * (1 - y) + (1 - 3 / 4)) * y
      = 6028 * p / 229635 + 776 / 45927 := by
  have e : EqOn (fun y => weight β0 (αp p) 2 (3 / 4 * (1 - y) + (1 - 3 / 4)) * y)
      (fun y => 0 + (8 / 81 - 8 * p / 729) * y + (10 * p / 81 - 14 / 27) * y ^ 2
        + (19 / 81 - 31 * p / 81) * y ^ 3 + (2 * p / 27 + 50 / 27) * y ^ 4 + (p - 4 / 3) * y ^ 5
        + (-2 * p / 3 - 4 / 3) * y ^ 6 + 1 * y ^ 7) (uIcc (1 / 3) 1) := by
    intro y hy
    rw [uIcc_of_le (by norm_num)] at hy
    obtain ⟨h0, h1⟩ := hy
    dsimp only
    rw [weight_two, cdf_αp, cdf_β0,
      show (3 / 4 * (1 - y) + (1 - 3 / 4)) / (3 / 4) = 4 / 3 - y by ring,
      show (3 / 4 * (1 - y) + (1 - 3 / 4) - (1 - 3 / 4)) / (3 / 4) = 1 - y by ring,
      cdf_lawA_mem (4 / 3 - y) (by linarith) (by linarith),
      cdf_lawA_mem (1 - y) (by linarith) (by linarith)]
    ring
  rw [intervalIntegral.integral_congr e, integral_poly8]
  ring

theorem base_one :
    ∫ y in (0 : ℝ)..1, weight β0 (αp p) 1 (3 / 4 * (1 - y)) * y
      = (28 * p / 1215 + 128 / 10935) + 425 * p / 8748 := by
  rw [← intervalIntegral.integral_add_adjacent_intervals (hi p 1 0 (2 / 3)) (hi p 1 (2 / 3) 1),
    base1_one, base2_one]

theorem shift_one :
    ∫ y in (0 : ℝ)..1, weight β0 (αp p) 1 (3 / 4 * (1 - y) + (1 - 3 / 4)) * y
      = 412 * p / 10935 + 232 / 10935 := by
  rw [← intervalIntegral.integral_add_adjacent_intervals (hs p 1 0 (1 / 3)) (hs p 1 (1 / 3) 1),
    shift0, shift1_one, zero_add]

theorem base_two :
    ∫ y in (0 : ℝ)..1, weight β0 (αp p) 2 (3 / 4 * (1 - y)) * y
      = (10916 * p / 688905 + 6304 / 688905) + 875 * p / 52488 := by
  rw [← intervalIntegral.integral_add_adjacent_intervals (hi p 2 0 (2 / 3)) (hi p 2 (2 / 3) 1),
    base1_two, base2_two]

theorem shift_two :
    ∫ y in (0 : ℝ)..1, weight β0 (αp p) 2 (3 / 4 * (1 - y) + (1 - 3 / 4)) * y
      = 6028 * p / 229635 + 776 / 45927 := by
  rw [← intervalIntegral.integral_add_adjacent_intervals (hs p 2 0 (1 / 3)) (hs p 2 (1 / 3) 1),
    shift0, shift1_two, zero_add]

/-- **The step from `Q = 1` to `Q = 2`, in closed form.** -/
theorem stepQ_one (V : ℝ) :
    stepQ V (αp p) β0 (αp p) 1
      = V * (-11 * p ^ 2 / 162 + 1901 * p / 21870 - 208 / 10935) := by
  rw [pmu_identity V (αp p) β0 (αp p) 1 le_rfl,
    integral_αp p _ (measurable_weight β0 (αp p) 1) 1 (weight_bound β0 (αp p) 1),
    integral_β0 _ (measurable_weight β0 (αp p) 1), lawA_base p 1, lawA_shift p 1, base_one p,
    shift_one p]
  ring

/-- **The step from `Q = 2` to `Q = 3`, in closed form.** -/
theorem stepQ_two (V : ℝ) :
    stepQ V (αp p) β0 (αp p) 2
      = V * (-4933 * p ^ 2 / 393660 + 77219 * p / 2755620 - 10672 / 688905) := by
  rw [pmu_identity V (αp p) β0 (αp p) 2 (by norm_num),
    integral_αp p _ (measurable_weight β0 (αp p) 2) 1 (weight_bound β0 (αp p) 2),
    integral_β0 _ (measurable_weight β0 (αp p) 2), lawA_base p 2, lawA_shift p 2, base_two p,
    shift_two p]
  ring

end Witness

section Conclusions

/-- **Rise then fall.** With `p = 1/2`, the first entrant's gain is larger with two challengers
    than with one, and smaller with three than with two. -/
theorem rise_then_fall (V : ℝ) (hV : 0 < V) :
    Delta V (αp (1 / 2)) β0 (αp (1 / 2)) 1 0 < Delta V (αp (1 / 2)) β0 (αp (1 / 2)) 2 0
      ∧ Delta V (αp (1 / 2)) β0 (αp (1 / 2)) 3 0 < Delta V (αp (1 / 2)) β0 (αp (1 / 2)) 2 0 := by
  haveI : Fact ((0 : ℝ) ≤ 1 / 2 ∧ (1 / 2 : ℝ) ≤ 1) := ⟨by norm_num⟩
  have h1 := stepQ_one (1 / 2) V
  have h2 := stepQ_two (1 / 2) V
  unfold stepQ at h1 h2
  simp only [show (1 : ℕ) + 1 = 2 from rfl, show (2 : ℕ) + 1 = 3 from rfl] at h1 h2
  have e1 : (-11 * (1 / 2 : ℝ) ^ 2 / 162 + 1901 * (1 / 2) / 21870 - 208 / 10935)
      = 653 / 87480 := by norm_num
  have e2 : (-4933 * (1 / 2 : ℝ) ^ 2 / 393660 + 77219 * (1 / 2) / 2755620 - 10672 / 688905)
      = -10169 / 2204496 := by norm_num
  rw [e1] at h1
  rw [e2] at h2
  have hpos : 0 < V * (653 / 87480) := by positivity
  have hneg : V * (-10169 / 2204496) < 0 := mul_neg_of_pos_of_neg hV (by norm_num)
  constructor <;> linarith

/-- **The referee's direction.** With `p = 1/4`, the first entrant's gain is smaller with two
    challengers than with one. -/
theorem fall_from_one (V : ℝ) (hV : 0 < V) :
    Delta V (αp (1 / 4)) β0 (αp (1 / 4)) 2 0 < Delta V (αp (1 / 4)) β0 (αp (1 / 4)) 1 0 := by
  haveI : Fact ((0 : ℝ) ≤ 1 / 4 ∧ (1 / 4 : ℝ) ≤ 1) := ⟨by norm_num⟩
  have h1 := stepQ_one (1 / 4) V
  unfold stepQ at h1
  simp only [show (1 : ℕ) + 1 = 2 from rfl] at h1
  have e1 : (-11 * (1 / 4 : ℝ) ^ 2 / 162 + 1901 * (1 / 4) / 21870 - 208 / 10935)
      = -179 / 116640 := by norm_num
  rw [e1] at h1
  have hneg : V * (-179 / 116640) < 0 := mul_neg_of_pos_of_neg hV (by norm_num)
  linarith

end Conclusions

end EntryContestFall
