import Mathlib
import StepIdentity
import EntryContestModel
import RepresentationFOSD

open MeasureTheory ProbabilityTheory Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestP6P7

open EntryContestModel EntryContestP1P2

theorem integrable_of_bounded {Ω : Type*} [MeasurableSpace Ω] (ρ : Measure Ω)
    [IsProbabilityMeasure ρ] (f : Ω → ℝ) (hf : Measurable f) (B : ℝ) (hB : ∀ x, |f x| ≤ B) :
    Integrable f ρ :=
  (integrable_const B).mono' hf.aestronglyMeasurable
    (ae_of_all _ (fun x => by rw [Real.norm_eq_abs]; exact hB x))

theorem abs_cdf_le_one (μ : Measure ℝ) [IsProbabilityMeasure μ] (x : ℝ) : |cdf μ x| ≤ 1 := by
  rw [abs_of_nonneg (cdf_nonneg μ x)]; exact cdf_le_one μ x

theorem integral_maxLaw (μ ν : Measure ℝ) [IsProbabilityMeasure μ] [IsProbabilityMeasure ν]
    [NoAtoms μ] (g : ℝ → ℝ) (hg : Measurable g) (B : ℝ) (hB : ∀ x, |g x| ≤ B) :
    ∫ x, g x ∂(maxLaw μ ν) = (∫ x, g x * cdf ν x ∂μ) + ∫ y, g y * cdf μ y ∂ν := by
  rw [maxLaw, integral_map measurable_max2.aemeasurable hg.aestronglyMeasurable]
  have hS1 : MeasurableSet {q : ℝ × ℝ | q.2 ≤ q.1} :=
    measurableSet_le measurable_snd measurable_fst
  have hS2 : MeasurableSet {q : ℝ × ℝ | q.1 < q.2} :=
    measurableSet_lt measurable_fst measurable_snd
  have hsplit : (fun p : ℝ × ℝ => g (max p.1 p.2))
      = fun p => {q : ℝ × ℝ | q.2 ≤ q.1}.indicator (fun q => g q.1) p
        + {q : ℝ × ℝ | q.1 < q.2}.indicator (fun q => g q.2) p := by
    funext p
    by_cases h : p.2 ≤ p.1
    · have h' : ¬ p.1 < p.2 := not_lt.mpr h
      simp [indicator, h, h']
    · have h' : p.1 < p.2 := not_le.mp h
      simp [indicator, h, h', max_eq_right h'.le]
  rw [hsplit]
  have hi1 : Integrable ({q : ℝ × ℝ | q.2 ≤ q.1}.indicator (fun q => g q.1)) (μ.prod ν) :=
    (integrable_of_bounded (μ.prod ν) (fun q => g q.1) (hg.comp measurable_fst) B
      (fun q => hB q.1)).indicator hS1
  have hi2 : Integrable ({q : ℝ × ℝ | q.1 < q.2}.indicator (fun q => g q.2)) (μ.prod ν) :=
    (integrable_of_bounded (μ.prod ν) (fun q => g q.2) (hg.comp measurable_snd) B
      (fun q => hB q.2)).indicator hS2
  rw [integral_add hi1 hi2, integral_prod _ hi1, integral_prod_symm _ hi2]
  congr 1
  · refine integral_congr_ae (ae_of_all _ (fun x => ?_))
    have h : (fun y => {q : ℝ × ℝ | q.2 ≤ q.1}.indicator (fun q => g q.1) (x, y))
        = (Iic x).indicator (fun _ => g x) := by
      funext y; by_cases hy : y ≤ x <;> simp [indicator, hy]
    simp only at h ⊢
    rw [h, integral_indicator_const _ measurableSet_Iic, cdf_eq_real, smul_eq_mul, mul_comm]
  · refine integral_congr_ae (ae_of_all _ (fun y => ?_))
    have h : (fun x => {q : ℝ × ℝ | q.1 < q.2}.indicator (fun q => g q.2) (x, y))
        = (Iio y).indicator (fun _ => g y) := by
      funext x; by_cases hx : x < y <;> simp [indicator, hx]
    simp only at h ⊢
    rw [h, integral_indicator_const _ measurableSet_Iio, cdf_eq_real, smul_eq_mul, mul_comm,
      measureReal_def, measureReal_def, measure_congr (Iio_ae_eq_Iic (μ := μ))]

variable (α : Measure ℝ) [IsProbabilityMeasure α] [NoAtoms α]

theorem cdf_iid (n : ℕ) (x : ℝ) : cdf (powLaw α α n) x = cdf α x ^ (n + 1) := by
  rw [cdf_powLaw, pow_succ]; ring

theorem integral_iid (n : ℕ) : ∀ (g : ℝ → ℝ), Measurable g → ∀ B : ℝ, (∀ x, |g x| ≤ B) →
    ∫ x, g x ∂(powLaw α α n) = (n + 1) * ∫ x, g x * cdf α x ^ n ∂α := by
  induction n with
  | zero =>
      intro g _ _ _
      simp [powLaw]
  | succ n ih =>
      intro g hg B hB
      have hB0 : 0 ≤ B := le_trans (abs_nonneg _) (hB 0)
      simp only [powLaw]
      rw [integral_maxLaw (powLaw α α n) α g hg B hB]
      have hgF : ∀ x, |g x * cdf α x| ≤ B := by
        intro x
        rw [abs_mul]
        calc |g x| * |cdf α x| ≤ B * 1 :=
              mul_le_mul (hB x) (abs_cdf_le_one α x) (abs_nonneg _) hB0
          _ = B := mul_one B
      rw [ih (fun x => g x * cdf α x) (hg.mul (cdf α).mono.measurable) B hgF]
      simp_rw [cdf_iid]
      have e : (fun x => g x * cdf α x * cdf α x ^ n) = fun x => g x * cdf α x ^ (n + 1) := by
        funext x; ring
      rw [e]
      push_cast
      ring

theorem integral_cdf_pow (m : ℕ) : ∫ x, cdf α x ^ m ∂α = 1 / (m + 1) := by
  cases m with
  | zero => simp
  | succ k =>
      have h := two_max_sum α (powLaw α α k)
      simp_rw [cdf_iid] at h
      rw [integral_iid α k (fun x => cdf α x) (cdf α).mono.measurable 1 (abs_cdf_le_one α)] at h
      have e : (fun x => cdf α x * cdf α x ^ k) = fun x => cdf α x ^ (k + 1) := by
        funext x; ring
      rw [e] at h
      have hk : (0 : ℝ) < (k : ℝ) + 2 := by positivity
      push_cast
      field_simp
      linarith

variable (β C : Measure ℝ) [IsProbabilityMeasure β] [IsProbabilityMeasure C]

theorem rivals_cdf_le (a b : ℕ) (x : ℝ) : cdf (rivals C α β a b) x ≤ cdf α x ^ a := by
  rw [cdf_rivals]
  have h1 : cdf C x ≤ 1 := cdf_le_one C x
  have h2 : cdf β x ^ b ≤ 1 := pow_le_one₀ (cdf_nonneg β x) (cdf_le_one β x)
  have h3 : 0 ≤ cdf α x ^ a := pow_nonneg (cdf_nonneg α x) a
  have h4 : 0 ≤ cdf C x := cdf_nonneg C x
  have h5 : 0 ≤ cdf β x ^ b := pow_nonneg (cdf_nonneg β x) b
  calc cdf C x * cdf α x ^ a * cdf β x ^ b ≤ 1 * cdf α x ^ a * 1 := by gcongr
    _ = cdf α x ^ a := by ring

theorem p7_Delta_le (V : ℝ) (hV : 0 ≤ V) (Q m : ℕ) :
    Delta V α β C Q m ≤ V / (m + 1) := by
  unfold Delta
  set H := rivals C α β m (Q - 1 - m)
  have hI2 : 0 ≤ ∫ x, cdf H x ∂β := integral_nonneg (fun x => cdf_nonneg H x)
  have hI1 : ∫ x, cdf H x ∂α ≤ ∫ x, cdf α x ^ m ∂α :=
    integral_mono (integrable_cdf H α)
      (integrable_of_bounded α _ ((cdf α).mono.measurable.pow_const m) 1
        (fun x => by
          rw [abs_of_nonneg (pow_nonneg (cdf_nonneg α x) m)]
          exact pow_le_one₀ (cdf_nonneg α x) (cdf_le_one α x)))
      (fun x => rivals_cdf_le α β C m (Q - 1 - m) x)
  rw [integral_cdf_pow α m] at hI1
  have hd : (∫ x, cdf H x ∂α) - (∫ x, cdf H x ∂β) ≤ 1 / (m + 1) := by linarith
  calc V * ((∫ x, cdf H x ∂α) - (∫ x, cdf H x ∂β)) ≤ V * (1 / (m + 1)) :=
        mul_le_mul_of_nonneg_left hd hV
    _ = V / (m + 1) := by ring

theorem p7_cap (V : ℝ) (hV : 0 ≤ V) (Q : ℕ) (kappa : ℝ) (m : ℕ)
    (hm : kappa ≤ Delta V α β C Q m) : ((m + 1 : ℕ) : ℝ) * kappa ≤ V :=
  EntryContest.entry_index_bounded (fun a b : ℝ => a ≤ b) (fun n a => (n : ℝ) * a)
    (fun _ _ _ => le_trans)
    (fun n a b hab => mul_le_mul_of_nonneg_left hab (Nat.cast_nonneg n))
    V kappa (Delta V α β C Q)
    (fun k => by
      have h := p7_Delta_le α β C V hV Q k
      have hk : (0 : ℝ) < (k : ℝ) + 1 := by positivity
      push_cast
      rw [le_div_iff₀ hk] at h
      linarith)
    m hm

theorem p7_count (V : ℝ) (hV : 0 ≤ V) (Q : ℕ) (kappa : ℝ) (hkappa : 0 < kappa) (m : ℕ)
    (hm : kappa ≤ Delta V α β C Q m) : ((m + 1 : ℕ) : ℝ) ≤ V / kappa := by
  rw [le_div_iff₀ hkappa]
  exact p7_cap α β C V hV Q kappa m hm

theorem cdf_dirac_zero (x : ℝ) (hx : 0 ≤ x) : cdf (Measure.dirac (0 : ℝ)) x = 1 := by
  rw [cdf_eq_real, measureReal_def, Measure.dirac_apply_of_mem (mem_Iic.mpr hx)]
  rfl

theorem p6_evaluation (V : ℝ) (Q m : ℕ) (h0 : α (Iic 0) = 0) :
    Delta V α (Measure.dirac 0) α Q m = V / (m + 2) := by
  unfold Delta
  have hF0 : cdf α 0 = 0 := by rw [cdf_eq_real, measureReal_def, h0]; rfl
  have hpos : ∀ᵐ x ∂α, 0 < x := by
    rw [ae_iff]
    have e : {x : ℝ | ¬ 0 < x} = Iic 0 := by ext x; simp
    rw [e, h0]
  have h1 : ∫ x, cdf (rivals α α (Measure.dirac 0) m (Q - 1 - m)) x ∂α
      = ∫ x, cdf α x ^ (m + 1) ∂α := by
    refine integral_congr_ae (hpos.mono (fun x hx => ?_))
    rw [cdf_rivals, cdf_dirac_zero x hx.le, one_pow, mul_one]
    ring
  have h2 : ∫ x, cdf (rivals α α (Measure.dirac 0) m (Q - 1 - m)) x ∂(Measure.dirac 0) = 0 := by
    rw [integral_dirac, cdf_rivals, hF0]
    simp
  rw [h1, h2, integral_cdf_pow α (m + 1)]
  push_cast
  ring

section Primitives

variable (ρr ρs : Measure ℝ) [IsProbabilityMeasure ρr] [IsProbabilityMeasure ρs]

theorem nonInvestorLaw_zero : nonInvestorLaw 0 ρr = Measure.dirac 0 := by
  have e : (fun r : ℝ => (0 : ℝ) * r) = fun _ => 0 := by funext r; ring
  rw [nonInvestorLaw, e, Measure.map_const, measure_univ, one_smul]

theorem investorLaw_zero : investorLaw 0 ρr ρs = ρs := by
  have e : (fun p : ℝ × ℝ => (0 : ℝ) * p.1 + (1 - 0) * p.2) = Prod.snd := by funext p; ring
  rw [investorLaw, e, Measure.map_snd_prod, measure_univ, one_smul]

theorem Iic_zero_null [NoAtoms ρs] (hs : ρs (Iio 0) = 0) : ρs (Iic 0) = 0 := by
  rw [← measure_congr (Iio_ae_eq_Iic (μ := ρs))]
  exact hs

theorem p6_at_zero [NoAtoms ρs] (hs : ρs (Iio 0) = 0) (V : ℝ) (Q m : ℕ) :
    Delta V (investorLaw 0 ρr ρs) (nonInvestorLaw 0 ρr) (investorLaw 0 ρr ρs) Q m
      = V / (m + 2) := by
  rw [investorLaw_zero, nonInvestorLaw_zero]
  exact p6_evaluation ρs V Q m (Iic_zero_null ρs hs)

theorem tendsto_indicator_Iic {l : Filter ℝ} (a b : ℝ → ℝ) (a0 b0 : ℝ)
    (ha : Tendsto a l (𝓝 a0)) (hb : Tendsto b l (𝓝 b0)) (hne : a0 ≠ b0) :
    Tendsto (fun t => (Iic (b t)).indicator (fun _ => (1 : ℝ)) (a t)) l
      (𝓝 ((Iic b0).indicator (fun _ => (1 : ℝ)) a0)) := by
  have hd : Tendsto (fun t => b t - a t) l (𝓝 (b0 - a0)) := hb.sub ha
  rcases lt_or_gt_of_ne hne with h | h
  · have hev : ∀ᶠ t in l, 0 < b t - a t := hd.eventually (eventually_gt_nhds (by linarith))
    rw [indicator_of_mem (mem_Iic.mpr h.le)]
    refine tendsto_const_nhds.congr' (hev.mono (fun t ht => ?_))
    dsimp only
    rw [indicator_of_mem (mem_Iic.mpr (by linarith))]
  · have hev : ∀ᶠ t in l, b t - a t < 0 := hd.eventually (eventually_lt_nhds (by linarith))
    rw [indicator_of_notMem (fun h' => absurd (mem_Iic.mp h') (not_le.mpr h))]
    refine tendsto_const_nhds.congr' (hev.mono (fun t ht => ?_))
    dsimp only
    rw [indicator_of_notMem (fun h' => by have := mem_Iic.mp h'; linarith)]

theorem measurable_ind (z : ℝ) : Measurable ((Iic z).indicator (fun _ : ℝ => (1 : ℝ))) :=
  measurable_const.indicator measurableSet_Iic

theorem cdf_as_integral (ν : Measure ℝ) [IsProbabilityMeasure ν] (z : ℝ) :
    cdf ν z = ∫ x, (Iic z).indicator (fun _ => (1 : ℝ)) x ∂ν := by
  rw [integral_indicator_const _ measurableSet_Iic, smul_eq_mul, mul_one, cdf_eq_real]

theorem integral_investorLaw (t : ℝ) (g : ℝ → ℝ) (hg : Measurable g) :
    ∫ x, g x ∂(investorLaw t ρr ρs) = ∫ p, g (t * p.1 + (1 - t) * p.2) ∂(ρr.prod ρs) :=
  integral_map (measurable_investorScore t).aemeasurable hg.aestronglyMeasurable

theorem integral_nonInvestorLaw (t : ℝ) (g : ℝ → ℝ) (hg : Measurable g) :
    ∫ x, g x ∂(nonInvestorLaw t ρr) = ∫ r, g (t * r) ∂ρr :=
  integral_map (measurable_nonInvestorScore t).aemeasurable hg.aestronglyMeasurable

theorem integral_snd_prod (g : ℝ → ℝ) (hg : Measurable g) :
    ∫ p, g p.2 ∂(ρr.prod ρs) = ∫ x, g x ∂ρs := by
  have h := integral_map (μ := ρr.prod ρs) measurable_snd.aemeasurable
    (f := g) hg.aestronglyMeasurable
  rw [Measure.map_snd_prod, measure_univ, one_smul] at h
  exact h.symm

theorem tendsto_cdf_investorLaw (y : ℝ) (hy : ρs {y} = 0) (yf : ℝ → ℝ)
    (hyf : Tendsto yf (𝓝 0) (𝓝 y)) :
    Tendsto (fun t => cdf (investorLaw t ρr ρs) (yf t)) (𝓝 0) (𝓝 (cdf ρs y)) := by
  have hlim : ∫ p, (Iic y).indicator (fun _ => (1 : ℝ)) p.2 ∂(ρr.prod ρs) = cdf ρs y := by
    rw [integral_snd_prod ρr ρs _ (measurable_ind y), cdf_as_integral]
  have e : ∀ t, cdf (investorLaw t ρr ρs) (yf t)
      = ∫ p, (Iic (yf t)).indicator (fun _ => (1 : ℝ)) (t * p.1 + (1 - t) * p.2)
          ∂(ρr.prod ρs) := fun t => by
    rw [cdf_as_integral, integral_investorLaw ρr ρs t _ (measurable_ind (yf t))]
  rw [← hlim]
  refine (tendsto_congr e).mpr ?_
  refine tendsto_integral_filter_of_dominated_convergence (fun _ => (1 : ℝ)) ?_ ?_
    (integrable_const 1) ?_
  · exact Eventually.of_forall (fun t =>
      ((measurable_ind (yf t)).comp (measurable_investorScore t)).aestronglyMeasurable)
  · refine Eventually.of_forall (fun t => ae_of_all _ (fun p => ?_))
    rw [indicator_apply]
    split_ifs <;> simp
  · have hne : ∀ᵐ p ∂(ρr.prod ρs), p.2 ≠ y := by
      rw [ae_iff]
      have e' : {p : ℝ × ℝ | ¬ p.2 ≠ y} = univ ×ˢ {y} := by ext p; simp
      rw [e', Measure.prod_prod, hy, mul_zero]
    refine hne.mono (fun p hp => ?_)
    have hc : Continuous (fun t : ℝ => t * p.1 + (1 - t) * p.2) := by fun_prop
    exact tendsto_indicator_Iic (fun t => t * p.1 + (1 - t) * p.2) yf p.2 y
      (by simpa using hc.tendsto 0) hyf hp

theorem tendsto_cdf_nonInvestorLaw (y : ℝ) (hy : 0 < y) (yf : ℝ → ℝ)
    (hyf : Tendsto yf (𝓝 0) (𝓝 y)) :
    Tendsto (fun t => cdf (nonInvestorLaw t ρr) (yf t)) (𝓝 0) (𝓝 1) := by
  have hlim : ∫ _r, (Iic y).indicator (fun _ => (1 : ℝ)) 0 ∂ρr = 1 := by
    rw [indicator_of_mem (mem_Iic.mpr hy.le)]
    simp
  have e : ∀ t, cdf (nonInvestorLaw t ρr) (yf t)
      = ∫ r, (Iic (yf t)).indicator (fun _ => (1 : ℝ)) (t * r) ∂ρr := fun t => by
    rw [cdf_as_integral, integral_nonInvestorLaw ρr t _ (measurable_ind (yf t))]
  suffices h : Tendsto (fun t => cdf (nonInvestorLaw t ρr) (yf t)) (𝓝 0)
      (𝓝 (∫ _r, (Iic y).indicator (fun _ => (1 : ℝ)) 0 ∂ρr)) by rwa [hlim] at h
  refine (tendsto_congr e).mpr ?_
  refine tendsto_integral_filter_of_dominated_convergence (fun _ => (1 : ℝ)) ?_ ?_
    (integrable_const 1) ?_
  · exact Eventually.of_forall (fun t =>
      ((measurable_ind (yf t)).comp (measurable_nonInvestorScore t)).aestronglyMeasurable)
  · refine Eventually.of_forall (fun t => ae_of_all _ (fun r => ?_))
    rw [indicator_apply]
    split_ifs <;> simp
  · refine ae_of_all _ (fun r => ?_)
    have hc : Continuous (fun t : ℝ => t * r) := by fun_prop
    exact tendsto_indicator_Iic (fun t => t * r) yf 0 y (by simpa using hc.tendsto 0) hyf hy.ne

theorem p6_limit [NoAtoms ρs] (hs : ρs (Iio 0) = 0) (V : ℝ) (Q m : ℕ) :
    Tendsto (fun t => Delta V (investorLaw t ρr ρs) (nonInvestorLaw t ρr) (investorLaw t ρr ρs) Q m)
      (𝓝 0) (𝓝 (V / (m + 2))) := by
  have hs' := Iic_zero_null ρs hs
  have hlim : ∫ p, cdf ρs p.2 * cdf ρs p.2 ^ m * 1 ^ (Q - 1 - m) ∂(ρr.prod ρs)
      = 1 / (((m + 1 : ℕ) : ℝ) + 1) := by
    rw [integral_snd_prod ρr ρs (fun x => cdf ρs x * cdf ρs x ^ m * 1 ^ (Q - 1 - m))
      (((cdf ρs).mono.measurable.mul ((cdf ρs).mono.measurable.pow_const m)).mul
        measurable_const), ← integral_cdf_pow ρs (m + 1)]
    congr 1
    funext x
    rw [one_pow, mul_one]
    ring
  have hA : Tendsto (fun t => ∫ x, cdf (rivals (investorLaw t ρr ρs) (investorLaw t ρr ρs)
      (nonInvestorLaw t ρr) m (Q - 1 - m)) x ∂(investorLaw t ρr ρs)) (𝓝 0)
      (𝓝 (∫ p, cdf ρs p.2 * cdf ρs p.2 ^ m * 1 ^ (Q - 1 - m) ∂(ρr.prod ρs))) := by
    have e : ∀ t, ∫ x, cdf (rivals (investorLaw t ρr ρs) (investorLaw t ρr ρs)
        (nonInvestorLaw t ρr) m (Q - 1 - m)) x ∂(investorLaw t ρr ρs)
        = ∫ p, cdf (rivals (investorLaw t ρr ρs) (investorLaw t ρr ρs)
          (nonInvestorLaw t ρr) m (Q - 1 - m)) (t * p.1 + (1 - t) * p.2) ∂(ρr.prod ρs) :=
      fun t => integral_investorLaw ρr ρs t _ (cdf _).mono.measurable
    refine (tendsto_congr e).mpr ?_
    refine tendsto_integral_filter_of_dominated_convergence (fun _ => (1 : ℝ)) ?_ ?_
      (integrable_const 1) ?_
    · exact Eventually.of_forall (fun t =>
        ((cdf _).mono.measurable.comp (measurable_investorScore t)).aestronglyMeasurable)
    · exact Eventually.of_forall (fun t => ae_of_all _ (fun p => by
        rw [Real.norm_eq_abs]; exact abs_cdf_le_one _ _))
    · have hpos : ∀ᵐ p ∂(ρr.prod ρs), 0 < p.2 := by
        rw [ae_iff]
        have e' : {p : ℝ × ℝ | ¬ 0 < p.2} = univ ×ˢ Iic 0 := by ext p; simp
        rw [e', Measure.prod_prod, hs', mul_zero]
      refine hpos.mono (fun p hp => ?_)
      simp only [cdf_rivals]
      have hc : Continuous (fun t : ℝ => t * p.1 + (1 - t) * p.2) := by fun_prop
      have hT : Tendsto (fun t : ℝ => t * p.1 + (1 - t) * p.2) (𝓝 0) (𝓝 p.2) := by
        simpa using hc.tendsto 0
      have hF := tendsto_cdf_investorLaw ρr ρs p.2 (measure_singleton p.2) _ hT
      have hG := tendsto_cdf_nonInvestorLaw ρr p.2 hp _ hT
      exact (hF.mul (hF.pow m)).mul (hG.pow (Q - 1 - m))
  have hB : Tendsto (fun t => ∫ x, cdf (rivals (investorLaw t ρr ρs) (investorLaw t ρr ρs)
      (nonInvestorLaw t ρr) m (Q - 1 - m)) x ∂(nonInvestorLaw t ρr)) (𝓝 0)
      (𝓝 (∫ _r, (0 : ℝ) ∂ρr)) := by
    have e : ∀ t, ∫ x, cdf (rivals (investorLaw t ρr ρs) (investorLaw t ρr ρs)
        (nonInvestorLaw t ρr) m (Q - 1 - m)) x ∂(nonInvestorLaw t ρr)
        = ∫ r, cdf (rivals (investorLaw t ρr ρs) (investorLaw t ρr ρs)
          (nonInvestorLaw t ρr) m (Q - 1 - m)) (t * r) ∂ρr :=
      fun t => integral_nonInvestorLaw ρr t _ (cdf _).mono.measurable
    refine (tendsto_congr e).mpr ?_
    refine tendsto_integral_filter_of_dominated_convergence (fun _ => (1 : ℝ)) ?_ ?_
      (integrable_const 1) ?_
    · exact Eventually.of_forall (fun t =>
        ((cdf _).mono.measurable.comp (measurable_nonInvestorScore t)).aestronglyMeasurable)
    · exact Eventually.of_forall (fun t => ae_of_all _ (fun r => by
        rw [Real.norm_eq_abs]; exact abs_cdf_le_one _ _))
    · refine ae_of_all _ (fun r => ?_)
      have hc : Continuous (fun t : ℝ => t * r) := by fun_prop
      have hT : Tendsto (fun t : ℝ => t * r) (𝓝 0) (𝓝 0) := by simpa using hc.tendsto 0
      have hF := tendsto_cdf_investorLaw ρr ρs 0 (measure_singleton 0) _ hT
      have hF0 : cdf ρs 0 = 0 := by rw [cdf_eq_real, measureReal_def, hs']; rfl
      rw [hF0] at hF
      refine squeeze_zero (fun t => cdf_nonneg _ _) (fun t => ?_) hF
      have a := cdf_nonneg (investorLaw t ρr ρs) (t * r)
      have b : cdf (investorLaw t ρr ρs) (t * r) ^ m ≤ 1 :=
        pow_le_one₀ (cdf_nonneg _ _) (cdf_le_one _ _)
      have c : cdf (nonInvestorLaw t ρr) (t * r) ^ (Q - 1 - m) ≤ 1 :=
        pow_le_one₀ (cdf_nonneg _ _) (cdf_le_one _ _)
      have c0 := pow_nonneg (cdf_nonneg (nonInvestorLaw t ρr) (t * r)) (Q - 1 - m)
      rw [cdf_rivals, mul_assoc]
      exact mul_le_of_le_one_right a (mul_le_one₀ b c0 c)
  rw [hlim] at hA
  rw [integral_zero] at hB
  have h := (hA.sub hB).const_mul V
  have e : V * (1 / (((m + 1 : ℕ) : ℝ) + 1) - 0) = V / (m + 2) := by push_cast; ring
  rw [e] at h
  exact h

end Primitives

end EntryContestP6P7
