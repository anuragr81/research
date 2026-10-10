import Mathlib.Analysis.SpecialFunctions.Exp
import Mathlib.Topology.Algebra.Order.Field
import Mathlib.Algebra.BigOperators.Field
import Mathlib.Tactic

open Real Filter Topology Finset

namespace GibbonsWaldman1998

noncomputable def threshold (dA cA dB cB : ℝ) : ℝ := (dA - dB) / (cB - cA)

theorem prefer_higher_iff {dA cA dB cB η : ℝ} (hc : cA < cB) :
    dA + cA * η ≤ dB + cB * η ↔ threshold dA cA dB cB ≤ η := by
  unfold threshold
  rw [div_le_iff₀ (sub_pos.mpr hc)]
  constructor <;> intro h <;> linarith

theorem prefer_lower_iff {dA cA dB cB η : ℝ} (hc : cA < cB) :
    dB + cB * η < dA + cA * η ↔ η < threshold dA cA dB cB := by
  unfold threshold
  rw [lt_div_iff₀ (sub_pos.mpr hc)]
  constructor <;> intro h <;> linarith

theorem control_prefer_needs_order :
    ¬ ((0 : ℝ) + 1 * 0 ≤ 1 + 0 * 0 ↔ threshold 0 1 1 0 ≤ 0) := by
  unfold threshold
  norm_num

noncomputable def wage (d₁ c₁ d₂ c₂ d₃ c₃ η : ℝ) : ℝ :=
  max (max (d₁ + c₁ * η) (d₂ + c₂ * η)) (d₃ + c₃ * η)

theorem prop1_job1 {d₁ c₁ d₂ c₂ d₃ c₃ η : ℝ} (h12 : c₁ < c₂) (h23 : c₂ < c₃)
    (hthr : threshold d₁ c₁ d₂ c₂ < threshold d₂ c₂ d₃ c₃) (hη : η < threshold d₁ c₁ d₂ c₂) :
    wage d₁ c₁ d₂ c₂ d₃ c₃ η = d₁ + c₁ * η := by
  have a := (prefer_lower_iff h12).mpr hη
  have b := (prefer_lower_iff h23).mpr (hη.trans hthr)
  unfold wage
  rw [max_eq_left a.le, max_eq_left (by linarith)]

theorem prop1_job2 {d₁ c₁ d₂ c₂ d₃ c₃ η : ℝ} (h12 : c₁ < c₂) (h23 : c₂ < c₃)
    (hη₁ : threshold d₁ c₁ d₂ c₂ ≤ η) (hη₂ : η < threshold d₂ c₂ d₃ c₃) :
    wage d₁ c₁ d₂ c₂ d₃ c₃ η = d₂ + c₂ * η := by
  have a := (prefer_higher_iff h12).mpr hη₁
  have b := (prefer_lower_iff h23).mpr hη₂
  unfold wage
  rw [max_eq_right a, max_eq_left b.le]

theorem prop1_job3 {d₁ c₁ d₂ c₂ d₃ c₃ η : ℝ} (h12 : c₁ < c₂) (h23 : c₂ < c₃)
    (hthr : threshold d₁ c₁ d₂ c₂ < threshold d₂ c₂ d₃ c₃) (hη : threshold d₂ c₂ d₃ c₃ ≤ η) :
    wage d₁ c₁ d₂ c₂ d₃ c₃ η = d₃ + c₃ * η := by
  have a := (prefer_higher_iff h12).mpr (hthr.le.trans hη)
  have b := (prefer_higher_iff h23).mpr hη
  unfold wage
  rw [max_eq_right a, max_eq_right b]

theorem wage_strictMono {d₁ c₁ d₂ c₂ d₃ c₃ η₁ η₂ : ℝ} (hc₁ : 0 < c₁) (h12 : c₁ < c₂)
    (h23 : c₂ < c₃) (hη : η₁ < η₂) :
    wage d₁ c₁ d₂ c₂ d₃ c₃ η₁ < wage d₁ c₁ d₂ c₂ d₃ c₃ η₂ := by
  unfold wage
  have e1 : d₁ + c₁ * η₁ < d₁ + c₁ * η₂ := by nlinarith
  have e2 : d₂ + c₂ * η₁ < d₂ + c₂ * η₂ := by nlinarith
  have e3 : d₃ + c₃ * η₁ < d₃ + c₃ * η₂ := by nlinarith
  apply max_lt (max_lt _ _) _
  · exact lt_of_lt_of_le e1 ((le_max_left _ _).trans (le_max_left _ _))
  · exact lt_of_lt_of_le e2 ((le_max_right _ _).trans (le_max_left _ _))
  · exact lt_of_lt_of_le e3 (le_max_right _ _)

theorem control_wage_mono_needs_positive_slopes :
    ¬ (wage 0 (-3) 0 (-2) 0 (-1) (-2) < wage 0 (-3) 0 (-2) 0 (-1) (-1)) := by
  unfold wage
  norm_num

theorem full_info_wage_rises {d₁ c₁ d₂ c₂ d₃ c₃ θ f₀ f₁ : ℝ} (hc₁ : 0 < c₁) (h12 : c₁ < c₂)
    (h23 : c₂ < c₃) (hθ : 0 < θ) (hf : f₀ < f₁) :
    wage d₁ c₁ d₂ c₂ d₃ c₃ (θ * f₀) < wage d₁ c₁ d₂ c₂ d₃ c₃ (θ * f₁) :=
  wage_strictMono hc₁ h12 h23 (mul_lt_mul_of_pos_left hf hθ)

theorem serial_correlation_full_info {d₁ c₁ θL θH f₀ f₁ : ℝ} (hc₁ : 0 < c₁) (hθ : θL < θH)
    (hf : f₀ < f₁) :
    (d₁ + c₁ * (θL * f₁)) - (d₁ + c₁ * (θL * f₀)) < (d₁ + c₁ * (θH * f₁)) - (d₁ + c₁ * (θH * f₀)) := by
  have : 0 < c₁ * (θH - θL) * (f₁ - f₀) := mul_pos (mul_pos hc₁ (by linarith)) (by linarith)
  nlinarith

noncomputable def jobOf (η' η'' η : ℝ) : ℕ := if η < η' then 1 else if η < η'' then 2 else 3

theorem jobOf_mono {η' η'' η₁ η₂ : ℝ} (hη : η₁ ≤ η₂) : jobOf η' η'' η₁ ≤ jobOf η' η'' η₂ := by
  unfold jobOf
  split_ifs <;> first | omega | (exfalso; linarith)

theorem high_ability_promoted_no_later {η' η'' θL θH f : ℝ} (hθ : θL ≤ θH) (hf : 0 ≤ f) :
    jobOf η' η'' (θL * f) ≤ jobOf η' η'' (θH * f) :=
  jobOf_mono (mul_le_mul_of_nonneg_right hθ hf)

theorem demotion_implies_wage_decrease {d₁ c₁ d₂ c₂ d₃ c₃ η' η'' η₁ η₂ : ℝ} (hc₁ : 0 < c₁)
    (h12 : c₁ < c₂) (h23 : c₂ < c₃) (hdem : jobOf η' η'' η₂ < jobOf η' η'' η₁) :
    wage d₁ c₁ d₂ c₂ d₃ c₃ η₂ < wage d₁ c₁ d₂ c₂ d₃ c₃ η₁ := by
  have : η₂ < η₁ := by
    by_contra h
    push_neg at h
    exact absurd (jobOf_mono (η' := η') (η'' := η'') h) (not_le.mpr hdem)
  exact wage_strictMono hc₁ h12 h23 this

theorem promotion_raise_decomposition (d₁ c₁ d₂ c₂ η₁ η₂ : ℝ) :
    (d₂ + c₂ * η₂) - (d₁ + c₁ * η₁)
      = c₁ * (η₂ - η₁) + ((d₂ + c₂ * η₂) - (d₁ + c₁ * η₂)) := by
  ring

theorem promotion_raise_exceeds_within_job {d₁ c₁ d₂ c₂ η₁ η₂ : ℝ} (h12 : c₁ < c₂)
    (hη : threshold d₁ c₁ d₂ c₂ ≤ η₂) :
    c₁ * (η₂ - η₁) ≤ (d₂ + c₂ * η₂) - (d₁ + c₁ * η₁) := by
  have := (prefer_higher_iff h12).mpr hη
  linarith [promotion_raise_decomposition d₁ c₁ d₂ c₂ η₁ η₂]

noncomputable def posterior (p L : ℝ) : ℝ := p / (p + (1 - p) * L)

noncomputable def likelihoodRatio (σ θH θL f z : ℝ) : ℝ :=
  Real.exp (-(1 / (2 * σ ^ 2)) * ((z - θL * f) ^ 2 - (z - θH * f) ^ 2))

theorem likelihoodRatio_A2 (σ θH θL f z : ℝ) :
    likelihoodRatio σ θH θL f z
      = Real.exp (-(1 / (2 * σ ^ 2)) * (-2 * z * f * (θL - θH) + f ^ 2 * (θL ^ 2 - θH ^ 2))) := by
  unfold likelihoodRatio
  congr 2
  ring

theorem likelihoodRatio_strictAnti {σ θH θL f : ℝ} (hσ : σ ≠ 0) (hf : 0 < f) (hθ : θL < θH) :
    StrictAnti (likelihoodRatio σ θH θL f) := by
  intro z₁ z₂ hz
  unfold likelihoodRatio
  apply Real.exp_lt_exp.mpr
  have hk : 0 < 1 / (2 * σ ^ 2) := by positivity
  have e : -(1 / (2 * σ ^ 2)) * ((z₂ - θL * f) ^ 2 - (z₂ - θH * f) ^ 2)
      - -(1 / (2 * σ ^ 2)) * ((z₁ - θL * f) ^ 2 - (z₁ - θH * f) ^ 2)
      = -(1 / (2 * σ ^ 2)) * (2 * f * (θH - θL) * (z₂ - z₁)) := by ring
  have : 0 < (1 / (2 * σ ^ 2)) * (2 * f * (θH - θL) * (z₂ - z₁)) := by
    apply mul_pos hk
    have : 0 < θH - θL := by linarith
    have : 0 < z₂ - z₁ := by linarith
    positivity
  linarith

theorem posterior_strictAnti_ratio {p L₁ L₂ : ℝ} (hp0 : 0 < p) (hp1 : p < 1) (hL₁ : 0 ≤ L₁)
    (hL : L₁ < L₂) : posterior p L₂ < posterior p L₁ := by
  unfold posterior
  apply div_lt_div_of_pos_left hp0
  · nlinarith
  · nlinarith

theorem posterior_strictMono_prior {p₁ p₂ L : ℝ} (h1 : 0 < p₁) (h12 : p₁ < p₂) (h2 : p₂ < 1)
    (hL : 0 < L) : posterior p₁ L < posterior p₂ L := by
  unfold posterior
  have d1 : 0 < p₁ + (1 - p₁) * L := by nlinarith
  have d2 : 0 < p₂ + (1 - p₂) * L := by nlinarith
  rw [div_lt_div_iff₀ d1 d2]
  nlinarith

theorem posterior_strictMono_signal {σ θH θL f p : ℝ} (hσ : σ ≠ 0) (hf : 0 < f) (hθ : θL < θH)
    (hp0 : 0 < p) (hp1 : p < 1) :
    StrictMono (fun z => posterior p (likelihoodRatio σ θH θL f z)) := by
  intro z₁ z₂ hz
  exact posterior_strictAnti_ratio hp0 hp1 (Real.exp_pos _).le
    (likelihoodRatio_strictAnti hσ hf hθ hz)

theorem control_posterior_needs_interior_prior : posterior 1 1 = posterior 1 2 := by
  unfold posterior
  norm_num

theorem likelihoodRatio_affine (σ θH θL f z : ℝ) :
    likelihoodRatio σ θH θL f z
      = Real.exp (-(1 / (2 * σ ^ 2)) * (f ^ 2 * (θL ^ 2 - θH ^ 2)))
        * Real.exp (-((f * (θH - θL) / σ ^ 2) * z)) := by
  unfold likelihoodRatio
  rw [← Real.exp_add]
  congr 1
  ring

theorem belief_tends_to_high {σ θH θL f p : ℝ} (hσ : σ ≠ 0) (hf : 0 < f) (hθ : θL < θH)
    (hp0 : 0 < p) :
    Tendsto (fun z => posterior p (likelihoodRatio σ θH θL f z)) atTop (𝓝 1) := by
  have hk : 0 < f * (θH - θL) / σ ^ 2 := by
    have : 0 < θH - θL := by linarith
    positivity
  have hlin : Tendsto (fun z : ℝ => -((f * (θH - θL) / σ ^ 2) * z)) atTop atBot :=
    tendsto_neg_atTop_atBot.comp (tendsto_id.const_mul_atTop hk)
  have hL : Tendsto (fun z => likelihoodRatio σ θH θL f z) atTop (𝓝 0) := by
    simp_rw [likelihoodRatio_affine]
    have h0 := (Real.tendsto_exp_atBot.comp hlin).const_mul
      (Real.exp (-(1 / (2 * σ ^ 2)) * (f ^ 2 * (θL ^ 2 - θH ^ 2))))
    rw [mul_zero] at h0
    exact h0
  have hden : Tendsto (fun z => p + (1 - p) * likelihoodRatio σ θH θL f z) atTop (𝓝 p) := by
    have h1 := (hL.const_mul (1 - p)).const_add p
    rw [mul_zero, add_zero] at h1
    exact h1
  have h3 := (tendsto_const_nhds (x := p)).div hden hp0.ne'
  rw [div_self hp0.ne'] at h3
  unfold posterior
  exact h3

theorem belief_tends_to_low {σ θH θL f p : ℝ} (hσ : σ ≠ 0) (hf : 0 < f) (hθ : θL < θH)
    (hp1 : p < 1) :
    Tendsto (fun z => posterior p (likelihoodRatio σ θH θL f z)) atBot (𝓝 0) := by
  have hk : 0 < f * (θH - θL) / σ ^ 2 := by
    have : 0 < θH - θL := by linarith
    positivity
  have hlin : Tendsto (fun z : ℝ => -((f * (θH - θL) / σ ^ 2) * z)) atBot atTop :=
    tendsto_neg_atBot_atTop.comp (tendsto_id.const_mul_atBot hk)
  have hC : 0 < Real.exp (-(1 / (2 * σ ^ 2)) * (f ^ 2 * (θL ^ 2 - θH ^ 2))) := Real.exp_pos _
  have hL : Tendsto (fun z => likelihoodRatio σ θH θL f z) atBot atTop := by
    simp_rw [likelihoodRatio_affine]
    exact (Real.tendsto_exp_atTop.comp hlin).const_mul_atTop hC
  have hden : Tendsto (fun z => p + (1 - p) * likelihoodRatio σ θH θL f z) atBot atTop :=
    tendsto_atTop_add_const_left _ _ (hL.const_mul_atTop (by linarith))
  exact (tendsto_const_nhds (x := p)).div_atTop hden

theorem beliefs_martingale {ι : Type*} (s : Finset ι) (hH hL : ι → ℝ) (p : ℝ) (hp0 : 0 < p)
    (hp1 : p < 1) (hHpos : ∀ z ∈ s, 0 < hH z) (hLnn : ∀ z ∈ s, 0 ≤ hL z)
    (hsum : ∑ z ∈ s, hH z = 1) :
    ∑ z ∈ s, (p * hH z + (1 - p) * hL z) * (p * hH z / (p * hH z + (1 - p) * hL z)) = p := by
  have hterm : ∀ z ∈ s, (p * hH z + (1 - p) * hL z) * (p * hH z / (p * hH z + (1 - p) * hL z))
      = p * hH z := by
    intro z hz
    have : 0 < p * hH z + (1 - p) * hL z := by
      have := hHpos z hz
      have := hLnn z hz
      nlinarith
    field_simp
  rw [Finset.sum_congr rfl hterm, ← Finset.mul_sum, hsum, mul_one]

theorem promotion_probability_A4 (PH PL p : ℝ) : PH * p + PL * (1 - p) = PL + p * (PH - PL) := by
  ring

theorem promotion_probability_increasing {PH PL p₁ p₂ : ℝ} (hP : PL < PH) (hp : p₁ < p₂) :
    PL + p₁ * (PH - PL) < PL + p₂ * (PH - PL) := by
  nlinarith

theorem ratio_increasing_of_concave {f₀ f₁ f₂ : ℝ} (h0 : 0 < f₀) (h1 : 0 < f₁) (h2 : 0 < f₂)
    (hconc : f₂ - f₁ ≤ f₁ - f₀) : f₀ / f₁ ≤ f₁ / f₂ := by
  rw [div_le_div_iff₀ h1 h2]
  nlinarith [sq_nonneg (f₁ - f₀)]

theorem control_ratio_needs_concavity : ¬ ((1 : ℝ) / 2 ≤ 2 / 8) := by
  norm_num

theorem wage_decrease_threshold_persists {θL θH f₀ f₁ f₂ : ℝ} (hθL : 0 < θL) (h0 : 0 < f₀)
    (h1 : 0 < f₁) (h2 : 0 < f₂) (hconc : f₂ - f₁ ≤ f₁ - f₀) (h : θL * f₁ < θH * f₀) :
    θL * f₂ < θH * f₁ := by
  have hr : f₀ * f₂ ≤ f₁ * f₁ := by nlinarith [sq_nonneg (f₁ - f₀)]
  have hθH : 0 < θH := by nlinarith
  have step : θL * f₁ * f₂ < θH * f₀ * f₂ := mul_lt_mul_of_pos_right h h2
  have step2 : θH * f₀ * f₂ ≤ θH * f₁ * f₁ := by nlinarith
  nlinarith

theorem no_wage_decrease_before_threshold {θL θH f₀ f₁ p q : ℝ} (hθ : θL ≤ θH) (h0 : 0 ≤ f₀)
    (h1 : 0 ≤ f₁) (hp1 : p ≤ 1) (hq0 : 0 ≤ q) (h : θH * f₀ ≤ θL * f₁) :
    (p * θH + (1 - p) * θL) * f₀ ≤ (q * θH + (1 - q) * θL) * f₁ := by
  have a : (p * θH + (1 - p) * θL) * f₀ ≤ θH * f₀ := by
    apply mul_le_mul_of_nonneg_right _ h0
    nlinarith
  have b : θL * f₁ ≤ (q * θH + (1 - q) * θL) * f₁ := by
    apply mul_le_mul_of_nonneg_right _ h1
    nlinarith
  linarith

theorem wage_decrease_possible {θL θH f₀ f₁ : ℝ} (hθ : θL < θH) (h0 : 0 < f₀) (h1 : 0 < f₁)
    (h : θL * f₁ < θH * f₀) :
    ∃ p q : ℝ, 0 < p ∧ p < 1 ∧ 0 < q ∧ q < 1
      ∧ (q * θH + (1 - q) * θL) * f₁ < (p * θH + (1 - p) * θL) * f₀ := by
  set gap := θH * f₀ - θL * f₁
  set D := (θH - θL) * (f₀ + f₁)
  have hgap : 0 < gap := by simp only [gap]; linarith
  have hD : 0 < D := mul_pos (by linarith) (by linarith)
  set ε := gap / (2 * (D + gap))
  have hε0 : 0 < ε := by positivity
  have hε1 : ε < 1 / 2 := by
    rw [div_lt_iff₀ (by positivity)]
    linarith
  have hεD : ε * D < gap := by
    have : ε * (2 * (D + gap)) = gap := by
      simp only [ε]
      field_simp
    nlinarith
  refine ⟨1 - ε, ε, by linarith, by linarith, hε0, by linarith, ?_⟩
  have e : (1 - ε) * θH + (1 - (1 - ε)) * θL = θH - ε * (θH - θL) := by ring
  have e2 : ε * θH + (1 - ε) * θL = θL + ε * (θH - θL) := by ring
  rw [e, e2]
  have : (θH - ε * (θH - θL)) * f₀ - (θL + ε * (θH - θL)) * f₁ = gap - ε * D := by
    simp only [gap, D]
    ring
  linarith

theorem tie_rule_reading {d₁ c₁ d₂ c₂ : ℝ} (h12 : c₁ < c₂) :
    d₁ + c₁ * threshold d₁ c₁ d₂ c₂ = d₂ + c₂ * threshold d₁ c₁ d₂ c₂ := by
  unfold threshold
  have : c₂ - c₁ ≠ 0 := by linarith
  field_simp
  ring

end GibbonsWaldman1998
