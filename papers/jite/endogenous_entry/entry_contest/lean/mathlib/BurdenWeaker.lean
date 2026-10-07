import Mathlib
import KappaSpread

open MeasureTheory Real Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestBM

open EntryContestKappa

section Criteria

/-- **Burden-monotonicity from the derivative across a span.** If `u'` falls across every span
    of length `c`, the burden is strictly decreasing. No sign of `u''` is assumed. -/
theorem burden_strictAnti_of_deriv (u u' : ℝ → ℝ) (c : ℝ) (hc : 0 < c)
    (hd : ∀ x, 0 < x → HasDerivAt u (u' x) x) (hspan : ∀ w, c < w → u' w < u' (w - c)) :
    StrictAntiOn (kappa u c) (Ioi c) := by
  have hk : ∀ w, c < w → HasDerivAt (kappa u c) (u' w - u' (w - c)) w := by
    intro w hw
    have h1 := hd w (by linarith)
    have h2 := (hd (w - c) (by linarith)).comp w ((hasDerivAt_id' w).sub_const c)
    have h := h1.sub h2
    have e : (u - u ∘ fun x => x - c) = kappa u c := by funext x; rfl
    rw [e, mul_one] at h
    exact h
  refine strictAntiOn_of_deriv_neg (convex_Ioi c)
    (fun w hw => (hk w hw).continuousAt.continuousWithinAt) (fun w hw => ?_)
  rw [interior_Ioi] at hw
  rw [(hk w hw).deriv]
  linarith [hspan w hw]

/-- **A convex stretch wider than `c` rules out burden-monotonicity.** On such a stretch the
    increment of `u` over a span of length `c` cannot fall. -/
theorem not_burden_of_convex_stretch (u : ℝ → ℝ) (c a b : ℝ) (hc : 0 < c) (ha : 0 < a)
    (hab : c < b - a) (hu : ConvexOn ℝ (Icc a b) u) : ¬ StrictAntiOn (kappa u c) (Ioi c) := by
  intro hanti
  have hA : a ∈ Icc a b := ⟨le_refl a, by linarith⟩
  have hB : b ∈ Icc a b := ⟨by linarith, le_refl b⟩
  have hAc : a + c ∈ Icc a b := ⟨by linarith, by linarith⟩
  have hBc : b - c ∈ Icc a b := ⟨by linarith, by linarith⟩
  have h1 := hu.secant_mono hA hAc hB (ne_of_gt (by linarith)) (ne_of_gt (by linarith))
    (by linarith : a + c ≤ b)
  have h2 := hu.secant_mono hB hA hBc (ne_of_lt (by linarith)) (ne_of_lt (by linarith))
    (by linarith : a ≤ b - c)
  have q1 : (u (a + c) - u a) / (a + c - a) = (u (a + c) - u a) / c := by
    rw [show a + c - a = c by ring]
  have q2 : (u a - u b) / (a - b) = (u b - u a) / (b - a) := by
    rw [div_eq_div_iff (ne_of_lt (by linarith)) (ne_of_gt (by linarith))]
    ring
  have q3 : (u (b - c) - u b) / (b - c - b) = (u b - u (b - c)) / c := by
    rw [div_eq_div_iff (ne_of_lt (by linarith)) hc.ne']
    ring
  rw [q1] at h1
  rw [q2, q3] at h2
  have key : kappa u c (a + c) ≤ kappa u c b := by
    unfold kappa
    rw [show a + c - c = a by ring]
    exact (div_le_div_iff_of_pos_right hc).mp (h1.trans h2)
  have hlt := hanti (show c < a + c by linarith) (show c < b by linarith)
    (show a + c < b by linarith)
  linarith

end Criteria

section Original

/-- **The example of Claim BM in `PROOFS.tex` is not increasing**, for any amplitude `ε > 0`
    and frequency `k > 0`. Where `cos(kx) = −1` and `x` is large, `u'(x) = 1/(2√x) − εk < 0`. -/
theorem bm_example_not_monotone (ε k : ℝ) (hε : 0 < ε) (hk : 0 < k) :
    ¬ MonotoneOn (fun x => Real.sqrt x + ε * Real.sin (k * x)) (Ioi 0) := by
  intro hmono
  have h2π : 0 < 2 * π := by positivity
  set A := 1 / (4 * ε ^ 2 * k ^ 2) with hA
  obtain ⟨n, hn⟩ := exists_nat_gt (A * k / (2 * π))
  set x := ((n : ℝ) * (2 * π) + π) / k with hxdef
  have hx : 0 < x := div_pos (by positivity) hk
  have hkx : k * x = (n : ℝ) * (2 * π) + π := by rw [hxdef]; field_simp
  have hbig : A < x := by
    have h1 : A * k < n * (2 * π) := by rwa [div_lt_iff₀ h2π] at hn
    have h2 : A < n * (2 * π) / k := by rw [lt_div_iff₀ hk]; linarith
    have h3 : (n : ℝ) * (2 * π) / k ≤ x :=
      div_le_div_of_nonneg_right (by linarith [pi_pos]) hk.le
    linarith
  have hd : HasDerivAt (fun y => Real.sqrt y + ε * Real.sin (k * y))
      (1 / (2 * Real.sqrt x) + ε * (Real.cos (k * x) * (k * 1))) x :=
    (Real.hasDerivAt_sqrt hx.ne').add
      (((Real.hasDerivAt_sin (k * x)).comp x ((hasDerivAt_id' x).const_mul k)).const_mul ε)
  have hεk : 0 < 2 * ε * k := mul_pos (mul_pos two_pos hε) hk
  have hsx : 0 < 2 * Real.sqrt x := mul_pos two_pos (Real.sqrt_pos.mpr hx)
  have hs : 1 / (2 * ε * k) < Real.sqrt x := by
    rw [Real.lt_sqrt (by positivity)]
    calc (1 / (2 * ε * k)) ^ 2 = A := by rw [hA]; field_simp; ring
      _ < x := hbig
  have hlt : 1 / (2 * Real.sqrt x) < ε * k := by
    rw [div_lt_iff₀ hsx]
    rw [div_lt_iff₀ hεk] at hs
    linarith
  have hneg : 1 / (2 * Real.sqrt x) + ε * (Real.cos (k * x) * (k * 1)) < 0 := by
    rw [hkx, cos_nat_mul_two_pi_add_pi]
    have e : ε * (-1 * (k * 1)) = -(ε * k) := by ring
    rw [e]
    linarith
  have hacc : AccPt x (𝓟 (Ioi 0)) := by
    rw [accPt_iff_nhds]
    intro U hU
    obtain ⟨δ, hδ, hball⟩ := Metric.mem_nhds_iff.mp hU
    refine ⟨x + δ / 2, ⟨hball ?_, ?_⟩, ne_of_gt (by linarith)⟩
    · rw [Metric.mem_ball, Real.dist_eq, show x + δ / 2 - x = δ / 2 by ring,
        abs_of_pos (half_pos hδ)]
      exact half_lt_self hδ
    · show (0 : ℝ) < x + δ / 2
      linarith
  have := hd.hasDerivWithinAt.nonneg_of_monotoneOn hacc hmono
  linarith

end Original

section Ramp

/-- A continuous ramp from 0 to 1 over `[x0, x0 + σ]`. -/
noncomputable def ramp (x0 σ x : ℝ) : ℝ := max 0 (min 1 ((x - x0) / σ))

theorem continuous_ramp (x0 σ : ℝ) : Continuous (ramp x0 σ) :=
  continuous_const.max (continuous_const.min ((continuous_id.sub continuous_const).div_const σ))

theorem ramp_nonneg (x0 σ x : ℝ) : 0 ≤ ramp x0 σ x := le_max_left _ _

theorem ramp_le_one (x0 σ x : ℝ) : ramp x0 σ x ≤ 1 := max_le zero_le_one (min_le_left _ _)

theorem ramp_of_le (x0 σ x : ℝ) (hσ : 0 < σ) (hx : x ≤ x0) : ramp x0 σ x = 0 := by
  have h : (x - x0) / σ ≤ 0 := by rw [div_le_iff₀ hσ]; linarith
  exact max_eq_left (min_le_of_right_le h)

theorem ramp_of_ge (x0 σ x : ℝ) (hσ : 0 < σ) (hx : x0 + σ ≤ x) : ramp x0 σ x = 1 := by
  have h : 1 ≤ (x - x0) / σ := by rw [le_div_iff₀ hσ]; linarith
  unfold ramp
  rw [min_eq_left h, max_eq_right zero_le_one]

theorem ramp_of_mem (x0 σ x : ℝ) (hσ : 0 < σ) (h1 : x0 ≤ x) (h2 : x ≤ x0 + σ) :
    ramp x0 σ x = (x - x0) / σ := by
  have hle : (x - x0) / σ ≤ 1 := by rw [div_le_iff₀ hσ]; linarith
  have hge : 0 ≤ (x - x0) / σ := div_nonneg (by linarith) hσ.le
  unfold ramp
  rw [min_eq_right hle, max_eq_right hge]

/-- The corrected example, `u(x) = log x + x + η ∫₀ˣ ramp`. Its marginal utility is
    `1/x + 1 + η · ramp(x)`, which rises on the ramp when `η/σ` is large. -/
noncomputable def rampU (x0 σ η x : ℝ) : ℝ := Real.log x + x + η * ∫ t in (0 : ℝ)..x, ramp x0 σ t

theorem rampU_hasDerivAt (x0 σ η x : ℝ) (hx : 0 < x) :
    HasDerivAt (rampU x0 σ η) (x⁻¹ + 1 + η * ramp x0 σ x) x :=
  ((Real.hasDerivAt_log hx.ne').add (hasDerivAt_id' x)).add
    (((continuous_ramp x0 σ).integral_hasStrictDerivAt 0 x).hasDerivAt.const_mul η)

theorem rampU_strictMono (x0 σ η : ℝ) (hη : 0 ≤ η) : StrictMonoOn (rampU x0 σ η) (Ioi 0) := by
  refine strictMonoOn_of_deriv_pos (convex_Ioi 0)
    (fun x hx => (rampU_hasDerivAt x0 σ η x hx).continuousAt.continuousWithinAt) (fun x hx => ?_)
  rw [interior_Ioi] at hx
  have hx' : 0 < x := hx
  rw [(rampU_hasDerivAt x0 σ η x hx').deriv]
  have h1 := mul_nonneg hη (ramp_nonneg x0 σ x)
  have h2 := inv_pos.mpr hx'
  linarith

theorem rampU_burden (x0 σ η c : ℝ) (hc : 0 < c) (hσ : 0 < σ) (hη : 0 ≤ η)
    (hbm : η * ((x0 + σ + c) * (x0 + σ)) < c) :
    StrictAntiOn (kappa (rampU x0 σ η) c) (Ioi c) := by
  refine burden_strictAnti_of_deriv (rampU x0 σ η) (fun x => x⁻¹ + 1 + η * ramp x0 σ x) c hc
    (fun x hx => rampU_hasDerivAt x0 σ η x hx) (fun w hw => ?_)
  have hw0 : 0 < w := by linarith
  have hwc : 0 < w - c := by linarith
  have hgap : w⁻¹ + c / (w * (w - c)) = (w - c)⁻¹ := by
    field_simp [hw0.ne', hwc.ne']
    ring
  have hpos : 0 < c / (w * (w - c)) := div_pos hc (mul_pos hw0 hwc)
  show w⁻¹ + 1 + η * ramp x0 σ w < (w - c)⁻¹ + 1 + η * ramp x0 σ (w - c)
  by_cases h1 : w ≤ x0
  · rw [ramp_of_le x0 σ w hσ h1, ramp_of_le x0 σ (w - c) hσ (by linarith)]
    linarith
  by_cases h2 : x0 + σ ≤ w - c
  · rw [ramp_of_ge x0 σ w hσ (by linarith), ramp_of_ge x0 σ (w - c) hσ h2]
    linarith
  push_neg at h1 h2
  have hprod : w * (w - c) < (x0 + σ + c) * (x0 + σ) :=
    mul_lt_mul'' (by linarith) (by linarith) hw0.le hwc.le
  have hlow : η < c / (w * (w - c)) := by
    rw [lt_div_iff₀ (mul_pos hw0 hwc)]
    calc η * (w * (w - c)) ≤ η * ((x0 + σ + c) * (x0 + σ)) :=
          mul_le_mul_of_nonneg_left hprod.le hη
      _ < c := hbm
  linarith [mul_le_of_le_one_right hη (ramp_le_one x0 σ w),
    mul_nonneg hη (ramp_nonneg x0 σ (w - c))]

theorem rampU_kappa_diverges (x0 σ η c : ℝ) (hc : 0 < c) :
    Tendsto (kappa (rampU x0 σ η) c) (𝓝[>] c) atTop := by
  have hprim : Continuous (fun x => ∫ t in (0 : ℝ)..x, ramp x0 σ t) :=
    continuous_iff_continuousAt.mpr (fun x =>
      ((continuous_ramp x0 σ).integral_hasStrictDerivAt 0 x).hasDerivAt.continuousAt)
  have hrest : Continuous (fun x => x + η * ∫ t in (0 : ℝ)..x, ramp x0 σ t) :=
    continuous_id.add (continuous_const.mul hprim)
  have hu0 : Tendsto (rampU x0 σ η) (𝓝[>] 0) atBot := by
    refine (tendsto_log_nhdsGT_zero.atBot_add
      (hrest.continuousAt.tendsto.mono_left nhdsWithin_le_nhds)).congr (fun x => ?_)
    simp only [rampU]
    ring
  exact (kappa_diverges_iff (rampU x0 σ η) c
    (rampU_hasDerivAt x0 σ η c hc).continuousAt).mpr hu0

theorem rampU_strictConvex (x0 σ η : ℝ) (hx0 : 0 < x0) (hσ : 0 < σ) (hconv : σ < η * x0 ^ 2) :
    StrictConvexOn ℝ (Icc x0 (x0 + σ)) (rampU x0 σ η) := by
  refine StrictMonoOn.strictConvexOn_of_deriv (convex_Icc _ _)
    (fun x hx => (rampU_hasDerivAt x0 σ η x (by linarith [hx.1])).continuousAt.continuousWithinAt)
    ?_
  rw [interior_Icc]
  intro x hx y hy hxy
  have hx0' : 0 < x := by linarith [hx.1]
  have hy0' : 0 < y := by linarith [hy.1]
  rw [(rampU_hasDerivAt x0 σ η x hx0').deriv, (rampU_hasDerivAt x0 σ η y hy0').deriv,
    ramp_of_mem x0 σ x hσ hx.1.le hx.2.le, ramp_of_mem x0 σ y hσ hy.1.le hy.2.le]
  have hxy0 : 0 < y - x := by linarith
  have e1 : x⁻¹ - y⁻¹ = (y - x) / (x * y) := inv_sub_inv hx0'.ne' hy0'.ne'
  have hprod : x0 ^ 2 < x * y := by nlinarith [hx.1, hy.1]
  have h1 : (y - x) / (x * y) < (y - x) / x0 ^ 2 :=
    div_lt_div_of_pos_left hxy0 (by positivity) hprod
  have h2 : (y - x) / x0 ^ 2 < η * ((y - x) / σ) := by
    rw [div_lt_iff₀ (by positivity)]
    have e : η * ((y - x) / σ) * x0 ^ 2 = (y - x) * (η * x0 ^ 2 / σ) := by ring
    rw [e]
    have h3 : 1 < η * x0 ^ 2 / σ := by rw [one_lt_div hσ]; exact hconv
    nlinarith
  have e2 : η * ((y - x0) / σ) - η * ((x - x0) / σ) = η * ((y - x) / σ) := by ring
  linarith

theorem rampU_not_concave (x0 σ η : ℝ) (hx0 : 0 < x0) (hσ : 0 < σ) (hconv : σ < η * x0 ^ 2) :
    ¬ ConcaveOn ℝ (Ioi 0) (rampU x0 σ η) := by
  intro hcc
  have hsc := rampU_strictConvex x0 σ η hx0 hσ hconv
  have ha : x0 ∈ Icc x0 (x0 + σ) := ⟨le_refl _, by linarith⟩
  have hb : x0 + σ ∈ Icc x0 (x0 + σ) := ⟨by linarith, le_refl _⟩
  have h1 := hsc.2 ha hb (ne_of_lt (by linarith)) (by norm_num : (0 : ℝ) < 1 / 2)
    (by norm_num : (0 : ℝ) < 1 / 2) (by norm_num)
  have h2 := hcc.2 (show x0 ∈ Ioi (0 : ℝ) from hx0) (show x0 + σ ∈ Ioi (0 : ℝ) by
    show (0 : ℝ) < x0 + σ; linarith) (by norm_num : (0 : ℝ) ≤ 1 / 2)
    (by norm_num : (0 : ℝ) ≤ 1 / 2) (by norm_num)
  linarith

/-- The parameters `(x0, σ, η)` for which the corrected example is admissible and non-concave. -/
def admissibleParams (c : ℝ) : Set (ℝ × ℝ × ℝ) :=
  {p | 0 < p.1} ∩ {p | 0 < p.2.1} ∩ {p | p.2.1 < p.2.2 * p.1 ^ 2}
    ∩ {p | p.2.2 * ((p.1 + p.2.1 + c) * (p.1 + p.2.1)) < c}

/-- **The example is not knife-edge.** The admissible parameters form an open set. -/
theorem admissibleParams_open (c : ℝ) : IsOpen (admissibleParams c) := by
  unfold admissibleParams
  refine (((isOpen_lt continuous_const continuous_fst).inter
    (isOpen_lt continuous_const (continuous_fst.comp continuous_snd))).inter
    (isOpen_lt (continuous_fst.comp continuous_snd) ?_)).inter (isOpen_lt ?_ continuous_const)
  · exact (continuous_snd.comp continuous_snd).mul (continuous_fst.pow 2)
  · exact (continuous_snd.comp continuous_snd).mul
      (((continuous_fst.add (continuous_fst.comp continuous_snd)).add continuous_const).mul
        (continuous_fst.add (continuous_fst.comp continuous_snd)))

/-- Every admissible parameter gives a strictly increasing, non-concave utility whose burden is
    strictly decreasing and diverges at `c`. -/
theorem rampU_admissible (c : ℝ) (hc : 0 < c) (p : ℝ × ℝ × ℝ) (hp : p ∈ admissibleParams c) :
    StrictMonoOn (rampU p.1 p.2.1 p.2.2) (Ioi 0)
      ∧ StrictConvexOn ℝ (Icc p.1 (p.1 + p.2.1)) (rampU p.1 p.2.1 p.2.2)
      ∧ ¬ ConcaveOn ℝ (Ioi 0) (rampU p.1 p.2.1 p.2.2)
      ∧ StrictAntiOn (kappa (rampU p.1 p.2.1 p.2.2) c) (Ioi c)
      ∧ Tendsto (kappa (rampU p.1 p.2.1 p.2.2) c) (𝓝[>] c) atTop := by
  obtain ⟨⟨⟨h1, h2⟩, h3⟩, h4⟩ := hp
  have hx0 : 0 < p.1 := h1
  have hσ : 0 < p.2.1 := h2
  have hconv : p.2.1 < p.2.2 * p.1 ^ 2 := h3
  have hbm : p.2.2 * ((p.1 + p.2.1 + c) * (p.1 + p.2.1)) < c := h4
  have hη : 0 ≤ p.2.2 := by
    by_contra h
    have : p.2.2 * p.1 ^ 2 ≤ 0 := mul_nonpos_of_nonpos_of_nonneg (le_of_lt (not_le.mp h))
      (sq_nonneg _)
    linarith
  exact ⟨rampU_strictMono _ _ _ hη, rampU_strictConvex _ _ _ hx0 hσ hconv,
    rampU_not_concave _ _ _ hx0 hσ hconv, rampU_burden _ _ _ c hc hσ hη hbm,
    rampU_kappa_diverges _ _ _ c hc⟩

/-- **Every width below `c` is admissible.** For each fee `c > 0` and width `σ < c` there are
    parameters with a convex stretch of width `σ`. With `not_burden_of_convex_stretch`, the
    supremum of admissible convex widths is exactly `c`. -/
theorem admissible_width (c σ : ℝ) (hc : 0 < c) (hσ : 0 < σ) (hσc : σ < c) :
    ∃ x0 η, (x0, σ, η) ∈ admissibleParams c := by
  have hcs : 0 < c - σ := by linarith
  set A := σ * (2 * σ + c) + σ ^ 2 * (σ + c) with hAdef
  have hA : 0 ≤ A := by positivity
  set x0 := (A + 1) / (c - σ) + 1 with hx0def
  have hx01 : 1 ≤ x0 := by
    have := div_nonneg (by linarith : 0 ≤ A + 1) hcs.le
    linarith
  have hx0pos : 0 < x0 := by linarith
  have hlin : (c - σ) * x0 = A + 1 + (c - σ) := by
    rw [hx0def, mul_add, mul_div_cancel₀ _ hcs.ne', mul_one]
  have hsq : (c - σ) * x0 * x0 = (A + 1 + (c - σ)) * x0 := by rw [hlin]
  have hP : 0 < (x0 + σ + c) * (x0 + σ) := by positivity
  have key : σ * ((x0 + σ + c) * (x0 + σ)) < c * x0 ^ 2 := by
    have k1 : 0 ≤ σ ^ 2 * (σ + c) * (x0 - 1) :=
      mul_nonneg (by positivity) (by linarith)
    have k2 : 0 < x0 * (1 + c - σ) := mul_pos hx0pos (by linarith)
    rw [hAdef] at hsq
    nlinarith
  have hx0sq : 0 < x0 ^ 2 := by positivity
  have hLR : σ / x0 ^ 2 < c / ((x0 + σ + c) * (x0 + σ)) := by
    rw [div_lt_div_iff₀ hx0sq hP]
    linarith
  set η := (σ / x0 ^ 2 + c / ((x0 + σ + c) * (x0 + σ))) / 2 with hηdef
  have hL : σ / x0 ^ 2 < η := by rw [hηdef]; linarith
  have hR : η < c / ((x0 + σ + c) * (x0 + σ)) := by rw [hηdef]; linarith
  refine ⟨x0, η, ⟨⟨⟨hx0pos, hσ⟩, ?_⟩, ?_⟩⟩
  · show σ < η * x0 ^ 2
    exact (div_lt_iff₀ hx0sq).mp hL
  · show η * ((x0 + σ + c) * (x0 + σ)) < c
    exact (lt_div_iff₀ hP).mp hR

/-- **Claim BM, corrected.** For every fee `c > 0` there is a strictly increasing utility that is
    not concave, whose burden is strictly decreasing and diverges at `c`. The admissible class is
    therefore strictly larger than the concave class. -/
theorem bm_strictly_weaker (c : ℝ) (hc : 0 < c) :
    ∃ u : ℝ → ℝ, StrictMonoOn u (Ioi 0) ∧ ¬ ConcaveOn ℝ (Ioi 0) u
      ∧ StrictAntiOn (kappa u c) (Ioi c) ∧ Tendsto (kappa u c) (𝓝[>] c) atTop := by
  obtain ⟨x0, η, hp⟩ := admissible_width c (c / 2) hc (by linarith) (by linarith)
  obtain ⟨h1, _, h3, h4, h5⟩ := rampU_admissible c hc (x0, c / 2, η) hp
  exact ⟨_, h1, h3, h4, h5⟩

end Ramp

end EntryContestBM
