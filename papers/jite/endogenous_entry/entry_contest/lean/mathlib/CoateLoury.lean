import Mathlib

/-!
# Coate and Loury, "Will Affirmative-Action Policies Eliminate Negative Stereotypes?"

American Economic Review 83(5), 1220-1240 (December 1993).  Page numbers are the AER pages.

Setup (pp.1223-1225): a worker invests at a private cost `c`, drawn from `G`, to become
qualified for task one.  The employer sees a signal `θ` with densities `f_q` (qualified) and
`f_u` (unqualified) and likelihood ratio `φ = f_u/f_q`, non-increasing.  A qualified worker
in task one earns the employer `x_q`, an unqualified one costs `x_u`, and `r = x_q/x_u`.
Every worker gains `ω` from task one.  Facing the standard `s`, a worker gains
`β(s) = ω (F_u(s) - F_q(s))` by investing, and a self-confirming belief solves
(3) `π = G(β(s*(π)))`.  Affirmative action requires equal rates of assignment to task one.

## What is proved

1. The employer's rule (p.1224, footnote 18): the posterior (1) (`posterior_eq`), the rule
   "assign iff `r ≥ [(1-π)/π] φ`" (`assign_iff`), the derivative of the employer's payoff in
   the standard and its sign (`P_hasDerivAt`, `dP_pos_iff`), and the EE curve
   `π = φ/(r+φ)` (`ee_iff`).
2. The benefit of investing (p.1225): `β' = ω (f_u - f_q)`, positive exactly when `φ > 1`
   (`beta_hasDerivAt`, `beta_deriv_pos_iff`).
3. Proposition 1 (p.1226): if the WW curve rises above the EE curve somewhere, the two cross
   twice at a positive belief (`prop1`).
4. Proposition 2 and eq. (5) (p.1229-1230): a strictly decreasing assignment rate forces equal
   standards and beliefs (`prop2`); the derivative of the assignment rate is
   `f_q (φ(η-1) - η)` with `η(c) = d[c G(c)]/dc` (`rhoHat_hasDerivAt`, `rhoHat_deriv_eq`), so
   it is negative where `φ ≤ 1` (`rhoHat_deriv_neg_of_le_one`) and, where `φ > 1`, exactly
   when (5) holds (`eq5_iff`); the left side of (5) rises as `φ` falls (`lhs5_anti`); the
   uniform and exponential cost examples (`eta_uniform`, `uniform_sufficient`, `eta_exp`,
   `eta_exp_le`, `eta_exp_at_max`, `exp_threshold_iff`).
5. The uniform example (pp.1230-1232): the benefits of investing under liberal and
   conservative employers are `ω(1 - p_u)` and `ω(1 - p_q)` (`beta_liberal`,
   `beta_conservative`), the likelihood ratio on the unclear band is `p_u/p_q`
   (`psi_eq`), employers are liberal exactly when `π ≥ π̂` (`liberal_iff`), with `π̂` as in
   (6) (`pihat_eq`); (6) makes `β` fall on the unclear band (`six_beta_falls`); both
   investment rates fall as the unclear band widens (`unclear_band_mono`,
   `rates_fall_with_band`).
6. Proposition 3 (pp.1231-1232): the patronizing probability (7) (`alpha_eq`), the two
   equilibrium beliefs of (8) (`eq8_iff`), the map of (9) (`nine_eq`, `T_sub`), the
   convergence of the adjustment process (9) to `1 - π_l` from any initial belief in
   `[0, π_l)` (`iter_tendsto`), the slopes at the two fixed
   points (`T_hasDerivAt`, `patronizing_stable`, `colorblind_unstable`), the worsening
   condition (`worsens_iff`), footnote 20's threshold `λ̂` (`lambda_hat_iff`,
   `lambda_hat_lt_one`) and footnote 21's numbers (`fn21_pihat`, `fn21`, `fn21_nonempty`,
   `fn21_worse_iff`).
7. Section III (pp.1233-1234): the first-order conditions (11a) and (11b) (`rho_hasDerivAt`,
   `foc_w_iff`, `foc_b_iff`) and the two analytic steps of Proposition 4's proof
   (`prop4_D_hasDerivAt`, `prop4_D_deriv_neg`, `prop4_root`).

## Not formalised

The existence of equilibria in Definitions 1 and 2, the implicit-function step of
Proposition 4, the local stability of solutions of (3) on p.1226, and the graphical arguments
of Section IV (unequal cost distributions and subsidies).
-/

open Set Filter Topology

namespace CoateLoury1993

/-! ## 1. The employer's rule (p.1224, footnote 18) -/

/-- The posterior (1), p.1224. -/
theorem posterior_eq (π fq fu : ℝ) (hπ0 : 0 < π) (hπ1 : π < 1) (hq : 0 < fq) (hu : 0 ≤ fu) :
    π * fq / (π * fq + (1 - π) * fu) = 1 / (1 + (1 - π) / π * (fu / fq)) := by
  have h1 : 0 < 1 - π := by linarith
  have hden : 0 < π * fq + (1 - π) * fu := by positivity
  rw [eq_div_iff (by positivity), div_mul_eq_mul_div, div_eq_one_iff_eq hden.ne']
  field_simp

/-- p.1224: the employer assigns to task one exactly when `r ≥ [(1-π)/π] φ`. -/
theorem assign_iff (π fq fu xq xu : ℝ) (hπ0 : 0 < π) (hπ1 : π < 1) (hq : 0 < fq) (hu : 0 ≤ fu)
    (hxu : 0 < xu) :
    0 ≤ π * fq / (π * fq + (1 - π) * fu) * xq - (1 - π * fq / (π * fq + (1 - π) * fu)) * xu ↔
      (1 - π) / π * (fu / fq) ≤ xq / xu := by
  have h1 : 0 < 1 - π := by linarith
  have hden : 0 < π * fq + (1 - π) * fu := by positivity
  have e : π * fq / (π * fq + (1 - π) * fu) * xq - (1 - π * fq / (π * fq + (1 - π) * fu)) * xu
      = (π * fq * xq - (1 - π) * fu * xu) / (π * fq + (1 - π) * fu) := by
    field_simp
    ring
  rw [e, div_nonneg_iff, div_mul_div_comm, div_le_div_iff₀ (by positivity) hxu]
  constructor
  · rintro (⟨h, -⟩ | ⟨-, h⟩)
    · nlinarith
    · linarith
  · intro h
    left
    exact ⟨by nlinarith, hden.le⟩

/-- The employer's expected payoff per worker from the standard `s`, p.1228. -/
noncomputable def P (Fq Fu : ℝ → ℝ) (xq xu π s : ℝ) : ℝ :=
  π * (1 - Fq s) * xq - (1 - π) * (1 - Fu s) * xu

theorem P_hasDerivAt (Fq Fu : ℝ → ℝ) (fq fu xq xu π s : ℝ) (hq : HasDerivAt Fq fq s)
    (hu : HasDerivAt Fu fu s) :
    HasDerivAt (P Fq Fu xq xu π) (-(π * fq * xq) + (1 - π) * fu * xu) s := by
  have h := ((((hq.const_sub 1).const_mul π).mul_const xq)).sub
    (((hu.const_sub 1).const_mul (1 - π)).mul_const xu)
  refine h.congr_deriv ?_
  ring

/-- Footnote 18: `∂P/∂s > 0` exactly when `r < [(1-π)/π] φ(s)`. -/
theorem dP_pos_iff (π fq fu xq xu : ℝ) (hπ0 : 0 < π) (hq : 0 < fq) (hxu : 0 < xu) :
    0 < -(π * fq * xq) + (1 - π) * fu * xu ↔ xq / xu < (1 - π) / π * (fu / fq) := by
  rw [div_mul_div_comm, div_lt_div_iff₀ hxu (by positivity)]
  constructor <;> intro h <;> nlinarith

/-- Proof of Proposition 1: `(s, π)` is on the EE curve exactly when `π = φ/(r+φ)`. -/
theorem ee_iff (π φ r : ℝ) (hπ : π ≠ 0) (hr : 0 < r) (hφ : 0 ≤ φ) :
    r = (1 - π) / π * φ ↔ π = φ / (r + φ) := by
  rw [eq_div_iff (by positivity : r + φ ≠ 0)]
  constructor
  · intro h
    rw [h]
    field_simp
    ring
  · intro h
    field_simp
    linarith

/-! ## 2. The benefit of investing (p.1225) -/

theorem beta_hasDerivAt (Fq Fu : ℝ → ℝ) (fq fu ω s : ℝ) (hq : HasDerivAt Fq fq s)
    (hu : HasDerivAt Fu fu s) :
    HasDerivAt (fun t => ω * (Fu t - Fq t)) (ω * (fu - fq)) s :=
  (hu.sub hq).const_mul ω

/-- `β` rises exactly where the likelihood ratio exceeds one. -/
theorem beta_deriv_pos_iff (ω fq fu : ℝ) (hω : 0 < ω) (hq : 0 < fq) :
    0 < ω * (fu - fq) ↔ 1 < fu / fq := by
  rw [one_lt_div hq, mul_pos_iff_of_pos_left hω, sub_pos]

/-! ## 3. Proposition 1 (p.1226) -/

/-- Proposition 1: if the WW curve `W` lies above the EE curve `φ/(r+φ)` at `s₀` and below
it at `a < s₀ < b`, the two curves cross on each side of `s₀` at a positive belief. -/
theorem prop1 (φ W : ℝ → ℝ) (r a s₀ b : ℝ) (hr : 0 < r) (hφ : ∀ s ∈ Icc a b, 0 < φ s)
    (ha : a < s₀) (hb : s₀ < b) (hc : ContinuousOn (fun s => φ s / (r + φ s) - W s) (Icc a b))
    (h_a : W a < φ a / (r + φ a)) (h_s₀ : φ s₀ / (r + φ s₀) < W s₀)
    (h_b : W b < φ b / (r + φ b)) :
    ∃ s₁ ∈ Ioo a s₀, ∃ s₂ ∈ Ioo s₀ b, W s₁ = φ s₁ / (r + φ s₁) ∧ W s₂ = φ s₂ / (r + φ s₂) ∧
      0 < W s₁ ∧ 0 < W s₂ := by
  set h := fun s => φ s / (r + φ s) - W s
  have hl : (0 : ℝ) ∈ Ioo (h s₀) (h a) := ⟨by simp [h]; linarith, by simp [h]; linarith⟩
  have hr' : (0 : ℝ) ∈ Ioo (h s₀) (h b) := ⟨by simp [h]; linarith, by simp [h]; linarith⟩
  obtain ⟨s₁, hs₁, e₁⟩ :=
    intermediate_value_Ioo' ha.le (hc.mono (Icc_subset_Icc le_rfl hb.le)) hl
  obtain ⟨s₂, hs₂, e₂⟩ :=
    intermediate_value_Ioo hb.le (hc.mono (Icc_subset_Icc ha.le le_rfl)) hr'
  have w₁ : W s₁ = φ s₁ / (r + φ s₁) := by simp only [h] at e₁; linarith
  have w₂ : W s₂ = φ s₂ / (r + φ s₂) := by simp only [h] at e₂; linarith
  have p₁ := hφ s₁ ⟨hs₁.1.le, (hs₁.2.trans hb).le⟩
  have p₂ := hφ s₂ ⟨(ha.trans hs₂.1).le, hs₂.2.le⟩
  exact ⟨s₁, hs₁, s₂, hs₂, w₁, w₂, by rw [w₁]; positivity, by rw [w₂]; positivity⟩

/-! ## 4. Proposition 2 and eq. (5) (pp.1229-1230) -/

/-- Proposition 2: if the equilibrium assignment rate `ρ̂` is strictly decreasing on `[0,1]`,
equal assignment rates force equal standards, and so equal beliefs `π_i = G(β(s_i))`. -/
theorem prop2 (ρhat G β : ℝ → ℝ) (sb sw : ℝ) (hdec : StrictAntiOn ρhat (Icc 0 1))
    (hb : sb ∈ Icc (0 : ℝ) 1) (hw : sw ∈ Icc (0 : ℝ) 1) (heq : ρhat sb = ρhat sw) :
    sb = sw ∧ G (β sb) = G (β sw) := by
  have := hdec.injOn hb hw heq
  exact ⟨this, by rw [this]⟩

/-- The rate at which a group facing the standard `s` is assigned to task one when its
members invest at the rate `G(β(s))`, p.1229. -/
noncomputable def rhoHat (G Fq Fu : ℝ → ℝ) (ω s : ℝ) : ℝ :=
  G (ω * (Fu s - Fq s)) * (1 - Fq s) + (1 - G (ω * (Fu s - Fq s))) * (1 - Fu s)

theorem rhoHat_hasDerivAt (G g Fq Fu : ℝ → ℝ) (fq fu ω s : ℝ) (hq : HasDerivAt Fq fq s)
    (hu : HasDerivAt Fu fu s)
    (hG : HasDerivAt G (g (ω * (Fu s - Fq s))) (ω * (Fu s - Fq s))) :
    HasDerivAt (rhoHat G Fq Fu ω)
      (g (ω * (Fu s - Fq s)) * (ω * (fu - fq)) * (Fu s - Fq s)
        - G (ω * (Fu s - Fq s)) * fq - (1 - G (ω * (Fu s - Fq s))) * fu) s := by
  have hπ : HasDerivAt (fun t => G (ω * (Fu t - Fq t))) (g (ω * (Fu s - Fq s)) * (ω * (fu - fq))) s :=
    hG.comp s ((hu.sub hq).const_mul ω)
  have h := (hπ.mul (hq.const_sub 1)).add ((hπ.const_sub 1).mul (hu.const_sub 1))
  unfold rhoHat
  refine h.congr_deriv ?_
  ring

/-- p.1229: with `b = β(s)`, `φ = f_u/f_q` and `η(b) = G(b) + b g(b) = d[c G(c)]/dc` at `b`,
the derivative of `ρ̂` is `f_q (φ(η - 1) - η)`. -/
theorem rhoHat_deriv_eq (Gb gb fq fu ω D : ℝ) (hq : fq ≠ 0) :
    gb * (ω * (fu - fq)) * D - Gb * fq - (1 - Gb) * fu
      = fq * (fu / fq * ((Gb + ω * D * gb) - 1) - (Gb + ω * D * gb)) := by
  field_simp
  ring

/-- Where `φ ≤ 1` (that is, `s ≥ ŝ`), `ρ̂` is decreasing. -/
theorem rhoHat_deriv_neg_of_le_one (fq φ η : ℝ) (hq : 0 < fq) (hφ0 : 0 < φ) (hφ1 : φ ≤ 1)
    (hη : 0 ≤ η) : fq * (φ * (η - 1) - η) < 0 := by
  apply mul_neg_of_pos_of_neg hq
  nlinarith

/-- Eq. (5): where `φ > 1` (that is, `s < ŝ`), `ρ̂' < 0` exactly when
`φ/(φ - 1) > η(β(s))`. -/
theorem eq5_iff (fq φ η : ℝ) (hq : 0 < fq) (hφ : 1 < φ) :
    fq * (φ * (η - 1) - η) < 0 ↔ η < φ / (φ - 1) := by
  rw [mul_neg_iff, lt_div_iff₀ (by linarith)]
  constructor
  · rintro (⟨-, h⟩ | ⟨h, -⟩)
    · nlinarith
    · linarith
  · intro h
    left
    exact ⟨hq, by nlinarith⟩

/-- p.1230: the left side of (5) rises as `φ` falls. -/
theorem lhs5_anti (φ₁ φ₂ : ℝ) (h1 : 1 < φ₁) (h12 : φ₁ < φ₂) : φ₂ / (φ₂ - 1) < φ₁ / (φ₁ - 1) := by
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith

/-- Uniform costs on `[0, 2μ]`: `η(c) = c/μ`. -/
theorem eta_uniform (μ c : ℝ) (hμ : μ ≠ 0) :
    HasDerivAt (fun x => x * (x / (2 * μ))) (c / μ) c := by
  have h := (hasDerivAt_id c).mul ((hasDerivAt_id c).div_const (2 * μ))
  refine h.congr_deriv ?_
  simp only [id]
  field_simp
  ring

/-- p.1230: with uniform costs on `[0, 2μ]`, either `φ(0) < 2` or `β(ŝ) ≤ μ` gives (5). -/
theorem uniform_sufficient (φ₀ b μ : ℝ) (hμ : 0 < μ) (hφ : 1 < φ₀) (hb2 : b ≤ 2 * μ)
    (h : φ₀ < 2 ∨ b ≤ μ) : b / μ < φ₀ / (φ₀ - 1) := by
  have hpos : 0 < φ₀ - 1 := by linarith
  have one_lt : 1 < φ₀ / (φ₀ - 1) := by rw [one_lt_div hpos]; linarith
  rcases h with h | h
  · have two_lt : 2 < φ₀ / (φ₀ - 1) := by rw [lt_div_iff₀ hpos]; linarith
    have : b / μ ≤ 2 := by rw [div_le_iff₀ hμ]; linarith
    linarith
  · have : b / μ ≤ 1 := by rw [div_le_one hμ]; exact h
    linarith

/-- Exponential costs with mean `μ`: `η(c) = (c/μ - 1) e^{-c/μ} + 1`. -/
theorem eta_exp (μ c : ℝ) (hμ : μ ≠ 0) :
    HasDerivAt (fun x => x * (1 - Real.exp (-x / μ)))
      ((c / μ - 1) * Real.exp (-c / μ) + 1) c := by
  have he : HasDerivAt (fun x => Real.exp (-x / μ)) (Real.exp (-c / μ) * (-1 / μ)) c := by
    have := ((hasDerivAt_id c).neg.div_const μ).exp
    simpa [id, neg_div] using this
  have h := (hasDerivAt_id c).mul (he.const_sub 1)
  refine h.congr_deriv ?_
  simp only [id]
  field_simp
  ring

/-- p.1230: `η(c) ≤ 1 + e^{-2}` for exponential costs. -/
theorem eta_exp_le (μ c : ℝ) :
    (c / μ - 1) * Real.exp (-c / μ) + 1 ≤ 1 + Real.exp (-2) := by
  have key : c / μ - 1 ≤ Real.exp (c / μ - 2) := by
    have := Real.add_one_le_exp (c / μ - 2)
    linarith
  have hx : Real.exp (c / μ - 2) * Real.exp (-c / μ) = Real.exp (-2) := by
    rw [← Real.exp_add]
    congr 1
    ring
  have := mul_le_mul_of_nonneg_right key (Real.exp_pos (-c / μ)).le
  linarith

/-- The maximum is attained at `c = 2μ`. -/
theorem eta_exp_at_max (μ : ℝ) (hμ : 0 < μ) :
    (2 * μ / μ - 1) * Real.exp (-(2 * μ) / μ) + 1 = 1 + Real.exp (-2) := by
  have : 2 * μ / μ = 2 := by field_simp
  have h2 : -(2 * μ) / μ = -2 := by field_simp
  rw [this, h2]
  ring

/-- p.1230: for `φ(0) > 1`, `φ(0)/(φ(0)-1) > 1 + e^{-2}` exactly when `φ(0) < 1 + e^2`. -/
theorem exp_threshold_iff (φ₀ : ℝ) (hφ : 1 < φ₀) :
    1 + Real.exp (-2) < φ₀ / (φ₀ - 1) ↔ φ₀ < 1 + Real.exp 2 := by
  have hpos : 0 < φ₀ - 1 := by linarith
  have hinv : Real.exp (-2) * Real.exp 2 = 1 := by rw [← Real.exp_add]; norm_num
  have e2 := Real.exp_pos 2
  have em2 := Real.exp_pos (-2)
  rw [lt_div_iff₀ hpos]
  constructor
  · intro h
    nlinarith
  · intro h
    nlinarith

/-! ## 5. The uniform example (pp.1230-1231) -/

/-- The probability that a qualified worker's test is unclear: signal uniform on `[θ_q, 1]`. -/
noncomputable def pq (θq θu : ℝ) : ℝ := (θu - θq) / (1 - θq)

/-- The probability that an unqualified worker's test is unclear: signal uniform on
`[0, θ_u]`. -/
noncomputable def pu (θq θu : ℝ) : ℝ := (θu - θq) / θu

/-- The signal CDFs of the example. -/
noncomputable def FqU (θq s : ℝ) : ℝ := max 0 ((s - θq) / (1 - θq))
noncomputable def FuU (θu s : ℝ) : ℝ := min 1 (s / θu)

/-- A liberal employer uses the standard `θ_q`; the benefit of investing is `ω(1 - p_u)`. -/
theorem beta_liberal (ω θq θu : ℝ) (h0 : 0 ≤ θq) (hqu : θq < θu) :
    ω * (FuU θu θq - FqU θq θq) = ω * (1 - pu θq θu) := by
  have hu : 0 < θu := by linarith
  have : θq / θu ≤ 1 := by rw [div_le_one hu]; linarith
  simp only [FuU, FqU, pu, sub_self, zero_div, max_self, min_eq_right this]
  field_simp
  ring

/-- A conservative employer uses the standard `θ_u`; the benefit is `ω(1 - p_q)`. -/
theorem beta_conservative (ω θq θu : ℝ) (h0 : 0 ≤ θq) (hqu : θq < θu) (h1 : θu < 1) :
    ω * (FuU θu θu - FqU θq θu) = ω * (1 - pq θq θu) := by
  have hu : 0 < θu := by linarith
  have h1q : 0 < 1 - θq := by linarith
  have : 0 ≤ (θu - θq) / (1 - θq) := div_nonneg (by linarith) h1q.le
  simp only [FuU, FqU, pq, div_self hu.ne', min_self, max_eq_right this]

/-- p.1230: the likelihood ratio on the unclear band, `(1-θ_q)/θ_u`, is `p_u/p_q`. -/
theorem psi_eq (θq θu : ℝ) (hqu : θq < θu) (h1 : θu < 1) (h0 : 0 ≤ θq) :
    pu θq θu / pq θq θu = (1 - θq) / θu := by
  have hu : 0 < θu := by linarith
  have h1q : 0 < 1 - θq := by linarith
  have hd : 0 < θu - θq := by linarith
  simp only [pu, pq]
  field_simp

/-- p.1230: with `ξ` the posterior after an unclear result, the employer is liberal exactly
when `π ≥ π̂ = ψ/(r+ψ)`, `ψ = p_u/p_q`. -/
theorem liberal_iff (π pq pu xq xu : ℝ) (hπ0 : 0 < π) (hπ1 : π < 1) (hq : 0 < pq) (hu : 0 < pu)
    (hxq : 0 < xq) (hxu : 0 < xu) :
    (let ξ := π * pq / (π * pq + (1 - π) * pu); (1 - ξ) * xu ≤ ξ * xq) ↔
      (pu / pq) / (xq / xu + pu / pq) ≤ π := by
  simp only
  rw [← sub_nonneg, assign_iff π pq pu xq xu hπ0 hπ1 hq hu.le hxu]
  have hr : 0 < xq / xu := by positivity
  have hψ : 0 < pu / pq := by positivity
  rw [div_le_iff₀ (by positivity), div_mul_eq_mul_div, div_le_iff₀ hπ0]
  constructor <;> intro h <;> nlinarith

/-- Condition (6): `π̂ = x_u p_u/[x_q p_q + x_u p_u]`. -/
theorem pihat_eq (pq pu xq xu : ℝ) (hq : 0 < pq) (hu : 0 < pu) (hxq : 0 < xq) (hxu : 0 < xu) :
    (pu / pq) / (xq / xu + pu / pq) = xu * pu / (xq * pq + xu * pu) := by
  field_simp

/-- p.1231: (6) gives `π_c < π_l`, so `p_u < p_q` and the likelihood ratio on the unclear band
is below one; `β` then falls across the band, and incentives peak at the liberal standard. -/
theorem six_beta_falls (ω θq θu s s' : ℝ) (hω : 0 < ω) (h0 : 0 ≤ θq) (hqu : θq < θu)
    (h1 : θu < 1) (h6 : ω * (1 - pq θq θu) < ω * (1 - pu θq θu)) (hs : θq ≤ s) (hss : s ≤ s')
    (hs' : s' ≤ θu) :
    ω * (FuU θu s' - FqU θq s') ≤ ω * (FuU θu s - FqU θq s) := by
  have hu : 0 < θu := by linarith
  have h1q : 0 < 1 - θq := by linarith
  have hd : 0 < θu - θq := by linarith
  have hlt : pu θq θu < pq θq θu := by nlinarith
  simp only [pu, pq] at hlt
  rw [div_lt_div_iff_of_pos_left hd hu h1q] at hlt
  have m1 : ∀ t, t ≤ θu → FuU θu t = t / θu := fun t ht =>
    min_eq_right (by rw [div_le_one hu]; exact ht)
  have m2 : ∀ t, θq ≤ t → FqU θq t = (t - θq) / (1 - θq) := fun t ht =>
    max_eq_right (div_nonneg (by linarith) h1q.le)
  rw [m1 s' hs', m2 s' (hs.trans hss), m1 s (hss.trans hs'), m2 s hs]
  apply mul_le_mul_of_nonneg_left _ hω.le
  have e : s' / θu - (s' - θq) / (1 - θq) - (s / θu - (s - θq) / (1 - θq))
      = (s' - s) * ((1 - θq) - θu) / (θu * (1 - θq)) := by
    field_simp
    ring
  have : (s' - s) * ((1 - θq) - θu) / (θu * (1 - θq)) ≤ 0 :=
    div_nonpos_of_nonpos_of_nonneg (mul_nonpos_of_nonneg_of_nonpos (by linarith) (by linarith))
      (by positivity)
  linarith

/-- Widening the unclear band `[θ_q, θ_u]` (a less accurate test) raises both `p_u` and
`p_q`. -/
theorem unclear_band_mono (θq θu θq' θu' : ℝ) (h0 : 0 < θq') (hq : θq' ≤ θq) (hqu : θq < θu)
    (hu : θu ≤ θu') (h1 : θu' < 1) :
    pu θq θu ≤ pu θq' θu' ∧ pq θq θu ≤ pq θq' θu' := by
  have hθu : 0 < θu := by linarith
  have hθu' : 0 < θu' := by linarith
  have h1q : 0 < 1 - θq := by linarith
  have h1q' : 0 < 1 - θq' := by linarith
  simp only [pu, pq]
  constructor
  · rw [div_le_div_iff₀ hθu hθu']
    nlinarith
  · rw [div_le_div_iff₀ h1q h1q']
    nlinarith

/-- Hence the investment rates `π_l = ω(1-p_u)` and `π_c = ω(1-p_q)` fall as the test becomes
less accurate. -/
theorem rates_fall_with_band (ω θq θu θq' θu' : ℝ) (hω : 0 ≤ ω) (h0 : 0 < θq') (hq : θq' ≤ θq)
    (hqu : θq < θu) (hu : θu ≤ θu') (h1 : θu' < 1) :
    ω * (1 - pu θq' θu') ≤ ω * (1 - pu θq θu) ∧ ω * (1 - pq θq' θu') ≤ ω * (1 - pq θq θu) := by
  obtain ⟨a, b⟩ := unclear_band_mono θq θu θq' θu' h0 hq hqu hu h1
  exact ⟨mul_le_mul_of_nonneg_left (by linarith) hω, mul_le_mul_of_nonneg_left (by linarith) hω⟩

/-! ## 6. Proposition 3 (pp.1231-1232) -/

/-- Eq. (7): the probability `α` of patronizing a failing B that equates the assignment
rates. -/
theorem alpha_eq (πl πb pu α : ℝ) (hpu : pu < 1) (hπb : πb < 1) :
    πl + (1 - πl) * pu = πb + (1 - πb) * (pu + (1 - pu) * α) ↔ α = (πl - πb) / (1 - πb) := by
  have h1 : (1 - πb) * (1 - pu) ≠ 0 := mul_ne_zero (by linarith) (by linarith)
  rw [eq_div_iff (by linarith : (1 - πb) ≠ 0)]
  constructor
  · intro h
    have : (1 - πb) * (1 - pu) * α = (πl - πb) * (1 - pu) := by linarith
    have h2 := mul_right_cancel₀ (by linarith : (1 - pu) ≠ 0)
      (show α * (1 - πb) * (1 - pu) = (πl - πb) * (1 - pu) by linarith)
    linarith
  · intro h
    have : (1 - πb) * (1 - pu) * α = (πl - πb) * (1 - pu) := by
      calc (1 - πb) * (1 - pu) * α = (α * (1 - πb)) * (1 - pu) := by ring
        _ = (πl - πb) * (1 - pu) := by rw [h]
    linarith

/-- Eq. (8): with `π_l = ω(1-p_u)` and B's investing at `ω(1-α)(1-p_u)`, the equilibrium
beliefs about B's are `π_l` and `1 - π_l`. -/
theorem eq8_iff (ω pu πb : ℝ) (hπb : πb < 1) :
    πb = ω * (1 - (ω * (1 - pu) - πb) / (1 - πb)) * (1 - pu) ↔
      πb = ω * (1 - pu) ∨ πb = 1 - ω * (1 - pu) := by
  set πl := ω * (1 - pu)
  have h1 : (1 - πb) ≠ 0 := by linarith
  have e : ω * (1 - (πl - πb) / (1 - πb)) * (1 - pu) = πl * (1 - πl) / (1 - πb) := by
    simp only [πl]
    field_simp
    ring
  rw [e, eq_div_iff h1]
  constructor
  · intro h
    have : (πb - πl) * (πb - (1 - πl)) = 0 := by linarith
    rcases mul_eq_zero.mp this with h | h
    · left; linarith
    · right; linarith
  · rintro (h | h)
    · rw [h]
    · rw [h]; ring

/-- The map of the adjustment process (9): `π_b^{t+1} = π_l(1-π_l)/(1-π_b^t)`. -/
noncomputable def T (a x : ℝ) : ℝ := a * (1 - a) / (1 - x)

/-- The solution of (9) from the initial belief `x₀`. -/
noncomputable def iter (a x₀ : ℝ) : ℕ → ℝ
  | 0 => x₀
  | n + 1 => T a (iter a x₀ n)

/-- (9) agrees with (8): `[1 - α(x)] π_l = π_l(1-π_l)/(1-x)`. -/
theorem nine_eq (a x : ℝ) (hx : x < 1) : (1 - (a - x) / (1 - x)) * a = T a x := by
  have : 1 - x ≠ 0 := by linarith
  unfold T
  field_simp
  ring

theorem T_sub (a x : ℝ) (hx : x < 1) :
    T a x - (1 - a) = (1 - a) * (x - (1 - a)) / (1 - x) := by
  have : 1 - x ≠ 0 := by linarith
  unfold T
  field_simp
  ring

/-- p.1232: for `π_l > 1/2`, the solution of (9) from any initial belief in `[0, π_l)`
converges to `1 - π_l`. -/
theorem iter_tendsto (a x₀ : ℝ) (ha : 1 / 2 < a) (ha1 : a < 1) (hx0 : 0 ≤ x₀) (hxa : x₀ < a) :
    Tendsto (iter a x₀) atTop (𝓝 (1 - a)) := by
  set b := 1 - a with hb
  set m := max x₀ b
  have hb0 : 0 < b := by linarith
  have hba : b < a := by linarith
  have hma : m < a := max_lt hxa hba
  have h1m : b < 1 - m := by linarith
  set q := b / (1 - m)
  have hq0 : 0 ≤ q := div_nonneg hb0.le (by linarith)
  have hq1 : q < 1 := by rw [div_lt_one (by linarith)]; exact h1m
  have step : ∀ x, 0 ≤ x → x ≤ m →
      (0 ≤ T a x ∧ T a x ≤ m) ∧ |T a x - b| ≤ q * |x - b| := by
    intro x hx0 hxm
    have h1x : 0 < 1 - x := by linarith
    have hTsub := T_sub a x (by linarith)
    rw [← hb] at hTsub
    refine ⟨⟨?_, ?_⟩, ?_⟩
    · unfold T; exact div_nonneg (mul_nonneg (by linarith) (by linarith)) h1x.le
    · rcases le_or_gt b x with h | h
      · have : T a x - x = (x - b) * (x - a) / (1 - x) := by
          unfold T; field_simp; rw [hb]; ring
        have : T a x - x ≤ 0 := by
          rw [this]
          exact div_nonpos_of_nonpos_of_nonneg
            (mul_nonpos_of_nonneg_of_nonpos (by linarith) (by linarith)) h1x.le
        linarith
      · have : T a x - b < 0 := by
          rw [hTsub]
          exact div_neg_of_neg_of_pos (mul_neg_of_pos_of_neg hb0 (by linarith)) h1x
        have : b ≤ m := le_max_right _ _
        linarith
    · rw [hTsub, abs_div, abs_mul, abs_of_pos hb0, abs_of_pos h1x]
      rw [div_le_iff₀ h1x]
      have : b ≤ q * (1 - x) := by
        rw [div_mul_eq_mul_div, le_div_iff₀ (by linarith)]
        nlinarith
      nlinarith [abs_nonneg (x - b)]
  have bound : ∀ n, (0 ≤ iter a x₀ n ∧ iter a x₀ n ≤ m) ∧
      |iter a x₀ n - b| ≤ q ^ n * |x₀ - b| := by
    intro n
    induction n with
    | zero => exact ⟨⟨hx0, le_max_left _ _⟩, by simp [iter]⟩
    | succ n ih =>
      obtain ⟨⟨h0, hm⟩, hd⟩ := ih
      obtain ⟨hr, hs⟩ := step _ h0 hm
      refine ⟨hr, ?_⟩
      show |T a (iter a x₀ n) - b| ≤ q ^ (n + 1) * |x₀ - b|
      calc |T a (iter a x₀ n) - b| ≤ q * |iter a x₀ n - b| := hs
        _ ≤ q * (q ^ n * |x₀ - b|) := mul_le_mul_of_nonneg_left hd hq0
        _ = q ^ (n + 1) * |x₀ - b| := by ring
  rw [tendsto_iff_norm_sub_tendsto_zero]
  have hlim : Tendsto (fun n : ℕ => q ^ n * |x₀ - b|) atTop (𝓝 0) := by
    simpa using (tendsto_pow_atTop_nhds_zero_of_lt_one hq0 hq1).mul_const |x₀ - b|
  exact squeeze_zero (fun n => norm_nonneg _) (fun n => (bound n).2) hlim

theorem T_hasDerivAt (a x : ℝ) (hx : x ≠ 1) :
    HasDerivAt (T a) (a * (1 - a) / (1 - x) ^ 2) x := by
  have h1 : 1 - x ≠ 0 := sub_ne_zero.mpr (Ne.symm hx)
  have h := (((hasDerivAt_id x).const_sub 1).inv h1).const_mul (a * (1 - a))
  have e : T a = fun y => a * (1 - a) * (1 - id y)⁻¹ := by
    funext y
    simp [T, div_eq_mul_inv]
  rw [e]
  refine h.congr_deriv ?_
  simp only [id]
  field_simp

/-- The slope of (9) at the patronizing belief `1 - π_l` is `(1-π_l)/π_l < 1`. -/
theorem patronizing_stable (a : ℝ) (ha : 1 / 2 < a) (ha1 : a < 1) :
    a * (1 - a) / (1 - (1 - a)) ^ 2 = (1 - a) / a ∧ (1 - a) / a < 1 := by
  have ha0 : 0 < a := by linarith
  have e : 1 - (1 - a) = a := by ring
  refine ⟨by rw [e]; field_simp, ?_⟩
  rw [div_lt_one ha0]
  linarith

/-- The slope of (9) at the color-blind belief `π_l` is `π_l/(1-π_l) > 1`. -/
theorem colorblind_unstable (a : ℝ) (ha : 1 / 2 < a) (ha1 : a < 1) :
    a * (1 - a) / (1 - a) ^ 2 = a / (1 - a) ∧ 1 < a / (1 - a) := by
  have h1 : 0 < 1 - a := by linarith
  refine ⟨by field_simp, ?_⟩
  rw [one_lt_div h1]
  linarith

/-- p.1232: the stereotype worsens, `1 - π_l < π_c`, exactly when `π_l + π_c > 1`. -/
theorem worsens_iff (πl πc : ℝ) : 1 - πl < πc ↔ 1 < πl + πc := by
  constructor <;> intro h <;> linarith

/-- Footnote 20: patronizing a failing B beats demoting an unclear W exactly when
`λ > λ̂ = 1/(ξ_l (1 + r))`. -/
theorem lambda_hat_iff (lam ξ xq xu : ℝ) (hl0 : 0 < lam) (hl1 : lam < 1) (hξ : 0 < ξ)
    (hxq : 0 < xq) (hxu : 0 < xu) :
    xu < lam / (1 - lam) * (ξ * xq - (1 - ξ) * xu) ↔ 1 / (ξ * (1 + xq / xu)) < lam := by
  have h1 : 0 < 1 - lam := by linarith
  have hd : 0 < ξ * (1 + xq / xu) := by positivity
  rw [div_mul_eq_mul_div, lt_div_iff₀ h1, div_lt_iff₀ hd]
  have e : lam * (ξ * (1 + xq / xu)) * xu = lam * ξ * (xu + xq) := by
    field_simp
  constructor
  · intro h
    have : xu < lam * ξ * (xu + xq) := by nlinarith
    have h' : 1 * xu < lam * (ξ * (1 + xq / xu)) * xu := by rw [e]; linarith
    exact lt_of_mul_lt_mul_right h' hxu.le
  · intro h
    have h' := mul_lt_mul_of_pos_right h hxu
    rw [e] at h'
    nlinarith

/-- Footnote 20: `π_l > π̂` (the employer is strictly liberal toward W's) implies `λ̂ < 1`. -/
theorem lambda_hat_lt_one (ξ xq xu : ℝ) (hξ : 0 < ξ) (hxq : 0 < xq) (hxu : 0 < xu)
    (hlib : (1 - ξ) * xu < ξ * xq) : 1 / (ξ * (1 + xq / xu)) < 1 := by
  have hd : 0 < ξ * (1 + xq / xu) := by positivity
  rw [div_lt_one hd]
  have e : ξ * (1 + xq / xu) * xu = ξ * xu + ξ * xq := by field_simp
  have : 1 * xu < ξ * (1 + xq / xu) * xu := by rw [e]; linarith
  exact lt_of_mul_lt_mul_right this hxu.le

/-- Footnote 21: with `p_u = 0.2`, `p_q = 0.3` and `r = 2/3`, `π̂ = 1/2`. -/
theorem fn21_pihat : ((1 / 5 : ℝ) / (3 / 10)) / (2 / 3 + (1 / 5) / (3 / 10)) = 1 / 2 := by
  norm_num

/-- Footnote 21: for `λ ∈ (0.9, 1)` and `5/7 > ω > 0.5/[λ - 0.2]`, the hypotheses of
Proposition 3 hold: `π_l > π̂ > π_c`, `π_l > 1/2`, and `λ > λ̂`. -/
theorem fn21 (lam ω : ℝ) (hl : 9 / 10 < lam) (hl1 : lam < 1) (hω : 1 / 2 / (lam - 1 / 5) < ω)
    (hω' : ω < 5 / 7) :
    ω * (1 - 3 / 10) < 1 / 2 ∧ 1 / 2 < ω * (1 - 1 / 5) ∧
      1 / ((ω * (1 - 1 / 5) * (3 / 10) / (ω * (1 - 1 / 5) * (3 / 10) +
        (1 - ω * (1 - 1 / 5)) * (1 / 5))) * (1 + 2 / 3)) < lam := by
  have hd : 0 < lam - 1 / 5 := by linarith
  have hω1 : 1 / 2 < ω * (lam - 1 / 5) := by rwa [div_lt_iff₀ hd] at hω
  have hω0 : 5 / 8 < ω := by nlinarith
  refine ⟨by linarith, by linarith, ?_⟩
  have hπ : 0 < ω * (1 - 1 / 5) := by linarith
  have hden : 0 < ω * (1 - 1 / 5) * (3 / 10) + (1 - ω * (1 - 1 / 5)) * (1 / 5) := by nlinarith
  have e : 1 / ((ω * (1 - 1 / 5) * (3 / 10) / (ω * (1 - 1 / 5) * (3 / 10) +
      (1 - ω * (1 - 1 / 5)) * (1 / 5))) * (1 + 2 / 3))
      = (ω * (1 - 1 / 5) * (3 / 10) + (1 - ω * (1 - 1 / 5)) * (1 / 5)) / (ω * (2 / 5)) := by
    field_simp
    ring
  rw [e, div_lt_iff₀ (by linarith)]
  nlinarith

/-- Footnote 21: the interval for `ω` is non-empty when `λ > 0.9`. -/
theorem fn21_nonempty (lam : ℝ) (hl : 9 / 10 < lam) : 1 / 2 / (lam - 1 / 5) < 5 / 7 := by
  rw [div_lt_iff₀ (by linarith)]
  linarith

/-- Footnote 21: the stereotype worsens exactly when `ω > 2/3`. -/
theorem fn21_worse_iff (ω : ℝ) : 1 < ω * (1 - 1 / 5) + ω * (1 - 3 / 10) ↔ 2 / 3 < ω := by
  constructor <;> intro h <;> linarith

/-! ## 7. Section III (pp.1233-1234) -/

/-- The rate at which a group with belief `π` facing the standard `s` is assigned. -/
noncomputable def rho (Fq Fu : ℝ → ℝ) (π s : ℝ) : ℝ := π * (1 - Fq s) + (1 - π) * (1 - Fu s)

theorem rho_hasDerivAt (Fq Fu : ℝ → ℝ) (fq fu π s : ℝ) (hq : HasDerivAt Fq fq s)
    (hu : HasDerivAt Fu fu s) :
    HasDerivAt (rho Fq Fu π) (-(π * fq) - (1 - π) * fu) s := by
  have h := ((hq.const_sub 1).const_mul π).add ((hu.const_sub 1).const_mul (1 - π))
  refine h.congr_deriv ?_
  ring

/-- (11a): `∂ℒ/∂s_w = λ ∂P/∂s - γ ∂ρ/∂s = 0` exactly when
`[x_q - γ/λ]/[x_u + γ/λ] = [(1-π_w)/π_w] φ(s_w)`. -/
theorem foc_w_iff (lam γ π fq fu xq xu : ℝ) (hl : 0 < lam) (hπ : 0 < π) (hq : 0 < fq)
    (hpos : 0 < xu + γ / lam) :
    lam * (-(π * fq * xq) + (1 - π) * fu * xu) - γ * (-(π * fq) - (1 - π) * fu) = 0 ↔
      (xq - γ / lam) / (xu + γ / lam) = (1 - π) / π * (fu / fq) := by
  rw [div_mul_div_comm, div_eq_div_iff hpos.ne' (by positivity)]
  constructor
  · intro h
    field_simp
    linarith
  · intro h
    field_simp at h
    linarith

/-- (11b): `∂ℒ/∂s_b = (1-λ) ∂P/∂s + γ ∂ρ/∂s = 0` exactly when
`[x_q + γ/(1-λ)]/[x_u - γ/(1-λ)] = [(1-π_b)/π_b] φ(s_b)`. -/
theorem foc_b_iff (lam γ π fq fu xq xu : ℝ) (hl : lam < 1) (hπ : 0 < π) (hq : 0 < fq)
    (hpos : 0 < xu - γ / (1 - lam)) :
    (1 - lam) * (-(π * fq * xq) + (1 - π) * fu * xu) + γ * (-(π * fq) - (1 - π) * fu) = 0 ↔
      (xq + γ / (1 - lam)) / (xu - γ / (1 - lam)) = (1 - π) / π * (fu / fq) := by
  have h1 : (1 - lam) ≠ 0 := by linarith
  rw [div_mul_div_comm, div_eq_div_iff hpos.ne' (by positivity)]
  constructor
  · intro h
    field_simp
    linarith
  · intro h
    field_simp at h
    linarith

/-- Proof of Proposition 4: `D(γ) = ρ̂(s_b(γ)) - ρ̂(s_w(γ))` has derivative
`ρ̂'(s̄)(s_b'(0) - s_w'(0))` at zero. -/
theorem prop4_D_hasDerivAt (ρhat sb sw : ℝ → ℝ) (sbar p db dw : ℝ) (hρ : HasDerivAt ρhat p sbar)
    (hb : HasDerivAt sb db 0) (hw : HasDerivAt sw dw 0) (hb0 : sb 0 = sbar) (hw0 : sw 0 = sbar) :
    HasDerivAt (fun y => ρhat (sb y) - ρhat (sw y)) (p * db - p * dw) 0 := by
  have h1 : HasDerivAt (fun y => ρhat (sb y)) (p * db) 0 := by
    rw [← hb0] at hρ; exact hρ.comp 0 hb
  have h2 : HasDerivAt (fun y => ρhat (sw y)) (p * dw) 0 := by
    rw [← hw0] at hρ; exact hρ.comp 0 hw
  exact h1.sub h2

/-- `D'(0) < 0` when `ρ̂'(s̄) > 0`, `s_b' < 0` and `s_w' > 0`. -/
theorem prop4_D_deriv_neg (p db dw : ℝ) (hp : 0 < p) (hb : db < 0) (hw : 0 < dw) :
    p * db - p * dw < 0 := by
  nlinarith

/-- Proof of Proposition 4: `D(0) = 0`, `D'(0) < 0` and `D(γ') > 0` give a root
`γ* ∈ (0, γ')`. -/
theorem prop4_root (D : ℝ → ℝ) (d γ' : ℝ) (hγ' : 0 < γ') (hc : ContinuousOn D (Icc 0 γ'))
    (h0 : D 0 = 0) (hd : HasDerivAt D d 0) (hneg : d < 0) (hpos : 0 < D γ') :
    ∃ γ ∈ Ioo 0 γ', D γ = 0 := by
  have hs : Tendsto (slope D 0) (𝓝[>] 0) (𝓝 d) :=
    (hasDerivAt_iff_tendsto_slope.mp hd).mono_left (nhdsWithin_mono _ (fun x hx => ne_of_gt hx))
  have ev1 : ∀ᶠ x in 𝓝[>] (0 : ℝ), slope D 0 x < 0 := hs (Iio_mem_nhds hneg)
  have ev2 : ∀ᶠ x in 𝓝[>] (0 : ℝ), x ∈ Ioo 0 γ' := Ioo_mem_nhdsGT hγ'
  obtain ⟨ε, hε1, hε2⟩ := (ev1.and ev2).exists
  rw [slope_def_field, h0, sub_zero, sub_zero] at hε1
  have hDε : D ε < 0 := by
    rcases (div_neg_iff.mp hε1) with ⟨-, h⟩ | ⟨h, -⟩
    · linarith [hε2.1]
    · exact h
  obtain ⟨γ, hγ, e⟩ := intermediate_value_Ioo hε2.2.le
    (hc.mono (Icc_subset_Icc hε2.1.le le_rfl)) ⟨hDε, hpos⟩
  exact ⟨γ, ⟨hε2.1.trans hγ.1, hγ.2⟩, e⟩

end CoateLoury1993
