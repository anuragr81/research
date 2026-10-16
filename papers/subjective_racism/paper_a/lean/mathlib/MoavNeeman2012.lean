import Mathlib

/-! Moav and Neeman, "Saving Rates and Poverty: The Role of Conspicuous
Consumption and Human Capital", *Economic Journal* 122 (2012), Sections 2 to 4
and Appendix A, in the paper's notation. `λ` (written `l`) is the weight on
status, `h` human capital, `y` income, `yl` the lowest income `y̲(h)` of a person
with human capital `h`, and `x(h, y) = λ y [1 − (y̲(h)/y)^{1/(1−λ)}]` is the
fully separating expenditure on conspicuous consumption, eq. (9). -/

namespace MoavNeeman2012

open Real

/-- Eq. (9). -/
noncomputable def x (l yl y : ℝ) : ℝ := l * y * (1 - (yl / y) ^ (1 / (1 - l)))

/-- Eq. (9) solves the first order condition (7) in the separating equilibrium,
where `ỹ = y`. Read with `dx/dy = 1/(dỹ/dx)`, (7) is
`dx/dy = (λ/(1 − λ)) (y − x)/y`. -/
theorem eq9_solves_eq7 {l yl y : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hyl : 0 < yl)
    (hy : 0 < y) :
    HasDerivAt (x l yl) (l / (1 - l) * (y - x l yl y) / y) y := by
  set a := 1 / (1 - l) with ha
  have h1l : (1 - l) ≠ 0 := by linarith
  have hf : HasDerivAt (fun t => yl / t) (-(yl / y ^ 2)) y := by
    have := (hasDerivAt_inv (ne_of_gt hy)).const_mul yl
    convert this using 1
    · ring
  have hfy : yl / y ≠ 0 := div_ne_zero (ne_of_gt hyl) (ne_of_gt hy)
  have hp : HasDerivAt (fun t => (yl / t) ^ a) (-(yl / y ^ 2) * a * (yl / y) ^ (a - 1)) y :=
    hf.rpow_const (Or.inl hfy)
  have hx : HasDerivAt (x l yl)
      (l * 1 * (1 - (yl / y) ^ a) + l * y * (-(-(yl / y ^ 2) * a * (yl / y) ^ (a - 1)))) y := by
    have := ((hasDerivAt_id y).const_mul l).mul ((hasDerivAt_const y (1 : ℝ)).sub hp)
    unfold x
    convert this using 1
    · simp [id]
  convert hx using 1
  rw [Real.rpow_sub_one hfy]
  unfold x
  rw [← ha]
  set z := (yl / y) ^ a
  have hy0 : y ≠ 0 := ne_of_gt hy
  have hyl0 : yl ≠ 0 := ne_of_gt hyl
  have hya : a * (1 - l) = 1 := by rw [ha]; field_simp
  have e1 : l * y * -(-(yl / y ^ 2) * a * (z / (yl / y))) = l * a * z := by
    field_simp
  have e2 : l / (1 - l) * (y - l * y * (1 - z)) / y = l * a * (1 - l + l * z) := by
    rw [ha]; field_simp; ring
  rw [e1, e2]
  linear_combination (l - l * z) * hya

/-- Property (i). The person with the lowest income spends nothing. -/
theorem x_at_floor {l yl : ℝ} (hyl : yl ≠ 0) : x l yl yl = 0 := by
  unfold x; rw [div_self hyl, Real.one_rpow]; ring

/-- The share of the average person with human capital `h`, `x(h, h)/h`, from
eq. (9), as in the proof of Proposition 2. -/
theorem share_avg {l yl h : ℝ} (hh : h ≠ 0) :
    x l yl h / h = l * (1 - (yl / h) ^ (1 / (1 - l))) := by
  unfold x; field_simp

/-- Proposition 2, the step the proof uses. If `y̲(h)/h` is non-negative and
non-decreasing in `h`, the share `λ (1 − (y̲(h)/h)^{1/(1−λ)})` is
non-increasing in `h`. -/
theorem prop2 {l : ℝ} (hl0 : 0 < l) (hl1 : l < 1) {r : ℝ → ℝ} (hr0 : ∀ h, 0 ≤ r h)
    (hmono : Monotone r) : Antitone (fun h => l * (1 - r h ^ (1 / (1 - l)))) := by
  intro h₁ h₂ h12
  have ha : 0 ≤ 1 / (1 - l) := by
    have : 0 < 1 - l := by linarith
    positivity
  have := Real.rpow_le_rpow (hr0 h₁) (hmono h12) ha
  simp only
  nlinarith

/-- The Remark of Section 3. With human capital unobserved, the lowest income
is 0 and the separating equilibrium spends the constant share `λ`. -/
theorem remark_constant_share {l y : ℝ} (hl1 : l < 1) (_hy : y ≠ 0) :
    x l 0 y = l * y := by
  have ha : 1 / (1 - l) ≠ 0 := by
    have : 1 - l ≠ 0 := by linarith
    exact one_div_ne_zero this
  unfold x; rw [zero_div, Real.zero_rpow ha]; ring

/-- Section 4. The mapping of human capital across generations along the
average path, `φ(h) = θ + γ β (h − x(h, h))`, from eqs. (1), (4) and (9). In
case (i) of Section 4, `y̲(h) = 0`, it is linear, `θ + γ β (1 − λ) h`. -/
noncomputable def φ (θ γ β l yl h : ℝ) : ℝ := θ + γ * β * (h - x l yl h)

theorem phi_case_i {θ γ β l h : ℝ} (hl1 : l < 1) (hh : h ≠ 0) :
    φ θ γ β l 0 h = θ + γ * β * (1 - l) * h := by
  unfold φ; rw [remark_constant_share hl1 hh]; ring

/-- Control: Proposition 2 as stated needs more than a non-decreasing, convex
lowest income. A constant lowest income `c > 0` is both, yet the share of the
average person rises in `h` (reading MN-D2). The proof's step needs `y̲(h)/h`
non-decreasing, which a positive constant violates. -/
theorem control_prop2_constant_floor {l c h₁ h₂ : ℝ} (hl0 : 0 < l) (hl1 : l < 1)
    (hc : 0 < c) (h1 : c < h₁) (h12 : h₁ < h₂) :
    Monotone (fun _ : ℝ => c) ∧ ConvexOn ℝ Set.univ (fun _ : ℝ => c) ∧
      x l c h₁ / h₁ < x l c h₂ / h₂ := by
  refine ⟨fun _ _ _ => le_rfl, convexOn_const c convex_univ, ?_⟩
  have hh₁ : 0 < h₁ := lt_trans hc h1
  have hh₂ : 0 < h₂ := lt_trans hh₁ h12
  rw [share_avg (ne_of_gt hh₁), share_avg (ne_of_gt hh₂)]
  have ha : 0 < 1 / (1 - l) := by
    have : 0 < 1 - l := by linarith
    positivity
  have hlt : c / h₂ < c / h₁ := div_lt_div_of_pos_left hc hh₁ h12
  have := Real.rpow_lt_rpow (by positivity) hlt ha
  nlinarith

/-- Control: without observable human capital the share does not fall with
income. It is `λ` at every income, so the declining share of Proposition 2
needs `y̲(h) > 0`. -/
theorem control_unobserved_constant {l y₁ y₂ : ℝ} (hl1 : l < 1) (h1 : y₁ ≠ 0)
    (h2 : y₂ ≠ 0) : x l 0 y₁ / y₁ = x l 0 y₂ / y₂ := by
  rw [remark_constant_share hl1 h1, remark_constant_share hl1 h2]
  field_simp

/-! ### Section 3 in full: the separating equilibrium for a fixed `h`

For a fixed `h`, write `yl = y̲(h)`. The reduced utility (6) is
`(y − x)^{1−λ} ỹ^λ`, which for positive arguments is the exponential of
`U = (1 − λ) log(y − x) + λ log ỹ` (`u_eq_exp_U`). A fully separating
equilibrium is a spending function `xs` on incomes `y ≥ yl` such that a person
whose spending is believed to reveal income `y'` cannot gain by spending
`xs y'` instead of `xs y` (part (i) of the Definition, with the beliefs of part
(ii) equal to the revealed income on the path), and such that spending nothing
when believed to have the lowest income `yl` does not pay either. -/

/-- The log of the reduced utility (6). -/
noncomputable def U (l y xx b : ℝ) : ℝ := (1 - l) * Real.log (y - xx) + l * Real.log b

theorem u_eq_exp_U {l y xx b : ℝ} (h1 : 0 < y - xx) (hb : 0 < b) :
    (y - xx) ^ (1 - l) * b ^ l = Real.exp (U l y xx b) := by
  unfold U
  rw [Real.rpow_def_of_pos h1, Real.rpow_def_of_pos hb, ← Real.exp_add]
  ring_nf

/-- Definition, parts (i) and (ii), for a fully separating equilibrium at a
fixed level of human capital. -/
structure SeparatingEq (l yl : ℝ) (xs : ℝ → ℝ) : Prop where
  nonneg : ∀ y, yl ≤ y → 0 ≤ xs y
  feasible : ∀ y, yl ≤ y → xs y < y
  ic : ∀ y y', yl ≤ y → yl ≤ y' → xs y' < y → U l y (xs y') y' ≤ U l y (xs y) y
  zero_dev : ∀ y, yl ≤ y → U l y 0 yl ≤ U l y (xs y) y

theorem x_nonneg {l yl t : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hyl : 0 < yl) (ht : yl ≤ t) :
    0 ≤ x l yl t := by
  have htp : 0 < t := lt_of_lt_of_le hyl ht
  have ha : 0 ≤ 1 / (1 - l) := by
    have : 0 < 1 - l := by linarith
    positivity
  have hr : yl / t ≤ 1 := (div_le_one htp).2 ht
  have := Real.rpow_le_one (by positivity) hr ha
  unfold x
  have : 0 ≤ 1 - (yl / t) ^ (1 / (1 - l)) := by linarith
  positivity

theorem x_le {l yl t : ℝ} (hl0 : 0 < l) (hyl : 0 < yl) (ht : yl ≤ t) :
    x l yl t ≤ l * t := by
  have htp : 0 < t := lt_of_lt_of_le hyl ht
  have := Real.rpow_nonneg (le_of_lt (div_pos hyl htp)) (1 / (1 - l))
  have := mul_nonneg (mul_nonneg hl0.le htp.le) this
  unfold x
  nlinarith

theorem x_lt {l yl t : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hyl : 0 < yl) (ht : yl ≤ t) :
    x l yl t < t := by
  have htp : 0 < t := lt_of_lt_of_le hyl ht
  have := x_le hl0 hyl ht
  nlinarith

/-- The derivative of eq. (9) in closed form, `λ + λ² a (y̲/y)^a` with
`a = 1/(1 − λ)`. -/
theorem x_deriv {l yl t : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hyl : 0 < yl) (ht : 0 < t) :
    HasDerivAt (x l yl) (l + l ^ 2 * (1 / (1 - l)) * (yl / t) ^ (1 / (1 - l))) t := by
  convert eq9_solves_eq7 hl0 hl1 hyl ht using 1
  unfold x
  have h1 : 1 - l ≠ 0 := by linarith
  have h0 : t ≠ 0 := ne_of_gt ht
  field_simp
  ring

/-- Property (ii). Spending rises in income. -/
theorem x_strictMonoOn {l yl : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hyl : 0 < yl) :
    StrictMonoOn (x l yl) (Set.Ici yl) := by
  apply strictMonoOn_of_deriv_pos (convex_Ici yl)
  · intro t ht
    exact (x_deriv hl0 hl1 hyl (lt_of_lt_of_le hyl ht)).continuousAt.continuousWithinAt
  · intro t ht
    rw [interior_Ici] at ht
    have htp : 0 < t := lt_trans hyl ht
    rw [(x_deriv hl0 hl1 hyl htp).deriv]
    have : 0 ≤ (yl / t) ^ (1 / (1 - l)) := Real.rpow_nonneg (by positivity) _
    have : 0 < 1 / (1 - l) := by
      have : 0 < 1 - l := by linarith
      positivity
    positivity

/-- The derivative of the mimicking utility. A person with income `y` who
spends `x(t)` and is believed to have income `t` has utility
`U(y, x(t), t)`, whose derivative in `t` is `(λ/t)(y − t)/(y − x(t))`. -/
theorem mimic_deriv {l yl y t : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hyl : 0 < yl) (ht : 0 < t)
    (hyx : 0 < y - x l yl t) :
    HasDerivAt (fun s => U l y (x l yl s) s) (l / t * (y - t) / (y - x l yl t)) t := by
  have hx := eq9_solves_eq7 hl0 hl1 hyl ht
  have h1 : HasDerivAt (fun s => Real.log (y - x l yl s))
      (-(l / (1 - l) * (t - x l yl t) / t) / (y - x l yl t)) t := by
    have := ((hasDerivAt_const t y).sub hx).log (ne_of_gt hyx)
    simpa using this
  have h2 := (Real.hasDerivAt_log (ne_of_gt ht)).const_mul l
  have := (h1.const_mul (1 - l)).add h2
  unfold U
  convert this using 1
  have h1l : 1 - l ≠ 0 := by linarith
  field_simp
  ring

/-- Proposition 1, existence. Eq. (9) is incentive compatible: no person gains
by mimicking the spending of another income. -/
theorem prop1_ic {l yl y y' : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hyl : 0 < yl) (hy : yl ≤ y)
    (hy' : yl ≤ y') (hfeas : x l yl y' < y) :
    U l y (x l yl y') y' ≤ U l y (x l yl y) y := by
  have hmono := x_strictMonoOn hl0 hl1 hyl
  have hd : ∀ t, yl ≤ t → x l yl t < y →
      HasDerivAt (fun s => U l y (x l yl s) s) (l / t * (y - t) / (y - x l yl t)) t :=
    fun t ht hxt => mimic_deriv hl0 hl1 hyl (lt_of_lt_of_le hyl ht) (by linarith)
  rcases le_total y' y with hle | hle
  · -- below: the mimicking utility rises on [y', y]
    have hxt : ∀ t ∈ Set.Icc y' y, x l yl t < y := fun t ht =>
      lt_of_lt_of_le (x_lt hl0 hl1 hyl (le_trans hy' ht.1)) ht.2
    have hm : MonotoneOn (fun s => U l y (x l yl s) s) (Set.Icc y' y) := by
      apply monotoneOn_of_deriv_nonneg (convex_Icc y' y)
      · intro t ht
        exact (hd t (le_trans hy' ht.1) (hxt t ht)).continuousAt.continuousWithinAt
      · intro t ht
        rw [interior_Icc] at ht
        exact (hd t (le_trans hy' ht.1.le) (hxt t ⟨ht.1.le, ht.2.le⟩)).differentiableAt.differentiableWithinAt
      · intro t ht
        rw [interior_Icc] at ht
        have htp : 0 < t := lt_of_lt_of_le hyl (le_trans hy' ht.1.le)
        have hxt' := hxt t ⟨ht.1.le, ht.2.le⟩
        rw [(hd t (le_trans hy' ht.1.le) hxt').deriv]
        have : 0 ≤ y - t := by linarith [ht.2]
        have : 0 < y - x l yl t := by linarith
        positivity
    exact hm ⟨le_rfl, hle⟩ ⟨hle, le_rfl⟩ hle
  · -- above: the mimicking utility falls on [y, y']
    have hxt : ∀ t ∈ Set.Icc y y', x l yl t < y := by
      intro t ht
      have : x l yl t ≤ x l yl y' :=
        hmono.monotoneOn (le_trans hy ht.1) hy' ht.2
      linarith
    have hm : AntitoneOn (fun s => U l y (x l yl s) s) (Set.Icc y y') := by
      apply antitoneOn_of_deriv_nonpos (convex_Icc y y')
      · intro t ht
        exact (hd t (le_trans hy ht.1) (hxt t ht)).continuousAt.continuousWithinAt
      · intro t ht
        rw [interior_Icc] at ht
        exact (hd t (le_trans hy ht.1.le) (hxt t ⟨ht.1.le, ht.2.le⟩)).differentiableAt.differentiableWithinAt
      · intro t ht
        rw [interior_Icc] at ht
        have htp : 0 < t := lt_of_lt_of_le hyl (le_trans hy ht.1.le)
        have hxt' := hxt t ⟨ht.1.le, ht.2.le⟩
        rw [(hd t (le_trans hy ht.1.le) hxt').deriv]
        have h1 : y - t ≤ 0 := by linarith [ht.1]
        have h2 : 0 < y - x l yl t := by linarith
        have h3 : 0 < l / t := by positivity
        have : l / t * (y - t) ≤ 0 := mul_nonpos_of_nonneg_of_nonpos h3.le h1
        exact div_nonpos_of_nonpos_of_nonneg this h2.le
    exact hm ⟨le_rfl, hle⟩ ⟨hle, le_rfl⟩ hle

/-- Proposition 1, existence. Eq. (9) is a fully separating equilibrium. -/
theorem prop1_exists {l yl : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hyl : 0 < yl) :
    SeparatingEq l yl (x l yl) where
  nonneg := fun y hy => x_nonneg hl0 hl1 hyl hy
  feasible := fun y hy => x_lt hl0 hl1 hyl hy
  ic := fun y y' hy hy' hf => prop1_ic hl0 hl1 hyl hy hy' hf
  zero_dev := by
    intro y hy
    have h := prop1_ic hl0 hl1 hyl hy le_rfl (by rw [x_at_floor (ne_of_gt hyl)]; linarith)
    rwa [x_at_floor (ne_of_gt hyl)] at h

/-- The lowest type does not signal. In a separating equilibrium, `xs y̲ = 0`. -/
theorem prop1_floor {l yl : ℝ} {xs : ℝ → ℝ} (hl1 : l < 1) (hyl : 0 < yl)
    (he : SeparatingEq l yl xs) : xs yl = 0 := by
  have h := he.zero_dev yl le_rfl
  have hf := he.feasible yl le_rfl
  have hn := he.nonneg yl le_rfl
  unfold U at h
  have h1l : 0 < 1 - l := by linarith
  have hlog : Real.log (yl - 0) ≤ Real.log (yl - xs yl) := by
    have := le_of_add_le_add_right h
    exact le_of_mul_le_mul_left this h1l
  rw [sub_zero, Real.log_le_log_iff hyl (by linarith)] at hlog
  linarith

/-- Proposition 1, the first order condition. A differentiable separating
equilibrium satisfies eq. (7) along `ỹ = y`. -/
theorem prop1_foc {l yl t xs' : ℝ} {xs : ℝ → ℝ} (_hl0 : 0 < l) (hl1 : l < 1) (hyl : 0 < yl)
    (he : SeparatingEq l yl xs) (ht : yl < t) (hd : HasDerivAt xs xs' t) :
    xs' = l / (1 - l) * (t - xs t) / t := by
  have htp : 0 < t := lt_trans hyl ht
  have hft := he.feasible t ht.le
  have hmax : IsLocalMax (fun s => U l t (xs s) s) t := by
    have h1 : ∀ᶠ s in nhds t, yl < s := isOpen_Ioi.mem_nhds ht
    have h2 : ∀ᶠ s in nhds t, xs s < t :=
      hd.continuousAt.eventually (isOpen_Iio.mem_nhds hft)
    filter_upwards [h1, h2] with s hs1 hs2
    exact he.ic t s ht.le hs1.le hs2
  have hU : HasDerivAt (fun s => U l t (xs s) s)
      ((1 - l) * (-xs' / (t - xs t)) + l * t⁻¹) t := by
    have h1 : HasDerivAt (fun s => Real.log (t - xs s)) (-xs' / (t - xs t)) t := by
      have h0 : HasDerivAt (fun s => t - xs s) (0 - xs') t := (hasDerivAt_const t t).sub hd
      have := h0.log (sub_pos.2 hft).ne'
      simpa using this
    exact (h1.const_mul (1 - l)).add ((Real.hasDerivAt_log (ne_of_gt htp)).const_mul l)
  have h0 := hmax.hasDerivAt_eq_zero hU
  have h1l : 1 - l ≠ 0 := by linarith
  have htx : t - xs t ≠ 0 := by linarith
  field_simp at h0 ⊢
  linarith

/-- Proposition 1, uniqueness. A separating equilibrium that is continuous on
`[y̲, ∞)` and differentiable above `y̲` is eq. (9). -/
theorem prop1_unique {l yl : ℝ} {xs : ℝ → ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hyl : 0 < yl)
    (he : SeparatingEq l yl xs) (hcont : ContinuousOn xs (Set.Ici yl))
    (hdiff : ∀ t, yl < t → DifferentiableAt ℝ xs t) :
    ∀ y, yl ≤ y → xs y = x l yl y := by
  intro T hT
  rcases eq_or_lt_of_le hT with h | hT
  · subst h; rw [prop1_floor hl1 hyl he, x_at_floor (ne_of_gt hyl)]
  set c := l / (1 - l) with hc
  have h1l : 1 - l ≠ 0 := by linarith
  let g : ℝ → ℝ := fun s => (xs s - x l yl s) * s ^ c
  have hderiv : ∀ s ∈ Set.Ioo yl T, HasDerivAt g 0 s := by
    intro s hs
    have hsp : 0 < s := lt_trans hyl hs.1
    have hxs := (hdiff s hs.1).hasDerivAt
    have hfoc := prop1_foc hl0 hl1 hyl he hs.1 hxs
    have hX := eq9_solves_eq7 hl0 hl1 hyl hsp
    have hp := Real.hasDerivAt_rpow_const (p := c) (Or.inl (ne_of_gt hsp))
    have := (hxs.sub hX).mul hp
    convert this using 1
    simp only [Pi.sub_apply]
    rw [hfoc, Real.rpow_sub_one (ne_of_gt hsp), hc]
    field_simp
    ring
  have hcg : ContinuousOn g (Set.Icc yl T) := by
    intro s hs
    have hsp : 0 < s := lt_of_lt_of_le hyl hs.1
    apply ContinuousWithinAt.mul
    · apply ContinuousWithinAt.sub
      · exact (hcont s hs.1).mono Set.Icc_subset_Ici_self
      · exact (eq9_solves_eq7 hl0 hl1 hyl hsp).continuousAt.continuousWithinAt
    · exact (Real.hasDerivAt_rpow_const (p := c) (Or.inl (ne_of_gt hsp))).continuousAt.continuousWithinAt
  obtain ⟨d, _, hd⟩ := exists_hasDerivAt_eq_slope g (fun _ => 0) hT hcg hderiv
  have hg0 : g yl = 0 := by
    show (xs yl - x l yl yl) * yl ^ c = 0
    rw [prop1_floor hl1 hyl he, x_at_floor (ne_of_gt hyl)]; ring
  have hgT : g T = 0 := by
    have hne : T - yl ≠ 0 := by linarith
    have : g T - g yl = 0 := by
      have := hd.symm
      rw [div_eq_zero_iff] at this
      rcases this with h | h
      · exact h
      · exact absurd h hne
    linarith
  have hTp : 0 < T ^ c := Real.rpow_pos_of_pos (lt_trans hyl hT) c
  have : xs T - x l yl T = 0 := by
    rcases mul_eq_zero.mp hgT with h | h
    · exact h
    · linarith
  linarith

/-- Property (iii). Spending is concave in income. -/
theorem x_concaveOn {l yl : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hyl : 0 < yl) :
    ConcaveOn ℝ (Set.Ici yl) (x l yl) := by
  have ha : 0 ≤ 1 / (1 - l) := by
    have : 0 < 1 - l := by linarith
    positivity
  apply AntitoneOn.concaveOn_of_deriv (convex_Ici yl)
  · intro t ht
    exact (x_deriv hl0 hl1 hyl (lt_of_lt_of_le hyl ht)).continuousAt.continuousWithinAt
  · intro t ht
    rw [interior_Ici] at ht
    exact (x_deriv hl0 hl1 hyl (lt_trans hyl ht)).differentiableAt.differentiableWithinAt
  · intro t₁ h₁ t₂ h₂ h12
    rw [interior_Ici] at h₁ h₂
    have hp₁ : 0 < t₁ := lt_trans hyl h₁
    have hp₂ : 0 < t₂ := lt_trans hyl h₂
    rw [(x_deriv hl0 hl1 hyl hp₁).deriv, (x_deriv hl0 hl1 hyl hp₂).deriv]
    have hr : yl / t₂ ≤ yl / t₁ := div_le_div_of_nonneg_left hyl.le hp₁ h12
    have := Real.rpow_le_rpow (by positivity) hr ha
    have hc : 0 ≤ l ^ 2 * (1 / (1 - l)) := by positivity
    nlinarith

/-- Property (iv). At the lowest income the slope is `λ/(1 − λ)`. -/
theorem x_slope_at_floor {l yl : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hyl : 0 < yl) :
    HasDerivAt (x l yl) (l / (1 - l)) yl := by
  convert x_deriv hl0 hl1 hyl hyl using 1
  rw [div_self (ne_of_gt hyl), Real.one_rpow]
  have h1l : 1 - l ≠ 0 := by linarith
  field_simp
  ring

/-- Property (v). The slope tends to `λ` as income grows. -/
theorem x_slope_atTop {l yl : ℝ} (_hl0 : 0 < l) (hl1 : l < 1) (hyl : 0 < yl) :
    Filter.Tendsto (fun t => l + l ^ 2 * (1 / (1 - l)) * (yl / t) ^ (1 / (1 - l)))
      Filter.atTop (nhds l) := by
  have ha : 0 < 1 / (1 - l) := by
    have : 0 < 1 - l := by linarith
    positivity
  have h1 : Filter.Tendsto (fun t : ℝ => yl ^ (1 / (1 - l)) * t ^ (-(1 / (1 - l))))
      Filter.atTop (nhds (yl ^ (1 / (1 - l)) * 0)) :=
    (tendsto_rpow_neg_atTop ha).const_mul _
  rw [mul_zero] at h1
  have h2 : (fun t : ℝ => yl ^ (1 / (1 - l)) * t ^ (-(1 / (1 - l)))) =ᶠ[Filter.atTop]
      (fun t => (yl / t) ^ (1 / (1 - l))) := by
    filter_upwards [Filter.eventually_gt_atTop 0] with t ht
    rw [Real.div_rpow hyl.le ht.le, Real.rpow_neg ht.le]
    ring
  have h3 := (h1.congr' h2).const_mul (l ^ 2 * (1 / (1 - l)))
  have h4 := h3.const_add l
  simpa using h4

/-- Property (vi). At a fixed income, a lower floor `y̲(h)`, that is a smaller
human capital when `y̲` rises in `h`, or a larger unobserved component, gives
larger spending. -/
theorem x_anti_floor {l yl₁ yl₂ y : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (h1 : 0 ≤ yl₁)
    (h12 : yl₁ < yl₂) (hy : 0 < y) : x l yl₂ y < x l yl₁ y := by
  have ha : 0 < 1 / (1 - l) := by
    have : 0 < 1 - l := by linarith
    positivity
  have := Real.rpow_lt_rpow (div_nonneg h1 hy.le) (div_lt_div_of_pos_right h12 hy) ha
  unfold x
  have : 0 < l * y := by positivity
  nlinarith

/-- What Proposition 2 needs. A convex lowest income with `y̲(0) ≤ 0` makes
`y̲(h)/h` non-decreasing (reading MN-D1). -/
theorem ratio_mono_of_convex {f : ℝ → ℝ} (hf : ConvexOn ℝ (Set.Ici 0) f) (h0 : f 0 ≤ 0) :
    MonotoneOn (fun h => f h / h) (Set.Ioi 0) := by
  intro h₁ hh₁ h₂ hh₂ h12
  simp only [Set.mem_Ioi] at hh₁ hh₂
  have hw : 0 ≤ h₁ / h₂ := by positivity
  have hw' : 0 ≤ 1 - h₁ / h₂ := by rw [sub_nonneg, div_le_one hh₂]; exact h12
  have hc := hf.2 (Set.mem_Ici.2 hh₂.le) (Set.mem_Ici.2 le_rfl) hw hw' (by ring)
  simp only [smul_eq_mul, mul_zero, add_zero] at hc
  have e : h₁ / h₂ * h₂ = h₁ := by field_simp
  rw [e] at hc
  have : f h₁ ≤ h₁ / h₂ * f h₂ := by nlinarith
  show f h₁ / h₁ ≤ f h₂ / h₂
  rw [div_le_div_iff₀ hh₁ hh₂]
  have := mul_le_mul_of_nonneg_left this hh₂.le
  field_simp at this
  linarith

/-- Proposition 2 under reading MN-D1. If `y̲` is non-negative and convex on
`[0, ∞)` with `y̲(0) ≤ 0`, the share of the average person falls in `h`. -/
theorem prop2_corrected {l : ℝ} (hl0 : 0 < l) (hl1 : l < 1) {yl : ℝ → ℝ}
    (hconv : ConvexOn ℝ (Set.Ici 0) yl) (h0 : yl 0 ≤ 0) (hnn : ∀ h, 0 < h → 0 ≤ yl h) :
    AntitoneOn (fun h => x l (yl h) h / h) (Set.Ioi 0) := by
  intro h₁ hh₁ h₂ hh₂ h12
  simp only [Set.mem_Ioi] at hh₁ hh₂
  show x l (yl h₂) h₂ / h₂ ≤ x l (yl h₁) h₁ / h₁
  rw [share_avg (ne_of_gt hh₁), share_avg (ne_of_gt hh₂)]
  have hr := ratio_mono_of_convex hconv h0 hh₁ hh₂ h12
  simp only at hr
  have ha : 0 ≤ 1 / (1 - l) := by
    have : 0 < 1 - l := by linarith
    positivity
  have := Real.rpow_le_rpow (div_nonneg (hnn h₁ hh₁) hh₁.le) hr ha
  nlinarith

/-- Proposition 3, the pointwise step of its proof. For `π ≥ −|π̲|` and
`h ≥ |π̲|`, `(h − |π̲|)/(h + π)` rises in `h`. The code writes `p` for `|π̲|`
and `w` for `π`, since `π` is reserved. -/
theorem prop3_pointwise {p w h₁ h₂ : ℝ} (hw : -p < w) (h1 : p ≤ h₁) (h12 : h₁ ≤ h₂) :
    (h₁ - p) / (h₁ + w) ≤ (h₂ - p) / (h₂ + w) := by
  have hp1 : 0 < h₁ + w := by linarith
  have hp2 : 0 < h₂ + w := by linarith
  rw [div_le_div_iff₀ hp1 hp2]
  nlinarith

/-- Proposition 3. For `h ≥ |π̲|`, under any distribution of `π` with
`π > −|π̲|` almost surely, `E[((h − |π̲|)/(h + π))^{1/(1−λ)}]` rises in `h`. -/
theorem prop3_integral {l p : ℝ} (hl1 : l < 1) (μ : MeasureTheory.Measure ℝ)
    [MeasureTheory.IsProbabilityMeasure μ] (hsupp : ∀ᵐ w ∂μ, -p < w) {h₁ h₂ : ℝ}
    (h1 : p ≤ h₁) (h12 : h₁ ≤ h₂) :
    ∫ w, ((h₁ - p) / (h₁ + w)) ^ (1 / (1 - l)) ∂μ ≤
      ∫ w, ((h₂ - p) / (h₂ + w)) ^ (1 / (1 - l)) ∂μ := by
  have ha : 0 ≤ 1 / (1 - l) := by
    have : 0 < 1 - l := by linarith
    positivity
  have hint : ∀ h, p ≤ h → MeasureTheory.Integrable
      (fun w => ((h - p) / (h + w)) ^ (1 / (1 - l))) μ := by
    intro h hh
    refine MeasureTheory.Integrable.mono' (MeasureTheory.integrable_const (1 : ℝ)) ?_ ?_
    · exact (by fun_prop : Measurable fun w : ℝ => ((h - p) / (h + w)) ^ (1 / (1 - l))).aestronglyMeasurable
    · filter_upwards [hsupp] with w hw
      have hpos : 0 < h + w := by linarith
      have h0 : 0 ≤ (h - p) / (h + w) := div_nonneg (by linarith) hpos.le
      have h1' : (h - p) / (h + w) ≤ 1 := (div_le_one hpos).2 (by linarith)
      rw [Real.norm_eq_abs, abs_of_nonneg (Real.rpow_nonneg h0 _)]
      exact Real.rpow_le_one h0 h1' ha
  apply MeasureTheory.integral_mono_ae (hint h₁ h1) (hint h₂ (le_trans h1 h12))
  filter_upwards [hsupp] with w hw
  have hp1 : 0 < h₁ + w := by linarith
  exact Real.rpow_le_rpow (div_nonneg (by linarith) hp1.le) (prop3_pointwise hw h1 h12) ha

/-- Proposition 3, the share. For `h ≥ |π̲|`, `E[x(h, y)/y]` is
`λ (1 − E[((h − |π̲|)/(h + π))^{1/(1−λ)}])`, so it falls in `h`, and it is at
most `λ`, the share for `h < |π̲|` (`remark_constant_share`). -/
theorem prop3 {l p : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (μ : MeasureTheory.Measure ℝ)
    [MeasureTheory.IsProbabilityMeasure μ] (hsupp : ∀ᵐ w ∂μ, -p < w) {h₁ h₂ : ℝ}
    (h1 : p ≤ h₁) (h12 : h₁ ≤ h₂) :
    ∫ w, x l (h₂ - p) (h₂ + w) / (h₂ + w) ∂μ ≤ ∫ w, x l (h₁ - p) (h₁ + w) / (h₁ + w) ∂μ ∧
      ∫ w, x l (h₁ - p) (h₁ + w) / (h₁ + w) ∂μ ≤ l := by
  have ha : 0 ≤ 1 / (1 - l) := by
    have : 0 < 1 - l := by linarith
    positivity
  have hint : ∀ h, p ≤ h → MeasureTheory.Integrable
      (fun w => ((h - p) / (h + w)) ^ (1 / (1 - l))) μ := by
    intro h hh
    refine MeasureTheory.Integrable.mono' (MeasureTheory.integrable_const (1 : ℝ)) ?_ ?_
    · exact (by fun_prop : Measurable fun w : ℝ => ((h - p) / (h + w)) ^ (1 / (1 - l))).aestronglyMeasurable
    · filter_upwards [hsupp] with w hw
      have hpos : 0 < h + w := by linarith
      have h0 : 0 ≤ (h - p) / (h + w) := div_nonneg (by linarith) hpos.le
      have h1' : (h - p) / (h + w) ≤ 1 := (div_le_one hpos).2 (by linarith)
      rw [Real.norm_eq_abs, abs_of_nonneg (Real.rpow_nonneg h0 _)]
      exact Real.rpow_le_one h0 h1' ha
  have hsh : ∀ h, p ≤ h → ∫ w, x l (h - p) (h + w) / (h + w) ∂μ
      = l * (1 - ∫ w, ((h - p) / (h + w)) ^ (1 / (1 - l)) ∂μ) := by
    intro h hh
    have e : ∀ᵐ w ∂μ, x l (h - p) (h + w) / (h + w)
        = l * (1 - ((h - p) / (h + w)) ^ (1 / (1 - l))) := by
      filter_upwards [hsupp] with w hw
      exact share_avg (by linarith)
    rw [MeasureTheory.integral_congr_ae e, MeasureTheory.integral_const_mul,
      MeasureTheory.integral_sub (MeasureTheory.integrable_const _) (hint h hh)]
    simp
  have hnn : 0 ≤ ∫ w, ((h₁ - p) / (h₁ + w)) ^ (1 / (1 - l)) ∂μ := by
    apply MeasureTheory.integral_nonneg_of_ae
    filter_upwards [hsupp] with w hw
    exact Real.rpow_nonneg (div_nonneg (by linarith) (by linarith)) _
  have hmono := prop3_integral hl1 μ hsupp h1 h12
  rw [hsh h₁ h1, hsh h₂ (le_trans h1 h12)]
  constructor <;> nlinarith

/-- Footnote 14. With status the rank `F(y) = y/ȳ` under a uniform
distribution on `[0, ȳ]`, the first order condition is that of eq. (7). -/
theorem fn14_uniform {l y xx ybar dydx : ℝ} (hb : 0 < ybar) (hy : 0 < y) (hd : dydx ≠ 0) :
    l / (1 - l) * (y - xx) / (y / ybar) = 1 / (1 / ybar * dydx) ↔
      l / (1 - l) * (y - xx) / y = 1 / dydx := by
  have e1 : l / (1 - l) * (y - xx) / (y / ybar) = ybar * (l / (1 - l) * (y - xx) / y) := by
    field_simp
  have e2 : 1 / (1 / ybar * dydx) = ybar * (1 / dydx) := by
    field_simp
  rw [e1, e2]
  exact mul_right_inj' (ne_of_gt hb)

/-- Proposition A1, step (i). Spending more and being believed poorer is
dominated, so equilibrium beliefs are non-decreasing in spending. -/
theorem stepA1_i {l y xx xx' b b' : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hx : xx < xx')
    (hx' : xx' < y) (hb' : 0 < b') (hbb : b' ≤ b) : U l y xx' b' < U l y xx b := by
  unfold U
  have h1 : Real.log (y - xx') < Real.log (y - xx) := Real.log_lt_log (by linarith) (by linarith)
  have h2 : Real.log b' ≤ Real.log b := Real.log_le_log hb' hbb
  have h1l : 0 < 1 - l := by linarith
  nlinarith

/-- Proposition A1, step (ii), single crossing. If a person with income `y`
weakly prefers the larger spending `x` with belief `b` to the smaller `x'` with
belief `b'`, a richer person strictly prefers it, so spending is
non-decreasing in income. -/
theorem stepA1_ii {l y y' xx xx' b b' : ℝ} (hl1 : l < 1) (hy : y < y') (hxx : xx' < xx)
    (hxy : xx < y) (hpref : U l y xx' b' ≤ U l y xx b) : U l y' xx' b' < U l y' xx b := by
  unfold U at *
  have h1l : 0 < 1 - l := by linarith
  have hr : (y - xx) / (y - xx') < (y' - xx) / (y' - xx') := by
    rw [div_lt_div_iff₀ (by linarith) (by linarith)]
    nlinarith
  have hl := Real.log_lt_log (div_pos (by linarith) (by linarith)) hr
  rw [Real.log_div (by linarith) (by linarith), Real.log_div (by linarith) (by linarith)] at hl
  nlinarith

/-- Section 4. The mapping in the paper's closed form,
`φ(h) = θ + γβ{(1 − λ) h + λ y̲(h)^{1/(1−λ)}/h^{λ/(1−λ)}}`. -/
theorem phi_closed_form {θ γ β l yl h : ℝ} (hl1 : l < 1) (hyl : 0 ≤ yl) (hh : 0 < h) :
    φ θ γ β l yl h
      = θ + γ * β * ((1 - l) * h + l * (yl ^ (1 / (1 - l)) / h ^ (l / (1 - l)))) := by
  have h1l : 1 - l ≠ 0 := by linarith
  have e : l / (1 - l) = 1 / (1 - l) - 1 := by field_simp; ring
  rw [e, Real.rpow_sub_one (ne_of_gt hh)]
  unfold φ x
  rw [Real.div_rpow hyl hh.le]
  have : 0 < h ^ (1 / (1 - l)) := Real.rpow_pos_of_pos hh _
  field_simp
  ring

/-- Eqs. (4) and (5). Given `y − x`, the Cobb–Douglas bundle
`c^{1−β} b^β` is maximised by `c = (1 − β)(y − x)` and `b = β (y − x)`. -/
theorem eq45_split {β Y c b : ℝ} (hβ0 : 0 < β) (hβ1 : β < 1) (hY : 0 < Y) (hc : 0 ≤ c)
    (hb : 0 ≤ b) (hcb : c + b ≤ Y) :
    c ^ (1 - β) * b ^ β ≤ ((1 - β) * Y) ^ (1 - β) * (β * Y) ^ β := by
  have h1 : 0 < (1 - β) * Y := mul_pos (by linarith) hY
  have h2 : 0 < β * Y := by positivity
  have hg := Real.geom_mean_le_arith_mean2_weighted (w₁ := 1 - β) (w₂ := β)
    (p₁ := c / ((1 - β) * Y)) (p₂ := b / (β * Y)) (by linarith) hβ0.le
    (div_nonneg hc h1.le) (div_nonneg hb h2.le) (by ring)
  have h1β : (1 - β) ≠ 0 := by linarith
  have hs : (1 - β) * (c / ((1 - β) * Y)) + β * (b / (β * Y)) = (c + b) / Y := by
    field_simp
  rw [hs, Real.div_rpow hc h1.le, Real.div_rpow hb h2.le] at hg
  have hle : (c + b) / Y ≤ 1 := (div_le_one hY).2 hcb
  have hp1 : 0 < ((1 - β) * Y) ^ (1 - β) := Real.rpow_pos_of_pos h1 _
  have hp2 : 0 < (β * Y) ^ β := Real.rpow_pos_of_pos h2 _
  have e : c ^ (1 - β) / ((1 - β) * Y) ^ (1 - β) * (b ^ β / (β * Y) ^ β)
      = c ^ (1 - β) * b ^ β / (((1 - β) * Y) ^ (1 - β) * (β * Y) ^ β) := by
    field_simp
  rw [e, div_le_iff₀ (by positivity)] at hg
  have : (c + b) / Y * (((1 - β) * Y) ^ (1 - β) * (β * Y) ^ β)
      ≤ 1 * (((1 - β) * Y) ^ (1 - β) * (β * Y) ^ β) :=
    mul_le_mul_of_nonneg_right hle (by positivity)
  linarith

/-- Eq. (6). With the split of eqs. (4) and (5) and the constant
`B = [(1 − β)^{β−1} β^{−β}]^{1−λ}`, utility (3) is `(y − x)^{1−λ} S^λ`. -/
theorem eq6_reduced {β l Y S : ℝ} (hβ0 : 0 < β) (hβ1 : β < 1) (hY : 0 < Y) :
    ((1 - β) ^ (β - 1) * β ^ (-β)) ^ (1 - l) *
        (((1 - β) * Y) ^ (1 - β) * (β * Y) ^ β) ^ (1 - l) * S ^ l
      = Y ^ (1 - l) * S ^ l := by
  have h1β : 0 < 1 - β := by linarith
  have hin : ((1 - β) * Y) ^ (1 - β) * (β * Y) ^ β = (1 - β) ^ (1 - β) * β ^ β * Y := by
    rw [Real.mul_rpow h1β.le hY.le, Real.mul_rpow hβ0.le hY.le]
    have : Y ^ (1 - β) * Y ^ β = Y := by
      rw [← Real.rpow_add hY]; simp
    calc (1 - β) ^ (1 - β) * Y ^ (1 - β) * (β ^ β * Y ^ β)
        = (1 - β) ^ (1 - β) * β ^ β * (Y ^ (1 - β) * Y ^ β) := by ring
      _ = _ := by rw [this]
  have hk : (1 - β) ^ (β - 1) * β ^ (-β) * ((1 - β) ^ (1 - β) * β ^ β) = 1 := by
    have e1 : (1 - β) ^ (β - 1) * (1 - β) ^ (1 - β) = 1 := by
      rw [← Real.rpow_add h1β]; simp
    have e2 : β ^ (-β) * β ^ β = 1 := by
      rw [← Real.rpow_add hβ0]; simp
    calc (1 - β) ^ (β - 1) * β ^ (-β) * ((1 - β) ^ (1 - β) * β ^ β)
        = ((1 - β) ^ (β - 1) * (1 - β) ^ (1 - β)) * (β ^ (-β) * β ^ β) := by ring
      _ = 1 := by rw [e1, e2]; ring
  have hkn : 0 ≤ (1 - β) ^ (β - 1) * β ^ (-β) := by positivity
  have hin' : 0 ≤ (1 - β) ^ (1 - β) * β ^ β * Y := by positivity
  rw [hin, ← Real.mul_rpow hkn hin']
  congr 2
  calc (1 - β) ^ (β - 1) * β ^ (-β) * ((1 - β) ^ (1 - β) * β ^ β * Y)
      = ((1 - β) ^ (β - 1) * β ^ (-β) * ((1 - β) ^ (1 - β) * β ^ β)) * Y := by ring
    _ = Y := by rw [hk]; ring

/-- Eq. (8). Along the separating path `ỹ = y`, eq. (9) satisfies the implicit
equation `ỹ^{1/(1−λ)} − (x/λ) ỹ^{λ/(1−λ)} = y̲^{1/(1−λ)}`. -/
theorem eq8_from_eq9 {l yl y : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hyl : 0 ≤ yl) (hy : 0 < y) :
    y ^ (1 / (1 - l)) - x l yl y / l * y ^ (l / (1 - l)) = yl ^ (1 / (1 - l)) := by
  have h1l : 1 - l ≠ 0 := by linarith
  have e : l / (1 - l) = 1 / (1 - l) - 1 := by field_simp; ring
  rw [e, Real.rpow_sub_one (ne_of_gt hy)]
  unfold x
  rw [Real.div_rpow hyl hy.le]
  have : 0 < y ^ (1 / (1 - l)) := Real.rpow_pos_of_pos hy _
  field_simp
  ring

/-! ### Section 4, case (ii)

With a constant lower bound, `y̲(h) = h − p` for `h ≥ p`, where `p = −π̲`.
Write `a = 1/(1 − λ) > 1` and `kk(h) = (h − p)^a h^{1−a}`, so that
`φ(h) = θ + γβ((1 − λ) h + λ kk(h))`. -/

/-- The nonlinear part of `φ` in case (ii). -/
noncomputable def kk (a p h : ℝ) : ℝ := (h - p) ^ a * h ^ (1 - a)

/-- The slope of `kk`, written as `((h − p)/h)^{a−1} (1 + (a − 1) p/h)`. -/
noncomputable def D (a p h : ℝ) : ℝ := ((h - p) / h) ^ (a - 1) * (1 + (a - 1) * (p / h))

theorem phi_case_ii {θ γ β l p h : ℝ} (hl1 : l < 1) (hp : p ≤ h) (hh : 0 < h) :
    φ θ γ β l (h - p) h = θ + γ * β * ((1 - l) * h + l * kk (1 / (1 - l)) p h) := by
  rw [phi_closed_form hl1 (by linarith) hh]
  unfold kk
  have h1l : 1 - l ≠ 0 := by linarith
  have e : 1 - 1 / (1 - l) = -(l / (1 - l)) := by field_simp; ring
  rw [e, Real.rpow_neg hh.le, div_eq_mul_inv]

theorem kk_deriv {a p h : ℝ} (hp : 0 < p) (hph : p < h) :
    HasDerivAt (kk a p) (D a p h) h := by
  have hh : 0 < h := lt_trans hp hph
  have hhp : 0 < h - p := by linarith
  have h1 : HasDerivAt (fun t => (t - p) ^ a) (1 * a * (h - p) ^ (a - 1)) h :=
    ((hasDerivAt_id h).sub_const p).rpow_const (Or.inl (ne_of_gt hhp))
  have h2 : HasDerivAt (fun t => t ^ (1 - a)) ((1 - a) * h ^ (1 - a - 1)) h :=
    Real.hasDerivAt_rpow_const (Or.inl (ne_of_gt hh))
  have := h1.mul h2
  unfold kk D
  convert this using 1
  rw [Real.div_rpow hhp.le hh.le]
  have q1 : (h - p) ^ (a - 1) = (h - p) ^ a / (h - p) := Real.rpow_sub_one (ne_of_gt hhp) a
  have q2 : h ^ (a - 1) = h ^ a / h := Real.rpow_sub_one (ne_of_gt hh) a
  have q3 : h ^ (1 - a) = h / h ^ a := by
    rw [show (1 : ℝ) - a = -(a - 1) by ring, Real.rpow_neg hh.le, q2]; field_simp
  have q4 : h ^ (1 - a - 1) = 1 / h ^ a := by
    rw [show (1 : ℝ) - a - 1 = -a by ring, Real.rpow_neg hh.le]; field_simp
  rw [q1, q2, q3, q4]
  have ha : 0 < h ^ a := Real.rpow_pos_of_pos hh a
  have hpa : 0 < (h - p) ^ a := Real.rpow_pos_of_pos hhp a
  field_simp
  ring

theorem D_pos {a p h : ℝ} (ha : 1 < a) (hp : 0 < p) (hph : p < h) : 0 < D a p h := by
  have hh : 0 < h := lt_trans hp hph
  unfold D
  have : 0 < (h - p) / h := div_pos (by linarith) hh
  have : 0 < ((h - p) / h) ^ (a - 1) := Real.rpow_pos_of_pos this _
  have : 0 < 1 + (a - 1) * (p / h) := by
    have : 0 < (a - 1) * (p / h) := mul_pos (by linarith) (div_pos hp hh)
    linarith
  positivity

theorem phi_ii_deriv {θ γ β l p h : ℝ} (_hl1 : l < 1) (hp : 0 < p) (hph : p < h) :
    HasDerivAt (fun t => θ + γ * β * ((1 - l) * t + l * kk (1 / (1 - l)) p t))
      (γ * β * ((1 - l) + l * D (1 / (1 - l)) p h)) h := by
  have := ((((hasDerivAt_id h).const_mul (1 - l)).add
    ((kk_deriv (a := 1 / (1 - l)) hp hph).const_mul l)).const_mul (γ * β)).const_add θ
  convert this using 1
  ring

/-- Section 4, case (ii). `φ` is increasing above `p`. -/
theorem phi_ii_strictMono {θ γ β l p : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hγβ : 0 < γ * β)
    (hp : 0 < p) :
    StrictMonoOn (fun t => θ + γ * β * ((1 - l) * t + l * kk (1 / (1 - l)) p t)) (Set.Ioi p) := by
  have ha : 1 < 1 / (1 - l) := by rw [lt_div_iff₀ (by linarith)]; linarith
  apply strictMonoOn_of_deriv_pos (convex_Ioi p)
  · intro t ht
    exact (phi_ii_deriv hl1 hp ht).continuousAt.continuousWithinAt
  · intro t ht
    rw [interior_Ioi] at ht
    rw [(phi_ii_deriv hl1 hp ht).deriv]
    have := D_pos ha hp ht
    have : 0 < (1 - l) + l * D (1 / (1 - l)) p t := by
      have : 0 < l * D (1 / (1 - l)) p t := mul_pos hl0 this
      linarith
    positivity

/-- Section 4, case (ii). The slope of `φ` tends to `γβ` as `h` grows, the
growth rate `βγ − 1` of the text. -/
theorem phi_ii_slope_atTop {γ β l p : ℝ} (_hp : 0 < p) :
    Filter.Tendsto (fun h => γ * β * ((1 - l) + l * D (1 / (1 - l)) p h)) Filter.atTop
      (nhds (γ * β)) := by
  have h1 : Filter.Tendsto (fun h : ℝ => p / h) Filter.atTop (nhds 0) :=
    tendsto_const_nhds.div_atTop Filter.tendsto_id
  have h2 : Filter.Tendsto (fun h : ℝ => (h - p) / h) Filter.atTop (nhds 1) := by
    have : Filter.Tendsto (fun h : ℝ => 1 - p / h) Filter.atTop (nhds (1 - 0)) :=
      tendsto_const_nhds.sub h1
    rw [sub_zero] at this
    apply this.congr'
    filter_upwards [Filter.eventually_gt_atTop 0] with h hh
    field_simp
  have h3 : Filter.Tendsto (fun h : ℝ => ((h - p) / h) ^ (1 / (1 - l) - 1)) Filter.atTop
      (nhds ((1 : ℝ) ^ (1 / (1 - l) - 1))) :=
    (Real.continuousAt_rpow_const 1 _ (Or.inl one_ne_zero)).tendsto.comp h2
  rw [Real.one_rpow] at h3
  have h4 := h3.mul ((h1.const_mul (1 / (1 - l) - 1)).const_add 1)
  have h5 := ((h4.const_mul l).const_add (1 - l)).const_mul (γ * β)
  simp only [mul_zero, add_zero, mul_one] at h5
  have e : γ * β * (1 - l + l) = γ * β := by ring
  rw [e] at h5
  exact h5

/-- Section 4, case (ii). As `h` falls to `p` the slope of `φ` tends to
`γβ(1 − λ)`. -/
theorem phi_ii_slope_at_p {γ β l p : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hp : 0 < p) :
    Filter.Tendsto (fun h => γ * β * ((1 - l) + l * D (1 / (1 - l)) p h))
      (nhdsWithin p (Set.Ioi p)) (nhds (γ * β * (1 - l))) := by
  have hpos : 0 < 1 / (1 - l) - 1 := by
    have : 1 < 1 / (1 - l) := by rw [lt_div_iff₀ (by linarith)]; linarith
    linarith
  have hc : ContinuousAt (fun h : ℝ => ((h - p) / h) ^ (1 / (1 - l) - 1) *
      (1 + (1 / (1 - l) - 1) * (p / h))) p := by
    apply ContinuousAt.mul
    · apply ContinuousAt.rpow_const
      · exact (continuousAt_id.sub continuousAt_const).div continuousAt_id (ne_of_gt hp)
      · exact Or.inr hpos.le
    · exact continuousAt_const.add (continuousAt_const.mul
        (continuousAt_const.div continuousAt_id (ne_of_gt hp)))
  have h := (hc.tendsto.mono_left (nhdsWithin_le_nhds (s := Set.Ioi p)))
  have hz : ((p - p) / p) ^ (1 / (1 - l) - 1) * (1 + (1 / (1 - l) - 1) * (p / p)) = 0 := by
    rw [sub_self, zero_div, Real.zero_rpow (ne_of_gt hpos)]; ring
  rw [hz] at h
  have h5 := ((h.const_mul l).const_add (1 - l)).const_mul (γ * β)
  simp only [mul_zero, add_zero] at h5
  exact h5

/-- The factor `m(t) = (1 − t)^{a−1}(1 + (a − 1) t)` falls in `t ∈ (0, 1)`. -/
theorem m_antitone {a : ℝ} (ha : 1 < a) :
    AntitoneOn (fun t : ℝ => (1 - t) ^ (a - 1) * (1 + (a - 1) * t)) (Set.Icc 0 1) := by
  have hd : ∀ t ∈ Set.Ioo (0 : ℝ) 1, HasDerivAt (fun t : ℝ => (1 - t) ^ (a - 1) * (1 + (a - 1) * t))
      (-(a - 1) * a * t * (1 - t) ^ (a - 2)) t := by
    intro t ht
    have h1t : 0 < 1 - t := by linarith [ht.2]
    have h1 : HasDerivAt (fun t : ℝ => (1 - t) ^ (a - 1)) ((0 - 1) * (a - 1) * (1 - t) ^ (a - 1 - 1)) t :=
      ((hasDerivAt_const t (1 : ℝ)).sub (hasDerivAt_id t)).rpow_const (Or.inl (ne_of_gt h1t))
    have h2 : HasDerivAt (fun t : ℝ => 1 + (a - 1) * t) ((a - 1) * 1) t :=
      ((hasDerivAt_id t).const_mul (a - 1)).const_add 1
    convert h1.mul h2 using 1
    have q : (1 - t) ^ (a - 1) = (1 - t) ^ (a - 1 - 1) * (1 - t) := by
      rw [Real.rpow_sub_one (ne_of_gt h1t) (a - 1)]; field_simp
    rw [q, show a - 1 - 1 = a - 2 by ring]
    ring
  apply antitoneOn_of_deriv_nonpos (convex_Icc 0 1)
  · intro t ht
    apply ContinuousAt.continuousWithinAt
    apply ContinuousAt.mul
    · apply ContinuousAt.rpow_const (continuousAt_const.sub continuousAt_id)
      exact Or.inr (by linarith)
    · exact continuousAt_const.add (continuousAt_const.mul continuousAt_id)
  · intro t ht
    rw [interior_Icc] at ht
    exact (hd t ht).differentiableAt.differentiableWithinAt
  · intro t ht
    rw [interior_Icc] at ht
    rw [(hd t ht).deriv]
    have h1t : 0 < 1 - t := by linarith [ht.2]
    have : 0 ≤ (1 - t) ^ (a - 2) := Real.rpow_nonneg h1t.le _
    have : 0 ≤ (a - 1) * a * t * (1 - t) ^ (a - 2) := by
      have : 0 ≤ a - 1 := by linarith
      have : 0 ≤ a := by linarith
      have : 0 ≤ t := ht.1.le
      positivity
    linarith

theorem D_eq_m {a p h : ℝ} (hh : 0 < h) :
    D a p h = (1 - p / h) ^ (a - 1) * (1 + (a - 1) * (p / h)) := by
  unfold D; congr 2; field_simp

/-- Section 4, case (ii). `φ` is convex above `p`. -/
theorem phi_ii_convex {θ γ β l p : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hγβ : 0 < γ * β)
    (hp : 0 < p) :
    ConvexOn ℝ (Set.Ioi p) (fun t => θ + γ * β * ((1 - l) * t + l * kk (1 / (1 - l)) p t)) := by
  have ha : 1 < 1 / (1 - l) := by rw [lt_div_iff₀ (by linarith)]; linarith
  apply MonotoneOn.convexOn_of_deriv (convex_Ioi p)
  · intro t ht
    exact (phi_ii_deriv hl1 hp ht).continuousAt.continuousWithinAt
  · intro t ht
    rw [interior_Ioi] at ht
    exact (phi_ii_deriv hl1 hp ht).differentiableAt.differentiableWithinAt
  · intro t₁ h₁ t₂ h₂ h12
    rw [interior_Ioi] at h₁ h₂
    simp only [Set.mem_Ioi] at h₁ h₂
    rw [(phi_ii_deriv hl1 hp h₁).deriv, (phi_ii_deriv hl1 hp h₂).deriv]
    have hh₁ : 0 < t₁ := lt_trans hp h₁
    have hh₂ : 0 < t₂ := lt_trans hp h₂
    rw [D_eq_m hh₁, D_eq_m hh₂]
    have hm := m_antitone ha
      ⟨(div_pos hp hh₂).le, (div_le_one hh₂).2 (by linarith)⟩
      ⟨(div_pos hp hh₁).le, (div_le_one hh₁).2 (by linarith)⟩
      (div_le_div_of_nonneg_left hp.le hh₁ h12)
    simp only at hm
    have : l * ((1 - p / t₁) ^ (1 / (1 - l) - 1) * (1 + (1 / (1 - l) - 1) * (p / t₁))) ≤
        l * ((1 - p / t₂) ^ (1 / (1 - l) - 1) * (1 + (1 / (1 - l) - 1) * (p / t₂))) :=
      mul_le_mul_of_nonneg_left hm hl0.le
    nlinarith

/-- Section 4, the two crossings of Figure 2 (existence). With `βγ > 1`,
`(1 − λ)βγ < 1`, `θ > 0` and the paper's condition `π̲ < θ/[γβ(1 − λ) − 1]`,
that is `p > θ/(1 − γβ(1 − λ))`, the linear part of `φ` below `p` has a fixed
point, the low-income steady state, and the part above `p` has another. -/
theorem phi_two_crossings {θ γ β l p : ℝ} (hl0 : 0 < l) (hl1 : l < 1) (hθ : 0 < θ)
    (hgb : 1 < γ * β) (hgb1 : (1 - l) * (γ * β) < 1) (hp : θ / (1 - (1 - l) * (γ * β)) < p) :
    (∃ h₁ ∈ Set.Ioo 0 p, θ + γ * β * (1 - l) * h₁ = h₁) ∧
      (∃ h₂, p < h₂ ∧ θ + γ * β * ((1 - l) * h₂ + l * kk (1 / (1 - l)) p h₂) = h₂) := by
  have hk : 0 < 1 - (1 - l) * (γ * β) := by linarith
  have hp0 : 0 < p := lt_trans (div_pos hθ hk) hp
  have hlow : θ < (1 - (1 - l) * (γ * β)) * p := by
    rw [div_lt_iff₀ hk] at hp; linarith
  refine ⟨⟨θ / (1 - (1 - l) * (γ * β)), ⟨div_pos hθ hk, hp⟩, ?_⟩, ?_⟩
  · have e : θ / (1 - (1 - l) * (γ * β)) * (1 - (1 - l) * (γ * β)) = θ :=
      div_mul_cancel₀ θ (ne_of_gt hk)
    linear_combination (-1 : ℝ) * e
  set a := 1 / (1 - l) with ha_def
  have ha : 1 < a := by rw [ha_def, lt_div_iff₀ (by linarith)]; linarith
  set g : ℝ → ℝ := fun h => θ + γ * β * ((1 - l) * h + l * kk a p h) - h with hg
  have hgp : g p < 0 := by
    have : kk a p p = 0 := by
      unfold kk; rw [sub_self, Real.zero_rpow (by linarith)]; ring
    simp only [hg, this]
    nlinarith
  set H := γ * β * l * a * p / (γ * β - 1) + p + 1 with hH
  have hgb' : 0 < γ * β - 1 := by linarith
  have hHp : p < H := by
    have : 0 ≤ γ * β * l * a * p / (γ * β - 1) := by
      have : 0 ≤ γ * β * l * a * p := by
        have : 0 < γ * β := by linarith
        have : 0 < a := by linarith
        positivity
      positivity
    linarith
  have hH0 : 0 < H := lt_trans hp0 hHp
  have hkkH : H - a * p ≤ kk a p H := by
    have hb := one_add_mul_self_le_rpow_one_add (s := -(p / H))
      (by have : p / H ≤ 1 := (div_le_one hH0).2 hHp.le
          linarith) ha.le
    have e1 : kk a p H = H * (1 + -(p / H)) ^ a := by
      unfold kk
      have : 1 + -(p / H) = (H - p) / H := by field_simp; ring
      rw [this, Real.div_rpow (by linarith) hH0.le]
      have q : H ^ (1 - a) = H / H ^ a := by
        rw [Real.rpow_sub hH0, Real.rpow_one]
      rw [q]
      have : 0 < H ^ a := Real.rpow_pos_of_pos hH0 a
      field_simp
    rw [e1]
    have := mul_le_mul_of_nonneg_left hb hH0.le
    have e2 : H * (1 + a * -(p / H)) = H - a * p := by field_simp; ring
    linarith
  have hgH : 0 < g H := by
    simp only [hg]
    have hl : l * (H - a * p) ≤ l * kk a p H := mul_le_mul_of_nonneg_left hkkH hl0.le
    have hgbp : 0 < γ * β := by linarith
    have h1 := mul_le_mul_of_nonneg_left hl hgbp.le
    have h2 : (γ * β - 1) * H > γ * β * l * a * p := by
      have : (γ * β - 1) * (γ * β * l * a * p / (γ * β - 1)) = γ * β * l * a * p := by
        field_simp
      have h3 : (γ * β - 1) * H = (γ * β - 1) * (γ * β * l * a * p / (γ * β - 1)) +
          (γ * β - 1) * (p + 1) := by rw [hH]; ring
      have : 0 < (γ * β - 1) * (p + 1) := mul_pos hgb' (by linarith)
      linarith
    nlinarith
  have hcont : ContinuousOn g (Set.Icc p H) := by
    intro t ht
    have ht0 : 0 < t := lt_of_lt_of_le hp0 ht.1
    apply ContinuousAt.continuousWithinAt
    simp only [hg]
    apply ContinuousAt.sub _ continuousAt_id
    apply continuousAt_const.add
    apply continuousAt_const.mul
    apply (continuousAt_const.mul continuousAt_id).add
    apply continuousAt_const.mul
    unfold kk
    apply ContinuousAt.mul
    · exact ContinuousAt.rpow_const (continuousAt_id.sub continuousAt_const) (Or.inr (by linarith))
    · exact ContinuousAt.rpow_const continuousAt_id (Or.inl (ne_of_gt ht0))
  obtain ⟨h₂, hh₂, hz⟩ := intermediate_value_Icc hHp.le hcont ⟨hgp.le, hgH.le⟩
  have hne : h₂ ≠ p := by
    intro he; rw [he] at hz; linarith
  refine ⟨h₂, lt_of_le_of_ne hh₂.1 (Ne.symm hne), ?_⟩
  simp only [hg] at hz
  linarith

end MoavNeeman2012
