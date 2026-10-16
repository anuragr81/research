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

end MoavNeeman2012
