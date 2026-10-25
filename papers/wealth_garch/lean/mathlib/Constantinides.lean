import Mathlib

namespace Constantinides

/-- An antiderivative of (x - U)(u - x), the shape of V' - k⁻ on [U, u] when μ = 0 and β → 0. -/
noncomputable def F (U u x : ℝ) : ℝ := -x ^ 3 / 3 + (U + u) * x ^ 2 / 2 - U * u * x

theorem F_deriv (U u x : ℝ) : HasDerivAt (F U u) ((x - U) * (u - x)) x := by
  show HasDerivAt (fun y => -y ^ 3 / 3 + (U + u) * y ^ 2 / 2 - U * u * y) _ x
  have h := ((((hasDerivAt_pow 3 x).div_const 3).neg).add
    (((hasDerivAt_pow 2 x).const_mul (U + u)).div_const 2)).sub
    ((hasDerivAt_id x).const_mul (U * u))
  have e : (fun y : ℝ => -y ^ 3 / 3 + (U + u) * y ^ 2 / 2 - U * u * y) =
      ((-fun y : ℝ => y ^ 3 / 3) + fun y => (U + u) * y ^ 2 / 2) - fun y => U * u * id y := by
    funext y; simp only [Pi.add_apply, Pi.sub_apply, Pi.neg_apply, id]; ring
  rw [e]
  exact h.congr_deriv (by push_cast; ring)

/-- The fixed cost equals the area between V' and the proportional cost, (u - U)³/6 in units of h/σ². -/
theorem gap_area (U u : ℝ) : F U u u - F U u U = (u - U) ^ 3 / 6 := by
  unfold F; ring

/-- The printed equation's factor 1/2 differs from the area's 1/6. -/
theorem control_printed_factor : ((1 : ℝ) / 2) ≠ 1 / 6 := by norm_num

/-- With a gap g solving g³ = 6σ²K/p, the gap rises in K. -/
theorem gap_rises_in_K {g₁ g₂ σ p K₁ K₂ : ℝ} (hg₂ : 0 ≤ g₂) (hσ : 0 < σ) (hp : 0 < p)
    (e₁ : g₁ ^ 3 = 6 * σ ^ 2 * K₁ / p) (e₂ : g₂ ^ 3 = 6 * σ ^ 2 * K₂ / p) (hK : K₁ < K₂) : g₁ < g₂ := by
  have : g₁ ^ 3 < g₂ ^ 3 := by
    rw [e₁, e₂]; apply div_lt_div_of_pos_right _ hp; nlinarith [sq_pos_of_pos hσ]
  exact lt_of_pow_lt_pow_left₀ 3 hg₂ this

/-- With a gap g solving g³ = 6σ²K/p, the gap falls in p. -/
theorem gap_falls_in_p {g₁ g₂ σ p₁ p₂ K : ℝ} (hg₁ : 0 ≤ g₁) (hσ : 0 < σ) (hK : 0 < K)
    (hp₁ : 0 < p₁) (e₁ : g₁ ^ 3 = 6 * σ ^ 2 * K / p₁) (e₂ : g₂ ^ 3 = 6 * σ ^ 2 * K / p₂) (hp : p₁ < p₂) :
    g₂ < g₁ := by
  have h6 : 0 < 6 * σ ^ 2 * K := by positivity
  have : g₂ ^ 3 < g₁ ^ 3 := by
    rw [e₁, e₂]; exact div_lt_div_of_pos_left h6 hp₁ hp
  exact lt_of_pow_lt_pow_left₀ 3 hg₁ this

/-- Given h and σ, the four thresholds pin the penalty slope p and the fixed cost K⁺:
    h(u + U) = -p(d + D) fixes p, and (D - d)³ = 6σ²K⁺/p then fixes K⁺. -/
theorem thresholds_pin_pK {h σ d D u U p₁ p₂ K₁ K₂ : ℝ} (hσ : 0 < σ) (hs : d + D ≠ 0)
    (hp₁ : 0 < p₁) (hp₂ : 0 < p₂)
    (s₁ : h * (u + U) = -p₁ * (d + D)) (s₂ : h * (u + U) = -p₂ * (d + D))
    (g₁ : (D - d) ^ 3 = 6 * σ ^ 2 * K₁ / p₁) (g₂ : (D - d) ^ 3 = 6 * σ ^ 2 * K₂ / p₂) :
    p₁ = p₂ ∧ K₁ = K₂ := by
  have hp : p₁ = p₂ := by
    have : (p₁ - p₂) * (d + D) = 0 := by linarith
    rcases mul_eq_zero.mp this with h0 | h0
    · linarith
    · exact absurd h0 hs
  refine ⟨hp, ?_⟩
  subst hp
  have := g₁.symm.trans g₂
  field_simp at this
  exact this

/-- Without the sum relation the slope is free: two slopes fit the same gap with matching K. -/
theorem control_gap_alone_does_not_pin :
    ((1 : ℝ) - 0) ^ 3 = 6 * 1 ^ 2 * (1 / 6) / 1 ∧ ((1 : ℝ) - 0) ^ 3 = 6 * 1 ^ 2 * (2 / 6) / 2 := by
  norm_num

end Constantinides
