import Mathlib

/-! Charles, Hurst and Roussanov, "Conspicuous Consumption and Race", NBER
Working Paper 13392 (September 2007), Section 4 and the Theory Appendix, in the
paper's notation. `μ` is the mean income `μ_G` of the reference group, `c` is
visible consumption, `g` the public's inference function from visible
consumption to income, and status is `s = g(c)/μ_G` (Theory Appendix).
`B` stands for the bracket `u''(y − c*) + α u''(c*) + s_cc(c*, μ)` of eq. (4),
negative by the second order condition. -/

namespace CharlesEtAl2007

open Real Set

/-- Theory Appendix. Status is inferred income relative to the group mean. -/
noncomputable def s (g : ℝ → ℝ) (c μ : ℝ) : ℝ := g c / μ

/-- The marginal status return to visible consumption, `s_c = g'(c)/μ_G`. -/
noncomputable def s_c (g' μ : ℝ) : ℝ := g' / μ

/-- `s_cμ < 0` in the Theory Appendix. The derivative of `s_c = g'(c)/μ` in `μ`
is `−g'(c)/μ²`, negative when inference rises in consumption, `g'(c) > 0`. The
richer the group, the smaller the status gain from a unit of visible
consumption. -/
theorem s_c_mu {g' μ : ℝ} (hμ : 0 < μ) :
    HasDerivAt (fun m => s_c g' m) (-g' / μ ^ 2) μ := by
  have h := (hasDerivAt_inv (ne_of_gt hμ)).const_mul g'
  unfold s_c
  convert h using 1
  ring

theorem s_c_mu_neg {g' μ : ℝ} (hg : 0 < g') (hμ : 0 < μ) : -g' / μ ^ 2 < 0 := by
  have : 0 < g' / μ ^ 2 := div_pos hg (by positivity)
  rw [neg_div]; linarith

/-- Eq. (4). If `s_cμ = −c*_μ B` with `B < 0` (second order condition), then
visible consumption falls in the group mean exactly when `s_cμ < 0`. -/
theorem eq4_sign {scmu cmu B : ℝ} (hB : B < 0) (h4 : scmu = -cmu * B) :
    cmu < 0 ↔ scmu < 0 := by
  subst h4
  constructor
  · intro h; nlinarith
  · intro h; by_contra hc; push_neg at hc; nlinarith

/-- Theory Appendix, the initial condition. Without status, the maximiser of
`log(y − c) + α log c` satisfies the first order condition at the share
`x_min = α/(1 + α)`. -/
theorem no_status_share {α y : ℝ} (hα : 0 < α) (hy : 0 < y) :
    α / (α * y / (1 + α)) - 1 / (y - α * y / (1 + α)) = 0 := by
  have h1 : (1 + α) ≠ 0 := by linarith
  have h2 : y - α * y / (1 + α) = y / (1 + α) := by field_simp; ring
  rw [h2]
  field_simp
  ring

/-- The function whose increment the Theory Appendix integrates,
`F(x) = −ln(1 − x) − α ln x`, so that `w(x) = μ_G F(x) + K`. -/
noncomputable def F (α x : ℝ) : ℝ := -Real.log (1 - x) - α * Real.log x

theorem F_deriv {α x : ℝ} (hx0 : 0 < x) (hx1 : x < 1) :
    HasDerivAt (F α) (1 / (1 - x) - α / x) x := by
  have h1 : HasDerivAt (fun t => Real.log (1 - t)) (-1 / (1 - x)) x := by
    have := ((hasDerivAt_id x).const_sub 1).log (by simp; linarith)
    simpa [div_eq_mul_inv] using this
  have h2 : HasDerivAt (fun t => α * Real.log t) (α * x⁻¹) x :=
    (Real.hasDerivAt_log (ne_of_gt hx0)).const_mul α
  have := h1.neg.sub h2
  unfold F
  convert this using 1
  · ring

/-- `F` rises above the no-status share `α/(1 + α)`, where `F'(x) > 0`. -/
theorem F_strictMonoOn {α : ℝ} (hα : 0 < α) :
    StrictMonoOn (F α) (Ico (α / (1 + α)) 1) := by
  have hx0 : 0 < α / (1 + α) := by positivity
  apply strictMonoOn_of_deriv_pos (convex_Ico _ _)
  · intro x hx
    exact (F_deriv (lt_of_lt_of_le hx0 hx.1) hx.2).continuousAt.continuousWithinAt
  · intro x hx
    rw [interior_Ico] at hx
    have hxpos : 0 < x := lt_trans hx0 hx.1
    rw [(F_deriv hxpos hx.2).deriv]
    have h1 : 0 < 1 - x := by linarith [hx.2]
    have h2 : α < x * (1 + α) := by
      have := hx.1; rw [div_lt_iff₀ (by linarith)] at this; linarith
    rw [sub_pos, div_lt_div_iff₀ hxpos h1]
    nlinarith

/-- Theory Appendix, the summary claim, for the solution the appendix states.
With `y = y_min + μ_G (F x − F x_min)`, two groups with means `μ₁ < μ₂` and the
same minimum income give a person of income `y > y_min` the shares `x₁` and
`x₂`, and `x₂ < x₁`. The share of visible consumption falls in the group mean. -/
theorem share_decreasing_in_mu {α μ₁ μ₂ x₁ x₂ Δ : ℝ} (hα : 0 < α)
    (hμ₁ : 0 < μ₁) (hμ : μ₁ < μ₂) (hΔ : 0 < Δ)
    (hx₁ : x₁ ∈ Ico (α / (1 + α)) 1) (hx₂ : x₂ ∈ Ico (α / (1 + α)) 1)
    (h₁ : μ₁ * (F α x₁ - F α (α / (1 + α))) = Δ)
    (h₂ : μ₂ * (F α x₂ - F α (α / (1 + α))) = Δ) : x₂ < x₁ := by
  have hμ₂ : 0 < μ₂ := lt_trans hμ₁ hμ
  have e₁ : F α x₁ - F α (α / (1 + α)) = Δ / μ₁ := by
    field_simp; linarith
  have e₂ : F α x₂ - F α (α / (1 + α)) = Δ / μ₂ := by
    field_simp; linarith
  have hlt : Δ / μ₂ < Δ / μ₁ := div_lt_div_of_pos_left hΔ hμ₁ hμ
  have hF : F α x₂ < F α x₁ := by linarith
  exact (F_strictMonoOn hα).lt_iff_lt hx₂ hx₁ |>.mp hF

/-- The change of variables in the Theory Appendix. With `c = x w(x)` and
`w(x) = g(c(x))`, the chain rule gives `w' = g' (w + x w')`. The appendix's
`w'(x) = μ_G (1/(1 − x) − α/x)` equals `g' w`, which differs from the chain
rule whenever `x g' w' ≠ 0` (reading CH-D2). -/
theorem chain_rule_gap {w w' g' x : ℝ} (hchain : w' = g' * (w + x * w'))
    (h : x * g' * w' ≠ 0) : w' ≠ g' * w := by
  intro he
  apply h
  linarith [hchain, he]

/-- Control: eq. (4) with `s_cμ > 0`, Ireland's case in footnote 25. Visible
consumption then rises in the group mean, so the decline needs `s_cμ < 0`. -/
theorem control_status_gain_rising {scmu cmu B : ℝ} (hB : B < 0)
    (h4 : scmu = -cmu * B) (hs : 0 < scmu) : 0 < cmu := by
  subst h4; nlinarith

/-- Control: without the second order condition, `B > 0`, a status function
with `s_cμ < 0` gives visible consumption rising in the group mean. -/
theorem control_soc_needed {scmu cmu B : ℝ} (hB : 0 < B)
    (h4 : scmu = -cmu * B) (hs : scmu < 0) : 0 < cmu := by
  subst h4; nlinarith

end CharlesEtAl2007
