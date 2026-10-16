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

/-! ### Section 4, the general model

`u1, u2` stand for `u'(y − c*)` and `u'(c*)`, `upp1, upp2` for `u''(y − c*)`
and `u''(c*)`, and `sc, scc, scμ` for the partial derivatives of status. -/

/-- Eq. (2), the objective in visible consumption `c`. -/
noncomputable def objective (u : ℝ → ℝ) (α : ℝ) (s : ℝ → ℝ → ℝ) (y μ c : ℝ) : ℝ :=
  u (y - c) + α * u c + s c μ

/-- Eq. (3). At an interior optimum, `s_c(c*, μ_G) = u'(y − c*) − α u'(c*)`. -/
theorem eq3_foc {u : ℝ → ℝ} {s : ℝ → ℝ → ℝ} {α y μ c u1 u2 sc : ℝ}
    (hu1 : HasDerivAt u u1 (y - c)) (hu2 : HasDerivAt u u2 c)
    (hs : HasDerivAt (fun t => s t μ) sc c) (hmax : IsLocalMax (objective u α s y μ) c) :
    sc = u1 - α * u2 := by
  have h1 : HasDerivAt (fun t => u (y - t)) (u1 * (0 - 1)) c :=
    hu1.comp c ((hasDerivAt_const c y).sub (hasDerivAt_id c))
  have h := (h1.add (hu2.const_mul α)).add hs
  have h0 := hmax.hasDerivAt_eq_zero h
  linarith

/-- The first implication of eq. (3), p. 21. With `u'` decreasing, a positive
status return `s_c > 0` puts visible consumption above the no-status level
`c₀`, where `u'(y − c₀) = α u'(c₀)`. -/
theorem status_raises_c {up : ℝ → ℝ} (hup : StrictAnti up) {α y c c0 sc : ℝ} (hα : 0 < α)
    (hfoc : sc = up (y - c) - α * up c) (h0 : up (y - c0) = α * up c0) (hsc : 0 < sc) :
    c0 < c := by
  by_contra h
  push_neg at h
  have a1 : up (y - c) ≤ up (y - c0) := hup.antitone (by linarith)
  have a2 : up c0 ≤ up c := hup.antitone h
  nlinarith

/-- Control: with `s_c < 0` visible consumption falls below the no-status
level, so the first implication needs `s_c > 0`. -/
theorem control_status_negative {up : ℝ → ℝ} (hup : StrictAnti up) {α y c c0 sc : ℝ}
    (hα : 0 < α) (hfoc : sc = up (y - c) - α * up c) (h0 : up (y - c0) = α * up c0)
    (hsc : sc < 0) : c < c0 := by
  by_contra h
  push_neg at h
  have a1 : up (y - c0) ≤ up (y - c) := hup.antitone (by linarith)
  have a2 : up c ≤ up c0 := hup.antitone h
  nlinarith

/-- The second implication of eq. (3), p. 21. Differentiating (3) in own income
gives `s_cc c_y = u''(y − c)(1 − c_y) − α u''(c) c_y`. With the bracket `B` of
eq. (4) negative and `u'' < 0`, `c_y = u''(y − c)/B > 0`. -/
theorem own_income_increasing {upp1 upp2 scc cy α : ℝ}
    (himpl : scc * cy = upp1 * (1 - cy) - α * upp2 * cy) (hB : upp1 + α * upp2 + scc < 0)
    (hu : upp1 < 0) : 0 < cy := by
  have e : cy * (upp1 + α * upp2 + scc) = upp1 := by linarith
  by_contra h
  push_neg at h
  nlinarith

/-- Control: without concavity at `y − c`, `u''(y − c) > 0`, visible consumption
falls in own income. -/
theorem control_concavity_needed {upp1 upp2 scc cy α : ℝ}
    (himpl : scc * cy = upp1 * (1 - cy) - α * upp2 * cy) (hB : upp1 + α * upp2 + scc < 0)
    (hu : 0 < upp1) : cy < 0 := by
  have e : cy * (upp1 + α * upp2 + scc) = upp1 := by linarith
  by_contra h
  push_neg at h
  nlinarith

/-- Eq. (4) from differentiating eq. (3) in `μ_G`,
`s_cc c_μ + s_cμ = −u''(y − c) c_μ − α u''(c) c_μ`. -/
theorem eq4_derivation {upp1 upp2 scc scmu cmu α : ℝ}
    (himpl : scc * cmu + scmu = -upp1 * cmu - α * upp2 * cmu) :
    scmu = -cmu * (upp1 + α * upp2 + scc) := by linarith

/-! ### Theory Appendix -/

/-- The appendix's first order condition. -/
theorem appendix_foc {g : ℝ → ℝ} {g' α y c μ : ℝ} (hg : HasDerivAt g g' c) (hc : 0 < c)
    (hcy : c < y) :
    HasDerivAt (fun t => Real.log (y - t) + α * Real.log t + g t / μ)
      (-1 / (y - c) + α / c + g' / μ) c := by
  have h1 : HasDerivAt (fun t => Real.log (y - t)) (-1 / (y - c)) c := by
    have := ((hasDerivAt_const c y).sub (hasDerivAt_id c)).log (by simp; linarith)
    simpa [div_eq_mul_inv] using this
  have h2 := (Real.hasDerivAt_log (ne_of_gt hc)).const_mul α
  have h3 := hg.div_const μ
  convert (h1.add h2).add h3 using 1

/-- The appendix's ODE. With correct inference, `g(c) = y`, the first order
condition gives `g'(c) = μ_G (1/(g(c) − c) − α/c)`. -/
theorem appendix_ode {g' α y c μ : ℝ} (hμ : μ ≠ 0) (hc : c ≠ 0) (hcy : y - c ≠ 0)
    (hfoc : α / c - 1 / (y - c) + g' / μ = 0) : g' = μ * (1 / (y - c) - α / c) := by
  field_simp at hfoc ⊢
  linarith

/-- The appendix's stated solution `w(x) = μ_G F(x) + K` has the derivative the
appendix writes, `w'(x) = μ_G (1/(1 − x) − α/x)`. -/
theorem appendix_solution {α μ K x : ℝ} (hx0 : 0 < x) (hx1 : x < 1) :
    HasDerivAt (fun t => μ * F α t + K) (μ * (1 / (1 - x) - α / x)) x := by
  simpa using ((F_deriv (α := α) hx0 hx1).const_mul μ).add_const K

/-- The initial condition fixes `K = y_min − μ_G F(x_min)`. -/
theorem appendix_K {α μ ymin : ℝ} :
    μ * F α (α / (1 + α)) + (ymin - μ * F α (α / (1 + α))) = ymin := by ring

/-- The appendix's summary, first half. At fixed `μ_G`, the share rises in
income. -/
theorem share_increasing_in_income {α μ x₁ x₂ y₁ y₂ ymin : ℝ} (hα : 0 < α) (hμ : 0 < μ)
    (hx₁ : x₁ ∈ Ico (α / (1 + α)) 1) (hx₂ : x₂ ∈ Ico (α / (1 + α)) 1)
    (h₁ : y₁ = ymin + μ * (F α x₁ - F α (α / (1 + α))))
    (h₂ : y₂ = ymin + μ * (F α x₂ - F α (α / (1 + α)))) (hy : y₁ < y₂) : x₁ < x₂ := by
  have : F α x₁ < F α x₂ := by
    by_contra h; push_neg at h; nlinarith
  exact (F_strictMonoOn hα).lt_iff_lt hx₁ hx₂ |>.mp this

/-- With income known, the appendix's status is `y/μ_G`. The group mean still
moves status at fixed income, so showing income does not remove the group's
role in this paper. -/
theorem known_income_status {y μ₁ μ₂ : ℝ} (hy : 0 < y) (hμ₁ : 0 < μ₁) (h : μ₁ < μ₂) :
    y / μ₂ < y / μ₁ := div_lt_div_of_pos_left hy hμ₁ h

/-! ### Section 5, the estimating model -/

/-- Eqs. (1), (5), (6) and (7) share one linear form. `Spec` holds the
coefficients on the race dummies, log reference-group income, and the
remaining controls collapsed to one index. -/
structure Spec where
  β0 : ℝ
  β1 : ℝ
  β2 : ℝ
  δ : ℝ
  ctrl : ℝ

/-- The predicted log visible spending. Eq. (1) has `δ = 0`; eq. (6) has
`β1 = β2 = 0`; eq. (7) has all terms. -/
def Spec.predict (m : Spec) (black hisp logmu index : ℝ) : ℝ :=
  m.β0 + m.β1 * black + m.β2 * hisp + m.δ * logmu + m.ctrl * index

/-- Footnote 12. If Blacks save less, `E = κ P + θ·Black` with `θ > 0`, and
visible spending rises in permanent income `P`, then controlling for total
expenditure `E` gives a Black coefficient `β1 − γθ/κ` below the true `β1`. -/
theorem fn12_lower_bound {β1 γ κ θ black E : ℝ} (hγ : 0 < γ) (hκ : 0 < κ) (hθ : 0 < θ) :
    β1 * black + γ * ((E - θ * black) / κ) = (β1 - γ * θ / κ) * black + (γ / κ) * E ∧
      β1 - γ * θ / κ < β1 := by
  refine ⟨by field_simp; ring, ?_⟩
  have : 0 < γ * θ / κ := by positivity
  linarith

/-- Control: with the same saving across races, `θ = 0`, there is no bias. -/
theorem control_fn12_no_bias {β1 γ κ : ℝ} : β1 - γ * 0 / κ = β1 := by ring

/-- Table 8, the omitted-variable identity. If `Cov(y, B) = b Var(B) + d Cov(m, B)`
(the long regression with an error uncorrelated with `B`), the short
coefficient is `b + d π` with `π = Cov(m, B)/Var(B)`. -/
theorem ovb {covyB varB covmB b d : ℝ} (hv : varB ≠ 0) (h : covyB = b * varB + d * covmB) :
    covyB / varB = b + d * (covmB / varB) := by
  rw [h]; field_simp

/-- Table 8 read through `ovb`. From the short coefficient 0.32, the long
coefficient −0.09 and `δ = −0.52`, the implied coefficient of the race dummy
on log own-group income is `−41/52`, about −0.79. -/
theorem table8_implied : (32 / 100 : ℝ) = -9 / 100 + (-52 / 100) * (-41 / 52) := by norm_num

/-- Control: with `d = 0` the short and long coefficients coincide. -/
theorem control_ovb_no_channel {b covmB varB : ℝ} : b + 0 * (covmB / varB) = b := by ring

/-- The "doubling" readings of pp. 26 to 28 (reading CH-D4). For an elasticity
`δ < 0`, doubling the group mean multiplies visible spending by `2^δ`, and the
exact reduction `1 − 2^δ` is smaller than `−δ`. -/
theorem doubling_reduction_lt {δ : ℝ} (hδ : δ < 0) : 1 - (2 : ℝ) ^ δ < -δ := by
  have hl : Real.log 2 < 1 := by
    have := Real.log_two_lt_d9; linarith
  have hl0 : 0 < Real.log 2 := Real.log_pos (by norm_num)
  have h := Real.add_one_le_exp (δ * Real.log 2)
  rw [Real.rpow_def_of_pos (by norm_num : (0 : ℝ) < 2)]
  have : Real.log 2 * δ = δ * Real.log 2 := by ring
  rw [this]
  nlinarith

/-! ### Numbers in the text that follow from printed numbers -/

/-- p. 7 and Table 1. White income is 67 and 61 percent above Black and
Hispanic income; White spending is 50 and 38 percent above. -/
theorem numbers_table1 :
    |(63967 : ℝ) / 38364 - 1.67| < 0.005 ∧ |(63967 : ℝ) / 39763 - 1.61| < 0.005 ∧
      |(11585 : ℝ) / 7715 - 1.50| < 0.005 ∧ |(11585 : ℝ) / 8420 - 1.38| < 0.005 := by
  norm_num [abs_lt]

/-- pp. 12–13. `$7,204 = 4 × $1,801` and `0.32 × $7,204 ≈ $2,300`. -/
theorem numbers_visible_dollars :
    (4 : ℝ) * 1801 = 7204 ∧ |(32 / 100 : ℝ) * 7204 - 2300| < 10 := by
  norm_num [abs_lt]

/-- The sample sizes. `36,706 + 6,760 + 5,292 = 48,758`; the marital rows of
Table 3 sum to it; the pooled sample of Table 7 is `6,760 + 5,292 = 12,052`. -/
theorem numbers_samples :
    (36706 : ℕ) + 6760 + 5292 = 48758 ∧ (10258 : ℕ) + 13287 + 25213 = 48758 ∧
      (6760 : ℕ) + 5292 = 12052 := by norm_num

/-- Reading CH-D9. The education rows of Table 3 sum to 55,521 and the two age
rows within 18 to 49 to 60,539, not to the 48,758 of the sample. -/
theorem numbers_table3_mismatch :
    (5221 : ℕ) + 14425 + 18169 + 17706 = 55521 ∧ (29995 : ℕ) + 30544 = 60539 ∧
      (55521 : ℕ) ≠ 48758 ∧ (60539 : ℕ) ≠ 48758 := by norm_num

/-- pp. 24–25 and Figures 2a, 2c. Slopes of 0.017 and 0.013 per $1,000 give 17
and 13 percentage points per $10,000. The housing control removes
`(0.017 − 0.013)/0.017 = 4/17`, about 23.5 percent, of the slope, more than the
"no more than twenty percent" of the text (reading CH-D8). -/
theorem numbers_fig2 :
    (10 : ℝ) * 0.017 = 0.17 ∧ (10 : ℝ) * 0.013 = 0.13 ∧
      ((0.017 : ℝ) - 0.013) / 0.017 = 4 / 17 ∧ (4 / 17 : ℝ) > 1 / 5 := by norm_num

/-- pp. 31–32 and Table 9. The education gap moves by 0.25, the health gap by
0.08 (14 percent of 0.56), food by 0.04 and housing by 0.06 for Blacks, food by
0.03 and housing by 0.05 for Hispanics. -/
theorem numbers_table9 :
    (5 / 100 : ℝ) - (-20 / 100) = 25 / 100 ∧ (-48 / 100 : ℝ) - (-56 / 100) = 8 / 100 ∧
      |(8 / 100 : ℝ) / (56 / 100) - 0.14| < 0.005 ∧
      (-3 / 100 : ℝ) - (-7 / 100) = 4 / 100 ∧ (8 / 100 : ℝ) - 2 / 100 = 6 / 100 ∧
      (7 / 100 : ℝ) - 4 / 100 = 3 / 100 ∧ (9 / 100 : ℝ) - 4 / 100 = 5 / 100 := by
  norm_num [abs_lt]

/-- p. 32. The quarterly dollar amounts, their annual total of about $1,350,
and its ratio to $2,300, "nearly sixty percent". -/
theorem numbers_diversion :
    |(25 / 100 : ℝ) * 288 - 72| < 1 ∧ |(8 / 100 : ℝ) * 464 - 37| < 1 ∧
      |(4 / 100 : ℝ) * 1733 - 69| < 1 ∧ |(6 / 100 : ℝ) * 2670 - 160| < 1 ∧
      (4 : ℝ) * (72 + 37 + 69 + 160) = 1352 ∧ |(1350 : ℝ) / 2300 - 0.59| < 0.005 := by
  norm_num [abs_lt]

/-- p. 33 and Table 10. Permanent income and demographics explain about 60
percent of the unconditional wealth gap, and reference-group income about 60
percent of the rest. -/
theorem numbers_table10 :
    |((97046 : ℝ) - 40138) / 97046 - 0.59| < 0.005 ∧
      |((40138 : ℝ) - 15862) / 40138 - 0.60| < 0.01 := by
  norm_num [abs_lt]

end CharlesEtAl2007
