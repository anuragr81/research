import Mathlib

/-! Fang and Moro (2010), NBER working paper 15860, the structures Paper A1
relies on or sets against, in the survey's notation.

* Section 2.1, eq. (1). Skill `q ~ N(μ_j, σ_j²)`, signal `θ = q + ε`,
  `ε ~ N(0, σ_εj²)`. `E(q|θ) = σ_j²/(σ_j² + σ_εj²) θ + σ_εj²/(σ_j² + σ_εj²) μ_j`.
  Section 2.2.1 gives `Var(q|θ) = σ_j² σ_εj²/(σ_j² + σ_εj²)`. `s` is `σ_j²` and
  `e` is `σ_εj²`.
* Section 3.2, eqs. (4), (6), (7). The posterior that a worker with signal `θ`
  from a group with investment rate `π` is qualified,
  `p(θ; π) = π f_q(θ)/(π f_q(θ) + (1 − π) f_u(θ))`, and the assignment rule.
  `fq` and `fu` are the two densities at a given `θ`.
* Section 7.1. The pooled variance used when race may not be used. -/

namespace FangMoro2010

/-- Eq. (1), the weight on the signal. -/
noncomputable def wθ (s e : ℝ) : ℝ := s / (s + e)

/-- Eq. (1), the weight on the group mean. -/
noncomputable def wμ (s e : ℝ) : ℝ := e / (s + e)

/-- Section 2.2.1, the conditional variance. -/
noncomputable def condVar (s e : ℝ) : ℝ := s * e / (s + e)

/-- Eq. (1). The two weights sum to one. -/
theorem eq1_weights_sum {s e : ℝ} (h : s + e ≠ 0) : wθ s e + wμ s e = 1 := by
  unfold wθ wμ; field_simp

/-- Section 2.2.1. `Var(q|θ)` is increasing in the noise of the signal when the
group has spread in skill. -/
theorem condVar_strictMono_noise {s e₁ e₂ : ℝ} (hs : 0 < s) (h1 : 0 ≤ e₁) (h12 : e₁ < e₂) :
    condVar s e₁ < condVar s e₂ := by
  unfold condVar
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith [mul_pos (mul_pos hs hs) (sub_pos.2 h12)]

/-- Control: with no spread in skill within the group (`σ_j² = 0`), the
conditional variance is zero whatever the noise, so the noise carries no risk
discount. -/
theorem control_condVar_zero_spread (e : ℝ) : condVar 0 e = 0 := by
  unfold condVar; simp

/-- Eq. (4). The posterior that a worker is qualified. -/
noncomputable def post (π fq fu : ℝ) : ℝ := π * fq / (π * fq + (1 - π) * fu)

/-- Eq. (4). The posterior is increasing in the group's investment rate `π`.
This is the informational externality the survey calls the key source of
multiple equilibria. -/
theorem post_strictMono_prior {π₁ π₂ fq fu : ℝ} (hq : 0 < fq) (hu : 0 < fu)
    (h0 : 0 < π₁) (h12 : π₁ < π₂) (h1 : π₂ < 1) :
    post π₁ fq fu < post π₂ fq fu := by
  unfold post
  have d1 : 0 < π₁ * fq + (1 - π₁) * fu := by nlinarith
  have d2 : 0 < π₂ * fq + (1 - π₂) * fu := by nlinarith
  rw [div_lt_div_iff₀ d1 d2]
  nlinarith [mul_pos (mul_pos hq hu) (sub_pos.2 h12)]

/-- Eqs. (6) and (7). With `0 < π < 1`, positive densities and `x_q > 0`, the
firm assigns the worker to the complex task, `p x_q − (1 − p) x_u ≥ 0`, iff
`f_q/f_u ≥ (1 − π) x_u/(π x_q)`. -/
theorem eq7_iff {π fq fu xq xu : ℝ} (hπ0 : 0 < π) (hπ1 : π < 1) (hq : 0 < fq) (hu : 0 < fu)
    (hxq : 0 < xq) :
    0 ≤ post π fq fu * xq - (1 - post π fq fu) * xu ↔
      (1 - π) * xu / (π * xq) ≤ fq / fu := by
  unfold post
  have d : 0 < π * fq + (1 - π) * fu := by nlinarith
  have hd : π * fq + (1 - π) * fu ≠ 0 := d.ne'
  have e : π * fq / (π * fq + (1 - π) * fu) * xq - (1 - π * fq / (π * fq + (1 - π) * fu)) * xu
      = (π * fq * xq - (1 - π) * fu * xu) / (π * fq + (1 - π) * fu) := by
    field_simp; ring
  rw [e, div_le_div_iff₀ (by positivity) hu, le_div_iff₀ d, zero_mul, sub_nonneg]
  constructor <;> intro h <;> nlinarith

/-- Control: an uninformative signal (`f_q = f_u`) leaves the posterior at the
prior, so the group's investment rate is all the firm has. -/
theorem control_uninformative_signal {π f : ℝ} (hf : 0 < f) : post π f f = π := by
  unfold post
  have : π * f + (1 - π) * f = f := by ring
  rw [this]; field_simp

/-! ### The pooled variance of Section 7.1

A share `λ` of the population is group `B`, with mean `μ_B` and variance
`σ_B²`, and the rest is group `W`. The variance of skill in the pooled
population is `λ σ_B² + (1 − λ) σ_W² + λ (1 − λ)(μ_B − μ_W)²`, the law of total
variance. The survey writes `σ² = λ² σ_B² + (1 − λ)² σ_W²` (FM-D3). -/

/-- The mean of the pooled population. -/
def poolMean (l μB μW : ℝ) : ℝ := l * μB + (1 - l) * μW

/-- The second moment of the pooled population, from the groups' second
moments `σ² + μ²`. -/
def poolSecond (l μB μW sB sW : ℝ) : ℝ := l * (sB + μB ^ 2) + (1 - l) * (sW + μW ^ 2)

/-- The law of total variance for the pooled population. -/
theorem pool_variance (l μB μW sB sW : ℝ) :
    poolSecond l μB μW sB sW - poolMean l μB μW ^ 2
      = l * sB + (1 - l) * sW + l * (1 - l) * (μB - μW) ^ 2 := by
  unfold poolSecond poolMean; ring

/-- Control: the survey's expression is not the pooled variance. With equal
halves, unit variances and equal means, the pooled variance is `1` and the
survey's expression gives `1/2`. -/
theorem control_survey_pooled_variance :
    poolSecond (1 / 2) 0 0 1 1 - poolMean (1 / 2) 0 0 ^ 2
      ≠ (1 / 2 : ℝ) ^ 2 * 1 + (1 - 1 / 2) ^ 2 * 1 := by
  unfold poolSecond poolMean; norm_num

/-! ## Section 2.1, Phelps's signal extraction, and Section 2.2

`condExp s e μ θ` is eq. (1), `E(q|θ)`, for a group with skill variance
`s = σ_j²`, noise variance `e = σ_εj²` and mean `μ = μ_j`. -/

/-- Eq. (1). -/
noncomputable def condExp (s e μ θ : ℝ) : ℝ := wθ s e * θ + wμ s e * μ

/-- Case 1 (PDF p. 8). Equal variances and `μ_B < μ_W`: a `B` worker is paid
less than a `W` worker with the same signal. -/
theorem phelps_case1 {s e μB μW : ℝ} (hs : 0 ≤ s) (he : 0 < e) (hμ : μB < μW) (θ : ℝ) :
    condExp s e μB θ < condExp s e μW θ := by
  unfold condExp wμ
  have : 0 < e / (s + e) := div_pos he (by linarith)
  nlinarith

/-- Case 2 (PDF p. 8). Equal means and variances of skill, a noisier signal for
`B`: `B` workers with high signals are paid less, and with low signals more. -/
theorem phelps_case2 {s eB eW μ : ℝ} (hs : 0 < s) (hW : 0 ≤ eW) (hWB : eW < eB) (θ : ℝ) :
    condExp s eB μ θ < condExp s eW μ θ ↔ μ < θ := by
  have h1 : wθ s eB < wθ s eW := by
    unfold wθ; rw [div_lt_div_iff₀ (by linarith) (by linarith)]; nlinarith
  have hB : wμ s eB = 1 - wθ s eB := by
    have := eq1_weights_sum (s := s) (e := eB) (by linarith); linarith
  have hW' : wμ s eW = 1 - wθ s eW := by
    have := eq1_weights_sum (s := s) (e := eW) (by linarith); linarith
  unfold condExp; rw [hB, hW']
  constructor <;> intro h <;> nlinarith

/-- PDF p. 8. In Case 2 the two groups' average wages are equal, since the
average signal is `μ`. -/
theorem phelps_case2_average {s e μ : ℝ} (h : s + e ≠ 0) : condExp s e μ μ = μ := by
  unfold condExp
  have := eq1_weights_sum h
  calc wθ s e * μ + wμ s e * μ = (wθ s e + wμ s e) * μ := by ring
    _ = μ := by rw [this, one_mul]

/-- PDF p. 8. With a precise signal (`σ_εj² = 0`) the weight on the group mean is
zero and `E(q|θ) = θ`: the group no longer enters the wage. -/
theorem phelps_precise_signal {s : ℝ} (hs : s ≠ 0) (μ θ : ℝ) :
    wμ s 0 = 0 ∧ condExp s 0 μ θ = θ := by
  unfold condExp wμ wθ
  refine ⟨by simp, ?_⟩
  simp [hs]

/-- Control: with a noisy signal and spread in skill the group mean keeps a
positive weight. -/
theorem control_phelps_noisy_signal {s e : ℝ} (hs : 0 ≤ s) (he : 0 < e) : 0 < wμ s e := by
  unfold wμ; exact div_pos he (by linarith)

/-- Control for Case 2: with equally noisy signals there is no crossing. -/
theorem control_phelps_case2_equal_noise (s e μ θ : ℝ) :
    ¬ (condExp s e μ θ < condExp s e μ θ) := lt_irrefl _

/-- Section 2.2.1, the average wage with the risk discount `c Var(q|θ)/2`,
`μ − (c/2) Var(q|θ)`, which falls with the noise. -/
theorem aignerCain_average_wage {s e₁ e₂ c μ : ℝ} (hs : 0 < s) (hc : 0 < c) (h1 : 0 ≤ e₁)
    (h12 : e₁ < e₂) :
    μ - c / 2 * condVar s e₂ < μ - c / 2 * condVar s e₁ := by
  have := condVar_strictMono_noise hs h1 h12
  nlinarith

/-- Section 2.2.1. The survey prints the exponent of the expected utility as
`−c E(q|θ) + (c/2) Var(q|θ)`; Aigner and Cain's eq. (8) has `(c²/2)`. The two
rank risky prospects differently (FM-D11): at `c = 4` the printed exponent
prefers `(E, V) = (2, 1)` to `(1, 0)` and eq. (8) the reverse. -/
theorem control_survey_exponent :
    (-4 * (2 : ℝ) + 4 / 2 * 1 < -4 * 1 + 4 / 2 * 0) ∧
      ((-4 * (1 : ℝ) + 4 ^ 2 / 2 * 0) < -4 * 2 + 4 ^ 2 / 2 * 1) := by norm_num

/-! ### Section 2.2.2, Lundberg and Startz (1983) -/

/-- The objective of a group-`j` worker, `w (μ + b X) + (1 − w) μ − c X²/2`,
with `w = σ²/(σ² + σ_εj²)`, the expected wage of the display on PDF p. 10 net
of the cost `C(X) = c X²/2`. -/
noncomputable def lsObjective (w μ b c X : ℝ) : ℝ := w * (μ + b * X) + (1 - w) * μ - c * X ^ 2 / 2

/-- Eq. (2). `X* = (b/c) σ²/(σ² + σ_εj²)` maximises the objective. -/
theorem ls_eq2 {w μ b c : ℝ} (hc : 0 < c) (X : ℝ) :
    lsObjective w μ b c X ≤ lsObjective w μ b c (b / c * w) := by
  unfold lsObjective
  have e : w * (μ + b * (b / c * w)) + (1 - w) * μ - c * (b / c * w) ^ 2 / 2
      - (w * (μ + b * X) + (1 - w) * μ - c * X ^ 2 / 2) = c / 2 * (X - b / c * w) ^ 2 := by
    field_simp; ring
  nlinarith [mul_nonneg hc.le (sq_nonneg (X - b / c * w))]

/-- PDF p. 10. The group with the noisier signal invests less, and since the
average wage is `μ + b X*`, it earns less on average. -/
theorem ls_noisier_invests_less {s eB eW b c μ : ℝ} (hs : 0 < s) (hb : 0 < b) (hc : 0 < c)
    (hW : 0 ≤ eW) (hWB : eW < eB) :
    b / c * wθ s eB < b / c * wθ s eW ∧ μ + b * (b / c * wθ s eB) < μ + b * (b / c * wθ s eW) := by
  have h1 : wθ s eB < wθ s eW := by
    unfold wθ; rw [div_lt_div_iff₀ (by linarith) (by linarith)]; nlinarith
  have h2 : 0 < b / c := div_pos hb hc
  constructor
  · exact mul_lt_mul_of_pos_left h1 h2
  · nlinarith [mul_lt_mul_of_pos_left h1 h2]

/-- Control for eq. (2): with a signal of no information (`w = 0`) the worker
does not invest. -/
theorem control_ls_uninformative (b c : ℝ) : b / c * 0 = 0 := mul_zero _

/-! ### Section 2.2.3, Cornell and Welch (1996) -/

/-- PDF p. 10. The variance of expected productivity is `w² Var(θ)`,
`σ⁴/(σ² + σ_ε²)`, higher for the group with the more informative signal. -/
theorem cw_variance {s e₁ e₂ : ℝ} (hs : 0 < s) (h1 : 0 ≤ e₁) (h12 : e₁ < e₂) :
    wθ s e₂ ^ 2 * (s + e₂) < wθ s e₁ ^ 2 * (s + e₁) := by
  have k : ∀ e, 0 ≤ e → wθ s e ^ 2 * (s + e) = s ^ 2 / (s + e) := by
    intro e he; unfold wθ; field_simp
  rw [k e₂ (by linarith), k e₁ h1, div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith [mul_pos hs hs]

/-! ### Section 3.1, Arrow (1973) -/

/-- PDF p. 8. The two zero-profit conditions `r = π_j [f₁ − w_j]` give
`w_W = (π_B/π_W) w_B + (1 − π_B/π_W) f₁`. -/
theorem arrow_wage_relation {r πB πW f1 wB wW : ℝ} (hW : πW ≠ 0)
    (hzW : r = πW * (f1 - wW)) (hzB : r = πB * (f1 - wB)) :
    wW = πB / πW * wB + (1 - πB / πW) * f1 := by
  field_simp; linarith

/-- PDF p. 8. If `π_B < π_W` then `w_B < w_W`, given a positive testing cost. -/
theorem arrow_lower_wage {r πB πW f1 wB wW : ℝ} (hr : 0 < r) (hB : 0 < πB) (hBW : πB < πW)
    (hzW : r = πW * (f1 - wW)) (hzB : r = πB * (f1 - wB)) : wB < wW := by
  have h1 : 0 < f1 - wB := by
    by_contra h; push_neg at h; nlinarith
  have h2 := mul_lt_mul_of_pos_right hBW h1
  nlinarith

/-- Eq. (3), Arrow's equilibrium. -/
def ArrowEquilibrium (G : ℝ → ℝ) (wu : ℝ) (w π : Bool → ℝ) : Prop :=
  ∀ j, π j = G (w j - wu)

/-- Control for Arrow's result: with equal shares skilled the wages are equal. -/
theorem control_arrow_equal_shares {r π f1 wB wW : ℝ} (hπ : π ≠ 0)
    (hzW : r = π * (f1 - wW)) (hzB : r = π * (f1 - wB)) : wB = wW := by
  have : π * (wW - wB) = 0 := by linarith
  rcases mul_eq_zero.mp this with h | h
  · exact absurd h hπ
  · linarith

/-! ### Section 3.2, Coate and Loury (1993a) -/

/-- Eq. (4) with `l = f_q/f_u`, `p = π l/(π l + 1 − π)`. -/
noncomputable def postL (π l : ℝ) : ℝ := π * l / (π * l + (1 - π))

theorem postL_eq_post {π fq fu : ℝ} (hu : 0 < fu) : postL π (fq / fu) = post π fq fu := by
  unfold postL post
  have := hu.ne'
  field_simp

/-- PDF p. 11. Under MLRP the posterior rises with the signal. -/
theorem cl_posterior_mono_signal {π l₁ l₂ : ℝ} (h0 : 0 < π) (h1 : π < 1) (hl : 0 < l₁)
    (h12 : l₁ < l₂) : postL π l₁ < postL π l₂ := by
  unfold postL
  rw [div_lt_div_iff₀ (by nlinarith) (by nlinarith)]
  nlinarith [mul_pos (mul_pos h0 h0) (sub_pos.2 h12), mul_pos h0 (sub_pos.2 h1)]

/-- Eq. (5), the firm's expected profit from the complex task. -/
noncomputable def clProfit (p xq xu : ℝ) : ℝ := p * xq - (1 - p) * xu

/-- Eq. (8). Under MLRP the threshold is unique. -/
theorem cl_eq8_unique {l : ℝ → ℝ} (hl : StrictMono l) {k θ₁ θ₂ : ℝ} (h1 : l θ₁ = k)
    (h2 : l θ₂ = k) : θ₁ = θ₂ := hl.injective (h1.trans h2.symm)

/-- The right side of eqs. (7) and (8), `(1 − π) x_u/(π x_q)`. -/
noncomputable def clRatio (xq xu π : ℝ) : ℝ := (1 - π) * xu / (π * xq)

/-- Eq. (9) in sign. A higher prior lowers the threshold. -/
theorem cl_eq9 {l : ℝ → ℝ} (hl : StrictMono l) {xq xu π₁ π₂ θ₁ θ₂ : ℝ} (hxq : 0 < xq)
    (hxu : 0 < xu) (h0 : 0 < π₁) (h12 : π₁ < π₂) (h1 : l θ₁ = clRatio xq xu π₁)
    (h2 : l θ₂ = clRatio xq xu π₂) : θ₂ < θ₁ := by
  have hr : clRatio xq xu π₂ < clRatio xq xu π₁ := by
    unfold clRatio
    rw [div_lt_div_iff₀ (by nlinarith) (by nlinarith)]
    nlinarith [mul_pos (mul_pos hxu hxq) (sub_pos.2 h12)]
  rw [← h1, ← h2] at hr
  exact hl.lt_iff_lt.mp hr

/-- Eqs. (10) to (12). A worker with cost `c` invests iff `c ≤ I(θ̃) = [F_u(θ̃) − F_q(θ̃)] ω`. -/
theorem cl_eq12 (Fq Fu ω c : ℝ) :
    (1 - Fu) * ω ≤ (1 - Fq) * ω - c ↔ c ≤ (Fu - Fq) * ω := by
  constructor <;> intro h <;> linarith

/-- The benefit function `I(θ̃)`. -/
noncomputable def clI (Fq Fu : ℝ → ℝ) (ω θ : ℝ) : ℝ := (Fu θ - Fq θ) * ω

/-- PDF p. 13. `I(0) = I(1) = 0`. -/
theorem cl_I_endpoints {Fq Fu : ℝ → ℝ} (ω : ℝ) (hq0 : Fq 0 = 0) (hu0 : Fu 0 = 0)
    (hq1 : Fq 1 = 1) (hu1 : Fu 1 = 1) : clI Fq Fu ω 0 = 0 ∧ clI Fq Fu ω 1 = 0 := by
  unfold clI; simp [hq0, hu0, hq1, hu1]

/-- Eq. (13). `I′(θ̃) = ω [f_u(θ̃) − f_q(θ̃)]`, positive iff `l(θ̃) < 1`. -/
theorem cl_eq13 {Fq Fu : ℝ → ℝ} {fq fu ω θ : ℝ} (hq : HasDerivAt Fq fq θ) (hu : HasDerivAt Fu fu θ)
    (hω : 0 < ω) (hfu : 0 < fu) :
    HasDerivAt (clI Fq Fu ω) (ω * (fu - fq)) θ ∧ (0 < ω * (fu - fq) ↔ fq / fu < 1) := by
  refine ⟨?_, ?_⟩
  · have := (hu.sub hq).mul_const ω
    unfold clI
    convert this using 1; ring
  · rw [div_lt_one hfu]
    constructor
    · intro h; by_contra h'; push_neg at h'; nlinarith
    · intro h; nlinarith

/-- Eqs. (15) and (16), an equilibrium for one group: a threshold and an
investment rate that are mutual best responses. `θt π` is the threshold of eq.
(8) for prior `π`. -/
def CLEquilibrium (θt : ℝ → ℝ) (G : ℝ → ℝ) (Fq Fu : ℝ → ℝ) (ω θ π : ℝ) : Prop :=
  θ = θt π ∧ π = G (clI Fq Fu ω θ)

/-- PDF p. 14. With `G(0) = 0` there is always the trivial equilibrium `π = 0`,
`θ̃ = 1`. -/
theorem cl_trivial_equilibrium {θt G Fq Fu : ℝ → ℝ} (ω : ℝ) (hθ : θt 0 = 1) (hG : G 0 = 0)
    (hq1 : Fq 1 = 1) (hu1 : Fu 1 = 1) : CLEquilibrium θt G Fq Fu ω 1 0 := by
  refine ⟨hθ.symm, ?_⟩
  unfold clI; simp [hq1, hu1, hG]

/-- Control for the trivial equilibrium: if some workers have zero cost,
`G(0) > 0`, then `π = 0` is not an equilibrium. -/
theorem control_cl_trivial {θt G Fq Fu : ℝ → ℝ} (ω : ℝ) (hG : 0 < G 0)
    (hq1 : Fq 1 = 1) (hu1 : Fu 1 = 1) : ¬ CLEquilibrium θt G Fq Fu ω 1 0 := by
  rintro ⟨_, h⟩
  unfold clI at h; simp [hq1, hu1] at h; linarith

/-! ## Section 4.1, Moro and Norman (2004) -/

/-- PDF p. 16, the inputs. `S = Σ_j λ_j [π_j F_q(θ̃_j) + (1 − π_j) F_u(θ̃_j)]`. -/
noncomputable def mnS (lam π Fq Fu : Bool → ℝ) : ℝ :=
  ∑ j : Bool, lam j * (π j * Fq j + (1 - π j) * Fu j)

/-- PDF p. 16, `C = Σ_j λ_j π_j (1 − F_q(θ̃_j))`. Here `Fq j` and `Fu j` are the
distribution functions at the group's threshold. -/
noncomputable def mnC (lam π Fq : Bool → ℝ) : ℝ := ∑ j : Bool, lam j * π j * (1 - Fq j)

/-- Eq. (18). The first-order condition holds iff the posterior at the
threshold times `x_q` equals `x_u`: the threshold equates the marginal products
of the two tasks. -/
theorem mn_eq18 {π fq fu xq xu : ℝ} (hd : 0 < π * fq + (1 - π) * fu) :
    (π * fq + (1 - π) * fu) * xu = π * fq * xq ↔ post π fq fu * xq = xu := by
  unfold post
  rw [div_mul_eq_mul_div, div_eq_iff hd.ne']
  constructor <;> intro h <;> linarith

/-- Eq. (19), the wage schedule. `p` is the posterior at the signal. -/
noncomputable def mnWage (xq xu θt θ p : ℝ) : ℝ := if θ < θt then xu else p * xq

/-- PDF p. 17. By eq. (18) the wage is continuous at the threshold. -/
theorem mn_wage_at_threshold {xq xu θt p : ℝ} (h18 : p * xq = xu) :
    mnWage xq xu θt θt p = xu := by
  unfold mnWage; simp [h18]

/-- Eq. (20). Under MLRP, if the best signal of group `B` is worth less in the
complex task than in the simple task, every signal is. -/
theorem mn_eq20 {π : ℝ} {l : ℝ → ℝ} (hl : Monotone l) (h0 : 0 < π) (h1 : π < 1)
    (hpos : ∀ θ, 0 < l θ) {xq xu : ℝ} (hxq : 0 < xq) (h20 : postL π (l 1) * xq < xu)
    {θ : ℝ} (hθ : θ ≤ 1) : postL π (l θ) * xq < xu := by
  rcases (hl hθ).eq_or_lt with h | h
  · rw [h]; exact h20
  · have := cl_posterior_mono_signal h0 h1 (hpos θ) h
    nlinarith [mul_lt_mul_of_pos_right this hxq]

/-! ## Section 4.2, Mailath, Samuelson and Shaked (2000)

The symmetric steady state, eqs. (21) to (25). The survey states the worker's
value equations, the surplus equation and Nash bargaining, and obtains (24)
and (25). The vacant firm's value equation,
`[ρ_I H_I (γ_F + γ_I) + r + δ] V_F = ρ_I H_I (γ_F + γ_I) Z_F`, is not printed
and is supplied here as the counterpart of the worker's (FM-D12). -/

/-- Eqs. (22) and (23), the steady-state conditions. -/
def mssSteady (δ ρF ρI HI γI γF : ℝ) : Prop :=
  2 * δ * (1 - ρF) = ρF * ρI * HI * (γI + γF) ∧ 2 * δ * (1 - ρI) = ρF * ρI * (γI + γF)

/-- Eqs. (24) and (25), from the value equations on PDF pp. 21 and 22 and the
firm's value equation. -/
theorem mss_eq24_25 {r δ x ρF ρI HI γF γI VI VF ZI ZF S : ℝ} (hr : 0 < r) (hδ : 0 ≤ δ)
    (hρF : 0 < ρF) (hρI : 0 ≤ ρI) (hH : 0 ≤ HI) (hγ : 0 < γF + γI)
    (E1 : (ρF * (γF + γI) + r + δ) * VI = ρF * (γF + γI) * ZI)
    (E2 : (ρI * HI * (γF + γI) + r + δ) * VF = ρI * HI * (γF + γI) * ZF)
    (E3 : (r + 2 * δ) * S = x + δ * (VI + VF))
    (E4 : ZI = VI + (S - VF - VI) / 2) (E5 : ZF = VF + (S - VF - VI) / 2) :
    VI = ρF * (γF + γI) * x /
        ((r + δ) * ((ρF + ρI * HI) * (γF + γI) + 2 * (r + 2 * δ))) ∧
      VF = ρI * HI * (γF + γI) * x /
        ((r + δ) * ((ρF + ρI * HI) * (γF + γI) + 2 * (r + 2 * δ))) := by
  set a := ρF * (γF + γI) with ha
  set b := ρI * HI * (γF + γI) with hb
  have ha0 : 0 < a := mul_pos hρF hγ
  have hb0 : 0 ≤ b := mul_nonneg (mul_nonneg hρI hH) hγ.le
  have h1 : (a + 2 * (r + δ)) * VI + a * VF = a * S := by linear_combination 2 * E1 + 2 * a * E4
  have h2 : (b + 2 * (r + δ)) * VF + b * VI = b * S := by linear_combination 2 * E2 + 2 * b * E5
  have h3 : b * VI = a * VF := by
    have : 2 * (r + δ) * (b * VI - a * VF) = 0 := by linear_combination b * h1 - a * h2
    have hR : 2 * (r + δ) ≠ 0 := by positivity
    have := (mul_eq_zero.mp this).resolve_left hR
    linarith
  have hden : 0 < (r + δ) * ((ρF + ρI * HI) * (γF + γI) + 2 * (r + 2 * δ)) := by positivity
  have key : a * (VI * ((r + δ) * (a + b + 2 * (r + 2 * δ))) - a * x) = 0 := by
    linear_combination a * a * E3 + a * (r + 2 * δ) * h1 + a * (r + δ) * h3
  have e : (ρF + ρI * HI) * (γF + γI) = a + b := by rw [ha, hb]; ring
  have hVI : VI * ((r + δ) * (a + b + 2 * (r + 2 * δ))) = a * x := by
    have := (mul_eq_zero.mp key).resolve_left ha0.ne'; linarith
  rw [e] at hden ⊢
  constructor
  · rw [eq_div_iff hden.ne']; linarith
  · rw [eq_div_iff hden.ne']
    have : a * (VF * ((r + δ) * (a + b + 2 * (r + 2 * δ))) - b * x) = 0 := by
      linear_combination (-((r + δ) * (a + b + 2 * (r + 2 * δ)))) * h3 + b * hVI
    have := (mul_eq_zero.mp this).resolve_left ha0.ne'
    linarith

/-- Eqs. (26) to (30), the balance conditions of a white asymmetric steady state
in which firms search only white workers. -/
def mssAsymSteady (G : ℝ → ℝ) (δ ρF ρW ρB HW HB VW VB γI γF : ℝ) : Prop :=
  HW = G VW / 2 ∧ HB = G VB / 2 ∧
  2 * δ * (1 - ρF) = 2 * ρF * γF * HW * ρW + (ρW * HW + ρB * HB) * γI * ρF ∧
  2 * δ * (1 - ρW) = ρW * ρF * (γI + 2 * γF) ∧ 2 * δ * (1 - ρB) = ρB * ρF * γI

/-- PDF p. 24. With `G` increasing, `V_B < V_W` gives `H_B < H_W` through eqs. (26)
and (27). -/
theorem mss_HB_lt_HW {G : ℝ → ℝ} (hG : StrictMono G) {δ ρF ρW ρB HW HB VW VB γI γF : ℝ}
    (h : mssAsymSteady G δ ρF ρW ρB HW HB VW VB γI γF) (hV : VB < VW) : HB < HW := by
  obtain ⟨h1, h2, -⟩ := h
  rw [h1, h2]; linarith [hG hV]

/-- PDF p. 24, from eqs. (32) and (33). In a white asymmetric steady state black
workers value skills less, `V_B < V_W`, whenever firms search (`γ_F > 0`). -/
theorem mss_VB_lt_VW {r δ x ρF γF γI Δ : ℝ} (hx : 0 < x) (hρ : 0 < ρF) (hγF : 0 < γF)
    (hT : 0 < r + 2 * δ) (hD : 0 < (r + δ) * Δ) :
    ρF * γI * (2 * (r + 2 * δ) + (2 * γF + γI) * ρF) * x / ((r + δ) * Δ)
      < ρF * (2 * γF + γI) * (2 * (r + 2 * δ) + ρF * γI) * x / ((r + δ) * Δ) := by
  apply div_lt_div_of_pos_right _ hD
  have : ρF * γI * (2 * (r + 2 * δ) + (2 * γF + γI) * ρF)
      < ρF * (2 * γF + γI) * (2 * (r + 2 * δ) + ρF * γI) := by
    nlinarith [mul_pos (mul_pos hρ hγF) hT]
  nlinarith

/-- Control: if firms do not search (`γ_F = 0`) the two values are equal. -/
theorem control_mss_no_firm_search (r δ x ρF γI Δ : ℝ) :
    ρF * γI * (2 * (r + 2 * δ) + (2 * 0 + γI) * ρF) * x / ((r + δ) * Δ)
      = ρF * (2 * 0 + γI) * (2 * (r + 2 * δ) + ρF * γI) * x / ((r + δ) * Δ) := by ring_nf

/-! ## Section 4.3, Fang (2001)

A worker of type `(c, V)` chooses activity `A` or not and to invest or not.
`B`-workers are paid `1`; `A`-workers expect `W_q` if skilled and `W_u` if not. -/

/-- Eq. (37), the wage `w(π, θ) = max {1, p x_q}`. -/
noncomputable def fangWage (p xq : ℝ) : ℝ := max 1 (p * xq)

/-- The four payoffs: `A` and invest, `A` and not, `B` and not, `B` and invest. -/
def fangPayoff (Wq Wu c V : ℝ) : Fin 4 → ℝ := ![Wq - c + V, Wu + V, 1, 1 - c]

/-- PDF p. 28, the low-cost type. If `W_q − W_u ≥ c_l` and `c_l ≥ 0`, investing
and taking up `A` is a best choice iff `V ≥ Ṽ_q = 1 + c_l − W_q`. -/
theorem fang_low_cost {Wq Wu cl V : ℝ} (hgap : cl ≤ Wq - Wu) (hc : 0 ≤ cl) :
    (∀ k, fangPayoff Wq Wu cl V k ≤ fangPayoff Wq Wu cl V 0) ↔ 1 + cl - Wq ≤ V := by
  constructor
  · intro h; have := h 2; simp [fangPayoff] at this; linarith
  · intro h k; fin_cases k <;> simp [fangPayoff] <;> linarith

/-- PDF p. 28, the high-cost type. If `W_q − W_u ≤ c_h`, taking up `A` without
investing is a best choice iff `V ≥ Ṽ_u = 1 − W_u`. -/
theorem fang_high_cost {Wq Wu ch V : ℝ} (hgap : Wq - Wu ≤ ch) (hc : 0 ≤ ch) :
    (∀ k, fangPayoff Wq Wu ch V k ≤ fangPayoff Wq Wu ch V 1) ↔ 1 - Wu ≤ V := by
  constructor
  · intro h; have := h 2; simp [fangPayoff] at this; linarith
  · intro h k; fin_cases k <;> simp [fangPayoff] <;> linarith

/-- Eq. (38) and the single-crossing property, PDF p. 29. With `W_q − W_u ≥ c_l`,
`Ṽ_q ≤ Ṽ_u`, so `B(e_q, V) ≥ B(e_u, V)` for every `V`; and `Ṽ_u < 0` iff `W_u > 1`. -/
theorem fang_eq38 {Wq Wu cl : ℝ} (hgap : cl ≤ Wq - Wu) :
    1 + cl - Wq ≤ 1 - Wu ∧ (∀ V, V - (1 - Wu) ≤ V - (1 + cl - Wq)) ∧
      (1 - Wu < 0 ↔ 1 < Wu) := by
  refine ⟨by linarith, fun V => by linarith, by constructor <;> intro h <;> linarith⟩

/-- Control for eq. (38): if `W_q − W_u < c_l`, single crossing fails. -/
theorem control_fang_eq38 {Wq Wu cl : ℝ} (hgap : Wq - Wu < cl) : 1 - Wu < 1 + cl - Wq := by
  linarith

/-- PDF p. 30. A fixed point of `Ψ` requires `Δ = max (Ψ(π_A) − π_A) ≥ 0`; the
necessary half of the survey's condition. -/
theorem fang_fixed_point_necessary {Ψ : ℝ → ℝ} {πA : ℝ} (h : Ψ πA = πA) {Δ : ℝ}
    (hΔ : Ψ πA - πA ≤ Δ) : 0 ≤ Δ := by linarith

/-- PDF p. 30, welfare. With `W_u ≥ 1`, every worker's best payoff in a
cultural equilibrium is at least the non-cultural payoff `max(1 + V, 1)`. -/
theorem fang_pareto {Wq Wu c V : ℝ} (hWu : 1 ≤ Wu) :
    max (1 + V) 1 ≤ max (max (Wq - c + V) (Wu + V)) 1 := by
  apply max_le
  · exact le_trans (by linarith : 1 + V ≤ Wu + V) (le_trans (le_max_right _ _) (le_max_left _ _))
  · exact le_max_right _ _

/-! ## Section 4.4, Chaudhuri and Sethi (2008) -/

/-- PDF p. 30, the peer-group skill level `s_j = η π_j + (1 − η) π̄`. -/
def csPeer (η πj πbar : ℝ) : ℝ := η * πj + (1 - η) * πbar

/-- The gap in peer quality between the groups is `η (π_B − π_W)`, so it shrinks
with integration and vanishes at `η = 0`. -/
theorem cs_peer_gap (η πB πW πbar : ℝ) :
    csPeer η πB πbar - csPeer η πW πbar = η * (πB - πW) ∧ csPeer 0 πB πbar = csPeer 0 πW πbar := by
  unfold csPeer; constructor <;> ring

/-! ## Section 6.2, affirmative action in Coate and Loury (1993a) -/

/-- PDF p. 34, `ρ(θ̃, π)`, the probability a random worker of the group is
assigned to the complex task. -/
noncomputable def clRho (Fq Fu : ℝ → ℝ) (θ π : ℝ) : ℝ := π * (1 - Fq θ) + (1 - π) * (1 - Fu θ)

/-- Eq. (41) and the sufficient condition, PDF pp. 35–36. If `ρ̂` is strictly
decreasing, the quota `ρ̂(θ̃_W) = ρ̂(θ̃_B)` forces equal thresholds, and then
equal beliefs `G(I(θ̃))`. -/
theorem cl_aa_sufficient {ρhat GI : ℝ → ℝ} (h : StrictAnti ρhat) {θW θB : ℝ}
    (hq : ρhat θW = ρhat θB) : θW = θB ∧ GI θW = GI θB := by
  have := h.injective hq; exact ⟨this, by rw [this]⟩

/-- Eqs. (44) and (45). -/
noncomputable def pq (θq θu : ℝ) : ℝ := (θu - θq) / (1 - θq)
noncomputable def pu (θq θu : ℝ) : ℝ := (θu - θq) / θu

/-- Eq. (47). With an unclear score the firm assigns to the complex task iff
`π ≥ π̂ = (p_u/p_q)/(x_q/x_u + p_u/p_q)`. -/
theorem patron_eq47 {π p_q p_u xq xu : ℝ} (h0 : 0 < π) (h1 : π < 1) (hq : 0 < p_q)
    (hu : 0 < p_u) (hxq : 0 < xq) (hxu : 0 < xu) :
    0 ≤ post π p_q p_u * xq - (1 - post π p_q p_u) * xu ↔
      (p_u / p_q) / (xq / xu + p_u / p_q) ≤ π := by
  have d : 0 < π * p_q + (1 - π) * p_u := by nlinarith
  have e1 : post π p_q p_u * xq - (1 - post π p_q p_u) * xu
      = (π * p_q * xq - (1 - π) * p_u * xu) / (π * p_q + (1 - π) * p_u) := by
    unfold post; field_simp; ring
  have e2 : (p_u / p_q) / (xq / xu + p_u / p_q) = p_u * xu / (p_q * xq + p_u * xu) := by
    field_simp
  rw [e1, e2, div_le_iff₀ (by positivity), le_div_iff₀ d, zero_mul]
  constructor <;> intro h <;> nlinarith

/-- Eqs. (48) and (49). Under the liberal policy a skilled worker is assigned
for sure and an unskilled one with probability `p_u`; under the conservative
policy a skilled worker is assigned only with a pass, probability
`(1 − θ_u)/(1 − θ_q) = 1 − p_q`, and an unskilled one never. -/
theorem patron_eq48_49 {θq θu ω : ℝ} (hq : θq < 1) :
    ω * 1 - ω * pu θq θu = ω * (1 - pu θq θu) ∧
      (1 - θu) / (1 - θq) = 1 - pq θq θu := by
  refine ⟨by ring, ?_⟩
  unfold pq
  have : (1 - θq) ≠ 0 := by linarith
  field_simp; ring

/-- Eq. (50) and footnote 17. The patronising probability
`α(π_B) = (π_l − π_B)/(1 − π_B)` meets the quota, and the black worker's return
to investment is `(1 − α) π_l`. -/
theorem patron_eq50 {πl πB p_u ω : ℝ} (hB : πB ≠ 1) (hl : πl = ω * (1 - p_u)) :
    πl + (1 - πl) * p_u = πB + (1 - πB) * (p_u + (1 - p_u) * ((πl - πB) / (1 - πB))) ∧
      ω * (1 - (p_u + (1 - p_u) * ((πl - πB) / (1 - πB)))) = (1 - (πl - πB) / (1 - πB)) * πl := by
  have h : (1 - πB) ≠ 0 := sub_ne_zero.mpr (Ne.symm hB)
  constructor
  · field_simp; ring
  · rw [hl]; field_simp; ring

/-- Eq. (51). `π_B = [1 − α(π_B)] π_l = (1 − π_l) π_l/(1 − π_B)` has exactly the
solutions `π_B = π_l` and `π_B = 1 − π_l`. -/
theorem patron_eq51 {πl πB : ℝ} (hB : πB ≠ 1) :
    (1 - (πl - πB) / (1 - πB)) * πl = (1 - πl) * πl / (1 - πB) ∧
      (πB = (1 - πl) * πl / (1 - πB) ↔ πB = πl ∨ πB = 1 - πl) := by
  have h : (1 - πB) ≠ 0 := sub_ne_zero.mpr (Ne.symm hB)
  constructor
  · field_simp; ring
  · rw [eq_div_iff h]
    constructor
    · intro e
      have : (πB - πl) * (1 - πB - πl) = 0 := by linear_combination e
      rcases mul_eq_zero.mp this with h1 | h1
      · left; linarith
      · right; linarith
    · rintro (e | e) <;> subst e <;> ring

/-- PDF p. 40, the dynamics `π_B^{t+1} = (1 − π_l) π_l/(1 − π_B^t)`. -/
noncomputable def patronMap (πl π : ℝ) : ℝ := (1 - πl) * πl / (1 - π)

/-- The patronising fixed point `1 − π_l` has slope `(1 − π_l)/π_l`, below one iff
`π_l > 1/2`, so it is locally stable exactly when it lies below `π_l`. -/
theorem patron_dynamics_slope {πl : ℝ} (h0 : 0 < πl) :
    patronMap πl (1 - πl) = 1 - πl ∧
      HasDerivAt (patronMap πl) ((1 - πl) / πl) (1 - πl) ∧
      ((1 - πl) / πl < 1 ↔ 1 / 2 < πl) := by
  have hπ : πl ≠ 0 := h0.ne'
  refine ⟨?_, ?_, ?_⟩
  · unfold patronMap; rw [show (1 : ℝ) - (1 - πl) = πl by ring]; field_simp
  · have hd : HasDerivAt (fun π : ℝ => 1 - π) (-1) (1 - πl) := by
      simpa using (hasDerivAt_id (1 - πl)).const_sub 1
    have hne : (1 : ℝ) - (1 - πl) ≠ 0 := by linarith
    have := (hasDerivAt_const (1 - πl) ((1 - πl) * πl)).div hd hne
    unfold patronMap
    convert this using 1
    field_simp; ring
  · rw [div_lt_one h0]; constructor <;> intro h <;> linarith

/-- Control for eq. (51): at `π_l = 1/2` the two solutions coincide, so the
patronising equilibrium is the colour-blind one. -/
theorem control_patron_half : (1 : ℝ) - 1 / 2 = 1 / 2 := by norm_num

/-! ## Section 6.3, Moro and Norman (2003a) -/

/-- PDF p. 41. Under the quota `π_B F_q(θ̂_B) + (1 − π_B) F_u(θ̂_B) =
π_W F_q(θ̂_W) + (1 − π_W) F_u(θ̂_W)`, with `F_q < F_u` (qualified workers draw
higher signals) and the share in the simple task strictly increasing in the
threshold, `π_B < π_W` gives `θ̂_B < θ̂_W`: the discriminated group gets the
lower threshold. The survey prints `θ̂_B > θ̂_W` (FM-D13). -/
theorem mnaa_threshold_order {Fq Fu : ℝ → ℝ} {πB πW θB θW : ℝ}
    (hFOSD : ∀ θ, Fq θ < Fu θ) (hmono : StrictMono (fun θ => πB * Fq θ + (1 - πB) * Fu θ))
    (hBW : πB < πW)
    (hq : πB * Fq θB + (1 - πB) * Fu θB = πW * Fq θW + (1 - πW) * Fu θW) : θB < θW := by
  have h1 : πW * Fq θW + (1 - πW) * Fu θW < πB * Fq θW + (1 - πB) * Fu θW := by
    nlinarith [hFOSD θW]
  rw [← hq] at h1
  exact hmono.lt_iff_lt.mp h1

/-- Control: with an uninformative signal (`F_q = F_u`) the quota does not
separate the thresholds. -/
theorem control_mnaa_uninformative {F : ℝ → ℝ} {πB πW θ : ℝ} :
    πB * F θ + (1 - πB) * F θ = πW * F θ + (1 - πW) * F θ := by ring

/-! ## Section 6.4, Fang and Norman (2006) -/

/-- PDF p. 45, the posteriors after `h` and `l`, as `post` with signal
probabilities `p` and `1 − p`. -/
noncomputable def wh (β p π : ℝ) : ℝ := β * post π p (1 - p)
noncomputable def wl (β p π : ℝ) : ℝ := β * post π (1 - p) p

/-- PDF p. 45, `π̂_h` and `π̂_l` solve `w_θ(π̂_θ) = g`. -/
theorem fn_pihat {β p g : ℝ} (hp0 : 0 < p) (hp1 : p < 1) (hg : 0 < g) (hgβ : g < β) :
    wh β p (g * (1 - p) / (g * (1 - p) + p * (β - g))) = g ∧
      wl β p (g * p / (g * p + (1 - p) * (β - g))) = g := by
  have h1 : 0 < 1 - p := by linarith
  have h2 : 0 < β - g := by linarith
  have hβ : β ≠ 0 := by linarith
  have hp : p ≠ 0 := hp0.ne'
  have h1' : 1 - p ≠ 0 := h1.ne'
  have h3 : β - β * p ≠ 0 := by
    have : β - β * p = β * (1 - p) := by ring
    rw [this]; exact mul_ne_zero hβ h1'
  constructor
  · unfold wh post
    have hD : 0 < g * (1 - p) + p * (β - g) := by positivity
    have e : g * (1 - p) / (g * (1 - p) + p * (β - g)) * p
        + (1 - g * (1 - p) / (g * (1 - p) + p * (β - g))) * (1 - p)
        = p * (1 - p) * β / (g * (1 - p) + p * (β - g)) := by
      field_simp; ring
    rw [e]
    field_simp
  · unfold wl post
    have hD : 0 < g * p + (1 - p) * (β - g) := by positivity
    have e : g * p / (g * p + (1 - p) * (β - g)) * (1 - p)
        + (1 - g * p / (g * p + (1 - p) * (β - g))) * p
        = p * (1 - p) * β / (g * p + (1 - p) * (β - g)) := by
      field_simp; ring
    rw [e]
    field_simp

/-- The payoff after signal `θ` with wage `w`, if the worker applies to the
public sector (accepted with probability `ρ`) or not. -/
noncomputable def fnPay (apply : Bool) (ρ g w : ℝ) : ℝ := if apply then ρ * g + (1 - ρ) * w else w

/-- Eq. (54). The incentive to invest is `(2p − 1)` times the difference of the
payoffs after `h` and after `l`, and in each of the three regions it takes the
printed form. -/
theorem fn_eq54 (p ρ g whv wlv : ℝ) (ah al : Bool) :
    (p * fnPay ah ρ g whv + (1 - p) * fnPay al ρ g wlv)
        - ((1 - p) * fnPay ah ρ g whv + p * fnPay al ρ g wlv)
      = (2 * p - 1) * (fnPay ah ρ g whv - fnPay al ρ g wlv) ∧
    (2 * p - 1) * (fnPay true ρ g whv - fnPay true ρ g wlv) = (2 * p - 1) * (1 - ρ) * (whv - wlv) ∧
    (2 * p - 1) * (fnPay false ρ g whv - fnPay true ρ g wlv)
      = (2 * p - 1) * ((1 - ρ) * (whv - wlv) + ρ * (whv - g)) ∧
    (2 * p - 1) * (fnPay false ρ g whv - fnPay false ρ g wlv) = (2 * p - 1) * (whv - wlv) := by
  unfold fnPay
  refine ⟨by ring, ?_, ?_, ?_⟩ <;> simp only [Bool.false_eq_true, ↓reduceIte] <;> ring

/-- Eq. (55). The derivatives in `ρ` of the three cases of eq. (54). -/
theorem fn_eq55 (p g whv wlv ρ : ℝ) :
    HasDerivAt (fun ρ => (2 * p - 1) * (1 - ρ) * (whv - wlv)) (-(2 * p - 1) * (whv - wlv)) ρ ∧
    HasDerivAt (fun ρ => (2 * p - 1) * ((1 - ρ) * (whv - wlv) + ρ * (whv - g)))
      ((2 * p - 1) * (wlv - g)) ρ ∧
    HasDerivAt (fun _ : ℝ => (2 * p - 1) * (whv - wlv)) 0 ρ := by
  refine ⟨?_, ?_, hasDerivAt_const _ _⟩
  · have := (((hasDerivAt_id ρ).const_sub 1).const_mul (2 * p - 1)).mul_const (whv - wlv)
    convert this using 1; ring
  · have h1 := ((hasDerivAt_id ρ).const_sub 1).mul_const (whv - wlv)
    have h2 := (hasDerivAt_id ρ).mul_const (whv - g)
    have := (h1.add h2).const_mul (2 * p - 1)
    convert this using 1; simp

/-- PDF p. 46. The window `pβ < g < 4p(1 − p)β` is non-empty iff `p < 3/4`. -/
theorem fn_precision {p β : ℝ} (hp : 0 < p) (hβ : 0 < β) :
    (∃ g, p * β < g ∧ g < 4 * p * (1 - p) * β) ↔ p < 3 / 4 := by
  constructor
  · rintro ⟨g, h1, h2⟩
    by_contra h; push_neg at h
    nlinarith [mul_pos hp hβ]
  · intro h
    refine ⟨(p * β + 4 * p * (1 - p) * β) / 2, ?_, ?_⟩ <;> nlinarith [mul_pos hp hβ]

/-- Control: at `p = 3/4` the window is empty. -/
theorem control_fn_precision (β : ℝ) :
    ¬ ∃ g, 3 / 4 * β < g ∧ g < 4 * (3 / 4) * (1 - 3 / 4) * β := by
  rintro ⟨g, h1, h2⟩; nlinarith

/-! ## Section 6.5, Chung (2000) -/

/-- The prior that the state is `p₁` next period, after belief `q` this period. -/
noncomputable def chungM (θ12 θ21 q : ℝ) : ℝ := q * (1 - θ12) + (1 - q) * θ21

/-- Footnote 20. `q*`, the belief that stays put after a simple placement, solves
the printed quadratic: the quadratic is the fixed-point condition
`q* = m(1 − αp₁)/(m(1 − αp₁) + (1 − m)(1 − αp₂))` with `m = m(q*)`, where `α` is the
share of low-cost types, who alone invest. -/
theorem chung_fn20 (α p1 p2 θ12 θ21 q : ℝ) :
    q * (chungM θ12 θ21 q * (1 - α * p1) + (1 - chungM θ12 θ21 q) * (1 - α * p2))
        - chungM θ12 θ21 q * (1 - α * p1)
      = α * (p2 - p1) * (1 - θ12 - θ21) * q ^ 2
        + ((θ12 + θ21) * (1 - α * p1) - α * (1 - θ21) * (p2 - p1)) * q - θ21 * (1 - α * p1) := by
  unfold chungM; ring

/-- PDF p. 48. After the first complex placement the belief in the bad state
falls, `q_n < q*`. -/
theorem chung_role_model {α p1 p2 θ12 θ21 q : ℝ} (hα0 : 0 ≤ α) (hα1 : α ≤ 1) (hp1 : 0 < p1)
    (hp12 : p1 < p2) (hp2 : p2 < 1) (ht12 : 0 < θ12) (ht12' : θ12 < 1 / 2) (ht21 : 0 < θ21)
    (ht21' : θ21 < 1 / 2) (hq0 : 0 ≤ q) (hq1 : q ≤ 1)
    (hfix : q * (chungM θ12 θ21 q * (1 - α * p1) + (1 - chungM θ12 θ21 q) * (1 - α * p2))
      = chungM θ12 θ21 q * (1 - α * p1)) :
    chungM θ12 θ21 q * p1 / (chungM θ12 θ21 q * p1 + (1 - chungM θ12 θ21 q) * p2) < q := by
  set m := chungM θ12 θ21 q with hm
  have hm0 : 0 < m := by rw [hm]; unfold chungM; nlinarith
  have hm1 : m < 1 := by rw [hm]; unfold chungM; nlinarith
  have hd : 0 < m * p1 + (1 - m) * p2 := by nlinarith
  have hqn : m * p1 / (m * p1 + (1 - m) * p2) < m := by
    rw [div_lt_iff₀ hd]; nlinarith [mul_pos (mul_pos hm0 (sub_pos.2 hm1)) (sub_pos.2 hp12)]
  have hmq : m ≤ q := by
    have hden : 0 < m * (1 - α * p1) + (1 - m) * (1 - α * p2) := by nlinarith
    have hle : m * (1 - α * p1) + (1 - m) * (1 - α * p2) ≤ 1 - α * p1 := by
      nlinarith [mul_nonneg (mul_nonneg (sub_pos.2 hm1).le hα0) (sub_pos.2 hp12).le]
    nlinarith [mul_le_mul_of_nonneg_left hle hm0.le]
  linarith

/-! ## Section 6.6.2, Chan and Eyster (2003) -/

/-- Eq. (59), `γ(t) = t + (α/C) λ_B f_B/(λ_B f_B + λ_W f_W)`, at a score `t` with
densities `fB`, `fW`. -/
noncomputable def ceGamma (α C lB lW t fB fW : ℝ) : ℝ := t + α / C * (lB * fB / (lB * fB + lW * fW))

/-- Eq. (58), the integrand. Up to the constant `αλ_B`, the colour-blind
objective at score `t` is `r(t) γ(t) [λ_B f_B(t) + λ_W f_W(t)]`. -/
theorem ce_eq58 {α C lB lW t fB fW r : ℝ} (h : lB * fB + lW * fW ≠ 0) :
    r * t * (lB * fB + lW * fW) + α / C * (r * lB * fB)
      = r * ceGamma α C lB lW t fB fW * (lB * fB + lW * fW) := by
  unfold ceGamma; field_simp

/-- PDF p. 52. Under Assumption 2 the diversity term of `γ` falls as
`f_W/f_B` rises. -/
theorem ce_share_decreasing {lB lW ρ₁ ρ₂ : ℝ} (hB : 0 < lB) (hW : 0 < lW) (h1 : 0 ≤ ρ₁)
    (h12 : ρ₁ < ρ₂) : lB / (lB + lW * ρ₂) < lB / (lB + lW * ρ₁) := by
  apply div_lt_div_of_pos_left hB (by positivity); nlinarith

/-- PDF p. 51. The colour-sighted first-order conditions equate the marginal
value of the last admitted black and white applicant to the capacity
multiplier, so `t_B + α/C = t_W`. -/
theorem ce_foc {tB tW α C μ : ℝ} (hB : tB + α / C = μ) (hW : tW = μ) : tB + α / C = tW := by
  rw [hB, hW]

/-! ## Section 6.6.3, Fryer and Loury (2008) -/

/-- PDF p. 55, `F(v; π) = π H₁(v) + (1 − π) H₀(v)`. -/
def flF (H1 H0 : ℝ → ℝ) (π v : ℝ) : ℝ := π * H1 v + (1 - π) * H0 v

/-- PDF p. 58. From eqs. (69) and (70), the efficient colour-sighted slot
subsidy is `τ_B = F⁻¹(1 − ρ_W; π_W) − F⁻¹(1 − ρ_B; π_B)`, with
`ρ_W = (C − λ_B ρ_B)/λ_W` from slot clearing. -/
theorem fl_colorsighted {p τB qB qW C lB lW ρB ρW : ℝ} (h69 : p - τB = qB) (h70 : p = qW)
    (hW : lW ≠ 0) (hclear : lW * ρW + lB * ρB = C) :
    τB = qW - qB ∧ ρW = (C - lB * ρB) / lW := by
  constructor
  · linarith
  · field_simp; linarith

/-- Eqs. (75), (78) and (79). -/
theorem fl_eq79 {C FB F ρB αc : ℝ} (h75 : αc = C / (1 - F))
    (h78 : ρB = αc * (1 - FB)) : ρB = C * (1 - FB) / (1 - F) := by
  rw [h78, h75]; ring

/-! ## Section 7, efficiency -/

/-- PDF p. 62. Total product is average productivity whether or not race is
used: each group's average wage is its mean, and pooled the average wage is
the pooled mean. -/
theorem eff_total_product {l sB eB sW eW s e μB μW : ℝ} (hB : sB + eB ≠ 0) (hW : sW + eW ≠ 0)
    (h : s + e ≠ 0) :
    l * condExp sB eB μB μB + (1 - l) * condExp sW eW μW μW = l * μB + (1 - l) * μW ∧
      condExp s e (l * μB + (1 - l) * μW) (l * μB + (1 - l) * μW) = l * μB + (1 - l) * μW := by
  rw [phelps_case2_average hB, phelps_case2_average hW, phelps_case2_average h]
  exact ⟨rfl, rfl⟩

/-- PDF p. 63, the net social product of investment in group `j`,
`MP(X) − MP(0) − C(X) = (b²/c) w (1 − w/2)` with `w = σ²/(σ² + σ_εj²)`. -/
theorem ls_net_product {a b c w : ℝ} (hc : c ≠ 0) :
    (a + b * (b / c * w)) - a - c * (b / c * w) ^ 2 / 2 = b ^ 2 / c * w * (1 - w / 2) := by
  field_simp; ring

/-- Control: the survey prints `(b²/c)(1 − w/2)`, without the factor `w`
(FM-D14). At `b = c = 1`, `w = 1/2` the two differ. -/
theorem control_ls_printed_net_product :
    (1 : ℝ) ^ 2 / 1 * (1 / 2) * (1 - 1 / 2 / 2) ≠ 1 ^ 2 / 1 * (1 - 1 / 2 / 2) := by norm_num

/-- PDF pp. 65–66, Norman's example with `y = √(CS)` and `f = 2/3`. -/
theorem norman_example :
    Real.sqrt (1 / 2 * (1 / 2)) = 1 / 2 ∧ Real.sqrt (2 / 3 * (1 / 2) * (1 / 2)) < 1 / 2 ∧
      (3 / 2 : ℝ) * (2 / 3 - 1 / 3) = 1 / 2 ∧ (3 / 2 : ℝ) * (2 / 3) * (1 / 2) = 1 / 2 ∧
      (1 / 2 : ℝ) * (1 / 10) = 1 / 20 := by
  refine ⟨?_, ?_, by norm_num, by norm_num, by norm_num⟩
  · rw [show (1 / 2 * (1 / 2) : ℝ) = (1 / 2) ^ 2 by norm_num]
    exact Real.sqrt_sq (by norm_num)
  · rw [Real.sqrt_lt' (by norm_num)]; norm_num

/-- Eq. (82). The expected productivity of a high-signal worker is higher in the
group that invests more. -/
theorem norman_eq82 {πB πW f : ℝ} (hf : 1 / 2 < f) (hf1 : f < 1) (h0 : 0 < πB) (hBW : πB < πW)
    (hW : πW < 1) : post πB f (1 - f) < post πW f (1 - f) :=
  post_strictMono_prior (by linarith) (by linarith) h0 hBW hW

/-- PDF p. 67, the step of the Pareto ranking. A worker who invests only in the
high-investment equilibrium `π₁` gets at least the non-investors' wage under
`π₂`, given wages that are weakly higher under `π₁`. The survey's strict
inequality needs a strict step (FM-D15). -/
theorem mn_pareto_chain {aq1 c au1 au2 : ℝ} (hinv : au1 ≤ aq1 - c) (hw : au2 ≤ au1) :
    au2 ≤ aq1 - c := le_trans hw hinv

end FangMoro2010
