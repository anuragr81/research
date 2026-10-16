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

end FangMoro2010
