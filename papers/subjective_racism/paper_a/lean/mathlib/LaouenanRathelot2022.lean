import Mathlib

/-! Laouénan and Rathelot, "Can Information Reduce Ethnic Discrimination?
Evidence from Airbnb", *American Economic Journal: Applied Economics* 14(1),
2022, pp. 107–132, Section II and Section III.A–B, in the paper's notation.

A listing's log quality is `q = ζ + ν`. Guests see `ζ` and the host's
ethnicity `m` (1 for a minority host). The unobserved part `ν` given `m` is
normal with mean `ν̄_m` and variance `σ_ν²`. Each of `K` reviews is a signal of
`ν` with error variance `σ²`, and `r` is their average. With `ρ = σ²/σ_ν²` the
guests' expected `ν` is `(K r + ρ ν̄_m)/(K + ρ)` (p. 118). The host's log price
is eq. (2), `p = p₀ − λγm + λαw + λβζ + λβ (K r + ρ ν̄_m)/(K + ρ)`.

What is formalised. The posterior mean and its rewriting as the prior plus
`K/(K + ρ)` times the surprise, which is the step from eq. (2) to the
regressions (3) and (4); the half-information property of `ρ`; the price gap
between two listings of the same quality and its limit; the identification of
`βm` and `β̃m` and the decomposition `βm = β̃m + (βm − β̃m)` of Section III.B;
and the host's first-order condition behind eq. (1), whose constant `p₀` we
read with the opposite sign (LR-D2). The normal updating itself is taken from
the paper and not reproved. -/

namespace LaouenanRathelot2022

open Filter Topology

/-- p. 118. The guests' expected `ν` for a listing with `K` reviews of average
`r` and host prior `ν̄_m`, `E(ν | r, K, m) = (K r + ρ ν̄_m)/(K + ρ)`. -/
noncomputable def postMean (K ρ r νbar : ℝ) : ℝ := (K * r + ρ * νbar) / (K + ρ)

/-- Table 4 notes. `f(K) = K/(K + ρ)`. -/
noncomputable def f (K ρ : ℝ) : ℝ := K / (K + ρ)

/-- The posterior mean is the prior plus `f(K)` times the surprise `r − ν̄_m`.
This is the step from eq. (2) to eqs. (3) and (4). -/
theorem postMean_eq {K ρ : ℝ} (r νbar : ℝ) (h : K + ρ ≠ 0) :
    postMean K ρ r νbar = νbar + f K ρ * (r - νbar) := by
  unfold postMean f
  field_simp
  ring

/-- With no reviews the guests' expectation is the group prior, so ethnicity is
all they have (p. 108). -/
theorem postMean_no_reviews {ρ : ℝ} (r νbar : ℝ) (hρ : ρ ≠ 0) :
    postMean 0 ρ r νbar = νbar := by
  unfold postMean
  rw [zero_mul, zero_add, zero_add]
  field_simp

/-- p. 121. After `ρ` reviews half of the information is revealed,
`f(ρ) = 1/2`. -/
theorem half_at_rho {ρ : ℝ} (hρ : 0 < ρ) : f ρ ρ = 1 / 2 := by
  unfold f
  field_simp
  ring

/-- Eq. (2), the host's log price. -/
noncomputable def price (p0 lam γ α w β ζ m K ρ r νbar : ℝ) : ℝ :=
  p0 - lam * γ * m + lam * α * w + lam * β * ζ + lam * β * postMean K ρ r νbar

/-- Two listings of the same quality, same `ζ`, `w`, `r` and `K`, one held by a
minority host (`m = 1`, prior `ν̄₁`) and one by a majority host (`m = 0`,
prior `ν̄₀`). Their price gap is the taste term plus the statistical term,
which is `λβ (ν̄₁ − ν̄₀)` scaled by `ρ/(K + ρ)`. -/
theorem gap_same_quality {p0 lam γ α w β ζ K ρ r ν1 ν0 : ℝ} (h : K + ρ ≠ 0) :
    price p0 lam γ α w β ζ 1 K ρ r ν1 - price p0 lam γ α w β ζ 0 K ρ r ν0
      = -(lam * γ) + lam * β * (ν1 - ν0) * (ρ / (K + ρ)) := by
  unfold price postMean
  field_simp
  ring

/-- p. 109 and p. 120. Without taste-based discrimination (`γ = 0`) the gap
between two listings of the same quality tends to zero as reviews accumulate. -/
theorem gap_tendsto_zero {p0 lam α w β ζ ρ r ν1 ν0 : ℝ} (hρ : 0 < ρ) :
    Tendsto (fun K => price p0 lam 0 α w β ζ 1 K ρ r ν1 - price p0 lam 0 α w β ζ 0 K ρ r ν0)
      atTop (𝓝 0) := by
  have hK : Tendsto (fun K : ℝ => K + ρ) atTop atTop := tendsto_atTop_add_const_right _ _ tendsto_id
  have h1 : Tendsto (fun K : ℝ => ρ / (K + ρ)) atTop (𝓝 0) :=
    Tendsto.div_atTop tendsto_const_nhds hK
  have h2 : Tendsto (fun K : ℝ => -(lam * 0) + lam * β * (ν1 - ν0) * (ρ / (K + ρ))) atTop
      (𝓝 (-(lam * 0) + lam * β * (ν1 - ν0) * 0)) :=
    tendsto_const_nhds.add (tendsto_const_nhds.mul h1)
  simp only [mul_zero, neg_zero, zero_add] at h2
  refine h2.congr' ?_
  filter_upwards [eventually_gt_atTop 0] with K hK0
  rw [gap_same_quality (by linarith)]
  ring

/-- Control: with taste-based discrimination the gap does not vanish. It tends
to `−λγ`, so with `λγ ≠ 0` it does not tend to zero. This is why the pooled
residual of p. 119 is absorbed by the listing fixed effect rather than read
off the slope in reviews. -/
theorem control_taste_gap_survives {p0 lam γ α w β ζ ρ r ν1 ν0 : ℝ} (hρ : 0 < ρ)
    (hγ : lam * γ ≠ 0) :
    Tendsto (fun K => price p0 lam γ α w β ζ 1 K ρ r ν1 - price p0 lam γ α w β ζ 0 K ρ r ν0)
        atTop (𝓝 (-(lam * γ))) ∧
      ¬ Tendsto (fun K => price p0 lam γ α w β ζ 1 K ρ r ν1 - price p0 lam γ α w β ζ 0 K ρ r ν0)
        atTop (𝓝 0) := by
  have hK : Tendsto (fun K : ℝ => K + ρ) atTop atTop := tendsto_atTop_add_const_right _ _ tendsto_id
  have h1 : Tendsto (fun K : ℝ => ρ / (K + ρ)) atTop (𝓝 0) :=
    Tendsto.div_atTop tendsto_const_nhds hK
  have h2 : Tendsto (fun K : ℝ => -(lam * γ) + lam * β * (ν1 - ν0) * (ρ / (K + ρ))) atTop
      (𝓝 (-(lam * γ) + lam * β * (ν1 - ν0) * 0)) :=
    tendsto_const_nhds.add (tendsto_const_nhds.mul h1)
  simp only [mul_zero, add_zero] at h2
  have hlim : Tendsto
      (fun K => price p0 lam γ α w β ζ 1 K ρ r ν1 - price p0 lam γ α w β ζ 0 K ρ r ν0)
      atTop (𝓝 (-(lam * γ))) := by
    refine h2.congr' ?_
    filter_upwards [eventually_gt_atTop 0] with K hK0
    rw [gap_same_quality (by linarith)]
  refine ⟨hlim, fun h0 => hγ ?_⟩
  have := tendsto_nhds_unique hlim h0
  linarith

/-! ### Section III.B. Accurate and inaccurate beliefs

Guests hold the true prior `ν̄₀` for majority listings and a possibly wrong
prior `ν̃₁` for minority listings. The true minority mean is `ν̄₁`. As functions
of `m ∈ {0, 1}`, the guests' prior is `ν̄₀ + m (ν̃₁ − ν̄₀)` and the true mean is
`ν̄₀ + m (ν̄₁ − ν̄₀)`. -/

/-- Prediction 1. `βm = λβ (ν̃₁ − ν̄₀)`, the gap attributed to statistical
discrimination. -/
def betaM (lam β νt1 ν0 : ℝ) : ℝ := lam * β * (νt1 - ν0)

/-- Prediction 2. `β̃m = λβ (ν̃₁ − ν̄₁)`, the gap attributed to inaccurate
beliefs. -/
def betaTildeM (lam β νt1 ν1 : ℝ) : ℝ := lam * β * (νt1 - ν1)

/-- Eq. (3). Holding the signal `r` fixed (the paper conditions on the final
rating as its proxy), the review-driven part of the price,
`λβ f(K) (r − prior_m)`, is a common term in `r` minus `βm m f(K)`. -/
theorem eq3_form (lam β fK r m νt1 ν0 : ℝ) :
    lam * β * fK * (r - (ν0 + m * (νt1 - ν0)))
      = lam * β * fK * (r - ν0) - betaM lam β νt1 ν0 * m * fK := by
  unfold betaM; ring

/-- Eq. (4). Without conditioning on the rating, `r` is replaced by the group's
true mean, and the review-driven part of the price is `−β̃m m f(K)`. -/
theorem eq4_form (lam β fK m νt1 ν1 ν0 : ℝ) :
    lam * β * fK * ((ν0 + m * (ν1 - ν0)) - (ν0 + m * (νt1 - ν0)))
      = -(betaTildeM lam β νt1 ν1 * m * fK) := by
  unfold betaTildeM; ring

/-- Section III.B. The total statistical gap is the inaccurate-belief part plus
the part due to the true difference in means, `βm − β̃m = λβ (ν̄₁ − ν̄₀)`. -/
theorem decomposition (lam β νt1 ν1 ν0 : ℝ) :
    betaM lam β νt1 ν0 = betaTildeM lam β νt1 ν1 + lam * β * (ν1 - ν0) := by
  unfold betaM betaTildeM; ring

/-- p. 120. With accurate beliefs `β̃m = 0`. -/
theorem accurate_beliefs_tilde_zero (lam β ν1 : ℝ) : betaTildeM lam β ν1 ν1 = 0 := by
  unfold betaTildeM; ring

/-- p. 120. With equal true means, `βm = β̃m`. -/
theorem equal_means_same (lam β νt1 ν0 : ℝ) :
    betaM lam β νt1 ν0 = betaTildeM lam β νt1 ν0 := by
  unfold betaM betaTildeM; ring

/-- Control: `βm = β̃m` needs equal true means. With `λβ ≠ 0` and `ν̄₁ ≠ ν̄₀`
the two differ, by `λβ (ν̄₁ − ν̄₀)`. -/
theorem control_equal_means_needed {lam β νt1 ν1 ν0 : ℝ} (hlb : lam * β ≠ 0)
    (hν : ν1 ≠ ν0) : betaM lam β νt1 ν0 ≠ betaTildeM lam β νt1 ν1 := by
  intro h
  rw [decomposition lam β νt1 ν1 ν0] at h
  have : lam * β * (ν1 - ν0) = 0 := by linarith
  rcases mul_eq_zero.mp this with h1 | h1
  · exact hlb h1
  · exact hν (by linarith)

/-- p. 109. With accurate beliefs and a true difference in means, the average
price gap between the groups, with `r` at each group's true mean, is
`λβ (ν̄₁ − ν̄₀)` at every `K`. It does not shrink with reviews, although the gap
between two listings of the same quality does (`gap_tendsto_zero`). -/
theorem group_average_gap_constant {K ρ : ℝ} (lam β ν1 ν0 : ℝ) (h : K + ρ ≠ 0) :
    lam * β * postMean K ρ ν1 ν1 - lam * β * postMean K ρ ν0 ν0 = lam * β * (ν1 - ν0) := by
  unfold postMean
  field_simp

/-! ### Eq. (1), the host's price

The host maximises `P D(P) + (1 − D(P)^{1/α̃}) W` with `D(P) = A P^{−κ}`,
where `A = Q^β/Γ^m`. Setting the derivative to zero and taking logs gives our
log first-order condition below. Solving it gives eq. (1) with
`p₀ = λα log(κ/(α̃(κ − 1)))`, where the paper prints
`p₀ = λα log(α̃(κ − 1)/κ)`. The two differ in sign (LR-D2). `p₀` is a constant
absorbed by the listing fixed effects and no result of the paper uses it. -/

/-- The objective in terms of `A`, `P`, `W`, with `D^{1/α̃}` written as
`A^{1/α̃} P^{−κ/α̃}`. -/
noncomputable def objective (A W κ αt P : ℝ) : ℝ :=
  A * P ^ (1 - κ) + (1 - A ^ (1 / αt) * P ^ (-κ / αt)) * W

/-- `D^{1/α̃} = A^{1/α̃} P^{−κ/α̃}` for `A, P > 0`, so `objective` is the
paper's program. -/
theorem objective_eq {A W κ αt P : ℝ} (hA : 0 < A) (hP : 0 < P) :
    objective A W κ αt P
      = P * (A * P ^ (-κ)) + (1 - (A * P ^ (-κ)) ^ (1 / αt)) * W := by
  unfold objective
  rw [Real.mul_rpow hA.le (Real.rpow_nonneg hP.le _), ← Real.rpow_mul hP.le]
  have e1 : P * (A * P ^ (-κ)) = A * P ^ (1 - κ) := by
    rw [Real.rpow_sub hP, Real.rpow_one, Real.rpow_neg hP.le]
    field_simp
  rw [e1]
  congr 3
  ring_nf

/-- The derivative of the objective in `P`. -/
theorem hasDerivAt_objective {A W κ αt P : ℝ} (hP : 0 < P) :
    HasDerivAt (objective A W κ αt)
      (A * ((1 - κ) * P ^ (1 - κ - 1))
        - A ^ (1 / αt) * ((-κ / αt) * P ^ (-κ / αt - 1)) * W) P := by
  have h1 := (Real.hasDerivAt_rpow_const (p := 1 - κ) (Or.inl hP.ne')).const_mul A
  have h2 := ((Real.hasDerivAt_rpow_const (p := -κ / αt) (Or.inl hP.ne')).const_mul
    (A ^ (1 / αt))).const_sub 1
  have h3 := h1.add (h2.mul_const W)
  unfold objective
  convert h3 using 1
  ring

/-- Our log first-order condition. A zero derivative at `P > 0` with `A, W > 0`,
`κ > 1`, `0 < α̃` is
`log(κ − 1) + log A − κ p = w + log(κ/α̃) + log A/α̃ − (κ/α̃ + 1) p`
with `p = log P`, `w = log W`. -/
theorem foc_log {A W κ αt P : ℝ} (hA : 0 < A) (hW : 0 < W) (hκ : 1 < κ) (hα : 0 < αt)
    (hP : 0 < P)
    (h0 : A * ((1 - κ) * P ^ (1 - κ - 1))
        - A ^ (1 / αt) * ((-κ / αt) * P ^ (-κ / αt - 1)) * W = 0) :
    Real.log (κ - 1) + Real.log A - κ * Real.log P
      = Real.log W + Real.log (κ / αt) + Real.log A / αt - (κ / αt + 1) * Real.log P := by
  have e : (κ - 1) * A * P ^ (-κ) = W * (κ / αt) * A ^ (1 / αt) * P ^ (-κ / αt - 1) := by
    have : (1 - κ - 1) = -κ := by ring
    rw [this] at h0
    linear_combination -h0
  have hl := congrArg Real.log e
  have p1 : 0 < κ - 1 := by linarith
  have p2 : 0 < κ / αt := div_pos (by linarith) hα
  rw [Real.log_mul (by positivity) (by positivity), Real.log_mul (by positivity) (by positivity),
    Real.log_mul (by positivity) (by positivity), Real.log_mul (by positivity) (by positivity),
    Real.log_mul (by positivity) (by positivity), Real.log_rpow hP, Real.log_rpow hP,
    Real.log_rpow hA] at hl
  linear_combination hl

/-- Eq. (1) from the log first-order condition, with `α = α̃/(1 − α̃)`,
`λ = (κ + α)⁻¹` and our constant `λα log(κ/(α̃(κ − 1)))`. Here `a` stands for
`log A = βq − γm`. -/
theorem eq1_from_foc {κ αt p w a : ℝ} (hκ : 1 < κ) (hα0 : 0 < αt) (hα1 : αt < 1)
    (hfoc : Real.log (κ - 1) + a - κ * p
      = w + Real.log (κ / αt) + a / αt - (κ / αt + 1) * p) :
    let α := αt / (1 - αt)
    let lam := (κ + α)⁻¹
    p = lam * α * Real.log (κ / (αt * (κ - 1))) + lam * α * w + lam * a := by
  intro α lam
  have h1 : 0 < 1 - αt := by linarith
  have hk1 : 0 < κ - 1 := by linarith
  have hL : Real.log (κ / (αt * (κ - 1))) = Real.log (κ / αt) - Real.log (κ - 1) := by
    rw [show κ / (αt * (κ - 1)) = (κ / αt) / (κ - 1) by field_simp]
    exact Real.log_div (by positivity) hk1.ne'
  rw [hL]
  have hden : κ * (1 - αt) + αt ≠ 0 := by nlinarith
  have hlam : lam = (1 - αt) / (κ * (1 - αt) + αt) := by
    simp only [lam, α]
    field_simp
  rw [hlam]
  simp only [α]
  field_simp
  field_simp at hfoc
  linear_combination hfoc

/-- Control: the constant printed on p. 117, `λα log(α̃(κ − 1)/κ)`, is minus the
one the first-order condition gives, so it agrees with it only when
`κ = α̃(κ − 1)`. -/
theorem control_printed_p0 {κ αt c : ℝ} (hκ : 1 < κ) (hα0 : 0 < αt) (hc : c ≠ 0)
    (hne : κ ≠ αt * (κ - 1)) :
    c * Real.log (αt * (κ - 1) / κ) ≠ c * Real.log (κ / (αt * (κ - 1))) := by
  intro h
  have hk1 : 0 < κ - 1 := by linarith
  have hpos : 0 < κ / (αt * (κ - 1)) := by positivity
  have hinv : αt * (κ - 1) / κ = (κ / (αt * (κ - 1)))⁻¹ := by
    field_simp
  rw [hinv, Real.log_inv] at h
  have h0 : Real.log (κ / (αt * (κ - 1))) = 0 := by
    have := mul_left_cancel₀ hc (show c * (-Real.log (κ / (αt * (κ - 1))))
      = c * Real.log (κ / (αt * (κ - 1))) from h)
    linarith
  rcases Real.log_eq_zero.mp h0 with h' | h' | h'
  · exact absurd h' (by positivity)
  · apply hne
    field_simp at h'
    linarith
  · linarith [hpos]

/-! ### The estimating equations as specifications

Each specification is the conditional mean of `p_it` (or of another outcome),
with listing effect `μ` and `X β_x` collapsed into the scalars `μ` and `xb`.
`βr` is the coefficient on `1{r̄_i = r̄} f(K)` for the listing's rating bin. -/

/-- Eq. (3). -/
def eq3 (βr βm m fK μ xb : ℝ) : ℝ := βr * fK - βm * m * fK + μ + xb

/-- Eq. (4). -/
def eq4 (βk βtm m fK μ xb : ℝ) : ℝ := βk * fK - βtm * m * fK + μ + xb

/-- Eq. (5), with `K` linear and quadratic. -/
def eq5 (βr1 βr2 βm1 βm2 m K μ xb : ℝ) : ℝ :=
  (βr1 * K + βr2 * K ^ 2) - m * (βm1 * K + βm2 * K ^ 2) + μ + xb

/-- Eqs. (7), (8) and (9) share one form, with `b_r` and `b_m` in place of
`β_r` and `β_m`. -/
def eq789 (br bm m fK μ xb : ℝ) : ℝ := br * fK - bm * m * fK + μ + xb

/-- Predictions 0 and 1 (PDF pp. 12–13). With guests' prior `ν̄₀ + m(ν̃₁ − ν̄₀)`
and the signal `r` taken as known (the paper proxies it by the last rating),
eq. (2) is eq. (3) with `βr = λβ(r − ν̄₀)`, `βm = λβ(ν̃₁ − ν̄₀)` and the
time-invariant terms in `μ`. -/
theorem model_is_eq3 {p0 lam γ α w β ζ m K ρ r νt1 ν0 : ℝ} (h : K + ρ ≠ 0) :
    price p0 lam γ α w β ζ m K ρ r (ν0 + m * (νt1 - ν0))
      = eq3 (lam * β * (r - ν0)) (betaM lam β νt1 ν0) m (f K ρ)
          (p0 - lam * γ * m + lam * α * w + lam * β * ζ + lam * β * (ν0 + m * (νt1 - ν0))) 0 := by
  unfold price eq3 betaM
  rw [postMean_eq _ _ h]
  ring

/-- Prediction 2 (PDF p. 13). With `r` at its group's true mean `ν̄₀ + m(ν̄₁ − ν̄₀)`,
eq. (2) is eq. (4) with `βk = 0` and `β̃m = λβ(ν̃₁ − ν̄₁)`. The paper's `βk f(K)`
picks up the average drift of `r` across listings, which this deterministic
reading sets to zero. -/
theorem model_is_eq4 {p0 lam γ α w β ζ m K ρ νt1 ν1 ν0 : ℝ} (h : K + ρ ≠ 0) :
    price p0 lam γ α w β ζ m K ρ (ν0 + m * (ν1 - ν0)) (ν0 + m * (νt1 - ν0))
      = eq4 0 (betaTildeM lam β νt1 ν1) m (f K ρ)
          (p0 - lam * γ * m + lam * α * w + lam * β * ζ + lam * β * (ν0 + m * (νt1 - ν0))) 0 := by
  unfold price eq4 betaTildeM
  rw [postMean_eq _ _ h]
  ring

/-! ### Normal learning as precision weighting (PDF pp. 11–12)

The paper states the posterior mean of `ν` after `K` reviews. Under normal
learning it is the precision-weighted average of the prior mean and the review
average. The conjugacy of the normal prior and normal signals is taken as
given (Mathlib has no Gaussian conjugacy); what is checked is that the
precision-weighted mean is the paper's `(K r + ρ ν̄_m)/(K + ρ)` with
`ρ = σ²/σ_ν²`. -/

theorem precision_weighted {σ2 σν2 K : ℝ} (r νbar : ℝ) (hσ : 0 < σ2) (hν : 0 < σν2)
    (hK : 0 ≤ K) :
    (νbar / σν2 + K * r / σ2) / (1 / σν2 + K / σ2) = postMean K (σ2 / σν2) r νbar := by
  unfold postMean
  have h1 : 0 < 1 / σν2 + K / σ2 := by positivity
  have h2 : 0 < K + σ2 / σν2 := by positivity
  field_simp
  ring

/-- `f(K) = K/(K + ρ)` rises strictly in `K`. -/
theorem f_strictMonoOn {ρ : ℝ} (hρ : 0 < ρ) : StrictMonoOn (fun K => f K ρ) (Set.Ici 0) := by
  intro x hx y hy hxy
  simp only [Set.mem_Ici] at hx hy
  unfold f
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith

/-- PDF p. 122. `f` is concave on `K ≥ 0`, which is why the quadratic term on
`K` in eq. (5) should be negative, `βm,2 < 0`. -/
theorem f_concaveOn {ρ : ℝ} (hρ : 0 < ρ) : ConcaveOn ℝ (Set.Ici 0) (fun K => f K ρ) := by
  refine ⟨convex_Ici 0, ?_⟩
  intro x hx y hy a b ha hb hab
  simp only [Set.mem_Ici] at hx hy
  simp only [smul_eq_mul]
  unfold f
  have hb' : b = 1 - a := by linarith
  subst hb'
  have hx' : 0 < x + ρ := by linarith
  have hy' : 0 < y + ρ := by linarith
  have hz : 0 < a * x + (1 - a) * y + ρ := by nlinarith
  rw [← mul_div_assoc, ← mul_div_assoc, div_add_div _ _ hx'.ne' hy'.ne',
    div_le_div_iff₀ (by positivity) hz]
  nlinarith [mul_nonneg (mul_nonneg (mul_nonneg ha hb) hρ.le) (sq_nonneg (x - y))]

/-- `ρ` is identified from any listing with reviews. If `K/(K + ρ) = K/(K + ρ')`
at some `K > 0`, then `ρ = ρ'`. -/
theorem rho_identified {K ρ ρ' : ℝ} (hK : 0 < K) (hρ : 0 < ρ) (hρ' : 0 < ρ')
    (h : f K ρ = f K ρ') : ρ = ρ' := by
  unfold f at h
  rw [div_eq_div_iff (by linarith) (by linarith)] at h
  have : K * (ρ' - ρ) = 0 := by linarith
  rcases mul_eq_zero.mp this with h1 | h1
  · linarith
  · linarith

/-- Control: at `K = 0` every `ρ` gives the same `f`, so listings without
reviews say nothing about `ρ`. -/
theorem control_rho_needs_reviews (ρ ρ' : ℝ) : f 0 ρ = f 0 ρ' := by
  unfold f; simp

/-! ### The identifying condition of Section III.A

"Conditional on listing fixed effects and `X_it`, the variability in prices
over time does not come from variations in features `ζ` or the outside option
`w`" (PDF p. 12). Under it the change in a listing's price between two dates is
`λβ` times the change in the guests' expected `ν`, whatever `p₀`, `γ`, `α`,
`w`, `ζ` and `m`. -/

theorem within_listing_difference (p0 lam γ α w β ζ m K1 K2 ρ r1 r2 νbar : ℝ) :
    price p0 lam γ α w β ζ m K1 ρ r1 νbar - price p0 lam γ α w β ζ m K2 ρ r2 νbar
      = lam * β * (postMean K1 ρ r1 νbar - postMean K2 ρ r2 νbar) := by
  unfold price; ring

/-- Control: if `ζ` moves between the dates, the within-listing change picks up
`λβ (ζ₁ − ζ₂)` as well, and the slope in reviews no longer isolates the
statistical term. -/
theorem control_zeta_varies {p0 lam γ α w β ζ1 ζ2 m K1 K2 ρ r1 r2 νbar : ℝ}
    (hlb : lam * β ≠ 0) (hζ : ζ1 ≠ ζ2) :
    price p0 lam γ α w β ζ1 m K1 ρ r1 νbar - price p0 lam γ α w β ζ2 m K2 ρ r2 νbar
      ≠ lam * β * (postMean K1 ρ r1 νbar - postMean K2 ρ r2 νbar) := by
  unfold price
  intro h
  have : lam * β * (ζ1 - ζ2) = 0 := by linarith
  rcases mul_eq_zero.mp this with h1 | h1
  · exact hlb h1
  · exact hζ (by linarith)

/-! ### Section IV.E. The rating of the new reviews (PDF p. 126)

`r̃_it = (r̄_it K_it − r̄_i,t−1 K_i,t−1)/(K_it − K_i,t−1)`. With `S_t` the sum of
all ratings at `t`, so that `r̄_t = S_t/K_t`, this is the average of the reviews
that arrived between `t − 1` and `t`. -/

theorem new_reviews_average {Kt Ks St Ss : ℝ} (hKt : Kt ≠ 0) (hKs : Ks ≠ 0) :
    ((St / Kt) * Kt - (Ss / Ks) * Ks) / (Kt - Ks) = (St - Ss) / (Kt - Ks) := by
  rw [div_mul_cancel₀ _ hKt, div_mul_cancel₀ _ hKs]

/-! ### Section IV.G. Eq. (9) (PDF pp. 128–129)

In eq. (9) the minority term is `−bm m f(K)`. A minority listing's expected
flow of new reviews at `K = 0` exceeds its flow as `K → ∞` (where `f → 1`) by
`bm`. Table 10 gives `−bm = −0.018`, the "0.02 more reviews". -/

theorem table10_initial_difference (bm : ℝ) : (-(bm * 1 * 0)) - (-(bm * 1 * 1)) = bm := by
  ring

/-! ### Numbers that follow from printed numbers -/

/-- Table 1 (PDF p. 8). The five groups sum to the 2,474,551 observations of
Tables 2 and 4, and so do the five subsamples of Table 3 (PDF p. 10). The
minority share is 6.2 percent and the majority share 93.8 percent. -/
theorem sample_sizes :
    (2320285 + 49706 + 21365 + 31145 + 52050 : ℚ) = 2474551 ∧
      (351631 + 808000 + 789798 + 352906 + 172216 : ℚ) = 2474551 ∧
      (615 / 10000 : ℚ) < (49706 + 21365 + 31145 + 52050) / 2474551 ∧
      ((49706 + 21365 + 31145 + 52050) / 2474551 : ℚ) < 625 / 10000 ∧
      (9375 / 10000 : ℚ) < 2320285 / 2474551 ∧ (2320285 / 2474551 : ℚ) < 9385 / 10000 := by
  norm_num

/-- PDF pp. 3 and 9. The raw gap of −0.169 log points is a price ratio of
`exp(−0.169)`, a gap between 15 and 16 percent, which the text rounds to
"around 16 percent" on PDF p. 3 and reports as "17 percent" on PDF p. 9 by
reading log points as percent (LR-D8). -/
theorem raw_gap_percent :
    (15 / 100 : ℝ) < 1 - Real.exp (-(169 / 1000)) ∧ 1 - Real.exp (-(169 / 1000)) < 16 / 100 := by
  have hx0 : (0 : ℝ) ≤ 169 / 1000 := by norm_num
  have lo := Real.quadratic_le_exp_of_nonneg hx0
  have hi := Real.exp_bound' hx0 (by norm_num : (169 / 1000 : ℝ) ≤ 1) (by norm_num : 0 < 3)
  simp [Finset.sum_range_succ, Nat.factorial] at hi
  have hpos := Real.exp_pos (169 / 1000 : ℝ)
  rw [Real.exp_neg]
  constructor
  · rw [lt_sub_iff_add_lt, ← lt_sub_iff_add_lt', inv_lt_comm₀ hpos (by norm_num)]
    norm_num at hi ⊢
    linarith
  · rw [sub_lt_iff_lt_add, ← sub_lt_iff_lt_add', lt_inv_comm₀ (by norm_num) hpos]
    norm_num at lo ⊢
    linarith

/-- PDF p. 10. A 3.2 percent gap on an average price of $136 is $4.35, which
the text rounds to $4.4, and it is 16.7 percent of the $26 surplus, "a 17
percent ethnic differential in surplus". -/
theorem surplus_numbers :
    (32 / 1000 : ℚ) * 136 = 4352 / 1000 ∧
      (165 / 1000 : ℚ) < 4352 / 1000 / 26 ∧ (4352 / 1000 / 26 : ℚ) < 175 / 1000 := by
  norm_num

/-- PDF p. 14 and footnote 23. `3.4 − 2.5 = 0.9`; `2.5/3.4` is about
three-quarters and `0.9/3.4` about one quarter; and the point estimate 0.009 of
`βm − β̃m` lies in its interval `[0.006, 0.010]`. -/
theorem decomposition_numbers :
    (34 / 10 : ℚ) - 25 / 10 = 9 / 10 ∧
      (73 / 100 : ℚ) < (25 / 10) / (34 / 10) ∧ ((25 / 10) / (34 / 10) : ℚ) < 74 / 100 ∧
      (26 / 100 : ℚ) < (9 / 10) / (34 / 10) ∧ ((9 / 10) / (34 / 10) : ℚ) < 27 / 100 ∧
      (6 / 1000 : ℚ) ≤ 9 / 1000 ∧ (9 / 1000 : ℚ) ≤ 10 / 1000 := by
  norm_num

/-- PDF p. 18, Table 6. `ρ` is 22 for shared flats and 12 for entire flats,
"roughly twice larger". -/
theorem shared_flat_rho : (18 / 10 : ℚ) < 22 / 12 ∧ (22 / 12 : ℚ) < 19 / 10 := by norm_num

/-! ### Our extension, not in the source: concealing the marker

The conclusion (PDF p. 24) weighs concealing identity against disclosing more
information. The source does not model concealment. Within its posterior, a
hidden `m` replaces the group prior by the pooled prior `s ν̄₁ + (1 − s) ν̄₀`,
where `s` is the minority share, and a visible proxy `ℓ` (location, say)
replaces it by `π_ℓ ν̄₁ + (1 − π_ℓ) ν̄₀`, with `π_ℓ` the minority share among
listings with that proxy. These three theorems are ours. -/

/-- The prior given a proxy whose minority share is `π`. -/
def proxyPrior (π ν1 ν0 : ℝ) : ℝ := π * ν1 + (1 - π) * ν0

/-- Ours. Two listings of the same quality with proxy shares `π₁`, `π₀` differ
in price by `λβ (π₁ − π₀)(ν̄₁ − ν̄₀) ρ/(K + ρ)` when `m` is hidden. -/
theorem proxy_gap {p0 lam α w β ζ K ρ r π1 π0 ν1 ν0 : ℝ} (h : K + ρ ≠ 0) :
    price p0 lam 0 α w β ζ 0 K ρ r (proxyPrior π1 ν1 ν0)
        - price p0 lam 0 α w β ζ 0 K ρ r (proxyPrior π0 ν1 ν0)
      = lam * β * (π1 - π0) * (ν1 - ν0) * (ρ / (K + ρ)) := by
  unfold price postMean proxyPrior
  field_simp
  ring

/-- Ours. A proxy that separates the groups perfectly restores the whole
statistical gap of `gap_same_quality` at `γ = 0`. -/
theorem perfect_proxy_restores_gap {p0 lam α w β ζ K ρ r ν1 ν0 : ℝ} (h : K + ρ ≠ 0) :
    price p0 lam 0 α w β ζ 0 K ρ r (proxyPrior 1 ν1 ν0)
        - price p0 lam 0 α w β ζ 0 K ρ r (proxyPrior 0 ν1 ν0)
      = price p0 lam 0 α w β ζ 1 K ρ r ν1 - price p0 lam 0 α w β ζ 0 K ρ r ν0 := by
  rw [proxy_gap h, gap_same_quality h]
  ring

/-- Ours. Control: an uninformative proxy, `π₁ = π₀`, leaves no gap. -/
theorem control_uninformative_proxy {p0 lam α w β ζ K ρ r π1 π0 ν1 ν0 : ℝ} (h : K + ρ ≠ 0)
    (hπ : π1 = π0) :
    price p0 lam 0 α w β ζ 0 K ρ r (proxyPrior π1 ν1 ν0)
        - price p0 lam 0 α w β ζ 0 K ρ r (proxyPrior π0 ν1 ν0) = 0 := by
  rw [proxy_gap h, hπ]; ring

end LaouenanRathelot2022
