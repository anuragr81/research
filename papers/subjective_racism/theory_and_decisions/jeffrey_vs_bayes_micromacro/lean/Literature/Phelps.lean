/-
# Phelps (1972), "The Statistical Theory of Racism and Sexism"

*American Economic Review* 62(4), 1972, 659-661.  (Drive copy: a scan of the
journal pages; the article is on PDF pages 1-3, pages 4-7 are blank.)

Formalization of the paper's own model and its three cases, not of Paper B's
claims.

## The paper's model (pp. 659-661)

* Test score (eq. 1, p. 659): `y_i = q_i + μ_i`, `q_i` the applicant's (continuous)
  "promise or degree of qualification", `μ` normal with mean zero.
* No group information (eq. 2, p. 660): the employer uses the least-squares
  "regression-type relation" `q'_i = a₁ y'_i + u'_i` with
  `a₁ = var q' / (var q' + var μ)`, `0 < a₁ < 1` (primes = deviations from means).
  Footnote 3: `a₁` is the probability limit of the OLS coefficient
  `â₁ = Σ y'q' / Σ (y')²`.
* With the race dummy observed (eqs. 3, 3a, 4, p. 660):
  `q_i = α + x_i + η_i`, `x_i = (-β + ε_i) c_i`, `β > 0`, `c_i = 1` iff black;
  writing `λ_i = η_i + c_i ε_i`, `z_i = -β c_i`: `q = α + z + λ`, `y = α + z + λ + μ`.
* Prediction (eqs. 5, 5', p. 660): `q' - z' = a₁ (y' - z') + u`,
  `a₁ = var λ / (var λ + var μ)`; equivalently the predicted qualification is the
  weighted average `(1 - a₁)(α + z) + a₁ y` of the group prior mean and the score.
  Under Phelps's normality this linear predictor is the posterior mean
  `E[q | y, c]`; the paper itself only calls it the least-squares predictor.
* **Case 1** (p. 660): `ε ≡ 0`; the coefficient is the same for both groups and the
  black prediction line "lies parallel and below" the white one.
* **Case 2** (eq. 6, pp. 660-661): `var λ_i = var η + c_i² var ε`; blacks have the
  larger variance of *qualification*, so the score coefficient is greater for
  blacks, tends to one as `var ε → ∞`, and at high enough scores the black
  applicant is predicted to excel the white applicant with the same score.
* **Further Case** (eq. 7, p. 661): `μ_i = ξ_i + c_i ρ_i`; whites' *test scores*
  are more reliable, the white line is the steeper one, and at low enough scores
  whites are predicted to be less qualified than equally scoring blacks.

## What is formalized

* `relRatio` (Phelps's `a₁`) with `relRatio_pos`, `relRatio_lt_one`,
  `one_sub_relRatio`.
* `mse_sub_mse_opt` / `relRatio_minimizes_mse`: `a₁` is the unique population
  least-squares slope, `MSE(a) - MSE(a₁) = (var q + var μ)(a - a₁)²` (eq. 2).
* `ssr_sub_ssr_ols`: the finite-sample version behind footnote 3, for the OLS
  coefficient `Σ y q / Σ y²`.
* `predict_eq5`, `predict_eq5'`: eqs. (5) and (5') for the group-specific
  predictor, including the shrinkage form `(1 - γ)·(prior mean) + γ·y`.
* `gap_identity`: the black-minus-white prediction gap at a common score is
  `(a_B - a_W)(y - α) - (1 - a_B)β` -- the identity every case reads from.
* `case1_parallel`: Case 1.
* `case2_slope_gt`, `case2_slope_tendsto_one`, `case2_crossing`: Case 2.
* `further_slope_lt`, `further_crossing`: the Further Case.
* `relRatio_lt_iff`: which group gets the steeper line is decided by comparing
  signal-to-noise ratios `var λ / var μ` -- the paper's "greater reliability ...
  might overcome any tendency for them to have less credibility" (p. 661).
* `no_mean_gap_still_differential`: with `β = 0` (no believed difference in mean
  qualification) but different reliabilities, equal scores are still treated
  differently at every score except the common mean.
-/
import Mathlib

namespace Literature.Phelps

open Filter Topology Finset

/-! ## The reliability ratio `a₁` (eq. 2, p. 660) -/

/-- Phelps's coefficient `a₁ = var q / (var q + var μ)`, eq. (2).  Applied to
`var λ` in place of `var q` it is the coefficient of eq. (5). -/
noncomputable def relRatio (vq vμ : ℝ) : ℝ := vq / (vq + vμ)

/-- `0 < a₁`, eq. (2). -/
theorem relRatio_pos {vq vμ : ℝ} (hq : 0 < vq) (hμ : 0 < vμ) : 0 < relRatio vq vμ := by
  unfold relRatio; positivity

/-- `a₁ < 1`, eq. (2). -/
theorem relRatio_lt_one {vq vμ : ℝ} (hq : 0 < vq) (hμ : 0 < vμ) : relRatio vq vμ < 1 := by
  unfold relRatio
  rw [div_lt_one (by linarith)]
  linarith

/-- The weight on the prior mean: `1 - a₁ = var μ / (var q + var μ)`, the weight
in the second term of eq. (5'). -/
theorem one_sub_relRatio {vq vμ : ℝ} (h : vq + vμ ≠ 0) :
    1 - relRatio vq vμ = vμ / (vq + vμ) := by
  unfold relRatio
  field_simp
  ring

/-! ## `a₁` is the least-squares slope (eq. 2 and footnote 3) -/

/-- Population mean squared error of the linear predictor `a · y'` of `q'` when
`y' = q' + μ` with `q'` and `μ` uncorrelated:
`E[(q' - a(q' + μ))²] = (1 - a)² var q + a² var μ`. -/
def mse (vq vμ a : ℝ) : ℝ := (1 - a) ^ 2 * vq + a ^ 2 * vμ

/-- **Eq. (2) as a least-squares statement.**  The excess loss of any slope `a`
over Phelps's `a₁` is `(var q + var μ)(a - a₁)²`. -/
theorem mse_sub_mse_opt {vq vμ : ℝ} (h : vq + vμ ≠ 0) (a : ℝ) :
    mse vq vμ a - mse vq vμ (relRatio vq vμ) = (vq + vμ) * (a - relRatio vq vμ) ^ 2 := by
  unfold mse relRatio
  field_simp
  ring

/-- Hence `a₁` is the unique minimizer of the mean squared error. -/
theorem relRatio_minimizes_mse {vq vμ : ℝ} (hq : 0 < vq) (hμ : 0 < vμ) (a : ℝ) :
    mse vq vμ (relRatio vq vμ) ≤ mse vq vμ a ∧
      (mse vq vμ a = mse vq vμ (relRatio vq vμ) → a = relRatio vq vμ) := by
  have hs : 0 < vq + vμ := by linarith
  have key := mse_sub_mse_opt hs.ne' a
  constructor
  · nlinarith [sq_nonneg (a - relRatio vq vμ)]
  · intro he
    have h0 : (vq + vμ) * (a - relRatio vq vμ) ^ 2 = 0 := by linarith
    have : (a - relRatio vq vμ) ^ 2 = 0 := by
      rcases mul_eq_zero.mp h0 with h | h
      · linarith
      · exact h
    have := pow_eq_zero_iff (n := 2) (by norm_num) |>.mp this
    linarith

section OLS

variable {N : ℕ}

/-- Sum of squared residuals of the regression of `q'` on `y'` through the
origin (deviations from means), slope `a`. -/
def ssr (y q : Fin N → ℝ) (a : ℝ) : ℝ := ∑ i, (q i - a * y i) ^ 2

/-- The OLS slope of footnote 3, `â₁ = Σ y'q' / Σ (y')²`. -/
noncomputable def olsSlope (y q : Fin N → ℝ) : ℝ := (∑ i, y i * q i) / ∑ i, y i ^ 2

theorem ssr_expand (y q : Fin N → ℝ) (a : ℝ) :
    ssr y q a = (∑ i, q i ^ 2) - 2 * a * (∑ i, y i * q i) + a ^ 2 * (∑ i, y i ^ 2) := by
  unfold ssr
  rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_sub_distrib, ← Finset.sum_add_distrib]
  exact Finset.sum_congr rfl fun i _ => by ring

/-- **Footnote 3.**  `â₁` minimizes the sum of squared residuals:
`SSR(a) - SSR(â₁) = (a - â₁)² Σ (y')²`. -/
theorem ssr_sub_ssr_ols (y q : Fin N → ℝ) (hy : ∑ i, y i ^ 2 ≠ 0) (a : ℝ) :
    ssr y q a - ssr y q (olsSlope y q) = (a - olsSlope y q) ^ 2 * ∑ i, y i ^ 2 := by
  rw [ssr_expand, ssr_expand]
  unfold olsSlope
  field_simp
  ring

end OLS

/-! ## The prediction with the race dummy observed (eqs. 3-5', p. 660) -/

/-- The employer's predicted qualification for an applicant with group dummy `c`
and score `y`, when the score coefficient for that group is `a`:
`(1 - a)(α - β c) + a y`.  `α - β c = α + z` is the believed group mean. -/
def predict (α β a c y : ℝ) : ℝ := (1 - a) * (α - β * c) + a * y

/-- **Eq. (5)** in levels: prediction net of the race factor equals `a₁` times the
score net of the race factor. -/
theorem predict_eq5 (α β a c y : ℝ) :
    predict α β a c y - (α - β * c) = a * (y - (α - β * c)) := by
  unfold predict; ring

/-- **Eq. (5')**: the prediction is the weighted average
`a · y + (1 - a) · (α + z)` of the score and the believed group mean, i.e. the
shrinkage form `(1 - γ) m + γ y` with `γ = a₁`. -/
theorem predict_eq5' (α β a c y : ℝ) :
    predict α β a c y = a * y + (1 - a) * (α - β * c) := by
  unfold predict; ring

/-- Eq. (6): `var λ_i = var η + c_i² var ε`. -/
def varLam (vη vε c : ℝ) : ℝ := vη + c ^ 2 * vε

/-- Eq. (7): `var μ_i = var ξ + c_i² var ρ` (with `ξ`, `ρ` uncorrelated). -/
def varMu (vξ vρ c : ℝ) : ℝ := vξ + c ^ 2 * vρ

/-- The score coefficient of eq. (5) for group `c`. -/
noncomputable def coef (vη vε vξ vρ c : ℝ) : ℝ := relRatio (varLam vη vε c) (varMu vξ vρ c)

/-- **The identity every case is read from.**  At a common score `y`, the black
(`c = 1`) minus white (`c = 0`) prediction gap is
`(a_B - a_W)(y - α) - (1 - a_B) β`. -/
theorem gap_identity (α β aW aB y : ℝ) :
    predict α β aB 1 y - predict α β aW 0 y = (aB - aW) * (y - α) - (1 - aB) * β := by
  unfold predict; ring

/-! ### Case 1 (p. 660) -/

/-- **Case 1.**  With `ε ≡ 0` and a common test error, the coefficient is the same
for both groups, and at *every* score the white prediction exceeds the black one
by the constant `(1 - a₁) β > 0`: the black prediction line "lies parallel and
below that for whites". -/
theorem case1_parallel {vη vξ : ℝ} (hη : 0 < vη) (hξ : 0 < vξ) (α β y : ℝ) (hβ : 0 < β) :
    coef vη 0 vξ 0 1 = coef vη 0 vξ 0 0 ∧
      predict α β (coef vη 0 vξ 0 0) 0 y - predict α β (coef vη 0 vξ 0 1) 1 y
        = (1 - coef vη 0 vξ 0 0) * β ∧
      0 < (1 - coef vη 0 vξ 0 0) * β := by
  have hsame : coef vη 0 vξ 0 1 = coef vη 0 vξ 0 0 := by
    simp [coef, varLam, varMu]
  refine ⟨hsame, ?_, ?_⟩
  · rw [hsame]; unfold predict; ring
  · have : coef vη 0 vξ 0 0 < 1 := by
      simpa [coef, varLam, varMu] using relRatio_lt_one hη hξ
    exact mul_pos (by linarith) hβ

/-! ### Case 2 (eq. 6, pp. 660-661) -/

/-- **Case 2.**  With `var ε > 0` (and a common test error) the coefficient of the
test score "is *greater* for blacks than for whites". -/
theorem case2_slope_gt {vη vε vξ : ℝ} (hη : 0 < vη) (hε : 0 < vε) (hξ : 0 < vξ) :
    coef vη vε vξ 0 0 < coef vη vε vξ 0 1 := by
  simp only [coef, varLam, varMu, relRatio]
  norm_num
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith

/-- **Case 2, the limit.**  "In the limit, as `var ε → ∞`, the coefficient of `y_i`
-- the slope of the prediction curve for blacks -- approaches one." -/
theorem case2_slope_tendsto_one (vη vξ : ℝ) :
    Tendsto (fun vε => coef vη vε vξ 0 1) atTop (𝓝 1) := by
  have hden : Tendsto (fun v : ℝ => vη + v + vξ) atTop atTop :=
    tendsto_atTop_add_const_right _ _ (tendsto_atTop_add_const_left _ _ tendsto_id)
  have hlim : Tendsto (fun v : ℝ => 1 - vξ / (vη + v + vξ)) atTop (𝓝 (1 - 0)) :=
    tendsto_const_nhds.sub (tendsto_const_nhds.div_atTop hden)
  rw [sub_zero] at hlim
  refine hlim.congr' ?_
  filter_upwards [hden.eventually_gt_atTop 0] with v hv
  simp only [coef, varLam, varMu, relRatio]
  norm_num
  field_simp
  ring

/-- **Case 2, the crossing.**  If the black coefficient exceeds the white one,
then above the score `α + (1 - a_B) β / (a_B - a_W)` "the black applicant is
predicted by the employer to excel over any white applicant with the same ...
score". -/
theorem case2_crossing {α β aW aB y : ℝ} (hgt : aW < aB)
    (hy : α + (1 - aB) * β / (aB - aW) < y) :
    predict α β aW 0 y < predict α β aB 1 y := by
  have hd : 0 < aB - aW := by linarith
  have h1 : (1 - aB) * β < (aB - aW) * (y - α) := by
    have := (div_lt_iff₀ hd).mp (by linarith : (1 - aB) * β / (aB - aW) < y - α)
    linarith
  have := gap_identity α β aW aB y
  linarith

/-! ### Further Case (eq. 7, p. 661) -/

/-- **Further Case.**  With a common qualification variance and `var ρ > 0`, the
white coefficient exceeds the black: "the white prediction curve would be the
steeper curve". -/
theorem further_slope_lt {vη vξ vρ : ℝ} (hη : 0 < vη) (hξ : 0 < vξ) (hρ : 0 < vρ) :
    coef vη 0 vξ vρ 1 < coef vη 0 vξ vρ 0 := by
  simp only [coef, varLam, varMu, relRatio]
  norm_num
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith

/-- **Further Case, the crossing.**  If the white coefficient exceeds the black,
then below the score `α - (1 - a_B) β / (a_W - a_B)` "whites are predicted to be
less qualified than equally high scoring blacks". -/
theorem further_crossing {α β aW aB y : ℝ} (hlt : aB < aW)
    (hy : y < α - (1 - aB) * β / (aW - aB)) :
    predict α β aW 0 y < predict α β aB 1 y := by
  have hd : 0 < aW - aB := by linarith
  have h1 : (1 - aB) * β < (aW - aB) * (α - y) := by
    have := (div_lt_iff₀ hd).mp (by linarith : (1 - aB) * β / (aW - aB) < α - y)
    linarith
  have := gap_identity α β aW aB y
  nlinarith

/-- Which group's line is steeper is decided by comparing signal-to-noise ratios
`var λ / var μ`: both Case 2 and the Further Case are instances, and when both
act, "the greater reliability of whites' test scores might overcome any tendency
for them to have less credibility" (p. 661). -/
theorem relRatio_lt_iff {l₁ m₁ l₂ m₂ : ℝ} (hl₁ : 0 < l₁) (hm₁ : 0 < m₁) (hl₂ : 0 < l₂)
    (hm₂ : 0 < m₂) : relRatio l₁ m₁ < relRatio l₂ m₂ ↔ l₁ / m₁ < l₂ / m₂ := by
  unfold relRatio
  rw [div_lt_div_iff₀ (by linarith) (by linarith), div_lt_div_iff₀ hm₁ hm₂]
  constructor <;> intro h <;> nlinarith

/-- **No mean gap, still differential treatment.**  With `β = 0` -- no believed
difference in the groups' mean qualification -- but different coefficients (from
Case 2 variances or Further-Case reliabilities), equally scoring applicants from
the two groups receive different predictions at every score other than the common
mean `α`.  The group difference that drives Phelps's Case 2 and Further Case is a
difference in believed *dispersion* or *test reliability*, not in believed mean. -/
theorem no_mean_gap_still_differential {α aW aB y : ℝ} (hne : aB ≠ aW) (hy : y ≠ α) :
    predict α 0 aB 1 y ≠ predict α 0 aW 0 y := by
  intro h
  have := gap_identity α 0 aW aB y
  rw [h, sub_self, mul_zero, sub_zero] at this
  rcases mul_eq_zero.mp this.symm with h1 | h1
  · exact hne (by linarith)
  · exact hy (by linarith)

end Literature.Phelps
