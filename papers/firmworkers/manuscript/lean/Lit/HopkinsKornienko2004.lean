import Mathlib.Analysis.SpecialFunctions.Integrals.Basic
import Mathlib.Tactic

open intervalIntegral

namespace HopkinsKornienko2004

/-! Status (1), p.1088: `S = γ F(x) + (1 - γ) F⁻(x) + a`. -/

def status (γ F Fm a : ℝ) : ℝ := γ * F + (1 - γ) * Fm + a

theorem tie_penalty {γ a : ℝ} (hγ : γ < 1) : status γ 1 0 a < status γ 1 1 a := by
  unfold status
  linarith

theorem control_tie_needs_gamma_lt_one (a : ℝ) : status 1 1 0 a = status 1 1 1 a := by
  unfold status
  ring

theorem poor_worse_off_at_cooperative {V GA GB a : ℝ} (hV : 0 < V) (h : GA < GB) :
    V * (GA + a) < V * (GB + a) := by
  nlinarith

theorem control_poor_comparison_needs_positive_V (GA GB a : ℝ) : (0 : ℝ) * (GA + a) = 0 * (GB + a) := by
  ring

/-! Preferences `U = x y S` of Examples 1 and 2: `V = x y`, `V₁ = y`, `V₂ = x`. -/

theorem cooperative_tangency {p z : ℝ} (hp : 0 < p) (hz : 0 < z) :
    (z - p * (z / (2 * p))) / (z / (2 * p)) = p := by
  field_simp
  ring

theorem tax_formula_cobb_douglas {g G a p z : ℝ} (hp : 0 < p) (hz : 0 < z) (hG : 0 < G + a) :
    g * ((z / (2 * p)) * (z / 2)) / ((G + a) * p * (z / (2 * p)) * (1 / (2 * p))) = z * g / (G + a) := by
  field_simp
  ring

theorem example1_tax_one {z : ℝ} (hz : 0 < z) : z * 1 / (z + 0) = 1 := by
  field_simp

theorem control_example1_tax_needs_a_zero {z a : ℝ} (hz : 0 < z) (ha : 0 < a) : z * 1 / (z + a) ≠ 1 := by
  rw [mul_one, ne_eq, div_eq_one_iff_eq (by linarith)]
  linarith

theorem example1_solves_ode {p z : ℝ} (hp : 0 < p) (hz : 0 < z) :
    HasDerivAt (fun z => 2 * z / (3 * p)) (2 / (3 * p)) z ∧
      1 / z * ((2 * z / (3 * p)) * (z - p * (2 * z / (3 * p))))
        / (p * (2 * z / (3 * p)) - (z - p * (2 * z / (3 * p)))) = 2 / (3 * p) := by
  constructor
  · have := ((hasDerivAt_id z).const_mul 2).div_const (3 * p)
    simpa using this
  · have hy : z - p * (2 * z / (3 * p)) = z / 3 := by
      field_simp
      ring
    have hx : p * (2 * z / (3 * p)) = 2 * z / 3 := by
      field_simp
      ring
    have h3 : 2 * z / 3 - z / 3 = z / 3 := by ring
    rw [hy, hx, h3]
    field_simp
    ring

theorem example1_allocation {p z : ℝ} (hp : 0 < p) :
    z - p * (2 * z / (3 * p)) = z / 3 := by
  field_simp
  ring

theorem example1_exceeds_cooperative {p z : ℝ} (hp : 0 < p) (hz : 0 < z) :
    z / (2 * p) < 2 * z / (3 * p) := by
  rw [div_lt_div_iff₀ (by positivity) (by positivity)]
  nlinarith

theorem example1_revenue_neutral (z : ℝ) :
    (1 + 1 / 3 : ℝ) / (1 - 1 / 3) = 1 + 1 ∧ (1 / 3) * (z / 2) = (1 / 3) * (z / 2) ∧
      (1 + 1 / 3) * (z / 2) + (1 - 1 / 3) * (z / 2) = z := by
  refine ⟨by norm_num, rfl, by ring⟩

/-! Example 2, p.1098: `G_A(z) = 3z² - 2z³` against the uniform `G_B(z) = z`. -/

def GA (z : ℝ) : ℝ := 3 * z ^ 2 - 2 * z ^ 3

theorem example2_cdf : GA 0 = 0 ∧ GA 1 = 1 ∧ GA (1 / 2) = 1 / 2 ∧
    ∀ z, HasDerivAt GA (6 * z * (1 - z)) z := by
  refine ⟨by norm_num [GA], by norm_num [GA], by norm_num [GA], fun z => ?_⟩
  have h := (((hasDerivAt_pow 2 z).const_mul 3).sub ((hasDerivAt_pow 3 z).const_mul 2))
  convert h using 1
  ring

theorem example2_tax_from_formula {z : ℝ} (h0 : 0 < z) (h1 : z < 1) :
    z * (6 * z * (1 - z)) / (GA z + 0) = 6 * (1 - z) / (3 - 2 * z) := by
  unfold GA
  have : 0 < 3 - 2 * z := by linarith
  have : 3 * z ^ 2 - 2 * z ^ 3 = z ^ 2 * (3 - 2 * z) := by ring
  rw [add_zero, this]
  field_simp
  ring

theorem example2_revenue_neutral_rate {z : ℝ} (h1 : z ≤ 1) :
    (6 * (1 - z) / (3 - 2 * z)) / (2 + 6 * (1 - z) / (3 - 2 * z)) = 3 * (1 - z) / (6 - 5 * z) ∧
      (1 + 3 * (1 - z) / (6 - 5 * z)) / (1 - 3 * (1 - z) / (6 - 5 * z)) = 1 + 6 * (1 - z) / (3 - 2 * z) := by
  have a : 0 < 3 - 2 * z := by linarith
  have b : 0 < 6 - 5 * z := by linarith
  have c : 0 < 3 - 2 * z := by linarith
  constructor
  · have : 2 + 6 * (1 - z) / (3 - 2 * z) = (12 - 10 * z) / (3 - 2 * z) := by
      field_simp
      ring
    rw [this, div_div_div_cancel_right₀ a.ne']
    rw [div_eq_div_iff (by linarith) b.ne']
    ring
  · have d : 1 - 3 * (1 - z) / (6 - 5 * z) = (3 - 2 * z) / (6 - 5 * z) := by
      field_simp
      ring
    rw [d]
    field_simp
    ring

theorem example2_rate_endpoints :
    3 * (1 - (0 : ℝ)) / (6 - 5 * 0) = 1 / 2 ∧ 3 * (1 - (1 : ℝ)) / (6 - 5 * 1) = 0 := by
  constructor <;> norm_num

theorem example2_rate_decreasing {z w : ℝ} (hzw : z < w) (hw : w ≤ 1) :
    3 * (1 - w) / (6 - 5 * w) < 3 * (1 - z) / (6 - 5 * z) := by
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith

theorem example2_integrated_cdfs (z : ℝ) :
    ∫ t in (0 : ℝ)..z, GA t = z ^ 3 - z ^ 4 / 2 ∧ ∫ t in (0 : ℝ)..z, t = z ^ 2 / 2 := by
  constructor
  · unfold GA
    rw [integral_sub, integral_const_mul, integral_const_mul, integral_pow, integral_pow]
    · ring
    · exact (continuous_const.mul (continuous_pow 2)).intervalIntegrable _ _
    · exact (continuous_const.mul (continuous_pow 3)).intervalIntegrable _ _
  · rw [integral_id]
    ring

theorem example2_mean_preserving_spread (z : ℝ) :
    z ^ 3 - z ^ 4 / 2 ≤ z ^ 2 / 2 ∧ ((1 : ℝ) ^ 3 - 1 ^ 4 / 2 = 1 ^ 2 / 2) := by
  constructor
  · have : z ^ 2 / 2 - (z ^ 3 - z ^ 4 / 2) = z ^ 2 * (z - 1) ^ 2 / 2 := by ring
    nlinarith [sq_nonneg (z * (z - 1))]
  · norm_num

theorem example2_ulr {z w : ℝ} :
    (0 ≤ z → z < w → w ≤ 1 / 2 → 6 * z * (1 - z) < 6 * w * (1 - w)) ∧
      (1 / 2 ≤ z → z < w → w ≤ 1 → 6 * w * (1 - w) < 6 * z * (1 - z)) := by
  constructor
  · intro h0 h1 h2
    nlinarith
  · intro h0 h1 h2
    nlinarith

theorem example2_poor_lower_status {z : ℝ} (h0 : 0 < z) (h1 : z < 1 / 2) : GA z < z := by
  unfold GA
  have : z - (3 * z ^ 2 - 2 * z ^ 3) = z * (2 * z - 1) * (z - 1) := by ring
  have h2 : 0 < z * (2 * z - 1) * (z - 1) := by
    apply mul_pos_of_neg_of_neg
    · nlinarith
    · linarith
  linarith

theorem example2_rich_higher_status {z : ℝ} (h0 : 1 / 2 < z) (h1 : z < 1) : z < GA z := by
  unfold GA
  have : (3 * z ^ 2 - 2 * z ^ 3) - z = z * (2 * z - 1) * (1 - z) := by ring
  have h2 : 0 < z * (2 * z - 1) * (1 - z) := by
    apply mul_pos
    · apply mul_pos <;> linarith
    · linarith
  linarith

end HopkinsKornienko2004
