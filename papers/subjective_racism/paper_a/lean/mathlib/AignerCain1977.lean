import Mathlib

/-! Aigner and Cain (1977), the structure Paper A1 relies on or sets against, in
the paper's notation. Productivity `q` is normal with group mean `α` and
variance `Vq = Var(q)`; the test score is `y = q + u` with `Vu = Var(u)`
(eq. 1). The reliability is `γ = Var(q)/(Var(q) + Var(u))` (eq. 3), the
prediction is `q̂ = (1 − γ) α + γ y` (eq. 2), and the conditional variance is
`Var(q|y) = Var(q)(1 − γ)` (footnote 15). The employer's utility is
`U(q) = a − b e^{−c q}` (eq. 7), whose expectation under a normal belief is
eq. (8), and the risk factor is `R = k Var(q|y)` with `k = c/2` (p. 181).

The last section is a contrast and not an attribution. It sets the
mean–variance form of eq. (8) beside Paper A1's quadratic loss in class
distance, `v − κ (c_i − x)²`. -/

namespace AignerCain1977

open MeasureTheory ProbabilityTheory Real

/-- Eq. (3). The reliability `γ`. -/
noncomputable def gamma (Vq Vu : ℝ) : ℝ := Vq / (Vq + Vu)

/-- Eq. (2). The predicted productivity given the test score `y`. -/
noncomputable def qhat (α γ y : ℝ) : ℝ := (1 - γ) * α + γ * y

/-- Footnote 15. The conditional variance `Var(q|y) = Var(q)(1 − γ)`. -/
noncomputable def condVar (Vq γ : ℝ) : ℝ := Vq * (1 - γ)

/-- The risk-discounted pay `E(q|y) − k Var(q|y)` (p. 181). -/
noncomputable def pay (k α Vq Vu y : ℝ) : ℝ :=
  qhat α (gamma Vq Vu) y - k * condVar Vq (gamma Vq Vu)

/-- Eq. (3). `0 < γ < 1` when both variances are positive. -/
theorem gamma_mem {Vq Vu : ℝ} (hq : 0 < Vq) (hu : 0 < Vu) :
    0 < gamma Vq Vu ∧ gamma Vq Vu < 1 := by
  unfold gamma
  have h : 0 < Vq + Vu := by linarith
  exact ⟨div_pos hq h, (div_lt_one h).2 (by linarith)⟩

/-- Footnote 18. The risk discount is symmetric in `Var(q)` and `Var(u)`,
`Var(q|y) = Var(q) Var(u)/(Var(q) + Var(u))`. -/
theorem condVar_symm {Vq Vu : ℝ} (h : Vq + Vu ≠ 0) :
    condVar Vq (gamma Vq Vu) = Vq * Vu / (Vq + Vu) := by
  unfold condVar gamma; field_simp; ring

/-- Footnote 17 and eq. (8). If `q` is normal with mean `m` and variance `v`,
the expected utility of `U(q) = a − b e^{−c q}` is `a − b e^{−c m + (c²/2) v}`. -/
theorem eq8 (a b c m : ℝ) (v : NNReal) :
    ∫ q, (a - b * exp (-c * q)) ∂(gaussianReal m v)
      = a - b * exp (-c * m + c ^ 2 / 2 * v) := by
  have hi : Integrable (fun q => exp (-c * q)) (gaussianReal m v) :=
    integrable_exp_mul_gaussianReal (-c)
  rw [integral_sub (integrable_const a) (hi.const_mul b), integral_const,
    integral_const_mul]
  have hm : ∫ q, exp (-c * q) ∂(gaussianReal m v) = exp (m * (-c) + v * (-c) ^ 2 / 2) := by
    have := congrFun (mgf_id_gaussianReal (μ := m) (v := v)) (-c)
    simpa [mgf] using this
  rw [hm, measureReal_univ_eq_one, smul_eq_mul, one_mul]
  congr 2
  ring_nf

/-- P. 181. Maximising eq. (8) is maximising `E(q|y) − k Var(q|y)` with
`k = c/2`, for `b, c > 0`. -/
theorem eq8_order {a b c m₁ v₁ m₂ v₂ : ℝ} (hb : 0 < b) (hc : 0 < c) :
    a - b * exp (-c * m₁ + c ^ 2 / 2 * v₁) < a - b * exp (-c * m₂ + c ^ 2 / 2 * v₂)
      ↔ m₁ - c / 2 * v₁ < m₂ - c / 2 * v₂ := by
  have e₁ : -c * m₁ + c ^ 2 / 2 * v₁ = -(c * (m₁ - c / 2 * v₁)) := by ring
  have e₂ : -c * m₂ + c ^ 2 / 2 * v₂ = -(c * (m₂ - c / 2 * v₂)) := by ring
  rw [e₁, e₂, sub_lt_sub_iff_left, mul_lt_mul_iff_of_pos_left hb, exp_lt_exp, neg_lt_neg_iff,
    mul_lt_mul_iff_of_pos_left hc]

/-- The risk discount rises with the measurement noise. A test less reliable
for one group gives that group the larger `Var(q|y)`. -/
theorem condVar_strictMono_Vu {Vq Vu₁ Vu₂ : ℝ} (hq : 0 < Vq) (h1 : 0 ≤ Vu₁)
    (h12 : Vu₁ < Vu₂) :
    condVar Vq (gamma Vq Vu₁) < condVar Vq (gamma Vq Vu₂) := by
  rw [condVar_symm (by linarith), condVar_symm (by linarith),
    div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith [mul_pos (mul_pos hq hq) (sub_pos.2 h12)]

/-- Figure 2. With equal means and equal `Var(q)`, at `y = α` the group whose
test is less reliable (`Var(u^B) > Var(u^W)`) is paid less, entirely because
of its larger risk factor. -/
theorem fig2_black_paid_less {k α Vq VuW VuB : ℝ} (hk : 0 < k) (hq : 0 < Vq)
    (hW : 0 ≤ VuW) (hWB : VuW < VuB) :
    pay k α Vq VuB α < pay k α Vq VuW α := by
  have h := condVar_strictMono_Vu hq hW hWB
  unfold pay qhat
  nlinarith

/-- With zero reliability (`γ = 0`) the test says nothing about the individual,
the prediction is the group mean and the risk factor is `k Var(q)`, the
group's own variance. -/
theorem zero_reliability (α Vq y : ℝ) :
    qhat α 0 y = α ∧ condVar Vq 0 = Vq := by
  unfold qhat condVar; constructor <;> ring

/-- Control: without risk aversion there is no discount. With `k = 0` the two
groups are paid the same at `y = α`, whatever the noise of their tests. -/
theorem control_risk_neutral (α Vq VuW VuB : ℝ) :
    pay 0 α Vq VuB α = pay 0 α Vq VuW α := by
  unfold pay qhat; ring

/-- Control: without spread in productivity there is no discount. With
`Var(q) = 0` the risk factor is zero whatever the noise of the test. -/
theorem control_no_spread (k α Vu y : ℝ) :
    pay k α 0 Vu y = qhat α (gamma 0 Vu) y := by
  unfold pay condVar; ring

/-! ### Contrast with Paper A1's quadratic loss

A finite belief is a list of weights `w` on class values `x`, with
`Σ w = 1`. Under Paper A1's engagement utility `v − κ (c − x)²`, the expected
utility is `v − κ ((x̄ − c)² + σ²)`. The variance enters as the risk premium
of a concave utility, as in eq. (8), but exactly and for any distribution,
and the mean enters through the gap to the observer's own class `c`. -/

variable {n : ℕ}

/-- The mean of a finite belief. -/
def mean (w x : Fin n → ℝ) : ℝ := ∑ i, w i * x i

/-- The variance of a finite belief. -/
def var (w x : Fin n → ℝ) : ℝ := ∑ i, w i * (x i - mean w x) ^ 2

/-- Contrast. Expected quadratic utility is the utility at the mean less
`κ σ²`, for any finite belief. -/
theorem contrast_quadratic_ce (w x : Fin n → ℝ) (hw : ∑ i, w i = 1) (v κ c : ℝ) :
    ∑ i, w i * (v - κ * (c - x i) ^ 2)
      = v - κ * ((mean w x - c) ^ 2 + var w x) := by
  unfold var
  generalize hm : mean w x = m
  have hS : ∑ i, w i * x i = m := by rw [← hm]; rfl
  have e1 : ∀ i, w i * (v - κ * (c - x i) ^ 2)
      = (v - κ * c ^ 2) * w i + (2 * κ * c) * (w i * x i) - κ * (w i * x i ^ 2) := by
    intro i; ring
  have e2 : ∀ i, w i * (x i - m) ^ 2 = m ^ 2 * w i - 2 * m * (w i * x i) + w i * x i ^ 2 := by
    intro i; ring
  rw [Finset.sum_congr rfl (fun i _ => e1 i), Finset.sum_congr rfl (fun i _ => e2 i)]
  simp only [Finset.sum_add_distrib, Finset.sum_sub_distrib, ← Finset.mul_sum, hw, hS]
  ring

/-- Contrast. Under the mean–variance form of eq. (8), two groups with the same
variance are ranked by their means alike by every employer, whatever `k`. -/
theorem contrast_cara_common_ranking {k m₁ m₂ V : ℝ} :
    m₂ - k * V < m₁ - k * V ↔ m₂ < m₁ := by
  constructor <;> intro h <;> linarith

/-- Contrast. Under the quadratic loss, two groups with the same variance and
means `0 < 1` are ranked oppositely by observers of class `0` and class `1`. -/
theorem contrast_quadratic_ranking_depends_on_own_class (v κ σ2 : ℝ) (hκ : 0 < κ) :
    (v - κ * ((1 - 0) ^ 2 + σ2) < v - κ * ((0 - 0) ^ 2 + σ2)) ∧
      (v - κ * ((0 - 1) ^ 2 + σ2) < v - κ * ((1 - 1) ^ 2 + σ2)) := by
  constructor <;> nlinarith

end AignerCain1977
