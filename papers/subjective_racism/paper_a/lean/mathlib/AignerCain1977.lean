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

/-! ### The rest of the paper's model

A group is the triple `(α, Var(q), Var(u))` of eqs. (1), (5a) and (5b). The
solution concept is competitive pay at expected productivity, eq. (2) and p. 4,
or at risk-discounted productivity, p. 181, with beliefs about `α` that equal
the actual mean (p. 4). -/

/-- A group of workers, eqs. (1), (5a), (5b). -/
structure Group where
  α : ℝ
  Vq : ℝ
  Vu : ℝ

/-- The group's reliability, eq. (3). -/
noncomputable def Group.γ (G : Group) : ℝ := gamma G.Vq G.Vu

/-- Pay in the Phelps model, eqs. (5a) and (5b). -/
noncomputable def Group.wage (G : Group) (y : ℝ) : ℝ := qhat G.α G.γ y

/-- P. 4. Employers' subjective group mean equals the actual one. -/
def ConsistentBeliefs (αSubj : ℝ) (G : Group) : Prop := αSubj = G.α

/-- P. 181. The employer prefers group `G₁` to `G₂` at score `y`. -/
def Prefers (k : ℝ) (G₁ G₂ : Group) (y : ℝ) : Prop :=
  pay k G₂.α G₂.Vq G₂.Vu y < pay k G₁.α G₁.Vq G₁.Vu y

/-- Eq. (3). With `u` independent of `q`, `Cov(q, y) = Var(q)` and
`Var(y) = Var(q) + Var(u)`, so `γ = Cov(q,y)/Var(y) = Cov(q,y)²/(Var(q)Var(y))`. -/
theorem eq3_cov {Vq Vu : ℝ} (hq : Vq ≠ 0) (hy : Vq + Vu ≠ 0) :
    gamma Vq Vu = Vq / (Vq + Vu) ∧ gamma Vq Vu = Vq ^ 2 / (Vq * (Vq + Vu)) := by
  refine ⟨rfl, ?_⟩
  unfold gamma; field_simp

/-- Eq. (2) as a least-squares regression, p. 3. Among slopes `b` on the centred
score, the mean squared error `Var(q) − 2 b Cov(q,y) + b² Var(y)` is least at
`b = γ = Cov(q,y)/Var(y)`. -/
theorem eq2_least_squares {Vq Vu : ℝ} (hy : 0 < Vq + Vu) (b : ℝ) :
    Vq - 2 * gamma Vq Vu * Vq + gamma Vq Vu ^ 2 * (Vq + Vu)
      ≤ Vq - 2 * b * Vq + b ^ 2 * (Vq + Vu) := by
  have hd : (Vq + Vu) ≠ 0 := hy.ne'
  have e : Vq - 2 * b * Vq + b ^ 2 * (Vq + Vu)
      - (Vq - 2 * gamma Vq Vu * Vq + gamma Vq Vu ^ 2 * (Vq + Vu))
      = (Vq + Vu) * (b - gamma Vq Vu) ^ 2 := by
    unfold gamma; field_simp; ring
  nlinarith [mul_nonneg hy.le (sq_nonneg (b - gamma Vq Vu))]

/-- Footnote 15. `Var(q|y) = Var(q) − γ² Var(y) = Var(q)(1 − γ)`. -/
theorem fn15 {Vq Vu : ℝ} (hy : Vq + Vu ≠ 0) :
    Vq - gamma Vq Vu ^ 2 * (Vq + Vu) = condVar Vq (gamma Vq Vu) := by
  unfold condVar gamma; field_simp

/-- Eq. (6). With `E(y|q) = q`, `E(q̂|q) = (1 − γ) α + γ q`, which is eq. (2)
evaluated at `y = q`. -/
theorem eq6 (α γ q : ℝ) : qhat α γ q = (1 - γ) * α + γ * q := rfl

/-- Pp. 6 and 7. Each group's mean pay is its mean ability, since `E(y) = α`:
groups with equal means receive equal mean wages, in Figs. 1A and 1B. -/
theorem mean_wage (α γ : ℝ) : qhat α γ α = α := by unfold qhat; ring

/-- Fig. 1A. With equal `Var(u)`, a larger `Var(q^B)` gives blacks the steeper
slope, `γ^B > γ^W`. -/
theorem fig1A_slope {VqW VqB Vu : ℝ} (hu : 0 < Vu) (hW : 0 ≤ VqW) (hWB : VqW < VqB) :
    gamma VqW Vu < gamma VqB Vu := by
  unfold gamma
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith

/-- Figs. 1A and 1B. With equal means, the group with the steeper slope is
predicted higher exactly above the mean score and lower below it. -/
theorem fig1_crossing {α γ₁ γ₂ : ℝ} (h : γ₂ < γ₁) (y : ℝ) :
    qhat α γ₂ y < qhat α γ₁ y ↔ α < y := by
  unfold qhat
  constructor <;> intro h' <;> nlinarith

/-- Control for Figs. 1A and 1B: without a difference in slopes the crossing
fails, since at any score above the mean neither line lies above the other. -/
theorem control_fig1_equal_slopes (α γ : ℝ) :
    ¬ ∀ y, (qhat α γ y < qhat α γ y ↔ α < y) := by
  intro h
  have := (h (α + 1)).2 (by linarith)
  exact lt_irrefl _ this

/-- Fig. 2. With equal means, a flatter line for blacks (`γ^B < γ^W`) and a
larger risk discount for blacks, black pay exceeds white pay exactly below a
score `y₀ < α`. -/
theorem fig2_crossing {k α γB γW VB VW : ℝ} (hk : 0 < k) (hγ : γB < γW) (hV : VW < VB) :
    ∃ y₀ < α, ∀ y, (qhat α γW y - k * VW < qhat α γB y - k * VB ↔ y < y₀) := by
  have hg : 0 < γW - γB := by linarith
  refine ⟨α - k * (VB - VW) / (γW - γB), ?_, fun y => ?_⟩
  · have : 0 < k * (VB - VW) / (γW - γB) := div_pos (by nlinarith) hg
    linarith
  · unfold qhat
    have key : α - k * (VB - VW) / (γW - γB) = (α * (γW - γB) - k * (VB - VW)) / (γW - γB) := by
      field_simp
    rw [key, lt_div_iff₀ hg]
    constructor <;> intro h <;> nlinarith

/-- Control for Fig. 2: without risk aversion the lines cross at the mean
score, not below it. -/
theorem control_fig2_no_discount {α γB γW VB VW : ℝ} (hγ : γB < γW) (y : ℝ) :
    qhat α γW y - 0 * VW < qhat α γB y - 0 * VB ↔ y < α := by
  unfold qhat
  constructor <;> intro h <;> nlinarith

/-- Fig. 4, the extreme selection–truncation case. With zero reliability no
black worker's prediction exceeds the floor `α`, and with perfect reliability a
white worker's does exactly when `y > α`. -/
theorem fig4_truncation (α y : ℝ) :
    ¬ (α < qhat α 0 y) ∧ (α < qhat α 1 y ↔ α < y) := by
  unfold qhat; constructor
  · simp
  · simp

/-- Control for Fig. 4: with perfect reliability a black worker scoring above
the floor is hired, so the exclusion needs the zero reliability. -/
theorem control_fig4_reliable (α : ℝ) : α < qhat α 1 (α + 1) := by
  unfold qhat; linarith

/-- Fig. 5 and eq. (6). With equal slopes `γ < 1` and `α^B < α^W`, the black line
lies below the white one by `(1 − γ)(α^W − α^B)` at every score, and at every
true ability. -/
theorem fig5_parallel {αB αW γ : ℝ} (hγ : γ < 1) (hα : αB < αW) (y : ℝ) :
    qhat αW γ y - qhat αB γ y = (1 - γ) * (αW - αB) ∧ qhat αB γ y < qhat αW γ y := by
  unfold qhat
  constructor
  · ring
  · nlinarith

/-- P. 12 (AC-Q13). With perfect information, `γ = 1`, the two lines coincide
with the 45° line and the conditional variance, hence the risk discount, is
zero. -/
theorem perfect_information (αB αW Vq y : ℝ) :
    qhat αB 1 y = y ∧ qhat αW 1 y = y ∧ condVar Vq 1 = 0 := by
  unfold qhat condVar; refine ⟨by ring, by ring, by ring⟩

/-- Control for Fig. 5: with `γ = 1` the gap of Fig. 5 is zero, so the result
needs imperfect information. -/
theorem control_fig5_perfect_information (αB αW y : ℝ) :
    qhat αW 1 y - qhat αB 1 y = 0 := by unfold qhat; ring

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
