import ClosureMap

namespace Bifurcation

open ClosureMap

/-- The mobility threshold of the canonical map, `γ* = b - 2 √(μ b)`. -/
noncomputable def gammaStar (μ b : ℝ) : ℝ := b - 2 * Real.sqrt (μ * b)

/-- The threshold on `b = s δ`, `b* = (√μ + √(μ + γ))²`. -/
noncomputable def bStar (μ γ : ℝ) : ℝ := (Real.sqrt μ + Real.sqrt (μ + γ)) ^ 2

theorem gammaStar_eq {μ b : ℝ} (hμ : 0 ≤ μ) (hb : 0 ≤ b) :
    gammaStar μ b = (Real.sqrt b - Real.sqrt μ) ^ 2 - μ := by
  unfold gammaStar
  rw [Real.sqrt_mul hμ b]
  have h1 := Real.sq_sqrt hμ
  have h2 := Real.sq_sqrt hb
  nlinarith [h1, h2]

/-! ### Row M9. The bifurcation in mobility -/

/-- Row M9 (i). The trap condition of M5 holds exactly below the threshold. -/
theorem trap_iff_gamma {μ γ b : ℝ} (hμ : 0 < μ) (hb : 0 < b) :
    (0 < b - γ ∧ 4 * μ * b < (b - γ) ^ 2) ↔ γ < gammaStar μ b := by
  unfold gammaStar
  have hr0 : 0 ≤ Real.sqrt (μ * b) := Real.sqrt_nonneg _
  have hr2 : Real.sqrt (μ * b) ^ 2 = μ * b := Real.sq_sqrt (by positivity)
  constructor
  · rintro ⟨hB, hD⟩
    by_contra h
    push_neg at h
    have h' : b - γ ≤ 2 * Real.sqrt (μ * b) := by linarith
    nlinarith [mul_le_mul h' h' hB.le (by positivity)]
  · intro h
    have hB : 2 * Real.sqrt (μ * b) < b - γ := by linarith
    refine ⟨by linarith, ?_⟩
    nlinarith [mul_lt_mul'' hB hB (by positivity) (by positivity)]

/-- Row M9 (ii). Above the threshold, `Q` is positive on `[0, ∞)`. -/
theorem Q_pos_above {μ γ b c : ℝ} (hμ : 0 < μ) (hb : 0 < b)
    (hγ : gammaStar μ b < γ) (hc : 0 ≤ c) : 0 < Q μ γ b c := by
  rw [gammaStar_eq hμ.le hb.le] at hγ
  have hu := Real.sq_sqrt hb.le
  have hv := Real.sq_sqrt hμ.le
  have hu0 := Real.sqrt_nonneg b
  have hv0 := Real.sqrt_nonneg μ
  have hid : Q μ γ b c = (Real.sqrt b * c - Real.sqrt μ) ^ 2
      + (2 * Real.sqrt b * Real.sqrt μ - b + γ) * c := by
    unfold Q; nlinarith [hu, hv]
  have hcoef : 0 < 2 * Real.sqrt b * Real.sqrt μ - b + γ := by nlinarith [hu, hv]
  rw [hid]
  rcases eq_or_lt_of_le hc with h | h
  · rw [← h]; nlinarith [hv, Real.sqrt_pos.2 hμ]
  · have := mul_pos hcoef h
    nlinarith [sq_nonneg (Real.sqrt b * c - Real.sqrt μ)]

/-- Row M9 (ii). Above the threshold, integration is the only fixed point in `[0, 1]`. -/
theorem only_one_above {μ γ b c : ℝ} (hμ : 0 < μ) (hb : 0 < b)
    (hγ : gammaStar μ b < γ) (h0 : 0 ≤ c) (h1 : c ≤ 1) :
    H μ γ b c = c ↔ c = 1 := by
  constructor
  · intro hfix
    by_contra hne
    have := (fixed_iff_Q hne).1 hfix
    linarith [Q_pos_above hμ hb hγ h0]
  · rintro rfl; exact H_one μ γ b

/-- Row M9 (iii). At the threshold the two interior fixed points merge into a
double root at `√μ / √b`. -/
theorem at_threshold_factor {μ b c : ℝ} (hμ : 0 < μ) (hb : 0 < b) :
    H μ (gammaStar μ b) b c - c = (1 - c) * (Real.sqrt b * c - Real.sqrt μ) ^ 2 := by
  rw [H_sub_self, gammaStar_eq hμ.le hb.le]
  have hu := Real.sq_sqrt hb.le
  have hv := Real.sq_sqrt hμ.le
  unfold Q
  linear_combination (1 - c) * ((c - c ^ 2) * hu + (c - 1) * hv)

theorem at_threshold_fixed {μ b c : ℝ} (hμ : 0 < μ) (hb : 0 < b) :
    H μ (gammaStar μ b) b c = c ↔ c = Real.sqrt μ / Real.sqrt b ∨ c = 1 := by
  have hf := at_threshold_factor (c := c) hμ hb
  have hu : 0 < Real.sqrt b := Real.sqrt_pos.2 hb
  constructor
  · intro h
    have : (1 - c) * (Real.sqrt b * c - Real.sqrt μ) ^ 2 = 0 := by linarith
    rcases mul_eq_zero.1 this with h1 | h1
    · right; linarith
    · left
      have := pow_eq_zero_iff (n := 2) (by norm_num) |>.1 h1
      field_simp
      linarith
  · rintro (h | h)
    · have : Real.sqrt b * c - Real.sqrt μ = 0 := by
        rw [h]; field_simp; ring
      rw [this] at hf; linarith
    · rw [h]; exact H_one μ _ b

/-- The double root lies in `(0, 1)` when `μ < b`, which holds whenever `γ* ≥ 0`. -/
theorem double_root_mem {μ b : ℝ} (hμ : 0 < μ) (hb : μ < b) :
    0 < Real.sqrt μ / Real.sqrt b ∧ Real.sqrt μ / Real.sqrt b < 1 := by
  have hu : 0 < Real.sqrt b := Real.sqrt_pos.2 (by linarith)
  have hv : 0 < Real.sqrt μ := Real.sqrt_pos.2 hμ
  refine ⟨div_pos hv hu, (div_lt_one hu).2 (Real.sqrt_lt_sqrt hμ.le hb)⟩

/-- The threshold is positive exactly when `b > 4 μ`. -/
theorem gammaStar_pos_iff {μ b : ℝ} (hμ : 0 < μ) (hb : 0 < b) :
    0 < gammaStar μ b ↔ 4 * μ < b := by
  rw [gammaStar_eq hμ.le hb.le]
  have hu := Real.sq_sqrt hb.le
  have hv := Real.sq_sqrt hμ.le
  have hu0 := Real.sqrt_nonneg b
  have hv0 : 0 < Real.sqrt μ := Real.sqrt_pos.2 hμ
  constructor
  · intro h
    -- (u - v)² > v² forces u > 2 v, since u ≥ 0
    by_contra h'
    push_neg at h'
    have hle : Real.sqrt b ≤ 2 * Real.sqrt μ := by
      rw [show 2 * Real.sqrt μ = Real.sqrt (4 * μ) by
        rw [Real.sqrt_mul (by norm_num), show (4 : ℝ) = 2 ^ 2 by norm_num,
          Real.sqrt_sq (by norm_num)]]
      exact Real.sqrt_le_sqrt h'
    nlinarith [mul_nonneg hu0 (by linarith : 0 ≤ 2 * Real.sqrt μ - Real.sqrt b)]
  · intro h
    have hlt : 2 * Real.sqrt μ < Real.sqrt b := by
      rw [show 2 * Real.sqrt μ = Real.sqrt (4 * μ) by
        rw [Real.sqrt_mul (by norm_num), show (4 : ℝ) = 2 ^ 2 by norm_num,
          Real.sqrt_sq (by norm_num)]]
      exact Real.sqrt_lt_sqrt (by linarith) h
    nlinarith

/-- Control: the threshold need not be positive. At `μ = 1/20`, `b = 1/5`,
`γ* = 0`, so no `γ ≥ 0` gives the trap. -/
theorem control_threshold_zero : gammaStar (1 / 20) (1 / 5) = 0 := by
  unfold gammaStar
  rw [show (1 / 20 : ℝ) * (1 / 5) = (1 / 10) ^ 2 by norm_num,
    Real.sqrt_sq (by norm_num)]
  norm_num

/-! ### Row M10. The threshold in the cost of exclusion and in salience -/

/-- Row M10. With `γ ≥ 0`, the trap condition holds exactly when `b > b*`. -/
theorem trap_iff_b {μ γ b : ℝ} (hμ : 0 < μ) (hγ : 0 ≤ γ) (hb : 0 < b) :
    γ < gammaStar μ b ↔ bStar μ γ < b := by
  rw [gammaStar_eq hμ.le hb.le]
  unfold bStar
  have hu := Real.sq_sqrt hb.le
  have hv := Real.sq_sqrt hμ.le
  have hw := Real.sq_sqrt (by linarith : 0 ≤ μ + γ)
  have hu0 : 0 < Real.sqrt b := Real.sqrt_pos.2 hb
  have hv0 := Real.sqrt_nonneg μ
  have hw0 := Real.sqrt_nonneg (μ + γ)
  have hvw : Real.sqrt μ ≤ Real.sqrt (μ + γ) := Real.sqrt_le_sqrt (by linarith)
  constructor
  · intro h
    have hgt : Real.sqrt μ + Real.sqrt (μ + γ) < Real.sqrt b := by
      by_contra h'
      push_neg at h'
      have h1 : Real.sqrt b - Real.sqrt μ ≤ Real.sqrt (μ + γ) := by linarith
      have h2 : -Real.sqrt (μ + γ) ≤ Real.sqrt b - Real.sqrt μ := by linarith
      nlinarith [mul_nonneg (by linarith : 0 ≤ Real.sqrt (μ + γ) - (Real.sqrt b - Real.sqrt μ))
        (by linarith : 0 ≤ Real.sqrt (μ + γ) + (Real.sqrt b - Real.sqrt μ))]
    nlinarith [mul_lt_mul'' hgt hgt (by positivity) (by positivity)]
  · intro h
    have hgt : Real.sqrt μ + Real.sqrt (μ + γ) < Real.sqrt b := by
      by_contra h'
      push_neg at h'
      nlinarith [mul_le_mul h' h' hu0.le (by positivity)]
    have h1 : Real.sqrt (μ + γ) < Real.sqrt b - Real.sqrt μ := by linarith
    nlinarith [mul_lt_mul'' h1 h1 hw0 hw0]

/-- Row M10. The threshold rises with `b` from `b = μ` on, so with salience at a
fixed positive cost of exclusion. -/
theorem gammaStar_strictMono {μ b₁ b₂ : ℝ} (hμ : 0 ≤ μ) (h1 : μ ≤ b₁) (h12 : b₁ < b₂) :
    gammaStar μ b₁ < gammaStar μ b₂ := by
  rw [gammaStar_eq hμ (by linarith), gammaStar_eq hμ (by linarith)]
  have hs1 : Real.sqrt μ ≤ Real.sqrt b₁ := Real.sqrt_le_sqrt h1
  have hs2 : Real.sqrt b₁ < Real.sqrt b₂ := Real.sqrt_lt_sqrt (by linarith) h12
  nlinarith

theorem gammaStar_strictMono_salience {μ δ s₁ s₂ : ℝ} (hμ : 0 ≤ μ) (hδ : 0 < δ)
    (h1 : μ ≤ s₁ * δ) (h12 : s₁ < s₂) :
    gammaStar μ (s₁ * δ) < gammaStar μ (s₂ * δ) :=
  gammaStar_strictMono hμ h1 (mul_lt_mul_of_pos_right h12 hδ)

/-- Row M10. Wherever the threshold is positive it rises with `b`, since a
positive threshold needs `b > 4 μ ≥ μ`. -/
theorem gammaStar_rises_where_positive {μ b₁ b₂ : ℝ} (hμ : 0 < μ) (hb : 0 < b₁)
    (hpos : 0 < gammaStar μ b₁) (h12 : b₁ < b₂) :
    gammaStar μ b₁ < gammaStar μ b₂ := by
  have h4 := (gammaStar_pos_iff hμ hb).1 hpos
  exact gammaStar_strictMono hμ.le (by linarith) h12

/-- At zero mobility the exclusion threshold is `4 μ`. -/
theorem bStar_zero_mobility {μ : ℝ} (hμ : 0 ≤ μ) : bStar μ 0 = 4 * μ := by
  unfold bStar
  rw [add_zero, ← two_mul, mul_pow, Real.sq_sqrt hμ]
  ring

/-- The predecessor's bifurcation figure, `μ = γ = 1/20`, `s = 1`, reports
`δ* ≈ 0.29`. In closed form `0.29 < b* < 0.30`. -/
theorem bStar_at_figure : 29 / 100 < bStar (1 / 20) (1 / 20) ∧ bStar (1 / 20) (1 / 20) < 3 / 10 := by
  unfold bStar
  have h1 := Real.sq_sqrt (by norm_num : (0 : ℝ) ≤ 1 / 20)
  have h2 := Real.sq_sqrt (by norm_num : (0 : ℝ) ≤ 1 / 20 + 1 / 20)
  have h10 := Real.sqrt_nonneg (1 / 20 : ℝ)
  have h20 := Real.sqrt_nonneg (1 / 20 + 1 / 20 : ℝ)
  set x := Real.sqrt (1 / 20 : ℝ)
  set y := Real.sqrt (1 / 20 + 1 / 20 : ℝ)
  have hxy : 0 ≤ x * y := mul_nonneg h10 h20
  have hsq : (x * y) ^ 2 = 1 / 200 := by rw [mul_pow, h1, h2]; norm_num
  have hlo : 7 / 100 < x * y := by nlinarith
  have hhi : x * y < 3 / 40 := by nlinarith
  constructor <;> nlinarith

/-- Control: below `b = μ` the threshold is not increasing. At `μ = 1`,
`γ*(1/4) = -3/4 < 0 = γ*(0)`. -/
theorem control_not_mono_below_mu : gammaStar 1 (1 / 4) < gammaStar 1 0 := by
  unfold gammaStar
  rw [show (1 : ℝ) * (1 / 4) = (1 / 2) ^ 2 by norm_num, Real.sqrt_sq (by norm_num)]
  norm_num

/-! ### The slope at the merged root -/

/-- The derivative of the canonical map. -/
theorem H_hasDerivAt (μ γ b c : ℝ) :
    HasDerivAt (H μ γ b)
      (1 - μ + γ - b * (1 - c) ^ 2 + 2 * b * c * (1 - c) - 2 * γ * c) c := by
  have hid := hasDerivAt_id c
  have h1 : HasDerivAt (fun x : ℝ => 1 - x) (-1) c := by
    simpa using hid.const_sub 1
  have h2 := h1.pow 2
  have h3 := h2.const_mul b
  have h4 := h3.const_sub (1 - μ + γ)
  have h5 := hid.mul h4
  have h6 := (hid.pow 2).const_mul γ
  have h7 := (h5.const_add μ).sub h6
  convert h7 using 1
  simp only [Pi.pow_apply, id_eq, Nat.cast_ofNat, Nat.reduceSub, pow_one]
  ring

/-- At the threshold the slope of the map at the merged root is 1, the
saddle-node condition. -/
theorem slope_one_at_threshold {μ b : ℝ} (hμ : 0 < μ) (hb : 0 < b) :
    HasDerivAt (H μ (gammaStar μ b) b) 1 (Real.sqrt μ / Real.sqrt b) := by
  have h := H_hasDerivAt μ (gammaStar μ b) b (Real.sqrt μ / Real.sqrt b)
  convert h using 1
  rw [gammaStar_eq hμ.le hb.le]
  have hu0 : Real.sqrt b ≠ 0 := (Real.sqrt_pos.2 hb).ne'
  set u := Real.sqrt b with hu_def
  set v := Real.sqrt μ with hv_def
  have hu : u ^ 2 = b := Real.sq_sqrt hb.le
  have hv : v ^ 2 = μ := Real.sq_sqrt hμ.le
  have hc : u * (v / u) = v := by field_simp
  clear_value u v
  subst hu hv
  linear_combination (3 * u * (v / u) - 2 * u - v) * hc

/-- Control: away from the threshold the slope at `√μ / √b` is not 1. At
`μ = 1/20`, `b = 4/5`, where `√μ / √b = 1/4` and `γ* = 2/5`, the slope at
`1/4` under `γ = 1/10` is `17/20`. -/
theorem control_slope_off_threshold :
    HasDerivAt (H (1 / 20) (1 / 10) (4 / 5)) (17 / 20) (1 / 4) := by
  have h := H_hasDerivAt (1 / 20) (1 / 10) (4 / 5) (1 / 4)
  convert h using 1
  norm_num

theorem gammaStar_example : gammaStar (1 / 20) (4 / 5) = 2 / 5 := by
  unfold gammaStar
  rw [show (1 / 20 : ℝ) * (4 / 5) = (1 / 5) ^ 2 by norm_num, Real.sqrt_sq (by norm_num)]
  norm_num

end Bifurcation
