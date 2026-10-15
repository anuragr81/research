import ClosureMap

open Filter Topology

namespace FrozenMap

open ClosureMap

/-- The composed map with the group prior frozen at its first value. The
discrimination rate is then the constant `k = s δ F β₀`. -/
def Hk (μ γ k c : ℝ) : ℝ :=
  μ + c * (1 - μ + γ - k) - γ * c ^ 2

/-- The fixed-point polynomial of the frozen map, `Hk c - c = - P c`. -/
def P (μ γ k c : ℝ) : ℝ :=
  γ * c ^ 2 + (μ + k - γ) * c - μ

/-! ### Row M7. One fixed point, set by the frozen belief -/

/-- Freezing the prior at `β₀` turns `Hf` into `Hk` with `k = s δ F β₀`. -/
theorem Hf_frozen (F : ℝ → ℝ) (β₀ μ γ δ s c : ℝ) :
    Hf (fun _ => F β₀) μ γ δ s c = Hk μ γ (s * δ * F β₀) c := by
  unfold Hf Hk; ring

theorem Hk_sub_self (μ γ k c : ℝ) : Hk μ γ k c - c = - P μ γ k c := by
  unfold Hk P; ring

theorem fixed_iff_P {μ γ k c : ℝ} : Hk μ γ k c = c ↔ P μ γ k c = 0 := by
  have h := Hk_sub_self μ γ k c
  constructor
  · intro hc; linarith
  · intro hc; linarith

/-- The cross identity behind uniqueness and the sign of `P`. -/
theorem P_cross (μ γ k x y : ℝ) :
    y * P μ γ k x - x * P μ γ k y = (x - y) * (γ * x * y + μ) := by
  unfold P; ring

/-- With `μ > 0` and `γ ≥ 0`, `P` has at most one root in `[0, ∞)`. -/
theorem fixed_unique {μ γ k x y : ℝ} (hμ : 0 < μ) (hγ : 0 ≤ γ)
    (hx : 0 ≤ x) (hy : 0 ≤ y) (hPx : P μ γ k x = 0) (hPy : P μ γ k y = 0) :
    x = y := by
  have h := P_cross μ γ k x y
  rw [hPx, hPy] at h
  have hpos : 0 < γ * x * y + μ := by positivity
  have : (x - y) * (γ * x * y + μ) = 0 := by linarith
  rcases mul_eq_zero.1 this with h1 | h1
  · linarith
  · linarith

/-- With `μ > 0` and `k > 0`, `P` has a root strictly inside `(0, 1)`. -/
theorem exists_fixed {μ γ k : ℝ} (hμ : 0 < μ) (hk : 0 < k) :
    ∃ c, 0 < c ∧ c < 1 ∧ P μ γ k c = 0 := by
  have hcont : ContinuousOn (P μ γ k) (Set.Icc 0 1) := by
    unfold P; fun_prop
  have h0 : P μ γ k 0 = -μ := by unfold P; ring
  have h1 : P μ γ k 1 = k := by unfold P; ring
  have hmem : (0 : ℝ) ∈ Set.Ioo (P μ γ k 0) (P μ γ k 1) := by
    rw [h0, h1]; exact ⟨by linarith, hk⟩
  obtain ⟨c, ⟨hc0, hc1⟩, hc⟩ := intermediate_value_Ioo zero_le_one hcont hmem
  exact ⟨c, hc0, hc1, hc⟩

/-- Below the root `P` is negative and above it `P` is positive, on `[0, ∞)`. -/
theorem P_neg_below {μ γ k c x : ℝ} (hμ : 0 < μ) (hγ : 0 ≤ γ) (hc : 0 < c)
    (hPc : P μ γ k c = 0) (hx : 0 ≤ x) (hxc : x < c) : P μ γ k x < 0 := by
  have h := P_cross μ γ k x c
  rw [hPc, mul_zero, sub_zero] at h
  have hpos : 0 < γ * x * c + μ := by positivity
  have : c * P μ γ k x < c * 0 := by
    rw [h, mul_zero]; exact mul_neg_of_neg_of_pos (by linarith) hpos
  exact lt_of_mul_lt_mul_left this hc.le

theorem P_pos_above {μ γ k c x : ℝ} (hμ : 0 < μ) (hγ : 0 ≤ γ) (hc : 0 < c)
    (hPc : P μ γ k c = 0) (hxc : c < x) : 0 < P μ γ k x := by
  have h := P_cross μ γ k x c
  rw [hPc, mul_zero, sub_zero] at h
  have hpos : 0 < γ * x * c + μ := by
    have : 0 ≤ x := by linarith
    positivity
  have : c * 0 < c * P μ γ k x := by
    rw [h, mul_zero]; exact mul_pos (by linarith) hpos
  exact lt_of_mul_lt_mul_left this hc.le

/-- A higher frozen discrimination rate gives a strictly lower fixed point. -/
theorem fixed_anti {μ γ k₁ k₂ c₁ c₂ : ℝ} (hμ : 0 < μ) (hγ : 0 ≤ γ)
    (hk : k₁ < k₂) (hc₁ : 0 < c₁) (hP₁ : P μ γ k₁ c₁ = 0)
    (hc₂ : 0 < c₂) (hP₂ : P μ γ k₂ c₂ = 0) : c₂ < c₁ := by
  have hshift : P μ γ k₂ c₁ = P μ γ k₁ c₁ + (k₂ - k₁) * c₁ := by unfold P; ring
  have hpos : 0 < P μ γ k₂ c₁ := by
    rw [hshift, hP₁, zero_add]; exact mul_pos (by linarith) hc₁
  by_contra h
  push_neg at h
  rcases eq_or_lt_of_le h with h' | h'
  · rw [h'] at hpos; linarith
  · linarith [P_neg_below hμ hγ hc₂ hP₂ hc₁.le h']

/-- Through `k = s δ F β₀`: with `F` lower at a higher belief, a higher frozen
belief gives a strictly higher fixed point. -/
theorem fixed_mono_belief {F : ℝ → ℝ} {μ γ δ s β₁ β₂ c₁ c₂ : ℝ}
    (hμ : 0 < μ) (hγ : 0 ≤ γ) (hsd : 0 < s * δ) (hF : F β₂ < F β₁)
    (hc₁ : 0 < c₁) (hP₁ : P μ γ (s * δ * F β₁) c₁ = 0)
    (hc₂ : 0 < c₂) (hP₂ : P μ γ (s * δ * F β₂) c₂ = 0) : c₁ < c₂ :=
  fixed_anti hμ hγ (mul_lt_mul_of_pos_left hF hsd) hc₂ hP₂ hc₁ hP₁

/-- Control: without `μ > 0` the fixed point need not be unique. At `μ = 0`,
`γ = 1/2`, `k = 1/4`, both `0` and `1/2` are roots. -/
theorem control_mu_needed :
    P 0 (1 / 2) (1 / 4) 0 = 0 ∧ P 0 (1 / 2) (1 / 4) (1 / 2) = 0 := by
  norm_num [P]

/-! ### Row M8. The fixed point attracts every standing -/

theorem Hk_continuous (μ γ k : ℝ) : Continuous (Hk μ γ k) := by
  unfold Hk; fun_prop

/-- With `γ ≥ 0` and `k < 1 - μ - γ`, the frozen map is strictly increasing up to
1, and so on `[0, 1]`. -/
theorem Hk_strictMonoOn {μ γ k x y : ℝ} (hγ : 0 ≤ γ) (hk : k < 1 - μ - γ)
    (hxy : x < y) (hy : y ≤ 1) : Hk μ γ k x < Hk μ γ k y := by
  have hdiff : Hk μ γ k y - Hk μ γ k x = (y - x) * (1 - μ + γ - k - γ * (x + y)) := by
    unfold Hk; ring
  have hg : γ * (x + y) ≤ γ * 2 := mul_le_mul_of_nonneg_left (by linarith) hγ
  have : 0 < (y - x) * (1 - μ + γ - k - γ * (x + y)) :=
    mul_pos (by linarith) (by linarith)
  linarith

/-- Row M8. Under the same bound, every standing in `[0, 1]` converges to the
unique fixed point. -/
theorem converges_to_fixed {μ γ k c : ℝ} (hμ : 0 < μ) (hγ : 0 ≤ γ) (hk : k < 1 - μ - γ)
    (hc : 0 < c) (hc1 : c < 1) (hPc : P μ γ k c = 0) :
    ∀ c₀, 0 ≤ c₀ → c₀ ≤ 1 → Tendsto (fun n => (Hk μ γ k)^[n] c₀) atTop (𝓝 c) := by
  have hfix : Hk μ γ k c = c := fixed_iff_P.2 hPc
  have mono : ∀ x y, 0 ≤ x → x ≤ y → y ≤ 1 → Hk μ γ k x ≤ Hk μ γ k y := by
    intro x y hx hxy hy
    rcases eq_or_lt_of_le hxy with h | h
    · rw [h]
    · exact (Hk_strictMonoOn hγ hk h hy).le
  intro c₀ h0 h1
  rcases le_or_gt c₀ c with hle | hlt
  · apply tendsto_up (Hk_continuous μ γ k) hle hfix
    · intro x y hx hxy hy; exact mono x y (by linarith) hxy (by linarith)
    · intro x hx hxc
      have := P_neg_below hμ hγ hc hPc (by linarith) hxc
      linarith [Hk_sub_self μ γ k x]
  · apply tendsto_down (Hk_continuous μ γ k) hlt.le hfix
    · intro x y hx hxy hy; exact mono x y (by linarith) hxy (by linarith)
    · intro x hx _
      have := P_pos_above hμ hγ hc hPc hx
      linarith [Hk_sub_self μ γ k x]

/-- The fixed point of the control below is `1/4`. -/
theorem control_cycle_fixed : P (1 / 2) 0 (3 / 2) (1 / 4) = 0 := by
  norm_num [P]

/-- Control: without the bound on `k` convergence can fail. At `μ = 1/2`,
`γ = 0`, `k = 3/2` the map is `c ↦ 1/2 - c`, and the orbit of 0 alternates
between 0 and 1/2 and converges to nothing. -/
theorem control_cycle :
    ¬ ∃ L, Tendsto (fun n => (Hk (1 / 2) 0 (3 / 2))^[n] 0) atTop (𝓝 L) := by
  rintro ⟨L, hL⟩
  set u : ℕ → ℝ := fun n => (Hk (1 / 2) 0 (3 / 2))^[n] 0 with hu
  have hstep : ∀ n, u (n + 1) = 1 / 2 - u n := by
    intro n
    simp only [hu]
    rw [Function.iterate_succ_apply']
    unfold Hk; ring
  have hval : ∀ n, |u n - 1 / 4| = 1 / 4 := by
    intro n
    induction n with
    | zero => simp only [hu, Function.iterate_zero, id]; norm_num
    | succ n ih =>
      rw [hstep, show (1 : ℝ) / 2 - u n - 1 / 4 = -(u n - 1 / 4) by ring, abs_neg]
      exact ih
  have h1 : Tendsto (fun n => u (n + 1) + u n) atTop (𝓝 (L + L)) :=
    (hL.comp (tendsto_add_atTop_nat 1)).add hL
  have h2 : (fun n => u (n + 1) + u n) = fun _ => (1 / 2 : ℝ) := by
    funext n; rw [hstep]; ring
  rw [h2] at h1
  have hLL : (1 / 2 : ℝ) = L + L := tendsto_nhds_unique tendsto_const_nhds h1
  have hL4 : L = 1 / 4 := by linarith
  rw [hL4] at hL
  obtain ⟨N, hN⟩ := Metric.tendsto_atTop.1 hL (1 / 4) (by norm_num)
  have hd := hN N le_rfl
  rw [Real.dist_eq, hval N] at hd
  exact lt_irrefl _ hd

end FrozenMap
