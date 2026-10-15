import Mathlib

open Filter Topology

namespace ClosureMap

/-- The composed map of the predecessor's equation (composed), for a general
discrimination-rate function `F`: `μ + c (1 − μ + γ − s δ F c) − γ c²`. -/
def Hf (F : ℝ → ℝ) (μ γ δ s c : ℝ) : ℝ :=
  μ + c * (1 - μ + γ - s * δ * F c) - γ * c ^ 2

/-- The composed map for the canonical `F c = (1 − c)²`, with `b = s δ`. -/
def H (μ γ b c : ℝ) : ℝ :=
  μ + c * (1 - μ + γ - b * (1 - c) ^ 2) - γ * c ^ 2

/-- The quadratic whose roots are the fixed points of `H` other than 1. -/
def Q (μ γ b c : ℝ) : ℝ :=
  b * c ^ 2 - (b - γ) * c + μ

/-! ### Row M4: the boundary values -/

theorem Hf_zero (F : ℝ → ℝ) (μ γ δ s : ℝ) : Hf F μ γ δ s 0 = μ := by
  simp [Hf]

theorem Hf_one_iff (F : ℝ → ℝ) (μ γ δ s : ℝ) (hsd : s * δ ≠ 0) :
    Hf F μ γ δ s 1 = 1 ↔ F 1 = 0 := by
  unfold Hf
  constructor
  · intro h
    have h' : s * δ * F 1 = 0 := by linarith
    rcases mul_eq_zero.mp h' with h0 | h0
    · exact absurd h0 hsd
    · exact h0
  · intro h
    rw [h]
    ring

theorem H_eq_Hf (μ γ δ s c : ℝ) :
    Hf (fun x => (1 - x) ^ 2) μ γ δ s c = H μ γ (s * δ) c := by
  simp [Hf, H]

/-- Control: with `F 1 ≠ 0` the integrated standing is not a fixed point. -/
theorem control_F_one_needed :
    Hf (fun _ => 1) (1 / 20) (1 / 10) 1 1 1 ≠ 1 := by
  norm_num [Hf]

/-! ### Row M5: the fixed points -/

theorem H_sub_self (μ γ b c : ℝ) : H μ γ b c - c = (1 - c) * Q μ γ b c := by
  unfold H Q
  ring

theorem fixed_iff_Q {μ γ b c : ℝ} (hc : c ≠ 1) : H μ γ b c = c ↔ Q μ γ b c = 0 := by
  have h1 : (1 - c) ≠ 0 := sub_ne_zero.mpr (Ne.symm hc)
  constructor
  · intro h
    have : (1 - c) * Q μ γ b c = 0 := by rw [← H_sub_self]; linarith
    exact (mul_eq_zero.mp this).resolve_left h1
  · intro h
    have : H μ γ b c - c = 0 := by rw [H_sub_self, h, mul_zero]
    linarith

theorem H_one (μ γ b : ℝ) : H μ γ b 1 = 1 := by
  unfold H
  ring

/-- Two distinct roots of `Q` satisfy Vieta's relations. -/
theorem vieta {μ γ b c₁ c₂ : ℝ} (h₁ : Q μ γ b c₁ = 0) (h₂ : Q μ γ b c₂ = 0)
    (hne : c₁ ≠ c₂) : b * (c₁ + c₂) = b - γ ∧ b * c₁ * c₂ = μ := by
  unfold Q at h₁ h₂
  have hsum : b * (c₁ + c₂) = b - γ := by
    have hd : c₁ - c₂ ≠ 0 := sub_ne_zero.mpr hne
    apply mul_left_cancel₀ hd
    linear_combination h₁ - h₂
  refine ⟨hsum, ?_⟩
  linear_combination c₁ * hsum - h₁

/-- Under Vieta's relations, `Q` factors through its two roots. -/
theorem Q_factor {μ γ b c₁ c₂ : ℝ} (hsum : b * (c₁ + c₂) = b - γ)
    (hprod : b * c₁ * c₂ = μ) (c : ℝ) :
    Q μ γ b c = b * (c - c₁) * (c - c₂) := by
  unfold Q
  linear_combination c * hsum - hprod

/-- The trap condition. With `μ > 0`, `γ ≥ 0` and `b = s δ > 0`, the map has two
fixed points strictly inside `(0, 1)` exactly when `b − γ > 0` and
`(b − γ)² > 4 μ b`. -/
theorem two_interior_iff {μ γ b : ℝ} (hμ : 0 < μ) (hγ : 0 ≤ γ) (hb : 0 < b) :
    (∃ c₁ c₂, 0 < c₁ ∧ c₁ < c₂ ∧ c₂ < 1 ∧ H μ γ b c₁ = c₁ ∧ H μ γ b c₂ = c₂) ↔
      (0 < b - γ ∧ 4 * μ * b < (b - γ) ^ 2) := by
  constructor
  · rintro ⟨c₁, c₂, h0, h12, h1, hf1, hf2⟩
    have q1 := (fixed_iff_Q (by linarith : c₁ ≠ 1)).mp hf1
    have q2 := (fixed_iff_Q (by linarith : c₂ ≠ 1)).mp hf2
    obtain ⟨hsum, hprod⟩ := vieta q1 q2 (ne_of_lt h12)
    have hpos : 0 < b * (c₁ + c₂) := mul_pos hb (by linarith)
    refine ⟨by linarith, ?_⟩
    have hsq : 0 < (b * (c₂ - c₁)) ^ 2 := by
      have : b * (c₂ - c₁) ≠ 0 := mul_ne_zero hb.ne' (sub_ne_zero.mpr (ne_of_gt h12))
      positivity
    have : (b - γ) ^ 2 - 4 * μ * b = (b * (c₂ - c₁)) ^ 2 := by
      rw [← hsum, ← hprod]
      ring
    linarith
  · rintro ⟨hB, hD⟩
    set B := b - γ with hBdef
    set D := B ^ 2 - 4 * μ * b with hDdef
    have hD0 : 0 < D := by rw [hDdef]; linarith
    set r := Real.sqrt D with hr
    have hs : r ^ 2 = D := Real.sq_sqrt hD0.le
    have hr0 : 0 < r := Real.sqrt_pos.mpr hD0
    have hrB : r < B := by
      rw [hr, Real.sqrt_lt' hB]
      rw [hDdef]
      nlinarith
    have hrB' : r < b + γ := by
      rw [hr, Real.sqrt_lt' (by linarith)]
      rw [hDdef, hBdef]
      nlinarith
    set i := (2 * b)⁻¹ with hi
    have hi0 : 0 < i := by positivity
    have hi1 : 2 * b * i = 1 := by rw [hi]; field_simp
    refine ⟨(B - r) * i, (B + r) * i, ?_, ?_, ?_, ?_, ?_⟩
    · exact mul_pos (by linarith) hi0
    · exact mul_lt_mul_of_pos_right (by linarith) hi0
    · have : (B + r) * i < 2 * b * i := mul_lt_mul_of_pos_right (by linarith) hi0
      linarith
    · have hne : (B - r) * i ≠ 1 := by
        have : (B - r) * i < 2 * b * i := mul_lt_mul_of_pos_right (by linarith) hi0
        linarith
      rw [fixed_iff_Q hne]
      unfold Q
      linear_combination (i / 2) * hs + (i * (B - r) ^ 2 / 2 - μ) * hi1
    · have hne : (B + r) * i ≠ 1 := by
        have : (B + r) * i < 2 * b * i := mul_lt_mul_of_pos_right (by linarith) hi0
        linarith
      rw [fixed_iff_Q hne]
      unfold Q
      linear_combination (i / 2) * hs + (i * (B + r) ^ 2 / 2 - μ) * hi1

/-- With two interior fixed points `c₁ < c₂`, the fixed points of `H` in `[0, 1]`
are exactly `c₁`, `c₂` and 1. -/
theorem fixed_points_exactly {μ γ b c₁ c₂ : ℝ} (hb : 0 < b)
    (h12 : c₁ < c₂) (h2 : c₂ < 1) (hf1 : H μ γ b c₁ = c₁) (hf2 : H μ γ b c₂ = c₂) (c : ℝ) :
    H μ γ b c = c ↔ c = c₁ ∨ c = c₂ ∨ c = 1 := by
  have q1 := (fixed_iff_Q (by linarith : c₁ ≠ 1)).mp hf1
  have q2 := (fixed_iff_Q (by linarith : c₂ ≠ 1)).mp hf2
  obtain ⟨hsum, hprod⟩ := vieta q1 q2 (ne_of_lt h12)
  have key : H μ γ b c - c = (1 - c) * (b * (c - c₁) * (c - c₂)) := by
    rw [H_sub_self, Q_factor hsum hprod]
  constructor
  · intro h
    have h0 : (1 - c) * (b * (c - c₁) * (c - c₂)) = 0 := by rw [← key]; linarith
    rcases mul_eq_zero.mp h0 with h3 | h3
    · right; right; linarith
    · rcases mul_eq_zero.mp h3 with h4 | h4
      · rcases mul_eq_zero.mp h4 with h5 | h5
        · exact absurd h5 hb.ne'
        · left; linarith
      · right; left; linarith
  · rintro (rfl | rfl | rfl)
    · exact hf1
    · exact hf2
    · exact H_one μ γ b

/-- Control: below the trap condition there is no interior fixed point. At
`μ = 1/20`, `γ = 1/10`, `b = 1/5`, `(b − γ)² = 1/100 < 4 μ b = 1/25`. -/
theorem control_no_trap :
    ¬ ∃ c₁ c₂, 0 < c₁ ∧ c₁ < c₂ ∧ c₂ < 1 ∧
      H (1 / 20) (1 / 10) (1 / 5) c₁ = c₁ ∧ H (1 / 20) (1 / 10) (1 / 5) c₂ = c₂ := by
  rw [two_interior_iff (by norm_num) (by norm_num) (by norm_num)]
  norm_num

/-- The trap condition is met at the predecessor's figure parameters,
`μ = 1/20`, `γ = 1/10`, `b = 1/2`. -/
theorem trap_at_figure_parameters :
    ∃ c₁ c₂, 0 < c₁ ∧ c₁ < c₂ ∧ c₂ < 1 ∧
      H (1 / 20) (1 / 10) (1 / 2) c₁ = c₁ ∧ H (1 / 20) (1 / 10) (1 / 2) c₂ = c₂ := by
  rw [two_interior_iff (by norm_num) (by norm_num) (by norm_num)]
  norm_num

/-! ### Row M6: the basins -/

/-- A continuous map, nondecreasing on `[x₀, p]`, with `p` fixed and `f x > x` on
`[x₀, p)`, carries `x₀` to `p`. -/
theorem tendsto_up {f : ℝ → ℝ} (hf : Continuous f) {x₀ p : ℝ} (hx₀ : x₀ ≤ p)
    (hfp : f p = p)
    (hmono : ∀ x y, x₀ ≤ x → x ≤ y → y ≤ p → f x ≤ f y)
    (hup : ∀ x, x₀ ≤ x → x < p → x < f x) :
    Tendsto (fun n => f^[n] x₀) atTop (𝓝 p) := by
  set u : ℕ → ℝ := fun n => f^[n] x₀ with hu
  have hstep : ∀ n, u (n + 1) = f (u n) := fun n => Function.iterate_succ_apply' f n x₀
  have hbd : ∀ n, x₀ ≤ u n ∧ u n ≤ p := by
    intro n
    induction n with
    | zero => exact ⟨le_rfl, hx₀⟩
    | succ n ih =>
      rw [hstep]
      constructor
      · rcases eq_or_lt_of_le ih.2 with h | h
        · rw [h, hfp]; exact hx₀
        · exact le_trans ih.1 (hup _ ih.1 h).le
      · calc f (u n) ≤ f p := hmono _ _ ih.1 ih.2 le_rfl
          _ = p := hfp
  have hmon : Monotone u := by
    apply monotone_nat_of_le_succ
    intro n
    rw [hstep]
    rcases eq_or_lt_of_le (hbd n).2 with h | h
    · rw [h, hfp]
    · exact (hup _ (hbd n).1 h).le
  have hbdd : BddAbove (Set.range u) := ⟨p, by rintro _ ⟨n, rfl⟩; exact (hbd n).2⟩
  have hlim := tendsto_atTop_ciSup hmon hbdd
  have hL1 : (⨆ n, u n) ≤ p := ciSup_le fun n => (hbd n).2
  have hL0 : x₀ ≤ ⨆ n, u n := le_trans (hbd 0).1 (le_ciSup hbdd 0)
  have hfix : f (⨆ n, u n) = ⨆ n, u n := by
    have h1 : Tendsto (fun n => u (n + 1)) atTop (𝓝 (⨆ n, u n)) :=
      hlim.comp (tendsto_add_atTop_nat 1)
    have h2 : Tendsto (fun n => u (n + 1)) atTop (𝓝 (f (⨆ n, u n))) := by
      simp_rw [hstep]
      exact (hf.tendsto _).comp hlim
    exact tendsto_nhds_unique h2 h1
  have hLp : (⨆ n, u n) = p := by
    rcases eq_or_lt_of_le hL1 with h | h
    · exact h
    · exact absurd hfix (ne_of_gt (hup _ hL0 h))
  rw [← hLp]
  exact hlim

/-- The mirror image: nondecreasing on `[p, x₀]`, `p` fixed, `f x < x` on
`(p, x₀]`, carries `x₀` down to `p`. -/
theorem tendsto_down {f : ℝ → ℝ} (hf : Continuous f) {x₀ p : ℝ} (hx₀ : p ≤ x₀)
    (hfp : f p = p)
    (hmono : ∀ x y, p ≤ x → x ≤ y → y ≤ x₀ → f x ≤ f y)
    (hdown : ∀ x, p < x → x ≤ x₀ → f x < x) :
    Tendsto (fun n => f^[n] x₀) atTop (𝓝 p) := by
  set u : ℕ → ℝ := fun n => f^[n] x₀ with hu
  have hstep : ∀ n, u (n + 1) = f (u n) := fun n => Function.iterate_succ_apply' f n x₀
  have hbd : ∀ n, p ≤ u n ∧ u n ≤ x₀ := by
    intro n
    induction n with
    | zero => exact ⟨hx₀, le_rfl⟩
    | succ n ih =>
      rw [hstep]
      constructor
      · calc p = f p := hfp.symm
          _ ≤ f (u n) := hmono _ _ le_rfl ih.1 ih.2
      · rcases eq_or_lt_of_le ih.1 with h | h
        · rw [← h, hfp]; exact hx₀
        · exact le_trans (hdown _ h ih.2).le ih.2
  have hanti : Antitone u := by
    apply antitone_nat_of_succ_le
    intro n
    rw [hstep]
    rcases eq_or_lt_of_le (hbd n).1 with h | h
    · rw [← h, hfp]
    · exact (hdown _ h (hbd n).2).le
  have hbdd : BddBelow (Set.range u) := ⟨p, by rintro _ ⟨n, rfl⟩; exact (hbd n).1⟩
  have hlim := tendsto_atTop_ciInf hanti hbdd
  have hL1 : p ≤ ⨅ n, u n := le_ciInf fun n => (hbd n).1
  have hL0 : (⨅ n, u n) ≤ x₀ := le_trans (ciInf_le hbdd 0) (hbd 0).2
  have hfix : f (⨅ n, u n) = ⨅ n, u n := by
    have h1 : Tendsto (fun n => u (n + 1)) atTop (𝓝 (⨅ n, u n)) :=
      hlim.comp (tendsto_add_atTop_nat 1)
    have h2 : Tendsto (fun n => u (n + 1)) atTop (𝓝 (f (⨅ n, u n))) := by
      simp_rw [hstep]
      exact (hf.tendsto _).comp hlim
    exact tendsto_nhds_unique h2 h1
  have hLp : (⨅ n, u n) = p := by
    rcases eq_or_lt_of_le hL1 with h | h
    · exact h.symm
    · exact absurd hfix (ne_of_lt (hdown _ h hL0))
  rw [← hLp]
  exact hlim

theorem H_continuous (μ γ b : ℝ) : Continuous (H μ γ b) := by
  unfold H
  fun_prop

/-- `H` is strictly increasing on `[0, 1]` when `b < 1 − μ + γ` and `μ + γ < 1`. -/
theorem H_strictMonoOn {μ γ b : ℝ} (hb : 0 < b)
    (hA : b < 1 - μ + γ) (hμγ : μ + γ < 1) {x y : ℝ}
    (hx : 0 ≤ x) (hxy : x < y) (hy : y ≤ 1) : H μ γ b x < H μ γ b y := by
  set A := 1 - μ + γ - b with hAdef
  set m := min A (1 - μ - γ) with hm
  have hm0 : 0 < m := lt_min (by linarith) (by linarith)
  have hmA : m ≤ A := min_le_left _ _
  have hmC : m ≤ 1 - μ - γ := min_le_right _ _
  have hw1 : 0 ≤ (1 - x) * (1 - y) := mul_nonneg (by linarith) (by linarith)
  have hw2 : 0 ≤ x * (1 - y) + y * (1 - x) := by
    have := mul_nonneg hx (by linarith : (0 : ℝ) ≤ 1 - y)
    have := mul_nonneg (by linarith : (0 : ℝ) ≤ y) (by linarith : (0 : ℝ) ≤ 1 - x)
    linarith
  have hw3 : 0 ≤ x * y := mul_nonneg hx (by linarith)
  have hG : 0 < A + (2 * b - γ) * (x + y) - b * (x ^ 2 + x * y + y ^ 2) := by
    have hdec : A + (2 * b - γ) * (x + y) - b * (x ^ 2 + x * y + y ^ 2)
        = A * ((1 - x) * (1 - y)) + (1 - μ) * (x * (1 - y) + y * (1 - x))
          + (1 - μ - γ) * (x * y) + b * (x * (1 - x)) + b * (y * (1 - y)) := by
      rw [hAdef]; ring
    have e1 : m * ((1 - x) * (1 - y)) ≤ A * ((1 - x) * (1 - y)) :=
      mul_le_mul_of_nonneg_right hmA hw1
    have e2 : m * (x * (1 - y) + y * (1 - x)) ≤ (1 - μ) * (x * (1 - y) + y * (1 - x)) :=
      mul_le_mul_of_nonneg_right (by linarith) hw2
    have e3 : m * (x * y) ≤ (1 - μ - γ) * (x * y) := mul_le_mul_of_nonneg_right hmC hw3
    have e4 : 0 ≤ b * (x * (1 - x)) := mul_nonneg hb.le (mul_nonneg hx (by linarith))
    have e5 : 0 ≤ b * (y * (1 - y)) :=
      mul_nonneg hb.le (mul_nonneg (by linarith) (by linarith))
    have hsum1 : (1 - x) * (1 - y) + (x * (1 - y) + y * (1 - x)) + x * y = 1 := by ring
    rw [hdec]
    nlinarith
  have hdiff : H μ γ b y - H μ γ b x
      = (y - x) * (A + (2 * b - γ) * (x + y) - b * (x ^ 2 + x * y + y ^ 2)) := by
    unfold H; rw [hAdef]; ring
  have : 0 < H μ γ b y - H μ γ b x := by
    rw [hdiff]; exact mul_pos (by linarith) hG
  linarith

/-- The basins. With interior fixed points `c₁ < c₂` and `H` increasing on
`[0, 1]`, every standing in `[0, c₂)` converges to `c₁`, and every standing in
`(c₂, 1]` converges to 1. -/
theorem basins {μ γ b c₁ c₂ : ℝ} (hb : 0 < b)
    (hA : b < 1 - μ + γ) (hμγ : μ + γ < 1)
    (h0 : 0 < c₁) (h12 : c₁ < c₂) (h2 : c₂ < 1)
    (hf1 : H μ γ b c₁ = c₁) (hf2 : H μ γ b c₂ = c₂) :
    (∀ c₀, 0 ≤ c₀ → c₀ < c₂ →
        Tendsto (fun n => (H μ γ b)^[n] c₀) atTop (𝓝 c₁)) ∧
    (∀ c₀, c₂ < c₀ → c₀ ≤ 1 →
        Tendsto (fun n => (H μ γ b)^[n] c₀) atTop (𝓝 1)) := by
  have q1 := (fixed_iff_Q (by linarith : c₁ ≠ 1)).mp hf1
  have q2 := (fixed_iff_Q (by linarith : c₂ ≠ 1)).mp hf2
  obtain ⟨hsum, hprod⟩ := vieta q1 q2 (ne_of_lt h12)
  have key : ∀ c, H μ γ b c - c = (1 - c) * (b * (c - c₁) * (c - c₂)) := by
    intro c; rw [H_sub_self, Q_factor hsum hprod]
  have mono : ∀ x y, 0 ≤ x → x ≤ y → y ≤ 1 → H μ γ b x ≤ H μ γ b y := by
    intro x y hx hxy hy
    rcases eq_or_lt_of_le hxy with h | h
    · rw [h]
    · exact (H_strictMonoOn hb hA hμγ hx h hy).le
  have hc := H_continuous μ γ b
  refine ⟨fun c₀ hc0 hc2 => ?_, fun c₀ hc2 hc1 => ?_⟩
  · rcases le_or_gt c₀ c₁ with hle | hlt
    · apply tendsto_up hc hle hf1
      · intro x y hx hxy hy; exact mono x y (by linarith) hxy (by linarith)
      · intro x hx hxp
        have hk := key x
        have : 0 < (1 - x) * (b * (c₁ - x) * (c₂ - x)) := by
          have : 0 < 1 - x := by linarith
          have : 0 < c₁ - x := by linarith
          have : 0 < c₂ - x := by linarith
          positivity
        nlinarith
    · apply tendsto_down hc hlt.le hf1
      · intro x y hx hxy hy; exact mono x y (by linarith) hxy (by linarith)
      · intro x hx hxp
        have hk := key x
        have : 0 < (1 - x) * (b * (x - c₁) * (c₂ - x)) := by
          have : 0 < 1 - x := by linarith
          have : 0 < x - c₁ := by linarith
          have : 0 < c₂ - x := by linarith
          positivity
        nlinarith
  · apply tendsto_up hc hc1 (H_one μ γ b)
    · intro x y hx hxy hy; exact mono x y (by linarith) hxy hy
    · intro x hx hxp
      have hk := key x
      have : 0 < (1 - x) * (b * (x - c₁) * (x - c₂)) := by
        have : 0 < 1 - x := by linarith
        have : 0 < x - c₁ := by linarith
        have : 0 < x - c₂ := by linarith
        positivity
      nlinarith

/-- Every hypothesis of `basins` holds at the predecessor's figure parameters. -/
theorem basins_hypotheses_at_figure :
    (1 / 2 : ℝ) < 1 - 1 / 20 + 1 / 10 ∧ (1 / 20 : ℝ) + 1 / 10 < 1 := by
  norm_num

/-- Control: without a bound on `b = s δ` the map leaves `[0, 1]`. At `μ = 1/20`,
`γ = 0`, `b = 3`, the standing `1/5` is sent below 0. -/
theorem control_leaves_unit : H (1 / 20) 0 3 (1 / 5) < 0 := by
  norm_num [H]

/-- Control: with `b ≥ 1 − μ + γ` the map is not increasing near 0. At
`μ = 1/20`, `γ = 0`, `b = 1`, `H (1/100) < H 0`. -/
theorem control_not_monotone : H (1 / 20) 0 1 (1 / 100) < H (1 / 20) 0 1 0 := by
  norm_num [H]

end ClosureMap
