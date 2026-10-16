import Mathlib

/-! Kim and Loury (2012), "Stereotypes and Inequality: A 'Signaling' Theory of
Identity Choice", KDI Journal of Economic Policy 34(2), pp. 1–15, in the
paper's notation.

`V₀ π` and `V₁ π` are the expected wages of an unskilled and a skilled worker in
a group believed to invest at rate `π` (eq. 2). The results on the selection of
types (Lemma 1 to Proposition 2) take them as given functions, as the article
does once eq. (9) is written. Eqs. (1) to (8) are derived for a finite signal
space (`Signal`), in place of the article's densities on `[0, 1]`.
`R π = V₁ π − V₀ π` is the return to skill (eq. 3), `U π c` the payoff of a
worker with skill cost `c` (eq. 9), and `dU πA πB c` the gain `ΔU(πA, πB; c)`
from affect A over affect B.

The cost distribution `G` is taken to be finite, a tractable special case of
the paper's continuous `G`. Cost types are `j : ι` with cost `c j` and mass
`g j`. `H` is the distribution function of the affect cost `k`, and a worker
chooses affect A if and only if `ΔU ≥ k`, so a type chooses A with
probability `H (ΔU)` (eqs. 10 to 13). -/

namespace KimLoury2012

open Finset

/-- Eq. (3). The return to acquiring skill in a group believed to invest at `π`. -/
def R (V₀ V₁ : ℝ → ℝ) (π : ℝ) : ℝ := V₁ π - V₀ π

/-- Eq. (9). The payoff of a worker with skill cost `c` in a group believed to
invest at `π`, `U(π, c) = max {V₁(π) − c; V₀(π)}`. -/
noncomputable def U (V₀ V₁ : ℝ → ℝ) (π c : ℝ) : ℝ := max (V₁ π - c) (V₀ π)

/-- `ΔU(πA, πB; c) ≡ U(πA, c) − U(πB, c)`, the gain from affect A over affect B. -/
noncomputable def dU (V₀ V₁ : ℝ → ℝ) (πA πB c : ℝ) : ℝ := U V₀ V₁ πA c - U V₀ V₁ πB c

/-- Eq. (15). `U(π, c) = max {R(π) − c; 0} + V₀(π)`. -/
theorem eq_15 (V₀ V₁ : ℝ → ℝ) (π c : ℝ) :
    U V₀ V₁ π c = max (R V₀ V₁ π - c) 0 + V₀ π := by
  unfold U R
  simp only [max_def]
  split_ifs <;> linarith

/-- Lemma 1, low costs. Below both returns, every type invests in either group
and `ΔU = V₁(πA) − V₁(πB)`. -/
theorem lemma1_low {V₀ V₁ : ℝ → ℝ} {πA πB c : ℝ}
    (hA : c ≤ R V₀ V₁ πA) (hB : c ≤ R V₀ V₁ πB) :
    dU V₀ V₁ πA πB c = V₁ πA - V₁ πB := by
  unfold dU U
  unfold R at hA hB
  rw [max_eq_left (by linarith), max_eq_left (by linarith)]
  ring

/-- Lemma 1, high costs. Above both returns, no type invests and
`ΔU = V₀(πA) − V₀(πB)`. -/
theorem lemma1_high {V₀ V₁ : ℝ → ℝ} {πA πB c : ℝ}
    (hA : R V₀ V₁ πA ≤ c) (hB : R V₀ V₁ πB ≤ c) :
    dU V₀ V₁ πA πB c = V₀ πA - V₀ πB := by
  unfold dU U
  unfold R at hA hB
  rw [max_eq_right (by linarith), max_eq_right (by linarith)]

/-- Lemma 1, eq. (16), first line. Between the returns, with `R(πA) ≥ R(πB)`,
`ΔU = V₁(πA) − V₀(πB) − c`. -/
theorem lemma1_mid {V₀ V₁ : ℝ → ℝ} {πA πB c : ℝ}
    (hB : R V₀ V₁ πB ≤ c) (hA : c ≤ R V₀ V₁ πA) :
    dU V₀ V₁ πA πB c = V₁ πA - V₀ πB - c := by
  unfold dU U
  unfold R at hA hB
  rw [max_eq_left (by linarith), max_eq_right (by linarith)]
  ring

/-- Lemma 1, read as on p. 11. `ΔU` is non-increasing in `c` whenever
`R(πA) ≥ R(πB)`. -/
theorem dU_antitone {V₀ V₁ : ℝ → ℝ} {πA πB : ℝ} (h : R V₀ V₁ πB ≤ R V₀ V₁ πA) :
    Antitone (dU V₀ V₁ πA πB) := by
  intro x y hxy
  simp only [dU, eq_15]
  unfold R at h ⊢
  simp only [max_def]
  split_ifs <;> linarith

/-- `ΔU(πB, πA; c) = −ΔU(πA, πB; c)` (p. 9). -/
theorem dU_swap (V₀ V₁ : ℝ → ℝ) (πA πB c : ℝ) :
    dU V₀ V₁ πB πA c = - dU V₀ V₁ πA πB c := by
  unfold dU; ring

/-- Proposition 1, pointwise. If both wages are higher in group A (`V_e` rising
in `π` and `πA > πB`), every cost type gains from affect A. -/
theorem prop1_pointwise {V₀ V₁ : ℝ → ℝ} {πA πB : ℝ}
    (h0 : V₀ πB < V₀ πA) (h1 : V₁ πB < V₁ πA) (c : ℝ) :
    0 < dU V₀ V₁ πA πB c := by
  unfold dU U
  have : max (V₁ πB - c) (V₀ πB) < max (V₁ πA - c) (V₀ πA) :=
    max_lt (lt_max_of_lt_left (by linarith)) (lt_max_of_lt_right h0)
  linarith

/-! ### The signal and the wage, eqs. (1) to (8), with a finite signal space -/

/-- The employers' posterior that a worker with signal densities `f₀`, `f₁` at
his signal is skilled, in a group believed to invest at `π`,
`ρ(π, t) = π f₁(t) / (π f₁(t) + (1 − π) f₀(t))`. -/
noncomputable def rhoRaw (f₀ f₁ π : ℝ) : ℝ := π * f₁ / (π * f₁ + (1 - π) * f₀)

/-- A finite signal space with densities `f₀` and `f₁` of full support. -/
structure Signal (κ : Type*) [Fintype κ] where
  f₀ : κ → ℝ
  f₁ : κ → ℝ
  pos₀ : ∀ t, 0 < f₀ t
  pos₁ : ∀ t, 0 < f₁ t
  sum₀ : ∑ t, f₀ t = 1
  sum₁ : ∑ t, f₁ t = 1

variable {κ : Type*} [Fintype κ]

/-- `f(π, t) = π f₁(t) + (1 − π) f₀(t)`. -/
def fpi (s : Signal κ) (π : ℝ) (t : κ) : ℝ := π * s.f₁ t + (1 - π) * s.f₀ t

/-- `ρ(π, t)`. -/
noncomputable def rho (s : Signal κ) (π : ℝ) (t : κ) : ℝ := rhoRaw (s.f₀ t) (s.f₁ t) π

/-- Eq. (1). `W(π, t) = ω ρ(π, t)`. -/
noncomputable def W (s : Signal κ) (ω π : ℝ) (t : κ) : ℝ := ω * rho s π t

/-- Eq. (2). `V_e(π) = Σ_t f_e(t) W(π, t)`, with `fe` the density of `e`. -/
noncomputable def V (s : Signal κ) (ω : ℝ) (fe : κ → ℝ) (π : ℝ) : ℝ := ∑ t, fe t * W s ω π t

/-- `V₀` and `V₁`. -/
noncomputable def V0s (s : Signal κ) (ω : ℝ) : ℝ → ℝ := V s ω s.f₀
noncomputable def V1s (s : Signal κ) (ω : ℝ) : ℝ → ℝ := V s ω s.f₁

/-- Eq. (3). `R(π) = V₁(π) − V₀(π)` for the signal model. -/
noncomputable def Rs (s : Signal κ) (ω : ℝ) (π : ℝ) : ℝ := R (V0s s ω) (V1s s ω) π

theorem fpi_pos (s : Signal κ) {π : ℝ} (hπ : π ∈ Set.Icc (0 : ℝ) 1) (t : κ) : 0 < fpi s π t := by
  obtain ⟨h0, h1⟩ := hπ
  have a := s.pos₀ t
  have b := s.pos₁ t
  unfold fpi
  rcases le_or_gt π (1 / 2) with h | h
  · nlinarith [mul_nonneg h0 b.le]
  · nlinarith [mul_nonneg (sub_nonneg.2 h1) a.le]

/-- Eq. (1), the claim below it. Under full support, `ρ(π, t)` rises strictly in `π`. -/
theorem rhoRaw_strictMonoOn {f₀ f₁ : ℝ} (h0 : 0 < f₀) (h1 : 0 < f₁) :
    StrictMonoOn (rhoRaw f₀ f₁) (Set.Icc 0 1) := by
  intro p hp q hq hpq
  obtain ⟨hp0, hp1⟩ := hp
  obtain ⟨hq0, hq1⟩ := hq
  have dp : 0 < p * f₁ + (1 - p) * f₀ := by
    rcases le_or_gt p (1 / 2) with h | h
    · nlinarith [mul_nonneg hp0 h1.le]
    · nlinarith [mul_nonneg (sub_nonneg.2 hp1) h0.le]
  have dq : 0 < q * f₁ + (1 - q) * f₀ := by
    rcases le_or_gt q (1 / 2) with h | h
    · nlinarith [mul_nonneg hq0 h1.le]
    · nlinarith [mul_nonneg (sub_nonneg.2 hq1) h0.le]
  unfold rhoRaw
  rw [div_lt_div_iff₀ dp dq]
  nlinarith [mul_pos (mul_pos h1 h0) (sub_pos.2 hpq), mul_pos h1 h1]

/-- Eq. (1), the claim below it, in the signal. Under MLRP, a signal with a higher
likelihood ratio `f₁/f₀` gives a weakly higher posterior. -/
theorem rhoRaw_mono_ratio {a₀ a₁ b₀ b₁ π : ℝ} (ha0 : 0 < a₀) (ha1 : 0 < a₁) (hb0 : 0 < b₀)
    (hb1 : 0 < b₁) (hπ : π ∈ Set.Icc (0 : ℝ) 1) (hr : a₁ / a₀ ≤ b₁ / b₀) :
    rhoRaw a₀ a₁ π ≤ rhoRaw b₀ b₁ π := by
  obtain ⟨h0, h1⟩ := hπ
  have da : 0 < π * a₁ + (1 - π) * a₀ := by
    rcases le_or_gt π (1 / 2) with h | h
    · nlinarith [mul_nonneg h0 ha1.le]
    · nlinarith [mul_nonneg (sub_nonneg.2 h1) ha0.le]
  have db : 0 < π * b₁ + (1 - π) * b₀ := by
    rcases le_or_gt π (1 / 2) with h | h
    · nlinarith [mul_nonneg h0 hb1.le]
    · nlinarith [mul_nonneg (sub_nonneg.2 h1) hb0.le]
  rw [div_le_div_iff₀ ha0 hb0] at hr
  unfold rhoRaw
  rw [div_le_div_iff₀ da db]
  nlinarith [mul_nonneg (mul_nonneg h0 (sub_nonneg.2 h1)) (sub_nonneg.2 hr)]

/-- Our extension, not a result of the article. At a signal that only a skilled
worker can send (`f₀ = 0`), the posterior is 1 whatever the group's
reputation, so the group drops out of the wage. -/
theorem rhoRaw_revealing {f₁ π : ℝ} (h1 : 0 < f₁) (hπ : 0 < π) : rhoRaw 0 f₁ π = 1 := by
  unfold rhoRaw
  simp only [mul_zero, add_zero]
  exact div_self (mul_pos hπ h1).ne'

/-- Control for `rhoRaw_revealing`. At a signal both types send, the wage depends
on the group's reputation, `ρ(.2, t) ≠ ρ(.8, t)`. -/
theorem control_group_matters_interior {f₀ f₁ : ℝ} (h0 : 0 < f₀) (h1 : 0 < f₁) :
    rhoRaw f₀ f₁ (2 / 10) ≠ rhoRaw f₀ f₁ (8 / 10) :=
  (rhoRaw_strictMonoOn h0 h1 ⟨by norm_num, by norm_num⟩ ⟨by norm_num, by norm_num⟩
    (by norm_num)).ne

/-- `V_e` rises strictly in `π` on `[0, 1]` (p. 7, "`V_e′(π)` is positive"). -/
theorem V_strictMonoOn (s : Signal κ) {ω : ℝ} (hω : 0 < ω) {fe : κ → ℝ}
    (hfe : ∀ t, 0 < fe t) : StrictMonoOn (V s ω fe) (Set.Icc 0 1) := by
  intro p hp q hq hpq
  have : Nonempty κ := by
    by_contra h
    rw [not_nonempty_iff] at h
    have := s.sum₀
    simp at this
  unfold V W rho
  apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
  intro t _
  have := rhoRaw_strictMonoOn (s.pos₀ t) (s.pos₁ t) hp hq hpq
  exact mul_lt_mul_of_pos_left (mul_lt_mul_of_pos_left this hω) (hfe t)

/-- `ρ(0, t) = 0` and `ρ(1, t) = 1`. -/
theorem rho_zero (s : Signal κ) (t : κ) : rho s 0 t = 0 := by simp [rho, rhoRaw]
theorem rho_one (s : Signal κ) (t : κ) : rho s 1 t = 1 := by
  simp [rho, rhoRaw, (s.pos₁ t).ne']

/-- `R(0) = R(1) = 0` (p. 8). -/
theorem R_zero_one (s : Signal κ) (ω : ℝ) : Rs s ω 0 = 0 ∧ Rs s ω 1 = 0 := by
  constructor
  · simp [Rs, R, V0s, V1s, V, W, rho_zero]
  · simp only [Rs, R, V0s, V1s, V, W, rho_one, mul_one]
    rw [← Finset.sum_mul, ← Finset.sum_mul, s.sum₀, s.sum₁]; ring

/-- Eq. (4). `R(π) = ω π Σ_t (f₁(t) − f₀(t)) f₁(t) / f(π, t)`. -/
theorem eq_4 (s : Signal κ) (ω π : ℝ) :
    Rs s ω π = ω * π * ∑ t, (s.f₁ t - s.f₀ t) * s.f₁ t / fpi s π t := by
  unfold Rs R V0s V1s V W rho rhoRaw fpi
  rw [← Finset.sum_sub_distrib, Finset.mul_sum]
  exact Finset.sum_congr rfl (fun t _ => by ring)

/-- p. 9. `ω Σ f₁ − R(π) = ω Σ_t f₁(t) f₀(t) / f(π, t)`, so `R(π) < ω` on `[0, 1]`
and investment falls short of the social optimum `G(ω)`. -/
theorem R_lt_omega (s : Signal κ) {ω π : ℝ} (hω : 0 < ω) (hπ : π ∈ Set.Icc (0 : ℝ) 1) :
    ω - Rs s ω π = ω * ∑ t, s.f₁ t * s.f₀ t / fpi s π t ∧ Rs s ω π < ω := by
  have hid : ω - Rs s ω π = ω * ∑ t, s.f₁ t * s.f₀ t / fpi s π t := by
    have key : ∑ t, (ω * s.f₁ t - ω * π * ((s.f₁ t - s.f₀ t) * s.f₁ t / fpi s π t))
        = ω * ∑ t, s.f₁ t * s.f₀ t / fpi s π t := by
      rw [Finset.mul_sum]
      refine Finset.sum_congr rfl (fun t _ => ?_)
      have hF := mul_inv_cancel₀ (fpi_pos s hπ t).ne'
      unfold fpi at hF ⊢
      simp only [div_eq_mul_inv]
      linear_combination (-(ω * s.f₁ t)) * hF
    rw [Finset.sum_sub_distrib, ← Finset.mul_sum, ← Finset.mul_sum, s.sum₁] at key
    rw [eq_4]
    linarith [key]
  refine ⟨hid, ?_⟩
  have : Nonempty κ := by
    by_contra h
    rw [not_nonempty_iff] at h
    have := s.sum₀
    simp at this
  have : 0 < ∑ t, s.f₁ t * s.f₀ t / fpi s π t :=
    Finset.sum_pos (fun t _ => div_pos (mul_pos (s.pos₁ t) (s.pos₀ t)) (fpi_pos s hπ t))
      Finset.univ_nonempty
  nlinarith

/-- The derivative of the posterior in `π`, `∂ρ/∂π = f₁ f₀ / f²`. -/
theorem hasDerivAt_rhoRaw {f₀ f₁ π : ℝ} (h : π * f₁ + (1 - π) * f₀ ≠ 0) :
    HasDerivAt (rhoRaw f₀ f₁) (f₁ * f₀ / (π * f₁ + (1 - π) * f₀) ^ 2) π := by
  have hn : HasDerivAt (fun p : ℝ => p * f₁) f₁ π := by
    simpa using (hasDerivAt_id π).mul_const f₁
  have hd : HasDerivAt (fun p : ℝ => p * f₁ + (1 - p) * f₀) (f₁ - f₀) π := by
    have := ((hasDerivAt_id π).mul_const f₁).add
      (((hasDerivAt_const π (1 : ℝ)).sub (hasDerivAt_id π)).mul_const f₀)
    convert this using 1
    simp; ring
  have := hn.div hd h
  unfold rhoRaw
  convert this using 1
  field_simp
  ring

/-- Eqs. (7) and (8). `V_e′(π) = Σ_t ω f_e(t) f₁(t) f₀(t) / f(π, t)²`. -/
theorem eq_7_8 (s : Signal κ) (ω : ℝ) (fe : κ → ℝ) {π : ℝ} (hπ : π ∈ Set.Icc (0 : ℝ) 1) :
    HasDerivAt (V s ω fe) (∑ t, ω * fe t * s.f₁ t * s.f₀ t / fpi s π t ^ 2) π := by
  have h : ∀ t ∈ (univ : Finset κ), HasDerivAt (fun p => fe t * W s ω p t)
      (ω * fe t * s.f₁ t * s.f₀ t / fpi s π t ^ 2) π := by
    intro t _
    have := ((hasDerivAt_rhoRaw (fpi_pos s hπ t).ne').const_mul ω).const_mul (fe t)
    convert this using 1
    unfold fpi; ring
  have := HasDerivAt.fun_sum h
  convert this using 1

/-- `R′(π)`, the right side of eq. (5). -/
noncomputable def Rp (s : Signal κ) (ω π : ℝ) : ℝ :=
  ∑ t, ω * (s.f₁ t - s.f₀ t) * s.f₁ t * s.f₀ t / fpi s π t ^ 2

/-- Eq. (5). -/
theorem eq_5 (s : Signal κ) (ω : ℝ) {π : ℝ} (hπ : π ∈ Set.Icc (0 : ℝ) 1) :
    HasDerivAt (Rs s ω) (Rp s ω π) π := by
  have := (eq_7_8 s ω s.f₁ hπ).sub (eq_7_8 s ω s.f₀ hπ)
  unfold Rs R V0s V1s Rp
  convert this using 1
  rw [← Finset.sum_sub_distrib]
  exact Finset.sum_congr rfl (fun t _ => by ring)

/-- Eq. (6). `R″(π) = −2ω Σ_t (f₁ − f₀)² f₁ f₀ / f³`. -/
theorem eq_6 (s : Signal κ) (ω : ℝ) {π : ℝ} (hπ : π ∈ Set.Icc (0 : ℝ) 1) :
    HasDerivAt (Rp s ω)
      (-2 * ω * ∑ t, (s.f₁ t - s.f₀ t) ^ 2 * s.f₁ t * s.f₀ t / fpi s π t ^ 3) π := by
  have h : ∀ t ∈ (univ : Finset κ), HasDerivAt
      (fun p => ω * (s.f₁ t - s.f₀ t) * s.f₁ t * s.f₀ t / fpi s p t ^ 2)
      (-2 * ω * ((s.f₁ t - s.f₀ t) ^ 2 * s.f₁ t * s.f₀ t / fpi s π t ^ 3)) π := by
    intro t _
    have hd : HasDerivAt (fun p : ℝ => fpi s p t) (s.f₁ t - s.f₀ t) π := by
      have := ((hasDerivAt_id π).mul_const (s.f₁ t)).add
        (((hasDerivAt_const π (1 : ℝ)).sub (hasDerivAt_id π)).mul_const (s.f₀ t))
      unfold fpi
      convert this using 1
      simp; ring
    have hne := (fpi_pos s hπ t).ne'
    have := (hasDerivAt_const π (ω * (s.f₁ t - s.f₀ t) * s.f₁ t * s.f₀ t)).div (hd.pow 2)
      (pow_ne_zero 2 hne)
    convert this using 1
    simp only [Pi.pow_apply]
    field_simp
    ring
  have := HasDerivAt.fun_sum h
  unfold Rp
  rw [Finset.mul_sum]
  convert this using 1

/-- `V_e′(π)`, the right side of eqs. (7) and (8). -/
noncomputable def Vp (s : Signal κ) (ω : ℝ) (fe : κ → ℝ) (π : ℝ) : ℝ :=
  ∑ t, ω * fe t * s.f₁ t * s.f₀ t / fpi s π t ^ 2

theorem Rp_eq (s : Signal κ) (ω π : ℝ) : Rp s ω π = Vp s ω s.f₁ π - Vp s ω s.f₀ π := by
  unfold Rp Vp
  rw [← Finset.sum_sub_distrib]
  exact Finset.sum_congr rfl (fun t _ => by ring)

theorem sqpos {a b : ℝ} (h : a ≠ b) : 0 < (a - b) ^ 2 :=
  lt_of_le_of_ne (sq_nonneg _) (Ne.symm (pow_ne_zero 2 (sub_ne_zero.2 h)))

theorem nonempty_of_signal (s : Signal κ) : Nonempty κ := by
  by_contra h
  rw [not_nonempty_iff] at h
  have := s.sum₀
  simp at this

/-- Eq. (6), its sign. `R″(π) < 0` when the signal is informative at some `t`. -/
theorem eq_6_neg (s : Signal κ) {ω π : ℝ} (hω : 0 < ω) (hπ : π ∈ Set.Icc (0 : ℝ) 1)
    (hinf : ∃ t, s.f₁ t ≠ s.f₀ t) :
    -2 * ω * ∑ t, (s.f₁ t - s.f₀ t) ^ 2 * s.f₁ t * s.f₀ t / fpi s π t ^ 3 < 0 := by
  obtain ⟨t₀, ht₀⟩ := hinf
  have hpos : 0 < ∑ t, (s.f₁ t - s.f₀ t) ^ 2 * s.f₁ t * s.f₀ t / fpi s π t ^ 3 := by
    apply Finset.sum_pos'
    · intro t _
      exact div_nonneg (mul_nonneg (mul_nonneg (sq_nonneg _) (s.pos₁ t).le) (s.pos₀ t).le)
        (pow_pos (fpi_pos s hπ t) 3).le
    · exact ⟨t₀, Finset.mem_univ _, div_pos (mul_pos (mul_pos (sqpos ht₀) (s.pos₁ t₀))
        (s.pos₀ t₀)) (pow_pos (fpi_pos s hπ t₀) 3)⟩
  nlinarith

/-- p. 8. `R` is strictly concave on `[0, 1]` when the signal is informative. -/
theorem R_strictConcaveOn (s : Signal κ) {ω : ℝ} (hω : 0 < ω) (hinf : ∃ t, s.f₁ t ≠ s.f₀ t) :
    StrictConcaveOn ℝ (Set.Icc 0 1) (Rs s ω) := by
  apply StrictAntiOn.strictConcaveOn_of_deriv (convex_Icc 0 1)
  · exact fun x hx => (eq_5 s ω hx).continuousAt.continuousWithinAt
  · rw [interior_Icc]
    have anti : StrictAntiOn (Rp s ω) (Set.Ioo 0 1) := by
      apply strictAntiOn_of_deriv_neg (convex_Ioo 0 1)
      · exact fun x hx => (eq_6 s ω (Set.Ioo_subset_Icc_self hx)).continuousAt.continuousWithinAt
      · intro x hx
        rw [interior_Ioo] at hx
        rw [(eq_6 s ω (Set.Ioo_subset_Icc_self hx)).deriv]
        exact eq_6_neg s hω (Set.Ioo_subset_Icc_self hx) hinf
    exact anti.congr (fun x hx => ((eq_5 s ω (Set.Ioo_subset_Icc_self hx)).deriv).symm)

/-- p. 8. `R′(0) = ω Σ_t (f₁ − f₀)² / f₀ > 0`. -/
theorem Rp_zero (s : Signal κ) {ω : ℝ} (hω : 0 < ω) (hinf : ∃ t, s.f₁ t ≠ s.f₀ t) :
    Rp s ω 0 = ω * ∑ t, (s.f₁ t - s.f₀ t) ^ 2 / s.f₀ t ∧ 0 < Rp s ω 0 := by
  have hid : Rp s ω 0 = ω * ∑ t, (s.f₁ t - s.f₀ t) ^ 2 / s.f₀ t := by
    have term : ∀ t ∈ (univ : Finset κ), ω * (s.f₁ t - s.f₀ t) * s.f₁ t * s.f₀ t / fpi s 0 t ^ 2
        = ω * ((s.f₁ t - s.f₀ t) ^ 2 / s.f₀ t) + ω * (s.f₁ t - s.f₀ t) := by
      intro t _
      have := (s.pos₀ t).ne'
      simp only [fpi, zero_mul, sub_zero, one_mul, zero_add]; field_simp; ring
    unfold Rp
    rw [Finset.sum_congr rfl term, Finset.sum_add_distrib, ← Finset.mul_sum, ← Finset.mul_sum,
      Finset.sum_sub_distrib, s.sum₀, s.sum₁]
    ring
  refine ⟨hid, ?_⟩
  obtain ⟨t₀, ht₀⟩ := hinf
  have : 0 < ∑ t, (s.f₁ t - s.f₀ t) ^ 2 / s.f₀ t := by
    exact Finset.sum_pos' (fun t _ => div_nonneg (sq_nonneg (s.f₁ t - s.f₀ t)) (s.pos₀ t).le)
      ⟨t₀, Finset.mem_univ _, div_pos (sqpos ht₀) (s.pos₀ t₀)⟩
  rw [hid]; exact mul_pos hω this

/-- p. 8. `R′(1) = −ω Σ_t (f₁ − f₀)² / f₁ < 0`. -/
theorem Rp_one (s : Signal κ) {ω : ℝ} (hω : 0 < ω) (hinf : ∃ t, s.f₁ t ≠ s.f₀ t) :
    Rp s ω 1 = -(ω * ∑ t, (s.f₁ t - s.f₀ t) ^ 2 / s.f₁ t) ∧ Rp s ω 1 < 0 := by
  have hid : Rp s ω 1 = -(ω * ∑ t, (s.f₁ t - s.f₀ t) ^ 2 / s.f₁ t) := by
    have term : ∀ t ∈ (univ : Finset κ), ω * (s.f₁ t - s.f₀ t) * s.f₁ t * s.f₀ t / fpi s 1 t ^ 2
        = -(ω * ((s.f₁ t - s.f₀ t) ^ 2 / s.f₁ t)) + ω * (s.f₁ t - s.f₀ t) := by
      intro t _
      have := (s.pos₁ t).ne'
      simp only [fpi, one_mul, sub_self, zero_mul, add_zero]; field_simp; ring
    unfold Rp
    rw [Finset.sum_congr rfl term, Finset.sum_add_distrib, Finset.sum_neg_distrib,
      ← Finset.mul_sum, ← Finset.mul_sum, Finset.sum_sub_distrib, s.sum₀, s.sum₁]
    ring
  refine ⟨hid, ?_⟩
  obtain ⟨t₀, ht₀⟩ := hinf
  have : 0 < ∑ t, (s.f₁ t - s.f₀ t) ^ 2 / s.f₁ t := by
    exact Finset.sum_pos' (fun t _ => div_nonneg (sq_nonneg (s.f₁ t - s.f₀ t)) (s.pos₁ t).le)
      ⟨t₀, Finset.mem_univ _, div_pos (sqpos ht₀) (s.pos₁ t₀)⟩
  rw [hid]; nlinarith [mul_pos hω this]

/-- p. 8. `V₀′(0) = ω` and `V₁′(1) = ω`. -/
theorem Vp_ends (s : Signal κ) (ω : ℝ) : Vp s ω s.f₀ 0 = ω ∧ Vp s ω s.f₁ 1 = ω := by
  constructor
  · have : ∀ t ∈ (univ : Finset κ), ω * s.f₀ t * s.f₁ t * s.f₀ t / fpi s 0 t ^ 2 = ω * s.f₁ t := by
      intro t _
      have := (s.pos₀ t).ne'
      simp only [fpi, zero_mul, sub_zero, one_mul, zero_add]; field_simp
    unfold Vp; rw [Finset.sum_congr rfl this, ← Finset.mul_sum, s.sum₁, mul_one]
  · have : ∀ t ∈ (univ : Finset κ), ω * s.f₁ t * s.f₁ t * s.f₀ t / fpi s 1 t ^ 2 = ω * s.f₀ t := by
      intro t _
      have := (s.pos₁ t).ne'
      simp only [fpi, one_mul, sub_self, zero_mul, add_zero]; field_simp
    unfold Vp; rw [Finset.sum_congr rfl this, ← Finset.mul_sum, s.sum₀, mul_one]

/-- p. 8. `V₁′(0) > ω` and `V₀′(1) > ω`. -/
theorem Vp_cross (s : Signal κ) {ω : ℝ} (hω : 0 < ω) (hinf : ∃ t, s.f₁ t ≠ s.f₀ t) :
    ω < Vp s ω s.f₁ 0 ∧ ω < Vp s ω s.f₀ 1 := by
  have h0 := (Rp_zero s hω hinf).2
  have h1 := (Rp_one s hω hinf).2
  have e := Vp_ends s ω
  rw [Rp_eq] at h0 h1
  constructor <;> linarith [e.1, e.2]



/-- Eq. (9). `U(π, c)` is non-increasing in `c`. -/
theorem U_antitone_c (V₀ V₁ : ℝ → ℝ) (π : ℝ) : Antitone (U V₀ V₁ π) := by
  intro x y h
  unfold U
  exact max_le_max (by linarith) le_rfl

/-- p. 9. With `V_e` strictly increasing on `[0, 1]`, `ΔU(πA, πB; c) > 0` for every
`c` if and only if `πA > πB`. -/
theorem dU_pos_iff {V₀ V₁ : ℝ → ℝ} (hV₀ : StrictMonoOn V₀ (Set.Icc 0 1))
    (hV₁ : StrictMonoOn V₁ (Set.Icc 0 1)) {πA πB : ℝ} (hA : πA ∈ Set.Icc (0 : ℝ) 1)
    (hB : πB ∈ Set.Icc (0 : ℝ) 1) : (∀ c, 0 < dU V₀ V₁ πA πB c) ↔ πB < πA := by
  constructor
  · intro h
    by_contra hle
    push_neg at hle
    have h0 := hV₀.monotoneOn hA hB hle
    have h1 := hV₁.monotoneOn hA hB hle
    have := h 0
    unfold dU U at this
    have : max (V₁ πA - 0) (V₀ πA) ≤ max (V₁ πB - 0) (V₀ πB) :=
      max_le_max (by linarith) h0
    linarith
  · intro hlt c
    exact prop1_pointwise (hV₀ hB hA hlt) (hV₁ hB hA hlt) c

/-- In the signal model, both wages rise strictly in the reputation, so
Proposition 1 applies to any `πB < πA` in `[0, 1]`. -/
theorem prop1_signal_wages (s : Signal κ) {ω : ℝ} (hω : 0 < ω) {πA πB : ℝ}
    (hA : πA ∈ Set.Icc (0 : ℝ) 1) (hB : πB ∈ Set.Icc (0 : ℝ) 1) (h : πB < πA) :
    V0s s ω πB < V0s s ω πA ∧ V1s s ω πB < V1s s ω πA :=
  ⟨V_strictMonoOn s hω s.pos₀ hB hA h, V_strictMonoOn s hω s.pos₁ hB hA h⟩

/-- Between the threshold and above it, `ΔU` falls strictly: for `c j < R(πA) ≤ c k`
and `R(πB) < R(πA)`, `ΔU(c k) < ΔU(c j)`. -/
theorem dU_gap {V₀ V₁ : ℝ → ℝ} {πA πB x y : ℝ} (hR : R V₀ V₁ πB < R V₀ V₁ πA)
    (hx : x < R V₀ V₁ πA) (hy : R V₀ V₁ πA ≤ y) :
    dU V₀ V₁ πA πB y < dU V₀ V₁ πA πB x := by
  simp only [dU, eq_15]
  unfold R at *
  simp only [max_def]
  split_ifs <;> linarith

/-! ### The finite cost distribution -/

variable {ι : Type*} [Fintype ι]

/-- `G(r)`, the mass of cost types at or below `r`. -/
noncomputable def G (c g : ι → ℝ) (r : ℝ) : ℝ :=
  ∑ j ∈ univ.filter (fun j => c j ≤ r), g j

/-- The investment rate among the members of a group whose members are drawn
with weights `w`, when members invest at costs up to `r`. -/
noncomputable def selRate (c g w : ι → ℝ) (r : ℝ) : ℝ :=
  (∑ j ∈ univ.filter (fun j => c j ≤ r), g j * w j) / ∑ j, g j * w j

/-- Eq. (10). `Σ^A`, the fraction choosing affect A. -/
noncomputable def SigmaA (V₀ V₁ H : ℝ → ℝ) (c g : ι → ℝ) (πA πB : ℝ) : ℝ :=
  ∑ j, g j * H (dU V₀ V₁ πA πB (c j))

/-- Eq. (14). `φ(πA, πB) = σ^A / Σ^A`, the actual investment rate in group A
given the beliefs `(πA, πB)`. With the arguments swapped it is `φ(πB, πA)`, the
rate in group B. -/
noncomputable def phi (V₀ V₁ H : ℝ → ℝ) (c g : ι → ℝ) (πA πB : ℝ) : ℝ :=
  selRate c g (fun j => H (dU V₀ V₁ πA πB (c j))) (R V₀ V₁ πA)

/-- The Phenotypic Stereotyping Equilibrium of p. 8, `π̂ = G(R(π̂))`. -/
def IsPSE (V₀ V₁ : ℝ → ℝ) (c g : ι → ℝ) (x : ℝ) : Prop := x = G c g (R V₀ V₁ x)

/-- The Affective Stereotyping Equilibrium of p. 10. The employers' belief about
each group equals the actual investment rate of the group's members,
`π*_A = φ(π*_A, π*_B)` and `π*_B = φ(π*_B, π*_A)`. -/
def IsASE (V₀ V₁ H : ℝ → ℝ) (c g : ι → ℝ) (πA πB : ℝ) : Prop :=
  πA = phi V₀ V₁ H c g πA πB ∧ πB = phi V₀ V₁ H c g πB πA

/-- The symmetry `H(k) = 1 − H(−k)` gives `H(0) = 1/2`. -/
theorem H_zero {H : ℝ → ℝ} (hH : ∀ k, H k = 1 - H (-k)) : H 0 = 1 / 2 := by
  have := hH 0; simp at this; linarith

/-- Proposition 1. With `V_e(πB) < V_e(πA)`, a symmetric and strictly increasing
`H`, and a cost distribution of total mass 1, more than half the workers choose
affect A, `Σ^A > .5`. -/
theorem prop1 {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ} {πA πB : ℝ}
    (h0 : V₀ πB < V₀ πA) (h1 : V₁ πB < V₁ πA)
    (hH : ∀ k, H k = 1 - H (-k)) (hmono : StrictMono H)
    (hg : ∀ j, 0 ≤ g j) (hsum : ∑ j, g j = 1) :
    1 / 2 < SigmaA V₀ V₁ H c g πA πB := by
  have hpos : ∃ j ∈ (univ : Finset ι), 0 < g j * (H (dU V₀ V₁ πA πB (c j)) - 1 / 2) := by
    by_contra hne
    push_neg at hne
    have hall : ∀ j, g j = 0 := by
      intro j
      have hd := hmono (prop1_pointwise h0 h1 (c j))
      rw [H_zero hH] at hd
      have := hne j (mem_univ j)
      have hgj := hg j
      nlinarith
    simp [hall] at hsum
  have hs : 0 < ∑ j, g j * (H (dU V₀ V₁ πA πB (c j)) - 1 / 2) := by
    apply Finset.sum_pos' _ hpos
    intro j _
    have hd := hmono (prop1_pointwise h0 h1 (c j))
    rw [H_zero hH] at hd
    exact mul_nonneg (hg j) (by linarith)
  have e : ∑ j, g j * (H (dU V₀ V₁ πA πB (c j)) - 1 / 2)
      = SigmaA V₀ V₁ H c g πA πB - 1 / 2 * ∑ j, g j := by
    unfold SigmaA
    rw [Finset.mul_sum, ← Finset.sum_sub_distrib]
    exact Finset.sum_congr rfl (fun j _ => by ring)
  rw [e, hsum] at hs
  linarith

/-- The core of Lemma 2. If a group draws its members with weights that are
higher for every type that invests than for every type that does not, its
investment rate is at least `G(r)`, the rate under random membership. -/
theorem selRate_ge {c g w : ι → ℝ} {r : ℝ} (hg : ∀ j, 0 ≤ g j)
    (hsum : ∑ j, g j = 1) (hpos : 0 < ∑ j, g j * w j)
    (hw : ∀ j k, c j ≤ r → r < c k → w k ≤ w j) :
    G c g r ≤ selRate c g w r := by
  unfold selRate G
  set S := univ.filter (fun j => c j ≤ r)
  set T := univ.filter (fun j => ¬ c j ≤ r)
  have split1 : ∑ j, g j = ∑ j ∈ S, g j + ∑ j ∈ T, g j :=
    (Finset.sum_filter_add_sum_filter_not _ _ _).symm
  have split2 : ∑ j, g j * w j = ∑ j ∈ S, g j * w j + ∑ j ∈ T, g j * w j :=
    (Finset.sum_filter_add_sum_filter_not _ _ _).symm
  have key : (∑ j ∈ S, g j * w j) * (∑ k ∈ T, g k) - (∑ j ∈ S, g j) * (∑ k ∈ T, g k * w k)
      = ∑ j ∈ S, ∑ k ∈ T, g j * g k * (w j - w k) := by
    rw [Finset.sum_mul_sum, Finset.sum_mul_sum, ← Finset.sum_sub_distrib]
    refine Finset.sum_congr rfl (fun j _ => ?_)
    rw [← Finset.sum_sub_distrib]
    exact Finset.sum_congr rfl (fun k _ => by ring)
  have nonneg : 0 ≤ ∑ j ∈ S, ∑ k ∈ T, g j * g k * (w j - w k) := by
    apply Finset.sum_nonneg
    intro j hj
    apply Finset.sum_nonneg
    intro k hk
    have hj' : c j ≤ r := (Finset.mem_filter.1 hj).2
    have hk' : r < c k := lt_of_not_ge (Finset.mem_filter.1 hk).2
    exact mul_nonneg (mul_nonneg (hg j) (hg k)) (by linarith [hw j k hj' hk'])
  rw [le_div_iff₀ hpos]
  rw [split1] at hsum
  rw [split2]
  have e : ∑ j ∈ S, g j * w j = (∑ j ∈ S, g j * w j) * (∑ j ∈ S, g j + ∑ j ∈ T, g j) := by
    rw [hsum, mul_one]
  nlinarith [key, nonneg, e]

/-- The mirror of `selRate_ge`. Weights higher for every type that does not
invest give an investment rate of at most `G(r)`. -/
theorem selRate_le {c g w : ι → ℝ} {r : ℝ} (hg : ∀ j, 0 ≤ g j)
    (hsum : ∑ j, g j = 1) (hpos : 0 < ∑ j, g j * w j)
    (hw : ∀ j k, c j ≤ r → r < c k → w j ≤ w k) :
    selRate c g w r ≤ G c g r := by
  unfold selRate G
  set S := univ.filter (fun j => c j ≤ r)
  set T := univ.filter (fun j => ¬ c j ≤ r)
  have split1 : ∑ j, g j = ∑ j ∈ S, g j + ∑ j ∈ T, g j :=
    (Finset.sum_filter_add_sum_filter_not _ _ _).symm
  have split2 : ∑ j, g j * w j = ∑ j ∈ S, g j * w j + ∑ j ∈ T, g j * w j :=
    (Finset.sum_filter_add_sum_filter_not _ _ _).symm
  have key : (∑ j ∈ S, g j) * (∑ k ∈ T, g k * w k) - (∑ j ∈ S, g j * w j) * (∑ k ∈ T, g k)
      = ∑ j ∈ S, ∑ k ∈ T, g j * g k * (w k - w j) := by
    rw [Finset.sum_mul_sum, Finset.sum_mul_sum, ← Finset.sum_sub_distrib]
    refine Finset.sum_congr rfl (fun j _ => ?_)
    rw [← Finset.sum_sub_distrib]
    exact Finset.sum_congr rfl (fun k _ => by ring)
  have nonneg : 0 ≤ ∑ j ∈ S, ∑ k ∈ T, g j * g k * (w k - w j) := by
    apply Finset.sum_nonneg
    intro j hj
    apply Finset.sum_nonneg
    intro k hk
    have hj' : c j ≤ r := (Finset.mem_filter.1 hj).2
    have hk' : r < c k := lt_of_not_ge (Finset.mem_filter.1 hk).2
    exact mul_nonneg (mul_nonneg (hg j) (hg k)) (by linarith [hw j k hj' hk'])
  rw [div_le_iff₀ hpos]
  rw [split1] at hsum
  rw [split2]
  have e : ∑ j ∈ S, g j * w j = (∑ j ∈ S, g j * w j) * (∑ j ∈ S, g j + ∑ j ∈ T, g j) := by
    rw [hsum, mul_one]
  nlinarith [key, nonneg, e]

/-- Lemma 2 and Proposition 2, the selection result, in the finite case.
Whenever `R(πA) ≥ R(πB)`, the investment rate of group A is at least
`G(R(πA))` and that of group B at most `G(R(πB))`. The more able (low `c`)
types are drawn to the group with the greater return to skill. -/
theorem prop2 {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ} {πA πB : ℝ}
    (hR : R V₀ V₁ πB ≤ R V₀ V₁ πA) (hmono : Monotone H)
    (hg : ∀ j, 0 ≤ g j) (hsum : ∑ j, g j = 1)
    (hA : 0 < ∑ j, g j * H (dU V₀ V₁ πA πB (c j)))
    (hB : 0 < ∑ j, g j * H (dU V₀ V₁ πB πA (c j))) :
    G c g (R V₀ V₁ πA) ≤ phi V₀ V₁ H c g πA πB ∧
      phi V₀ V₁ H c g πB πA ≤ G c g (R V₀ V₁ πB) := by
  have anti := dU_antitone hR
  refine ⟨selRate_ge hg hsum hA (fun j k hj hk => ?_), selRate_le hg hsum hB (fun j k hj hk => ?_)⟩
  · exact hmono (anti (by linarith))
  · apply hmono
    rw [dU_swap V₀ V₁ πA πB (c j), dU_swap V₀ V₁ πA πB (c k)]
    have := anti (show c j ≤ c k by linarith)
    linarith

/-- Every PSE gives a trivial ASE, `(x, x) ∈ Ω_KL` if `x ∈ Ω_CL` (p. 10). With
equal beliefs `ΔU = 0`, every type chooses A with probability `H(0) = 1/2`, and
`φ(x, x) = G(R(x))`. -/
theorem trivial_ase {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ} {x : ℝ}
    (hH : ∀ k, H k = 1 - H (-k)) (hsum : ∑ j, g j = 1) (hx : IsPSE V₀ V₁ c g x) :
    IsASE V₀ V₁ H c g x x := by
  have h0 := H_zero hH
  have hd : ∀ y, dU V₀ V₁ x x y = 0 := fun y => by unfold dU; ring
  have hphi : phi V₀ V₁ H c g x x = G c g (R V₀ V₁ x) := by
    unfold phi selRate G
    simp only [hd, h0]
    rw [← Finset.sum_mul, ← Finset.sum_mul, hsum]
    field_simp
  exact ⟨hx.trans hphi.symm, hx.trans hphi.symm⟩

/-- Control: with equal returns, `R(πA) = R(πB)`, there is no selection, even
when `πA ≠ πB`. `ΔU` is the same for every cost type, so group A's investment
rate is `G(R(πA))` exactly (Lemma 2, last sentence). The selection of
`prop2` needs a difference in returns, not only a difference in beliefs. -/
theorem control_no_selection_equal_return {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ}
    {πA πB : ℝ} (hR : R V₀ V₁ πA = R V₀ V₁ πB) (hsum : ∑ j, g j = 1)
    (hpos : 0 < ∑ j, g j * H (dU V₀ V₁ πA πB (c j))) :
    phi V₀ V₁ H c g πA πB = G c g (R V₀ V₁ πA) := by
  have hconst : ∀ y, dU V₀ V₁ πA πB y = V₀ πA - V₀ πB := by
    intro y
    rw [dU, eq_15, eq_15, hR]; ring
  have hw : ∑ j, g j * H (dU V₀ V₁ πA πB (c j)) = H (V₀ πA - V₀ πB) := by
    simp only [hconst]; rw [← Finset.sum_mul, hsum, one_mul]
  unfold phi selRate G
  rw [hw]
  simp only [hconst]
  rw [← Finset.sum_mul]
  have hne : H (V₀ πA - V₀ πB) ≠ 0 := by rw [← hw]; exact hpos.ne'
  field_simp

/-- Eq. (12). With the symmetry of `H`, `Σ^B = 1 − Σ^A`. -/
theorem sigmaB_eq {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ} {πA πB : ℝ}
    (hH : ∀ k, H k = 1 - H (-k)) (hsum : ∑ j, g j = 1) :
    SigmaA V₀ V₁ H c g πB πA = 1 - SigmaA V₀ V₁ H c g πA πB := by
  unfold SigmaA
  have e : ∀ j ∈ (univ : Finset ι), g j * H (dU V₀ V₁ πB πA (c j))
      = g j - g j * H (dU V₀ V₁ πA πB (c j)) := by
    intro j _
    rw [dU_swap, hH (-(dU V₀ V₁ πA πB (c j))), neg_neg]
    have := hH (dU V₀ V₁ πA πB (c j))
    linarith [this]
  rw [Finset.sum_congr rfl e, Finset.sum_sub_distrib, hsum]

/-- Proposition 1, both groups. `Σ^A > .5 > Σ^B` when `πA > πB`. -/
theorem prop1_both {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ} {πA πB : ℝ}
    (h0 : V₀ πB < V₀ πA) (h1 : V₁ πB < V₁ πA)
    (hH : ∀ k, H k = 1 - H (-k)) (hmono : StrictMono H)
    (hg : ∀ j, 0 ≤ g j) (hsum : ∑ j, g j = 1) :
    1 / 2 < SigmaA V₀ V₁ H c g πA πB ∧ SigmaA V₀ V₁ H c g πB πA < 1 / 2 := by
  have := prop1 (c := c) h0 h1 hH hmono hg hsum
  have e := sigmaB_eq (V₀ := V₀) (V₁ := V₁) (c := c) (πA := πA) (πB := πB) hH hsum
  constructor <;> linarith

/-- The strict form of `selRate_ge`, with positive mass on a type that invests
and a type that does not, and a strictly higher weight on the first. -/
theorem selRate_gt {c g w : ι → ℝ} {r : ℝ} (hg : ∀ j, 0 ≤ g j)
    (hsum : ∑ j, g j = 1) (hpos : 0 < ∑ j, g j * w j)
    (hw : ∀ j k, c j ≤ r → r < c k → w k ≤ w j)
    (hs : ∃ j k, c j ≤ r ∧ r < c k ∧ 0 < g j ∧ 0 < g k ∧ w k < w j) :
    G c g r < selRate c g w r := by
  unfold selRate G
  set S := univ.filter (fun j => c j ≤ r)
  set T := univ.filter (fun j => ¬ c j ≤ r)
  have split1 : ∑ j, g j = ∑ j ∈ S, g j + ∑ j ∈ T, g j :=
    (Finset.sum_filter_add_sum_filter_not _ _ _).symm
  have split2 : ∑ j, g j * w j = ∑ j ∈ S, g j * w j + ∑ j ∈ T, g j * w j :=
    (Finset.sum_filter_add_sum_filter_not _ _ _).symm
  have key : (∑ j ∈ S, g j * w j) * (∑ k ∈ T, g k) - (∑ j ∈ S, g j) * (∑ k ∈ T, g k * w k)
      = ∑ j ∈ S, ∑ k ∈ T, g j * g k * (w j - w k) := by
    rw [Finset.sum_mul_sum, Finset.sum_mul_sum, ← Finset.sum_sub_distrib]
    refine Finset.sum_congr rfl (fun j _ => ?_)
    rw [← Finset.sum_sub_distrib]
    exact Finset.sum_congr rfl (fun k _ => by ring)
  have term : ∀ j ∈ S, ∀ k ∈ T, 0 ≤ g j * g k * (w j - w k) := by
    intro j hj k hk
    have hj' : c j ≤ r := (Finset.mem_filter.1 hj).2
    have hk' : r < c k := lt_of_not_ge (Finset.mem_filter.1 hk).2
    exact mul_nonneg (mul_nonneg (hg j) (hg k)) (by linarith [hw j k hj' hk'])
  obtain ⟨j₀, k₀, hj₀, hk₀, gj, gk, hw₀⟩ := hs
  have hj₀S : j₀ ∈ S := Finset.mem_filter.2 ⟨Finset.mem_univ _, hj₀⟩
  have hk₀T : k₀ ∈ T := Finset.mem_filter.2 ⟨Finset.mem_univ _, not_le.2 hk₀⟩
  have pos : 0 < ∑ j ∈ S, ∑ k ∈ T, g j * g k * (w j - w k) := by
    apply Finset.sum_pos' (fun j hj => Finset.sum_nonneg (term j hj))
    refine ⟨j₀, hj₀S, Finset.sum_pos' (term j₀ hj₀S) ⟨k₀, hk₀T, ?_⟩⟩
    exact mul_pos (mul_pos gj gk) (sub_pos.2 hw₀)
  rw [lt_div_iff₀ hpos]
  rw [split1] at hsum
  rw [split2]
  have e : ∑ j ∈ S, g j * w j = (∑ j ∈ S, g j * w j) * (∑ j ∈ S, g j + ∑ j ∈ T, g j) := by
    rw [hsum, mul_one]
  nlinarith [key, pos, e]

/-- The strict form of `selRate_le`. -/
theorem selRate_lt {c g w : ι → ℝ} {r : ℝ} (hg : ∀ j, 0 ≤ g j)
    (hsum : ∑ j, g j = 1) (hpos : 0 < ∑ j, g j * w j)
    (hw : ∀ j k, c j ≤ r → r < c k → w j ≤ w k)
    (hs : ∃ j k, c j ≤ r ∧ r < c k ∧ 0 < g j ∧ 0 < g k ∧ w j < w k) :
    selRate c g w r < G c g r := by
  unfold selRate G
  set S := univ.filter (fun j => c j ≤ r)
  set T := univ.filter (fun j => ¬ c j ≤ r)
  have split1 : ∑ j, g j = ∑ j ∈ S, g j + ∑ j ∈ T, g j :=
    (Finset.sum_filter_add_sum_filter_not _ _ _).symm
  have split2 : ∑ j, g j * w j = ∑ j ∈ S, g j * w j + ∑ j ∈ T, g j * w j :=
    (Finset.sum_filter_add_sum_filter_not _ _ _).symm
  have key : (∑ j ∈ S, g j) * (∑ k ∈ T, g k * w k) - (∑ j ∈ S, g j * w j) * (∑ k ∈ T, g k)
      = ∑ j ∈ S, ∑ k ∈ T, g j * g k * (w k - w j) := by
    rw [Finset.sum_mul_sum, Finset.sum_mul_sum, ← Finset.sum_sub_distrib]
    refine Finset.sum_congr rfl (fun j _ => ?_)
    rw [← Finset.sum_sub_distrib]
    exact Finset.sum_congr rfl (fun k _ => by ring)
  have term : ∀ j ∈ S, ∀ k ∈ T, 0 ≤ g j * g k * (w k - w j) := by
    intro j hj k hk
    have hj' : c j ≤ r := (Finset.mem_filter.1 hj).2
    have hk' : r < c k := lt_of_not_ge (Finset.mem_filter.1 hk).2
    exact mul_nonneg (mul_nonneg (hg j) (hg k)) (by linarith [hw j k hj' hk'])
  obtain ⟨j₀, k₀, hj₀, hk₀, gj, gk, hw₀⟩ := hs
  have hj₀S : j₀ ∈ S := Finset.mem_filter.2 ⟨Finset.mem_univ _, hj₀⟩
  have hk₀T : k₀ ∈ T := Finset.mem_filter.2 ⟨Finset.mem_univ _, not_le.2 hk₀⟩
  have pos : 0 < ∑ j ∈ S, ∑ k ∈ T, g j * g k * (w k - w j) := by
    apply Finset.sum_pos' (fun j hj => Finset.sum_nonneg (term j hj))
    refine ⟨j₀, hj₀S, Finset.sum_pos' (term j₀ hj₀S) ⟨k₀, hk₀T, ?_⟩⟩
    exact mul_pos (mul_pos gj gk) (sub_pos.2 hw₀)
  rw [div_lt_iff₀ hpos]
  rw [split1] at hsum
  rw [split2]
  have e : ∑ j ∈ S, g j * w j = (∑ j ∈ S, g j * w j) * (∑ j ∈ S, g j + ∑ j ∈ T, g j) := by
    rw [hsum, mul_one]
  nlinarith [key, pos, e]

/-- Lemma 2 and Proposition 2 with strict inequalities, as the article states
them. With `R(πA) > R(πB)`, `H` strictly increasing, positive mass on a type that
invests in both groups (`c ≤ R(πB)`) and on a type that invests in neither
(`c > R(πA)`), `φ(πA, πB) > G(R(πA))` and `φ(πB, πA) < G(R(πB))`. -/
theorem prop2_strict {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ} {πA πB : ℝ}
    (hR : R V₀ V₁ πB < R V₀ V₁ πA) (hmono : StrictMono H)
    (hg : ∀ j, 0 ≤ g j) (hsum : ∑ j, g j = 1)
    (hA : 0 < ∑ j, g j * H (dU V₀ V₁ πA πB (c j)))
    (hB : 0 < ∑ j, g j * H (dU V₀ V₁ πB πA (c j)))
    (hlo : ∃ j, c j ≤ R V₀ V₁ πB ∧ 0 < g j) (hhi : ∃ k, R V₀ V₁ πA < c k ∧ 0 < g k) :
    G c g (R V₀ V₁ πA) < phi V₀ V₁ H c g πA πB ∧
      phi V₀ V₁ H c g πB πA < G c g (R V₀ V₁ πB) := by
  have anti := dU_antitone hR.le
  obtain ⟨j₀, hj₀, gj⟩ := hlo
  obtain ⟨k₀, hk₀, gk⟩ := hhi
  have gap := dU_gap hR (show c j₀ < R V₀ V₁ πA by linarith) hk₀.le
  refine ⟨selRate_gt hg hsum hA (fun j k hj hk => hmono.monotone (anti (by linarith)))
    ⟨j₀, k₀, by linarith, hk₀, gj, gk, hmono gap⟩, selRate_lt hg hsum hB
    (fun j k hj hk => ?_) ⟨j₀, k₀, hj₀, by linarith, gj, gk, ?_⟩⟩
  · apply hmono.monotone
    rw [dU_swap V₀ V₁ πA πB (c j), dU_swap V₀ V₁ πA πB (c k)]
    have := anti (show c j ≤ c k by linarith)
    linarith
  · apply hmono
    rw [dU_swap V₀ V₁ πA πB (c j₀), dU_swap V₀ V₁ πA πB (c k₀)]
    linarith

/-- Lemma 2, last sentence. With `R(πA) = R(πB)`, even when `πA ≠ πB`,
`φ(πA, πB) = G(R(πA))` and `φ(πB, πA) = G(R(πB))`. -/
theorem lemma2_equal {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ} {πA πB : ℝ}
    (hR : R V₀ V₁ πA = R V₀ V₁ πB) (hsum : ∑ j, g j = 1)
    (hA : 0 < ∑ j, g j * H (dU V₀ V₁ πA πB (c j)))
    (hB : 0 < ∑ j, g j * H (dU V₀ V₁ πB πA (c j))) :
    phi V₀ V₁ H c g πA πB = G c g (R V₀ V₁ πA) ∧
      phi V₀ V₁ H c g πB πA = G c g (R V₀ V₁ πB) :=
  ⟨control_no_selection_equal_return hR hsum hA,
    control_no_selection_equal_return hR.symm hsum hB⟩

/-- p. 10. With equal beliefs, affect does not affect investment,
`φ(πA, πA) = G(R(πA))`. -/
theorem equal_beliefs {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ} {π : ℝ}
    (hH : ∀ k, H k = 1 - H (-k)) (hsum : ∑ j, g j = 1) :
    phi V₀ V₁ H c g π π = G c g (R V₀ V₁ π) := by
  have h0 := H_zero hH
  have hd : ∀ y, dU V₀ V₁ π π y = 0 := fun y => by unfold dU; ring
  unfold phi selRate G
  simp only [hd, h0]
  rw [← Finset.sum_mul, ← Finset.sum_mul, hsum]
  field_simp

/-- Section IV, p. 11. Two distinct PSE `π_h`, `π_l` with `R(π_h) > R(π_l)` do not
form an ASE when the groups take them as their reputations: group `h`'s members
invest at a rate above `G(R(π_h)) = π_h`. -/
theorem not_ase_of_two_pse {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ} {πh πl : ℝ}
    (hh : IsPSE V₀ V₁ c g πh) (hR : R V₀ V₁ πl < R V₀ V₁ πh) (hmono : StrictMono H)
    (hg : ∀ j, 0 ≤ g j) (hsum : ∑ j, g j = 1)
    (hA : 0 < ∑ j, g j * H (dU V₀ V₁ πh πl (c j)))
    (hB : 0 < ∑ j, g j * H (dU V₀ V₁ πl πh (c j)))
    (hlo : ∃ j, c j ≤ R V₀ V₁ πl ∧ 0 < g j) (hhi : ∃ k, R V₀ V₁ πh < c k ∧ 0 < g k) :
    ¬ IsASE V₀ V₁ H c g πh πl := by
  rintro ⟨h1, _⟩
  have := (prop2_strict hR hmono hg hsum hA hB hlo hhi).1
  unfold IsPSE at hh
  linarith

/-- Section IV, p. 11. At two PSE, the skill gap between the entering members of the
two groups exceeds the gap between the groups' reputations,
`φ(π_h, π_l) − φ(π_l, π_h) > π_h − π_l`. -/
theorem cohort_gap {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ} {πh πl : ℝ}
    (hh : IsPSE V₀ V₁ c g πh) (hl : IsPSE V₀ V₁ c g πl)
    (hR : R V₀ V₁ πl < R V₀ V₁ πh) (hmono : StrictMono H)
    (hg : ∀ j, 0 ≤ g j) (hsum : ∑ j, g j = 1)
    (hA : 0 < ∑ j, g j * H (dU V₀ V₁ πh πl (c j)))
    (hB : 0 < ∑ j, g j * H (dU V₀ V₁ πl πh (c j)))
    (hlo : ∃ j, c j ≤ R V₀ V₁ πl ∧ 0 < g j) (hhi : ∃ k, R V₀ V₁ πh < c k ∧ 0 < g k) :
    πh - πl < phi V₀ V₁ H c g πh πl - phi V₀ V₁ H c g πl πh := by
  have := prop2_strict hR hmono hg hsum hA hB hlo hhi
  unfold IsPSE at hh hl
  linarith [this.1, this.2]

/-! ### Control: a better reputation without a greater return

Group A is believed to invest at `πA = .9` and group B at `πB = .6`, with
`V₀(π) = 2π` and `V₁(π) = 3π − π²`, so both wages are higher in A but the
return `R(π) = π − π²` is lower in A (`.09` against `.24`), as in the right
graph of Figure 3, Panel A. `H` is uniform on `[−1, 1]`, which footnote 3
allows. Two cost types, `c = .05` and `c = .5`, have mass `1/2` each. -/

noncomputable def V₀ex (π : ℝ) : ℝ := 2 * π
noncomputable def V₁ex (π : ℝ) : ℝ := 3 * π - π ^ 2
noncomputable def Hu (k : ℝ) : ℝ := max 0 (min 1 (1 / 2 + k / 2))
noncomputable def cex : Fin 2 → ℝ := ![1 / 20, 1 / 2]
noncomputable def gex : Fin 2 → ℝ := ![1 / 2, 1 / 2]

/-- In the example, both wages are higher in group A. -/
theorem example_wages_higher :
    V₀ex (6 / 10) < V₀ex (9 / 10) ∧ V₁ex (6 / 10) < V₁ex (9 / 10) := by
  unfold V₀ex V₁ex; norm_num

/-- Control: a better reputation does not by itself draw the able. Group A has
the better reputation and pays more at every skill level, so more than half
choose A (Proposition 1), yet A's investment rate falls below `G(R(πA))`. The
selection runs toward the group with the greater return to skill, B. -/
theorem control_reputation_without_return :
    phi V₀ex V₁ex Hu cex gex (9 / 10) (6 / 10) < G cex gex (R V₀ex V₁ex (9 / 10)) := by
  unfold phi selRate G R dU U V₀ex V₁ex Hu cex gex
  simp only [Fin.sum_univ_two, Finset.sum_filter, Matrix.cons_val_zero, Matrix.cons_val_one]
  norm_num [max_def, min_def]

/-- Control for `prop1`. Without the symmetry of `H`, the group with the better
reputation can draw fewer than half. With `H` uniform on `[0, 2]`, so `H(0) = 0`,
and one cost type, `Σ^A = H(.45) < 1/2` although both wages are higher in A. -/
theorem control_prop1_needs_symmetry :
    SigmaA V₀ex V₁ex (fun k => max 0 (min 1 (k / 2))) (fun _ : Fin 1 => (1 : ℝ) / 20)
      (fun _ => 1) (9 / 10) (6 / 10) < 1 / 2 := by
  unfold SigmaA dU U V₀ex V₁ex
  simp only [Finset.univ_unique, Finset.sum_singleton]
  norm_num [max_def, min_def]

/-- Control for `R_strictConcaveOn` and `Rp_zero`. With an uninformative signal,
`f₁ = f₀`, the return to skill is zero at every reputation, so neither strict
concavity nor `R′(0) > 0` holds. -/
theorem control_uninformative_flat (s : Signal κ) (ω : ℝ) (h : ∀ t, s.f₁ t = s.f₀ t) (π : ℝ) :
    Rs s ω π = 0 := by
  rw [eq_4]; simp [h]

/-- Control for `prop2_strict`. Without a type that invests in neither group, every
type invests in A and `φ(πA, πB) = G(R(πA)) = 1`. The strict selection needs mass
above the threshold. -/
theorem control_strict_needs_mass_above {V₀ V₁ H : ℝ → ℝ} {c g : ι → ℝ} {πA πB : ℝ}
    (hall : ∀ j, c j ≤ R V₀ V₁ πA) (hsum : ∑ j, g j = 1)
    (hA : 0 < ∑ j, g j * H (dU V₀ V₁ πA πB (c j))) :
    phi V₀ V₁ H c g πA πB = G c g (R V₀ V₁ πA) := by
  have hf : univ.filter (fun j => c j ≤ R V₀ V₁ πA) = univ :=
    Finset.filter_true_of_mem (fun j _ => hall j)
  unfold phi selRate G
  rw [hf, div_self hA.ne', hsum]

end KimLoury2012
