import Mathlib

/-! Pęski and Szentes, "Spontaneous Discrimination", BFI Working Paper
2012-005 (7 May 2012), in the paper's notation as far as Lean allows. The
shock CDF is `G(s) = 1 − e^{−λ s}`; `λ` is a reserved word in Lean, so it is
written `lam`. `γ` is the probability that an employer's social colour moves,
`α` the probability that a moving colour takes the worker's physical colour,
`μc` the share of the employer's own physical colour and `μo` the share of the
other colour. `K = M δ γ / 2r`. A cutoff `x` of colour `c` is the cutoff
`x^c_{−c,−c}` against a worker of the other physical and social colour; the
other three cutoffs are fixed by eq. (3).

Formalised: the cutoff of Lemma 1 for one worker type, the independence of
the employer's own social colour, the best-response function (4) and (9),
Lemma 3 (ii) and (iii), Lemma 4 (ii), Proposition 1 (i), and Proposition 1
(ii) with Remark 1 (existence of the one-sided equilibrium `(x^c, 0)`).
Not formalised: Lemma 5, Proposition 2, the stability results Theorems 1
to 3. -/

namespace PeskiSzentes2012

/-- The CDF of the match shock, `G(s) = 1 − e^{−λ s}`. -/
noncomputable def G (lam s : ℝ) : ℝ := 1 - Real.exp (-(lam * s))

theorem G_zero (lam : ℝ) : G lam 0 = 0 := by simp [G]

theorem G_nonneg {lam s : ℝ} (hl : 0 ≤ lam) (hs : 0 ≤ s) : 0 ≤ G lam s := by
  unfold G
  have : Real.exp (-(lam * s)) ≤ 1 := Real.exp_le_one_iff.2 (by nlinarith)
  linarith

theorem G_mono {lam s t : ℝ} (hl : 0 ≤ lam) (hst : s ≤ t) : G lam s ≤ G lam t := by
  unfold G
  have : Real.exp (-(lam * t)) ≤ Real.exp (-(lam * s)) :=
    Real.exp_le_exp.2 (by nlinarith)
  linarith

/-- Eq. (1). A white employer of social colour `c` hires a worker of type
`(b, w)` at shock `s`. -/
def hireVal (γ α s Vwc Vwb Vww : ℝ) : ℝ :=
  s + (1 - γ) * Vwc + γ * α * Vwb + γ * (1 - α) * Vww

/-- Eq. (2). The same employer does not hire. -/
def noHireVal (γ Vwc Vww : ℝ) : ℝ := (1 - γ) * Vwc + γ * Vww

/-- Lemma 1, the case `x^w_{b,w}`. Hiring beats not hiring exactly when the
shock exceeds `γ α (V_{w,w} − V_{w,b})`. -/
theorem hire_iff (γ α s Vwc Vwb Vww : ℝ) :
    noHireVal γ Vwc Vww ≤ hireVal γ α s Vwc Vwb Vww ↔ γ * α * (Vww - Vwb) ≤ s := by
  unfold hireVal noHireVal
  constructor <;> intro h <;> nlinarith

/-- The employer's own social colour cancels from the comparison of (1) and
(2), so the cutoff cannot depend on it. -/
theorem cutoff_indep_own_social_colour (γ α s Vwc Vwc' Vwb Vww : ℝ) :
    hireVal γ α s Vwc Vwb Vww - noHireVal γ Vwc Vww
      = hireVal γ α s Vwc' Vwb Vww - noHireVal γ Vwc' Vww := by
  unfold hireVal noHireVal; ring

/-- `K = M δ γ / 2r`, below eq. (8). -/
noncomputable def K (M δ γ r : ℝ) : ℝ := M * δ * γ / (2 * r)

/-- Eq. (9). The best response of colour `c` when its own colour uses `x` and
the other colour uses `y`, before truncation at zero. -/
noncomputable def bt (K μc μo α lam x y : ℝ) : ℝ :=
  K * (μc * G lam ((1 - α) * x) + μo * (G lam (α * y) - G lam y))

/-- Eq. (4), `b^c = max {0, b̃^c}`. -/
noncomputable def b (K μc μo α lam x y : ℝ) : ℝ := max 0 (bt K μc μo α lam x y)

/-- Standing conditions on the parameters. -/
structure Params (K μc μo α lam : ℝ) : Prop where
  hK : 0 ≤ K
  hμc : 0 ≤ μc
  hμo : 0 ≤ μo
  hα0 : 0 ≤ α
  hα1 : α ≤ 1
  hlam : 0 ≤ lam

/-- The term carried by the other colour's cutoff is never positive. -/
theorem other_term_nonpos {K μc μo α lam y : ℝ} (P : Params K μc μo α lam) (hy : 0 ≤ y) :
    K * (μo * (G lam (α * y) - G lam y)) ≤ 0 := by
  have h1 : G lam (α * y) ≤ G lam y := G_mono P.hlam (by nlinarith [P.hα1])
  have h2 : μo * (G lam (α * y) - G lam y) ≤ 0 :=
    mul_nonpos_of_nonneg_of_nonpos P.hμo (by linarith)
  exact mul_nonpos_of_nonneg_of_nonpos P.hK h2

/-- Lemma 3 (ii). If the own colour does not discriminate, neither does the
best response, whatever the other colour does. -/
theorem lemma3_ii {K μc μo α lam y : ℝ} (P : Params K μc μo α lam) (hy : 0 ≤ y) :
    b K μc μo α lam 0 y = 0 := by
  unfold b
  have h := other_term_nonpos P hy
  have : bt K μc μo α lam 0 y ≤ 0 := by
    unfold bt; rw [mul_zero, G_zero, mul_zero, zero_add]; exact h
  exact max_eq_left this

/-- Eq. (9) at `y = 0` is `K μc G((1 − α) x)`. -/
theorem bt_at_zero (K μc μo α lam x : ℝ) :
    bt K μc μo α lam x 0 = K * (μc * G lam ((1 - α) * x)) := by
  unfold bt; simp [G_zero]

/-- Lemma 3 (iii), the inequality. Any discrimination by the other colour
lowers the best response, `b^c(x, 0) ≥ b^c(x, y)`. -/
theorem lemma3_iii {K μc μo α lam x y : ℝ} (P : Params K μc μo α lam) (hy : 0 ≤ y) :
    b K μc μo α lam x y ≤ b K μc μo α lam x 0 := by
  unfold b
  apply max_le_max le_rfl
  have h := other_term_nonpos P hy
  rw [bt_at_zero]; unfold bt; nlinarith

/-- Lemma 4 (ii). At or below the threshold `λ ≤ λ₀ = 1/(K(1 − α)μc)`, written
`K μc (1 − α) λ ≤ 1`, the curve `b^c(·, 0)` lies strictly below the 45-degree
line at every positive cutoff. -/
theorem lemma4_ii {K μc μo α lam x : ℝ} (P : Params K μc μo α lam)
    (hthr : K * μc * (1 - α) * lam ≤ 1) (hx : 0 < x) :
    b K μc μo α lam x 0 < x := by
  unfold b
  apply max_lt hx
  rw [bt_at_zero]
  set t := lam * ((1 - α) * x) with ht
  have hKμ : 0 ≤ K * μc := mul_nonneg P.hK P.hμc
  have hG : G lam ((1 - α) * x) = 1 - Real.exp (-t) := rfl
  rw [hG]
  by_cases h0 : t = 0
  · rw [h0]; simp; exact hx
  · have hlt : 1 - Real.exp (-t) < t := by
      have := Real.add_one_lt_exp (show -t ≠ 0 by simpa using h0)
      linarith
    rcases hKμ.lt_or_eq with hpos | hzero
    · have : K * (μc * (1 - Real.exp (-t))) < K * μc * t := by
        rw [← mul_assoc]; exact mul_lt_mul_of_pos_left hlt hpos
      have : K * μc * t = (K * μc * (1 - α) * lam) * x := by rw [ht]; ring
      nlinarith
    · rw [← mul_assoc, ← hzero]; simpa using hx

/-- An equilibrium cutoff profile, eq. (10): `x = b^c(x, y)` and
`y = b^{−c}(y, x)`, where colour `c` has share `μc` and the other `μo`. -/
def IsEq (K μc μo α lam x y : ℝ) : Prop :=
  0 ≤ x ∧ 0 ≤ y ∧ x = b K μc μo α lam x y ∧ y = b K μo μc α lam y x

/-- Proposition 1 (i). If both colours are at or below the threshold, the
colour-blind profile `(0, 0)` is the only equilibrium. -/
theorem prop1_i {K μc μo α lam x y : ℝ} (P : Params K μc μo α lam)
    (hc : K * μc * (1 - α) * lam ≤ 1) (ho : K * μo * (1 - α) * lam ≤ 1)
    (h : IsEq K μc μo α lam x y) : x = 0 ∧ y = 0 := by
  obtain ⟨hx, hy, ex, ey⟩ := h
  have P' : Params K μo μc α lam := ⟨P.hK, P.hμo, P.hμc, P.hα0, P.hα1, P.hlam⟩
  constructor
  · rcases hx.lt_or_eq with hxp | hx0
    · have h1 := lemma3_iii P hy (x := x)
      have h2 := lemma4_ii P hc hxp
      linarith
    · exact hx0.symm
  · rcases hy.lt_or_eq with hyp | hy0
    · have h1 := lemma3_iii P' hx (x := y)
      have h2 := lemma4_ii P' ho hyp
      linarith
    · exact hy0.symm

/-- `1 − e^{−t} ≥ t/(1 + t)` for `t ≥ 0`, used for existence. -/
theorem one_sub_exp_neg_ge {t : ℝ} (ht : 0 ≤ t) : t / (1 + t) ≤ 1 - Real.exp (-t) := by
  have h1 : 1 + t ≤ Real.exp t := by linarith [Real.add_one_le_exp t]
  have hpos : 0 < 1 + t := by linarith
  have h2 : Real.exp (-t) ≤ 1 / (1 + t) := by
    rw [Real.exp_neg, inv_eq_one_div]
    exact one_div_le_one_div_of_le hpos h1
  have h3 : t / (1 + t) = 1 - 1 / (1 + t) := by field_simp; ring
  linarith

/-- Proposition 1 (ii) and Remark 1. Above the threshold, `K μc (1 − α) λ > 1`,
there is a positive cutoff `x` with `(x, 0)` an equilibrium: colour `c`
discriminates and the other colour does not. -/
theorem prop1_ii {K μc μo α lam : ℝ} (P : Params K μc μo α lam) (hα : α < 1)
    (hthr : 1 < K * μc * (1 - α) * lam) :
    ∃ x, 0 < x ∧ IsEq K μc μo α lam x 0 := by
  obtain ⟨a, ha⟩ : ∃ a, a = lam * (1 - α) := ⟨_, rfl⟩
  have hKμ : 0 ≤ K * μc := mul_nonneg P.hK P.hμc
  have hthr' : 1 < K * μc * a := by
    have : K * μc * (1 - α) * lam = K * μc * a := by rw [ha]; ring
    linarith
  have hapos : 0 < a := by
    rcases (mul_nonneg P.hlam (by linarith : (0:ℝ) ≤ 1 - α)).lt_or_eq with h | h
    · rw [ha]; exact h
    · exfalso
      rw [ha, ← h] at hthr'; simp at hthr'; linarith
  obtain ⟨A, hA⟩ : ∃ A, A = K * μc * a := ⟨_, rfl⟩
  have hA1 : 1 < A := by rw [hA]; exact hthr'
  have hKμpos : 0 < K * μc := by
    rcases hKμ.lt_or_eq with h | h
    · exact h
    · rw [hA, ← h] at hA1; simp at hA1; linarith
  let f : ℝ → ℝ := fun x => K * μc * (1 - Real.exp (-(a * x))) - x
  have hf : Continuous f := by fun_prop
  have hbt : ∀ x, bt K μc μo α lam x 0 - x = f x := by
    intro x; rw [bt_at_zero]; simp only [f, G, ha]; ring_nf
  -- a point where the curve is above the 45-degree line
  set x0 := (A - 1) / (2 * a) with hx0
  have hx0pos : 0 < x0 := div_pos (by linarith) (by linarith)
  have hax0 : a * x0 = (A - 1) / 2 := by rw [hx0]; field_simp
  have hf0 : 0 < f x0 := by
    have hlb := one_sub_exp_neg_ge (le_of_lt (mul_pos hapos hx0pos))
    have hden : 0 < 1 + a * x0 := by nlinarith
    have : K * μc * (a * x0 / (1 + a * x0)) - x0 = x0 * (A / (1 + a * x0) - 1) := by
      rw [hA]; field_simp
    have hgt : 1 < A / (1 + a * x0) := by
      rw [lt_div_iff₀ hden, hax0]; linarith
    have : 0 < K * μc * (a * x0 / (1 + a * x0)) - x0 := by
      rw [this]; exact mul_pos hx0pos (by linarith)
    have : K * μc * (a * x0 / (1 + a * x0)) ≤ K * μc * (1 - Real.exp (-(a * x0))) :=
      mul_le_mul_of_nonneg_left hlb hKμ
    simp only [f]; linarith
  -- a point where it is below
  set x1 := K * μc with hx1
  have hf1 : f x1 < 0 := by
    simp only [f]
    have : 0 < Real.exp (-(a * x1)) := Real.exp_pos _
    nlinarith
  have hx01 : x0 ≤ x1 := by
    have : f x0 ≤ x1 - x0 := by
      simp only [f]
      have : 0 < Real.exp (-(a * x0)) := Real.exp_pos _
      nlinarith
    linarith
  obtain ⟨x, hxI, hfx⟩ :=
    intermediate_value_Icc' hx01 hf.continuousOn ⟨le_of_lt hf1, le_of_lt hf0⟩
  have hxpos : 0 < x := lt_of_lt_of_le hx0pos hxI.1
  refine ⟨x, hxpos, le_of_lt hxpos, le_rfl, ?_, ?_⟩
  · have hnn : 0 ≤ bt K μc μo α lam x 0 := by
      rw [bt_at_zero]
      exact mul_nonneg P.hK (mul_nonneg P.hμc
        (G_nonneg P.hlam (mul_nonneg (by linarith) (le_of_lt hxpos))))
    have := hbt x
    unfold b; rw [max_eq_right hnn]; linarith
  · have P' : Params K μo μc α lam := ⟨P.hK, P.hμo, P.hμc, P.hα0, P.hα1, P.hlam⟩
    exact (lemma3_ii P' (le_of_lt hxpos)).symm

/-- Our reading of the bias equation below eq. (7), with the employer's branch
carried. If being an employer changes `V_{c,c} − V_{c,−c}` only through the
event, of probability `1 − γ`, that the social colour stays, then
`D = δ/(δ+r) [½ (M A + D) + ½ (1 − γ) D]` solves to `D = δ M A /(2r + δγ)`. -/
theorem bias_with_employer_branch {δ r M A D γ : ℝ} (hδ : 0 < δ) (hr : 0 < r)
    (hγ : 0 ≤ γ)
    (h : D = δ / (δ + r) * ((M * A + D) / 2 + (1 - γ) * D / 2)) :
    D = δ * M * A / (2 * r + δ * γ) := by
  have h1 : 0 < δ + r := by linarith
  have h2 : 0 < 2 * r + δ * γ := by nlinarith
  rw [eq_div_iff h2.ne']
  field_simp at h
  nlinarith

/-- The printed line. `D = δ/(δ+r) [½ M A + D]` solves to `D = M δ A / 2r`, the
expression the paper uses to define `K`. -/
theorem bias_as_printed {δ r M A D : ℝ} (hδ : 0 < δ) (hr : 0 < r)
    (h : D = δ / (δ + r) * (M * A / 2 + D)) :
    D = M * δ / (2 * r) * A := by
  have h1 : 0 < δ + r := by linarith
  field_simp at h ⊢
  nlinarith

/-- Control: without movement of social colour (`γ = 0`, so `K = 0`) no
colour discriminates in any profile. The paper's claim that the model with
physical colours but no social colours has only the colour-blind outcome. -/
theorem control_no_social_colour (M δ r μc μo α lam x y : ℝ) :
    b (K M δ 0 r) μc μo α lam x y = 0 := by
  unfold b bt K; simp

/-- Control: Proposition 1 (ii) needs the threshold. At or below it no
equilibrium has colour `c` discriminating against the other colour. -/
theorem control_below_threshold_no_discrimination {K μc μo α lam : ℝ}
    (P : Params K μc μo α lam)
    (hc : K * μc * (1 - α) * lam ≤ 1) (ho : K * μo * (1 - α) * lam ≤ 1) :
    ¬ ∃ x y, 0 < x ∧ IsEq K μc μo α lam x y := by
  rintro ⟨x, y, hx, he⟩
  have := (prop1_i P hc ho he).1
  linarith

end PeskiSzentes2012
