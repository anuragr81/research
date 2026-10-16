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

/-! ## Second pass: the remaining structure of the model

Everything below implements the rest of Sections 3 to 6 and the appendix in
the paper's notation. -/

open Filter Topology

/-! ### Lemma 1 in general and eq. (3) -/

/-- The employer's value from hiring, for any worker type. The employer's own
physical colour is fixed. `V s` is his value with social colour `s`, where
`true` means "equal to his own physical colour". `e` is his current social
colour and `(c1, c2)` the worker's type in the same coding. -/
def hireV (γ α s : ℝ) (V : Bool → ℝ) (e c1 c2 : Bool) : ℝ :=
  s + (1 - γ) * V e + γ * α * V c1 + γ * (1 - α) * V c2

/-- The employer's value from not hiring. -/
def noHireV (γ : ℝ) (V : Bool → ℝ) (e : Bool) : ℝ := (1 - γ) * V e + γ * V true

/-- The best-response cutoff against a worker of type `(c1, c2)`, before
truncation at zero. -/
def cut (γ α : ℝ) (V : Bool → ℝ) (c1 c2 : Bool) : ℝ :=
  γ * (V true - α * V c1 - (1 - α) * V c2)

/-- Lemma 1, the comparison. Hiring beats not hiring exactly when the shock is
at least `cut`, whatever the employer's own social colour `e`. -/
theorem lemma1_hire (γ α s : ℝ) (V : Bool → ℝ) (e c1 c2 : Bool) :
    noHireV γ V e ≤ hireV γ α s V e c1 c2 ↔ cut γ α V c1 c2 ≤ s := by
  have key : hireV γ α s V e c1 c2 - noHireV γ V e = s - cut γ α V c1 c2 := by
    unfold hireV noHireV cut; ring
  constructor <;> intro h <;> linarith

/-- Lemma 1, the four cutoffs. With `true` for the employer's colour `c` and
`false` for `−c`, they are `γ (V_{c,c} − V_{c,−c})` times `1`, `1 − α`, `α`
and `0`. -/
theorem lemma1_cutoffs (γ α : ℝ) (V : Bool → ℝ) :
    cut γ α V false false = γ * (V true - V false) ∧
    cut γ α V true false = γ * (1 - α) * (V true - V false) ∧
    cut γ α V false true = γ * α * (V true - V false) ∧
    cut γ α V true true = 0 := by
  unfold cut; refine ⟨by ring, by ring, by ring, by ring⟩

/-- Eq. (3). After truncation at zero, the cutoffs against `(c, −c)` and
`(−c, c)` workers are `(1 − α) x^c` and `α x^c`, with `x^c` the truncated
cutoff against `(−c, −c)`. -/
theorem eq3 {γ α : ℝ} (V : Bool → ℝ) (hα0 : 0 ≤ α) (hα1 : α ≤ 1) :
    max 0 (cut γ α V true false) = (1 - α) * max 0 (cut γ α V false false) ∧
    max 0 (cut γ α V false true) = α * max 0 (cut γ α V false false) ∧
    max 0 (cut γ α V true true) = 0 := by
  obtain ⟨h1, h2, h3, h4⟩ := lemma1_cutoffs γ α V
  rw [h1, h2, h3, h4]
  have e1 : γ * (1 - α) * (V true - V false) = (1 - α) * (γ * (V true - V false)) := by ring
  have e2 : γ * α * (V true - V false) = α * (γ * (V true - V false)) := by ring
  rw [e1, e2, mul_max_of_nonneg _ _ (by linarith), mul_max_of_nonneg _ _ hα0]
  simp

/-! ### Lemma 2 -/

/-- Eq. (6). The value of a worker of type `(c, c)`. `A3 = E G(X^{−c})`. -/
def PiL_same (M μc μo A3 Vs : ℝ) : ℝ := M * (μc + μo * (1 - A3)) + Vs

/-- Eq. (7). The value of a worker of type `(c, −c)`. `A1 = E G((1 − α) X^c)`,
`A2 = E G(α X^{−c})`. -/
def PiL_other (M μc μo A1 A2 Vo : ℝ) : ℝ := M * (μc * (1 - A1) + μo * (1 - A2)) + Vo

theorem worker_gap (M μc μo A1 A2 A3 Vs Vo : ℝ) :
    PiL_same M μc μo A3 Vs - PiL_other M μc μo A1 A2 Vo
      = M * (μc * A1 + μo * (A2 - A3)) + (Vs - Vo) := by
  unfold PiL_same PiL_other; ring

/-- Lemma 2 and eq. (8), as printed. If the bias satisfies the printed line
below eq. (7), with the employer half equal to the bias itself, the cutoff
`γ (V_{c,c} − V_{c,−c})` is `K [μ_c A1 + μ_{−c}(A2 − A3)]` with
`K = Mδγ/2r`. -/
theorem lemma2 {δ r M γ μc μo A1 A2 A3 Vs Vo : ℝ} (hδ : 0 < δ) (hr : 0 < r)
    (h : Vs - Vo = δ / (δ + r) *
      ((PiL_same M μc μo A3 Vs - PiL_other M μc μo A1 A2 Vo) / 2 + (Vs - Vo) / 2)) :
    γ * (Vs - Vo) = K M δ γ r * (μc * A1 + μo * (A2 - A3)) := by
  have h' : Vs - Vo = δ / (δ + r) * (M * (μc * A1 + μo * (A2 - A3)) / 2 + (Vs - Vo)) := by
    calc Vs - Vo = _ := h
      _ = _ := by rw [worker_gap]; ring
  rw [bias_as_printed hδ hr h']
  unfold K; ring

/-- Lemma 2 on our reading (PS-D3). With the employer half carrying
`(1 − γ)(V_{c,c} − V_{c,−c})`, the cutoff has the same form with
`K = Mδγ/(2r + δγ)`. -/
theorem lemma2_our_reading {δ r M γ μc μo A1 A2 A3 Vs Vo : ℝ} (hδ : 0 < δ) (hr : 0 < r)
    (hγ : 0 ≤ γ)
    (h : Vs - Vo = δ / (δ + r) *
      ((PiL_same M μc μo A3 Vs - PiL_other M μc μo A1 A2 Vo) / 2 + (1 - γ) * (Vs - Vo) / 2)) :
    γ * (Vs - Vo) = M * δ * γ / (2 * r + δ * γ) * (μc * A1 + μo * (A2 - A3)) := by
  have h' : Vs - Vo = δ / (δ + r) *
      ((M * (μc * A1 + μo * (A2 - A3)) + (Vs - Vo)) / 2 + (1 - γ) * (Vs - Vo) / 2) := by
    calc Vs - Vo = _ := h
      _ = _ := by rw [worker_gap]
  rw [bias_with_employer_branch hδ hr hγ h']
  ring

/-! ### Lemma 3 (i) and the limit in Lemma 3 (iii) -/

/-- Lemma 3 (i), monotonicity. `b̃^c` is strictly increasing in the own
colour's cutoff. -/
theorem lemma3_i_strictMono {K μc μo α lam y : ℝ} (hK : 0 < K * μc) (hl : 0 < lam)
    (hα : α < 1) : StrictMono (fun x => bt K μc μo α lam x y) := by
  intro u v huv
  have hk : 0 < lam * (1 - α) := mul_pos hl (by linarith)
  have h1 : Real.exp (-(lam * ((1 - α) * v))) < Real.exp (-(lam * ((1 - α) * u))) :=
    Real.exp_lt_exp.2 (by nlinarith)
  have h2 := mul_lt_mul_of_pos_left h1 hK
  show bt K μc μo α lam u y < bt K μc μo α lam v y
  unfold bt G
  set C := μo * (1 - Real.exp (-(lam * (α * y))) - (1 - Real.exp (-(lam * y))))
  nlinarith

/-- Lemma 3 (i), concavity. `b̃^c` is concave in the own colour's cutoff, on
the whole line. -/
theorem lemma3_i_concave {K μc μo α lam y : ℝ} (hK : 0 ≤ K * μc) :
    ConcaveOn ℝ Set.univ (fun x => bt K μc μo α lam x y) := by
  refine ⟨convex_univ, ?_⟩
  intro x _ z _ a c ha hc hac
  have hcv := convexOn_exp.2 (Set.mem_univ (-(lam * ((1 - α) * x))))
    (Set.mem_univ (-(lam * ((1 - α) * z)))) ha hc hac
  simp only [smul_eq_mul] at hcv ⊢
  have e : a * -(lam * ((1 - α) * x)) + c * -(lam * ((1 - α) * z))
      = -(lam * ((1 - α) * (a * x + c * z))) := by ring
  rw [e] at hcv
  have h2 := mul_le_mul_of_nonneg_left hcv hK
  have hc' : c = 1 - a := by linarith
  subst hc'
  unfold bt G
  nlinarith

/-- The other colour's term tends to zero as its cutoff grows, when
`α > 0` and `λ > 0`. Property (7) of the proof of Lemma 5, at infinity. -/
theorem G_shift_tendsto {lam α : ℝ} (hl : 0 < lam) (hα : 0 < α) :
    Tendsto (fun y => G lam (α * y) - G lam y) atTop (𝓝 0) := by
  have hz1 : Tendsto (fun y : ℝ => Real.exp (-(lam * (α * y)))) atTop (𝓝 0) :=
    Real.tendsto_exp_neg_atTop_nhds_zero.comp
      ((tendsto_id.const_mul_atTop hα).const_mul_atTop hl)
  have hz2 : Tendsto (fun y : ℝ => Real.exp (-(lam * y))) atTop (𝓝 0) :=
    Real.tendsto_exp_neg_atTop_nhds_zero.comp (tendsto_id.const_mul_atTop hl)
  have := ((tendsto_const_nhds (x := (1 : ℝ))).sub hz1).sub
    ((tendsto_const_nhds (x := (1 : ℝ))).sub hz2)
  simpa [G] using this

/-- Lemma 3 (iii), the equality. `b^c(x, 0) = lim_{x^{−c} → ∞} b^c(x, x^{−c})`,
for `α > 0` and `λ > 0`. -/
theorem lemma3_iii_limit {K μc μo α lam x : ℝ} (hl : 0 < lam) (hα : 0 < α) :
    Tendsto (fun y => b K μc μo α lam x y) atTop (𝓝 (b K μc μo α lam x 0)) := by
  have h := ((G_shift_tendsto hl hα).const_mul μo |>.const_add (μc * G lam ((1 - α) * x))).const_mul K
  have h' := (tendsto_const_nhds (x := (0 : ℝ))).max h
  have e : b K μc μo α lam x 0 = max 0 (K * (μc * G lam ((1 - α) * x) + μo * 0)) := by
    unfold b; rw [bt_at_zero]; simp
  rw [e]
  exact h'

/-- Control: the equality in Lemma 3 (iii) needs `α > 0`. With `α = 0` the
other colour's term tends to `−K μ_{−c}`, not to zero, so `b̃^c(x, y)` does not
tend to `b̃^c(x, 0)` when `K μ_{−c} ≠ 0`. -/
theorem control_lemma3_iii_needs_alpha {K μc μo lam x : ℝ} (hl : 0 < lam)
    (hKμ : K * μo ≠ 0) :
    ¬ Tendsto (fun y => bt K μc μo 0 lam x y) atTop (𝓝 (bt K μc μo 0 lam x 0)) := by
  intro h
  have hz2 : Tendsto (fun y : ℝ => Real.exp (-(lam * y))) atTop (𝓝 0) :=
    Real.tendsto_exp_neg_atTop_nhds_zero.comp (tendsto_id.const_mul_atTop hl)
  have h2 := (((tendsto_const_nhds (x := (1 : ℝ))).sub hz2).const_mul (-(K * μo))).const_add
    (bt K μc μo 0 lam x 0)
  have h3 : Tendsto (fun y => bt K μc μo 0 lam x y) atTop
      (𝓝 (bt K μc μo 0 lam x 0 + -(K * μo) * (1 - 0))) := by
    refine h2.congr (fun y => ?_)
    rw [bt_at_zero]; unfold bt G
    simp only [zero_mul, mul_zero, neg_zero, Real.exp_zero, sub_self]; ring
  have := tendsto_nhds_unique h h3
  apply hKμ; linarith

/-! ### Lemma 4 (i), Remark 1 and comparative statics -/

/-- The function whose positive roots are the positive fixed points of
`b^c(·, 0)`, with `A = K μ_c` and `a = λ(1 − α)`. -/
noncomputable def fp (A a x : ℝ) : ℝ := A * (1 - Real.exp (-(a * x))) - x

theorem bt_zero_sub (K μc μo α lam x : ℝ) :
    bt K μc μo α lam x 0 - x = fp (K * μc) (lam * (1 - α)) x := by
  rw [bt_at_zero]; unfold fp G
  rw [show lam * ((1 - α) * x) = lam * (1 - α) * x by ring]; ring

/-- Strict concavity of `fp` along a chord from the origin, where `fp = 0`. -/
theorem fp_chord {A a x y : ℝ} (hA : 0 < A) (ha : 0 < a) (hx : 0 < x) (hxy : x < y) :
    x / y * fp A a y < fp A a x := by
  have hy : 0 < y := by linarith
  have ht0 : 0 < x / y := div_pos hx hy
  have ht1 : x / y < 1 := (div_lt_one hy).2 hxy
  have hxt : x = x / y * y := by field_simp
  have hne : (0 : ℝ) ≠ -(a * y) := by nlinarith
  have hc := strictConvexOn_exp.2 (Set.mem_univ (0 : ℝ)) (Set.mem_univ (-(a * y))) hne
    (by linarith : 0 < 1 - x / y) ht0 (by ring)
  simp only [smul_eq_mul, mul_zero, zero_add, Real.exp_zero, mul_one] at hc
  have e : x / y * -(a * y) = -(a * x) := by rw [hxt]; field_simp
  rw [e] at hc
  have key : fp A a x - x / y * fp A a y
      = A * ((1 - x / y + x / y * Real.exp (-(a * y))) - Real.exp (-(a * x))) := by
    unfold fp; linear_combination (-1 : ℝ) * hxt
  have : 0 < A * ((1 - x / y + x / y * Real.exp (-(a * y))) - Real.exp (-(a * x))) :=
    mul_pos hA (by linarith)
  linarith

/-- `fp` has at most one positive root. -/
theorem fp_root_unique {A a x y : ℝ} (hA : 0 < A) (ha : 0 < a) (hx : 0 < x) (hy : 0 < y)
    (h1 : fp A a x = 0) (h2 : fp A a y = 0) : x = y := by
  rcases lt_trichotomy x y with hxy | hxy | hxy
  · have := fp_chord hA ha hx hxy; rw [h1, h2] at this; simp at this
  · exact hxy
  · have := fp_chord hA ha hy hxy; rw [h1, h2] at this; simp at this

/-- Above the threshold, `K μ_c > 0` and `λ(1 − α) > 0`. -/
theorem pos_of_thr {K μc μo α lam : ℝ} (P : Params K μc μo α lam)
    (hthr : 1 < K * μc * (1 - α) * lam) : 0 < K * μc ∧ 0 < lam * (1 - α) := by
  have hKμ : 0 ≤ K * μc := mul_nonneg P.hK P.hμc
  have hk : 0 ≤ lam * (1 - α) := by
    by_contra h; push_neg at h; nlinarith
  have e : K * μc * (1 - α) * lam = K * μc * (lam * (1 - α)) := by ring
  rw [e] at hthr
  constructor
  · rcases hKμ.lt_or_eq with h | h
    · exact h
    · rw [← h] at hthr; simp at hthr; linarith
  · rcases hk.lt_or_eq with h | h
    · exact h
    · rw [← h] at hthr; simp at hthr; linarith

/-- A positive fixed point of `b^c(·, 0)` is a root of `fp`. -/
theorem fixed_point_fp {K μc μo α lam x : ℝ} (P : Params K μc μo α lam) (hα : α < 1)
    (hx : 0 < x) (h : b K μc μo α lam x 0 = x) : fp (K * μc) (lam * (1 - α)) x = 0 := by
  have hnn : 0 ≤ bt K μc μo α lam x 0 := by
    rw [bt_at_zero]
    exact mul_nonneg P.hK (mul_nonneg P.hμc
      (G_nonneg P.hlam (mul_nonneg (by linarith) hx.le)))
  unfold b at h; rw [max_eq_right hnn] at h
  rw [← bt_zero_sub]; linarith

/-- Lemma 4 (i). Above the threshold `b^c(·, 0)` has exactly one positive
fixed point. -/
theorem lemma4_i {K μc μo α lam : ℝ} (P : Params K μc μo α lam) (hα : α < 1)
    (hthr : 1 < K * μc * (1 - α) * lam) :
    ∃ x, 0 < x ∧ b K μc μo α lam x 0 = x ∧
      ∀ y, 0 < y → b K μc μo α lam y 0 = y → y = x := by
  obtain ⟨x, hx, -, -, hxe, -⟩ := prop1_ii P hα hthr
  obtain ⟨hA, ha⟩ := pos_of_thr P hthr
  refine ⟨x, hx, hxe.symm, fun y hy hye => ?_⟩
  exact fp_root_unique hA ha hy hx (fixed_point_fp P hα hy hye)
    (fixed_point_fp P hα hx hxe.symm)

/-- Proposition 1, first claim. The colour-blind profile `(0, 0)` is an
equilibrium. -/
theorem prop1_zero {K μc μo α lam : ℝ} (P : Params K μc μo α lam) :
    IsEq K μc μo α lam 0 0 := by
  have P' : Params K μo μc α lam := ⟨P.hK, P.hμo, P.hμc, P.hα0, P.hα1, P.hlam⟩
  exact ⟨le_rfl, le_rfl, (lemma3_ii P le_rfl).symm, (lemma3_ii P' le_rfl).symm⟩

/-- Remark 1. Above the threshold there is exactly one `x > 0` with `(x, 0)`
an equilibrium. -/
theorem remark1 {K μc μo α lam : ℝ} (P : Params K μc μo α lam) (hα : α < 1)
    (hthr : 1 < K * μc * (1 - α) * lam) :
    ∃ x, 0 < x ∧ IsEq K μc μo α lam x 0 ∧
      ∀ y, 0 < y → IsEq K μc μo α lam y 0 → y = x := by
  obtain ⟨x, hx, he⟩ := prop1_ii P hα hthr
  obtain ⟨hA, ha⟩ := pos_of_thr P hthr
  refine ⟨x, hx, he, fun y hy hye => ?_⟩
  exact fp_root_unique hA ha hy hx (fixed_point_fp P hα hy hye.2.2.1.symm)
    (fixed_point_fp P hα hx he.2.2.1.symm)

/-- Section 6.1, for the one-sided equilibria. If `K μ_c` or `λ(1 − α)`
rises, the positive cutoff of the discriminating colour rises. Since
`K = Mδγ/2r`, this gives the table's signs for `r`, `M`, `δ`, `γ` and `λ`,
and for `μ_w` the sign of `x^w` in `(x^w, 0)`. -/
theorem compstat_one_sided {A A' a a' x x' : ℝ} (hA : 0 < A) (ha : 0 < a)
    (hAA : A ≤ A') (haa : a ≤ a') (hs : A < A' ∨ a < a') (hx : 0 < x) (hx' : 0 < x')
    (h : fp A a x = 0) (h' : fp A' a' x' = 0) : x < x' := by
  have hA' : 0 < A' := by linarith
  have ha' : 0 < a' := by linarith
  have hu : 0 < 1 - Real.exp (-(a * x)) := by
    have : Real.exp (-(a * x)) < 1 := by
      have := Real.exp_lt_exp.2 (show -(a * x) < 0 by nlinarith); simpa using this
    linarith
  have hmono : Real.exp (-(a' * x)) ≤ Real.exp (-(a * x)) :=
    Real.exp_le_exp.2 (by nlinarith)
  have hgt : 0 < fp A' a' x := by
    unfold fp at h ⊢
    rcases hs with hs | hs
    · nlinarith
    · have : Real.exp (-(a' * x)) < Real.exp (-(a * x)) := Real.exp_lt_exp.2 (by nlinarith)
      nlinarith
  rcases lt_trichotomy x x' with hlt | heq | hgt'
  · exact hlt
  · rw [heq] at hgt; rw [h'] at hgt; exact absurd hgt (lt_irrefl 0)
  · have := fp_chord hA' ha' hx' hgt'
    rw [h'] at this
    have : 0 < x' / x * fp A' a' x := mul_pos (div_pos hx' hx) hgt
    linarith

/-! ### Lemma 5, the vertical shift -/

/-- `H(x^{−c}) = K μ_{−c} (G(α x^{−c}) − G(x^{−c}))`, the shift in the proof of
Lemma 5. -/
noncomputable def H (K μo α lam y : ℝ) : ℝ := K * (μo * (G lam (α * y) - G lam y))

/-- Proof of Lemma 5. `B^{x^{−c}}(x^c) = B^0(x^c) + H(x^{−c})`. -/
theorem lemma5_shift (K μc μo α lam x y : ℝ) :
    bt K μc μo α lam x y - x = (bt K μc μo α lam x 0 - x) + H K μo α lam y := by
  rw [bt_at_zero]; unfold bt H; ring

/-- Proof of Lemma 5, property (6). `H(x^{−c}) < 0` for `x^{−c} > 0`, when
`0 < α < 1`. -/
theorem lemma5_H_neg {K μo α lam y : ℝ} (hK : 0 < K * μo) (hl : 0 < lam) (hα1 : α < 1)
    (hy : 0 < y) : H K μo α lam y < 0 := by
  unfold H G
  have : Real.exp (-(lam * y)) < Real.exp (-(lam * (α * y))) :=
    Real.exp_lt_exp.2 (by nlinarith [mul_pos (mul_pos hl hy) (sub_pos.2 hα1)])
  nlinarith

/-- Proof of Lemma 5, property (7). `H(x^{−c}) → 0` as `x^{−c} → ∞`, and
`H(0) = 0`. -/
theorem lemma5_H_limits {K μo α lam : ℝ} (hl : 0 < lam) (hα : 0 < α) :
    Tendsto (fun y => H K μo α lam y) atTop (𝓝 0) ∧ H K μo α lam 0 = 0 := by
  refine ⟨?_, by simp [H, G_zero]⟩
  have := ((G_shift_tendsto hl hα).const_mul μo).const_mul K
  simpa [H] using this

/-- Proof of Lemma 5, property (4). At `x^c = 0`, `B^{x^{−c}}(0) = H(x^{−c})`,
negative for `x^{−c} > 0` by `lemma5_H_neg`. -/
theorem lemma5_B_zero (K μc μo α lam y : ℝ) : bt K μc μo α lam 0 y = H K μo α lam y := by
  unfold bt H; rw [mul_zero, G_zero, mul_zero, zero_add]

/-! ### Appendix: Lemma 6, eq. (15) and Lemma 7 -/

/-- Lemma 6, on the domain where it holds, `0 ≤ ξ ≤ log 2`:
`1 − e^{−ξ} ≥ ξ/2`. The printed statement omits `0 ≤ ξ` (PS-D7). -/
theorem lemma6 {ξ : ℝ} (h0 : 0 ≤ ξ) (h1 : ξ ≤ Real.log 2) : ξ / 2 ≤ 1 - Real.exp (-ξ) := by
  have hl : 0 < Real.log 2 := Real.log_pos (by norm_num)
  have hl1 : Real.log 2 < 1 := by have := Real.log_two_lt_d9; linarith
  set θ := ξ / Real.log 2 with hθ
  have hθ0 : 0 ≤ θ := div_nonneg h0 hl.le
  have hθ1 : θ ≤ 1 := (div_le_one hl).2 h1
  have hc := convexOn_exp.2 (Set.mem_univ (0 : ℝ)) (Set.mem_univ (-Real.log 2))
    (by linarith : 0 ≤ 1 - θ) hθ0 (by ring)
  simp only [smul_eq_mul, mul_zero, zero_add, Real.exp_zero, mul_one] at hc
  have e1 : θ * -Real.log 2 = -ξ := by rw [hθ]; field_simp
  have e2 : Real.exp (-Real.log 2) = 1 / 2 := by
    rw [Real.exp_neg, Real.exp_log (by norm_num)]; norm_num
  rw [e1, e2] at hc
  have : ξ / 2 ≤ θ / 2 := by
    rw [hθ]; apply div_le_div_of_nonneg_right _ (by norm_num)
    rw [le_div_iff₀ hl]; nlinarith
  linarith

/-- Control: Lemma 6 fails for negative `ξ`, as printed. At `ξ = −1`,
`1 − e^{1} < −1/2`. -/
theorem control_lemma6_needs_nonneg : ¬ ((-1 : ℝ) / 2 ≤ 1 - Real.exp (-(-1))) := by
  have := Real.add_one_le_exp (1 : ℝ)
  simp only [neg_neg]
  linarith

/-- The factorisation used in eq. (15),
`G((1 − α)x) + G(αx) − G(x) = (1 − e^{−αλx})(1 − e^{−(1−α)λx})`. -/
theorem eq15_term (lam α x : ℝ) :
    G lam ((1 - α) * x) + G lam (α * x) - G lam x
      = (1 - Real.exp (-(α * lam * x))) * (1 - Real.exp (-((1 - α) * lam * x))) := by
  unfold G
  have h : Real.exp (-(lam * x)) = Real.exp (-(α * lam * x)) * Real.exp (-((1 - α) * lam * x)) := by
    rw [← Real.exp_add]; congr 1; ring
  rw [show lam * ((1 - α) * x) = (1 - α) * lam * x by ring,
    show lam * (α * x) = α * lam * x by ring, h]
  ring

/-- Eq. (15). With both cutoffs positive, the sum of the two best responses
regroups by colour. -/
theorem eq15 (K μb μw α lam xb xw : ℝ) :
    bt K μb μw α lam xb xw + bt K μw μb α lam xw xb
      = K * μb * ((1 - Real.exp (-(α * lam * xb))) * (1 - Real.exp (-((1 - α) * lam * xb))))
        + K * μw * ((1 - Real.exp (-(α * lam * xw))) * (1 - Real.exp (-((1 - α) * lam * xw)))) := by
  have hb := eq15_term lam α xb
  have hw := eq15_term lam α xw
  unfold bt
  linear_combination K * μb * hb + K * μw * hw

/-- `P(x) = (1 − e^{−αλx})(1 − e^{−(1−α)λx})`, the summand of eq. (15). -/
noncomputable def Pf (α lam x : ℝ) : ℝ :=
  (1 - Real.exp (-(α * lam * x))) * (1 - Real.exp (-((1 - α) * lam * x)))

theorem one_sub_exp_nonneg {t : ℝ} (ht : 0 ≤ t) : 0 ≤ 1 - Real.exp (-t) := by
  have : Real.exp (-t) ≤ 1 := Real.exp_le_one_iff.2 (by linarith)
  linarith

theorem Pf_nonneg {α lam x : ℝ} (hα0 : 0 ≤ α) (hα1 : α ≤ 1) (hl : 0 ≤ lam) (hx : 0 ≤ x) :
    0 ≤ Pf α lam x := by
  unfold Pf
  exact mul_nonneg (one_sub_exp_nonneg (mul_nonneg (mul_nonneg hα0 hl) hx))
    (one_sub_exp_nonneg (mul_nonneg (mul_nonneg (by linarith) hl) hx))

theorem Pf_mono {α lam x y : ℝ} (hα0 : 0 ≤ α) (hα1 : α ≤ 1) (hl : 0 ≤ lam) (hx : 0 ≤ x)
    (hxy : x ≤ y) : Pf α lam x ≤ Pf α lam y := by
  unfold Pf
  have h1 : Real.exp (-(α * lam * y)) ≤ Real.exp (-(α * lam * x)) :=
    Real.exp_le_exp.2 (by have := mul_nonneg hα0 hl; nlinarith)
  have h2 : Real.exp (-((1 - α) * lam * y)) ≤ Real.exp (-((1 - α) * lam * x)) :=
    Real.exp_le_exp.2 (by have := mul_nonneg (by linarith : (0:ℝ) ≤ 1 - α) hl; nlinarith)
  apply mul_le_mul (by linarith) (by linarith)
    (one_sub_exp_nonneg (mul_nonneg (mul_nonneg (by linarith) hl) hx))
  exact one_sub_exp_nonneg (mul_nonneg (mul_nonneg hα0 hl) (le_trans hx hxy))

/-- Lemma 6 applied to both factors, inequality (16). -/
theorem Pf_ge_sq {α lam x : ℝ} (hα0 : 0 ≤ α) (hα1 : α ≤ 1) (hl : 0 ≤ lam) (hx : 0 ≤ x)
    (hsmall : lam * x ≤ Real.log 2) :
    α * (1 - α) * lam ^ 2 * x ^ 2 / 4 ≤ Pf α lam x := by
  unfold Pf
  have hlx : 0 ≤ lam * x := mul_nonneg hl hx
  have h1 := lemma6 (ξ := α * lam * x) (mul_nonneg (mul_nonneg hα0 hl) hx) (by nlinarith)
  have h2 := lemma6 (ξ := (1 - α) * lam * x)
    (mul_nonneg (mul_nonneg (by linarith) hl) hx) (by nlinarith)
  have e : α * (1 - α) * lam ^ 2 * x ^ 2 / 4 = (α * lam * x / 2) * ((1 - α) * lam * x / 2) := by
    ring
  rw [e]
  exact mul_le_mul h1 h2 (by have := mul_nonneg (by linarith : (0:ℝ) ≤ 1 - α) hlx; nlinarith)
    (one_sub_exp_nonneg (mul_nonneg (mul_nonneg hα0 hl) hx))

/-- `ψ₁` of eq. (13), with `2^{−α}` written `e^{−α log 2}`. -/
noncomputable def ψ1 (K μmin α : ℝ) : ℝ :=
  K * μmin * (1 / 2) * ((1 - Real.exp (-(α * Real.log 2))) *
    (1 - Real.exp (-((1 - α) * Real.log 2))))

theorem Pf_at_log2 {α lam : ℝ} (hl : 0 < lam) :
    Pf α lam (Real.log 2 / lam) = (1 - Real.exp (-(α * Real.log 2))) *
      (1 - Real.exp (-((1 - α) * Real.log 2))) := by
  unfold Pf
  rw [show α * lam * (Real.log 2 / lam) = α * Real.log 2 by field_simp,
    show (1 - α) * lam * (Real.log 2 / lam) = (1 - α) * Real.log 2 by field_simp]

/-- Lemma 7, the case of two positive cutoffs, for every `λ > 0`. Either
`(1/4) K α (1 − α) μ_min λ² max{x^b, x^w} ≤ 2`, that is
`max{x^b, x^w} ≤ 2 ψ₀ λ^{−2}`, or `max{x^b, x^w} ≥ ψ₁`. The printed proof
concludes `≤ ψ₀ λ^{−2}`, dropping the factor 2 from
`max{x^b, x^w} ≥ (x^b + x^w)/2` (PS-D8). -/
theorem lemma7_both_pos {K μb μw μmin α lam xb xw : ℝ} (hK : 0 < K) (hl : 0 < lam)
    (hα0 : 0 ≤ α) (hα1 : α ≤ 1) (hm0 : 0 ≤ μmin) (hmb : μmin ≤ μb) (hmw : μmin ≤ μw)
    (hb : 0 < xb) (hw : 0 < xw)
    (eb : xb = bt K μb μw α lam xb xw) (ew : xw = bt K μw μb α lam xw xb) :
    (1 / 4) * K * α * (1 - α) * μmin * lam ^ 2 * max xb xw ≤ 2 ∨
      ψ1 K μmin α ≤ max xb xw := by
  have hsum : xb + xw = K * μb * Pf α lam xb + K * μw * Pf α lam xw := by
    have := eq15 K μb μw α lam xb xw
    unfold Pf; linarith
  have hPb := Pf_nonneg hα0 hα1 hl.le hb.le
  have hPw := Pf_nonneg hα0 hα1 hl.le hw.le
  have hKb : 0 ≤ K * μb := mul_nonneg hK.le (by linarith)
  have hKw : 0 ≤ K * μw := mul_nonneg hK.le (by linarith)
  set m := max xb xw with hm
  have hbm : xb ≤ m := le_max_left _ _
  have hwm : xw ≤ m := le_max_right _ _
  have hmpos : 0 < m := lt_of_lt_of_le hb hbm
  by_cases hbig : Real.log 2 / lam ≤ m
  · right
    have hq := Pf_at_log2 (α := α) hl
    have hlog : 0 ≤ Real.log 2 / lam := div_nonneg (Real.log_pos (by norm_num)).le hl.le
    have hQ : 0 ≤ (1 - Real.exp (-(α * Real.log 2))) *
        (1 - Real.exp (-((1 - α) * Real.log 2))) := by
      rw [← hq]; exact Pf_nonneg hα0 hα1 hl.le hlog
    unfold ψ1
    rcases le_total xw xb with hwb | hbw
    · have hmb' : m = xb := max_eq_left hwb
      have := Pf_mono hα0 hα1 hl.le hlog (hmb' ▸ hbig)
      rw [hq] at this
      have h1 : K * μmin * ((1 - Real.exp (-(α * Real.log 2))) *
          (1 - Real.exp (-((1 - α) * Real.log 2)))) ≤ K * μb * Pf α lam xb := by
        apply mul_le_mul (mul_le_mul_of_nonneg_left hmb hK.le) this hQ hKb
      have := mul_nonneg hKw hPw
      linarith
    · have hmw' : m = xw := max_eq_right hbw
      have := Pf_mono hα0 hα1 hl.le hlog (hmw' ▸ hbig)
      rw [hq] at this
      have h1 : K * μmin * ((1 - Real.exp (-(α * Real.log 2))) *
          (1 - Real.exp (-((1 - α) * Real.log 2)))) ≤ K * μw * Pf α lam xw := by
        apply mul_le_mul (mul_le_mul_of_nonneg_left hmw hK.le) this hQ hKw
      have := mul_nonneg hKb hPb
      linarith
  · left
    push_neg at hbig
    have hsb : lam * xb ≤ Real.log 2 := by
      have := (lt_div_iff₀ hl).1 (lt_of_le_of_lt hbm hbig); linarith
    have hsw : lam * xw ≤ Real.log 2 := by
      have := (lt_div_iff₀ hl).1 (lt_of_le_of_lt hwm hbig); linarith
    have q1 := Pf_ge_sq hα0 hα1 hl.le hb.le hsb
    have q2 := Pf_ge_sq hα0 hα1 hl.le hw.le hsw
    have hα' : 0 ≤ α * (1 - α) := mul_nonneg hα0 (by linarith)
    have hc : 0 ≤ α * (1 - α) * lam ^ 2 / 4 :=
      div_nonneg (mul_nonneg hα' (sq_nonneg _)) (by norm_num)
    have r1 : K * μmin * (α * (1 - α) * lam ^ 2 * xb ^ 2 / 4) ≤ K * μb * Pf α lam xb :=
      mul_le_mul (mul_le_mul_of_nonneg_left hmb hK.le) q1
        (div_nonneg (mul_nonneg (mul_nonneg hα' (sq_nonneg _)) (sq_nonneg _)) (by norm_num)) hKb
    have r2 : K * μmin * (α * (1 - α) * lam ^ 2 * xw ^ 2 / 4) ≤ K * μw * Pf α lam xw :=
      mul_le_mul (mul_le_mul_of_nonneg_left hmw hK.le) q2
        (div_nonneg (mul_nonneg (mul_nonneg hα' (sq_nonneg _)) (sq_nonneg _)) (by norm_num)) hKw
    have hsq : m ^ 2 ≤ xb ^ 2 + xw ^ 2 := by
      rcases le_total xw xb with h | h
      · rw [hm, max_eq_left h]; have := sq_nonneg xw; linarith
      · rw [hm, max_eq_right h]; have := sq_nonneg xb; linarith
    have hKc : 0 ≤ K * μmin * (α * (1 - α) * lam ^ 2 / 4) :=
      mul_nonneg (mul_nonneg hK.le hm0) hc
    have h3 : K * μmin * (α * (1 - α) * lam ^ 2 / 4) * m ^ 2 ≤ 2 * m := by
      have := mul_le_mul_of_nonneg_left hsq hKc
      have e : K * μmin * (α * (1 - α) * lam ^ 2 / 4) * (xb ^ 2 + xw ^ 2)
          = K * μmin * (α * (1 - α) * lam ^ 2 * xb ^ 2 / 4)
            + K * μmin * (α * (1 - α) * lam ^ 2 * xw ^ 2 / 4) := by ring
      linarith
    have h3' : (K * μmin * (α * (1 - α) * lam ^ 2 / 4) * m) * m ≤ 2 * m := by
      have e : (K * μmin * (α * (1 - α) * lam ^ 2 / 4) * m) * m
          = K * μmin * (α * (1 - α) * lam ^ 2 / 4) * m ^ 2 := by ring
      linarith
    have h4 := le_of_mul_le_mul_right h3' hmpos
    have e : (1 / 4) * K * α * (1 - α) * μmin * lam ^ 2 * m
        = K * μmin * (α * (1 - α) * lam ^ 2 / 4) * m := by ring
    linarith

/-- Lemma 7, the case of one zero cutoff. With `x^b = 0`, `x^w > 0` and
`λ > 2/(K μ_w (1 − α))`, the cutoff satisfies `x^w ≥ ψ₁`. The printed proof
uses `λ > 1/(2 K μ_w (1 − α))`, from `x^w ≥ 2 K μ_w (1 − α) λ x^w`, where
Lemma 6 gives the factor `1/2` rather than `2` (PS-D8). -/
theorem lemma7_one_zero {K μb μw μmin α lam xw : ℝ} (hK : 0 < K) (hl : 0 < lam)
    (hα0 : 0 ≤ α) (hα1 : α < 1) (hm0 : 0 ≤ μmin) (hmw : μmin ≤ μw)
    (hlarge : 2 / (K * μw * (1 - α)) < lam) (hw : 0 < xw)
    (ew : xw = bt K μw μb α lam xw 0) : ψ1 K μmin α ≤ xw := by
  have hμw0 : 0 ≤ μw := le_trans hm0 hmw
  have h1α : 0 < 1 - α := by linarith
  have ew' : xw = K * μw * (1 - Real.exp (-(lam * ((1 - α) * xw)))) := by
    calc xw = _ := ew
      _ = _ := by rw [bt_at_zero]; unfold G; ring
  have hμw : 0 < μw := by
    rcases hμw0.lt_or_eq with h | h
    · exact h
    · rw [← h] at ew'; simp at ew'; linarith
  have hk : 0 < K * μw * (1 - α) := mul_pos (mul_pos hK hμw) h1α
  have hlog : 0 < Real.log 2 := Real.log_pos (by norm_num)
  have e1pos : 0 < Real.exp (-(α * Real.log 2)) := Real.exp_pos _
  have e2le : Real.exp (-((1 - α) * Real.log 2)) ≤ 1 :=
    Real.exp_le_one_iff.2 (by nlinarith)
  by_cases hbig : Real.log 2 / lam ≤ xw
  · have hlx : Real.log 2 ≤ lam * xw := by rw [div_le_iff₀ hl] at hbig; linarith
    have hmono : Real.exp (-(lam * ((1 - α) * xw))) ≤ Real.exp (-((1 - α) * Real.log 2)) :=
      Real.exp_le_exp.2 (by nlinarith)
    have hxw : K * μw * (1 - Real.exp (-((1 - α) * Real.log 2))) ≤ xw := by
      rw [ew']; nlinarith [mul_pos hK hμw]
    have hψ : ψ1 K μmin α ≤ K * μw * (1 - Real.exp (-((1 - α) * Real.log 2))) := by
      unfold ψ1
      have hKm : K * μmin ≤ K * μw := mul_le_mul_of_nonneg_left hmw hK.le
      have hKm0 : 0 ≤ K * μmin := mul_nonneg hK.le hm0
      have hf : (1 / 2) * (1 - Real.exp (-(α * Real.log 2))) ≤ 1 := by linarith
      have hg : 0 ≤ 1 - Real.exp (-((1 - α) * Real.log 2)) := by linarith
      calc K * μmin * (1 / 2) * ((1 - Real.exp (-(α * Real.log 2))) *
              (1 - Real.exp (-((1 - α) * Real.log 2))))
          = K * μmin * ((1 / 2) * (1 - Real.exp (-(α * Real.log 2)))) *
              (1 - Real.exp (-((1 - α) * Real.log 2))) := by ring
        _ ≤ K * μmin * 1 * (1 - Real.exp (-((1 - α) * Real.log 2))) := by
              apply mul_le_mul_of_nonneg_right _ hg
              exact mul_le_mul_of_nonneg_left hf hKm0
        _ ≤ K * μw * (1 - Real.exp (-((1 - α) * Real.log 2))) := by
              rw [mul_one]; exact mul_le_mul_of_nonneg_right hKm hg
    linarith
  · exfalso
    push_neg at hbig
    have hlx : lam * xw < Real.log 2 := by rw [lt_div_iff₀ hl] at hbig; linarith
    have hξ0 : 0 ≤ lam * ((1 - α) * xw) := mul_nonneg hl.le (mul_nonneg h1α.le hw.le)
    have hξ1 : lam * ((1 - α) * xw) ≤ Real.log 2 := by nlinarith
    have h6 := lemma6 hξ0 hξ1
    have h7 : K * μw * (lam * ((1 - α) * xw) / 2) ≤ xw := by
      rw [ew'] at *; nlinarith [mul_pos hK hμw]
    have h8 : K * μw * (1 - α) * lam ≤ 2 := by
      by_contra h; push_neg at h; nlinarith
    rw [div_lt_iff₀ hk] at hlarge
    linarith

/-- A positive equilibrium cutoff equals the untruncated best response. -/
theorem bt_eq_of_pos {K μc μo α lam x y : ℝ} (h : x = b K μc μo α lam x y) (hx : 0 < x) :
    x = bt K μc μo α lam x y := by
  unfold b at h
  rcases le_total 0 (bt K μc μo α lam x y) with h1 | h1
  · rwa [max_eq_right h1] at h
  · rw [max_eq_left h1] at h; linarith

/-- Lemma 7, assembled. For `λ` above `2/(K μ_c (1 − α))` for both colours,
every equilibrium has `max{x^b, x^w} ≤ 2ψ₀λ^{−2}` or `max{x^b, x^w} ≥ ψ₁`,
with the constants corrected as in PS-D8. -/
theorem lemma7 {K μb μw μmin α lam xb xw : ℝ} (P : Params K μb μw α lam) (hK : 0 < K)
    (hl : 0 < lam) (hα1 : α < 1) (hm0 : 0 ≤ μmin) (hmb : μmin ≤ μb) (hmw : μmin ≤ μw)
    (hlarge_b : 2 / (K * μb * (1 - α)) < lam) (hlarge_w : 2 / (K * μw * (1 - α)) < lam)
    (h : IsEq K μb μw α lam xb xw) :
    (1 / 4) * K * α * (1 - α) * μmin * lam ^ 2 * max xb xw ≤ 2 ∨
      ψ1 K μmin α ≤ max xb xw := by
  obtain ⟨hx, hy, ex, ey⟩ := h
  rcases hx.lt_or_eq with hxp | hx0 <;> rcases hy.lt_or_eq with hyp | hy0
  · exact lemma7_both_pos hK hl P.hα0 hα1.le hm0 hmb hmw hxp hyp
      (bt_eq_of_pos ex hxp) (bt_eq_of_pos ey hyp)
  · right
    rw [← hy0] at ex ⊢
    rw [max_eq_left hx]
    exact lemma7_one_zero hK hl P.hα0 hα1 hm0 hmb hlarge_b hxp (bt_eq_of_pos ex hxp)
  · right
    rw [← hx0] at ey ⊢
    rw [max_eq_right hy]
    exact lemma7_one_zero hK hl P.hα0 hα1 hm0 hmw hlarge_w hyp (bt_eq_of_pos ey hyp)
  · left
    rw [← hx0, ← hy0]; simp

/-! ### Eq. (12) on the axis, the content of Theorem 2 we formalise -/

/-- The slope of `b̃^c(·, 0)` at the origin is `K μ_c (1 − α) λ`, the quantity
compared with 1 in Lemma 4 and Theorem 2. -/
theorem slope_at_zero (K μc μo α lam : ℝ) :
    HasDerivAt (fun x => bt K μc μo α lam x 0) (K * μc * (1 - α) * lam) 0 := by
  have h1 : HasDerivAt (fun x : ℝ => -(lam * ((1 - α) * x))) (-(lam * ((1 - α) * 1))) 0 :=
    (((hasDerivAt_id (0 : ℝ)).const_mul (1 - α)).const_mul lam).neg
  have h3 := ((h1.exp.const_sub 1).const_mul μc).const_mul K
  have e : (fun x => bt K μc μo α lam x 0)
      = fun x => K * (μc * (1 - Real.exp (-(lam * ((1 - α) * x))))) := by
    funext x; rw [bt_at_zero]; rfl
  rw [e]
  convert h3 using 1
  simp; ring

/-- Eq. (12) on the axis `x^{−c} = 0`, at or below the threshold. The drift
`b̃^c(x, 0) − x` is negative at every positive cutoff, so the cutoff falls back
to zero along the axis. The one-dimensional content of Theorem 2 (i). -/
theorem drift_neg_below {K μc μo α lam x : ℝ} (P : Params K μc μo α lam)
    (hthr : K * μc * (1 - α) * lam ≤ 1) (hx : 0 < x) : bt K μc μo α lam x 0 - x < 0 := by
  have h1 := lemma4_ii (μo := μo) P hthr hx
  have h2 : bt K μc μo α lam x 0 ≤ b K μc μo α lam x 0 := le_max_right _ _
  linarith

/-- Eq. (12) on the axis, above the threshold. Near zero the drift is
positive, so the cutoff moves away from the colour-blind profile. Case 1 of the
proof of Theorem 1, and the instability in Theorem 2 (ii). -/
theorem drift_pos_near_zero {K μc μo α lam : ℝ} (P : Params K μc μo α lam)
    (hthr : 1 < K * μc * (1 - α) * lam) :
    ∃ ε > 0, ∀ x, 0 < x → x < ε → 0 < bt K μc μo α lam x 0 - x := by
  obtain ⟨hA, ha⟩ := pos_of_thr P hthr
  have hAa : 1 < K * μc * (lam * (1 - α)) := by
    have : K * μc * (1 - α) * lam = K * μc * (lam * (1 - α)) := by ring
    linarith
  refine ⟨(K * μc * (lam * (1 - α)) - 1) / (lam * (1 - α)), div_pos (by linarith) ha,
    fun x hx hxe => ?_⟩
  rw [bt_zero_sub]
  have hax : lam * (1 - α) * x < K * μc * (lam * (1 - α)) - 1 := by
    rw [lt_div_iff₀ ha] at hxe; linarith
  have hlb := one_sub_exp_neg_ge (mul_pos ha hx).le
  have hden : 0 < 1 + lam * (1 - α) * x := by nlinarith
  have h1 : K * μc * (lam * (1 - α) * x / (1 + lam * (1 - α) * x))
      ≤ K * μc * (1 - Real.exp (-(lam * (1 - α) * x))) := mul_le_mul_of_nonneg_left hlb hA.le
  have h2 : x < K * μc * (lam * (1 - α) * x / (1 + lam * (1 - α) * x)) := by
    rw [mul_div_assoc', lt_div_iff₀ hden]
    nlinarith [mul_lt_mul_of_pos_left hax hx]
  unfold fp
  linarith

end PeskiSzentes2012
