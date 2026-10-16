import Mathlib

/-! Eeckhout, "Minorities and Endogenous Segregation", *Review of Economic
Studies* 73 (2006), 31–53, in the paper's notation. The stage game is a
prisoner's dilemma with temptation `g`, sucker loss `l` and mutual
cooperation 1; `δ` is the discount factor; `π` is the white (majority) share
of the population and `Π` the white share among singles (written `Ps`, since `Π` is reserved). `Vbar` is the
constrained efficient pay-off `V̄ = 1 − g (1 − δ)/δ`.

Formalised: the incubation constraint (2), the incubation case of
Proposition 2, the algebraic step of Proposition 3, the welfare identity and
the two incentive constraints (A.10) and (A.11) of Proposition 4, the
stationary singles share of A.6 and its root, and the conditions of §4.2.3
under which exclusive segregation leaves the minority worse off. Not
formalised: Proposition 1 (a genericity argument) and the interior-`σ` case
of Proposition 2 (an implicit-function argument). -/

namespace Eeckhout2006

/-- `V̄ = 1 − g (1 − δ)/δ`, the highest pay-off with no deviation in the
cooperation phase, eq. (3). -/
noncomputable def Vbar (g δ : ℝ) : ℝ := 1 - g * (1 - δ) / δ

/-- Eq. (2). With incubation, `v_I = δ`, and a one-shot deviation in the
cooperation phase does not pay, `1 ≥ (1 + g)(1 − δ) + δ v_I`, exactly when
`g ≤ δ`. -/
theorem incubation_ic {g δ : ℝ} (h1 : δ < 1) :
    (1 + g) * (1 - δ) + δ * δ ≤ 1 ↔ g ≤ δ := by
  have hp : 0 < 1 - δ := by linarith
  constructor
  · intro h; nlinarith
  · intro h; nlinarith

/-- Proposition 2, the incubation case. Cooperating at once with one's own
colour and incubating otherwise gives `v(s^S) = π + (1 − π) δ`, above the
colour-blind `v(s*) = δ` whenever a same-colour match has positive
probability. -/
theorem prop2_incubation_pareto {π δ : ℝ} (hπ : 0 < π) (hδ : δ < 1) :
    δ < π + (1 - π) * δ := by nlinarith

/-- Proposition 2, the incubation case. The segregation pay-off satisfies the
incentive constraint `v(s^S) ≤ V̄` exactly when `π ≤ 1 − g/δ`, so a small
enough group exists whenever `g < δ`. -/
theorem prop2_incubation_ic {g π δ : ℝ} (h0 : 0 < δ) (h1 : δ < 1) :
    π + (1 - π) * δ ≤ Vbar g δ ↔ π ≤ 1 - g / δ := by
  unfold Vbar
  have hp : 0 < 1 - δ := by linarith
  have e : 1 - g * (1 - δ) / δ - (π + (1 - π) * δ) = (1 - δ) * (1 - g / δ - π) := by
    field_simp; ring
  constructor
  · intro h
    have : 0 ≤ (1 - δ) * (1 - g / δ - π) := by linarith
    have := (mul_nonneg_iff_of_pos_left hp).1 this
    linarith
  · intro h
    have : 0 ≤ (1 - δ) * (1 - g / δ - π) := mul_nonneg hp.le (by linarith)
    linarith

/-- Proposition 3, the step in A.4. With symmetric type-dependent strategies
the pay-offs are `v_w = Ps v(s_i) + (1 − Ps) v(s_{−i})` and
`v_b = (1 − Ps) v(s_i) + Ps v(s_{−i})`. If they are equal and `Ps ≠ 1/2`, the
strategies give the same pay-off in same-colour and mixed matches. -/
theorem prop3_step {Ps vi vo : ℝ} (hPs : Ps ≠ 1 / 2)
    (h : Ps * vi + (1 - Ps) * vo = (1 - Ps) * vi + Ps * vo) : vi = vo := by
  have e : (1 - 2 * Ps) * (vi - vo) = 0 := by linarith
  rcases mul_eq_zero.1 e with h1 | h1
  · exfalso; apply hPs; linarith
  · linarith

/-- Control: Proposition 3 needs `Ps ≠ 1/2`. With equal shares the two
colours get equal pay-offs whatever the same-colour and mixed pay-offs are. -/
theorem control_prop3_equal_shares (vi vo : ℝ) :
    (1 / 2 : ℝ) * vi + (1 - 1 / 2) * vo = (1 - 1 / 2) * vi + 1 / 2 * vo := by ring

/-- Eq. (4), the white pay-off under minority bias. -/
def vw (π l δ : ℝ) : ℝ := π + (1 - π) * (-l * (1 - δ) + δ)

/-- Eq. (5), the black pay-off under minority bias. -/
def vb (π g δ : ℝ) : ℝ := (1 - π) * δ + π * ((1 + g) * (1 - δ) + δ)

/-- Aggregate pay-off under symmetric segregation with incubation in mixed
matches, A.5. -/
def W (π δ : ℝ) : ℝ := π * (π + (1 - π) * δ) + (1 - π) * ((1 - π) + π * δ)

/-- Proposition 4, the identity in A.5,
`W^m = W + (1 − π)(1 − δ)[π(2 + g − l) − 1]`. -/
theorem prop4_identity (π g l δ : ℝ) :
    π * vw π l δ + (1 - π) * vb π g δ
      = W π δ + (1 - π) * (1 - δ) * (π * (2 + g - l) - 1) := by
  unfold vw vb W; ring

/-- Proposition 4. Minority bias raises aggregate welfare exactly when
`π > 1/(2 + g − l)`. -/
theorem prop4_welfare {π g l δ : ℝ} (hπ : π < 1) (hδ : δ < 1) (hgl : 0 < 2 + g - l) :
    W π δ < π * vw π l δ + (1 - π) * vb π g δ ↔ 1 / (2 + g - l) < π := by
  rw [prop4_identity]
  have hp : 0 < (1 - π) * (1 - δ) := mul_pos (by linarith) (by linarith)
  rw [div_lt_iff₀ hgl]
  constructor
  · intro h
    have : 0 < (1 - π) * (1 - δ) * (π * (2 + g - l) - 1) := by linarith
    have := (mul_pos_iff_of_pos_left hp).1 this
    linarith
  · intro h
    have : 0 < (1 - π) * (1 - δ) * (π * (2 + g - l) - 1) := mul_pos hp (by linarith)
    linarith

/-- (A.10). A white in the first phase of a mixed match does not deviate,
`δ v_w ≤ −l(1 − δ) + δ`, exactly when `π ≤ 1 − l/((1 + l) δ)`. -/
theorem eqA10 {π l δ : ℝ} (h0 : 0 < δ) (h1 : δ < 1) (hl : 0 < l) :
    δ * vw π l δ ≤ -l * (1 - δ) + δ ↔ π ≤ 1 - l / ((1 + l) * δ) := by
  unfold vw
  have hp : 0 < (1 + l) * δ := by nlinarith
  have hq : 0 < 1 - δ := by linarith
  have e : -l * (1 - δ) + δ - δ * (π + (1 - π) * (-l * (1 - δ) + δ))
      = (1 - δ) * ((1 + l) * δ) * (1 - l / ((1 + l) * δ) - π) := by
    field_simp; ring
  have hpq : 0 < (1 - δ) * ((1 + l) * δ) := mul_pos hq hp
  constructor
  · intro h
    have : 0 ≤ (1 - δ) * ((1 + l) * δ) * (1 - l / ((1 + l) * δ) - π) := by linarith
    have := (mul_nonneg_iff_of_pos_left hpq).1 this
    linarith
  · intro h
    have : 0 ≤ (1 - δ) * ((1 + l) * δ) * (1 - l / ((1 + l) * δ) - π) :=
      mul_nonneg hpq.le (by linarith)
    linarith

/-- (A.11). The black pay-off satisfies `v_b ≤ V̄` exactly when
`π ≤ 1 − g(1 + δ)/((1 + g) δ)`. -/
theorem eqA11 {π g δ : ℝ} (h0 : 0 < δ) (h1 : δ < 1) (hg : 0 < g) :
    vb π g δ ≤ Vbar g δ ↔ π ≤ 1 - g * (1 + δ) / ((1 + g) * δ) := by
  unfold vb Vbar
  have hp : 0 < (1 + g) * δ := by nlinarith
  have hq : 0 < 1 - δ := by linarith
  have e : 1 - g * (1 - δ) / δ - ((1 - π) * δ + π * ((1 + g) * (1 - δ) + δ))
      = (1 - δ) * (1 + g) * (1 - g * (1 + δ) / ((1 + g) * δ) - π) := by
    field_simp; ring
  have hpq : 0 < (1 - δ) * (1 + g) := mul_pos hq (by linarith)
  constructor
  · intro h
    have : 0 ≤ (1 - δ) * (1 + g) * (1 - g * (1 + δ) / ((1 + g) * δ) - π) := by linarith
    have := (mul_nonneg_iff_of_pos_left hpq).1 this
    linarith
  · intro h
    have : 0 ≤ (1 - δ) * (1 + g) * (1 - g * (1 + δ) / ((1 + g) * δ) - π) :=
      mul_nonneg hpq.le (by linarith)
    linarith

/-- A.6. Under exclusive segregation the stationary measures of single whites
and blacks are `n_w = π/Ps` and `n_b = (1 − π)/(1 − Ps)` (inflow `λ = 1`), and
the white share among singles `Ps = n_w/(n_w + n_b)` then solves
`Ps²(1 − 2π) + 2πPs − π = 0`. -/
theorem singles_quadratic {π Ps : ℝ} (hPs0 : 0 < Ps) (hPs1 : Ps < 1) (hπ0 : 0 < π)
    (hπ1 : π < 1) (h : Ps = (π / Ps) / (π / Ps + (1 - π) / (1 - Ps))) :
    Ps ^ 2 * (1 - 2 * π) + Ps * 2 * π - π = 0 := by
  have h1 : 0 < 1 - Ps := by linarith
  have hs : 0 < π / Ps + (1 - π) / (1 - Ps) :=
    add_pos (div_pos hπ0 hPs0) (div_pos (by linarith) h1)
  rw [eq_div_iff hs.ne'] at h
  field_simp at h
  nlinarith

/-- A.6. The positive root `Ps = (−π + √(π(1 − π)))/(1 − 2π)` stated on p. 44
solves the quadratic. -/
theorem singles_root {π : ℝ} (hπ0 : 0 ≤ π) (hπ1 : π ≤ 1) (hh : π ≠ 1 / 2) :
    let Ps := (-π + Real.sqrt (π * (1 - π))) / (1 - 2 * π)
    Ps ^ 2 * (1 - 2 * π) + Ps * 2 * π - π = 0 := by
  intro Ps
  have hd : (1 - 2 * π) ≠ 0 := by intro h; apply hh; linarith
  have hs : Real.sqrt (π * (1 - π)) ^ 2 = π * (1 - π) :=
    Real.sq_sqrt (mul_nonneg hπ0 (by linarith))
  have hPs : Ps * (1 - 2 * π) = -π + Real.sqrt (π * (1 - π)) := by
    show (-π + Real.sqrt (π * (1 - π))) / (1 - 2 * π) * (1 - 2 * π) = _
    exact div_mul_cancel₀ _ hd
  have key : (Ps ^ 2 * (1 - 2 * π) + Ps * 2 * π - π) * (1 - 2 * π) = 0 := by
    have : (Ps ^ 2 * (1 - 2 * π) + Ps * 2 * π - π) * (1 - 2 * π)
        = (Ps * (1 - 2 * π)) ^ 2 + 2 * π * (Ps * (1 - 2 * π)) - π * (1 - 2 * π) := by ring
    rw [this, hPs]; nlinarith [hs]
  rcases mul_eq_zero.1 key with h | h
  · exact h
  · exact absurd h hd

/-- §4.2.3. The minority's pay-off under exclusive segregation,
`(1 − Ps)/(1 − Ps δ)`, is below the incubation pay-off `δ` exactly when
`Ps > Ps1 = 1/(1 + δ)`. -/
theorem minority_worse {Ps δ : ℝ} (h0 : 0 < δ) (h1 : δ < 1) (hPs1 : Ps ≤ 1) :
    (1 - Ps) / (1 - Ps * δ) < δ ↔ 1 / (1 + δ) < Ps := by
  have hd : 0 < 1 - Ps * δ := by nlinarith
  rw [div_lt_iff₀ hd, div_lt_iff₀ (by linarith)]
  constructor <;> intro h <;> nlinarith

/-- §4.2.3. The majority's exclusive-segregation pay-off `Ps/(1 − (1 − Ps) δ)`
satisfies the incentive constraint strictly exactly when
`Ps < Ps0 = 1 − g/((1 + g) δ)`. -/
theorem majority_ic {Ps g δ : ℝ} (h0 : 0 < δ) (h1 : δ < 1) (hg : 0 < g)
    (hPs0 : 0 ≤ Ps) :
    Ps / (1 - (1 - Ps) * δ) < Vbar g δ ↔ Ps < 1 - g / ((1 + g) * δ) := by
  unfold Vbar
  have hd : 0 < 1 - (1 - Ps) * δ := by nlinarith
  have hp : 0 < (1 + g) * δ := by nlinarith
  have e1 : 1 - g * (1 - δ) / δ = (δ - g * (1 - δ)) / δ := by field_simp
  have e2 : 1 - g / ((1 + g) * δ) = ((1 + g) * δ - g) / ((1 + g) * δ) := by field_simp
  rw [e1, e2, div_lt_div_iff₀ hd h0, lt_div_iff₀ hp]
  constructor <;> intro h <;> nlinarith

/-- §4.2.3. The interval `(Ps1, Ps0)` on which the minority loses is non-empty
exactly when `g < δ²/(1 + δ − δ²)`. -/
theorem interval_nonempty {g δ : ℝ} (h0 : 0 < δ) (h1 : δ < 1) (hg : 0 < g) :
    1 / (1 + δ) < 1 - g / ((1 + g) * δ) ↔ g < δ ^ 2 / (1 + δ - δ ^ 2) := by
  have hp : 0 < (1 + g) * δ := by nlinarith
  have hq : 0 < 1 + δ - δ ^ 2 := by nlinarith
  have e2 : 1 - g / ((1 + g) * δ) = ((1 + g) * δ - g) / ((1 + g) * δ) := by field_simp
  rw [e2, div_lt_div_iff₀ (by linarith) hp, lt_div_iff₀ hq]
  constructor <;> intro h <;> nlinarith

/-- Control: the minority's loss needs an unequal split of singles. At
`Ps = 1/2` exclusive segregation gives each colour `1/(2 − δ) ≥ δ`, no less
than the incubation pay-off. -/
theorem control_equal_shares_no_loss {δ : ℝ} (h0 : 0 < δ) (h1 : δ < 1) :
    ¬ ((1 - 1 / 2 : ℝ) / (1 - 1 / 2 * δ) < δ) := by
  rw [minority_worse h0 h1 (by norm_num)]
  rw [not_lt, le_div_iff₀ (by linarith)]
  linarith

/-- Control: Proposition 2's gain needs same-colour matches. With `π = 0`
the segregation pay-off equals the colour-blind pay-off `δ`. -/
theorem control_no_same_colour_no_gain (δ : ℝ) : (0 : ℝ) + (1 - 0) * δ = δ := by ring

end Eeckhout2006
