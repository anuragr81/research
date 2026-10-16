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

/-! ## Second pass: the remaining structure of the model -/

/-- The two stage-game actions. -/
inductive Act
  | C
  | D
  deriving DecidableEq

/-- The stage-game pay-off `γ` to a player who plays `a` against `a'`. -/
def stage (g l : ℝ) : Act → Act → ℝ
  | .C, .C => 1
  | .C, .D => -l
  | .D, .C => 1 + g
  | .D, .D => 0

/-- Section 2. `D` strictly dominates `C`, so `(D, D)` is the unique Nash
equilibrium of the stage game. -/
theorem stage_dominant {g l : ℝ} (hg : 0 < g) (hl : 0 < l) (a' : Act) :
    stage g l .C a' < stage g l .D a' := by
  cases a' <;> simp [stage] <;> linarith

/-- Eq. (1), the normalisation. Cooperation forever is worth `1`. -/
theorem coop_forever {δ : ℝ} (h0 : 0 ≤ δ) (h1 : δ < 1) :
    (1 - δ) * ∑' t : ℕ, δ ^ t = 1 := by
  have hp : (1 - δ) ≠ 0 := (by linarith : (0 : ℝ) < 1 - δ).ne'
  rw [tsum_geometric_of_lt_one h0 h1]; exact mul_inv_cancel₀ hp

/-- Eq. (1) for incubation. Zero in the first period and `1` after is worth
`v_I = δ`. -/
theorem incubation_value {δ : ℝ} (h0 : 0 ≤ δ) (h1 : δ < 1) :
    (1 - δ) * ∑' t : ℕ, δ ^ (t + 1) = δ := by
  have : ∑' t : ℕ, δ ^ (t + 1) = δ * ∑' t : ℕ, δ ^ t := by
    rw [← tsum_mul_left]; congr 1; funext t; ring
  have hp : (1 - δ) ≠ 0 := (by linarith : (0 : ℝ) < 1 - δ).ne'
  rw [this, tsum_geometric_of_lt_one h0 h1]; field_simp

/-- Eq. (3). A one-shot deviation in the cooperation phase does not pay,
`(1 + g)(1 − δ) + δ v ≤ 1`, exactly when `v ≤ V̄`. -/
theorem eq3 {g δ v : ℝ} (h0 : 0 < δ) :
    (1 + g) * (1 - δ) + δ * v ≤ 1 ↔ v ≤ Vbar g δ := by
  unfold Vbar
  have e : 1 - ((1 + g) * (1 - δ) + δ * v) = δ * (1 - g * (1 - δ) / δ - v) := by
    field_simp; ring
  constructor
  · intro h
    have : 0 ≤ δ * (1 - g * (1 - δ) / δ - v) := by linarith
    have := (mul_nonneg_iff_of_pos_left h0).1 this
    linarith
  · intro h
    have : 0 ≤ δ * (1 - g * (1 - δ) / δ - v) := mul_nonneg h0.le (by linarith)
    linarith

/-- Footnote 13. With `k` periods of `(D, D)` the value is `δ^k`, and the
strategy survives a one-shot deviation exactly when
`g ≤ δ(1 − δ^k)/(1 − δ)`. -/
theorem k_incubation_ic {g δ : ℝ} (h1 : δ < 1) (k : ℕ) :
    (1 + g) * (1 - δ) + δ * δ ^ k ≤ 1 ↔ g ≤ δ * (1 - δ ^ k) / (1 - δ) := by
  have hp : 0 < 1 - δ := by linarith
  rw [le_div_iff₀ hp]
  constructor <;> intro h <;> nlinarith

/-- Footnote 13, `k = 2`. The bound is `δ(1 + δ)`. -/
theorem k_incubation_two {δ : ℝ} (h1 : δ < 1) :
    δ * (1 - δ ^ 2) / (1 - δ) = δ * (1 + δ) := by
  have hp : 1 - δ ≠ 0 := by intro h; linarith
  field_simp; ring

/-- The continuation pay-off in the subgame after first-period actions, when
the player continues with probability `p`, the partner with `q`, and
rematching is worth `v`. -/
def cont (p q v : ℝ) : ℝ := p * (q + (1 - q) * v) + (1 - p) * v

/-- Lemma A1. When rematching is worth less than cooperation, `v < 1`, the
pure Nash equilibria of the continuation subgame are exactly `(0, 0)` and
`(1, 1)`. So `P ∈ {0, 1}³`. -/
theorem lemmaA1 {v p q : ℝ} (hv : v < 1) (hp : p ∈ Set.Icc (0 : ℝ) 1)
    (hq : q ∈ Set.Icc (0 : ℝ) 1) :
    ((∀ p' ∈ Set.Icc (0 : ℝ) 1, cont p' q v ≤ cont p q v) ∧
      (∀ q' ∈ Set.Icc (0 : ℝ) 1, cont q' p v ≤ cont q p v)) ↔
      (p = 0 ∧ q = 0) ∨ (p = 1 ∧ q = 1) := by
  have hw : 0 < 1 - v := by linarith
  constructor
  · rintro ⟨h1, h2⟩
    have k1 := h1 1 ⟨by norm_num, le_rfl⟩
    have k2 := h2 1 ⟨by norm_num, le_rfl⟩
    unfold cont at k1 k2
    have k1' : (1 - p) * (q * (1 - v)) ≤ 0 := by nlinarith
    have k2' : (1 - q) * (p * (1 - v)) ≤ 0 := by nlinarith
    rcases hq.1.lt_or_eq with hq0 | hq0
    · have hp1 : p = 1 := by
        by_contra hne
        have : 0 < 1 - p := by
          rcases hp.2.lt_or_eq with h | h
          · linarith
          · exact absurd h hne
        have := mul_pos this (mul_pos hq0 hw)
        linarith
      have hq1 : q = 1 := by
        by_contra hne
        have : 0 < 1 - q := by
          rcases hq.2.lt_or_eq with h | h
          · linarith
          · exact absurd h hne
        have := mul_pos this (mul_pos (by rw [hp1]; norm_num : (0 : ℝ) < p) hw)
        linarith
      exact Or.inr ⟨hp1, hq1⟩
    · left
      refine ⟨?_, hq0.symm⟩
      rcases hp.1.lt_or_eq with hp0 | hp0
      · exfalso
        rw [← hq0] at k2'
        have := mul_pos hp0 hw
        linarith
      · exact hp0.symm
  · rintro (⟨rfl, rfl⟩ | ⟨rfl, rfl⟩)
    · exact ⟨fun p' _ => by unfold cont; nlinarith, fun q' _ => by unfold cont; nlinarith⟩
    · exact ⟨fun p' hp' => by unfold cont; nlinarith [hp'.2],
        fun q' hq' => by unfold cont; nlinarith [hq'.2]⟩

/-- Control: Lemma A1 needs `v < 1`. With `v = 1`, continuing with
probability one against a partner who ends the match is also an equilibrium. -/
theorem control_lemmaA1_needs_v_lt_one :
    (∀ p' ∈ Set.Icc (0 : ℝ) 1, cont p' 0 1 ≤ cont 1 0 1) ∧
      (∀ q' ∈ Set.Icc (0 : ℝ) 1, cont q' 1 1 ≤ cont 0 1 1) := by
  refine ⟨fun p' _ => ?_, fun q' _ => ?_⟩ <;> unfold cont <;> nlinarith

/-- Lemma A2. If `C` is played for sure, the value solves
`v = 1 − δ + δ(P₁ + (1 − P₁) v)`, so `v = 1`, above `V̄`. No such strategy is
an equilibrium. -/
theorem lemmaA2 {g δ P1 v : ℝ} (hg : 0 < g) (h0 : 0 < δ) (h1 : δ < 1) (hP : 0 ≤ P1)
    (h : v = 1 - δ + δ * (P1 + (1 - P1) * v)) : v = 1 ∧ Vbar g δ < v := by
  have hv : v = 1 := by
    have : (v - 1) * (1 - δ + δ * P1) = 0 := by linarith
    rcases mul_eq_zero.1 this with h2 | h2
    · linarith
    · nlinarith
  refine ⟨hv, ?_⟩
  rw [hv]; unfold Vbar
  have : 0 < g * (1 - δ) / δ := div_pos (mul_pos hg (by linarith)) h0
  linarith

/-- Proposition 1, the non-interior case. Incubation, `v = δ`, is below the
constrained efficient pay-off exactly when `g < δ`. -/
theorem prop1_incubation {g δ : ℝ} (h0 : 0 < δ) (h1 : δ < 1) : δ < Vbar g δ ↔ g < δ := by
  unfold Vbar
  have hp : 0 < 1 - δ := by linarith
  have e : 1 - g * (1 - δ) / δ - δ = (1 - δ) * (δ - g) / δ := by field_simp; ring
  constructor
  · intro h
    have : 0 < (1 - δ) * (δ - g) / δ := by linarith
    have := (div_pos_iff_of_pos_right h0).1 this
    have := (mul_pos_iff_of_pos_left hp).1 this
    linarith
  · intro h
    have : 0 < (1 - δ) * (δ - g) / δ := div_pos (mul_pos hp (by linarith)) h0
    linarith

/-- Control: at `g = δ` incubation is exactly constrained efficient,
`V̄ = δ`, so Proposition 1's strict inequality needs `g < δ`. -/
theorem control_prop1_boundary {δ : ℝ} (h0 : 0 < δ) : Vbar δ δ = δ := by
  unfold Vbar; field_simp; ring

/-- (A.2), the pay-off from playing `C` in the first period. -/
def F (σ P1 P2 l δ v : ℝ) : ℝ :=
  σ * (1 - δ + δ * (P1 + (1 - P1) * v)) + (1 - σ) * (-l * (1 - δ) + δ * (P2 + (1 - P2) * v))

/-- (A.3), the pay-off from playing `D` in the first period. -/
def Gd (σ P2 P3 g δ v : ℝ) : ℝ :=
  σ * ((1 + g) * (1 - δ) + δ * (P2 + (1 - P2) * v)) + (1 - σ) * δ * (P3 + (1 - P3) * v)

/-- (A.1). At an interior `σ`, indifference `F = G` makes the value equal to
both. -/
theorem eqA1 {σ F' G' : ℝ} (h : F' = G') : σ * F' + (1 - σ) * G' = F' := by rw [h]; ring

/-- Proof of Proposition 1, the interior case. Indifference `F = G` gives
`σ = (−l + δ/(1−δ)(P₂ − P₃)(1 − v)) / (g − l + δ/(1−δ)(2P₂ − P₁ − P₃)(1 − v))`.
The same formula, with `v(s^S)` for `v`, is `σ₁*` in A.2. -/
theorem sigma_interior {σ P1 P2 P3 g l δ v : ℝ} (h1 : δ < 1)
    (hden : g - l + δ / (1 - δ) * (2 * P2 - P1 - P3) * (1 - v) ≠ 0)
    (h : F σ P1 P2 l δ v = Gd σ P2 P3 g δ v) :
    σ = (-l + δ / (1 - δ) * (P2 - P3) * (1 - v)) /
      (g - l + δ / (1 - δ) * (2 * P2 - P1 - P3) * (1 - v)) := by
  have hp : (1 - δ) ≠ 0 := by intro h'; linarith
  have key : σ * (g - l + δ / (1 - δ) * (2 * P2 - P1 - P3) * (1 - v))
      - (-l + δ / (1 - δ) * (P2 - P3) * (1 - v))
      = (Gd σ P2 P3 g δ v - F σ P1 P2 l δ v) / (1 - δ) := by
    unfold F Gd; field_simp; ring
  rw [← h, sub_self, zero_div] at key
  rw [eq_div_iff hden]; linarith

/-- §3.3 and §4.1.1, the example `g = l = 0.5`, `δ = 0.8`, `P = (1, 1, 0)`.
Indifference and `v = F` give `σ = (10v − 7)/3` and `(1 − v)² = 3/80`. -/
theorem example_l05 {σ v : ℝ}
    (hF : v = F σ 1 1 (1 / 2) (4 / 5) v)
    (hFG : F σ 1 1 (1 / 2) (4 / 5) v = Gd σ 1 0 (1 / 2) (4 / 5) v) :
    σ = (10 * v - 7) / 3 ∧ (1 - v) ^ 2 = 3 / 80 := by
  unfold F at hF hFG; unfold Gd at hFG
  have hσ : σ = (10 * v - 7) / 3 := by linarith
  refine ⟨hσ, ?_⟩
  subst hσ
  nlinarith

/-- The printed values `v(s*) = 0·8063` and `σ = 0·3545` (PDF pp. 11 to 12). The
exact values satisfy `0·80635 < v < 0·80636` and `0·3545 < σ < 0·3546`, so the
printed `v` is truncated rather than rounded (EE-D9). -/
theorem example_l05_numbers {σ v : ℝ} (hv : v < 1)
    (hσ : σ = (10 * v - 7) / 3) (hq : (1 - v) ^ 2 = 3 / 80) :
    0.80635 < v ∧ v < 0.80636 ∧ 0.3545 < σ ∧ σ < 0.3546 := by
  have hw : 0 < 1 - v := by linarith
  have a1 : 1 - v < 0.19365 := by nlinarith
  have a2 : 0.19364 < 1 - v := by nlinarith
  refine ⟨by linarith, by linarith, by rw [hσ]; linarith, by rw [hσ]; linarith⟩

/-- Proposition 2 and §4.1.1, the inclusive strategy for any best
colour-blind pay-off below 1. Cooperating at once with one's own colour and
playing `s*` otherwise gives `π + (1 − π) v(s*) > v(s*)`. -/
theorem inclusive_pareto {π vs : ℝ} (hπ : 0 < π) (hv : vs < 1) : vs < π + (1 - π) * vs := by
  nlinarith

/-- §4.2.1. With incubation in mixed matches and cooperation in same-colour
matches, `v_w = π + (1 − π)δ` exceeds `v_b = (1 − π) + πδ` exactly by
`(2π − 1)(1 − δ)`, positive when `π > 1/2`. -/
theorem inclusive_majority_gains {π δ : ℝ} (hπ : 1 / 2 < π) (hδ : δ < 1) :
    (1 - π) + π * δ < π + (1 - π) * δ := by nlinarith

/-- A.5. Under minority bias `v_b > v_w`, by `π g (1 − δ) + (1 − π) l (1 − δ)`. -/
theorem minority_bias_vb_gt_vw {π g l δ : ℝ} (hπ0 : 0 < π) (hπ1 : π < 1) (hg : 0 < g)
    (hl : 0 < l) (hδ : δ < 1) : vw π l δ < vb π g δ := by
  unfold vw vb
  have e : (1 - π) * δ + π * ((1 + g) * (1 - δ) + δ) - (π + (1 - π) * (-l * (1 - δ) + δ))
      = π * g * (1 - δ) + (1 - π) * l * (1 - δ) := by ring
  have : 0 < π * g * (1 - δ) := mul_pos (mul_pos hπ0 hg) (by linarith)
  have : 0 < (1 - π) * l * (1 - δ) := mul_pos (mul_pos (by linarith) hl) (by linarith)
  linarith

/-- Proposition 4, assembled. If `π` lies in the printed range, minority bias
raises welfare, whites do not deviate in a mixed match (A.10), and the black
pay-off meets the incentive constraint (A.11). -/
theorem prop4 {π g l δ : ℝ} (h0 : 0 < δ) (h1 : δ < 1) (hg : 0 < g) (hl : 0 < l)
    (hgl : 0 < 2 + g - l) (hπ1 : π < 1)
    (hlo : 1 / (2 + g - l) < π) (hA10 : π ≤ 1 - l / ((1 + l) * δ))
    (hA11 : π ≤ 1 - g * (1 + δ) / ((1 + g) * δ)) :
    W π δ < π * vw π l δ + (1 - π) * vb π g δ ∧
      δ * vw π l δ ≤ -l * (1 - δ) + δ ∧ vb π g δ ≤ Vbar g δ :=
  ⟨(prop4_welfare hπ1 h1 hgl).2 hlo, (eqA10 h0 h1 hl).2 hA10, (eqA11 h0 h1 hg).2 hA11⟩

/-- Control: Proposition 4 needs `π > 1/(2 + g − l)`. At equality minority
bias leaves welfare unchanged. -/
theorem control_prop4_threshold {g l δ : ℝ} (hgl : 0 < 2 + g - l) :
    (1 / (2 + g - l)) * vw (1 / (2 + g - l)) l δ
      + (1 - 1 / (2 + g - l)) * vb (1 / (2 + g - l)) g δ = W (1 / (2 + g - l)) δ := by
  rw [prop4_identity]
  have : 1 / (2 + g - l) * (2 + g - l) - 1 = 0 := by field_simp; ring
  rw [this]; ring

/-- §4.2.3. For `π > 1/2`, any root `Π ∈ (0, 1)` of the singles quadratic
satisfies `1/2 < Π < π`. The text states `π ≥ Π ≥ 1/2`. -/
theorem singles_share_bounds {π Ps : ℝ} (hπ : 1 / 2 < π) (hπ1 : π < 1) (hPs0 : 0 < Ps)
    (hPs1 : Ps < 1) (hq : Ps ^ 2 * (1 - 2 * π) + Ps * 2 * π - π = 0) :
    1 / 2 < Ps ∧ Ps < π := by
  constructor
  · by_contra h; push_neg at h
    have : Ps ^ 2 * (1 - 2 * π) < 0 := mul_neg_of_pos_of_neg (by positivity) (by linarith)
    nlinarith
  · by_contra h; push_neg at h
    have e : Ps ^ 2 * (1 - 2 * π) + Ps * 2 * π - π
        = (Ps - π) * ((1 - 2 * π) * (Ps + π) + 2 * π) + π * (2 * π - 1) * (1 - π) := by ring
    have h2 : 0 < (1 - 2 * π) * (Ps + π) + 2 * π := by nlinarith
    have h3 : 0 < π * (2 * π - 1) * (1 - π) := by
      have := mul_pos (mul_pos (by linarith : (0 : ℝ) < π) (by linarith : (0 : ℝ) < 2 * π - 1))
        (by linarith : (0 : ℝ) < 1 - π)
      exact this
    have h4 : 0 ≤ (Ps - π) * ((1 - 2 * π) * (Ps + π) + 2 * π) :=
      mul_nonneg (by linarith) h2.le
    linarith

/-- §4.2.3. The majority's exclusive-segregation pay-off is below the inclusive
pay-off `π + (1 − π)δ` whenever `Π < π`. -/
theorem exclusive_below_inclusive {π Ps δ : ℝ} (h0 : 0 < δ) (h1 : δ < 1) (hPs0 : 0 ≤ Ps)
    (hPs : Ps < π) : Ps / (1 - (1 - Ps) * δ) < π + (1 - π) * δ := by
  have hd : 0 < 1 - (1 - Ps) * δ := by nlinarith
  rw [div_lt_iff₀ hd]
  have h2 : Ps + (1 - Ps) * δ < π + (1 - π) * δ := by
    nlinarith [mul_pos (sub_pos.2 hPs) (sub_pos.2 h1)]
  have h3 : Ps ≤ (Ps + (1 - Ps) * δ) * (1 - (1 - Ps) * δ) := by
    have e : (Ps + (1 - Ps) * δ) * (1 - (1 - Ps) * δ) - Ps
        = (1 - Ps) ^ 2 * (δ * (1 - δ)) := by ring
    have : 0 ≤ (1 - Ps) ^ 2 * (δ * (1 - δ)) :=
      mul_nonneg (sq_nonneg _) (mul_nonneg h0.le (by linarith))
    linarith
  have h4 := mul_lt_mul_of_pos_right h2 hd
  linarith

/-- §5.1. With an exogenous public randomisation device, cooperating when
`q` falls below `q̄` gives `q̄/(1 − (1 − q̄)δ)`, which meets the incentive
constraint exactly when `q̄ ≤ 1 − g/((1 + g)δ)`. -/
theorem public_randomization {q g δ : ℝ} (h0 : 0 < δ) (h1 : δ < 1) (hg : 0 < g)
    (hq0 : 0 ≤ q) :
    q / (1 - (1 - q) * δ) ≤ Vbar g δ ↔ q ≤ 1 - g / ((1 + g) * δ) := by
  unfold Vbar
  have hd : 0 < 1 - (1 - q) * δ := by nlinarith
  have hp : 0 < (1 + g) * δ := by nlinarith
  have e1 : 1 - g * (1 - δ) / δ = (δ - g * (1 - δ)) / δ := by field_simp
  have e2 : 1 - g / ((1 + g) * δ) = ((1 + g) * δ - g) / ((1 + g) * δ) := by field_simp
  rw [e1, e2, div_le_div_iff₀ hd h0, le_div_iff₀ hp]
  constructor <;> intro h <;> nlinarith

/-- §3.3, the example's constrained efficient pay-off, `V̄ = 0·875` at
`g = 0·5`, `δ = 0·8`. -/
theorem Vbar_example : Vbar (1 / 2) (4 / 5) = 7 / 8 := by unfold Vbar; norm_num

/-- Table 2, the row `l = 1`, where `s₁* = s₂* = s*` is incubation with
`v(s*) = 0·8`. The entries `0·9`, `0·8667` and `0·72` follow, and `0·9`
violates `V̄ = 0·875` as the text says. -/
theorem table2_l1 :
    (1 / 2 : ℝ) + (1 - 1 / 2) * (4 / 5) = 9 / 10 ∧ (9 / 10 : ℝ) > 7 / 8 ∧
    (1 / 3 : ℝ) + (1 - 1 / 3) * (4 / 5) = 13 / 15 ∧ (13 / 15 : ℝ) ≤ 7 / 8 ∧
    ((1 / 2 : ℝ) * (4 / 5) + (1 - 1 / 2)) * (4 / 5) = 18 / 25 := by norm_num

/-- Table 2, the row `l = 1`, entry `v(s₂^S)` at `π = 1/3`. The printed
formula gives `56/75 ≈ 0·7467`, not the printed `0·6133` (EE-D8). -/
theorem table2_discrepancy :
    ((1 / 3 : ℝ) * (4 / 5) + (1 - 1 / 3)) * (4 / 5) = 56 / 75 ∧ (56 / 75 : ℝ) ≠ 0.6133 := by
  norm_num

end Eeckhout2006
