import Mathlib

/-! Hitsch, Hortaçsu and Ariely, "What Makes You Click? — Mate Preferences and
Matching Outcomes in Online Dating", MIT Sloan Working Paper 4603-06, February
2006, in the paper's notation, for one scalar attribute.

Eq. (5): the latent utility of man `m` from woman `w` is
`x_w β + (|x_w − x_m|₊)^α γ⁺ + (|x_w − x_m|₋)^α γ⁻` plus categorical terms and a
taste shock, with `|a − b|₊ = max(a − b, 0)` and `|a − b|₋ = max(b − a, 0)`
(footnote 20). `m` contacts `w` iff the utility is at least his reservation value
`c_m` (eq. 1). The categorical terms and the shock are left out here. -/

namespace HitschEtAl2006

/-- `|a − b|₊`, footnote 20. -/
noncomputable def dpos (a b : ℝ) : ℝ := max (a - b) 0

/-- `|a − b|₋`, footnote 20. -/
noncomputable def dneg (a b : ℝ) : ℝ := max (b - a) 0

/-- Eq. (5) for one attribute, without the categorical terms and the shock. -/
noncomputable def UM (β γp γm : ℝ) (α : ℕ) (xw xm : ℝ) : ℝ :=
  xw * β + dpos xw xm ^ α * γp + dneg xw xm ^ α * γm

/-- Footnote 21: `x_w − |x_w − x_m|₊ + |x_w − x_m|₋ = x_m`. -/
theorem fn21_identity (xw xm : ℝ) : xw - dpos xw xm + dneg xw xm = xm := by
  unfold dpos dneg
  rcases le_total xw xm with h | h
  · rw [max_eq_right (by linarith), max_eq_left (by linarith)]; ring
  · rw [max_eq_left (by linarith), max_eq_right (by linarith)]; ring

/-- Footnote 21: with linear differences (`α = 1`) the model is not identified.
For `x_m ≠ 0` and any `a`, the parameters `β + a/x_m`, `γ⁺ − a/x_m`,
`γ⁻ + a/x_m` with the fixed effect `c_m + a` give the same utility net of the
fixed effect at every `x_w`. -/
theorem not_identified_linear {β γp γm c a xm : ℝ} (hxm : xm ≠ 0) (xw : ℝ) :
    UM (β + a / xm) (γp - a / xm) (γm + a / xm) 1 xw xm - (c + a)
      = UM β γp γm 1 xw xm - c := by
  have key := fn21_identity xw xm
  unfold UM
  simp only [pow_one]
  have : a / xm * (xw - dpos xw xm + dneg xw xm) = a := by
    rw [key]; field_simp
  linear_combination this

/-- Control: with quadratic differences (`α = 2`, the paper's logit
specification) the reparametrisation of footnote 21 changes the utility. At
`x_m = 1`, `x_w = 3`, `a = 1` it moves `U − c` by `−2`. -/
theorem control_identified_quadratic {β γp γm c : ℝ} :
    UM (β + 1 / 1) (γp - 1 / 1) (γm + 1 / 1) 2 3 1 - (c + 1)
      ≠ UM β γp γm 2 3 1 - c := by
  unfold UM dpos dneg
  norm_num
  intro h
  linarith

/-- With symmetric quadratic distance terms `γ⁺ = γ⁻ = −κ`, eq. (5) is
`β x_w − κ (x_w − x_m)²`, the form of Paper A1's engagement utility in an
observed attribute. -/
theorem symmetric_quadratic (β κ xw xm : ℝ) :
    UM β (-κ) (-κ) 2 xw xm = β * xw - κ * (xw - xm) ^ 2 := by
  unfold UM dpos dneg
  rcases le_total xw xm with h | h
  · rw [max_eq_right (by linarith), max_eq_left (by linarith)]; ring
  · rw [max_eq_left (by linarith), max_eq_right (by linarith)]; ring

/-- Section 5.1, assumption 1. Without distance terms (`γ⁺ = γ⁻ = 0`) every man
ranks women alike: the ranking of `w` and `w'` does not depend on `x_m`. -/
theorem vertical_rank_indep_own {β : ℝ} {α : ℕ} (xw xw' xm xm' : ℝ) :
    (UM β 0 0 α xw' xm ≤ UM β 0 0 α xw xm) ↔ (UM β 0 0 α xw' xm' ≤ UM β 0 0 α xw xm') := by
  unfold UM; simp

/-- Control: assumption 1 fails once the distance terms are present. With
`γ⁺ = γ⁻ = −1`, `β = 0`, a man at `x_m = 0` strictly prefers `x_w = 0` to
`x_w = 2`, and a man at `x_m = 2` strictly prefers the reverse. -/
theorem control_distance_terms_break_assumption1 :
    UM 0 (-1) (-1) 2 2 0 < UM 0 (-1) (-1) 2 0 0 ∧
      UM 0 (-1) (-1) 2 0 2 < UM 0 (-1) (-1) 2 2 2 := by
  rw [symmetric_quadratic, symmetric_quadratic, symmetric_quadratic, symmetric_quadratic]
  norm_num

/-! ### Trade-offs, Section 5.2 and Tables 5.4 to 5.6

With utility linear in income `y` with weight `b > 0` and a group term `ϑ < 0`,
the income that compensates is `−ϑ / b`. Where the marginal utility of income is
flat, no income compensates, which the paper reports as "Not feasible". -/

/-- The compensating income for a group term `ϑ` with income weight `b ≠ 0`. -/
theorem compensating_income {b ϑ y : ℝ} (hb : b ≠ 0) :
    b * (y + -ϑ / b) + ϑ = b * y := by
  field_simp; ring

/-- Control: with income utility capped at `ȳ`, `b · min y ȳ`, a negative group
term cannot be compensated by any extra income once `y ≥ ȳ`. -/
theorem control_flat_income_not_feasible {b ϑ y ybar : ℝ} (hϑ : ϑ < 0) (hy : ybar ≤ y) :
    ¬ ∃ Δ, 0 ≤ Δ ∧ b * min (y + Δ) ybar + ϑ ≥ b * min y ybar := by
  rintro ⟨Δ, hΔ, h⟩
  rw [min_eq_right (by linarith), min_eq_right hy] at h
  linarith

/-- Women's valuation of a man's income (in $1,000) in the fixed effects logit of
Table 5.3: `0.0171 y − 0.0058 (y − 50)₊ − 0.0094 (y − 100)₊ + 0.0076 (y − 200)₊`,
with the income difference terms, printed as `0.0000`, left out. -/
noncomputable def incomeW (y : ℝ) : ℝ :=
  171 / 10000 * y - 58 / 10000 * max (y - 50) 0 - 94 / 10000 * max (y - 100) 0
    + 76 / 10000 * max (y - 200) 0

/-- Table 5.6, our approximate reconstruction. From a baseline of $62,500, the
extra income that offsets the term `White, Mate: Black = −0.7159` lies between
$148,000 and $149,000 under the rounded coefficients, against the paper's
$154,000 (HHA-D3). -/
theorem table56_black_approx :
    incomeW (625 / 10 + 148) < incomeW (625 / 10) + 7159 / 10000 ∧
      incomeW (625 / 10) + 7159 / 10000 < incomeW (625 / 10 + 149) := by
  unfold incomeW
  norm_num [max_def]

/-! ### Section 3. The threshold rule and the search model -/

/-- Eq. (1): man `m` sends an e-mail to a browsed woman iff the utility of a match
is at least his outside option. -/
def Contacts (U v : ℝ) : Prop := v ≤ U

/-- The acceptance indicators `A_W`, `A_M` of Section 3 and the expected utility of
a meeting, `EU_M(m, w) = U A_M A_W + v (1 − A_W) A_M + v (1 − A_M)`. -/
noncomputable def EUM (U v : ℝ) (aM aW : Bool) : ℝ :=
  (if aM ∧ aW then U else v)

/-- The displayed `EU_M` with 0/1 indicators equals the case form: the match
utility on mutual acceptance and the outside option otherwise. -/
theorem EUM_display (U v : ℝ) (aM aW : Bool) :
    EUM U v aM aW = U * (if aM then 1 else 0) * (if aW then 1 else 0)
      + v * (1 - (if aW then 1 else 0)) * (if aM then 1 else 0)
      + v * (1 - (if aM then 1 else 0)) := by
  unfold EUM; cases aM <;> cases aW <;> simp

/-- Eq. (2) on a finite market with sampling weights `F w`: the reservation utility is
the discounted expected utility of the next meeting. A profile `v` solves eq. (2) when
this holds for every man. -/
noncomputable def bellmanM {M W : Type*} [Fintype W] (ρ : ℝ) (F : W → ℝ)
    (U : M → W → ℝ) (UW : W → M → ℝ) (vM : M → ℝ) (vW : W → ℝ) (m : M) : ℝ :=
  ρ * ∑ w, F w * (if vM m ≤ U m w ∧ vW w ≤ UW w m then U m w else vM m)

/-! ### Section 5.1. Under assumptions 1 and 2, higher-ranked users receive more e-mails

All men rank women by one index `u w` (assumption 1), every man browses every woman
with equal probability (assumption 2), and man `m` writes iff `v m ≤ u w`. The number
of e-mails woman `w` receives is then monotone in her index. -/

noncomputable def received {M : Type*} [Fintype M] (v : M → ℝ) (uw : ℝ) : ℕ :=
  (Finset.univ.filter (fun m => v m ≤ uw)).card

theorem received_mono {M : Type*} [Fintype M] (v : M → ℝ) {u u' : ℝ} (h : u ≤ u') :
    received v u ≤ received v u' := by
  unfold received
  apply Finset.card_le_card
  intro m; simp only [Finset.mem_filter, Finset.mem_univ, true_and]
  intro hm; linarith

/-- Control: without assumption 1, each man has his own valuation `u m` of a woman,
and the count received need not order two women by any common index: here woman 1 is
valued above woman 2 by man 1 and below by man 2, and both receive one e-mail. -/
theorem control_no_common_index :
    (Finset.univ.filter (fun m : Fin 2 => (0 : ℤ) ≤ ![1, -1] m)).card
      = (Finset.univ.filter (fun m : Fin 2 => (0 : ℤ) ≤ ![-1, 1] m)).card := by
  decide

/-! ### Poisson regression (Section 5.1)

`E(Y | x) = exp(x′θ)`. Two users differing only in one attribute with coefficient `θ_j`
have expected outcomes in the ratio `exp(θ_j)`, the incidence rate ratio. -/

theorem incidence_rate_ratio (xθ θj : ℝ) : Real.exp (xθ + θj) / Real.exp xθ = Real.exp θj := by
  rw [Real.exp_add]; field_simp

/-- PDF p. 20: the "college premium" `100 (exp(0.27) − 1)` is 31 percent, with
`exp(0.27)` between 1.30 and 1.32. -/
theorem college_premium : 1.30 < Real.exp 0.27 ∧ Real.exp 0.27 < 1.32 := by
  constructor
  · have := Real.quadratic_le_exp_of_nonneg (x := 0.27) (by norm_num)
    linarith [show (1 : ℝ) + 0.27 + 0.27 ^ 2 / 2 = 1.30645 by norm_num]
  · have := Real.exp_bound' (x := 0.27) (by norm_num) (by norm_num) (n := 3) (by norm_num)
    simp [Finset.sum_range_succ, Nat.factorial] at this
    linarith [this, show (1 : ℝ) + 0.27 + 0.27 ^ 2 / 2 + 0.27 ^ 3 * (3 + 1) / (6 * 3) < 1.32 by norm_num]

/-- HHA-D2: the printed coefficient 0.21 gives `exp(0.21) < 1.24`, not 1.31. -/
theorem printed_coefficient_inconsistent : Real.exp 0.21 < 1.24 := by
  have := Real.exp_bound' (x := 0.21) (by norm_num) (by norm_num) (n := 3) (by norm_num)
  simp [Finset.sum_range_succ, Nat.factorial] at this
  linarith [this, show (1 : ℝ) + 0.21 + 0.21 ^ 2 / 2 + 0.21 ^ 3 * (3 + 1) / (6 * 3) < 1.24 by norm_num]

/-! ### Section 5.2. The choice probabilities -/

/-- The fixed effects logit: `Pr{m contacts w | m browses w} = exp(U − c)/(1 + exp(U − c))`. -/
noncomputable def logitProb (U c : ℝ) : ℝ := Real.exp (U - c) / (1 + Real.exp (U - c))

/-- The logit probability rises in utility, so a lower utility from a partner's group
means fewer contacts. -/
theorem logitProb_strictMono (c : ℝ) : StrictMono (fun U => logitProb U c) := by
  intro U U' h
  unfold logitProb
  have e1 := Real.exp_pos (U - c)
  have e2 := Real.exp_pos (U' - c)
  have : Real.exp (U - c) < Real.exp (U' - c) := Real.exp_lt_exp.mpr (by linarith)
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith

/-- The probability depends on `U` and the fixed effect only through `U − c`, which
is why the fixed effect absorbs anything common to a chooser (footnotes 21, 23). -/
theorem logitProb_shift (U c a : ℝ) : logitProb (U + a) (c + a) = logitProb U c := by
  unfold logitProb; ring_nf

/-! ### Section 6.3. Absolute versus relative preferences

Two men and two women, each with a level `0` (high) or `1` (low). A matching is a
permutation `μ` (man `i` with woman `μ i`). It is stable unless some man and woman
both strictly prefer each other to their partners. Utilities are natural numbers so
that stability can be decided. -/

def Stable (UM UW : Fin 2 → Fin 2 → ℕ) (μ : Equiv.Perm (Fin 2)) : Prop :=
  ¬ ∃ m w, UM m (μ m) < UM m w ∧ UW w (μ.symm w) < UW w m

instance (UM UW : Fin 2 → Fin 2 → ℕ) (μ : Equiv.Perm (Fin 2)) : Decidable (Stable UM UW μ) := by
  unfold Stable; infer_instance

/-- Vertical preferences: everyone prefers the high partner. -/
def vertical : Fin 2 → Fin 2 → ℕ := fun _ partner => if partner = 0 then 2 else 1

/-- Preferences for similarity: everyone prefers a partner of the same level. -/
def similar : Fin 2 → Fin 2 → ℕ := fun self partner => if self = partner then 2 else 1

/-- PDF p. 34: vertical preferences and preferences for similarity both make the
assortative matching (high with high) the only stable one, so sorting alone does not
tell them apart. -/
theorem sorting_observationally_equivalent :
    (∀ μ : Equiv.Perm (Fin 2), Stable vertical vertical μ ↔ μ = 1) ∧
      (∀ μ : Equiv.Perm (Fin 2), Stable similar similar μ ↔ μ = 1) := by
  decide

/-- Control: preferences for a partner of the other level make the cross matching the
only stable one, so assortative sorting is not automatic. -/
def opposite : Fin 2 → Fin 2 → ℕ := fun self partner => if self = partner then 1 else 2

theorem control_opposite_tastes :
    ∀ μ : Equiv.Perm (Fin 2), Stable opposite opposite μ ↔ μ = Equiv.swap 0 1 := by
  decide

/-! ### Arithmetic in the tables and text -/

/-- Notes to Tables 5.4 to 5.6 and PDF p. 28: totals from the $62,500 baseline. -/
theorem table_note_totals :
    (62.5 : ℚ) + 143 = 205.5 ∧ (62.5 : ℚ) + 269 = 331.5 ∧ (62.5 : ℚ) + 247 = 309.5 ∧
      (62.5 : ℚ) + 186 = 248.5 := by norm_num

/-- PDF p. 35: under random matching a man's partner is of group `g` with probability
equal to `g`'s share of women, so 74.6 percent of black men would marry white women,
against 12.2 percent observed (Table 6.2, Panel I). -/
theorem random_matching_benchmark : (74.6 : ℚ) - 12.2 = 62.4 ∧ (12.2 : ℚ) < 74.6 := by norm_num

/-- Table 6.2, Panel I, men: each column of percentages sums to 100 within rounding. -/
theorem table62_columns :
    (93.5 : ℚ) + 0.2 + 3.2 + 2.2 + 0.9 = 100 ∧ |(12.2 : ℚ) + 75.6 + 5.3 + 3.9 + 2.9 - 100| ≤ 0.1 ∧
      (14.4 : ℚ) + 0.6 + 82.7 + 1.4 + 0.9 = 100 ∧ |(9.6 : ℚ) + 0.2 + 2.0 + 86.8 + 1.3 - 100| ≤ 0.1 := by
  norm_num [abs_le]

/-- Footnote 18 and the note to Table 2.2: the BMI in pounds and inches, `703 w/h²`,
matches kilograms per metre squared, since `0.45359237/0.0254²` lies in `(703, 703.1)`. -/
theorem bmi_constant : (703 : ℚ) < 0.45359237 / 0.0254 ^ 2 ∧ 0.45359237 / 0.0254 ^ 2 < (703.1 : ℚ) := by
  norm_num

end HitschEtAl2006
