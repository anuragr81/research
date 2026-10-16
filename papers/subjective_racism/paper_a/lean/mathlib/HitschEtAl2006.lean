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

end HitschEtAl2006
