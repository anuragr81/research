import Mathlib

/-! Grogger, "Speech and Wages", working paper Version 8 of 2 February 2018,
in the paper's notation. This is not the Grogger (2011) article in the Journal
of Human Resources, which the paper cites as its own earlier work.

The paper is empirical. What is formalised is the structure it states.

1. The speech measure of Section V.B. Each speaker has six listener reports,
   or three counted twice. A black speaker has mainstream speech when fewer
   than four (scaled) reports say the speaker is black.
2. The occupational sorting model of Section VI.A. The log wage in sector `t`
   is `w_t = α_t + λ_t S + ε_t` (eq. 1), with `λ₁ > λ₀ ≥ 0`, and the probability
   of choosing the task-intensive sector is `F[(α₁ − α₀) + (λ₁ − λ₀) S]`
   (eq. 2). Footnote 19 says that with workers in both sectors `α₁ < α₀`.
   In the Lean code `l` stands for the paper's `λ`, which Lean reserves.
3. The arithmetic of the coefficients the text reports from Tables 3 and 7. -/

namespace Grogger2018

/-! ### The speech measure -/

/-- The number of listener reports, scaled to six. A speaker heard on one
prompt has three reports, each counted twice (Section V.B, footnote 7). -/
def scaled (k : ℕ) (onePrompt : Bool) : ℕ := if onePrompt then 2 * k else k

/-- Mainstream speech for a black speaker, fewer than four scaled reports that
the speaker is black (Section V.B). -/
def mainstream (n : ℕ) : Prop := n < 4

instance (n : ℕ) : Decidable (mainstream n) := by unfold mainstream; infer_instance

/-- A speaker heard on one prompt is mainstream exactly when at most one of his
three listeners reported him black. -/
theorem three_report_mainstream_iff (k : ℕ) :
    mainstream (scaled k true) ↔ k ≤ 1 := by
  unfold mainstream scaled
  simp
  omega

/-- Control. Without the doubling, every speaker with three reports would
count as mainstream, including one whom all three listeners heard as black. -/
theorem control_doubling_needed : mainstream (scaled 3 false) ∧ ¬ mainstream (scaled 3 true) := by
  unfold mainstream scaled
  decide

/-! ### The occupational sorting model -/

/-- Eq. (1). The log wage in a sector with intercept `α` and skill price `l`. -/
def w (α l S ε : ℝ) : ℝ := α + l * S + ε

/-- The index of eq. (2), the deterministic part of `w₁ − w₀`. -/
def idx (α₁ α₀ l₁ l₀ S : ℝ) : ℝ := (α₁ - α₀) + (l₁ - l₀) * S

/-- `w₁ − w₀` is the index plus `ε₁ − ε₀`. -/
theorem w_diff (α₁ α₀ l₁ l₀ S ε₁ ε₀ : ℝ) :
    w α₁ l₁ S ε₁ - w α₀ l₀ S ε₀ = idx α₁ α₀ l₁ l₀ S + (ε₁ - ε₀) := by
  unfold w idx; ring

/-- Eq. (2) and the sorting prediction. With `l₁ > l₀` and an increasing cdf
`F`, the probability of choosing the task-intensive sector rises in `S`. -/
theorem sorting {F : ℝ → ℝ} (hF : StrictMono F) {α₁ α₀ l₁ l₀ : ℝ} (h : l₀ < l₁) :
    StrictMono (fun S => F (idx α₁ α₀ l₁ l₀ S)) := by
  intro S S' hS
  apply hF
  unfold idx
  have : (l₁ - l₀) * S < (l₁ - l₀) * S' := mul_lt_mul_of_pos_left hS (by linarith)
  linarith

/-- Control. With equal skill prices there is no sorting on `S`, which is the
null the probit of Table 6, column (1), tests. -/
theorem control_no_sorting {F : ℝ → ℝ} {α₁ α₀ l S S' : ℝ} :
    F (idx α₁ α₀ l l S) = F (idx α₁ α₀ l l S') := by
  unfold idx; ring_nf

/-- Footnote 19, without the disturbances. If `l₁ > l₀` and some worker with
`S ≥ 0` earns strictly more in sector 0, then `α₁ < α₀`. -/
theorem fn19 {α₁ α₀ l₁ l₀ S : ℝ} (h : l₀ < l₁) (hS : 0 ≤ S)
    (h0 : w α₁ l₁ S 0 < w α₀ l₀ S 0) : α₁ < α₀ := by
  unfold w at h0
  have : l₀ * S ≤ l₁ * S := mul_le_mul_of_nonneg_right h.le hS
  linarith

/-- Control for footnote 19. With `α₁ ≥ α₀` and `l₁ ≥ l₀`, every worker with
`S ≥ 0` earns at least as much in sector 1, so sector 0 holds no one who
strictly prefers it. -/
theorem control_fn19 {α₁ α₀ l₁ l₀ S : ℝ} (hα : α₀ ≤ α₁) (h : l₀ ≤ l₁) (hS : 0 ≤ S) :
    w α₀ l₀ S 0 ≤ w α₁ l₁ S 0 := by
  unfold w
  have : l₀ * S ≤ l₁ * S := mul_le_mul_of_nonneg_right h hS
  linarith

/-! ### The coefficients the text reports -/

/-- Table 3, column (2). Five more reports of black speech at `−0.037` each is
the "18.5 percent", and five more of Southern speech at `−0.046` the "roughly
23 percent", both log points. -/
theorem table3_col2 : (5 : ℝ) * (37 / 1000) = 185 / 1000 ∧ (5 : ℝ) * (46 / 1000) = 23 / 100 := by
  norm_num

/-- Table 3, column (5). The black coefficient plus the black-mainstream
interaction, `−0.152 + 0.136 = −0.016`, is the gap between black mainstream
speakers and comparably skilled whites. -/
theorem table3_col5 : (-152 / 1000 : ℝ) + 136 / 1000 = -16 / 1000 := by norm_num

/-- Table 4, column (2), with location fixed effects and family background.
`−0.069 + 0.119 = 0.050`. -/
theorem table4_col2 : (-69 / 1000 : ℝ) + 119 / 1000 = 50 / 1000 := by norm_num

/-- Table 7. Of 25 non-mainstream speakers in 2006, 2 (8%) were mainstream in
2011. Of 9 mainstream speakers in 2006, 4 (44%) were not. -/
theorem table7 : (2 : ℝ) / 25 = 8 / 100 ∧ (44 / 100 : ℝ) < 4 / 9 ∧ (4 : ℝ) / 9 < 45 / 100 := by
  norm_num

/-! ### Footnote 20, a special case

The footnote says that, under the "normal case", the OLS estimate of `λ₁` in
sector 1 is biased downward, because a low-`S` worker is found in sector 1
only with a high `ε₁`. A three-type example shows the sign. Take `α₀ = λ₀ = 0`,
`ε₀ = 0`, `α₁ = −1/2`, `λ₁ = 1`, `S ∈ {0, 1}` and `ε₁ ∈ {−1, 0, 1}`. The workers
in sector 1 are `(S, w₁) = (0, 1/2), (1, 1/2), (1, 3/2)`. -/

/-- The least-squares slope through three points. -/
noncomputable def ols3 (x₁ x₂ x₃ y₁ y₂ y₃ : ℝ) : ℝ :=
  let mx := (x₁ + x₂ + x₃) / 3
  let my := (y₁ + y₂ + y₃) / 3
  ((x₁ - mx) * (y₁ - my) + (x₂ - mx) * (y₂ - my) + (x₃ - mx) * (y₃ - my)) /
    ((x₁ - mx) ^ 2 + (x₂ - mx) ^ 2 + (x₃ - mx) ^ 2)

/-- Who is in sector 1 in the example. A worker chooses sector 1 when
`w₁ > w₀`, that is `−1/2 + S + ε₁ > 0`. -/
theorem fn20_sector1 :
    ((-1 / 2 : ℝ) + 0 + 1 > 0) ∧ ¬ ((-1 / 2 : ℝ) + 0 + 0 > 0) ∧ ¬ ((-1 / 2 : ℝ) + 0 + -1 > 0) ∧
      ((-1 / 2 : ℝ) + 1 + 1 > 0) ∧ ((-1 / 2 : ℝ) + 1 + 0 > 0) ∧ ¬ ((-1 / 2 : ℝ) + 1 + -1 > 0) := by
  norm_num

/-- Footnote 20 in the example. The sector-1 OLS slope is `1/2`, below
`λ₁ = 1`. -/
theorem fn20_downward : ols3 0 1 1 (1 / 2) (1 / 2) (3 / 2) = 1 / 2 := by
  unfold ols3; norm_num

/-- Control. Without selection, with the same `ε₁` at each `S`, the slope is
`λ₁`. Points `(0, −1/2), (1, 1/2)` and `(1, 1/2)`. -/
theorem control_fn20_no_selection : ols3 0 1 1 (-1 / 2) (1 / 2) (1 / 2) = 1 := by
  unfold ols3; norm_num

/-! ### Numbers in the text that follow from printed numbers -/

/-- Table 1 and PDF p. 11. Shares of listener reports, in tenths of a percent.
At least four of six heard black: 70.8% ("Seventy-one percent"). Mainstream
blacks, fewer than four: 29.3% ("29 percent", PDF p. 17). Southern whites heard
black by at least one: 39.8%. Non-Southern whites, 11.7%. Southern whites heard
Southern by at least one: 76.2%, by four or more: 32.2%. Mainstream Southern,
fewer than four Southern reports: 67.8% ("68 percent", PDF p. 18). -/
theorem table1_shares :
    (156 + 226 + 326 : ℕ) = 708 ∧ (53 + 55 + 58 + 127 : ℕ) = 293 ∧ (1000 - 602 : ℕ) = 398 ∧
      (1000 - 883 : ℕ) = 117 ∧ (1000 - 238 : ℕ) = 762 ∧ (126 + 100 + 96 : ℕ) = 322 ∧
      (238 + 153 + 149 + 138 : ℕ) = 678 := by
  norm_num

/-- Table 2 and PDF p. 12. Between the extremes of black speakers, "almost
three years" of education, `14.37 − 11.46 = 2.91`, and "nine-tenths of a
standard deviation" of AFQT, `0.07 − (−0.83) = 0.90`. -/
theorem table2_extremes :
    (1437 - 1146 : ℝ) / 100 = 291 / 100 ∧ (7 / 100 : ℝ) - (-83 / 100) = 9 / 10 := by norm_num

/-- Appendix Table 1 and the Data Appendix. Consent and loss rates. -/
theorem appendix_table1 :
    (1698 + 786 + 2034 + 1562 : ℕ) = 6080 ∧ (1402 + 651 + 1603 + 1251 : ℕ) = 4907 ∧
      (1168 + 602 + 1594 + 861 : ℕ) = 4225 ∧ (1698 + 786 : ℕ) = 2484 ∧
      (1402 + 651 : ℕ) = 2053 ∧ (2484 - 2053 : ℕ) = 431 ∧ (2034 - 1603 : ℕ) = 431 ∧
      (213 + 538 : ℕ) = 751 ∧ (1698 - 1691 : ℕ) = 7 ∧ (786 - 783 : ℕ) = 3 ∧
      (17 / 100 : ℝ) < 431 / 2484 ∧ (431 / 2484 : ℝ) < 18 / 100 ∧
      (21 / 100 : ℝ) < 431 / 2034 ∧ (431 / 2034 : ℝ) < 22 / 100 ∧
      (83 / 100 : ℝ) < 1168 / 1402 ∧ (92 / 100 : ℝ) < 602 / 651 ∧ (99 / 100 : ℝ) < 1594 / 1603 := by
  norm_num

/-- PDF p. 9 against Appendix Table 1 (GRG-D13). Among black respondents
interviewed in person, 1,698 of 1,833 consented, above 92%, not "83 percent".
The 83% is consent among all Round-15 black respondents, 1,698 of 2,036. -/
theorem consent_rates :
    (92 / 100 : ℝ) < 1698 / 1833 ∧ (83 / 100 : ℝ) < 1698 / 2036 ∧ (1698 / 2036 : ℝ) < 84 / 100 := by
  norm_num

/-- PDF p. 20 (GRG-D2). "About one-third" of the gap is explained if the black
coefficient falls from 0.152 to the text's 0.099, a share of `53/152`. With
Table 4's printed 0.050 the share is `102/152`, about two-thirds. -/
theorem one_third_share :
    (1 / 3 : ℝ) < 53 / 152 ∧ (53 / 152 : ℝ) < 36 / 100 ∧ (2 / 3 : ℝ) < 102 / 152 := by
  norm_num

/-- Table 4, column 3. College-educated blacks: `−0.067 + 0.191 = 0.124`, the
"small though insignificant wage premium for blacks". -/
theorem table4_col3_BA : (-67 / 1000 : ℝ) + 191 / 1000 = 124 / 1000 := by norm_num

/-- PDF p. 27 (GRG-D14). The t-statistic of the interactive sorting probit is
`0.598/0.147`, between 4.0 and 4.1, not the text's 4.4. -/
theorem table6_tstat : (4 : ℝ) < 598 / 147 ∧ (598 / 147 : ℝ) < 41 / 10 := by norm_num

/-- Table 7 row and column totals. -/
theorem table7_totals : (23 + 2 : ℕ) = 25 ∧ (4 + 5 : ℕ) = 9 ∧ (23 + 4 : ℕ) = 27 ∧ (2 + 5 : ℕ) = 7 := by
  norm_num

end Grogger2018
