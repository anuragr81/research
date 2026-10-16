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

end Grogger2018
