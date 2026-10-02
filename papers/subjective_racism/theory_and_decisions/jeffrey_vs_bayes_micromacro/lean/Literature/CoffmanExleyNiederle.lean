/-
# Coffman, Exley & Niederle (2021), "The Role of Beliefs in Driving Gender Discrimination"

*Management Science* 67(6), 3551-3569.  DOI 10.1287/mnsc.2020.3660.

Source read in full: the Harvard Business School working paper of 28 February
2020 (35 pp.), the copy the author linked; `p.N` is its printed page.  The
paper is experimental.  Its formal content is its belief measures and its
coding of decisions; the rest is reported numbers, checked here for the
claims the paper makes about them and for internal consistency.

## What is formalized

  * **Belief measures** (p.8; fn 18, p.17): the believed gap is a difference in
    average scores between two groups, "any feasible average difference (from
    -10 to 10 problems solved correctly)" (`gapRange`); differential improvement
    is the believed hard-quiz gap minus the believed easy-quiz gap
    (`diffImprovement`).
  * **Coding of decisions** (fn 17, p.16): 1 if the female-even-month worker is
    hired with certainty, 1/2 if chance decides, 0 otherwise (`code`); with no
    discrimination either way the expected code is 1/2 whatever the share of
    chance choices (`symmetric_expected_half`, the 50% benchmark of p.16).
  * **Hiring rates** (p.16): 43% (Gender) and 37% (Birth Month), both below
    1/2 (`rates_below_half`); their difference agrees with Table 1, column 1
    (0.061) to the rounding of the two rates (`table1_col1_consistent`).
  * **Information shown and beliefs held** (Fig. 1, p.9; fn 13, p.14): the
    easy-quiz gaps shown, 3.96 - 3.27 and 5.50 - 4.50, agree with the actual
    easy gaps of fn 13 (0.692 and 1.0) to rounding (`fig1_fn13_consistent`);
    posterior believed gaps "get the direction of the difference right on
    average, but exaggerate it" in all four cases (`fn13_exaggerate`).
  * **In-group** (pp.22-23): in-group employers believe in a smaller gap in both
    treatments (`ingroup_smaller_gap`) and hire the in-group worker more often,
    41% against 32% (`ingroup_hiring`).  Information stage (p.25): 33% against
    29% (`infostage_rates`).
  * **Design counts**: 54 = 6 screens x 9 decisions (p.10), the five risk
    levels of screens 2-6 (p.11), 12 information screens (p.8), pools of 25 and
    25 (Fig. 2, p.12), 400 employers per treatment (fn 7, p.5), the simplified
    study's 2000 - 5 = 1995 (p.27).
  * **Bearing on Paper B, not a claim of the paper**: with a 0/1 score a group's
    average score is the conditional probability of a high score given the
    group, so a believed gap between groups is the conditional difference
    `P(B=1|A=1) - P(B=1|A=0)` of the belief table (`gap_is_conditional_difference`),
    the statistic Paper B's Proposition PRO places in the protected class.

## What is not formalized

The regressions of Tables 1-3, the Kolmogorov-Smirnov and t-tests, and the
appendix tables, which the working paper does not include.
-/
import Mathlib

namespace Literature.CoffmanExleyNiederle

/-! ## Belief measures -/

/-- The believed gap is "any feasible average difference (from -10 to 10
problems solved correctly)" on a 10-question quiz (p.8). -/
def gapRange (g : ℤ) : Prop := -10 ≤ g ∧ g ≤ 10

/-- **Differential improvement** (fn 18): the believed hard-quiz gap minus the
believed easy-quiz gap, a difference-in-differences. -/
def diffImprovement (hardGap easyGap : ℚ) : ℚ := hardGap - easyGap

theorem diffImprovement_zero_iff (h e : ℚ) : diffImprovement h e = 0 ↔ h = e := by
  unfold diffImprovement; constructor <;> intro H <;> linarith

/-! ## Coding of decisions -/

/-- The three options of a hiring decision (p.10). -/
inductive Choice | female | male | chance

/-- Fn 17: "1 if she is hired with certainty, 0.5 if chance determines who is
hired, and 0 if a male-odd-month worker is instead hired with certainty". -/
def code : Choice → ℚ
  | .female => 1
  | .male => 0
  | .chance => 1 / 2

/-- With no discrimination either way, the two workers are picked with the same
probability `p` and chance with the rest, and the expected code is 1/2 whatever
`p` is: the 50% benchmark of p.16. -/
theorem symmetric_expected_half (p : ℚ) :
    p * code .female + p * code .male + (1 - 2 * p) * code .chance = 1 / 2 := by
  simp only [code]; ring

/-! ## Reported numbers -/

/-- P.16: female workers hired in 43% of decisions (Gender), even-month workers
in 37% (Birth Month), both "significantly below the 50% benchmark". -/
theorem rates_below_half : (43 / 100 : ℚ) < 1 / 2 ∧ (37 / 100 : ℚ) < 1 / 2 := by norm_num

/-- Table 1, column 1, Gender Treatment coefficient 0.061: the difference of the
two rounded rates is within the rounding of the rates. -/
theorem table1_col1_consistent : |(43 / 100 : ℚ) - 37 / 100 - 61 / 1000| ≤ 1 / 100 := by
  rw [abs_le]; constructor <;> norm_num

/-- Fig. 1 (p.9) shows easy-quiz averages 3.27 against 3.96 (math) and 4.50
against 5.50 (sports); fn 13 (p.14) gives the actual easy gaps 0.692 and 1.0. -/
theorem fig1_fn13_consistent :
    |(396 / 100 : ℚ) - 327 / 100 - 692 / 1000| ≤ 1 / 100 ∧ (550 / 100 : ℚ) - 450 / 100 = 1 := by
  refine ⟨?_, by norm_num⟩
  rw [abs_le]; constructor <;> norm_num

/-- Fn 13 (p.14): posterior believed gaps against actual gaps, sports (hard
2.76 vs 1.1, easy 2.62 vs 1.0) and math (hard 1.54 vs 0.451, easy 1.07 vs
0.692).  Each actual gap is positive (male-odd-month ahead) and each believed
gap exceeds it: right direction, exaggerated. -/
theorem fn13_exaggerate :
    (0 : ℚ) < 11 / 10 ∧ (11 / 10 : ℚ) < 276 / 100 ∧
    (0 : ℚ) < 1 ∧ (1 : ℚ) < 262 / 100 ∧
    (0 : ℚ) < 451 / 1000 ∧ (451 / 1000 : ℚ) < 154 / 100 ∧
    (0 : ℚ) < 692 / 1000 ∧ (692 / 1000 : ℚ) < 107 / 100 := by norm_num

/-- P.23: male employers believe the male advantage on the easy quiz is 2.39,
female employers 1.16; odd-month employers believe the odd-month advantage is
2.48, even-month employers 1.91.  In-group employers believe in a smaller gap
in both treatments. -/
theorem ingroup_smaller_gap : (116 / 100 : ℚ) < 239 / 100 ∧ (191 / 100 : ℚ) < 248 / 100 := by
  norm_num

/-- P.22: odd-month employers hire even-month workers 32% of the time,
even-month employers 41%. -/
theorem ingroup_hiring : (32 / 100 : ℚ) < 41 / 100 := by norm_num

/-- P.25, Information Stage: female workers hired 33% of the time (Gender),
even-month workers 29% (Birth Month). -/
theorem infostage_rates : (29 / 100 : ℚ) < 33 / 100 ∧ (33 / 100 : ℚ) < 1 / 2 := by norm_num

/-! ## Design counts -/

/-- P.10: 54 decisions on six screens of nine; each screen has three pairs with
equal scores and six in which the female-even-month worker scores higher
(pp.10-11). -/
theorem decisions_54 : 6 * 9 = 54 ∧ 3 + 6 = 9 := by decide

/-- P.11: screens 2-6 lower the probability of payment for hiring the
female-even-month worker through 99, 95, 90, 75 and 50 percent. -/
def riskLevels : List ℕ := [99, 95, 90, 75, 50]

theorem riskLevels_screens : riskLevels.length = 5 ∧ 1 + riskLevels.length = 6 ∧
    riskLevels.Pairwise (· > ·) := by decide

/-- P.8: twelve information screens, eleven subsets by birth date and the full
distributions last. -/
theorem info_screens : 11 + 1 = 12 := by decide

/-- Fig. 2 (p.12), fn 7 (p.5), p.27: two pools of 25 workers; 400 employers in
each of the two treatments; 2000 recruited for the simplified study, 5 dropped. -/
theorem sample_counts : 25 + 25 = 50 ∧ 2 * 400 = 800 ∧ 2000 - 5 = 1995 := by decide

/-! ## Bearing on Paper B (Paper B's reading, not a claim of the paper) -/

/-- A belief over group membership `a` and a high score `b`, cells `Q a b`. -/
abbrev Table := Bool → Bool → ℝ

/-- A group's average score when the score is 0 or 1. -/
noncomputable def meanScore (Q : Table) (a : Bool) : ℝ :=
  (Q a true * 1 + Q a false * 0) / (Q a true + Q a false)

/-- The conditional probability of a high score given the group. -/
noncomputable def condHigh (Q : Table) (a : Bool) : ℝ :=
  Q a true / (Q a true + Q a false)

theorem meanScore_eq_condHigh (Q : Table) (a : Bool) : meanScore Q a = condHigh Q a := by
  simp [meanScore, condHigh]

/-- With a 0/1 score, the believed gap between groups is the conditional
difference `P(B=1|A=1) - P(B=1|A=0)`. -/
theorem gap_is_conditional_difference (Q : Table) :
    meanScore Q true - meanScore Q false = condHigh Q true - condHigh Q false := by
  rw [meanScore_eq_condHigh, meanScore_eq_condHigh]

end Literature.CoffmanExleyNiederle
