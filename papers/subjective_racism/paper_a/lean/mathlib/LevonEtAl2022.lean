import Mathlib

/-! Levon, Sharma and Ilbury, *Speaking Up. Accents and Social Mobility*, The
Sutton Trust, November 2022, in the report's terms.

The report is a survey report and states no model. Formalised here are the
definitions and identities its numbers rest on, and **our reading** of one
inference it draws.

* "Percent agree" is the share who strongly or slightly agree on a 5-point
  scale (PDF p. 15). Figures 3 to 6 (PDF pp. 16–19) give each life stage's five
  shares, and the text gives the agree shares.
* The overall share of 24% with accent-based career anxiety (PDF p. 16) is
  read as the mean of the three life stages weighted by their sample sizes,
  `N = 511, 1029, 1014` (PDF p. 15).
* The authors read the lower reported bias among older senior managers as
  "older professionals having suppressed their non-RP accents to a greater
  extent" (PDF p. 33). We read a reported rate as `(1 − m) * b`, where `m` is
  the share who have modified their accent and `b` the rate at which those who
  have not are mocked. -/

namespace LevonEtAl2022

/-- "Percent agree", strongly agree plus slightly agree. -/
def agree (sa sl : ℕ) : ℕ := sa + sl

/-- Figures 3 to 6, the agree shares of university applicants, university
students and employees, against the shares stated in the text (PDF pp. 5,
15–17). -/
theorem agree_shares :
    (agree 3 21, agree 11 24, agree 4 19) = (24, 35, 23) ∧
    (agree 3 15, agree 8 25, agree 4 15) = (18, 33, 19) ∧
    (agree 4 25, agree 8 22, agree 5 20) = (29, 30, 25) ∧
    (agree 7 33, agree 12 35, agree 10 36) = (40, 47, 46) := by
  decide

/-- Each bar of Figures 3 to 6 sums to 100 or, by rounding, 101. -/
theorem bars_sum :
    [3 + 21 + 9 + 19 + 48, 11 + 24 + 12 + 16 + 38, 4 + 19 + 12 + 18 + 48,
     3 + 15 + 12 + 21 + 49, 8 + 25 + 12 + 17 + 38, 4 + 15 + 12 + 18 + 51,
     4 + 25 + 10 + 20 + 41, 8 + 22 + 15 + 18 + 38, 5 + 20 + 11 + 16 + 49,
     7 + 33 + 10 + 18 + 32, 12 + 35 + 12 + 14 + 27, 10 + 36 + 10 + 14 + 30]
      = [100, 101, 101, 100, 100, 100, 100, 101, 101, 100, 100, 100] := by
  decide

/-- The overall 24% (PDF p. 16) is the sample-weighted mean of 18%, 33% and
19%, which lies in `[23.5, 24.5)`. -/
theorem overall_is_weighted :
    let x : ℝ := (511 * 18 + 1029 * 33 + 1014 * 19) / (511 + 1029 + 1014)
    47 / 2 ≤ x ∧ x < 49 / 2 := by
  norm_num

/-- A reported rate among a cohort, `(1 − m) * b`. -/
def reported (m b : ℝ) : ℝ := (1 - m) * b

/-- With a fixed rate of mockery, a cohort in which more have modified their
accent reports less mockery, the pattern the authors read into Figure 17. -/
theorem modification_lowers_reports {m m' b : ℝ} (hb : 0 < b) (h : m < m') :
    reported m' b < reported m b := by
  unfold reported
  nlinarith

/-- Figure 17 (PDF p. 33). Reported mockery in the workplace is 19% at ages
45–54 and 20% at 55+, so the workplace series does not fall at every step. -/
theorem fig17_workplace_not_antitone : ¬ ((20 : ℕ) ≤ 19) := by decide

/-- Control: the inference to modification is not identified by the figure.
Social-setting reports of 52% at ages 18–34 and 23% at 55+ (Figure 17) arise
both from no modification and a falling rate of mockery, and from a fixed rate
of mockery and a rising share who have modified. -/
theorem control_not_identified :
    reported 0 (52 / 100) = 52 / 100 ∧ reported 0 (23 / 100) = 23 / 100 ∧
    reported 0 (52 / 100) = 52 / 100 ∧ reported (29 / 52) (52 / 100) = 23 / 100 := by
  unfold reported
  norm_num

/-- Control: drop the weighting by sample size. The unweighted mean of 18%,
33% and 19% is below 23.5%, so it does not round to the stated 24%. -/
theorem control_unweighted_not_24 : ((18 + 33 + 19) / 3 : ℝ) < 47 / 2 := by
  norm_num

end LevonEtAl2022
