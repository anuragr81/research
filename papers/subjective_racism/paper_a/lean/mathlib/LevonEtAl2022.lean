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

/-! ### Further printed numbers -/

/-- Socio-economic grades (PDF p. 15). AB 22%, C1 31%, C2 21% and DE 26% make
100%, so upper SEG (ABC1) is 53% and lower SEG (C2DE) 47% of the 2011
population. -/
theorem seg_shares : (22 + 31 + 21 + 26 : ℕ) = 100 ∧ (22 + 31 : ℕ) = 53 ∧ (21 + 26 : ℕ) = 47 := by
  decide

/-- "Almost a third of respondents overall reported some such experience in an
institutional context" (PDF p. 17). The sample-weighted mean of 29%, 30% and
25% is in `[27.5, 28.5)`, below a third. -/
theorem institutional_overall :
    let x : ℝ := (511 * 29 + 1029 * 30 + 1014 * 25) / (511 + 1029 + 1014)
    55 / 2 ≤ x ∧ x < 57 / 2 ∧ x < 100 / 3 := by
  norm_num

/-- Mockery in social settings exceeds mockery in institutions at every life
stage, 40 > 29, 47 > 30 and 46 > 25 (Figures 5 and 6). -/
theorem social_exceeds_institutional :
    agree 4 25 < agree 7 33 ∧ agree 8 22 < agree 12 35 ∧ agree 5 20 < agree 10 36 := by
  decide

/-- "Rates were marginally lower" in high-stakes contexts (PDF p. 17). For
school students (26% against 29%) and professionals (8% against 25%) they are
lower. For university students they are higher, 31% against 30%. -/
theorem high_stakes_against_institutional :
    (26 : ℕ) < 29 ∧ (8 : ℕ) < 25 ∧ (30 : ℕ) < 31 := by decide

/-- Senior managers by parental grade (PDF p. 34). The class gap in social
settings, 55 − 34, exceeds the gap in the workplace, 29 − 22. -/
theorem senior_gap_social_exceeds_workplace : (29 - 22 : ℕ) < 55 - 34 := by decide

/-! ### Modification and the accents heard in authority (PDF p. 7)

"Encouraging employees to change their accents will maintain an
over-representation of RP in contexts of authority." We read the share of RP
heard among `nR` native RP speakers and `nO` others, of whom a share `m`
modify to RP, as `(nR + m nO)/(nR + nO)`. -/

/-- The share of RP heard. -/
noncomputable def heardRP (nR nO m : ℝ) : ℝ := (nR + m * nO) / (nR + nO)

/-- More modification raises the share of RP heard above the native share. -/
theorem modification_raises_heard_RP {nR nO m m' : ℝ} (hR : 0 ≤ nR) (hO : 0 < nO)
    (h : m < m') : heardRP nR nO m < heardRP nR nO m' := by
  unfold heardRP
  have : 0 < nR + nO := by linarith
  rw [div_lt_div_iff_of_pos_right this]
  nlinarith

/-- Control: with no modification the share heard is the native share. -/
theorem control_no_modification_native_share (nR nO : ℝ) :
    heardRP nR nO 0 = nR / (nR + nO) := by
  unfold heardRP; ring_nf

/-! ### Accent strength (PDF pp. 13, 14)

A strong MLE accent was the lowest-rated recording and a milder one was rated
well. The sensitivity was weaker for Northern accents, "suggesting a penalty
for not modifying certain accents". We read a rating as falling linearly in
accent strength `s` with slope `β` for the accent. -/

/-- A rating that falls in accent strength. -/
def strengthRating (r0 β s : ℝ) : ℝ := r0 - β * s

/-- The return to lowering strength by `Δ` is `β Δ`. -/
theorem modification_return (r0 β s Δ : ℝ) :
    strengthRating r0 β (s - Δ) - strengthRating r0 β s = β * Δ := by
  unfold strengthRating; ring

/-- A steeper slope, as for MLE against Northern accents, gives a larger
return to the same modification. -/
theorem steeper_slope_larger_return {r0 βN βM s Δ : ℝ} (hβ : βN < βM) (hΔ : 0 < Δ) :
    strengthRating r0 βN (s - Δ) - strengthRating r0 βN s
      < strengthRating r0 βM (s - Δ) - strengthRating r0 βM s := by
  rw [modification_return, modification_return]
  nlinarith

/-- Control: with no sensitivity to strength, modification returns nothing. -/
theorem control_flat_strength (r0 s Δ : ℝ) :
    strengthRating r0 0 (s - Δ) - strengthRating r0 0 s = 0 := by
  rw [modification_return]; ring

end LevonEtAl2022
