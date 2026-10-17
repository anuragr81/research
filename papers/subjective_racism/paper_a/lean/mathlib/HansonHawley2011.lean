import Mathlib

/-! Hanson and Hawley (2011), "Do Landlords Discriminate in the Rental Housing
Market? Evidence from an Internet Field Experiment in U.S. Cities", Georgia State
University, Experimental Economics Center Working Paper 2011-05, published in the
*Journal of Urban Economics* 70(2), pp. 99–114.

What is formalised.
* The arithmetic of Tables 5 and 6 and of the fixed-effects marginal effect of
  p. 18 (PDF p. 20): differences, landlord-level shares and the audit counts.
* Two readings recorded as discrepancies (HH-D2, HH-D3): one row of Table 6 whose
  counts do not sum to its audits, and one whose "Respond to Neither" count does
  not give its printed share.
* The identity behind the landlord-level measure: on the same paired audits the
  net share of discriminating landlords equals the difference in response rates.
  The control shows that the two differ once the rates are pooled over other
  audits, as in the columns (1) to (3) of Table 6.
* Our reading of the class manipulation, in a linear predictor of a tenant's
  quality from the name's group and the e-mail: with a partial signal the group
  gap shrinks but does not vanish, and it vanishes only when the signal shows
  class fully.
* Our reading of the response, a threshold on the predicted quality: when a high
  signal puts both groups above the threshold the response is equal although
  the predictions still differ. -/

namespace HansonHawley2011

/-! ### Table 6, response rates by race and class (PDF p. 45) -/

/-- Response rates in percent, columns (1) and (2) of Table 6. -/
def rWH : ℚ := 5663 / 100
def rWL : ℚ := 5760 / 100
def rAH : ℚ := 5262 / 100
def rAL : ℚ := 4901 / 100

/-- Table 6, column (3). The race gap is 4.01 points between high-class e-mails
and 8.59 between low-class ones (printed −8.60 in the reversed order). The class
gap is −0.97 for white names and 3.61 for African American names (printed 3.62).
A high-class African American name gets 4.98 points fewer responses than a
low-class white name. -/
theorem table6_gaps :
    rWH - rAH = 401 / 100 ∧ rWL - rAL = 859 / 100 ∧
      rWH - rWL = -97 / 100 ∧ rAH - rAL = 361 / 100 ∧ rWL - rAH = 498 / 100 := by
  unfold rWH rWL rAH rAL; norm_num

/-- The class signal moves the response rate for African American names and not,
in the direction of class, for white names: the white high-class rate is below
the white low-class rate. -/
theorem table6_class_moves_one_group : rWH < rWL ∧ rAL < rAH := by
  unfold rWH rWL rAH rAL; norm_num

/-- The race gap between high-class e-mails is less than half the gap between
low-class e-mails, and it is not zero. -/
theorem table6_gap_halves_not_zero :
    0 < rWH - rAH ∧ 2 * (rWH - rAH) < rWL - rAL := by
  unfold rWH rWL rAH rAL; norm_num

/-- Table 6, e-mails sent. The four types add to the 4,729 white and 4,727
African American e-mails of Table 5, and to 9,456 in all. -/
theorem table6_emails :
    (2368 : ℕ) + 2361 = 4729 ∧ (2364 : ℕ) + 2363 = 4727 ∧ (4729 : ℕ) + 4727 = 9456 := by
  norm_num

/-- Table 6, landlord level. The audits in each of the six pairings, taken from
the rows whose counts add up, and the 787 implied by the shares of the other
two, add to the 4,728 audits of Table 3. The four cross-race pairings add to the
3,153 audits of Table 5. -/
theorem table6_audits :
    (790 : ℕ) + 788 + 788 + 787 + 787 + 788 = 4728 ∧ (790 : ℕ) + 788 + 787 + 788 = 3153 := by
  norm_num

/-- Table 6, row "White, High / African American, High". The counts 300, 326, 89
and 75 add to 790 audits, and each printed share is its count over 790 to two
decimals. The net share is 14/790, printed 1.77 percent. -/
theorem table6_high_high :
    (300 : ℕ) + 326 + 89 + 75 = 790 ∧
      |(300 : ℚ) / 790 * 100 - 3797 / 100| < 1 / 200 ∧
      |(326 : ℚ) / 790 * 100 - 4127 / 100| < 1 / 200 ∧
      |(89 : ℚ) / 790 * 100 - 1127 / 100| < 1 / 200 ∧
      |(75 : ℚ) / 790 * 100 - 949 / 100| < 1 / 200 ∧
      |((89 : ℚ) - 75) / 790 * 100 - 177 / 100| < 1 / 200 := by
  refine ⟨by norm_num, ?_, ?_, ?_, ?_, ?_⟩ <;>
    rw [abs_lt] <;> constructor <;> norm_num

/-- Table 6, row "African American, Low / White, Low". The counts add to 788
audits and the net share is −47/788, printed −5.96 percent. -/
theorem table6_low_low :
    (278 : ℕ) + 339 + 62 + 109 = 788 ∧
      |((62 : ℚ) - 109) / 788 * 100 - (-596 / 100)| < 1 / 200 := by
  refine ⟨by norm_num, ?_⟩
  rw [abs_lt]; constructor <;> norm_num

/-- Table 6, row "White, High / African American, Low". The counts add to 788
audits and the net share is 69/788, printed 8.76 percent. -/
theorem table6_high_low :
    (287 : ℕ) + 322 + 124 + 55 = 788 ∧
      |((124 : ℚ) - 55) / 788 * 100 - 876 / 100| < 1 / 200 := by
  refine ⟨by norm_num, ?_⟩
  rw [abs_lt]; constructor <;> norm_num

/-- HH-D2. The row "African American, High / African American, Low" prints the
counts 320, 300, 87 and 53, which add to 760, and the shares 40.66, 38.12, 11.05
and 6.73 percent, which add to 96.56. The shares are the counts over 787, not
760, so 27 audits are missing from the four columns. -/
theorem table6_aa_row_short :
    (320 : ℕ) + 300 + 87 + 53 = 760 ∧
      (4066 : ℚ) / 100 + 3812 / 100 + 1105 / 100 + 673 / 100 = 9656 / 100 ∧
      |(87 : ℚ) / 787 * 100 - 1105 / 100| < 1 / 200 ∧
      |(87 : ℚ) / 760 * 100 - 1105 / 100| > 1 / 200 := by
  refine ⟨by norm_num, by norm_num, ?_, ?_⟩
  · rw [abs_lt]; constructor <;> norm_num
  · rw [abs_of_pos (by norm_num)]; norm_num

/-- HH-D3. The row "White, Low / African American, High" prints 266 landlords
responding to neither and a share of 36.34 percent. The other three shares are
their counts over 787, and 266/787 is 33.80 percent, while 286/787 is 36.34. -/
theorem table6_wl_ah_neither :
    |(266 : ℚ) / 787 * 100 - 3634 / 100| > 1 / 200 ∧
      |(286 : ℚ) / 787 * 100 - 3634 / 100| < 1 / 200 ∧
      (286 : ℕ) + 370 + 72 + 59 = 787 := by
  refine ⟨?_, ?_, by norm_num⟩
  · rw [abs_of_neg (by norm_num)]; norm_num
  · rw [abs_lt]; constructor <;> norm_num

/-! ### Table 5 and the fixed-effects estimate -/

/-- Table 5, all audits. The counts add to the 3,153 cross-race audits, and the
net share of discriminating landlords is 143/3153, printed 4.54 percent. -/
theorem table5_all :
    (1151 : ℕ) + 1357 + 394 + 251 = 3153 ∧
      |((394 : ℚ) - 251) / 3153 * 100 - 454 / 100| < 1 / 200 := by
  refine ⟨by norm_num, ?_⟩
  rw [abs_lt]; constructor <;> norm_num

/-- P. 18 (PDF p. 20). Among the 645 landlords who answer one e-mail only, 394
answer the white name and 251 the African American name, 61 and 39 percent, and
the difference is 0.222, the fixed-effects marginal effect of Table 7,
column (3). -/
theorem fe_effect_discordant :
    (394 : ℕ) + 251 = 645 ∧ |((394 : ℚ) - 251) / 645 - 222 / 1000| < 1 / 2000 := by
  refine ⟨by norm_num, ?_⟩
  rw [abs_lt]; constructor <;> norm_num

/-! ### The landlord-level measure -/

/-- A paired audit's outcome counts: landlords answering neither, both, only the
first e-mail, or only the second. -/
structure Pairs where
  neither : ℚ
  both : ℚ
  firstOnly : ℚ
  secondOnly : ℚ

/-- The number of audits. -/
def Pairs.total (p : Pairs) : ℚ := p.neither + p.both + p.firstOnly + p.secondOnly

/-- The response rate of the first e-mail on these audits. -/
def Pairs.rateFirst (p : Pairs) : ℚ := (p.both + p.firstOnly) / p.total

/-- The response rate of the second e-mail on these audits. -/
def Pairs.rateSecond (p : Pairs) : ℚ := (p.both + p.secondOnly) / p.total

/-- The landlord-level net measure, column (6) less column (7). -/
def Pairs.net (p : Pairs) : ℚ := (p.firstOnly - p.secondOnly) / p.total

/-- On the same paired audits, the net share of discriminating landlords equals
the difference in response rates. The landlords who answer both or neither
cancel. -/
theorem net_eq_rate_diff (p : Pairs) : p.net = p.rateFirst - p.rateSecond := by
  unfold Pairs.net Pairs.rateFirst Pairs.rateSecond
  rw [div_sub_div_same]; congr 1; ring

/-- Control. Table 6's first row prints a rate difference of 4.01 points and a
net share of 1.77. The rates are over every e-mail of each type, 2,368 and 2,364,
pooled across pairings, and the net share is over the 790 audits of the row; the
identity of `net_eq_rate_diff` does not hold across the two samples. -/
theorem control_pooled_rates_differ :
    rWH - rAH ≠ ((89 : ℚ) - 75) / 790 * 100 := by
  unfold rWH rAH; norm_num

/-! ### Our reading of the class manipulation -/

/-- The predicted quality of a tenant from the group mean `μ` and the e-mail's
signal `s`, with weight `k` on the signal. -/
def predict (k μ s : ℝ) : ℝ := (1 - k) * μ + k * s

/-- With a common weight, the group gap in prediction at the same e-mail is
`(1 − k)(μW − μA)` whatever the e-mail says. -/
theorem gap_common_weight (k μW μA s : ℝ) :
    predict k μW s - predict k μA s = (1 - k) * (μW - μA) := by
  unfold predict; ring

/-- The gap vanishes when the signal shows class fully, `k = 1`. -/
theorem gap_vanishes_full_signal (μW μA s : ℝ) :
    predict 1 μW s - predict 1 μA s = 0 := by
  rw [gap_common_weight]; ring

/-- Control. With a partial signal, `k < 1`, and different group means, the gap
does not vanish. -/
theorem control_partial_signal_gap (k μW μA s : ℝ) (hk : k < 1) (hμ : μW ≠ μA) :
    predict k μW s - predict k μA s ≠ 0 := by
  rw [gap_common_weight]
  exact mul_ne_zero (by linarith) (sub_ne_zero.mpr hμ)

/-- The landlord answers when the predicted quality reaches the threshold `θ`. -/
def answers (θ x : ℝ) : Prop := θ ≤ x

/-- Our reading of Table 6's first row. With a partial signal the predictions
still differ, yet a signal high enough to put both groups over the threshold
gives equal treatment. -/
theorem threshold_equal_above (k μW μA s θ : ℝ) (hk : k < 1)
    (hW : θ ≤ predict k μW s) (hA : θ ≤ predict k μA s) (hμ : μW ≠ μA) :
    (answers θ (predict k μW s) ↔ answers θ (predict k μA s)) ∧
      predict k μW s ≠ predict k μA s := by
  refine ⟨⟨fun _ => hA, fun _ => hW⟩, ?_⟩
  exact sub_ne_zero.mp (control_partial_signal_gap k μW μA s hk hμ)

/-- Control. A low signal that puts the threshold between the two predictions
gives unequal treatment. -/
theorem control_threshold_between (θ xW xA : ℝ) (hA : xA < θ) (hW : θ ≤ xW) :
    answers θ xW ∧ ¬ answers θ xA := by
  unfold answers; exact ⟨hW, not_le.mpr hA⟩

/-- In the linear predictor with a common weight, raising the signal raises both
predictions by the same amount, so a high enough signal clears any threshold
for both groups when `k > 0`. -/
theorem high_signal_clears (k μW μA θ : ℝ) (hk : 0 < k) :
    ∃ s, θ ≤ predict k μW s ∧ θ ≤ predict k μA s := by
  refine ⟨(θ + |(1 - k) * μW| + |(1 - k) * μA|) / k, ?_, ?_⟩ <;>
  · unfold predict
    rw [mul_div_cancel₀ _ hk.ne']
    have h1 := neg_abs_le ((1 - k) * μW)
    have h2 := neg_abs_le ((1 - k) * μA)
    have h3 := abs_nonneg ((1 - k) * μW)
    have h4 := abs_nonneg ((1 - k) * μA)
    linarith

/-- Control. With `k = 0` the e-mail is ignored and no signal moves either
prediction. -/
theorem control_zero_weight (μ s s' : ℝ) : predict 0 μ s = predict 0 μ s' := by
  unfold predict; ring

/-! ### Neighbourhoods -/

/-- The "tipping" neighbourhood of note 26: between 80 and 95 percent white. -/
def tipping (w : ℝ) : Prop := 0.80 ≤ w ∧ w ≤ 0.95

/-- With white and minority shares summing to one, a tipping neighbourhood is
one with a minority share between five and 20 percent, the range of Card, Mas
and Rothstein (2008) as the paper reads it. -/
theorem tipping_iff_minority (w m : ℝ) (h : w + m = 1) :
    tipping w ↔ 0.05 ≤ m ∧ m ≤ 0.20 := by
  unfold tipping
  constructor
  · rintro ⟨h1, h2⟩; constructor <;> linarith
  · rintro ⟨h1, h2⟩; constructor <;> linarith

/-- Table 8, column (8). The net share of discriminating landlords is largest in
the band 0.80 to 0.95 white. -/
theorem table8_band_largest :
    (581 : ℚ) / 100 < 702 / 100 ∧ (443 : ℚ) / 100 < 702 / 100 ∧ (258 : ℚ) / 100 < 702 / 100 := by
  norm_num

/-- HH-D4. Table 8's e-mail counts add to 9,456, every e-mail of the experiment,
although p. 14 (PDF p. 16) matches only 3,060 units, 6,120 e-mails, to a census
tract. -/
theorem table8_emails_all :
    (630 : ℕ) + 632 + 1072 + 1068 + 1154 + 1202 + 1873 + 1825 = 9456 ∧
      (3060 : ℕ) * 2 = 6120 := by
  norm_num

/-- HH-D5. Table 8's landlord counts are not the numerators of its shares. In
the row "Less than .50" the four counts add to 825, and 167 of 825 is not the
printed 13.95 percent. -/
theorem table8_counts_not_shares :
    (430 : ℕ) + 168 + 167 + 60 = 825 ∧ |(167 : ℚ) / 825 * 100 - 1395 / 100| > 1 := by
  refine ⟨by norm_num, ?_⟩
  rw [abs_of_pos (by norm_num)]; norm_num

/-! ### Table 9, unit types -/

/-- Table 9. Single-family homes: 631 audits and a net share of 3/631, printed
0.48 percent. Apartments: 1,117 audits and 68/1117, printed 6.09 percent.
Condominiums: 303 audits and 36/303, printed 11.88 percent. -/
theorem table9_nets :
    (300 : ℕ) + 206 + 64 + 61 = 631 ∧ |((64 : ℚ) - 61) / 631 * 100 - 48 / 100| < 1 / 200 ∧
      (453 : ℕ) + 430 + 151 + 83 = 1117 ∧ |((151 : ℚ) - 83) / 1117 * 100 - 609 / 100| < 1 / 200 ∧
      (153 : ℕ) + 84 + 51 + 15 = 303 ∧ |((51 : ℚ) - 15) / 303 * 100 - 1188 / 100| < 1 / 200 := by
  refine ⟨by norm_num, ?_, by norm_num, ?_, by norm_num, ?_⟩ <;>
    rw [abs_lt] <;> constructor <;> norm_num

end HansonHawley2011
