import Mathlib

/-! Edelman, Luca and Svirsky, "Racial Discrimination in the Sharing Economy:
Evidence from a Field Experiment", *American Economic Journal: Applied
Economics* 9(2), 2017, the accepted version marked "forthcoming", in the
paper's notation. Pages are PDF pages.

The source is a field experiment and states no model. What is formalised is
the arithmetic and the identities it uses: the linear probability model with a
guest-race interaction, whose "implied coefficient" rows are `b + i`; the
acceptance rates and response shares that follow from the counts in Appendix
Table 4 (PDF p. 36); the percent reading of a percentage-point gap (footnote
11); and the cost of discrimination `ΔNet Revenue = (1 − π_replace)(p − f)`
with the two numbers of PDF p. 16. -/

namespace EdelmanEtAl2017

/-! ### The race gap in a linear probability model

Tables 3, 5 and 6 regress `1(Host Accepts)` on `Guest is African-American`, a
host or listing trait, and their interaction. With coefficient `b` on the guest
dummy and `i` on the interaction, the race gap among hosts with the trait is
`b + i`, the row "Implied Coefficient on Guest is African-American + Host Trait
* Guest is African-American". -/

/-- Acceptance probability in the linear probability model, with guest dummy
`aa` and trait dummy `t`, each 0 or 1. -/
def accept (c b h i aa t : ℝ) : ℝ := c + b * aa + h * t + i * t * aa

/-- The race gap among hosts with the trait is `b + i`. -/
theorem gap_with_trait (c b h i : ℝ) : accept c b h i 1 1 - accept c b h i 0 1 = b + i := by
  unfold accept; ring

/-- The race gap among hosts without the trait is `b`. -/
theorem gap_without_trait (c b h i : ℝ) : accept c b h i 1 0 - accept c b h i 0 0 = b := by
  unfold accept; ring

/-- The gap disappears among hosts with the trait exactly when the interaction
offsets the main effect, `i = −b`. Table 5, column 5, is the case of hosts
with at least one review from an African-American guest. -/
theorem gap_vanishes_iff (c b h i : ℝ) :
    accept c b h i 1 1 - accept c b h i 0 1 = 0 ↔ i = -b := by
  rw [gap_with_trait]; constructor <;> intro h <;> linarith

/-- Control: with no interaction the gap among hosts with the trait is the
main effect, so a nonzero main effect cannot vanish there. -/
theorem control_no_interaction {c b h : ℝ} (hb : b ≠ 0) :
    accept c b h 0 1 1 - accept c b h 0 0 1 ≠ 0 := by
  rw [gap_with_trait]; simpa using hb

/-! ### Appendix Table 4, PDF p. 36

Counts of host responses by guest race. The six "Yes" categories are Yes,
Yes but request for more information, Yes with lower price if booked now, Yes
if guest extends stay, Yes but in a different property, Yes at a higher price.
-/

/-- Total responses to White guests. -/
theorem white_total :
    (1152 + 375 + 11 + 10 + 18 + 4 + 339 + 154 + 429 + 12 + 663 : ℚ) = 3167 := by norm_num

/-- Total responses to African-American guests. -/
theorem aa_total :
    (940 + 308 + 10 + 15 + 8 + 0 + 323 + 175 + 423 + 15 + 873 : ℚ) = 3090 := by norm_num

/-- The six "Yes" categories pooled give the roughly 50% and 42% of PDF pp. 2
and 8, and the constant 0.49 of Table 2: White `1570/3167 ∈ (0.49, 0.50)`,
African-American `1281/3090 ∈ (0.41, 0.42)`. -/
theorem pooled_yes_rates :
    (49 / 100 : ℚ) < 1570 / 3167 ∧ (1570 / 3167 : ℚ) < 50 / 100 ∧
      (41 / 100 : ℚ) < 1281 / 3090 ∧ (1281 / 3090 : ℚ) < 42 / 100 := by
  norm_num

/-- The pooled gap is about 8 percentage points and about 16 percent of the
White rate. -/
theorem pooled_gap :
    (8 / 100 : ℚ) < 1570 / 3167 - 1281 / 3090 ∧ (1570 / 3167 - 1281 / 3090 : ℚ) < 9 / 100 ∧
      (16 / 100 : ℚ) < (1570 / 3167 - 1281 / 3090) / (1570 / 3167) ∧
      ((1570 / 3167 - 1281 / 3090) / (1570 / 3167) : ℚ) < 17 / 100 := by
  norm_num

/-- Control: the simplest response alone, "Yes", gives rates near 36% and 30%,
not 50% and 42%. So the paper's "Yes" in Table 2 pools the "Yes" categories,
although PDF p. 8 says it restricts attention to the simplest "Yes" (ELS-D1). -/
theorem control_simple_yes_not_fifty :
    (1152 / 3167 : ℚ) < 37 / 100 ∧ (940 / 3090 : ℚ) < 31 / 100 := by
  norm_num

/-- PDF p. 13. Rejected or no response (with "Not sure or check back later" and
"No unless more information is provided"), 44%; a request for more
information, 11%; accepted, 46%. -/
theorem response_shares :
    let n : ℚ := 3167 + 3090
    (435 / 1000 : ℚ) < (663 + 873 + 429 + 423 + 12 + 15 + 154 + 175) / n ∧
      (663 + 873 + 429 + 423 + 12 + 15 + 154 + 175) / n < (445 / 1000 : ℚ) ∧
      (105 / 1000 : ℚ) < (339 + 323) / n ∧ (339 + 323) / n < (115 / 1000 : ℚ) ∧
      (455 / 1000 : ℚ) < (1570 + 1281) / n ∧ (1570 + 1281) / n < (465 / 1000 : ℚ) := by
  norm_num

/-- Footnote 11, PDF p. 10. The same 8-point gap is 16% of a 50% base and
less than 14% of a 60% base, so a gap equal in points is not equal in
percent. -/
theorem same_points_not_same_percent :
    ((50 - 42) / 50 : ℚ) = 16 / 100 ∧ ((60 - 52) / 60 : ℚ) < 14 / 100 ∧
      ((50 - 42) / 50 : ℚ) ≠ (60 - 52) / 60 := by
  norm_num

/-! ### The cost of discrimination, PDF pp. 15–16 -/

/-- `ΔNet Revenue = (p − f) − π_replace (p − f)`. -/
def costOfDiscrimination (p f πreplace : ℝ) : ℝ := (p - f) - πreplace * (p - f)

/-- The identity of PDF p. 15, `(p − f) − π (p − f) = (1 − π)(p − f)`. -/
theorem cost_identity (p f πreplace : ℝ) :
    costOfDiscrimination p f πreplace = (1 - πreplace) * (p - f) := by
  unfold costOfDiscrimination; ring

/-- PDF p. 16. With the median price $163, a fee of 3% and `π = .594`, the
cost is `$64.19` to the cent; with `π = .379` it is `$98.19`. -/
theorem cost_numbers :
    |costOfDiscrimination 163 (3 / 100 * 163) (594 / 1000) - 6419 / 100| < 1 / 200 ∧
      |costOfDiscrimination 163 (3 / 100 * 163) (379 / 1000) - 9819 / 100| < 1 / 200 := by
  unfold costOfDiscrimination
  constructor <;> rw [abs_lt] <;> constructor <;> norm_num

/-- The cost is positive whenever the host may fail to fill the listing and
the price exceeds the fee. -/
theorem cost_pos {p f πreplace : ℝ} (hπ : πreplace < 1) (hp : f < p) :
    0 < costOfDiscrimination p f πreplace := by
  rw [cost_identity]; exact mul_pos (by linarith) (by linarith)

/-- Control: with a sure replacement guest, discrimination costs nothing in
net revenue (PDF p. 15, "If a host can easily find a replacement guest, then
discrimination is nearly costless for the host"). -/
theorem control_sure_replacement_costless (p f : ℝ) : costOfDiscrimination p f 1 = 0 := by
  unfold costOfDiscrimination; ring

end EdelmanEtAl2017
