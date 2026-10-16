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

/-! ### Implied coefficients printed in the tables

Each "Implied Coefficient" row should be `b + i` from the printed `b` and `i`,
up to rounding of the unrounded estimates. -/

/-- Appendix Table 3 (PDF p. 35). The five city rows add exactly:
Baltimore `−0.07 − 0.12 = −0.19`, Dallas `−0.08 − 0.01 = −0.09`, Los Angeles
`−0.10 + 0.03 = −0.07`, St. Louis `−0.08 + 0.02 = −0.06`, Washington
`−0.08 − 0.01 = −0.09`. -/
theorem appendix3_implied :
    (-7 / 100 - 12 / 100 : ℚ) = -19 / 100 ∧ (-8 / 100 - 1 / 100 : ℚ) = -9 / 100 ∧
      (-10 / 100 + 3 / 100 : ℚ) = -7 / 100 ∧ (-8 / 100 + 2 / 100 : ℚ) = -6 / 100 ∧
      (-8 / 100 - 1 / 100 : ℚ) = -9 / 100 := by
  norm_num

/-- Table 3 (PDF p. 28) and Table 5, columns 1 to 3 and 5 (PDF p. 30). Each
implied coefficient is within 0.01 of the sum of the printed coefficients. -/
theorem implied_within_rounding :
    |(-8 / 100 + 1 / 100 : ℚ) - (-7 / 100)| ≤ 1 / 100 ∧
      |(-9 / 100 - 11 / 100 : ℚ) - (-19 / 100)| ≤ 1 / 100 ∧
      |(-9 / 100 + 11 / 100 : ℚ) - 2 / 100| ≤ 1 / 100 ∧
      |(-7 / 100 - 6 / 100 : ℚ) - (-12 / 100)| ≤ 1 / 100 ∧
      |(-7 / 100 - 2 / 100 : ℚ) - (-9 / 100)| ≤ 1 / 100 ∧
      |(-8 / 100 - 1 / 100 : ℚ) - (-9 / 100)| ≤ 1 / 100 ∧
      |(-9 / 100 + 1 / 100 : ℚ) - (-8 / 100)| ≤ 1 / 100 ∧
      |(-9 / 100 + 6 / 100 : ℚ) - (-4 / 100)| ≤ 1 / 100 := by
  norm_num [abs_le]

/-- Table 5, column 4 (PDF p. 30, read from the image). The printed guest
coefficient −0.11 and interaction −0.01 sum to −0.12, but the implied
coefficient is printed as −0.08. The gap of 0.04 is not rounding (ELS-D8). -/
theorem table5_col4_inconsistent : (3 / 100 : ℚ) < |(-11 / 100 - 1 / 100 : ℚ) - (-8 / 100)| := by
  norm_num [abs_of_neg]

/-! ### Table 7 and Appendix Table 1 -/

/-- Table 7 (PDF p. 32). The twenty name counts sum to 6,269, not the 6,390
printed for the entire sample (ELS-D9). -/
theorem table7_count :
    (306 + 344 + 325 + 327 + 303 + 317 + 332 + 279 + 312 + 314
      + 324 + 331 + 327 + 339 + 309 + 285 + 328 + 300 + 313 + 254 : ℚ) = 6269 := by
  norm_num

/-- Table 7. Weighting each name's rate by its count gives 0.432 overall,
0.474 for White names and 0.389 for African-American names. The overall rate
matches the printed 0.43, and the gap of 8.5 points matches the main effect,
but the base is below the 0.496 of the pooled "Yes" (ELS-D9). -/
theorem table7_rates :
    let wN : ℚ := 306 + 344 + 325 + 327 + 303 + 317 + 332 + 279 + 312 + 314
    let aN : ℚ := 324 + 331 + 327 + 339 + 309 + 285 + 328 + 300 + 313 + 254
    let wY : ℚ := 49/100*306 + 56/100*344 + 48/100*325 + 50/100*327 + 49/100*303
      + 41/100*317 + 48/100*332 + 44/100*279 + 45/100*312 + 43/100*314
    let aY : ℚ := 42/100*324 + 35/100*331 + 43/100*327 + 47/100*339 + 40/100*309
      + 38/100*285 + 33/100*328 + 36/100*300 + 38/100*313 + 36/100*254
    (43 / 100 : ℚ) < (wY + aY) / (wN + aN) ∧ (wY + aY) / (wN + aN) < 435 / 1000 ∧
      (47 / 100 : ℚ) < wY / wN ∧ wY / wN < 48 / 100 ∧
      (38 / 100 : ℚ) < aY / aN ∧ aY / aN < 39 / 100 := by
  norm_num

/-- Footnote 7 (PDF p. 7) against Appendix Table 1 (PDF p. 33). Every White
female name scores at least 0.90, every African-American female name at most
0.10, every White male name at least 0.88, and every African-American male
name at most 0.10 except Jermaine Jones at 0.26. -/
theorem footnote7_survey :
    (∀ x ∈ ({93/100, 95/100, 97/100, 98/100, 1} : Finset ℚ), 90/100 ≤ x) ∧
      (∀ x ∈ ({3/100, 5/100, 7/100} : Finset ℚ), x ≤ 10/100) ∧
      (∀ x ∈ ({88/100, 90/100, 91/100, 93/100, 98/100} : Finset ℚ), 88/100 ≤ x) ∧
      (∀ x ∈ ({0, 6/100, 7/100, 10/100} : Finset ℚ), x ≤ 10/100) ∧ (10/100 : ℚ) < 26/100 := by
  refine ⟨?_, ?_, ?_, ?_, by norm_num⟩ <;>
    simp only [Finset.mem_insert, Finset.mem_singleton, forall_eq_or_imp, forall_eq] <;> norm_num

/-! ### The replacement probabilities and footnote 17 (PDF pp. 15–16) -/

/-- 25.9 percent vacant, 37.9 percent filled and 36.1 percent delisted sum to
99.9 percent, and excluding the delisted, the share that found a replacement is
`37.9/(25.9 + 37.9) = 0.594`. -/
theorem replacement_shares :
    (259 / 10 + 379 / 10 + 361 / 10 : ℚ) = 999 / 10 ∧
      (5935 / 10000 : ℚ) < (379 / 10) / (259 / 10 + 379 / 10) ∧
      ((379 / 10) / (259 / 10 + 379 / 10) : ℚ) < 5945 / 10000 := by
  norm_num

/-- Footnote 17. Counting hosts who said Yes, the median price $150 and the
replacement rates 64 and 45 percent give costs of $52.38 and $80.03, drops of
$11.81 and $18.17 from $64.19 and $98.19, "about $10 or $20". -/
theorem footnote17_drops :
    costOfDiscrimination 150 (3 / 100 * 150) (64 / 100) = 5238 / 100 ∧
      costOfDiscrimination 150 (3 / 100 * 150) (45 / 100) = 80025 / 1000 ∧
      (6419 / 100 : ℝ) - 5238 / 100 = 1181 / 100 ∧ (9819 / 100 : ℝ) - 80025 / 1000 = 18165 / 1000 := by
  unfold costOfDiscrimination
  norm_num

/-! ### The remedies of PDF pp. 17–20

The authors propose concealing guest names or expanding Instant Book, so that
hosts "accept guests without screening them first" and the design "would
eliminate the opportunity for discrimination" (PDF p. 18). Our reading: the
host's acceptance is a function of what the platform displays about an account,
and the experiment's two accounts differ only in the name. -/

/-- The race gap between two accounts under an acceptance rule acting on the
display. -/
def displayGap {Acct Info : Type*} (display : Acct → Info) (accept : Info → ℝ) (aa w : Acct) : ℝ :=
  accept (display aa) - accept (display w)

/-- Our reading of PDF p. 18. The gap is zero under every acceptance rule if
and only if the platform shows the two accounts identically. Hiding the name
removes the gap only when nothing else displayed differs between them. -/
theorem gap_zero_all_rules_iff {Acct Info : Type*} [DecidableEq Info] (display : Acct → Info)
    (aa w : Acct) :
    (∀ accept : Info → ℝ, displayGap display accept aa w = 0) ↔ display aa = display w := by
  constructor
  · intro h
    by_contra hne
    have hne' : display w ≠ display aa := fun e => hne e.symm
    have := h (fun i => if i = display aa then 1 else 0)
    simp [displayGap, hne'] at this
  · intro h accept
    simp [displayGap, h]

/-- Control: if a displayed variable differs, say a name or a photo, some rule
gives a nonzero gap. -/
theorem control_displayed_marker {Acct Info : Type*} [DecidableEq Info] (display : Acct → Info)
    (aa w : Acct) (hne : display aa ≠ display w) :
    ∃ accept : Info → ℝ, displayGap display accept aa w ≠ 0 := by
  by_contra h
  push_neg at h
  exact hne ((gap_zero_all_rules_iff display aa w).mp h)

end EdelmanEtAl2017
