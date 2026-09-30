/-
# Asch (1946), "Forming Impressions of Personality"

*Journal of Abnormal and Social Psychology* 41, 258-290.

Asch's paper is empirical and states no theorem.  What this file checks is the
**data the project quotes** from it and the **structure of Experiment VI** that
the citation audit relies on (`notes/citation_audit.md` items P13-P15;
`notes/citation_audit/verify_hogarth_asch.md` §2).  Every number below was
transcribed from the rendered scan, not the text layer (which scrambles the
table columns): Table 1 (Check List I) from journal p.262, the Experiment VI
series from p.270, Table 7 from p.271 (PDF p.14) and Table 8 from p.272
(PDF p.15).  The transcription agrees cell for cell with
`literature/asch1946/sympy/check_table7.py` and with the audit's table.

## What is formalized

  * **Experiment VI design** (p.270): Series A and Series B as lists of six
    stimulus terms, B the reverse of A (`seriesB_eq_reverse`), same members
    (`seriesB_perm`), six distinct terms (`seriesA_length`, `seriesA_nodup`).
  * **Check List I** (Table 1, p.262): 18 printed pairs (`checkListI_length`);
    the 18 Table 7 rows are the 18 pairs, in order, each row naming one member
    of its pair (`table7_rows_are_checklist_pairs`).  **No stimulus term is a
    check-list word** (`stimulus_disjoint_checklist`): 6 cues, 18 response
    items, no overlap (audit P14 / A27).
  * **Table 7** (p.271), all 72 cells: `ie`, `ei` (Experiment VI,
    intelligent→envious N = 34, envious→intelligent N = 24), `iev`, `evi`
    (Experiment VII, N = 46 and N = 53).
  * **The numbers the project quotes** (`quoted_all`, and one theorem per
    trait): restrained 64/9, good-looking 74/35, serious 97/100,
    persistent 82/87, reliable 84/91, humorous 52/21, good-natured 18/0,
    important 85/90 (first number I→E, second E→I).
  * **The order effect** `d = I→E − E→I`: 14 positive, 4 negative, none zero
    (`d_pos_count`, `d_neg_count`, `d_ne_zero`); the negative ones are exactly
    reliable, important, persistent, serious (`d_neg_exactly`); `Σ d = 265`
    (mean 14.7), `Σ |d| = 305` (mean 16.9); range −7 … +55 with restrained the
    largest (`d_le_restrained`, `d_ge_reliable`).
  * **"Swing enormously" vs "barely move"**: the four large swings have
    `|d| ≥ 18`, the four "barely move" traits `|d| ≤ 7`
    (`large_swings`, `small_moves`), and `|d| ≤ 7` is less than two E→I
    subjects (`small_moves_lt_two_subjects`: `24 |d| < 200`).
  * **Experiment VII** (right half of Table 7): 12 positive, 5 negative, 1 zero,
    `Σ d = 13` (mean 0.7); serious 44 vs 100 (`exp7_*`).
  * **Table 8** (p.272), the ranking of "envious": counts sum to 34 and 24,
    printed percentages to 100 (`table8_*_sum`); the modal rank is 6th under
    I→E (13 of 34) and 1st under E→I (7 of 24), both strict maxima
    (`envious_modal_rank_ie`, `envious_modal_rank_ei`), held by fewer than half
    of the subjects (`envious_modal_share`).  The printed I→E percentages 11
    (ranks 2, 5) and 39 (rank 6) are not the rounded counts 12 and 38; correct
    rounding totals 101 (`table8_ie_rounding`), while the E→I column is exactly
    the rounded counts (`table8_ei_rounding`).
  * **Nominal denominators**: 12 of the 18 E→I cells and 5 of the 18 I→E cells
    cannot be written `round(100k/N)` for the nominal `N`
    (`ei_unattainable_count`, `ie_unattainable_count`).
  * **Within-subject transition group** (p.271): 14 of 24 reported a change, so
    10 reported none (`within_no_change`).

## What is not formalized

The written sketches; the Fisher tests on reconstructed counts (in
`check_table7.py` only: they need a reconstruction of per-cell counts that
Asch's varying denominators do not license exactly); Tables 2-6 and 9-13;
Asch's Propositions I/Ia/Ib/II, which are verbal.

Rounding convention: `rnd p N k = ⌊100k/N + 1/2⌋`, computed in `ℕ` as
`(200k + N) / (2N)` (round half up, as `check_table7.py` does).
-/
import Mathlib

namespace Literature.Asch

/-! ## Experiment VI design (p.270) and Check List I (Table 1, p.262) -/

/-- Series A of Experiment VI (p.270), heard by Group A (N = 34). -/
def seriesA : List String :=
  ["intelligent", "industrious", "impulsive", "critical", "stubborn", "envious"]

/-- Series B of Experiment VI (p.270), heard by Group B (N = 24). -/
def seriesB : List String :=
  ["envious", "stubborn", "critical", "impulsive", "industrious", "intelligent"]

/-- p.270: "The two series are identical with regard to their members, differing
only in the order of succession": B is A reversed. -/
theorem seriesB_eq_reverse : seriesB = seriesA.reverse := by decide

theorem seriesB_perm : seriesB.Perm seriesA := by
  rw [seriesB_eq_reverse]; exact List.reverse_perm _

/-- Experiment VI has **six** stimulus terms, all distinct. -/
theorem seriesA_length : seriesA.length = 6 := by decide

theorem seriesA_nodup : seriesA.Nodup := by decide

/-- Check List I (Table 1, p.262): the 18 pairs "mostly opposites", each as
printed (left member, right member). -/
def checkListI : List (String × String) :=
  [("generous", "ungenerous"), ("shrewd", "wise"), ("unhappy", "happy"),
   ("irritable", "good-natured"), ("humorous", "humorless"),
   ("sociable", "unsociable"), ("popular", "unpopular"),
   ("unreliable", "reliable"), ("important", "insignificant"),
   ("ruthless", "humane"), ("good-looking", "unattractive"),
   ("persistent", "unstable"), ("frivolous", "serious"),
   ("restrained", "talkative"), ("self-centered", "altruistic"),
   ("imaginative", "hard-headed"), ("strong", "weak"), ("dishonest", "honest")]

/-- Experiment VI has **eighteen** response items. -/
theorem checkListI_length : checkListI.length = 18 := by decide

/-- **No stimulus term is a response item** (audit P14 / A27): none of the six
words of Series A (hence of Series B) occurs in either member of any of the 18
check-list pairs.  So Experiment VI has no cue-to-trait correspondence
("one cue per trait" is wrong). -/
theorem stimulus_disjoint_checklist :
    ∀ w ∈ seriesA, ∀ p ∈ checkListI, w ≠ p.1 ∧ w ≠ p.2 := by decide

theorem stimulus_disjoint_checklist_B :
    ∀ w ∈ seriesB, ∀ p ∈ checkListI, w ≠ p.1 ∧ w ≠ p.2 := by decide

/-! ## Table 7 (p.271) -/

/-- The 18 rows of Table 7, in printed order.  Each row is the listed member of
the corresponding Check List I pair. -/
inductive Trait
  | generous | wise | happy | goodNatured | humorous | sociable | popular
  | reliable | important | humane | goodLooking | persistent | serious
  | restrained | altruistic | imaginative | strong | honest
  deriving DecidableEq, Repr

open Trait

/-- The rows of Table 7 in printed order. -/
def allTraits : List Trait :=
  [generous, wise, happy, goodNatured, humorous, sociable, popular, reliable,
   important, humane, goodLooking, persistent, serious, restrained, altruistic,
   imaginative, strong, honest]

theorem mem_allTraits (t : Trait) : t ∈ allTraits := by cases t <;> decide

theorem allTraits_nodup : allTraits.Nodup := by decide

theorem allTraits_length : allTraits.length = 18 := by decide

/-- The word printed in Table 7 for each row. -/
def Trait.name : Trait → String
  | generous => "generous" | wise => "wise" | happy => "happy"
  | goodNatured => "good-natured" | humorous => "humorous"
  | sociable => "sociable" | popular => "popular" | reliable => "reliable"
  | important => "important" | humane => "humane"
  | goodLooking => "good-looking" | persistent => "persistent"
  | serious => "serious" | restrained => "restrained"
  | altruistic => "altruistic" | imaginative => "imaginative"
  | strong => "strong" | honest => "honest"

/-- Row `i` of Table 7 names a member of pair `i` of Check List I: the table's
18 rows are the checklist's 18 pairs, in the same order. -/
theorem table7_rows_are_checklist_pairs :
    ∀ x ∈ (allTraits.map Trait.name).zip checkListI, x.1 = x.2.1 ∨ x.1 = x.2.2 := by
  decide

/-- Table 7, Experiment VI, intelligent→envious (N = 34). -/
def ie : Trait → ℕ
  | generous => 24 | wise => 18 | happy => 32 | goodNatured => 18
  | humorous => 52 | sociable => 56 | popular => 35 | reliable => 84
  | important => 85 | humane => 36 | goodLooking => 74 | persistent => 82
  | serious => 97 | restrained => 64 | altruistic => 6 | imaginative => 26
  | strong => 94 | honest => 80

/-- Table 7, Experiment VI, envious→intelligent (N = 24). -/
def ei : Trait → ℕ
  | generous => 10 | wise => 17 | happy => 5 | goodNatured => 0
  | humorous => 21 | sociable => 27 | popular => 14 | reliable => 91
  | important => 90 | humane => 21 | goodLooking => 35 | persistent => 87
  | serious => 100 | restrained => 9 | altruistic => 5 | imaginative => 14
  | strong => 73 | honest => 79

/-- Table 7, Experiment VII, intelligent→evasive (N = 46). -/
def iev : Trait → ℕ
  | generous => 42 | wise => 35 | happy => 51 | goodNatured => 54
  | humorous => 53 | sociable => 50 | popular => 44 | reliable => 96
  | important => 77 | humane => 49 | goodLooking => 59 | persistent => 94
  | serious => 44 | restrained => 91 | altruistic => 32 | imaginative => 37
  | strong => 74 | honest => 66

/-- Table 7, Experiment VII, evasive→intelligent (N = 53). -/
def evi : Trait → ℕ
  | generous => 23 | wise => 19 | happy => 49 | goodNatured => 37
  | humorous => 29 | sociable => 48 | popular => 39 | reliable => 94
  | important => 89 | humane => 46 | goodLooking => 53 | persistent => 100
  | serious => 100 | restrained => 91 | altruistic => 25 | imaginative => 16
  | strong => 96 | honest => 81

/-- Nominal group sizes printed in Table 7's column heads. -/
def nIE : ℕ := 34
def nEI : ℕ := 24
def nIEv : ℕ := 46
def nEvI : ℕ := 53

/-! ### The numbers the project quotes (PLAN 3.B; review log Entry 12) -/

/-- The project's quoted (I→E, E→I) pairs. -/
def quoted : List (Trait × ℕ × ℕ) :=
  [(restrained, 64, 9), (goodLooking, 74, 35), (serious, 97, 100),
   (persistent, 82, 87), (reliable, 84, 91), (humorous, 52, 21),
   (goodNatured, 18, 0), (important, 85, 90)]

/-- Every Table 7 number the project quotes is correct and in the right column. -/
theorem quoted_all : ∀ q ∈ quoted, ie q.1 = q.2.1 ∧ ei q.1 = q.2.2 := by decide

theorem restrained_64_9 : ie restrained = 64 ∧ ei restrained = 9 := ⟨rfl, rfl⟩
theorem goodLooking_74_35 : ie goodLooking = 74 ∧ ei goodLooking = 35 := ⟨rfl, rfl⟩
theorem serious_97_100 : ie serious = 97 ∧ ei serious = 100 := ⟨rfl, rfl⟩
theorem persistent_82_87 : ie persistent = 82 ∧ ei persistent = 87 := ⟨rfl, rfl⟩
theorem reliable_84_91 : ie reliable = 84 ∧ ei reliable = 91 := ⟨rfl, rfl⟩
theorem humorous_52_21 : ie humorous = 52 ∧ ei humorous = 21 := ⟨rfl, rfl⟩
theorem goodNatured_18_0 : ie goodNatured = 18 ∧ ei goodNatured = 0 := ⟨rfl, rfl⟩
theorem important_85_90 : ie important = 85 ∧ ei important = 90 := ⟨rfl, rfl⟩

/-! ### The Experiment VI order effect -/

/-- The order effect on a trait, in percentage points: `I→E − E→I`. -/
def d (t : Trait) : ℤ := (ie t : ℤ) - ei t

/-- 14 traits are more often chosen when "intelligent" comes first. -/
theorem d_pos_count : (allTraits.filter (fun t => decide (0 < d t))).length = 14 := by
  decide

/-- 4 traits are more often chosen when "envious" comes first. -/
theorem d_neg_count : (allTraits.filter (fun t => decide (d t < 0))).length = 4 := by
  decide

/-- No trait is unmoved. -/
theorem d_ne_zero (t : Trait) : d t ≠ 0 := by cases t <;> decide

/-- **The four negative differences are exactly the four traits the project
says "barely move"**: reliable (−7), important (−5), persistent (−5) and
serious (−3). -/
theorem d_neg_exactly :
    allTraits.filter (fun t => decide (d t < 0)) = [reliable, important, persistent, serious] := by
  decide

theorem d_neg_values :
    d reliable = -7 ∧ d important = -5 ∧ d persistent = -5 ∧ d serious = -3 := by decide

/-- `Σ d = 265` over the 18 traits (mean `265/18 ≈ 14.7`). -/
theorem d_sum : (allTraits.map d).sum = 265 := by decide

/-- `Σ |d| = 305` (mean `305/18 ≈ 16.9`). -/
theorem d_abs_sum : (allTraits.map (fun t => |d t|)).sum = 305 := by decide

/-- The largest swing is restrained, `+55`. -/
theorem d_le_restrained (t : Trait) : d t ≤ d restrained := by cases t <;> decide

theorem d_restrained : d restrained = 55 := by decide

/-- The most negative difference is reliable, `−7`. -/
theorem d_ge_reliable (t : Trait) : d reliable ≤ d t := by cases t <;> decide

/-- The four swings the project calls large (restrained, good-looking, humorous,
good-natured) are all at least 18 points. -/
theorem large_swings :
    ∀ t ∈ [restrained, goodLooking, humorous, goodNatured], 18 ≤ d t := by decide

/-- The four traits the project says "barely move" all have `|d| ≤ 7`. -/
theorem small_moves :
    ∀ t ∈ [serious, persistent, reliable, important], |d t| ≤ 7 := by decide

/-- ... which is less than two subjects' worth in the E→I column
(one subject = `100/24 ≈ 4.2` points): `|d| · 24 < 2 · 100`. -/
theorem small_moves_lt_two_subjects :
    ∀ t ∈ [serious, persistent, reliable, important], |d t| * 24 < 2 * 100 := by decide

/-! ### Experiment VII (Table 7, right half) -/

/-- The order effect in Experiment VII: `I→Evasive − Evasive→I`. -/
def d7 (t : Trait) : ℤ := (iev t : ℤ) - evi t

theorem exp7_pos_count : (allTraits.filter (fun t => decide (0 < d7 t))).length = 12 := by
  decide

theorem exp7_neg_count : (allTraits.filter (fun t => decide (d7 t < 0))).length = 5 := by
  decide

theorem exp7_zero : allTraits.filter (fun t => decide (d7 t = 0)) = [restrained] := by decide

/-- `Σ d7 = 13` (mean `13/18 ≈ 0.7`, against 14.7 in Experiment VI). -/
theorem exp7_sum : (allTraits.map d7).sum = 13 := by decide

/-- The one strong reversal: serious, 44 under I→Evasive vs 100 under Evasive→I. -/
theorem exp7_serious : iev serious = 44 ∧ evi serious = 100 ∧ d7 serious = -56 := by decide

/-! ## Table 8 (p.272): the ranking of "envious" in Experiment VI -/

/-- Table 8, I→E: number of subjects ranking "envious" 1st, …, 6th. -/
def t8ieN : List ℕ := [5, 4, 5, 3, 4, 13]
/-- Table 8, I→E: printed percentages. -/
def t8iePct : List ℕ := [15, 11, 15, 9, 11, 39]
/-- Table 8, E→I: counts. -/
def t8eiN : List ℕ := [7, 4, 5, 2, 2, 4]
/-- Table 8, E→I: printed percentages. -/
def t8eiPct : List ℕ := [29, 17, 21, 8, 8, 17]

theorem table8_ie_sum : t8ieN.sum = nIE ∧ t8iePct.sum = 100 := by decide
theorem table8_ei_sum : t8eiN.sum = nEI ∧ t8eiPct.sum = 100 := by decide

/-- **Modal rank under I→E is 6th**: 13 subjects, strictly more than at any
other rank. -/
theorem envious_modal_rank_ie :
    t8ieN[5] = 13 ∧ ∀ i < 5, t8ieN[i]! < 13 := by decide

/-- **Modal rank under E→I is 1st**: 7 subjects, strictly more than at any
other rank. -/
theorem envious_modal_rank_ei :
    t8eiN[0] = 7 ∧ ∀ i < 6, 0 < i → t8eiN[i]! < 7 := by decide

/-- The modal ranks are held by a minority: `13/34 ≈ 38%` and `7/24 ≈ 29%`
(so "6th versus 1st" are modal ranks, audit A23/P15). -/
theorem envious_modal_share : 2 * 13 < nIE ∧ 2 * 7 < nEI := by decide

/-- Round half up of `100 k / N`, in `ℕ`. -/
def rnd (N k : ℕ) : ℕ := (200 * k + N) / (2 * N)

/-- **The printed I→E percentages are not all the rounded counts.** Rounding
`k/34` gives `[15, 12, 15, 9, 12, 38]`, which totals 101; Asch printed
`[15, 11, 15, 9, 11, 39]`, which totals 100.  (`13/34 = 38.2%` is printed 39.) -/
theorem table8_ie_rounding :
    t8ieN.map (rnd nIE) = [15, 12, 15, 9, 12, 38] ∧
    (t8ieN.map (rnd nIE)).sum = 101 ∧ t8ieN.map (rnd nIE) ≠ t8iePct := by decide

/-- The E→I column is exactly the rounded counts. -/
theorem table8_ei_rounding : t8eiN.map (rnd nEI) = t8eiPct := by decide

/-! ## Denominators: which Table 7 cells are attainable from the nominal N -/

/-- `p` is a rounded percentage `round(100k/N)` for some `k ≤ N`. -/
def attainable (N p : ℕ) : Bool := (List.range (N + 1)).any (fun k => rnd N k == p)

/-- **12 of the 18 E→I cells** cannot be `round(100k/24)` for any `k`: per-item
non-response (or a different base) must be common. -/
theorem ei_unattainable_count :
    (allTraits.filter (fun t => !attainable nEI (ei t))).length = 12 := by decide

/-- 5 of the 18 I→E cells cannot be `round(100k/34)`. -/
theorem ie_unattainable_count :
    (allTraits.filter (fun t => !attainable nIE (ie t))).length = 5 := by decide

/-- Examples named in the README: 91, 90, 87, 35 and 9 are not attainable
with `N = 24`. -/
theorem ei_unattainable_examples :
    ∀ p ∈ [91, 90, 87, 35, 9], attainable nEI p = false := by decide

/-! ## The within-subject transition group (p.271) -/

/-- p.271: of a new group of 24 who heard Series B then Series A, "14 out of 24
claimed that their impression suffered a change, while the remaining 10
subjects reported no change". -/
theorem within_no_change : 24 - 14 = 10 ∧ 2 * 10 < 24 := by decide

end Literature.Asch
