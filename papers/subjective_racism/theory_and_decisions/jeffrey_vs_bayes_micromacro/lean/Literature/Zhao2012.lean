/-
# Zhao, Crupi, Tentori, Fitelson & Osherson (2012), "Updating: Learning versus supposing"

*Cognition* 124, 373-378.  DOI 10.1016/j.cognition.2012.05.001.

Formalization of the Bayesian identity the paper tests and a check of the
numbers and design facts the project reports from it
(`literature/measurement_susceptibility_survey.md` lines 123-153;
`notes/citation_audit.md` item L10; `notes/citation_audit/verify_record_papers.md`
§4).  The paper is empirical; its formal content is eq. (1) and footnote 1.

## What is formalized

  * **Eq. (1)**, UPDATING FOR LEARNED EVENTS (p.373): if `B` is learned
    between times 1 and 2 (and nothing else relevant is learned), then
    `Pr₂(A) = Pr₁(A | B)`.  `learn P` is `Pr₁` conditioned on `B`;
    `learn_isDist` and `learn_prB` ("Pr₂ as defined by (1) is a genuine
    probability distribution and … Pr₂(B) = 1"), `eq1`.
  * **Footnote 1** (p.373): "Violation of (1) can be conceived as failure to
    respect the invariance of conditional probability for the learned event B.
    This is because failure to update via (1) yields
    `Pr₂(A|B) = Pr₂(A|Ω) = Pr₂(A) ≠ Pr₁(A|B)`."  `cond_eq_marg_of_certain`
    (`Pr₂(B) = 1 ⇒ Pr₂(A|B) = Pr₂(A)`) and `fn1` (violation of (1) ⟺ failure
    of invariance of `Pr(A|B)`); `learn_invariance` (updating by (1) keeps
    `Pr(A|B)`).
  * **Design** (pp.374, 376): five judgments per participant (Fig. 1: "20
    participants, each providing 5 estimates" = 100 per condition), three
    disjoint groups of 20 in Experiment 3 (a "new group of sixty"), 20 swing
    states (fn 4).  `five_per_participant`, `exp3_groups_disjoint`,
    `swingStates_*`.
  * **Experiment 3** (Table 5 and text, p.377): learn 0.64, suppose 0.53,
    control 0.51; `exp3_order`, `exp3_gaps` (learn − suppose = 0.11,
    suppose − control = 0.02, learn − control = 0.13), `exp3_suppose_near_control`.
    The swing-state result is a comparison of **group means**: the learn and
    suppose means against the control group's mean `Pr(A)`, three different
    groups of participants (`exp3_between_group`).  Consistent vs inconsistent
    pairs 0.72/0.50 (learn) and 0.55/0.48 (suppose), interaction 0.15 > 0
    (`exp3_interaction`).
  * **Quadratic penalty** (p.377): issuing 0.5 "guarantees a penalty of 0.25"
    (`brier_half`); suppose 0.25, learn 0.18, control 0.29 (`exp3_penalties`).
  * **Decks** (Tables 1 and 3, pp.374, 376): every deck has 20 cards
    (`table1_decks`, `table3_decks`); the objective conditional probabilities of
    Experiment 2 are more extreme than those of Experiment 1, as the paper
    says (p.375): mean `|Pr(A|B) − 1/2|` over the 20 (deck, conditional) pairs
    is `≈ 0.132` vs `≈ 0.302` (`exp2_more_extreme`), and the "(b)" rows of
    Tables 2 and 4 flip accordingly (`extremeness_rows`).

## What is not formalized

The t-tests, the ANOVA and the binomial tests (the binomial p-values are
recomputed in `literature/zhao2012/sympy/check_zhao2012.py`).  "Supposing" has
no formal model in the paper: it is a response mode, compared with learning.
-/
import Mathlib

namespace Literature.Zhao2012

/-! ## Eq. (1) and footnote 1 -/

/-- A distribution on the four cells `P b a` (`b`: whether `B` holds, `a`:
whether `A` holds), on a finite outcome space as in the paper (p.373). -/
abbrev Dist := Bool → Bool → ℝ

def IsDist (P : Dist) : Prop :=
  (∀ b a, 0 ≤ P b a) ∧ P true true + P true false + P false true + P false false = 1

def prB (P : Dist) : ℝ := P true true + P true false
def prA (P : Dist) : ℝ := P true true + P false true
/-- `Pr(A | B)`. -/
noncomputable def condAB (P : Dist) : ℝ := P true true / prB P

/-- Updating for a learned event `B`: `Pr₂ = Pr₁(· | B)`. -/
noncomputable def learn (P : Dist) : Dist := fun b a => if b then P b a / prB P else 0

/-- Pr₂ defined by (1) "is a genuine probability distribution" (p.373). -/
theorem learn_isDist (P : Dist) (hP : IsDist P) (hB : 0 < prB P) : IsDist (learn P) := by
  refine ⟨fun b a => ?_, ?_⟩
  · cases b
    · simp [learn]
    · simp only [learn, if_true]; exact div_nonneg (hP.1 _ _) hB.le
  · have : P true true + P true false ≠ 0 := hB.ne'
    simp only [learn, if_true, Bool.false_eq_true, if_false, prB]
    field_simp
    ring

/-- "… and that Pr₂(B) = 1 (as expected)" (p.373). -/
theorem learn_prB (P : Dist) (hB : prB P ≠ 0) : prB (learn P) = 1 := by
  have : P true true + P true false ≠ 0 := hB
  simp only [prB, learn, if_true]
  field_simp

/-- **Eq. (1)**, UPDATING FOR LEARNED EVENTS: `Pr₂(A) = Pr₁(A | B)`. -/
theorem eq1 (P : Dist) : prA (learn P) = condAB P := by
  simp [prA, learn, condAB]

/-- Updating by (1) respects the invariance of `Pr(A | B)`. -/
theorem learn_invariance (P : Dist) (hB : prB P ≠ 0) : condAB (learn P) = condAB P := by
  rw [condAB, learn_prB P hB, div_one]
  simp [learn, condAB]

/-- Footnote 1, first step: once `B` is certain, `Pr₂(A | B) = Pr₂(A | Ω) = Pr₂(A)`. -/
theorem cond_eq_marg_of_certain (Q : Dist) (hQ : IsDist Q) (h : prB Q = 1) :
    condAB Q = prA Q := by
  obtain ⟨hnn, hsum⟩ := hQ
  unfold prB at h
  have h0 : Q false true + Q false false = 0 := by linarith
  have h1 : Q false true = 0 := by linarith [hnn false true, hnn false false]
  unfold condAB prA prB
  rw [h, div_one, h1, add_zero]

/-- **Footnote 1**: for any time-2 distribution `Q` that makes the learned `B`
certain, violating (1) is the same thing as failing to keep `Pr(A|B)`
invariant: `Q(A) ≠ Pr₁(A|B) ⟺ Q(A|B) ≠ Pr₁(A|B)`. -/
theorem fn1 (P Q : Dist) (hQ : IsDist Q) (h : prB Q = 1) :
    prA Q ≠ condAB P ↔ condAB Q ≠ condAB P := by
  rw [cond_eq_marg_of_certain Q hQ h]

/-! ## Design facts -/

/-- **Five judgments per participant** (p.374, five decks; p.376, "This
procedure was performed five times per participant"): Fig. 1 shows 100
estimates per condition from 20 participants. -/
theorem five_per_participant : 100 / 20 = 5 ∧ 20 * 5 = 100 ∧ (5 : ℕ) ≠ 1 := by decide

/-- Experiments 1 and 2: 40 participants each, 20 learn and 20 suppose. -/
theorem exp12_groups : 20 + 20 = 40 := by decide

/-- **Experiment 3 has three disjoint groups of 20**: a "new group of sixty
undergraduates" (p.376) split into learn, suppose (each 20) and control
(N = 20).  If the control participants were learn or suppose participants the
total would be 40, not 60. -/
theorem exp3_groups_disjoint : 20 + 20 + 20 = 60 ∧ 20 + 20 ≠ 60 ∧ 60 * 5 = 300 := by decide

/-- The 20 swing states of footnote 4 (p.376). -/
def swingStates : List String :=
  ["AL", "AZ", "GA", "ID", "IN", "KS", "KY", "LA", "MI", "MN",
   "MO", "MS", "MT", "NC", "ND", "NM", "OH", "TN", "VA", "WV"]

theorem swingStates_length : swingStates.length = 20 := by decide
theorem swingStates_nodup : swingStates.Nodup := by decide

/-! ## Experiment 3 (Table 5 and text, p.377) -/

/-- Table 5 (a), learn: mean raw estimate of `Pr(A|B)` (`= Pr(A)` after learning `B`). -/
def learnMean : ℚ := 0.64
/-- Table 5 (a), suppose: mean raw estimate of `Pr(A|B)` supposing `B`. -/
def supposeMean : ℚ := 0.53
/-- Text p.377: control group's mean raw estimate of `Pr(A)` (no `B` evoked). -/
def controlMean : ℚ := 0.51

theorem exp3_order : controlMean < supposeMean ∧ supposeMean < learnMean := by
  norm_num [controlMean, supposeMean, learnMean]

theorem exp3_gaps :
    learnMean - supposeMean = 0.11 ∧ supposeMean - controlMean = 0.02 ∧
    learnMean - controlMean = 0.13 := by
  norm_num [controlMean, supposeMean, learnMean]

/-- "Suppose is indistinguishable from control": the suppose-control gap is a
sixth of the learn-control gap. -/
theorem exp3_suppose_near_control :
    6 * (supposeMean - controlMean) < learnMean - controlMean := by
  norm_num [controlMean, supposeMean, learnMean]

/-- A comparison of group means across three disjoint groups of participants
(the swing-state result, p.377): the learn mean exceeds the control mean
by 0.13, the suppose mean by 0.02.  Stated as the arithmetic it is: a
between-group difference of means, not a within-subject change. -/
theorem exp3_between_group :
    learnMean - controlMean = 0.13 ∧ supposeMean - controlMean = 0.02 ∧
    20 + 20 + 20 = 60 := by
  norm_num [controlMean, supposeMean, learnMean]

/-- Consistent vs inconsistent pairs (p.377): learn 0.72 vs 0.50, suppose 0.55
vs 0.48; the learn difference exceeds the suppose difference by 0.15 (the
direction of the reported interaction, F(1,19) = 17.7). -/
theorem exp3_interaction :
    ((0.72 : ℚ) - 0.50) - (0.55 - 0.48) = 0.15 ∧ (0 : ℚ) < 0.15 := by norm_num

/-- The quadratic (Brier) penalty for issuing `p` when the outcome is `o`. -/
def brier (p o : ℝ) : ℝ := (o - p) ^ 2

/-- "Assigning the noncommittal probability 0.5 guarantees a penalty of 0.25"
(p.377). -/
theorem brier_half (o : ℝ) (ho : o = 0 ∨ o = 1) : brier (1 / 2) o = 1 / 4 := by
  rcases ho with rfl | rfl <;> norm_num [brier]

/-- Table 5 (c) and text (p.377): learn 0.18, suppose 0.25 ("almost exactly
0.25"), control 0.29. -/
theorem exp3_penalties :
    (0.18 : ℚ) < 0.25 ∧ (0.25 : ℚ) = 1 / 4 ∧ (0.25 : ℚ) < 0.29 := by norm_num

/-! ## Decks (Tables 1 and 3) and the extremeness claim (p.375) -/

/-- A deck: (green dog, green duck, yellow dog, yellow duck). -/
abbrev Deck := ℕ × ℕ × ℕ × ℕ

/-- Table 1 (p.374), Experiment 1. -/
def table1 : List Deck := [(5, 4, 6, 5), (9, 2, 6, 3), (4, 8, 2, 6), (7, 8, 2, 3), (3, 3, 6, 8)]
/-- Table 3 (p.376), Experiment 2. -/
def table3 : List Deck := [(9, 2, 1, 8), (2, 8, 9, 1), (7, 1, 3, 9), (3, 8, 7, 2), (8, 3, 2, 7)]

theorem table1_decks : table1.length = 5 ∧ ∀ d ∈ table1, d.1 + d.2.1 + d.2.2.1 + d.2.2.2 = 20 := by
  decide
theorem table3_decks : table3.length = 5 ∧ ∀ d ∈ table3, d.1 + d.2.1 + d.2.2.1 + d.2.2.2 = 20 := by
  decide

/-- Sum over a deck's four distinct objective conditionals (dog | green,
dog | yellow, green | dog, green | duck) of `|Pr(A|B) − 1/2|`; the other four
conditionals are complements with the same deviation. -/
def deckDev (d : Deck) : ℚ :=
  let (gd, gk, yd, yk) := d
  |(gd : ℚ) / (gd + gk) - 1 / 2| + |(yd : ℚ) / (yd + yk) - 1 / 2| +
  |(gd : ℚ) / (gd + yd) - 1 / 2| + |(gk : ℚ) / (gk + yk) - 1 / 2|

/-- **The objective probabilities in Experiment 2 are more extreme** (p.375;
Fig. 1 caption): the mean deviation from 1/2 over the 20 distinct (deck,
conditional) pairs is below 0.14 in Experiment 1 and above 0.30 in
Experiment 2. -/
theorem exp2_more_extreme :
    (table1.map deckDev).sum / 20 < 0.14 ∧ 0.30 < (table3.map deckDev).sum / 20 := by
  simp only [table1, table3, List.map, List.sum_cons, List.sum_nil, deckDev]
  norm_num [abs_of_nonneg, abs_of_nonpos, abs_of_neg, abs_of_pos]

/-- Tables 2 and 4, row (b), mean absolute deviation from 0.5: suppose more
extreme in Experiment 1 (0.19 vs 0.14), learn more extreme in Experiment 2
(0.23 vs 0.18). -/
theorem extremeness_rows : (0.14 : ℚ) < 0.19 ∧ (0.18 : ℚ) < 0.23 := by norm_num

end Literature.Zhao2012
