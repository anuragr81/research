/-
# Canay, Mogstad & Mountjoy (2024), "On the Use of Outcome Tests for Detecting Bias in Decision Making"

*Review of Economic Studies* 91(4), 2135-2167.  DOI 10.1093/restud/rdad082.

Source read: NBER Working Paper 27802, revised 10 June 2023, Sections 2-4 in
full and Appendix A (Definitions A.1-A.3 and the proof of Theorem 4.1, cases
(i)-(ii)).  `p.N` is the printed page of the main text (PDF page minus two);
`A-p.N` the supplemental appendix's own numbering.

## The model (Sections 2.2-2.3, 4.1)

A judge `z` releases a defendant of race `r` and non-race characteristics `v`
iff `E[Δ|r,v] ≤ τ(z,r,v)` (Definition 2.1, the Generalized Roy Model, GRM),
with `τ = c + λ + β` the perceived benefit of release.  The Extended Roy Model
(ERM, Definition 2.2) restricts `τ(z,r,v) = τ(z,r)`.  The judge's marginal
defendant of race `r` has `v*_{z,r}` with `E[Δ|r,v*] = τ(z,r,v*)` (eq. 12,
p.19), assumed unique; the outcome test reads the difference
`E[Δ|w,v*_w] - E[Δ|b,v*_b]` (eq. 15, p.21).  Below `Λ r v` is `E[Δ|r,v]`
(Appendix eq. A.1) and the judge `z` is fixed, so `τ r v` is `τ(z,r,v)`.

## What is formalized

  * **Definitions 3.2 and 3.4** (pp.15, 17): τ-unbiased, globally and locally
    τ-biased against black or white defendants, and "unclassified" (p.17: any
    behaviour that cannot be labelled unbiased, locally or globally biased).
  * **The function class `F^cm(V)`** (Definitions A.2-A.3, A-p.1) on `V = ℝ`
    with range `I = ℝ`: both functions continuous and weakly monotone, a
    crossing (condition 4), and condition 5 for `τ(z,w,·)`.  Condition 3,
    `inf I < τ < sup I`, is vacuous for `I = ℝ`.
  * **Theorem 4.2** (p.24), in general: in the ERM the marginal outcome
    difference equals `τ(z,w) - τ(z,b)`, so the test concludes bias against
    black defendants iff `τ(z,w) > τ(z,b)` and no bias iff they are equal
    (`thm42`, `thm42_gt_iff`, `thm42_eq_iff`).
  * **Theorem 4.1** (pp.20-21), by explicit witnesses in `F^cm(ℝ)` with a
    unique marginal for each race: in each of the four cases (unbiased;
    globally biased against black, against white; locally biased against black,
    against white; unclassified) the difference (15) takes ANY prescribed real
    value, in particular a positive, a negative and a zero one
    (`thm41_case_i` ... `thm41_case_iv`).  The appendix proves the theorem by
    constructing the cost function around a given benefit function; the
    witnesses here are simpler instances of the same statement, not a
    transcription of that construction.
  * **The conclusion quoted by plan entry 7.2** (p.21, "the outcome test may
    conclude no bias even if the judge is locally or globally racially
    biased"): `biased_judge_passes`.

## What is not formalized

Theorem 4.1 for the larger classes `F`, `F^m(V')`, `F^cm(V')` (they contain
`F^cm(ℝ)`, so the witnesses here already lie in them up to the choice of
`V' ⊆ V`, which this file does not model); the β-bias version (Remark 4.1);
Section 5 on econometric viability.
-/
import Mathlib

namespace Literature.CanayMogstadMountjoy

/-- Defendant race: white or black. -/
inductive Race | w | b
  deriving DecidableEq

/-! ## Definitions 3.2 and 3.4 (τ-bias of a fixed judge) -/

/-- Definition 3.2: racially τ-unbiased. -/
def TauUnbiased (τ : Race → ℝ → ℝ) : Prop := ∀ v, τ .w v = τ .b v

/-- Definition 3.4 with (9): globally τ-biased against black defendants. -/
def GloballyAgainstBlack (τ : Race → ℝ → ℝ) : Prop := ∀ v, τ .w v > τ .b v

def GloballyAgainstWhite (τ : Race → ℝ → ℝ) : Prop := ∀ v, τ .b v > τ .w v

/-- Definition 3.4 with (8): locally τ-biased against black defendants, weakly
everywhere and strictly on a non-empty open set. -/
def LocallyAgainstBlack (τ : Race → ℝ → ℝ) : Prop :=
  (∀ v, τ .w v ≥ τ .b v) ∧ ∃ S : Set ℝ, IsOpen S ∧ S.Nonempty ∧ ∀ v ∈ S, τ .w v > τ .b v

def LocallyAgainstWhite (τ : Race → ℝ → ℝ) : Prop :=
  (∀ v, τ .b v ≥ τ .w v) ∧ ∃ S : Set ℝ, IsOpen S ∧ S.Nonempty ∧ ∀ v ∈ S, τ .b v > τ .w v

/-- "Unclassified" (p.17): neither unbiased nor locally or globally biased
against either race. -/
def Unclassified (τ : Race → ℝ → ℝ) : Prop :=
  ¬ TauUnbiased τ ∧ ¬ LocallyAgainstBlack τ ∧ ¬ LocallyAgainstWhite τ ∧
    ¬ GloballyAgainstBlack τ ∧ ¬ GloballyAgainstWhite τ

/-! ## The class F^cm(V), V = I = ℝ (Definitions A.2-A.3) -/

/-- Conditions 1, 2 and 4 of Definitions A.2-A.3 for one race: both functions
continuous and weakly monotone, crossing at `vs`. -/
def InFcm (Λ τ : ℝ → ℝ) (vs : ℝ) : Prop :=
  Continuous Λ ∧ (Monotone Λ ∨ Antitone Λ) ∧ Continuous τ ∧ (Monotone τ ∨ Antitone τ) ∧ Λ vs = τ vs

/-- Condition 5 for the white benefit function: the sets of points below and
above its marginal value are non-empty open sets. -/
def Cond5 (τ : ℝ → ℝ) (vs : ℝ) : Prop :=
  IsOpen {v | τ v < τ vs} ∧ {v | τ v < τ vs}.Nonempty ∧
    IsOpen {v | τ vs < τ v} ∧ {v | τ vs < τ v}.Nonempty

/-- The marginal value is unique (assumed on p.19). -/
def UniqueMarginal (Λ τ : ℝ → ℝ) (vs : ℝ) : Prop := ∀ v, Λ v = τ v → v = vs

/-- The witness bundle for one case of Theorem 4.1. -/
def Witness (P : (Race → ℝ → ℝ) → Prop) (t : ℝ) : Prop :=
  ∃ (Λ τ : Race → ℝ → ℝ) (vs : Race → ℝ),
    P τ ∧ (∀ r, InFcm (Λ r) (τ r) (vs r)) ∧ (∀ r, UniqueMarginal (Λ r) (τ r) (vs r)) ∧
      Cond5 (τ .w) (vs .w) ∧ Λ .w (vs .w) - Λ .b (vs .b) = t

/-! ## Theorem 4.2 (ERM): logical validity -/

/-- **Theorem 4.2, eq. (16).** In the ERM the benefit does not depend on `v`;
at the marginal defendants the outcome difference equals the bias. -/
theorem thm42 (Λ : Race → ℝ → ℝ) (τ : Race → ℝ) (vw vb : ℝ)
    (hw : Λ .w vw = τ .w) (hb : Λ .b vb = τ .b) :
    Λ .w vw - Λ .b vb = τ .w - τ .b := by rw [hw, hb]

theorem thm42_gt_iff (Λ : Race → ℝ → ℝ) (τ : Race → ℝ) (vw vb : ℝ)
    (hw : Λ .w vw = τ .w) (hb : Λ .b vb = τ .b) :
    Λ .w vw > Λ .b vb ↔ τ .w > τ .b := by rw [hw, hb]

theorem thm42_eq_iff (Λ : Race → ℝ → ℝ) (τ : Race → ℝ) (vw vb : ℝ)
    (hw : Λ .w vw = τ .w) (hb : Λ .b vb = τ .b) :
    Λ .w vw = Λ .b vb ↔ τ .w = τ .b := by rw [hw, hb]

/-! ## Building blocks for the witnesses -/

theorem continuous_sub_left (k : ℝ) : Continuous (fun v : ℝ => k - v) :=
  continuous_const.sub continuous_id

theorem antitone_sub_left (k : ℝ) : Antitone (fun v : ℝ => k - v) := by
  intro a b h; simp only; linarith

theorem cond5_of_strictMono_cont {τ : ℝ → ℝ} (hm : StrictMono τ) (hc : Continuous τ) (vs : ℝ) :
    Cond5 τ vs := by
  refine ⟨isOpen_lt hc continuous_const, ⟨vs - 1, ?_⟩, isOpen_lt continuous_const hc, ⟨vs + 1, ?_⟩⟩
  · exact hm (by linarith)
  · exact hm (by linarith)

/-- The kinked benefit `v + max 0 (-v) / 2`: equal to `v` on `[0,∞)` and to
`v/2` on `(-∞,0)`. -/
noncomputable def kink (v : ℝ) : ℝ := v + max 0 (-v) / 2

theorem kink_strictMono : StrictMono kink := by
  intro a b h
  unfold kink
  rcases le_total 0 (-a) with ha | ha <;> rcases le_total 0 (-b) with hb | hb <;>
    simp only [max_eq_right, max_eq_left, ha, hb] <;> linarith

theorem kink_continuous : Continuous kink := by
  unfold kink; fun_prop

theorem kink_ge (v : ℝ) : kink v ≥ v := by
  unfold kink; have := le_max_left (0 : ℝ) (-v); linarith

theorem kink_gt_of_neg {v : ℝ} (h : v < 0) : kink v > v := by
  unfold kink; rw [max_eq_right (by linarith)]; linarith

theorem kink_zero : kink 0 = 0 := by simp [kink]

/-- `-v = kink v` only at `v = 0`. -/
theorem neg_eq_kink_iff (v : ℝ) : (0 : ℝ) - v = kink v → v = 0 := by
  unfold kink
  rcases le_total 0 (-v) with h | h
  · rw [max_eq_right h]; intro e; linarith
  · rw [max_eq_left h]; intro e; linarith

/-! ## Theorem 4.1 (GRM): logical invalidity, by witnesses in F^cm(ℝ) -/

/-- **Case (i): τ-unbiased.** Benefit `v` for both races, cost `k_r - v`. -/
theorem thm41_case_i (t : ℝ) : Witness TauUnbiased t := by
  refine ⟨fun r v => match r with | .w => 2 * t - v | .b => 0 - v,
          fun _ v => v, fun r => match r with | .w => t | .b => 0, ?_, ?_, ?_, ?_, ?_⟩
  · intro v; rfl
  · intro r; cases r
    · exact ⟨continuous_sub_left _, Or.inr (antitone_sub_left _), continuous_id,
             Or.inl monotone_id, by (try simp only); ring⟩
    · exact ⟨continuous_sub_left _, Or.inr (antitone_sub_left _), continuous_id,
             Or.inl monotone_id, by simp⟩
  · intro r; cases r <;> intro v h <;> (try simp only at h ⊢) <;> linarith
  · exact cond5_of_strictMono_cont strictMono_id continuous_id _
  · (try simp only); ring

/-- **Case (ii), against black: globally τ-biased against black defendants.**
Benefit `v + 1` (white) and `v` (black). -/
theorem thm41_case_ii_black (t : ℝ) : Witness GloballyAgainstBlack t := by
  refine ⟨fun r v => match r with | .w => (2 * t - 1) - v | .b => 0 - v,
          fun r v => match r with | .w => v + 1 | .b => v,
          fun r => match r with | .w => t - 1 | .b => 0, ?_, ?_, ?_, ?_, ?_⟩
  · intro v; (try simp only); linarith
  · intro r; cases r
    · exact ⟨continuous_sub_left _, Or.inr (antitone_sub_left _), continuous_id.add continuous_const,
             Or.inl (fun a b h => by (try simp only); linarith), by (try simp only); ring⟩
    · exact ⟨continuous_sub_left _, Or.inr (antitone_sub_left _), continuous_id,
             Or.inl monotone_id, by simp⟩
  · intro r; cases r <;> intro v h <;> (try simp only at h ⊢) <;> linarith
  · exact cond5_of_strictMono_cont (τ := fun v => v + 1) (fun a b h => by (try simp only); linarith)
      (by fun_prop) _
  · (try simp only); ring

/-- **Case (ii), against white.** Benefit `v` (white) and `v + 1` (black). -/
theorem thm41_case_ii_white (t : ℝ) : Witness GloballyAgainstWhite t := by
  refine ⟨fun r v => match r with | .w => 2 * t - v | .b => (-1) - v,
          fun r v => match r with | .w => v | .b => v + 1,
          fun r => match r with | .w => t | .b => -1, ?_, ?_, ?_, ?_, ?_⟩
  · intro v; (try simp only); linarith
  · intro r; cases r
    · exact ⟨continuous_sub_left _, Or.inr (antitone_sub_left _), continuous_id,
             Or.inl monotone_id, by (try simp only); ring⟩
    · exact ⟨continuous_sub_left _, Or.inr (antitone_sub_left _), continuous_id.add continuous_const,
             Or.inl (fun a b h => by (try simp only); linarith), by (try simp only); ring⟩
  · intro r; cases r <;> intro v h <;> (try simp only at h ⊢) <;> linarith
  · exact cond5_of_strictMono_cont strictMono_id continuous_id _
  · (try simp only); ring

/-- **Case (iii), against black: locally τ-biased against black defendants.**
White benefit `kink v`, strictly above the black benefit `v` exactly on
`(-∞,0)`; the marginal white defendant sits at `v = 0`, outside that set. -/
theorem thm41_case_iii_black (t : ℝ) : Witness LocallyAgainstBlack t := by
  refine ⟨fun r v => match r with | .w => 0 - v | .b => (-2 * t) - v,
          fun r v => match r with | .w => kink v | .b => v,
          fun r => match r with | .w => 0 | .b => -t, ?_, ?_, ?_, ?_, ?_⟩
  · refine ⟨fun v => kink_ge v, Set.Iio 0, isOpen_Iio, ⟨-1, by norm_num⟩, fun v hv => kink_gt_of_neg hv⟩
  · intro r; cases r
    · exact ⟨continuous_sub_left _, Or.inr (antitone_sub_left _), kink_continuous,
             Or.inl kink_strictMono.monotone, by simp only; rw [kink_zero]; ring⟩
    · exact ⟨continuous_sub_left _, Or.inr (antitone_sub_left _), continuous_id,
             Or.inl monotone_id, by (try simp only); ring⟩
  · intro r; cases r
    · intro v h; exact neg_eq_kink_iff v h
    · intro v h; (try simp only at h ⊢); linarith
  · exact cond5_of_strictMono_cont kink_strictMono kink_continuous _
  · (try simp only); ring

/-- **Case (iii), against white.** Black benefit `kink v`, white benefit `v`. -/
theorem thm41_case_iii_white (t : ℝ) : Witness LocallyAgainstWhite t := by
  refine ⟨fun r v => match r with | .w => 2 * t - v | .b => 0 - v,
          fun r v => match r with | .w => v | .b => kink v,
          fun r => match r with | .w => t | .b => 0, ?_, ?_, ?_, ?_, ?_⟩
  · refine ⟨fun v => kink_ge v, Set.Iio 0, isOpen_Iio, ⟨-1, by norm_num⟩, fun v hv => kink_gt_of_neg hv⟩
  · intro r; cases r
    · exact ⟨continuous_sub_left _, Or.inr (antitone_sub_left _), continuous_id,
             Or.inl monotone_id, by (try simp only); ring⟩
    · exact ⟨continuous_sub_left _, Or.inr (antitone_sub_left _), kink_continuous,
             Or.inl kink_strictMono.monotone, by simp only; rw [kink_zero]; ring⟩
  · intro r; cases r
    · intro v h; (try simp only at h ⊢); linarith
    · intro v h; exact neg_eq_kink_iff v h
  · exact cond5_of_strictMono_cont strictMono_id continuous_id _
  · (try simp only); ring

/-- **Case (iv): unclassified.** White benefit `2v`, black benefit `v`: they
cross at `0`, white above on `(0,∞)` and below on `(-∞,0)`. -/
theorem thm41_case_iv (t : ℝ) : Witness Unclassified t := by
  refine ⟨fun r v => match r with | .w => 0 - v | .b => (-2 * t) - v,
          fun r v => match r with | .w => 2 * v | .b => v,
          fun r => match r with | .w => 0 | .b => -t, ?_, ?_, ?_, ?_, ?_⟩
  · refine ⟨fun h => ?_, fun h => ?_, fun h => ?_, fun h => ?_, fun h => ?_⟩
    · have := h 1; simp only at this; linarith
    · have := h.1 (-1); simp only at this; linarith
    · have := h.1 1; simp only at this; linarith
    · have := h (-1); simp only at this; linarith
    · have := h 1; simp only at this; linarith
  · intro r; cases r
    · exact ⟨continuous_sub_left _, Or.inr (antitone_sub_left _), continuous_const.mul continuous_id,
             Or.inl (fun a b h => by (try simp only); linarith), by (try simp only); ring⟩
    · exact ⟨continuous_sub_left _, Or.inr (antitone_sub_left _), continuous_id,
             Or.inl monotone_id, by (try simp only); ring⟩
  · intro r; cases r <;> intro v h <;> (try simp only at h ⊢) <;> linarith
  · exact cond5_of_strictMono_cont (τ := fun v => 2 * v) (fun a b h => by (try simp only); linarith)
      (by fun_prop) _
  · (try simp only); ring

/-- **The conclusion quoted by plan entry 7.2** (p.21): "the outcome test may
conclude no bias even if the judge is locally or globally racially biased".
A GRM judge globally τ-biased against black defendants, with well-behaved cost
and benefit functions and unique marginals, whose marginal white and black
defendants have equal outcomes. -/
theorem biased_judge_passes : Witness GloballyAgainstBlack 0 := thm41_case_ii_black 0

/-- And the converse failure: a τ-unbiased judge on whom the test concludes
bias against black defendants (the difference is positive). -/
theorem unbiased_judge_fails : Witness TauUnbiased 1 := thm41_case_i 1

end Literature.CanayMogstadMountjoy
