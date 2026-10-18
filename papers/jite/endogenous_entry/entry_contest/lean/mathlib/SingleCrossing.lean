import Mathlib
import Spreads

/-!
# Single-crossing widenings and narrowings without order preservation

`Spreads.lean` (M13) and `Implications.lean` (M33) state the sign rule for a change of profile
given by one non-decreasing map `T` applied to every wealth. Monotonicity of `T` keeps the order
of the challengers, so the marginal entrant is the same challenger before and after. This file
shows the sign rule does not need it.

A change is given challenger by challenger: the challenger at rank `i` of the sorted profile `w`
moves to the after-wealth `g i`, and the after-profile `w'` is any sorted rearrangement of `g`.
Challengers may overtake one another.

* A **single-crossing widening** about `x0`: every wealth at or above `x0` weakly rises and every
  wealth at or below `x0` weakly falls (`IsWidening`).
* A **single-crossing narrowing** about `x0`: every wealth moves toward `x0` without crossing it
  (`IsNarrowing`).

## What is proved

1. Two counting facts about a sorted rearrangement (`ge_of_many`, `le_of_many`).
2. Under a widening, the after-wealth at rank `j` is at least the before-wealth there when the
   before-wealth is at or above `x0` (`widening_up`), and at most it when at or below
   (`widening_down`); the same for a narrowing with the sides exchanged (`narrowing_up`,
   `narrowing_down`). So the rank-by-rank displacement of the sorted profiles crosses zero once.
3. The sign rule for the entry count, through the margin condition M12 (`widening_rise`,
   `widening_fall`, `narrowing_rise`, `narrowing_fall`).
4. The monotone class of M13 is a special case (`pivot_is_widening`, `pivot_sorted`).

Controls: a widening in which a poorer challenger overtakes a richer one is covered here and by no
monotone map (`overtaking_widening`, `overtaking_not_monotone`); a change that lowers a wealth above
`x0` is not a widening (`widening_needs_side`).
-/

open Finset

set_option linter.unusedSectionVars false

namespace SingleCrossing

open EntryContestSpread

variable {Q : ℕ}

/-- `w'` is a sorted rearrangement of the after-wealths `g`: sorted from the richest, and each
    challenger's after-wealth sits at some rank of `w'`, one challenger to a rank. -/
def IsSortedOf (w' : ℕ → ℝ) (g : Fin Q → ℝ) : Prop :=
  Antitone w' ∧ ∃ σ : Equiv.Perm (Fin Q), ∀ i, g i = w' (σ i)

/-- A single-crossing widening about `x0` of the sorted profile `w`. -/
def IsWidening (x0 : ℝ) (w : ℕ → ℝ) (g : Fin Q → ℝ) : Prop :=
  ∀ i : Fin Q, (x0 ≤ w i → w i ≤ g i) ∧ (w i ≤ x0 → g i ≤ w i)

/-- A single-crossing narrowing about `x0` of the sorted profile `w`. -/
def IsNarrowing (x0 : ℝ) (w : ℕ → ℝ) (g : Fin Q → ℝ) : Prop :=
  ∀ i : Fin Q, (x0 ≤ w i → x0 ≤ g i ∧ g i ≤ w i) ∧ (w i ≤ x0 → w i ≤ g i ∧ g i ≤ x0)

/-! ## 1. Counting facts about a sorted rearrangement -/

/-- If at least `j + 1` after-wealths are at least `v`, the after-wealth at rank `j` is. -/
theorem ge_of_many {w' : ℕ → ℝ} {g : Fin Q → ℝ} (hs : IsSortedOf w' g) (j : ℕ) (v : ℝ)
    (hmany : j + 1 ≤ #(univ.filter fun i => v ≤ g i)) : v ≤ w' j := by
  by_contra hlt
  push_neg at hlt
  obtain ⟨hanti, σ, hσ⟩ := hs
  have hle : #(univ.filter fun i => v ≤ g i) ≤ #(range j) := by
    refine card_le_card_of_injOn (fun i => ((σ i : Fin Q) : ℕ)) ?_ ?_
    · intro i hi
      simp only [coe_filter, mem_univ, true_and, Set.mem_setOf_eq] at hi
      simp only [coe_range, Set.mem_Iio]
      by_contra hge
      push_neg at hge
      have h1 := hanti hge
      rw [← hσ i] at h1
      linarith
    · intro a _ b _ hab
      exact σ.injective (Fin.ext hab)
  rw [card_range] at hle
  omega

/-- If at least `Q − j` after-wealths are at most `v`, the after-wealth at rank `j < Q` is. -/
theorem le_of_many {w' : ℕ → ℝ} {g : Fin Q → ℝ} (hs : IsSortedOf w' g) (j : ℕ) (hj : j < Q)
    (v : ℝ) (hmany : Q - j ≤ #(univ.filter fun i => g i ≤ v)) : w' j ≤ v := by
  by_contra hlt
  push_neg at hlt
  obtain ⟨hanti, σ, hσ⟩ := hs
  have hle : #(univ.filter fun i => g i ≤ v) ≤ #(Ioo j Q) := by
    refine card_le_card_of_injOn (fun i => ((σ i : Fin Q) : ℕ)) ?_ ?_
    · intro i hi
      simp only [coe_filter, mem_univ, true_and, Set.mem_setOf_eq] at hi
      simp only [coe_Ioo, Set.mem_Ioo]
      refine ⟨?_, (σ i).isLt⟩
      by_contra hge
      push_neg at hge
      have h1 := hanti hge
      rw [← hσ i] at h1
      linarith
    · intro a _ b _ hab
      exact σ.injective (Fin.ext hab)
  rw [Nat.card_Ioo] at hle
  omega

/-- The ranks `n < Q` as challengers. -/
def toFin (hQ : 0 < Q) (n : ℕ) : Fin Q := ⟨n % Q, Nat.mod_lt _ hQ⟩

theorem toFin_val (hQ : 0 < Q) {n : ℕ} (hn : n < Q) : ((toFin hQ n : Fin Q) : ℕ) = n :=
  Nat.mod_eq_of_lt hn

/-- At least `j + 1` after-wealths are at least `v` when every rank `n ≤ j` has one. -/
theorem many_ge {g : Fin Q → ℝ} (j : ℕ) (hj : j < Q) (v : ℝ)
    (h : ∀ i : Fin Q, (i : ℕ) ≤ j → v ≤ g i) : j + 1 ≤ #(univ.filter fun i => v ≤ g i) := by
  have hQ : 0 < Q := by omega
  have := card_le_card_of_injOn (s := range (j + 1)) (t := univ.filter fun i => v ≤ g i)
    (toFin hQ) ?_ ?_
  · rwa [card_range] at this
  · intro n hn
    simp only [coe_range, Set.mem_Iio] at hn
    simp only [coe_filter, mem_univ, true_and, Set.mem_setOf_eq]
    exact h _ (by rw [toFin_val hQ (by omega)]; omega)
  · intro a ha b hb hab
    simp only [coe_range, Set.mem_Iio] at ha hb
    have := congrArg Fin.val hab
    rwa [toFin_val hQ (by omega), toFin_val hQ (by omega)] at this

/-- At least `Q − j` after-wealths are at most `v` when every rank `n` with `j ≤ n < Q` has one. -/
theorem many_le {g : Fin Q → ℝ} (j : ℕ) (hj : j < Q) (v : ℝ)
    (h : ∀ i : Fin Q, j ≤ (i : ℕ) → g i ≤ v) : Q - j ≤ #(univ.filter fun i => g i ≤ v) := by
  have hQ : 0 < Q := by omega
  have := card_le_card_of_injOn (s := Ico j Q) (t := univ.filter fun i => g i ≤ v)
    (toFin hQ) ?_ ?_
  · rwa [Nat.card_Ico] at this
  · intro n hn
    simp only [coe_Ico, Set.mem_Ico] at hn
    simp only [coe_filter, mem_univ, true_and, Set.mem_setOf_eq]
    exact h _ (by rw [toFin_val hQ hn.2]; exact hn.1)
  · intro a ha b hb hab
    simp only [coe_Ico, Set.mem_Ico] at ha hb
    have := congrArg Fin.val hab
    rwa [toFin_val hQ ha.2, toFin_val hQ hb.2] at this

/-! ## 2. The sorted displacement crosses zero once -/

variable {x0 : ℝ} {w w' : ℕ → ℝ} {g : Fin Q → ℝ}

theorem widening_up (hw : Antitone w) (hW : IsWidening x0 w g) (hs : IsSortedOf w' g)
    (j : ℕ) (hj : j < Q) (hx : x0 ≤ w j) : w j ≤ w' j :=
  ge_of_many hs j (w j) (many_ge j hj (w j) fun i hi =>
    have hwi : w j ≤ w i := hw hi
    hwi.trans ((hW i).1 (hx.trans hwi)))

theorem widening_down (hw : Antitone w) (hW : IsWidening x0 w g) (hs : IsSortedOf w' g)
    (j : ℕ) (hj : j < Q) (hx : w j ≤ x0) : w' j ≤ w j :=
  le_of_many hs j hj (w j) (many_le j hj (w j) fun i hi =>
    have hwi : w i ≤ w j := hw hi
    ((hW i).2 (hwi.trans hx)).trans hwi)

theorem narrowing_up (hw : Antitone w) (hN : IsNarrowing x0 w g) (hs : IsSortedOf w' g)
    (j : ℕ) (hj : j < Q) (hx : w j ≤ x0) : w j ≤ w' j :=
  ge_of_many hs j (w j) (many_ge j hj (w j) fun i hi => by
    have hwi : w j ≤ w i := hw hi
    rcases le_total x0 (w i) with h | h
    · exact hx.trans ((hN i).1 h).1
    · exact hwi.trans ((hN i).2 h).1)

theorem narrowing_down (hw : Antitone w) (hN : IsNarrowing x0 w g) (hs : IsSortedOf w' g)
    (j : ℕ) (hj : j < Q) (hx : x0 ≤ w j) : w' j ≤ w j :=
  le_of_many hs j hj (w j) (many_le j hj (w j) fun i hi => by
    have hwi : w i ≤ w j := hw hi
    rcases le_total x0 (w i) with h | h
    · exact ((hN i).1 h).2.trans hwi
    · exact ((hN i).2 h).2.trans hx)

/-- Under a widening the sorted displacement never puts the margin in the band: a positive
    displacement at the first outsider forces a non-negative one at the marginal entrant. -/
theorem widening_not_band (hw : Antitone w) (hW : IsWidening x0 w g) (hs : IsSortedOf w' g)
    (k : ℕ) (hk1 : 1 ≤ k) (hkQ : k < Q) (hpos : 0 < w' k - w k) : 0 ≤ w' (k - 1) - w (k - 1) := by
  have hxk : x0 < w k := by
    by_contra h
    push_neg at h
    have := widening_down hw hW hs k hkQ h
    linarith
  have := widening_up hw hW hs (k - 1) (by omega) (hxk.le.trans (hw (by omega : k - 1 ≤ k)))
  linarith

/-! ## 3. The sign rule for the entry count -/

variable (κ : ℝ → ℝ) (c : ℝ) (Δ : ℕ → ℝ)

/-- **Widening, rise branch.** With the marginal entrant at or above `x0`, the count does not
    fall. -/
theorem widening_rise (hκ : AntitoneOn κ (Set.Ioi c)) (hΔ : Antitone Δ) (hw : Antitone w)
    (hW : IsWidening x0 w g) (hs : IsSortedOf w' g) {k k' : ℕ}
    (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q w' k') (hm : 1 ≤ k → x0 ≤ w (k - 1)) :
    k ≤ k' :=
  margin_rise κ c Δ Q hκ hΔ w w' hs.1 k k' hk hk'
    (fun h1 => widening_up hw hW hs (k - 1) (by have := hk.1; omega) (hm h1))

/-- **Widening, fall branch.** With the marginal entrant at or below `x0`, the count does not
    rise. -/
theorem widening_fall (hκ : AntitoneOn κ (Set.Ioi c)) (hw : Antitone w)
    (hW : IsWidening x0 w g) (hs : IsSortedOf w' g) {k k' : ℕ}
    (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q w' k') (h1 : 1 ≤ k)
    (hm : w (k - 1) ≤ x0) : k' ≤ k :=
  margin_fall κ c Δ Q hκ w w' k k' hk hk'
    (fun hkQ => widening_down hw hW hs k hkQ ((hw (by omega : k - 1 ≤ k)).trans hm))

/-- **Narrowing, rise branch.** With the marginal entrant at or below `x0`, the count does not
    fall. -/
theorem narrowing_rise (hκ : AntitoneOn κ (Set.Ioi c)) (hΔ : Antitone Δ) (hw : Antitone w)
    (hN : IsNarrowing x0 w g) (hs : IsSortedOf w' g) {k k' : ℕ}
    (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q w' k') (hm : 1 ≤ k → w (k - 1) ≤ x0) :
    k ≤ k' :=
  margin_rise κ c Δ Q hκ hΔ w w' hs.1 k k' hk hk'
    (fun h1 => narrowing_up hw hN hs (k - 1) (by have := hk.1; omega) (hm h1))

/-- **Narrowing, fall branch.** With the first outsider at or above `x0`, the count does not
    rise. -/
theorem narrowing_fall (hκ : AntitoneOn κ (Set.Ioi c)) (hw : Antitone w)
    (hN : IsNarrowing x0 w g) (hs : IsSortedOf w' g) {k k' : ℕ}
    (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q w' k') (hm : k < Q → x0 ≤ w k) :
    k' ≤ k :=
  margin_fall κ c Δ Q hκ w w' k k' hk hk'
    (fun hkQ => narrowing_down hw hN hs k hkQ (hm hkQ))

/-! ## 3a. The same, stated on the two sorted profiles alone

A single-crossing widening, read on the sorted profiles: the displacement is non-negative at
every rank whose first-profile wealth is at or above `x0` and non-positive at every rank at or
below. No pairing of challengers is needed, and a pairing that is a widening implies it. -/

/-- The displacement of the sorted profiles crosses zero once, from the top. -/
def IsRankWidening (Q : ℕ) (x0 : ℝ) (w w' : ℕ → ℝ) : Prop :=
  ∀ j, j < Q → (x0 ≤ w j → w j ≤ w' j) ∧ (w j ≤ x0 → w' j ≤ w j)

/-- The displacement of the sorted profiles crosses zero once, toward `x0`. -/
def IsRankNarrowing (Q : ℕ) (x0 : ℝ) (w w' : ℕ → ℝ) : Prop :=
  ∀ j, j < Q → (x0 ≤ w j → w' j ≤ w j) ∧ (w j ≤ x0 → w j ≤ w' j)

/-- A pairing that is a widening gives a rank widening of the sorted profiles. -/
theorem widening_rank (hw : Antitone w) (hW : IsWidening x0 w g) (hs : IsSortedOf w' g) :
    IsRankWidening Q x0 w w' :=
  fun j hj => ⟨widening_up hw hW hs j hj, widening_down hw hW hs j hj⟩

/-- A pairing that is a narrowing gives a rank narrowing of the sorted profiles. -/
theorem narrowing_rank (hw : Antitone w) (hN : IsNarrowing x0 w g) (hs : IsSortedOf w' g) :
    IsRankNarrowing Q x0 w w' :=
  fun j hj => ⟨narrowing_down hw hN hs j hj, narrowing_up hw hN hs j hj⟩

theorem rank_widening_rise (hκ : AntitoneOn κ (Set.Ioi c)) (hΔ : Antitone Δ) (hw' : Antitone w')
    (hR : IsRankWidening Q x0 w w') {k k' : ℕ}
    (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q w' k') (hm : 1 ≤ k → x0 ≤ w (k - 1)) :
    k ≤ k' :=
  margin_rise κ c Δ Q hκ hΔ w w' hw' k k' hk hk'
    (fun h1 => (hR (k - 1) (by have := hk.1; omega)).1 (hm h1))

theorem rank_widening_fall (hκ : AntitoneOn κ (Set.Ioi c)) (hw : Antitone w)
    (hR : IsRankWidening Q x0 w w') {k k' : ℕ}
    (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q w' k') (h1 : 1 ≤ k)
    (hm : w (k - 1) ≤ x0) : k' ≤ k :=
  margin_fall κ c Δ Q hκ w w' k k' hk hk'
    (fun hkQ => (hR k hkQ).2 ((hw (by omega : k - 1 ≤ k)).trans hm))

theorem rank_narrowing_rise (hκ : AntitoneOn κ (Set.Ioi c)) (hΔ : Antitone Δ) (hw' : Antitone w')
    (hR : IsRankNarrowing Q x0 w w') {k k' : ℕ}
    (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q w' k') (hm : 1 ≤ k → w (k - 1) ≤ x0) :
    k ≤ k' :=
  margin_rise κ c Δ Q hκ hΔ w w' hw' k k' hk hk'
    (fun h1 => (hR (k - 1) (by have := hk.1; omega)).2 (hm h1))

theorem rank_narrowing_fall (hκ : AntitoneOn κ (Set.Ioi c))
    (hR : IsRankNarrowing Q x0 w w') {k k' : ℕ}
    (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q w' k') (hm : k < Q → x0 ≤ w k) :
    k' ≤ k :=
  margin_fall κ c Δ Q hκ w w' k k' hk hk' (fun hkQ => (hR k hkQ).1 (hm hkQ))

/-- A rank widening never puts the margin in the band. -/
theorem rank_widening_not_band (hw : Antitone w) (hR : IsRankWidening Q x0 w w')
    (k : ℕ) (hk1 : 1 ≤ k) (hkQ : k < Q) (hpos : 0 < w' k - w k) : 0 ≤ w' (k - 1) - w (k - 1) := by
  have hxk : x0 < w k := by
    by_contra h
    push_neg at h
    have := (hR k hkQ).2 h
    linarith
  have := (hR (k - 1) (by omega)).1 (hxk.le.trans (hw (by omega : k - 1 ≤ k)))
  linarith

/-! ## 4. The monotone class of M13 is a special case -/

theorem pivot_is_widening {T : ℝ → ℝ} (hT : IsPivotSpread T x0) (w : ℕ → ℝ) :
    IsWidening x0 w (fun i : Fin Q => T (w i)) :=
  fun _ => ⟨hT.2.1 _, hT.2.2 _⟩

theorem pivot_sorted {T : ℝ → ℝ} (hT : IsPivotSpread T x0) (hw : Antitone w) :
    IsSortedOf (Q := Q) (fun j => T (w j)) (fun i : Fin Q => T (w i)) :=
  ⟨fun _ _ hij => hT.1 (hw hij), Equiv.refl _, fun _ => rfl⟩

/-! ## Controls -/

/-- Two challengers with wealths `3` and `2`, both above `x0 = 1`. The after-wealths `3` and `4`
    form a single-crossing widening in which the poorer challenger overtakes the richer. -/
def wTwo (j : ℕ) : ℝ := if j = 0 then 3 else 2

theorem overtaking_widening : IsWidening (Q := 2) 1 wTwo ![3, 4] := by
  intro i
  fin_cases i <;> norm_num [wTwo]

/-- No non-decreasing map sends the wealths `3` and `2` to `3` and `4`. -/
theorem overtaking_not_monotone :
    ¬ ∃ T : ℝ → ℝ, Monotone T ∧ T 3 = 3 ∧ T 2 = 4 := by
  rintro ⟨T, hT, h3, h2⟩
  have := hT (show (2 : ℝ) ≤ 3 by norm_num)
  rw [h3, h2] at this
  norm_num at this

/-- A change that lowers the wealth `3`, above `x0 = 1`, is not a widening. -/
theorem widening_needs_side : ¬ IsWidening (Q := 2) 1 wTwo ![2, 2] := by
  intro h
  have := (h 0).1 (by norm_num [wTwo])
  norm_num [wTwo] at this

end SingleCrossing
