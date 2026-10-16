import Mathlib

/-! Dahis, Nix and Qian, "Choosing Racial Identity in the United States,
1880–1940", NBER Working Paper 26465 (November 2019, revised June 2020), in the
paper's own terms where it has them.

The paper is empirical. What is formalised here is the structure it states and
the identities it computes, not its regressions.

1. The 2-Sided Unique Perfect (2SUP) link of Section 4 and Appendix C.1. A
   record `i` of year `t` and a record `j` of year `t + 10` are candidates,
   `cand i j`, when they agree on perfect spelling, birth place and the age
   window. A one-sided Unique Perfect link needs `j` to be the only candidate of
   `i`. A 2SUP link also needs `i` to be the only candidate of `j`. Figure A.4
   is checked as a finite example.
2. The extrapolation of Section 5.1.1. With a share `l` of black men linked, a
   rate `r` of passing among the linked and a rate `u` among the unlinked, the
   population rate is `l r + (1 − l) u`. The paper's lower bound sets `u = 0`.
   The adjusted weighted rate of Appendix F is checked as arithmetic.
3. Our reading, not the paper's statement. Section 5.3.7 reports that passing
   rises with the base-year occupational income score. If those who pass have
   a mean above the group's, the mean of those who remain falls. The paper does
   not draw this conclusion. It is the step Paper A1 would take from it. -/

namespace DahisEtAl2019

/-! ### The 2SUP link -/

section Links

variable {α β : Type*}

/-- The one-sided Unique Perfect link (Appendix C.1). `j` is the only
candidate of `i` in year `t + 10`. -/
def oneSided (cand : α → β → Prop) (i : α) (j : β) : Prop :=
  cand i j ∧ ∀ j', cand i j' → j' = j

/-- The 2-Sided Unique Perfect link (Section 4). The link is unique going
forwards and, from `j`, unique going backwards. -/
def twoSided (cand : α → β → Prop) (i : α) (j : β) : Prop :=
  oneSided cand i j ∧ ∀ i', cand i' j → i' = i

/-- Every 2SUP link is a one-sided link, so 2SUP links no more records. -/
theorem twoSided_oneSided {cand : α → β → Prop} {i : α} {j : β}
    (h : twoSided cand i j) : oneSided cand i j :=
  h.1

/-- A record of year `t + 10` is the 2SUP link of at most one record of year
`t`. The backward condition is what rules out the false link of Figure A.4. -/
theorem twoSided_unique_back {cand : α → β → Prop} {i i' : α} {j : β}
    (h : twoSided cand i j) (h' : twoSided cand i' j) : i' = i :=
  h.2 i' h'.1.1

instance decOneSided [Fintype β] [DecidableEq β] (cand : α → β → Prop)
    [∀ i j, Decidable (cand i j)] (i : α) (j : β) : Decidable (oneSided cand i j) := by
  unfold oneSided; infer_instance

instance decTwoSided [Fintype α] [Fintype β] [DecidableEq α] [DecidableEq β]
    (cand : α → β → Prop) [∀ i j, Decidable (cand i j)] (i : α) (j : β) :
    Decidable (twoSided cand i j) := by
  unfold twoSided; infer_instance

end Links

/-! ### Figure A.4

Year `t` has seven men. Records 0, 1, 2 are the black Samuel, Elijah and Abe,
and records 3 to 6 are four white Abes. Year `t + 10` has three men, record 0
the black Samuel, record 1 a white Elijah and record 2 a white Abe. Names are
coded 0 (Samuel), 1 (Elijah), 2 (Abe), and two records are candidates when the
names agree. -/

/-- The names in year `t`. -/
def nameT : Fin 7 → Fin 3 := ![0, 1, 2, 2, 2, 2, 2]

/-- The names in year `t + 10`. -/
def nameT10 : Fin 3 → Fin 3 := ![0, 1, 2]

/-- Candidates agree on the name. -/
def exCand (i : Fin 7) (j : Fin 3) : Prop := nameT i = nameT10 j

instance (i : Fin 7) (j : Fin 3) : Decidable (exCand i j) := by
  unfold exCand; infer_instance

/-- Race in year `t + 10`. Only record 0 is black. -/
def blackT10 (j : Fin 3) : Prop := j = 0

instance (j : Fin 3) : Decidable (blackT10 j) := by
  unfold blackT10; infer_instance

/-- Figure A.4a. The one-sided link joins the black Abe to the white Abe. -/
theorem figA4a_abe_linked : oneSided exCand 2 2 := by
  unfold oneSided exCand nameT nameT10
  decide

/-- Figure A.4b. The 2SUP link drops the black Abe, because the white Abe of
year `t + 10` has five candidates in year `t`. -/
theorem figA4b_abe_dropped : ∀ j, ¬ twoSided exCand 2 j := by
  unfold twoSided oneSided exCand nameT nameT10
  decide

/-- Figure A.4b. The 2SUP link keeps the black Elijah's link to the white
Elijah, who passed. -/
theorem figA4b_elijah_kept : twoSided exCand 1 1 := by
  unfold twoSided oneSided exCand nameT nameT10
  decide

/-- Figure A.4b. The 2SUP link keeps Samuel, who stays black. -/
theorem figA4b_samuel_kept : twoSided exCand 0 0 := by
  unfold twoSided oneSided exCand nameT nameT10
  decide

/-- Appendix C.1. Among the three black men of year `t`, the one-sided link
records two as passing, Elijah and Abe. -/
theorem figA4a_two_pass :
    ((Finset.univ : Finset (Fin 3)).filter
      (fun i => ∃ j, oneSided exCand (Fin.castLE (by norm_num) i) j ∧ ¬ blackT10 j)).card = 2 := by
  unfold oneSided exCand blackT10 nameT nameT10
  decide

/-- Appendix C.1. The 2SUP link records one, Elijah. -/
theorem figA4b_one_pass :
    ((Finset.univ : Finset (Fin 3)).filter
      (fun i => ∃ j, twoSided exCand (Fin.castLE (by norm_num) i) j ∧ ¬ blackT10 j)).card = 1 := by
  unfold twoSided oneSided exCand blackT10 nameT nameT10
  decide

/-- Control. Without the backward condition the black Abe is counted as
passing, although his only candidate is the future self of a white Abe. -/
theorem control_backward_needed : oneSided exCand 2 2 ∧ ¬ twoSided exCand 2 2 :=
  ⟨figA4a_abe_linked, figA4b_abe_dropped 2⟩

/-! ### The extrapolation to the population -/

/-- The population rate of passing when a share `l` is linked, the linked pass
at rate `r` and the unlinked at rate `u`. -/
def popRate (l r u : ℝ) : ℝ := l * r + (1 - l) * u

/-- Section 5.1.1. Assuming that the unlinked never pass gives a lower bound. -/
theorem lower_bound {l r u : ℝ} (hl : l ≤ 1) (hu : 0 ≤ u) : l * r ≤ popRate l r u := by
  unfold popRate
  have : 0 ≤ (1 - l) * u := mul_nonneg (by linarith) hu
  linarith

/-- Section 5.1.1. `0.086 × 0.166 = 0.014276`, the "1.4%" of the text, and
5.6 times Eckard's 0.25%. -/
theorem lower_bound_value :
    (86 / 1000 : ℝ) * (166 / 1000) = 14276 / 1000000 ∧
      (14 / 1000 : ℝ) / (25 / 10000) = 56 / 10 := by
  norm_num

/-- Section 5.1.1. 30,239 passers in the 10% sample are about 302,390 in the
population. -/
theorem scale_up : (30239 : ℕ) * 10 = 302390 := by norm_num

/-- Appendix F. Adding back the 38% of the population with no linked
counterpart, assumed never to pass, gives `0.16 × (1 − 0.38) = 0.0992`. -/
theorem adjusted_weighted : (16 / 100 : ℝ) * (1 - 38 / 100) = 992 / 10000 := by norm_num

/-- Control. If some unlinked men pass and not everyone is linked, the bound is
strict, so 1.4% is a bound and not an estimate. -/
theorem control_bound_strict {l r u : ℝ} (hl : l < 1) (hu : 0 < u) :
    l * r < popRate l r u := by
  unfold popRate
  have : 0 < (1 - l) * u := mul_pos (by linarith) hu
  linarith

/-- Table 8, column 4, as the text sums it. The passing coefficient, the
interaction with mulatto and the mulatto coefficient sum to 2.331 > 0. -/
theorem table8_col4_sum : (3178 / 1000 : ℝ) - 1301 / 1000 + 454 / 1000 = 2331 / 1000 := by
  norm_num

/-! ### The group left behind (our reading)

A group of mean `m` splits into a share `s` that passes, with mean `mp`, and a
share `1 − s` that remains, with mean `ms`, so `m = s mp + (1 − s) ms`. -/

/-- If those who pass are above the group's mean, those who remain are below
it. This is the step from the selection reported in Table 7 to the composition
of the group left behind. -/
theorem left_behind_lower {s m mp ms : ℝ} (hs0 : 0 < s) (hs1 : s < 1)
    (hm : m = s * mp + (1 - s) * ms) (hp : m < mp) : ms < m := by
  by_contra h
  push_neg at h
  have h1 : s * m < s * mp := mul_lt_mul_of_pos_left hp hs0
  have h2 : (1 - s) * m ≤ (1 - s) * ms := mul_le_mul_of_nonneg_left h (by linarith)
  nlinarith

/-- The mean of those who remain, `ms = (m − s mp)/(1 − s)`. -/
theorem left_behind_mean {s m mp ms : ℝ} (hs1 : s < 1)
    (hm : m = s * mp + (1 - s) * ms) : ms = (m - s * mp) / (1 - s) := by
  have : (1 - s) ≠ 0 := by linarith
  field_simp
  linarith

/-- Control. Without selection, when those who pass have the group's mean,
the mean of those who remain is unchanged. -/
theorem control_no_selection {s m ms : ℝ} (hs1 : s < 1)
    (hm : m = s * m + (1 - s) * ms) : ms = m := by
  have h : (1 - s) * ms = (1 - s) * m := by linarith
  have : (1 - s) ≠ 0 := by linarith
  exact mul_left_cancel₀ this h

end DahisEtAl2019
