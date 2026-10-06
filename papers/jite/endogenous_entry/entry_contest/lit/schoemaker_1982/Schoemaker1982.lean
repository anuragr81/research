namespace Schoemaker1982

def eu {α : Type} (U : α → Int) : List (Int × α) → Int
  | [] => 0
  | (w, o) :: L => w * U o + eu U L

def wsum {α : Type} : List (Int × α) → Int
  | [] => 0
  | (w, _) :: L => w + wsum L

theorem eu_affine {α : Type} (U : α → Int) (a b : Int) (L : List (Int × α)) :
    eu (fun o => a * U o + b) L = a * eu U L + b * wsum L := by
  induction L with
  | nil => simp [eu, wsum]
  | cons p L ih =>
    obtain ⟨w, o⟩ := p
    simp only [eu, wsum, ih]
    rw [Int.mul_add w (a * U o) b, Int.mul_add b w (wsum L), Int.mul_add a (w * U o) (eu U L),
      Int.mul_left_comm w a (U o),
      Int.mul_comm w b]
    omega

theorem affine_preserves_ranking {α : Type} (U : α → Int) (a b : Int) (ha : 0 < a)
    (L1 L2 : List (Int × α)) (hw : wsum L1 = wsum L2) :
    eu U L1 < eu U L2 ↔ eu (fun o => a * U o + b) L1 < eu (fun o => a * U o + b) L2 := by
  rw [eu_affine, eu_affine, hw, Int.add_lt_add_iff_right, Int.mul_lt_mul_left ha]

theorem affine_preserves_difference_order (a b u1 u2 u3 u4 : Int) (ha : 0 < a) :
    u1 - u2 < u3 - u4 ↔ (a * u1 + b) - (a * u2 + b) < (a * u3 + b) - (a * u4 + b) := by
  have e1 : (a * u1 + b) - (a * u2 + b) = a * (u1 - u2) := by rw [Int.mul_sub]; omega
  have e2 : (a * u3 + b) - (a * u4 + b) = a * (u3 - u4) := by rw [Int.mul_sub]; omega
  rw [e1, e2, Int.mul_lt_mul_left ha]

theorem sure_outcomes_ordinal (f : Int → Int) (hf : ∀ s t, s < t → f s < f t) (s t : Int) :
    s < t ↔ f s < f t := by
  constructor
  · exact hf s t
  · intro h
    by_cases hst : s < t
    · exact hst
    · have : t ≤ s := Int.not_lt.mp hst
      rcases Int.lt_or_eq_of_le this with hlt | heq
      · exact absurd (hf t s hlt) (Int.not_lt.mpr (Int.le_of_lt h))
      · subst heq; exact absurd h (Int.lt_irrefl _)

def stretch : Int → Int
  | 0 => 0
  | 3 => 3
  | 5 => 10
  | n => n

def lotA : List (Int × Int) := [(1, 0), (1, 5)]

def lotB : List (Int × Int) := [(2, 3)]

theorem control_monotone_nonaffine_reverses_ranking :
    eu id lotA < eu id lotB ∧ eu stretch lotB < eu stretch lotA := by
  decide

theorem control_stretch_monotone_on_support :
    stretch 0 < stretch 3 ∧ stretch 3 < stretch 5 := by
  decide

theorem control_stretch_not_affine_on_support :
    ∀ a b : Int, ¬ (a * 0 + b = stretch 0 ∧ a * 3 + b = stretch 3 ∧ a * 5 + b = stretch 5) := by
  intro a b h
  simp only [stretch] at h
  omega

theorem control_monotone_nonaffine_reverses_difference_order :
    (5 : Int) - 3 < 3 - 0 ∧ stretch 3 - stretch 0 < stretch 5 - stretch 3 := by
  decide

def lotC : List (Int × Int) := [(1, 1)]

def lotD : List (Int × Int) := [(2, 0)]

theorem control_unequal_total_weight_breaks_invariance :
    eu id lotD < eu id lotC ∧
    eu (fun o => 1 * id o + 5) lotC < eu (fun o => 1 * id o + 5) lotD := by
  decide

end Schoemaker1982
