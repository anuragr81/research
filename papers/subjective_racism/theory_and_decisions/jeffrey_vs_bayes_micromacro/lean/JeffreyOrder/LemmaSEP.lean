/-
# Lemma SEP (separability of attribute-local composites), Section 5.2.1

  For any number `N` of binary attributes and a joint prior `P` over `{0,1}ᴺ`,
  any finite sequence of attribute-local updates produces a *separable*
  reweighting

      `P'(x) = P(x) · ∏_{a} g_a(x_a)`

  for some single-attribute functions `g_a`.  Equivalently, `log(P'/P)`
  contains no interaction terms at any level.

The appendix proof uses exactly one property of a Jeffrey step: the factor it
multiplies by is a function of the single coordinate it reads -- state-dependent
only through a normaliser that is itself a function of that coordinate alone.
That is precisely what `Step` below records, so the theorem here holds for any
attribute-local rule, not for Jeffrey conditioning in particular, and at every
`N` and every sequence length.  Nothing in the proof is specific to `N = 2`.
-/
import Mathlib.Algebra.BigOperators.Fin
import Mathlib.Data.Real.Basic
import Mathlib.Tactic.FieldSimp
import Mathlib.Tactic.Ring

namespace JeffreyOrder

open Finset

variable {N : ℕ}

/-- A state of `N` binary attributes. -/
abbrev State (N : ℕ) := Fin N → Bool

/-- An attribute-local update: the attribute it reads, and the factor it
multiplies each cell by -- a function of that single coordinate alone.  A
Jeffrey step is of this form, with `factor b = q b / P(A_a = b)`. -/
structure Step (N : ℕ) where
  /-- the attribute the update reads -/
  attr : Fin N
  /-- the multiplicative factor, a function of that attribute's value alone -/
  factor : Bool → ℝ

/-- One attribute-local update. -/
def applyStep (P : State N → ℝ) (s : Step N) : State N → ℝ :=
  fun x => P x * s.factor (x s.attr)

/-- A finite sequence of attribute-local updates. -/
def applySteps (P : State N → ℝ) : List (Step N) → State N → ℝ
  | [] => P
  | s :: l => applySteps (applyStep P s) l

/-- `P'` is a *separable* reweighting of `P`. -/
def IsSeparable (P P' : State N → ℝ) : Prop :=
  ∃ g : Fin N → Bool → ℝ, ∀ x, P' x = P x * ∏ a, g a (x a)

/-- Multiplying one factor of a product by `k` multiplies the product by `k`. -/
theorem prod_update_mul (i : Fin N) (f : Fin N → ℝ) (k : ℝ) :
    ∏ a, (Function.update f i (f i * k)) a = (∏ a, f a) * k := by
  rw [Finset.prod_update_of_mem (Finset.mem_univ i),
    ← Finset.mul_prod_erase Finset.univ f (Finset.mem_univ i),
    Finset.sdiff_singleton_eq_erase]
  ring

/-- **Lemma SEP.**  Any finite sequence of attribute-local updates produces a
separable reweighting.  The proof is the appendix's induction on the update
sequence: each step folds into its own attribute's factor without ever coupling
two coordinates. -/
theorem isSeparable_applySteps (P : State N → ℝ) (l : List (Step N)) :
    IsSeparable P (applySteps P l) := by
  induction l generalizing P with
  | nil => exact ⟨fun _ _ => 1, by intro x; simp [applySteps]⟩
  | cons s l ih =>
      obtain ⟨g, hg⟩ := ih (applyStep P s)
      refine ⟨Function.update g s.attr (fun b => g s.attr b * s.factor b), ?_⟩
      intro x
      have hfun :
          (fun a => (Function.update g s.attr (fun b => g s.attr b * s.factor b)) a (x a))
            = Function.update (fun a => g a (x a)) s.attr
                (g s.attr (x s.attr) * s.factor (x s.attr)) := by
        funext a
        by_cases h : a = s.attr
        · subst h; simp
        · simp [Function.update_of_ne h]
      calc applySteps P (s :: l) x
          = applySteps (applyStep P s) l x := rfl
        _ = (applyStep P s) x * ∏ a, g a (x a) := hg x
        _ = P x * ((∏ a, g a (x a)) * s.factor (x s.attr)) := by
              simp only [applyStep]; ring
        _ = P x * ∏ a, (Function.update g s.attr
              (fun b => g s.attr b * s.factor b)) a (x a) := by
              rw [hfun, prod_update_mul]

/-! ### The equivalent formulation: no interactions of order ≥ 2

The appendix's second formulation is that `log(P'/P)` carries no interaction
terms.  Multiplicatively -- which avoids logarithms and any positivity
hypothesis -- that says every alternating product of `P'/P` over a sub-cube of
two or more coordinates equals one.  We prove the pairwise case, from which the
higher ones telescope exactly as the appendix describes.
-/

/-- Set two distinct coordinates of a state. -/
def set2 (x : State N) (i j : Fin N) (b d : Bool) : State N :=
  Function.update (Function.update x i b) j d

theorem set2_apply_of_ne {x : State N} {i j : Fin N} {b d : Bool} {a : Fin N}
    (hi : a ≠ i) (hj : a ≠ j) : set2 x i j b d a = x a := by
  simp [set2, Function.update_of_ne hi, Function.update_of_ne hj]

theorem set2_apply_left {x : State N} {i j : Fin N} {b d : Bool} (hij : i ≠ j) :
    set2 x i j b d i = b := by
  simp [set2, Function.update_of_ne hij]

theorem set2_apply_right {x : State N} {i j : Fin N} {b d : Bool} :
    set2 x i j b d j = d := by
  simp [set2]

/-- A separable weight splits off any two distinct coordinates. -/
theorem prod_split_two (g : Fin N → Bool → ℝ) (y : State N) {i j : Fin N} (hij : i ≠ j) :
    ∏ a, g a (y a)
      = g i (y i) * g j (y j) * ∏ a ∈ (Finset.univ.erase i).erase j, g a (y a) := by
  have hj : j ∈ Finset.univ.erase i := Finset.mem_erase.mpr ⟨(Ne.symm hij), Finset.mem_univ j⟩
  rw [← Finset.mul_prod_erase Finset.univ (fun a => g a (y a)) (Finset.mem_univ i),
    ← Finset.mul_prod_erase (Finset.univ.erase i) (fun a => g a (y a)) hj]
  ring

/-- **No pairwise interaction.**  For a separable weight, the alternating
product over any two distinct coordinates is trivial:
`F(1,1)·F(0,0) = F(1,0)·F(0,1)`.  Equivalently `Δ_{ij} log F = 0`. -/
theorem sep_no_pair_interaction (g : Fin N → Bool → ℝ) (x : State N)
    {i j : Fin N} (hij : i ≠ j) :
    (∏ a, g a (set2 x i j true true a)) * (∏ a, g a (set2 x i j false false a))
      = (∏ a, g a (set2 x i j true false a)) * (∏ a, g a (set2 x i j false true a)) := by
  have hrest : ∀ b d : Bool,
      ∏ a ∈ (Finset.univ.erase i).erase j, g a (set2 x i j b d a)
        = ∏ a ∈ (Finset.univ.erase i).erase j, g a (x a) := by
    intro b d
    refine Finset.prod_congr rfl ?_
    intro a ha
    have haj : a ≠ j := (Finset.mem_erase.mp ha).1
    have hai : a ≠ i := (Finset.mem_erase.mp (Finset.mem_erase.mp ha).2).1
    rw [set2_apply_of_ne hai haj]
  simp only [prod_split_two g _ hij, set2_apply_left hij, set2_apply_right, hrest]
  ring

/-- The same statement for the composite of an arbitrary sequence of
attribute-local updates: the ratio `P'/P` has no pairwise interaction, in the
multiplicative form `R(1,1)·R(0,0) = R(1,0)·R(0,1)`. -/
theorem applySteps_no_pair_interaction (P : State N → ℝ) (l : List (Step N))
    (x : State N) {i j : Fin N} (hij : i ≠ j)
    (hP : ∀ y, P y ≠ 0) :
    (applySteps P l (set2 x i j true true) / P (set2 x i j true true))
      * (applySteps P l (set2 x i j false false) / P (set2 x i j false false))
    = (applySteps P l (set2 x i j true false) / P (set2 x i j true false))
      * (applySteps P l (set2 x i j false true) / P (set2 x i j false true)) := by
  obtain ⟨g, hg⟩ := isSeparable_applySteps P l
  have h : ∀ y : State N, applySteps P l y / P y = ∏ a, g a (y a) := by
    intro y
    have hy := hP y
    rw [hg y]
    field_simp
  simp only [h]
  exact sep_no_pair_interaction g x hij

/-- **Separability is exactly what Proposition DEC needs at `N = 2`:** a
separable reweighting rescales the association by `g₀g₁h₀h₁` and can never
shift it.  (Stated here for the `N`-attribute machinery specialised to the two
coordinates `i ≠ j`; the `N = 2` matrix form is `sep_assoc` in `PropDEC.lean`.) -/
theorem sep_rescales_association (g : Fin N → Bool → ℝ) (P : State N → ℝ)
    (x : State N) {i j : Fin N} (hij : i ≠ j)
    (F : State N → ℝ) (hF : ∀ y, F y = P y * ∏ a, g a (y a)) :
    F (set2 x i j true true) * F (set2 x i j false false)
        - F (set2 x i j true false) * F (set2 x i j false true)
      = (g i true * g i false * g j true * g j false)
          * (∏ a ∈ (Finset.univ.erase i).erase j, g a (x a)) ^ 2
          * (P (set2 x i j true true) * P (set2 x i j false false)
             - P (set2 x i j true false) * P (set2 x i j false true)) := by
  have hrest : ∀ b d : Bool,
      ∏ a ∈ (Finset.univ.erase i).erase j, g a (set2 x i j b d a)
        = ∏ a ∈ (Finset.univ.erase i).erase j, g a (x a) := by
    intro b d
    refine Finset.prod_congr rfl ?_
    intro a ha
    have haj : a ≠ j := (Finset.mem_erase.mp ha).1
    have hai : a ≠ i := (Finset.mem_erase.mp (Finset.mem_erase.mp ha).2).1
    rw [set2_apply_of_ne hai haj]
  simp only [hF, prod_split_two g _ hij, set2_apply_left hij, set2_apply_right, hrest]
  ring

end JeffreyOrder
