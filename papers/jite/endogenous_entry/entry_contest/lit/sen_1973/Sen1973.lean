namespace Sen1973

def RevealedStrict {α : Type} (C : List α → α → Prop) (S : List α) (x y : α) : Prop :=
  C S x ∧ y ∈ S ∧ ¬ C S y

def WeakAxiom {α : Type} (C : List α → α → Prop) : Prop :=
  ∀ S T x y, RevealedStrict C S x y → x ∈ T → ¬ C T y

def NonemptyOn {α : Type} (C : List α → α → Prop) (S : List α) : Prop :=
  ∃ a, a ∈ S ∧ C S a

theorem weak_axiom_forces_transitive_pair {α : Type} (C : List α → α → Prop)
    (x y z : α)
    (hW : WeakAxiom C)
    (hxz : NonemptyOn C [x, z]) (hxyz : NonemptyOn C [x, y, z])
    (h1 : RevealedStrict C [x, y] x y) (h2 : RevealedStrict C [y, z] y z) :
    C [x, z] x ∧ ¬ C [x, z] z := by
  have hz : ¬ C [x, z] z := by
    intro hCz
    obtain ⟨a, ha, hCa⟩ := hxyz
    have ha' : a = x ∨ a = y ∨ a = z := by simpa using ha
    rcases ha' with rfl | rfl | rfl
    · have hz3 : ¬ C [a, y, z] z := hW [y, z] [a, y, z] y z h2 (by simp)
      exact hW [a, y, z] [a, z] a z ⟨hCa, by simp, hz3⟩ (by simp) hCz
    · exact hW [x, a] [x, a, z] x a h1 (by simp) hCa
    · exact hW [y, a] [x, y, a] y a h2 (by simp) hCa
  refine ⟨?_, hz⟩
  obtain ⟨a, ha, hCa⟩ := hxz
  have ha' : a = x ∨ a = z := by simpa using ha
  rcases ha' with rfl | rfl
  · exact hCa
  · exact absurd hCa hz

theorem cyclic_choice_violates_weak_axiom {α : Type} (C : List α → α → Prop)
    (x y z : α)
    (hxz : NonemptyOn C [x, z]) (hxyz : NonemptyOn C [x, y, z])
    (h1 : RevealedStrict C [x, y] x y) (h2 : RevealedStrict C [y, z] y z)
    (h3 : RevealedStrict C [x, z] z x) :
    ¬ WeakAxiom C := by
  intro hW
  have := weak_axiom_forces_transitive_pair C x y z hW hxz hxyz h1 h2
  exact h3.2.2 this.1

inductive Good where
  | x | y | z
  deriving DecidableEq

open Good

def cyc : List Good → Good → Bool
  | [x, y], g => g == x
  | [y, z], g => g == y
  | [x, z], g => g == z
  | _, _ => false

def pairMenus : List (List Good) := [[x, y], [y, z], [x, z]]

def goods : List Good := [x, y, z]

def weakAxiomOnBool (C : List Good → Good → Bool) (D : List (List Good)) : Bool :=
  D.all fun S => D.all fun T => goods.all fun a => goods.all fun b =>
    !(C S a && S.contains b && !C S b && T.contains a && C T b)

theorem cycle_on_pairs_satisfies_weak_axiom : weakAxiomOnBool cyc pairMenus = true := by
  decide

theorem cycle_on_pairs_is_cyclic :
    cyc [x, y] x = true ∧ cyc [x, y] y = false ∧
    cyc [y, z] y = true ∧ cyc [y, z] z = false ∧
    cyc [x, z] z = true ∧ cyc [x, z] x = false := by
  decide

inductive Act where
  | confess | notConfess
  deriving DecidableEq

open Act

def sentence : Act → Act → Int
  | confess, confess => -10
  | confess, notConfess => 0
  | notConfess, confess => -20
  | notConfess, notConfess => -2

theorem confess_strictly_dominant :
    ∀ other : Act, sentence notConfess other < sentence confess other := by
  intro other; cases other <;> decide

theorem mutual_nonconfession_better_for_each :
    sentence confess confess < sentence notConfess notConfess := by
  decide

theorem nonconfession_reveals_no_preferred_outcome :
    ¬ (sentence confess confess < sentence notConfess confess) ∧
    ¬ (sentence confess notConfess < sentence notConfess notConfess) := by
  decide

theorem other_regarding_makes_nonconfession_dominant :
    ∀ other : Act, sentence other confess < sentence other notConfess := by
  intro other; cases other <;> decide

theorem as_if_play_better_in_own_terms :
    sentence confess confess < sentence notConfess notConfess ∧
    (∀ other : Act, sentence notConfess other < sentence confess other) := by
  exact ⟨by decide, confess_strictly_dominant⟩

theorem dilemma_survives_asymmetric_sentences :
    let s : Act → Act → Int := fun me other =>
      match me, other with
      | confess, confess => -7
      | confess, notConfess => -1
      | notConfess, confess => -25
      | notConfess, notConfess => -3
    (∀ other, s notConfess other < s confess other) ∧ s confess confess < s notConfess notConfess := by
  refine ⟨?_, by decide⟩
  intro other; cases other <;> decide

def payoffFrom (t r p s : Int) : Act → Act → Int
  | confess, confess => p
  | confess, notConfess => t
  | notConfess, confess => s
  | notConfess, notConfess => r

theorem dilemma_from_orderings (t r p s : Int) (h1 : s < p) (h2 : p < r) (h3 : r < t) :
    (∀ other : Act, payoffFrom t r p s notConfess other < payoffFrom t r p s confess other) ∧
    payoffFrom t r p s confess confess < payoffFrom t r p s notConfess notConfess := by
  refine ⟨?_, by simp only [payoffFrom]; omega⟩
  intro other; cases other <;> simp only [payoffFrom] <;> omega

theorem control_dilemma_needs_reward_above_punishment :
    ¬ (payoffFrom 0 (-12) (-10) (-20) confess confess <
       payoffFrom 0 (-12) (-10) (-20) notConfess notConfess) := by
  decide

end Sen1973
