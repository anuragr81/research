import Mathlib

/-! Jeffrey, *The Logic of Decision*, second edition (1983), chapter 11,
"Probability Kinematics", in the chapter's notation. `prob` is the old belief
function and `PROB` the new one, on a finite set of outcomes `Ω`. `cell`
assigns each outcome to an atom `A_i` of the set (11-6) in which the change
originates, and `q i` is the new probability `PROB A_i`. -/

namespace Jeffrey1983

open Finset

variable {Ω ι : Type*} [Fintype Ω] [DecidableEq ι]

/-- `prob A_i`, the mass of atom `i`. -/
def cellMass (p : Ω → ℝ) (cell : Ω → ι) (i : ι) : ℝ :=
  ∑ ω, if cell ω = i then p ω else 0

/-- `prob (A A_i)`, the mass of event `A` within atom `i`. -/
def evMass (p : Ω → ℝ) (A : Ω → Prop) [DecidablePred A] (cell : Ω → ι) (i : ι) : ℝ :=
  ∑ ω, if A ω ∧ cell ω = i then p ω else 0

/-- Eq. (11-7) at the level of outcomes. Within each atom the old probabilities
are rescaled so that the atom gets its new probability. -/
noncomputable def kin (p : Ω → ℝ) (cell : Ω → ι) (q : ι → ℝ) (ω : Ω) : ℝ :=
  p ω / cellMass p cell (cell ω) * q (cell ω)

/-- Within an atom, the new outcome probabilities are the old ones times
`q i / prob A_i`. -/
theorem kin_on_cell {p : Ω → ℝ} {cell : Ω → ι} {q : ι → ℝ} {i : ι} {ω : Ω}
    (h : cell ω = i) : kin p cell q ω = q i / cellMass p cell i * p ω := by
  unfold kin; rw [h]; ring

/-- The new belief function gives each atom its new probability,
`PROB A_i = q i`. -/
theorem kin_cellMass {p : Ω → ℝ} {cell : Ω → ι} {q : ι → ℝ} {i : ι}
    (h : cellMass p cell i ≠ 0) : cellMass (kin p cell q) cell i = q i := by
  have e : ∀ ω, (if cell ω = i then kin p cell q ω else 0)
      = q i / cellMass p cell i * (if cell ω = i then p ω else 0) := by
    intro ω
    split_ifs with hc
    · exact kin_on_cell hc
    · ring
  have hs : cellMass (kin p cell q) cell i = q i / cellMass p cell i * cellMass p cell i := by
    show (∑ ω, if cell ω = i then kin p cell q ω else 0)
      = q i / cellMass p cell i * (∑ ω, if cell ω = i then p ω else 0)
    rw [Finset.sum_congr rfl (fun ω _ => e ω), ← Finset.mul_sum]
  rw [hs, div_mul_cancel₀ _ h]

/-- Eq. (11-8), rigidity. The change leaves every probability conditional on
an atom unchanged, `PROB (A A_i) / PROB A_i = prob (A A_i) / prob A_i`. -/
theorem kin_rigid {p : Ω → ℝ} {cell : Ω → ι} {q : ι → ℝ} {i : ι}
    (A : Ω → Prop) [DecidablePred A] (h : cellMass p cell i ≠ 0) (hq : q i ≠ 0) :
    evMass (kin p cell q) A cell i / cellMass (kin p cell q) cell i
      = evMass p A cell i / cellMass p cell i := by
  have e : ∀ ω, (if A ω ∧ cell ω = i then kin p cell q ω else 0)
      = q i / cellMass p cell i * (if A ω ∧ cell ω = i then p ω else 0) := by
    intro ω
    split_ifs with hc
    · exact kin_on_cell hc.2
    · ring
  have hs : evMass (kin p cell q) A cell i = q i / cellMass p cell i * evMass p A cell i := by
    show (∑ ω, if A ω ∧ cell ω = i then kin p cell q ω else 0)
      = q i / cellMass p cell i * (∑ ω, if A ω ∧ cell ω = i then p ω else 0)
    rw [Finset.sum_congr rfl (fun ω _ => e ω), ← Finset.mul_sum]
  rw [kin_cellMass h, hs]
  field_simp

/-- The converse direction of the equivalence of (11-7) and (11-8). A new
belief function that keeps the probabilities within each atom in the old
proportions, and gives atom `i` the probability `q i`, is the one of (11-7). -/
theorem rigid_is_kin {p P : Ω → ℝ} {cell : Ω → ι} {q : ι → ℝ}
    (hp : ∀ i, cellMass p cell i ≠ 0) (hq : ∀ i, cellMass P cell i = q i)
    (hrig : ∀ ω, P ω * cellMass p cell (cell ω) = p ω * cellMass P cell (cell ω)) :
    P = kin p cell q := by
  funext ω
  unfold kin
  rw [← hq (cell ω)]
  have := hp (cell ω)
  field_simp
  linarith [hrig ω]

/-- Eqs. (11-2), (11-3) (a) and (11-5), for an origin in one proposition `B`.
The change in `prob A` is the change in `prob B` times the relevance
`rel (A/B) = prob (A/B) − prob (A/B̄)`. -/
theorem eq_11_5 (pAgB pAgnB pB PB : ℝ) :
    (pAgB * PB + pAgnB * (1 - PB)) - (pAgB * pB + pAgnB * (1 - pB))
      = (PB - pB) * (pAgB - pAgnB) := by
  ring

/-- Examples 3 and 4, the mudrunner. `(.8)(.6) + (.1)(.4) = .52`,
`(.8)(.3) + (.1)(.7) = .31`, and the increase `.21` is seven tenths of `.3`. -/
theorem mudrunner :
    (8 / 10 : ℝ) * (6 / 10) + 1 / 10 * (4 / 10) = 52 / 100 ∧
      (8 / 10 : ℝ) * (3 / 10) + 1 / 10 * (7 / 10) = 31 / 100 ∧
      (52 / 100 : ℝ) - 31 / 100 = 7 / 10 * (6 / 10 - 3 / 10) := by
  norm_num

/-! ### Two applications need not commute

Outcomes `(B,C), (B,C̄), (B̄,C), (B̄,C̄)` with old probabilities
`.4, .1, .1, .4`. Setting `PROB B = .9` and then `PROB C = .2` gives a different
belief function from setting them in the opposite order, as the chapter says
on pp. 172 and 182. -/

/-- The old belief function of the example. -/
noncomputable def p0 : Fin 4 → ℝ := ![4 / 10, 1 / 10, 1 / 10, 4 / 10]

/-- The atoms `B, B̄` and `C, C̄`. -/
def cellB : Fin 4 → Fin 2 := ![0, 0, 1, 1]
def cellC : Fin 4 → Fin 2 := ![0, 1, 0, 1]

/-- The new probabilities `PROB B = .9` and `PROB C = .2`. -/
noncomputable def qB : Fin 2 → ℝ := ![9 / 10, 1 / 10]
noncomputable def qC : Fin 2 → ℝ := ![2 / 10, 8 / 10]

theorem order_BC : kin (kin p0 cellB qB) cellC qC 0 = 36 / 185 := by
  simp [kin, cellMass, Fin.sum_univ_four, p0, cellB, cellC, qB, qC]
  norm_num

theorem order_CB : kin (kin p0 cellC qC) cellB qB 0 = 9 / 20 := by
  simp [kin, cellMass, Fin.sum_univ_four, p0, cellB, cellC, qB, qC]
  norm_num

/-- Control: the order of two soft readings matters in general. -/
theorem control_not_commutative :
    kin (kin p0 cellB qB) cellC qC ≠ kin (kin p0 cellC qC) cellB qB := by
  intro h
  have := congrFun h 0
  rw [order_BC, order_CB] at this
  norm_num at this

/-- Control: rigidity fails without the rescaling. Moving atom `B` to `.9` by
adding the whole change to one outcome changes `PROB (C/B)`. -/
theorem control_rigidity_needed :
    let P : Fin 4 → ℝ := ![8 / 10, 1 / 10, 1 / 20, 1 / 20]
    P 0 / (P 0 + P 1) ≠ p0 0 / (p0 0 + p0 1) := by
  simp [p0]
  norm_num

end Jeffrey1983
