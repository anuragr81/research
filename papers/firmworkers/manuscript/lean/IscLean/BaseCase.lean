import IscLean.Mobility
import IscLean.Reassignment
import Mathlib.Tactic

/-!
The base case: exactly two displaced workers choose the lottery. Then the reassignment of
M36 is the exchange of their jobs, closure cannot act, and the lottery is a sure transfer of
income from the richer to the poorer entrant, less the two lottery costs. The preferences
that make both accept the exchange are in `Mobility.lean`.
-/

open Finset

namespace Isc

variable {ι : Type*} [Fintype ι] [DecidableEq ι]

/-- With two entrants, every reassignment in which nobody keeps their own former job is the
exchange. -/
theorem two_entrant_exchange {i j : ι} (hij : i ≠ j) (hall : ∀ x, x = i ∨ x = j)
    (π : Equiv.Perm ι) (hπ : ∀ x, π x ≠ x) : π i = j ∧ π j = i := by
  constructor
  · rcases hall (π i) with h | h
    · exact absurd h (hπ i)
    · exact h
  · rcases hall (π j) with h | h
    · exact h
    · exact absurd h (hπ j)

/-- Control: with three entrants a reassignment need not exchange two of them. The rotation of
three sends `0` to `1` but `1` to `2`, not back to `0`. -/
theorem control_exchange_needs_two :
    (∀ x : Fin 3, finRotate 3 x ≠ x) ∧ finRotate 3 (0 : Fin 3) = 1 ∧ finRotate 3 (1 : Fin 3) ≠ 0 := by
  decide

/-- With two entrants the closure-weighted chance that each receives the other's former job
is one, whatever the closure parameter below one and whether or not they share a status
group. -/
theorem two_entrant_closure_inert {i j : ι} (hij : i ≠ j) (hall : ∀ x, x = i ∨ x = j)
    {σ : ℝ} (hσ : σ < 1) (g : ι → Bool) : closureChance σ g i j = 1 := by
  have herase : univ.erase i = {j} := by
    ext x
    simp only [mem_erase, mem_univ, and_true, mem_singleton]
    constructor
    · intro hx
      rcases hall x with h | h
      · exact absurd h hx
      · exact h
    · intro hx
      rw [hx]
      exact hij.symm
  unfold closureChance
  rw [if_neg hij.symm, herase, sum_singleton]
  have hpos : 0 < crossWeight σ g i j := by
    unfold crossWeight
    split_ifs
    · norm_num
    · linarith
  exact div_self hpos.ne'

/-- Control: with three entrants closure acts. Entrants `0` and `1` in one group and `2` in
the other, at `σ = 1/2`: entrant `0` receives the former job of `2` with chance `1/3`, not
`1/2`. -/
theorem control_closure_acts_with_three :
    closureChance (1 / 2) (fun k : Fin 3 => decide (k.val < 2)) 0 2 = 1 / 3 := by
  unfold closureChance crossWeight
  rw [if_neg (by decide)]
  rw [show (univ : Finset (Fin 3)).erase 0 = {1, 2} by decide]
  rw [sum_pair (by decide)]
  simp
  norm_num

/-- The exchange is zero-sum in income and costs the pair twice the lottery cost. -/
theorem exchange_costs_the_pair {xi xj L : ℝ} :
    ((xj - L) - xi) + ((xi - L) - xj) = -(2 * L) := by
  ring

/-- The richer entrant always loses income in the exchange, and the poorer gains exactly when
the lottery cost is below the income gap. -/
theorem exchange_moves_income_to_poorer {xi xj L : ℝ} (hij : xi < xj) (hL : 0 ≤ L) :
    (xi - L) - xj < 0 ∧ (0 < (xj - L) - xi ↔ L < xj - xi) := by
  constructor
  · linarith
  · constructor <;> intro h <;> linarith

/-- Control: at a lottery cost equal to the income gap the poorer entrant gains nothing. -/
theorem control_poorer_gains_nothing_at_gap {xi xj : ℝ} :
    (xj - (xj - xi)) - xi = 0 := by
  ring

/-- Two entrants in one status group and one in the other: every reassignment in which nobody
keeps their own former job moves exactly two of the three jobs across the groups, so no rule
for weighting reassignments can let closure change how many jobs cross. -/
theorem two_one_pool_forces_crossing : ∀ π : Equiv.Perm (Fin 3), (∀ x, π x ≠ x) →
    (univ.filter (fun x : Fin 3 => decide (x.val < 2) ≠ decide ((π x).val < 2))).card = 2 := by
  decide

/-- Control: with two entrants in each group a reassignment exists in which no job crosses the
groups, so closure can act. -/
theorem control_two_two_pool_allows_no_crossing : ∃ π : Equiv.Perm (Fin 4), (∀ x, π x ≠ x) ∧
    (univ.filter (fun x : Fin 4 => decide (x.val < 2) ≠ decide ((π x).val < 2))).card = 0 :=
  ⟨Equiv.swap 0 1 * Equiv.swap 2 3, by decide⟩

/-! ### A pool of one (issue 14) -/

/-- If a lone entrant got their own former job back at the lottery cost, the lottery would beat
the return exactly when the lottery cost is below the upskilling cost. Both are paid once, so
the comparison is the same in lifetime value. -/
theorem lone_entrant_cheap_lottery_iff {xi L C : ℝ} : xi - C < xi - L ↔ L < C := by
  constructor <;> intro h <;> linarith

/-- The firm's receipt differs by the whole upskilling cost between the two routes: `C` on the
return, nothing from the lottery, whose cost leaks out of the firm. -/
theorem lone_entrant_firm_receipt_gap {C : ℝ} : C - 0 = C := by
  ring

/-- Control: at `L = C` the lone entrant is indifferent. -/
theorem control_lone_entrant_indifferent_at_equal_costs {xi C : ℝ} : xi - C = xi - C := rfl

end Isc
