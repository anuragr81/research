import Mathlib

namespace Assumptions

open Finset

/-! The plain-English readings of the assumptions in the manuscript's table
of assumptions. Where a reading restates algebra, the statement is checked
here. -/

/-- A4. A likelihood constant in class, `L c = k`, under a prior that sums to
one, has marginal `∑ L c p c = k`. -/
theorem marginal_of_flat {C : Type*} [Fintype C] {p L : C → ℝ} {k : ℝ}
    (hL : ∀ c, L c = k) (hp : ∑ c, p c = 1) : ∑ c, L c * p c = k := by
  simp only [hL, ← Finset.mul_sum, hp, mul_one]

end Assumptions
