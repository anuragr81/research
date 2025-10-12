import Mathlib.Algebra.GroupWithZero.Basic
import Mathlib.Tactic.Ring

lemma binomial_solution (x : ℤ) : x ^ 2 - 2 * x + 1 = 0 ↔ x = 1 := by
  have one_is_solution : x = 1 → x ^ 2 - 2 * x + 1 = 0 := by
    intro x_is_1
    rw [x_is_1]
    ring
  have solution_is_one : x ^ 2 - 2 * x + 1 = 0 → x = 1 := by
    intro x_is_solution
    have xminus1_squared_is_0 : (x - 1) ^ 2 = 0 := by
      calc
        (x - 1) ^ 2 = x ^ 2 - 2 * x + 1 := by ring_nf
        _ = 0 := x_is_solution
    have xminus1_is_0 : x - 1 = 0 := pow_eq_zero xminus1_squared_is_0
    calc
      x = x - 1 + 1 := by ring
      _ = 1 := by rw [xminus1_is_0]; ring
  exact ⟨solution_is_one, one_is_solution⟩

