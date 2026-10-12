/-
================================================================================
  The injection target does not depend on the starting point
  Companion to MANUSCRIPT.tex, row M16.
================================================================================

With a fixed cost K and a linear proportional cost (1 + κ)(y − x), the value of
injecting from x to y is V(y) − (1 + κ)(y − x) − K, which differs from
V(y) − (1 + κ)y by a constant in y. A target that is optimal from one
starting point is optimal from every starting point. With a convex cost
(y − x)² the optimal target moves with x (the control).
-/

import Mathlib

namespace Targeting

theorem target_free_of_start {V : ℝ → ℝ} {kap K x y : ℝ} :
    (∀ z, V z - (1 + kap) * z ≤ V y - (1 + kap) * y) ↔
      (∀ z, V z - (1 + kap) * (z - x) - K ≤ V y - (1 + kap) * (y - x) - K) := by
  constructor
  · intro h z
    have := h z
    linarith
  · intro h z
    have := h z
    linarith

theorem target_same_for_all_starts {V : ℝ → ℝ} {kap K x x' y : ℝ}
    (h : ∀ z, V z - (1 + kap) * (z - x) - K ≤ V y - (1 + kap) * (y - x) - K) :
    ∀ z, V z - (1 + kap) * (z - x') - K ≤ V y - (1 + kap) * (y - x') - K :=
  target_free_of_start.mp (target_free_of_start.mpr h)

theorem control_target_moves_with_convex_cost :
    (∀ z : ℝ, 2 * z - (z - 0) ^ 2 ≤ 2 * 1 - (1 - 0) ^ 2) ∧
      ¬ (∀ z : ℝ, 2 * z - (z - 1) ^ 2 ≤ 2 * 1 - (1 - 1) ^ 2) := by
  constructor
  · intro z
    nlinarith [sq_nonneg (z - 1)]
  · intro h
    have := h 2
    norm_num at this

end Targeting
