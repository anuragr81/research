/-
================================================================================
  Fries, Mella-Barral and Perraudin (1997): the facts the theory paper's rows use
  Written for the theory paper on 30 October 2026, with the record
  theory/lit/fries_mellabarral_perraudin_1997/.
================================================================================

1. Their equity-maximising closure point (11) is k* = (-A/(1-A)) times the cash flow at
   which net assets are zero, with A the negative root of their characteristic equation.
   For A < 0 the factor lies strictly between 0 and 1, so equity holders stop injecting
   capital while the bank's net assets are still positive (p. 453).
2. Their moral hazard (Definition 1, p. 459) is equity value rising in volatility. Noise
   added to the state leaves a value linear in it unchanged and raises a convex one: for a
   symmetric two-point shock ±e, the average of a linear value is unchanged and the
   average of k ↦ k² rises by e².
-/

import Mathlib

namespace FriesEtAl

/-- For a negative root A, the factor -A/(1-A) of their closure rule (11) is in (0, 1). -/
theorem closure_factor_in_unit {A : ℝ} (hA : A < 0) : 0 < -A / (1 - A) ∧ -A / (1 - A) < 1 := by
  have h1 : 0 < 1 - A := by linarith
  refine ⟨div_pos (by linarith) h1, ?_⟩
  rw [div_lt_one h1]
  linarith

/-- A value linear in the state is unchanged by a symmetric shock. -/
theorem linear_no_gain_from_noise (a b k e : ℝ) :
    ((a * (k + e) + b) + (a * (k - e) + b)) / 2 = a * k + b := by
  ring

/-- A convex value gains from a symmetric shock: for k ↦ k², by e². -/
theorem convex_gains_from_noise (k e : ℝ) : ((k + e) ^ 2 + (k - e) ^ 2) / 2 = k ^ 2 + e ^ 2 := by
  ring

/-- Control. With A > 0 the factor is negative (A = 1/2 gives -1), so the closure point
below zero net assets needs the negative root. -/
theorem control_factor_needs_negative_root : -(1 / 2 : ℝ) / (1 - 1 / 2) = -1 := by
  norm_num

/-- Control. With no shock the convex value does not gain. -/
theorem control_gain_needs_noise (k : ℝ) : ((k + 0) ^ 2 + (k - 0) ^ 2) / 2 = k ^ 2 := by
  ring

end FriesEtAl
