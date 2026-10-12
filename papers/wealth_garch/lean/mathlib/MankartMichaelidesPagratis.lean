/-
================================================================================
  Mankart, Michaelides and Pagratis (2020), Bank capital buffers in a dynamic
  model
  Companion to lit/mankart_michaelides_pagratis_2020 (NBER SI draft, May 2016).
================================================================================

With deposits normalised to one and no wholesale funding, total assets are
1 + E and the leverage limit (5), assets over equity at most λu, holds
exactly when E ≥ 1/(λu - 1), the minimum equity of Figure 4. The file proves
that equivalence, maps the limits λw and λu to the ratios of pp. 22 and 34 to
38, proves the smoothing gain of a strictly concave objective (9), and checks
that the model underpredicts mean equity (Table 5). The value function and
the simulated moments are not formalised.
-/

import Mathlib

namespace MankartMichaelidesPagratis

theorem min_equity_iff {E lu : ℝ} (hE : 0 < E) (hl : 1 < lu) :
    (1 + E) / E ≤ lu ↔ 1 / (lu - 1) ≤ E := by
  rw [div_le_iff₀ hE, div_le_iff₀ (by linarith)]
  constructor <;> intro h <;> nlinarith

theorem limits_to_ratios :
    |1 / 0.06 - (16.66 : ℝ)| < 0.01 ∧ 1 / 0.03 - (33.33 : ℝ) < 0.01 ∧ (1 : ℝ) / 25 = 0.04 ∧
      |1 / 41 - (0.0244 : ℝ)| < 0.0001 ∧ |1 / 13 - (0.077 : ℝ)| < 0.001 ∧ (1 : ℝ) / 20 = 0.05 := by
  norm_num [abs_lt]

theorem smoothing_gain {u : ℝ → ℝ} {a b : ℝ} (hu : StrictConcaveOn ℝ (Set.Ioi 0) u)
    (ha : 0 < a) (hb : 0 < b) (hab : a ≠ b) : (u a + u b) / 2 < u ((a + b) / 2) := by
  have h := hu.2 ha hb hab (by norm_num : (0 : ℝ) < 1 / 2) (by norm_num : (0 : ℝ) < 1 / 2) (by norm_num)
  simp only [smul_eq_mul] at h
  have e : (1 / 2 : ℝ) * a + 1 / 2 * b = (a + b) / 2 := by ring
  rw [e] at h
  linarith

theorem equity_underpredicted : (0.065 : ℝ) < 0.072 ∧ (0.067 : ℝ) < 0.099 := by
  norm_num

theorem control_min_equity_needs_limit_above_one {E : ℝ} (hE : 0 < E) : ¬ ((1 + E) / E ≤ 1) := by
  rw [div_le_iff₀ hE]
  linarith

theorem control_linear_objective_not_strictly_concave : ¬ StrictConcaveOn ℝ (Set.Ioi 0) (fun x : ℝ => x) := by
  intro h
  have k := h.2 (show (1 : ℝ) ∈ Set.Ioi 0 by norm_num) (show (2 : ℝ) ∈ Set.Ioi 0 by norm_num) (by norm_num)
    (by norm_num : (0 : ℝ) < 1 / 2) (by norm_num : (0 : ℝ) < 1 / 2) (by norm_num)
  simp only [smul_eq_mul] at k
  linarith

end MankartMichaelidesPagratis
