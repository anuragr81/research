import Mathlib

namespace LangMenno

theorem cbr_is_min_plus_ccob : (10 : ℚ) / 100 + 25 / 1000 = 125 / 1000 := by norm_num

theorem us_floor_same_construction : (8 : ℚ) / 100 + 25 / 1000 = 105 / 1000 := by norm_num

theorem control_cbr_needs_buffer : ¬ ((10 : ℚ) / 100 + 0 = 125 / 1000) := by norm_num

theorem control_us_floor_not_ecb : ¬ ((105 : ℚ) / 1000 = 125 / 1000) := by norm_num

/-- Lang and Menno's stigma charge per unit of loans, ξ·1{CR < R}. -/
noncomputable def stigma (ξ R x : ℝ) : ℝ := if x < R then ξ else 0

/-- The graduated shortfall penalty of the theory paper, (λ_S - 1)(R - x)^+. -/
noncomputable def graduated (lam R x : ℝ) : ℝ := (lam - 1) * max (R - x) 0

theorem stigma_flat_below {ξ R x y : ℝ} (hx : x < R) (hy : y < R) :
    stigma ξ R x = stigma ξ R y := by
  simp [stigma, hx, hy]

theorem stigma_zero_at_or_above {ξ R x : ℝ} (hx : R ≤ x) : stigma ξ R x = 0 := by
  simp [stigma, not_lt.mpr hx]

theorem graduated_grows_below {lam R x y : ℝ} (hl : 1 < lam) (hxy : x < y) (hy : y ≤ R) :
    graduated lam R y < graduated lam R x := by
  unfold graduated
  rw [max_eq_left (by linarith), max_eq_left (by linarith)]
  nlinarith

theorem control_graduated_flat_at_one {R x y : ℝ} :
    graduated 1 R x = graduated 1 R y := by
  simp [graduated]

end LangMenno
