import Mathlib

/-! Arrow (1973), read in the 1971 working-paper version, Section 4
("Imperfect Information"), in the paper's notation. `pW` and `pB` are the
employer's probabilities that a W or B worker is qualified, `M` is `MP_S`,
the marginal product of a qualified skilled worker, `wW` and `wB` the skilled
wages, and `r` the required return on the personnel investment. -/

namespace Arrow1973

/-- Eqs. (13) and (14). Equal required returns give
`w_W = q w_B + (1 − q) MP_S` with `q = p_B/p_W`. -/
theorem wage_eq14 {pW pB M wW wB r : ℝ} (hW : 0 < pW)
    (hrW : r = (M - wW) * pW) (hrB : r = (M - wB) * pB) :
    wW = pB / pW * wB + (1 - pB / pW) * M := by
  have h : (M - wW) * pW = (M - wB) * pB := by rw [← hrW, ← hrB]
  field_simp
  linarith

/-- Eq. (14). With `p_B < p_W` and `w_B < MP_S`, `w_W > w_B`. The belief gap
becomes a wage gap. -/
theorem wage_differential {pW pB M wW wB r : ℝ} (hB : 0 < pB) (hBW : pB < pW)
    (hwB : wB < M) (hrW : r = (M - wW) * pW) (hrB : r = (M - wB) * pB) :
    wB < wW := by
  have hW : 0 < pW := by linarith
  have h14 := wage_eq14 hW hrW hrB
  have hq : pB / pW < 1 := (div_lt_one hW).2 hBW
  have : 0 < (1 - pB / pW) * (M - wB) := mul_pos (by linarith) (by linarith)
  nlinarith

/-- Control: with equal beliefs there is no wage gap. -/
theorem control_equal_beliefs {p M wW wB r : ℝ} (hp : 0 < p)
    (hrW : r = (M - wW) * p) (hrB : r = (M - wB) * p) : wW = wB := by
  have : (M - wW) * p = (M - wB) * p := by rw [← hrW, ← hrB]
  have := mul_right_cancel₀ hp.ne' this
  linarith

/-- The difference mode of the Marshallian dynamics near the symmetric
equilibrium, our derivation. With `w_i = MP_S − r/p_i` and
`dp_i/dt = k [S(w_i − w_U) − p_i]`, the gap `p_W − p_B` grows at rate
`k (S′ r/p² − 1)`. With `S′ = E p / v`, `r/p = MP_S − w_S` and
`v = w_S − w_U`, the coefficient `S′ r/p²` is the left side of Arrow's
stability condition `E (MP_S − w_S)/(w_S − w_U) < 1`. -/
theorem diff_mode_coeff {E p v M w r : ℝ} (hp : 0 < p) (hv : 0 < v)
    (hr : r / p = M - w) :
    (E * p / v) * r / p ^ 2 = E * (M - w) / v := by
  rw [← hr]
  field_simp

/-- The gap in wages that a gap in beliefs produces, to first order, is
`(r/p²)` times the belief gap, from `w_i = MP_S − r/p_i`. -/
theorem wage_gap_of_belief_gap {M r pW pB : ℝ} (hW : 0 < pW) (hB : 0 < pB) :
    (M - r / pW) - (M - r / pB) = r * (pW - pB) / (pW * pB) := by
  field_simp
  ring

/-- Control: without a personnel investment cost, `r = 0`, the coefficient is
0, below 1, so the symmetric equilibrium is stable, as the paper says. -/
theorem control_no_personnel_cost {E p v : ℝ} :
    (E * p / v) * 0 / p ^ 2 = 0 := by
  simp

end Arrow1973
