import IscLean.Ladder
import Mathlib.Analysis.SpecialFunctions.Log.Basic

namespace Isc

theorem pareto_at_zero (x0 a : ℝ) : pareto x0 a 0 = x0 := by
  simp [pareto]

theorem quantile_from_wage {x0 a q : ℝ} (hx0 : 0 < x0) (ha : 0 < a) (hq : q < 1) :
    q = 1 - (pareto x0 a q / x0) ^ (-a) := by
  unfold pareto
  have h1 : 0 < 1 - q := by linarith
  have e : x0 * (1 - q) ^ (-(1 / a)) / x0 = (1 - q) ^ (-(1 / a)) := by
    field_simp
  rw [e, ← Real.rpow_mul h1.le]
  have h2 : -(1 / a) * -a = 1 := by
    field_simp
  rw [h2, Real.rpow_one]
  ring

theorem log_survival_linear {x0 a q : ℝ} (hx0 : 0 < x0) (ha : 0 < a) (hq : q < 1) :
    Real.log (1 - q) = -a * (Real.log (pareto x0 a q) - Real.log x0) := by
  unfold pareto
  have h1 : 0 < 1 - q := by linarith
  rw [Real.log_mul hx0.ne' (Real.rpow_pos_of_pos h1 _).ne', Real.log_rpow h1]
  field_simp
  ring

theorem control_quantile_needs_positive_tail : pareto 1 0 (1 / 2) = pareto 1 0 0 := by
  simp [pareto]

theorem cost_exponent_log_linear {d b : ℝ} (hd : 0 < d) : Real.log (d ^ b) = b * Real.log d :=
  Real.log_rpow hd b

theorem risk_scale_identified {σy s lam m : ℝ} (h : (1 - lam) * m ≠ 0)
    (hσ : σy = s * (1 - lam) * m) : s = σy / ((1 - lam) * m) := by
  rw [hσ]
  field_simp
  ring

noncomputable def crossShare (σ n m : ℝ) : ℝ := (1 - σ) * m / (n + (1 - σ) * m)

theorem crossShare_strictAnti {n m σ₁ σ₂ : ℝ} (hn : 0 < n) (hm : 0 < m) (h12 : σ₁ < σ₂)
    (h2 : σ₂ ≤ 1) : crossShare σ₂ n m < crossShare σ₁ n m := by
  unfold crossShare
  have hd₁ : 0 < n + (1 - σ₁) * m := by nlinarith
  have hd₂ : 0 < n + (1 - σ₂) * m := by nlinarith
  rw [div_lt_div_iff₀ hd₂ hd₁]
  nlinarith [mul_pos (mul_pos hm hn) (sub_pos.mpr h12)]

theorem closure_from_crossShare {n m σ : ℝ} (hn : 0 < n) (hm : 0 < m) (hσ1 : σ ≤ 1) :
    σ = 1 - n * crossShare σ n m / (m * (1 - crossShare σ n m)) := by
  unfold crossShare
  have hd : 0 < n + (1 - σ) * m := by nlinarith
  have hone : 1 - (1 - σ) * m / (n + (1 - σ) * m) = n / (n + (1 - σ) * m) := by
    field_simp
  rw [hone]
  field_simp
  ring

theorem control_closure_needs_outgroup_entrants (n σ₁ σ₂ : ℝ) :
    crossShare σ₁ n 0 = crossShare σ₂ n 0 := by
  simp [crossShare]

end Isc
