/-
# Factor inputs with the second factor weighted as `a^ω`

Exact zero set of the sequence effect on the `A`-marginal at `c = 0` when each cue
supplies a factor on its own attribute (the Bayes-factor reading) and the factor
read second is adopted in part, instantiated as `a^ω`.  Complements
`Ladder.lean`'s `factor_mA1_gap_iff`, which holds for an arbitrary weighted
factor `a'`.  Needs real powers and logarithms, hence its own file.
-/
import JeffreyOrder.Ladder
import Mathlib.Analysis.SpecialFunctions.Pow.Real

namespace JeffreyOrder

variable {α q₀ : ℝ}

/-! ### Factor inputs with the second factor weighted as `a^ω` -/

/-- `a₁ a₀^ω = a₀ a₁^ω` for positive factors exactly when `ω = 1` or `a₀ = a₁`. -/
theorem pow_factor_ratio_iff {a₀ a₁ ω : ℝ} (h0 : 0 < a₀) (h1 : 0 < a₁) :
    a₁ * a₀ ^ ω = a₀ * a₁ ^ ω ↔ ω = 1 ∨ a₀ = a₁ := by
  have p0 : 0 < a₀ ^ ω := Real.rpow_pos_of_pos h0 ω
  have p1 : 0 < a₁ ^ ω := Real.rpow_pos_of_pos h1 ω
  rw [← Real.log_injOn_pos.eq_iff (Set.mem_Ioi.2 (mul_pos h1 p0)) (Set.mem_Ioi.2 (mul_pos h0 p1)),
    Real.log_mul h1.ne' p0.ne', Real.log_mul h0.ne' p1.ne', Real.log_rpow h0, Real.log_rpow h1]
  constructor
  · intro h
    have h2 : (1 - ω) * (Real.log a₁ - Real.log a₀) = 0 := by linarith
    rcases mul_eq_zero.1 h2 with h3 | h3
    · left; linarith
    · right
      exact Real.log_injOn_pos (Set.mem_Ioi.2 h0) (Set.mem_Ioi.2 h1) (by linarith)
  · rintro (h | h)
    · subst h; ring
    · subst h; ring

/-- **The factor-input marginal gap at `c = 0` with `a' = a^ω`**, `a₀ = q₀/α`,
`a₁ = (1-q₀)/(1-α)`: the two sequences' `A`-marginals agree exactly when `ω = 1` or
the cue delivers the prior marginal, `q₀ = α`. -/
theorem factor_pow_gap_iff {ω : ℝ} (hα0 : 0 < α) (hα1 : α < 1) (hq0 : 0 < q₀) (hq1 : q₀ < 1) :
    (1 - α) * ((1 - q₀) / (1 - α)) / (α * (q₀ / α) + (1 - α) * ((1 - q₀) / (1 - α)))
      = (1 - α) * ((1 - q₀) / (1 - α)) ^ ω
          / (α * (q₀ / α) ^ ω + (1 - α) * ((1 - q₀) / (1 - α)) ^ ω)
      ↔ ω = 1 ∨ q₀ = α := by
  have ha0 : 0 < q₀ / α := div_pos hq0 hα0
  have ha1 : 0 < (1 - q₀) / (1 - α) := div_pos (by linarith) (by linarith)
  have hA : α * (q₀ / α) + (1 - α) * ((1 - q₀) / (1 - α)) ≠ 0 := by
    have := mul_pos hα0 ha0; have := mul_pos (show (0:ℝ) < 1 - α by linarith) ha1; linarith
  have hA' : α * (q₀ / α) ^ ω + (1 - α) * ((1 - q₀) / (1 - α)) ^ ω ≠ 0 := by
    have := mul_pos hα0 (Real.rpow_pos_of_pos ha0 ω)
    have := mul_pos (show (0:ℝ) < 1 - α by linarith) (Real.rpow_pos_of_pos ha1 ω); linarith
  rw [factor_mA1_gap_iff hα0.ne' (by linarith) hA hA', pow_factor_ratio_iff ha0 ha1]
  have key : q₀ / α = (1 - q₀) / (1 - α) ↔ q₀ = α := by
    rw [div_eq_div_iff hα0.ne' (by linarith)]
    constructor <;> intro h <;> nlinarith
  rw [key]

end JeffreyOrder
