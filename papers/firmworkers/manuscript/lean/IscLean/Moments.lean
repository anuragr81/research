import Mathlib.Probability.Moments.Variance

open MeasureTheory

namespace Isc

theorem taylor_expectation_integral {Ω : Type*} [MeasurableSpace Ω] (μ : Measure Ω)
    [IsProbabilityMeasure μ] (ε : Ω → ℝ) (hi1 : Integrable ε μ)
    (hi2 : Integrable (fun ω => ε ω ^ 2) μ) (h1 : ∫ ω, ε ω ∂μ = 0)
    (h2 : ∫ ω, ε ω ^ 2 ∂μ = 1) (u0 u1 u2 σ : ℝ) :
    ∫ ω, (u0 + u1 * (σ * ε ω) + (1 / 2) * u2 * (σ * ε ω) ^ 2) ∂μ
      = u0 + (1 / 2) * u2 * σ ^ 2 := by
  have hexp : ∀ ω, u0 + u1 * (σ * ε ω) + (1 / 2) * u2 * (σ * ε ω) ^ 2
      = u0 + (u1 * σ) * ε ω + ((1 / 2) * u2 * σ ^ 2) * ε ω ^ 2 := by
    intro ω
    ring
  have hc : Integrable (fun _ : Ω => u0) μ := integrable_const u0
  have hl : Integrable (fun ω => (u1 * σ) * ε ω) μ := hi1.const_mul _
  have hq : Integrable (fun ω => ((1 / 2) * u2 * σ ^ 2) * ε ω ^ 2) μ := hi2.const_mul _
  have hcl : Integrable (fun ω => u0 + (u1 * σ) * ε ω) μ := hc.add hl
  simp_rw [hexp]
  rw [integral_add hcl hq, integral_add hc hl, integral_const, integral_const_mul,
    integral_const_mul, h1, h2]
  simp

theorem variance_ne_neg_multiple {Ω : Type*} [MeasurableSpace Ω] (X : Ω → ℝ) (μ : Measure Ω)
    {p lam : ℝ} (hp : p < 0) (hlam : lam < 1) :
    ProbabilityTheory.variance X μ ≠ p * (1 - lam) ^ 2 := by
  have hv := ProbabilityTheory.variance_nonneg X μ
  have hs : 0 < (1 - lam) ^ 2 := by
    have : 0 < 1 - lam := by linarith
    positivity
  have hr : p * (1 - lam) ^ 2 < 0 := mul_neg_of_neg_of_pos hp hs
  intro h
  linarith

end Isc
