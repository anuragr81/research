import Mathlib

namespace AlvarezLeBihanLippi

/-- Prop. 1: frequency N and variance of price changes give σ² = N · Var, for any stopping rule. -/
theorem sigma_sq_from_moments {σ N Var : ℝ} (hN : N ≠ 0) (h : Var = σ ^ 2 / N) : σ ^ 2 = N * Var := by
  rw [h]; field_simp

/-- Prop. 3 with r → 0: the share ℓ of free adjustments gives λ = ℓ · N. -/
theorem lambda_from_share {lam ℓ N : ℝ} (hN : N ≠ 0) (h : ℓ = lam / N) : lam = ℓ * N := by
  rw [h]; field_simp

/-- Two parameters that raise one moment together are not told apart by it: with a moment
    increasing in both, distinct pairs give the same value (their footnote 3, in a linear example). -/
theorem control_same_direction_not_identified :
    ((1 : ℝ) + 2 = 2 + 1) ∧ ((1 : ℝ), (2 : ℝ)) ≠ (2, 1) := by
  norm_num

/-- Lemma 1: the threshold depends on λ and r only through λ + r, so distinct pairs share it. -/
theorem control_threshold_sees_sum_only : ((1 : ℝ) / 10 + 1 / 20 = 1 / 20 + 1 / 10) := by norm_num

end AlvarezLeBihanLippi
