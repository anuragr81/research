import Mathlib

open Finset

namespace Sufficiency

variable {C : Type*} [Fintype C]

/-- Bayes' posterior on class `c` within a group, from the prior `p c = P(c | g)`
and the marker likelihood `L c = P(m | c, g)`. -/
noncomputable def posterior (p L : C → ℝ) (c : C) : ℝ :=
  L c * p c / ∑ c', L c' * p c'

/-- A likelihood that does not depend on class leaves the posterior equal to the
prior. -/
theorem posterior_eq_prior_of_flat (p L : C → ℝ) (k : ℝ)
    (hL : ∀ c, L c = k) (hk : k ≠ 0) (hp : ∑ c, p c = 1) :
    ∀ c, posterior p L c = p c := by
  intro c
  have hZ : ∑ c', L c' * p c' = k := by
    rw [Finset.sum_congr rfl (fun c' _ => by rw [hL c']), ← Finset.mul_sum, hp, mul_one]
  unfold posterior
  rw [hZ, hL c]
  field_simp

/-- Conversely, under a prior with full support, a posterior equal to the prior at
every class forces the likelihood to equal the marginal at every class, so it does
not depend on class. -/
theorem flat_of_posterior_eq_prior (p L : C → ℝ)
    (hpos : ∀ c, 0 < p c) (hZ : ∑ c', L c' * p c' ≠ 0)
    (h : ∀ c, posterior p L c = p c) :
    ∀ c, L c = ∑ c', L c' * p c' := by
  intro c
  have h1 := h c
  unfold posterior at h1
  rw [div_eq_iff hZ] at h1
  apply mul_right_cancel₀ (hpos c).ne'
  linear_combination h1

/-- Control for the first theorem. With a likelihood that depends on class, the
posterior differs from the prior. -/
theorem control_flat_needed :
    ∃ p L : Fin 2 → ℝ, (∑ c, p c = 1) ∧ posterior p L 0 ≠ p 0 := by
  refine ⟨![1 / 2, 1 / 2], ![1, 0], ?_, ?_⟩
  · norm_num [Fin.sum_univ_two]
  · norm_num [posterior, Fin.sum_univ_two]

/-- Control for the second theorem. Without full support, the posterior can equal
the prior at every class while the likelihood depends on class. -/
theorem control_support_needed :
    ∃ p L : Fin 2 → ℝ, (∑ c, p c = 1) ∧ (∀ c, posterior p L c = p c) ∧ L 0 ≠ L 1 := by
  refine ⟨![1, 0], ![1, 2], ?_, ?_, ?_⟩
  · simp [Fin.sum_univ_two]
  · intro c
    fin_cases c <;> simp [posterior, Fin.sum_univ_two]
  · simp

end Sufficiency
