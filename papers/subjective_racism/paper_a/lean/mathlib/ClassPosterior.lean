import Mathlib

open Finset

namespace ClassPosterior

variable {G C : Type*} [Fintype G] [Fintype C]

/-- The posterior on class reached through group weights `w`: the within-group
class distributions `p g` mixed by `w`. -/
noncomputable def mixture (p : G → C → ℝ) (w : G → ℝ) (c : C) : ℝ :=
  ∑ g, p g c * w g

/-- The marginal probability of the marker, from group weights `q`, within-group
class distributions `p` and the likelihood `L g c = P(m | c, g)`. -/
noncomputable def marg (q : G → ℝ) (p L : G → C → ℝ) : ℝ :=
  ∑ g, ∑ c, L g c * p g c * q g

/-- The Bayesian posterior on class. -/
noncomputable def bayesClass (q : G → ℝ) (p L : G → C → ℝ) (c : C) : ℝ :=
  (∑ g, L g c * p g c * q g) / marg q p L

/-- The Bayesian posterior on group. -/
noncomputable def bayesGroup (q : G → ℝ) (p L : G → C → ℝ) (g : G) : ℝ :=
  (∑ c, L g c * p g c * q g) / marg q p L

/-- Under group sufficiency the Bayesian posterior on class is the mixture of the
within-group class distributions by the Bayesian posterior on group. -/
theorem bayes_class_eq_mixture (q : G → ℝ) (p L : G → C → ℝ) (lam : G → ℝ)
    (hp : ∀ g, ∑ c, p g c = 1) (hL : ∀ g c, L g c = lam g) :
    ∀ c, bayesClass q p L c = mixture p (bayesGroup q p L) c := by
  intro c
  have hg : ∀ g, ∑ c', L g c' * p g c' * q g = lam g * q g := by
    intro g
    calc ∑ c', L g c' * p g c' * q g = ∑ c', (lam g * q g) * p g c' := by
          apply Finset.sum_congr rfl
          intro c' _
          rw [hL g c']
          ring
      _ = (lam g * q g) * ∑ c', p g c' := by rw [Finset.mul_sum]
      _ = lam g * q g := by rw [hp g, mul_one]
  unfold bayesClass mixture bayesGroup
  simp_rw [hg, hL]
  rw [Finset.sum_div]
  apply Finset.sum_congr rfl
  intro g _
  ring

/-- Control: drop group sufficiency. One group, two classes, a uniform class
distribution and a likelihood `(1, 0)`. Bayes puts probability 1 on class 0, and
the mixture by the Bayesian group weight puts 1/2. -/
theorem control_sufficiency_needed :
    ∃ (q : Fin 1 → ℝ) (p L : Fin 1 → Fin 2 → ℝ),
      (∀ g, ∑ c, p g c = 1) ∧ bayesClass q p L 0 ≠ mixture p (bayesGroup q p L) 0 := by
  refine ⟨![1], ![![1 / 2, 1 / 2]], ![![1, 0]], ?_, ?_⟩
  · intro g; fin_cases g; norm_num [Fin.sum_univ_two]
  · norm_num [bayesClass, bayesGroup, mixture, marg, Fin.sum_univ_two]

/-- Control: drop the requirement that each within-group class distribution sums
to one. The likelihood is flat, but the class weights `(1, 1)` sum to 2, and the
two sides differ. -/
theorem control_normalised_needed :
    ∃ (q : Fin 1 → ℝ) (p L : Fin 1 → Fin 2 → ℝ),
      (∀ g c, L g c = 1) ∧ bayesClass q p L 0 ≠ mixture p (bayesGroup q p L) 0 := by
  refine ⟨![1], ![![1, 1]], ![![1, 1]], ?_, ?_⟩
  · intro g c; fin_cases g; fin_cases c <;> rfl
  · norm_num [bayesClass, bayesGroup, mixture, marg, Fin.sum_univ_two]

/-- The likelihood that reproduces target group weights `w` from prior weights
`q`: proportional to `w g / q g`, constant in class. -/
noncomputable def ratioLik (κ : ℝ) (q w : G → ℝ) : G → C → ℝ :=
  fun g _ => κ * w g / q g

/-- Every revision of group weights from a prior with full support is a Bayesian
update, for the likelihood `κ w g / q g`. -/
theorem bayes_group_of_ratio (κ : ℝ) (q w : G → ℝ) (p : G → C → ℝ)
    (hκ : κ ≠ 0) (hq : ∀ g, 0 < q g) (hw : ∑ g, w g = 1) (hp : ∀ g, ∑ c, p g c = 1) :
    ∀ g, bayesGroup q p (ratioLik κ q w) g = w g := by
  have hrow : ∀ g, ∑ c, ratioLik κ q w g c * p g c * q g = κ * w g := by
    intro g
    have hqg : q g ≠ 0 := (hq g).ne'
    calc ∑ c, ratioLik κ q w g c * p g c * q g = ∑ c, (κ * w g) * p g c := by
          apply Finset.sum_congr rfl
          intro c _
          unfold ratioLik
          field_simp
      _ = (κ * w g) * ∑ c, p g c := by rw [Finset.mul_sum]
      _ = κ * w g := by rw [hp g, mul_one]
  have hm : marg q p (ratioLik κ q w) = κ := by
    unfold marg
    simp_rw [hrow]
    rw [← Finset.mul_sum, hw, mul_one]
  intro g
  unfold bayesGroup
  rw [hrow g, hm]
  field_simp

/-- With that likelihood the Bayesian posterior on class is the mixture by `w`. -/
theorem bayes_class_of_ratio (κ : ℝ) (q w : G → ℝ) (p : G → C → ℝ)
    (hκ : κ ≠ 0) (hq : ∀ g, 0 < q g) (hw : ∑ g, w g = 1) (hp : ∀ g, ∑ c, p g c = 1) :
    ∀ c, bayesClass q p (ratioLik κ q w) c = mixture p w c := by
  intro c
  rw [bayes_class_eq_mixture q p (ratioLik κ q w) (fun g => κ * w g / q g) hp (fun _ _ => rfl)]
  unfold mixture
  apply Finset.sum_congr rfl
  intro g _
  rw [bayes_group_of_ratio κ q w p hκ hq hw hp g]

omit [Fintype G] [Fintype C] in
/-- The likelihood is a probability when `0 < κ ≤ q g` and `0 ≤ w g ≤ 1`. -/
theorem ratioLik_mem_unit (κ : ℝ) (q w : G → ℝ) (hκ : 0 < κ) (hκq : ∀ g, κ ≤ q g)
    (hw0 : ∀ g, 0 ≤ w g) (hw1 : ∀ g, w g ≤ 1) :
    ∀ g c, 0 ≤ (ratioLik κ q w : G → C → ℝ) g c ∧ (ratioLik κ q w : G → C → ℝ) g c ≤ 1 := by
  intro g c
  have hqg : 0 < q g := lt_of_lt_of_le hκ (hκq g)
  unfold ratioLik
  refine ⟨div_nonneg (mul_nonneg hκ.le (hw0 g)) hqg.le, ?_⟩
  rw [div_le_one hqg]
  nlinarith [hκq g, hw0 g, hw1 g]

/-- Control: drop full support. A group with prior weight 0 keeps posterior
weight 0 under every likelihood, so no Bayesian update reaches a revision that
gives it positive weight. -/
theorem control_prior_support_needed (p L : Fin 2 → C → ℝ) :
    bayesGroup ![1, 0] p L 1 = 0 := by
  simp [bayesGroup]

end ClassPosterior
