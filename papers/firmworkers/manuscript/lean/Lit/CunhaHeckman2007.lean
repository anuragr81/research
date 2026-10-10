import Mathlib.Analysis.SpecialFunctions.Pow.Real
import Mathlib.Tactic

open Real

namespace CunhaHeckman2007

noncomputable def ces (φ γ a b : ℝ) : ℝ := (γ * a ^ φ + (1 - γ) * b ^ φ) ^ (1 / φ)

theorem ces_perfect_substitutes (γ a b : ℝ) : ces 1 γ a b = γ * a + (1 - γ) * b := by
  simp [ces]

theorem timing_irrelevant_at_half (I : ℝ) : ces 1 (1 / 2) 0 I = ces 1 (1 / 2) I 0 := by
  rw [ces_perfect_substitutes, ces_perfect_substitutes]
  ring

theorem control_timing_needs_half : ces 1 (3 / 4) 0 1 ≠ ces 1 (3 / 4) 1 0 := by
  rw [ces_perfect_substitutes, ces_perfect_substitutes]
  norm_num

theorem output_along_budget (γ r E I₁ : ℝ) :
    γ * I₁ + (1 - γ) * ((1 + r) * (E - I₁))
      = (1 - γ) * (1 + r) * E + (γ - (1 - γ) * (1 + r)) * I₁ := by
  ring

theorem invest_early_iff {γ r E : ℝ} (hE : 0 < E) :
    (1 - γ) * ((1 + r) * E) < γ * E ↔ (1 - γ) * (1 + r) < γ := by
  constructor
  · intro h
    by_contra hc
    push_neg at hc
    nlinarith
  · intro h
    nlinarith

theorem control_invest_early_needs_budget :
    ¬ ((1 - 1) * ((1 + 0) * (0 : ℝ)) < 1 * 0 ↔ (1 - 1) * (1 + 0) < (1 : ℝ)) := by
  norm_num

theorem leontief_no_early_no_output {I₂ : ℝ} (h : 0 ≤ I₂) : min 0 I₂ = 0 :=
  min_eq_left h

theorem leontief_even_is_optimal {r E I₁ I₂ : ℝ} (hr : -1 < r)
    (hbudget : I₁ + I₂ / (1 + r) = E) :
    min I₁ I₂ ≤ E * (1 + r) / (2 + r) := by
  have h1r : 0 < 1 + r := by linarith
  have h2r : 0 < 2 + r := by linarith
  have hm1 : min I₁ I₂ ≤ I₁ := min_le_left _ _
  have hm2 : min I₁ I₂ ≤ I₂ := min_le_right _ _
  have hm2' : min I₁ I₂ / (1 + r) ≤ I₂ / (1 + r) := div_le_div_of_nonneg_right hm2 h1r.le
  rw [le_div_iff₀ h2r]
  have key : min I₁ I₂ + min I₁ I₂ / (1 + r) ≤ E := by linarith
  have e : min I₁ I₂ * (2 + r) = (min I₁ I₂ + min I₁ I₂ / (1 + r)) * (1 + r) := by
    field_simp
    ring
  rw [e]
  exact mul_le_mul_of_nonneg_right key h1r.le

theorem leontief_even_attains {r E : ℝ} (hr : -1 < r) :
    E * (1 + r) / (2 + r) + (E * (1 + r) / (2 + r)) / (1 + r) = E
      ∧ min (E * (1 + r) / (2 + r)) (E * (1 + r) / (2 + r)) = E * (1 + r) / (2 + r) := by
  have h1r : (1 + r) ≠ 0 := by linarith
  have h2r : (2 + r) ≠ 0 := by linarith
  refine ⟨?_, min_self _⟩
  field_simp
  ring

theorem leontief_dynamic_complementarity {a₁ a₂ b₁ b₂ : ℝ} (ha : a₁ ≤ a₂) (hb : b₁ ≤ b₂) :
    min a₂ b₁ - min a₁ b₁ ≤ min a₂ b₂ - min a₁ b₂ := by
  rcases le_total a₁ b₁ with h1 | h1 <;> rcases le_total a₂ b₁ with h2 | h2 <;>
    rcases le_total a₁ b₂ with h3 | h3 <;> rcases le_total a₂ b₂ with h4 | h4 <;>
    simp only [min_eq_left, min_eq_right, h1, h2, h3, h4] <;> linarith

theorem substitutes_no_complementarity (γ a₁ a₂ b₁ b₂ : ℝ) :
    ces 1 γ a₂ b₁ - ces 1 γ a₁ b₁ = ces 1 γ a₂ b₂ - ces 1 γ a₁ b₂ := by
  simp only [ces_perfect_substitutes]
  ring

noncomputable def optimalRatio (γ r φ : ℝ) : ℝ := (γ / ((1 - γ) * (1 + r))) ^ (1 / (1 - φ))

theorem optimal_ratio_of_foc {γ r φ I₁ I₂ : ℝ} (hγ0 : 0 < γ) (hγ1 : γ < 1) (hr : -1 < r)
    (hφ : φ < 1) (h1 : 0 < I₁) (h2 : 0 < I₂)
    (hfoc : γ * I₁ ^ (φ - 1) = (1 + r) * (1 - γ) * I₂ ^ (φ - 1)) :
    I₁ / I₂ = optimalRatio γ r φ := by
  have h1r : 0 < 1 + r := by linarith
  have hA : 0 < I₂ ^ (φ - 1) := Real.rpow_pos_of_pos h2 _
  have hq : (I₁ / I₂) ^ (φ - 1) = (1 + r) * (1 - γ) / γ := by
    rw [Real.div_rpow h1.le h2.le, div_eq_div_iff hA.ne' hγ0.ne']
    linear_combination hfoc
  have hpos : 0 < (1 + r) * (1 - γ) / γ := div_pos (mul_pos h1r (by linarith)) hγ0
  have hne : φ - 1 ≠ 0 := by linarith
  have hback : I₁ / I₂ = ((I₁ / I₂) ^ (φ - 1)) ^ (1 / (φ - 1)) := by
    rw [← Real.rpow_mul (div_pos h1 h2).le, mul_one_div_cancel hne, Real.rpow_one]
  rw [hback, hq, optimalRatio]
  have e : 1 / (φ - 1) = -(1 / (1 - φ)) := by
    have : 1 - φ ≠ 0 := by linarith
    field_simp
  rw [e, Real.rpow_neg hpos.le, ← Real.inv_rpow hpos.le, inv_div, mul_comm (1 + r)]

theorem control_ratio_needs_phi_lt_one :
    (1 / 2 : ℝ) * (2 : ℝ) ^ ((1 : ℝ) - 1) = (1 + 0) * (1 - 1 / 2) * (1 : ℝ) ^ ((1 : ℝ) - 1)
      ∧ (2 : ℝ) / 1 ≠ optimalRatio (1 / 2) 0 1 := by
  unfold optimalRatio
  norm_num

theorem optimal_ratio_cobbDouglas (γ r : ℝ) : optimalRatio γ r 0 = γ / ((1 - γ) * (1 + r)) := by
  simp [optimalRatio]

theorem optimal_ratio_strictMono_gamma {γ₁ γ₂ r φ : ℝ} (h0 : 0 < γ₁) (h12 : γ₁ < γ₂) (h21 : γ₂ < 1)
    (hr : -1 < r) (hφ : φ < 1) : optimalRatio γ₁ r φ < optimalRatio γ₂ r φ := by
  unfold optimalRatio
  have h1r : 0 < 1 + r := by linarith
  have hd₁ : 0 < (1 - γ₁) * (1 + r) := mul_pos (by linarith) h1r
  have hd₂ : 0 < (1 - γ₂) * (1 + r) := mul_pos (by linarith) h1r
  apply Real.rpow_lt_rpow (div_pos h0 hd₁).le _ (by apply div_pos one_pos; linarith)
  rw [div_lt_div_iff₀ hd₁ hd₂]
  nlinarith

noncomputable def printedConstrainedRatio (γ r φ σ β c₁ c₂ : ℝ) : ℝ :=
  (γ / ((1 - γ) * (1 + r))) ^ (1 / (1 - φ)) * (c₁ / (β * c₂)) ^ ((1 - σ) / (1 - φ))

noncomputable def derivedConstrainedRatio (γ φ σ β c₁ c₂ : ℝ) : ℝ :=
  (γ * β / (1 - γ)) ^ (1 / (1 - φ)) * (c₁ / c₂) ^ ((1 - σ) / (1 - φ))

theorem constrained_ratio_power_of_focs {γ β σ φ c₁ c₂ I₁ I₂ K : ℝ} (hγ1 : γ < 1)
    (hc₁ : 0 < c₁) (hc₂ : 0 < c₂) (h1 : 0 < I₁) (h2 : 0 < I₂)
    (hfoc1 : c₁ ^ (σ - 1) = K * γ * I₁ ^ (φ - 1))
    (hfoc2 : β * c₂ ^ (σ - 1) = K * (1 - γ) * I₂ ^ (φ - 1)) :
    (I₁ / I₂) ^ (1 - φ) = γ * β / (1 - γ) * (c₁ / c₂) ^ (1 - σ) := by
  have eφ : 1 - φ = -(φ - 1) := by ring
  have eσ : 1 - σ = -(σ - 1) := by ring
  have hA1 : 0 < I₁ ^ (φ - 1) := Real.rpow_pos_of_pos h1 _
  have hA2 : 0 < I₂ ^ (φ - 1) := Real.rpow_pos_of_pos h2 _
  have hC1 : 0 < c₁ ^ (σ - 1) := Real.rpow_pos_of_pos hc₁ _
  have hC2 : 0 < c₂ ^ (σ - 1) := Real.rpow_pos_of_pos hc₂ _
  have hg : 0 < 1 - γ := by linarith
  rw [Real.div_rpow h1.le h2.le, Real.div_rpow hc₁.le hc₂.le, eφ, eσ, Real.rpow_neg h1.le,
    Real.rpow_neg h2.le, Real.rpow_neg hc₁.le, Real.rpow_neg hc₂.le]
  field_simp
  linear_combination (1 - γ) * I₂ ^ (φ - 1) * hfoc1 - γ * I₁ ^ (φ - 1) * hfoc2

theorem constrained_ratio_of_focs {γ β σ φ c₁ c₂ I₁ I₂ K : ℝ} (hγ0 : 0 < γ) (hγ1 : γ < 1)
    (hβ : 0 < β) (hc₁ : 0 < c₁) (hc₂ : 0 < c₂) (h1 : 0 < I₁) (h2 : 0 < I₂) (hφ : φ < 1)
    (hfoc1 : c₁ ^ (σ - 1) = K * γ * I₁ ^ (φ - 1))
    (hfoc2 : β * c₂ ^ (σ - 1) = K * (1 - γ) * I₂ ^ (φ - 1)) :
    I₁ / I₂ = derivedConstrainedRatio γ φ σ β c₁ c₂ := by
  have hp := constrained_ratio_power_of_focs hγ1 hc₁ hc₂ h1 h2 hfoc1 hfoc2
  have hne : 1 - φ ≠ 0 := by linarith
  have hback : I₁ / I₂ = ((I₁ / I₂) ^ (1 - φ)) ^ (1 / (1 - φ)) := by
    rw [← Real.rpow_mul (div_pos h1 h2).le, mul_one_div_cancel hne, Real.rpow_one]
  have hk : 0 ≤ γ * β / (1 - γ) := (div_pos (mul_pos hγ0 hβ) (by linarith)).le
  have hc : 0 ≤ (c₁ / c₂) ^ (1 - σ) := (Real.rpow_pos_of_pos (div_pos hc₁ hc₂) _).le
  rw [hback, hp, Real.mul_rpow hk hc, ← Real.rpow_mul (div_pos hc₁ hc₂).le, derivedConstrainedRatio]
  congr 2
  ring

theorem printed_ratio_reading :
    printedConstrainedRatio (1 / 2) 0 0 0 (1 / 2) 1 1 = 2
      ∧ derivedConstrainedRatio (1 / 2) 0 0 (1 / 2) 1 1 = 1 / 2 := by
  unfold printedConstrainedRatio derivedConstrainedRatio
  norm_num

theorem derived_income_free_at_sigma_one (γ φ β c₁ c₂ : ℝ) :
    derivedConstrainedRatio γ φ 1 β c₁ c₂ = (γ * β / (1 - γ)) ^ (1 / (1 - φ)) := by
  simp [derivedConstrainedRatio]

theorem derived_below_optimal_iff {γ r φ σ c₁ c₂ : ℝ} (hγ0 : 0 < γ) (hγ1 : γ < 1) (hr : -1 < r)
    (hφ : φ < 1) (hσ : σ < 1) (hc₁ : 0 < c₁) (hc₂ : 0 < c₂) :
    derivedConstrainedRatio γ φ σ (1 / (1 + r)) c₁ c₂ < optimalRatio γ r φ ↔ c₁ < c₂ := by
  have h1r : 0 < 1 + r := by linarith
  have hsame : γ * (1 / (1 + r)) / (1 - γ) = γ / ((1 - γ) * (1 + r)) := by
    rw [mul_one_div, div_div, mul_comm (1 + r)]
  have hO : 0 < optimalRatio γ r φ :=
    Real.rpow_pos_of_pos (div_pos hγ0 (mul_pos (by linarith) h1r)) _
  have he : 0 < (1 - σ) / (1 - φ) := div_pos (by linarith) (by linarith)
  unfold derivedConstrainedRatio
  rw [hsame]
  change optimalRatio γ r φ * (c₁ / c₂) ^ ((1 - σ) / (1 - φ)) < optimalRatio γ r φ ↔ c₁ < c₂
  constructor
  · intro h
    by_contra hc
    push_neg at hc
    have h1le : 1 ≤ c₁ / c₂ := by rw [le_div_iff₀ hc₂]; linarith
    have : 1 ≤ (c₁ / c₂) ^ ((1 - σ) / (1 - φ)) := Real.one_le_rpow h1le he.le
    nlinarith
  · intro h
    have hlt : c₁ / c₂ < 1 := by rw [div_lt_one hc₂]; exact h
    have : (c₁ / c₂) ^ ((1 - σ) / (1 - φ)) < 1 := Real.rpow_lt_one (div_pos hc₁ hc₂).le hlt he
    nlinarith

theorem control_below_needs_sigma_lt_one :
    ¬ (derivedConstrainedRatio (1 / 2) 0 1 (1 / (1 + 0)) 1 2 < optimalRatio (1 / 2) 0 0) := by
  unfold derivedConstrainedRatio optimalRatio
  norm_num

theorem table1_text_matches :
    |(0.4109 : ℝ) - 0.41| < 0.005 ∧ |(0.0448 : ℝ) - 0.045| < 0.0005 ∧ (0.65 : ℝ) < 0.6579
      ∧ (0.12 : ℝ) < 0.1264 ∧ |(0.9135 : ℝ) - 0.91| < 0.005 ∧ |(0.0259 : ℝ) - 0.026| < 0.0005
      ∧ |(0.0905 : ℝ) / 0.1767 - 1 / 2| < 0.02 := by
  norm_num [abs_lt]

theorem table1_college_reading : 0.005 < |(0.3755 : ℝ) - 0.37| := by
  norm_num [lt_abs]

end CunhaHeckman2007
