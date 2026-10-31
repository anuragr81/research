/-
================================================================================
  Pagel, "Expectations-Based Reference-Dependent Preferences and Asset
  Pricing", Journal of the European Economic Association 14(2) (2016).
  What lit/pagel_2016/ attributes to the paper, in its notation (η the
  weight of gain-loss utility, λ loss aversion, γ the discount on
  prospective gain-loss utility, F = F(ε_t) the normal distribution
  function at the current shock, Q the discounted stream of consumption
  utility, ψ = Q + Ω + γQΩ the marginal value of savings, ρ the
  consumption-wealth ratio). G = ηF + ηλ(1 − F) is the marginal gain-loss
  weight of eq. (9); the paper writes it out in full. The power of
  (ρ/(1 − ρ)) and of wealth in the first-order condition of Appendix B.1
  is cancelled by hand before eq. (7); `foc_iff_rho` starts after that
  step.
================================================================================
-/

import Mathlib

open Real

namespace Pagel

/-- The marginal gain-loss weight ηF + ηλ(1 − F) of eq. (9). -/
def weight (η lam F : ℝ) : ℝ := η * F + η * lam * (1 - F)

/-- The price-consumption ratio (1 − ρ)/ρ as eq. (7) gives it, (ψ + γQG)/(1 + G). -/
noncomputable def pc (ψ γ Q G : ℝ) : ℝ := (ψ + γ * Q * G) / (1 + G)

/-- The consumption-wealth ratio of eq. (7). -/
noncomputable def rho (ψ γ Q G : ℝ) : ℝ := 1 / (1 + pc ψ γ Q G)

/-- The threshold γ̄ of Proposition 2, (ηλ − Ω/Q)/(Ω + ηλ). -/
noncomputable def gammaBar (η lam Ω Q : ℝ) : ℝ := (η * lam - Ω / Q) / (Ω + η * lam)

/-- The gross return of eq. (10), (ρ_t/(1 − ρ_t)) (C_{t+1}/C_t)/ρ_{t+1}. -/
noncomputable def grossReturn (ρ₀ ρ₁ c : ℝ) : ℝ := ρ₀ / (1 - ρ₀) * (c / ρ₁)

/-- The log of the gross return, with consumption growth e^{μ+ε}. -/
noncomputable def logReturn (ρ₀ ρ₁ μ ε : ℝ) : ℝ :=
  Real.log (grossReturn ρ₀ ρ₁ (Real.exp (μ + ε)))

/-- The weight lies in [η, ηλ] (p. 12). -/
theorem weight_mem {η lam F : ℝ} (hη : 0 ≤ η) (hlam : 1 ≤ lam) (hF0 : 0 ≤ F) (hF1 : F ≤ 1) :
    η ≤ weight η lam F ∧ weight η lam F ≤ η * lam := by
  unfold weight
  constructor <;> nlinarith [mul_nonneg hη (sub_nonneg.mpr hlam), mul_nonneg hη hF0,
    mul_nonneg (mul_nonneg hη (sub_nonneg.mpr hlam)) (sub_nonneg.mpr hF1),
    mul_nonneg (mul_nonneg hη (sub_nonneg.mpr hlam)) hF0]

/-- The weight falls as F rises when λ > 1. -/
theorem weight_strictAnti {η lam F₁ F₂ : ℝ} (hη : 0 < η) (hlam : 1 < lam) (h : F₁ < F₂) :
    weight η lam F₂ < weight η lam F₁ := by
  unfold weight
  nlinarith [mul_pos (mul_pos hη (sub_pos.mpr hlam)) (sub_pos.mpr h)]

/-- The step from the first-order condition to eq. (7): (1 + G)(1 − ρ) = ρ(ψ + γQG)
    holds exactly when ρ is the ratio of eq. (7). -/
theorem foc_iff_rho {ψ γ Q G ρ : ℝ} (hG : 0 < 1 + G) (hψ : 0 < ψ + γ * Q * G) :
    (1 + G) * (1 - ρ) = ρ * (ψ + γ * Q * G) ↔ ρ = rho ψ γ Q G := by
  unfold rho pc
  have h1 : 0 < 1 + (ψ + γ * Q * G) / (1 + G) := by positivity
  constructor
  · intro h
    rw [eq_div_iff h1.ne']
    field_simp
    linarith
  · intro h
    obtain ⟨D, hDdef⟩ : ∃ D, D = 1 + G + (ψ + γ * Q * G) := ⟨_, rfl⟩
    have hD : D ≠ 0 := by rw [hDdef]; linarith
    have hr : ρ = (1 + G) / D := by
      rw [h, hDdef, one_add_div hG.ne', one_div, inv_div]
    rw [hr]
    field_simp
    rw [hDdef]
    ring

/-- The consumption-wealth ratio lies in (0, 1). -/
theorem rho_mem {ψ γ Q G : ℝ} (hG : 0 < 1 + G) (hψ : 0 < ψ + γ * Q * G) :
    0 < rho ψ γ Q G ∧ rho ψ γ Q G < 1 := by
  unfold rho pc
  have hp : 0 < (ψ + γ * Q * G) / (1 + G) := div_pos hψ hG
  constructor
  · positivity
  · rw [div_lt_one (by linarith)]
    linarith

/-- The price-consumption ratio moves with the weight by the factor ψ − γQ. -/
theorem pc_sub {ψ γ Q G₁ G₂ : ℝ} (h₁ : 0 < 1 + G₁) (h₂ : 0 < 1 + G₂) :
    pc ψ γ Q G₁ - pc ψ γ Q G₂ = (ψ - γ * Q) * (G₂ - G₁) / ((1 + G₁) * (1 + G₂)) := by
  unfold pc
  field_simp
  ring

/-- Proposition 3, in differences: when ψ > γQ, a higher shock (larger F) lowers ρ. -/
theorem rho_strictAnti_in_F {ψ γ Q η lam F₁ F₂ : ℝ} (hη : 0 < η) (hlam : 1 < lam)
    (hF0 : 0 ≤ F₁) (h : F₁ < F₂) (hF1 : F₂ ≤ 1) (hψQ : γ * Q < ψ) (hγQ : 0 ≤ γ * Q) :
    rho ψ γ Q (weight η lam F₂) < rho ψ γ Q (weight η lam F₁) := by
  have hm₁ := weight_mem hη.le hlam.le hF0 (h.le.trans hF1)
  have hm₂ := weight_mem hη.le hlam.le (hF0.trans h.le) hF1
  set G₁ := weight η lam F₁
  set G₂ := weight η lam F₂
  have hG : G₂ < G₁ := weight_strictAnti hη hlam h
  have h1 : 0 < 1 + G₁ := by linarith
  have h2 : 0 < 1 + G₂ := by linarith
  have hpc : pc ψ γ Q G₁ < pc ψ γ Q G₂ := by
    have := pc_sub (ψ := ψ) (γ := γ) (Q := Q) h2 h1
    have hpos : 0 < (ψ - γ * Q) * (G₁ - G₂) / ((1 + G₂) * (1 + G₁)) := by
      apply div_pos
      · exact mul_pos (by linarith) (by linarith)
      · positivity
    linarith
  have hp1 : 0 < pc ψ γ Q G₁ := by
    unfold pc; apply div_pos _ h1; nlinarith
  unfold rho
  apply one_div_lt_one_div_of_lt (by linarith) (by linarith)

/-- Proposition 2's threshold solves the crossing condition at F = 0, where G = ηλ. -/
theorem gammaBar_solves {η lam Ω Q : ℝ} (hQ : 0 < Q) (hd : 0 < Ω + η * lam) :
    Q + Ω + gammaBar η lam Ω Q * Q * Ω + gammaBar η lam Ω Q * Q * (η * lam)
      = Q * (1 + η * lam) := by
  unfold gammaBar
  field_simp
  ring

/-- γ̄ < 1 whenever Ω > 0 (Proposition 2). -/
theorem gammaBar_lt_one {η lam Ω Q : ℝ} (hQ : 0 < Q) (hΩ : 0 < Ω) (hηl : 0 ≤ η * lam) :
    gammaBar η lam Ω Q < 1 := by
  unfold gammaBar
  rw [div_lt_one (by linarith)]
  have : 0 < Ω / Q := div_pos hΩ hQ
  linarith

theorem logReturn_eq {ρ₀ ρ₁ μ ε : ℝ} (h₀ : 0 < ρ₀) (h₀' : ρ₀ < 1) (h₁ : 0 < ρ₁) :
    logReturn ρ₀ ρ₁ μ ε = Real.log (ρ₀ / (1 - ρ₀)) + (μ + ε) - Real.log ρ₁ := by
  unfold logReturn grossReturn
  have hs : 0 < ρ₀ / (1 - ρ₀) := div_pos h₀ (by linarith)
  rw [Real.log_mul hs.ne' (div_pos (Real.exp_pos _) h₁).ne',
    Real.log_div (Real.exp_pos _).ne' h₁.ne', Real.log_exp]
  ring

/-- The dispersion of the log return across two next-period outcomes does not depend on
    the current state ρ₀: eq. (10) makes the current state a predetermined scale. -/
theorem log_dispersion_free_of_state {ρ₀ ρa ρb μ εa εb : ℝ} (h₀ : 0 < ρ₀) (h₀' : ρ₀ < 1)
    (ha : 0 < ρa) (hb : 0 < ρb) :
    logReturn ρ₀ ρa μ εa - logReturn ρ₀ ρb μ εb
      = (εa - Real.log ρa) - (εb - Real.log ρb) := by
  rw [logReturn_eq h₀ h₀' ha, logReturn_eq h₀ h₀' hb]
  ring

/-- Control: at ψ = γQ the ratio does not move with the weight (the case θ = 1, γ = 1). -/
theorem control_rho_flat_at_psi_eq_gammaQ {γ Q G₁ G₂ : ℝ} (h₁ : 0 < 1 + G₁) (h₂ : 0 < 1 + G₂) :
    rho (γ * Q) γ Q G₁ = rho (γ * Q) γ Q G₂ := by
  have := pc_sub (ψ := γ * Q) (γ := γ) (Q := Q) h₁ h₂
  have h : pc (γ * Q) γ Q G₁ = pc (γ * Q) γ Q G₂ := by
    rw [sub_self, zero_mul, zero_div] at this
    linarith
  unfold rho
  rw [h]

/-- Control: at λ = 1 the weight is η whatever F, so the shock does not move it. -/
theorem control_weight_flat_at_lam_one {η F : ℝ} : weight η 1 F = η := by
  unfold weight
  ring

/-- Control: the dispersion of the gross return does depend on the state. Scale 1
    (ρ₀ = 1/2) and scale 2 (ρ₀ = 2/3) give dispersions 2 and 4 between growth 1 and 2. -/
theorem control_gross_dispersion_depends_on_state :
    grossReturn (1 / 2) (1 / 2) 2 - grossReturn (1 / 2) (1 / 2) 1
      ≠ grossReturn (2 / 3) (1 / 2) 2 - grossReturn (2 / 3) (1 / 2) 1 := by
  unfold grossReturn
  norm_num

end Pagel
