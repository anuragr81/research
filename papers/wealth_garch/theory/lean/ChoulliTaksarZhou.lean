/-
================================================================================
  Choulli, Taksar and Zhou (2004), Interplay between dividend rate and business
  constraints for a financial corporation, Ann. Appl. Probab. 14(4), 1810-1837
  Companion to lit/choulli_taksar_zhou_2004.
================================================================================

The reserve is dR = (aμ - δ - c) dt + aσ dW with exposure α ≤ a ≤ β and
dividend rate 0 ≤ c ≤ M (2.1). Above the dividend threshold x₁ the HJB
equation is (3.3); its interior maximiser is a(x) = -μV'/(σ²V'') (3.5), and
where that maximiser is constant it equals c̃ = 2μ(δ+M)/(μ²+2γσ²) (3.15).
The roots of the characteristic equation σ²z²r²/2 + (zμ-δ-M)r - γ = 0 of
(3.3) at exposure z are r̃± of (3.9)-(3.10); `rtm` is r̃₋.

The file proves that the discriminant at z = c̃ is (δ+M)², so that
r̃₋(c̃) = -μ/(σ²c̃), the exponent of Theorem 3.4(ii) (p. 10); that the
feedback exposure along that exponential is c̃; that smooth fit V'(x₁) = 1
gives the constant K̃ = -σ²c̃/μ = 1/r̃₋(c̃); the condition c̃ ≥ β of
Theorem 3.4(i) in linear form; and the sign of the drift behind
Proposition 3.1. The two controls show that the exponent -σ²c̃/μ printed on
p. 9 and the constant K̃ = -μ/(σ²c̃) printed on pp. 9-10 fail, at one
parameter point, the characteristic equation and smooth fit (CTZ-D1, CTZ-D2).
The verification theorem (Theorem 2.2) and the case analysis of Section 3
are not formalised.
-/

import Mathlib

open Real

namespace ChoulliTaksarZhou

/-- r̃₋(z) of (3.10). -/
noncomputable def rtm (μ σ γ δ M z : ℝ) : ℝ :=
  (-(z * μ - δ - M) - sqrt ((z * μ - δ - M) ^ 2 + 2 * γ * σ ^ 2 * z ^ 2)) / (z ^ 2 * σ ^ 2)

/-- c̃ of (3.15). -/
noncomputable def ctilde (μ σ γ δ M : ℝ) : ℝ := 2 * μ * (δ + M) / (μ ^ 2 + 2 * γ * σ ^ 2)

/-- The characteristic polynomial of (3.3) at exposure z, evaluated at r. -/
noncomputable def charpoly (μ σ γ δ M z r : ℝ) : ℝ := σ ^ 2 * z ^ 2 * r ^ 2 / 2 + (z * μ - δ - M) * r - γ

theorem ctilde_pos {μ σ γ δ M : ℝ} (hμ : 0 < μ) (hγ : 0 < γ) (hdm : 0 < δ + M) :
    0 < ctilde μ σ γ δ M := by
  unfold ctilde
  have : 0 < μ ^ 2 + 2 * γ * σ ^ 2 := by positivity
  positivity

/-- The discriminant of (3.10) at z = c̃ is (δ+M)². -/
theorem disc_at_ctilde {μ σ γ δ M : ℝ} (hμ : 0 < μ) (hγ : 0 < γ) :
    (ctilde μ σ γ δ M * μ - δ - M) ^ 2 + 2 * γ * σ ^ 2 * ctilde μ σ γ δ M ^ 2 = (δ + M) ^ 2 := by
  unfold ctilde
  have hD : μ ^ 2 + 2 * γ * σ ^ 2 ≠ 0 := by positivity
  field_simp
  ring

/-- r̃₋(c̃) = -μ/(σ²c̃), the exponent of Theorem 3.4(ii). -/
theorem rtm_at_ctilde {μ σ γ δ M : ℝ} (hμ : 0 < μ) (hσ : 0 < σ) (hγ : 0 < γ) (hdm : 0 < δ + M) :
    rtm μ σ γ δ M (ctilde μ σ γ δ M) = -μ / (σ ^ 2 * ctilde μ σ γ δ M) := by
  have hc := ctilde_pos (σ := σ) hμ hγ hdm
  unfold rtm
  rw [disc_at_ctilde hμ hγ, Real.sqrt_sq hdm.le]
  field_simp
  ring

/-- The exponential with exponent -μ/(σ²c̃) solves the homogeneous part of (3.3) at a = c̃. -/
theorem charpoly_at_ctilde {μ σ γ δ M : ℝ} (hμ : 0 < μ) (hσ : 0 < σ) (hγ : 0 < γ) (hdm : 0 < δ + M) :
    charpoly μ σ γ δ M (ctilde μ σ γ δ M) (-μ / (σ ^ 2 * ctilde μ σ γ δ M)) = 0 := by
  have hc := ctilde_pos (σ := σ) hμ hγ hdm
  have hD : μ ^ 2 + 2 * γ * σ ^ 2 ≠ 0 := by positivity
  unfold charpoly
  have key : ctilde μ σ γ δ M * (μ ^ 2 + 2 * γ * σ ^ 2) = 2 * μ * (δ + M) := by
    unfold ctilde; field_simp
  field_simp
  nlinarith [key]

/-- Along V = M/γ + K e^{r(x-x₁)} the feedback a(x) = -μV'/(σ²V'') of (3.5) is -μ/(σ²r),
    which at r = -μ/(σ²c̃) is c̃. -/
theorem exposure_at_ctilde {μ σ c : ℝ} (hμ : 0 < μ) (hσ : 0 < σ) (hc : 0 < c) :
    -μ / (σ ^ 2 * (-μ / (σ ^ 2 * c))) = c := by
  field_simp

/-- Smooth fit V'(x₁) = K̃ r̃₋(c̃) = 1 holds with K̃ = -σ²c̃/μ. -/
theorem ktilde_smooth_fit {μ σ γ δ M : ℝ} (hμ : 0 < μ) (hσ : 0 < σ) (hγ : 0 < γ) (hdm : 0 < δ + M) :
    (-σ ^ 2 * ctilde μ σ γ δ M / μ) * rtm μ σ γ δ M (ctilde μ σ γ δ M) = 1 := by
  have hc := ctilde_pos (σ := σ) hμ hγ hdm
  rw [rtm_at_ctilde hμ hσ hγ hdm]
  field_simp

/-- Theorem 3.4(i): the exposure is β throughout [x₁, ∞) iff c̃ ≥ β, that is
    β(μ²+2γσ²) ≤ 2μ(δ+M). -/
theorem binds_upper_iff {μ σ γ δ M β : ℝ} (hμ : 0 < μ) (hγ : 0 < γ) :
    β ≤ ctilde μ σ γ δ M ↔ β * (μ ^ 2 + 2 * γ * σ ^ 2) ≤ 2 * μ * (δ + M) := by
  have hD : 0 < μ ^ 2 + 2 * γ * σ ^ 2 := by positivity
  unfold ctilde
  exact le_div_iff₀ hD

/-- Proposition 3.1: if βμ < δ, the drift aμ - δ is negative at every admissible exposure. -/
theorem drift_neg {μ δ a β : ℝ} (hμ : 0 < μ) (ha : a ≤ β) (h : β * μ < δ) : a * μ - δ < 0 := by
  nlinarith

/-- Control (CTZ-D1): the exponent -σ²c̃/μ printed on p. 9 is not a root of the
    characteristic equation at μ = σ = γ = 1, δ + M = 3, where c̃ = 2. -/
theorem control_printed_exponent_fails :
    charpoly 1 1 1 3 0 (ctilde 1 1 1 3 0) (-(1 : ℝ) ^ 2 * ctilde 1 1 1 3 0 / 1) ≠ 0 := by
  unfold charpoly ctilde
  norm_num

/-- Control (CTZ-D2): with the exponent of Theorem 3.4(ii), the constant K̃ = -μ/(σ²c̃)
    printed on pp. 9-10 fails smooth fit at the same point. -/
theorem control_printed_ktilde_fails :
    (-(1 : ℝ) / (1 ^ 2 * ctilde 1 1 1 3 0)) * (-(1 : ℝ) / (1 ^ 2 * ctilde 1 1 1 3 0)) ≠ 1 := by
  unfold ctilde
  norm_num

end ChoulliTaksarZhou
