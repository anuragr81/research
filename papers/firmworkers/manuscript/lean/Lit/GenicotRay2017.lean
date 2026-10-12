import Mathlib.Analysis.SpecialFunctions.Pow.Real
import Mathlib.Tactic

open Real

namespace GenicotRay2017

section Aspirations

variable {D : Type*}

theorem aspirations_ratio_decreasing {Ψ : ℝ → D → ℝ} {sc : ℝ → D → D}
    (hscale : ∀ lam y F, 0 < lam → Ψ (lam * y) (sc lam F) = lam * Ψ y F)
    (hmono : ∀ lam y F, 1 < lam → Ψ y F < Ψ y (sc lam F)) {y₁ y₂ : ℝ} (F : D) (h1 : 0 < y₁)
    (h12 : y₁ < y₂) : Ψ y₂ F / y₂ < Ψ y₁ F / y₁ := by
  set lam := y₂ / y₁ with hlam
  have hl : 1 < lam := (one_lt_div h1).mpr h12
  have hy2 : y₂ = lam * y₁ := by rw [hlam]; field_simp
  have key : Ψ y₂ F < lam * Ψ y₁ F := by
    have := hmono lam y₂ F hl
    rw [hy2, hscale lam y₁ F (by linarith)] at this
    rwa [hy2]
  rw [div_lt_div_iff₀ (by linarith) h1, hy2]
  rw [hy2] at key
  nlinarith

theorem gap_grows_with_income {y₁ y₂ a₁ a₂ : ℝ} (h1 : 0 < y₁) (h12 : y₁ < y₂)
    (hratio : a₂ / y₂ ≤ a₁ / y₁) (hsat : 0 ≤ y₁ - a₁) : y₁ - a₁ ≤ y₂ - a₂ := by
  have h2 : 0 < y₂ := by linarith
  rw [div_le_div_iff₀ h2 h1] at hratio
  nlinarith

theorem control_ratio_needs_social_monotonicity :
    (fun (y : ℝ) (_ : Unit) => y) 2 () / 2 = (fun (y : ℝ) (_ : Unit) => y) 1 () / 1 := by
  norm_num

theorem average_aspirations_ratio {m y₁ y₂ : ℝ} (hm : 0 < m) (h1 : 0 < y₁) (h12 : y₁ < y₂) :
    ((y₂ + m) / 2) / y₂ < ((y₁ + m) / 2) / y₁ := by
  have h2 : 0 < y₂ := by linarith
  rw [div_lt_div_iff₀ h2 h1]
  nlinarith

theorem average_aspirations_range {lo hi y m : ℝ} (hy : lo ≤ y ∧ y ≤ hi) (hm : lo ≤ m ∧ m ≤ hi) :
    lo ≤ (y + m) / 2 ∧ (y + m) / 2 ≤ hi := by
  constructor <;> linarith [hy.1, hy.2, hm.1, hm.2]

theorem average_aspirations_scale (lam y m : ℝ) : (lam * y + lam * m) / 2 = lam * ((y + m) / 2) := by
  ring

end Aspirations

section Equality

theorem single_point_aspiration {lo hi a y : ℝ} (hlo : lo = y) (hhi : hi = y)
    (hrange : lo ≤ a ∧ a ≤ hi) : a = y := by
  rw [hlo, hhi] at hrange
  linarith [hrange.1, hrange.2]

theorem no_stay_at_aspiration {w0' w1'0 rhs : ℝ} (hjump : 0 < w1'0) :
    ¬ (w0' ≥ rhs ∧ w0' + w1'0 ≤ rhs) := by
  rintro ⟨h1, h2⟩
  linarith

theorem control_stay_needs_milestone_jump (w0' : ℝ) : w0' ≥ w0' ∧ w0' + 0 ≤ w0' := by
  constructor <;> linarith

end Equality

section Growth

theorem scale_reduction {y z a ρ δ pr σ : ℝ} (hy : 0 < y) (hρ : 0 < ρ) (hz : 0 ≤ z)
    (hc : 0 ≤ y - z / ρ) :
    (y - z / ρ) ^ (1 - σ) + δ * (z ^ (1 - σ) + pr * (max (z - a) 0) ^ (1 - σ))
      = y ^ (1 - σ) * ((1 - (z / y) / ρ) ^ (1 - σ)
        + δ * ((z / y) ^ (1 - σ) + pr * (max (z / y - a / y) 0) ^ (1 - σ))) := by
  have e1 : y - z / ρ = y * (1 - (z / y) / ρ) := by
    field_simp
    ring
  have e2 : z = y * (z / y) := by field_simp
  have e3 : max (z - a) 0 = y * max (z / y - a / y) 0 := by
    rw [mul_max_of_nonneg _ _ hy.le, mul_zero]
    congr 1
    field_simp
  have hn1 : 0 ≤ 1 - (z / y) / ρ := by
    rw [e1] at hc
    exact (mul_nonneg_iff_of_pos_left hy).mp hc
  have hn2 : 0 ≤ z / y := div_nonneg hz hy.le
  have hn3 : 0 ≤ max (z / y - a / y) 0 := le_max_right _ _
  rw [e1, e3, Real.mul_rpow hy.le hn1, Real.mul_rpow hy.le hn3]
  conv_lhs => rw [show z ^ (1 - σ) = (y * (z / y)) ^ (1 - σ) by rw [← e2]]
  rw [Real.mul_rpow hy.le hn2]
  ring

theorem aspirations_raise_growth {L R : ℝ → ℝ} (hL : Monotone L) (hR : Antitone R) {extra : ℝ → ℝ}
    (hextra : ∀ g, 0 < extra g) {g₀ g₁ : ℝ} (h0 : L g₀ = R g₀) (h1 : L g₁ = R g₁ + extra g₁) :
    g₀ < g₁ := by
  by_contra h
  push_neg at h
  have a := hL h
  have b := hR h
  have c := hextra g₁
  linarith

theorem control_growth_needs_positive_extra {L R : ℝ → ℝ} {g : ℝ} (h0 : L g = R g) :
    L g = R g + (fun _ : ℝ => (0 : ℝ)) g := by
  simp [h0]

end Growth

end GenicotRay2017
