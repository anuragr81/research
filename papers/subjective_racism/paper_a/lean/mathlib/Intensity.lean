import Outsider

open Filter Topology

namespace Intensity

open ClosureMap Bifurcation

/-- The predecessor's intensity-scaled map, its equation (loury-rho) with the
canonical `F`. The intensity `ρ` scales the exclusion pressure. -/
def Hrho (μ γ δ s ρ c : ℝ) : ℝ := H μ γ (ρ * s * δ) c

theorem Hrho_eq (μ γ δ s ρ c : ℝ) :
    Hrho μ γ δ s ρ c = μ + c * (1 - μ - ρ * s * δ * (1 - c) ^ 2) + γ * c * (1 - c) := by
  unfold Hrho H; ring

/-- The intensity threshold `ρ* = b* / (s δ)`. -/
noncomputable def rhoStar (μ γ δ s : ℝ) : ℝ := bStar μ γ / (s * δ)

/-- The strict form of M10. With `γ ≥ 0`, `γ* < γ` exactly when `b < b*`. -/
theorem gammaStar_lt_iff {μ γ b : ℝ} (hμ : 0 < μ) (hγ : 0 ≤ γ) (hb : 0 < b) :
    gammaStar μ b < γ ↔ b < bStar μ γ := by
  rw [gammaStar_eq hμ.le hb.le]
  unfold bStar
  have hu := Real.sq_sqrt hb.le
  have hv := Real.sq_sqrt hμ.le
  have hw := Real.sq_sqrt (by linarith : 0 ≤ μ + γ)
  have hu0 : 0 < Real.sqrt b := Real.sqrt_pos.2 hb
  have hv0 := Real.sqrt_nonneg μ
  have hw0 := Real.sqrt_nonneg (μ + γ)
  have hvw : Real.sqrt μ ≤ Real.sqrt (μ + γ) := Real.sqrt_le_sqrt (by linarith)
  constructor
  · intro h
    have h1 : Real.sqrt b - Real.sqrt μ < Real.sqrt (μ + γ) := by
      by_contra h'
      push_neg at h'
      nlinarith [mul_le_mul h' h' hw0 (by linarith)]
    have h2 : Real.sqrt b < Real.sqrt μ + Real.sqrt (μ + γ) := by linarith
    nlinarith [mul_lt_mul'' h2 h2 hu0.le hu0.le]
  · intro h
    have h1 : Real.sqrt b < Real.sqrt μ + Real.sqrt (μ + γ) := by
      by_contra h'
      push_neg at h'
      nlinarith [mul_le_mul h' h' (by positivity) hu0.le]
    have h2 : Real.sqrt b - Real.sqrt μ < Real.sqrt (μ + γ) := by linarith
    have h3 : -Real.sqrt (μ + γ) < Real.sqrt b - Real.sqrt μ := by linarith
    nlinarith [mul_pos (by linarith : 0 < Real.sqrt (μ + γ) - (Real.sqrt b - Real.sqrt μ))
      (by linarith : 0 < Real.sqrt (μ + γ) + (Real.sqrt b - Real.sqrt μ))]

/-- Row M13 (i). Above the intensity threshold, the trap condition holds. -/
theorem trap_iff_rho {μ γ δ s ρ : ℝ} (hμ : 0 < μ) (hγ : 0 ≤ γ) (hsd : 0 < s * δ)
    (hρ : 0 < ρ) :
    γ < gammaStar μ (ρ * s * δ) ↔ rhoStar μ γ δ s < ρ := by
  have hb : 0 < ρ * s * δ := by rw [mul_assoc]; exact mul_pos hρ hsd
  rw [trap_iff_b hμ hγ hb]
  unfold rhoStar
  rw [div_lt_iff₀ hsd]
  constructor <;> intro h <;> linarith [show ρ * s * δ = ρ * (s * δ) by ring]

/-- Row M13 (ii). Below the intensity threshold, integration is the only fixed
point in `[0, 1]`. -/
theorem only_integration_below {μ γ δ s ρ c : ℝ} (hμ : 0 < μ) (hγ : 0 ≤ γ)
    (hsd : 0 < s * δ) (hρ : 0 < ρ) (hlt : ρ < rhoStar μ γ δ s)
    (h0 : 0 ≤ c) (h1 : c ≤ 1) :
    Hrho μ γ δ s ρ c = c ↔ c = 1 := by
  have hb : 0 < ρ * s * δ := by rw [mul_assoc]; exact mul_pos hρ hsd
  have hlt' : ρ * s * δ < bStar μ γ := by
    unfold rhoStar at hlt
    rw [lt_div_iff₀ hsd] at hlt
    linarith [show ρ * s * δ = ρ * (s * δ) by ring]
  unfold Hrho
  exact only_one_above hμ hb ((gammaStar_lt_iff hμ hγ hb).2 hlt') h0 h1

/-- Row M13 (iii). The threshold lies in `(0, 1)` exactly when the exclusion
pressure at full intensity exceeds `b*`. -/
theorem rhoStar_mem_iff {μ γ δ s : ℝ} (hμ : 0 < μ) (hsd : 0 < s * δ) :
    (0 < rhoStar μ γ δ s ∧ rhoStar μ γ δ s < 1) ↔ bStar μ γ < s * δ := by
  have hpos : 0 < bStar μ γ := by
    unfold bStar
    have := Real.sqrt_pos.2 hμ
    have := Real.sqrt_nonneg (μ + γ)
    positivity
  unfold rhoStar
  constructor
  · rintro ⟨_, h⟩
    rwa [div_lt_one hsd] at h
  · intro h
    exact ⟨div_pos hpos hsd, (div_lt_one hsd).2 h⟩

/-- The threshold is interior at the illustrative parameters
`μ = 1/20`, `γ = 1/10`, `s δ = 1/2`. -/
theorem rhoStar_interior_example :
    0 < rhoStar (1 / 20) (1 / 10) (1 / 2) 1 ∧ rhoStar (1 / 20) (1 / 10) (1 / 2) 1 < 1 := by
  rw [rhoStar_mem_iff (by norm_num) (by norm_num)]
  rw [← trap_iff_b (by norm_num) (by norm_num) (by norm_num)]
  simpa using Outsider.trapped_hypotheses_example

/-- Control: the threshold need not be interior. At the predecessor's
bifurcation figure, `μ = γ = 1/20`, with `s δ = 1/5`, `ρ* > 1`, so no
intensity in `(0, 1)` gives the trap. -/
theorem control_rhoStar_not_interior :
    1 < rhoStar (1 / 20) (1 / 20) (1 / 5) 1 := by
  unfold rhoStar
  rw [lt_div_iff₀ (by norm_num)]
  linarith [bStar_at_figure.1]

end Intensity
