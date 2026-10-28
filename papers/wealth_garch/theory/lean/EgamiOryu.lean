import Mathlib

namespace EgamiOryu

/-- Their remark 4.1: with a cost proportional to bank size, C(x, b') = e^x G(b'), so the minimiser
    in the trigger b' does not depend on size. -/
theorem argmin_free_of_size {G : ℝ → ℝ} {s : Set ℝ} {c b₀ : ℝ} (hc : 0 < c) :
    IsMinOn (fun b => c * G b) s b₀ ↔ IsMinOn G s b₀ := by
  simp only [isMinOn_iff]
  refine forall_congr' fun b => forall_congr' fun _ => ?_
  exact mul_le_mul_iff_of_pos_left hc

/-- With zero size every trigger ties, so size cannot be what selects the trigger. -/
theorem control_zero_scale_ties {G : ℝ → ℝ} {s : Set ℝ} {b₀ : ℝ} :
    IsMinOn (fun b => (0 : ℝ) * G b) s b₀ := by
  simp [isMinOn_iff]

end EgamiOryu
