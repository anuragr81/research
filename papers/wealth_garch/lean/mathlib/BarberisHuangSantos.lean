/-
================================================================================
  Barberis, Huang and Santos, "Prospect Theory and Asset Prices", in NBER
  Working Paper 7220 (July 1999) and in the Quarterly Journal of Economics
  116(1) (2001). What lit/barberis_huang_santos_1999/ and
  lit/barberis_huang_santos_2001/ attribute to the paper, in its notation
  (λ loss aversion, f the price-dividend ratio, g and σ the mean and
  volatility of log growth, ε the standard normal shock). The working
  paper's eq. (15) and the journal's eq. (46) have the same form, with
  consumption growth in the first and dividend growth in the second.
================================================================================
-/

import Mathlib

open Real

namespace BarberisHuangSantos

noncomputable def v (lam X : ℝ) : ℝ := if 0 ≤ X then X else lam * X

noncomputable def logReturn (f g σ ε : ℝ) : ℝ := Real.log ((1 + f) / f * Real.exp (g + σ * ε))

noncomputable def logReturnState (f₀ f₁ g σ ε : ℝ) : ℝ := Real.log ((1 + f₁) / f₀ * Real.exp (g + σ * ε))

noncomputable def lamOf (lam0 k z : ℝ) : ℝ := lam0 + k * (z - 1)

theorem v_homogeneous {lam S X : ℝ} (hS : 0 < S) : v lam (S * X) = S * v lam X := by
  unfold v
  by_cases hX : 0 ≤ X
  · rw [if_pos hX, if_pos (mul_nonneg hS.le hX)]
  · have : ¬ 0 ≤ S * X := not_le.mpr (mul_neg_of_pos_of_neg hS (not_le.mp hX))
    rw [if_neg hX, if_neg this]
    ring

theorem v_eq_min {lam X : ℝ} (hlam : 1 ≤ lam) : v lam X = min X (lam * X) := by
  unfold v
  by_cases hX : 0 ≤ X
  · rw [if_pos hX, min_eq_left (by nlinarith)]
  · have hX' : X < 0 := not_le.mp hX
    rw [if_neg hX, min_eq_right (by nlinarith)]

theorem v_concave {lam : ℝ} (hlam : 1 ≤ lam) : ConcaveOn ℝ Set.univ (v lam) := by
  have h1 : ConcaveOn ℝ Set.univ (fun X : ℝ => X) := concaveOn_id convex_univ
  have h2 : ConcaveOn ℝ Set.univ (fun X : ℝ => lam * X) :=
    (LinearMap.concaveOn (LinearMap.id.smulRight lam : ℝ →ₗ[ℝ] ℝ) convex_univ).congr
      fun X _ => by simp [mul_comm]
  exact (h1.inf h2).congr fun X _ => (v_eq_min hlam).symm

theorem logReturn_eq {f g σ ε : ℝ} (hf : 0 < f) :
    logReturn f g σ ε = Real.log ((1 + f) / f) + g + σ * ε := by
  unfold logReturn
  rw [Real.log_mul (by positivity) (Real.exp_pos _).ne', Real.log_exp]
  ring

theorem dispersion_free_of_f {f g σ ε₁ ε₂ : ℝ} (hf : 0 < f) :
    logReturn f g σ ε₁ - logReturn f g σ ε₂ = σ * (ε₁ - ε₂) := by
  rw [logReturn_eq hf, logReturn_eq hf]
  ring

theorem lam_after_ten_percent_fall : lamOf (225 / 100) 50 (11 / 10) = 725 / 100 := by
  unfold lamOf
  norm_num

theorem control_dispersion_depends_on_state :
    logReturnState 1 3 0 0 0 - logReturnState 1 1 0 0 0 ≠ 0 := by
  unfold logReturnState
  simp only [mul_zero, add_zero, Real.exp_zero, mul_one, div_one]
  norm_num
  intro h
  have := Real.log_injOn_pos (Set.mem_Ioi.mpr (by norm_num : (0:ℝ) < 4))
    (Set.mem_Ioi.mpr (by norm_num : (0:ℝ) < 2)) (sub_eq_zero.mp h)
  norm_num at this

theorem control_concavity_needs_lam_ge_one : ¬ ConcaveOn ℝ Set.univ (v (1 / 2)) := by
  intro h
  have := h.2 (Set.mem_univ (-1)) (Set.mem_univ 1) (by norm_num : (0:ℝ) ≤ 1 / 2)
    (by norm_num : (0:ℝ) ≤ 1 / 2) (by norm_num)
  unfold v at this
  norm_num at this

end BarberisHuangSantos
