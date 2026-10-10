/-
================================================================================
  Second-order conditions at the impulse boundaries
  Companion to PROOFS_v2.tex: prop:soc.
================================================================================

At the trigger x_L the interior equation, evaluated from the inaction side
with the smooth fit V'(x_L) = 1 + κ, gives ½σ²V'' = Ψ with
Ψ = ρ_L V + Λ − (1+κ)μ. The quasi-variational inequality on the intervention
side gives Ψ ≥ 0. Both enter as hypotheses in raw form.

PROOFS_v2 also asserts V''(y_post) < 0 strictly, from the sign change of
V' − (1+κ) at y_post. `refuted_target_strict` is a counterexample to that
step: a sign change of φ' at a point is compatible with φ'' = 0 there.
-/

import Mathlib

namespace SecondOrder

noncomputable def psi (rhoL V L kap mu : ℝ) : ℝ := rhoL * V + L - (1 + kap) * mu

theorem psi_identity {s2 Vpp Vp V mu rhoL L kap : ℝ}
    (hint : (1 / 2) * s2 * Vpp + mu * Vp - rhoL * V - L = 0) (hsf : Vp = 1 + kap) :
    (1 / 2) * s2 * Vpp = psi rhoL V L kap mu := by
  subst hsf
  unfold psi
  linear_combination hint

theorem psi_nonneg_of_qvi {mu kap rhoL V L : ℝ} (hqvi : mu * (1 + kap) - rhoL * V - L ≤ 0) :
    0 ≤ psi rhoL V L kap mu := by
  unfold psi
  linarith

theorem trigger_convex {s2 Vpp Vp V mu rhoL L kap : ℝ} (hs : 0 < s2)
    (hint : (1 / 2) * s2 * Vpp + mu * Vp - rhoL * V - L = 0) (hsf : Vp = 1 + kap)
    (hqvi : mu * (1 + kap) - rhoL * V - L ≤ 0) :
    0 ≤ Vpp := by
  have h1 := psi_identity hint hsf
  have h2 := psi_nonneg_of_qvi hqvi
  nlinarith

theorem trigger_strict_iff {s2 Vpp Vp V mu rhoL L kap : ℝ} (hs : 0 < s2)
    (hint : (1 / 2) * s2 * Vpp + mu * Vp - rhoL * V - L = 0) (hsf : Vp = 1 + kap) :
    0 < Vpp ↔ 0 < psi rhoL V L kap mu := by
  have h1 := psi_identity hint hsf
  constructor
  · intro h
    rw [← h1]
    positivity
  · intro h
    rw [← h1] at h
    by_contra hc
    push Not at hc
    nlinarith

theorem control_trigger_flat_when_psi_zero :
    ∃ s2 Vpp Vp V mu rhoL L kap : ℝ, 0 < s2 ∧
      (1 / 2) * s2 * Vpp + mu * Vp - rhoL * V - L = 0 ∧ Vp = 1 + kap ∧
      psi rhoL V L kap mu = 0 ∧ Vpp = 0 :=
  ⟨1, 0, 1, 0, 0, 1, 0, 0, by norm_num, by norm_num, by norm_num, by unfold psi; norm_num, rfl⟩

theorem refuted_target_strict :
    (∀ y : ℝ, HasDerivAt (fun t : ℝ => -(t ^ 4) / 4) (-(y ^ 3)) y) ∧
    (∀ y : ℝ, HasDerivAt (fun t : ℝ => -(t ^ 3)) (-(3 * y ^ 2)) y) ∧
    (∀ y : ℝ, y < 0 → 0 < -(y ^ 3)) ∧ (∀ y : ℝ, 0 < y → -(y ^ 3) < 0) ∧
    -(3 * (0 : ℝ) ^ 2) = 0 := by
  refine ⟨fun y => ?_, fun y => ?_, fun y hy => ?_, fun y hy => ?_, by norm_num⟩
  · have h := (hasDerivAt_pow 4 y).const_mul (-1 / 4 : ℝ)
    have e : (fun t : ℝ => -(t ^ 4) / 4) = fun t => (-1 / 4 : ℝ) * t ^ 4 := by
      funext t
      ring
    rw [e]
    exact h.congr_deriv (by norm_num; ring)
  · have h := (hasDerivAt_pow 3 y).const_mul (-1 : ℝ)
    have e : (fun t : ℝ => -(t ^ 3)) = fun t => (-1 : ℝ) * t ^ 3 := by
      funext t
      ring
    rw [e]
    exact h.congr_deriv (by norm_num)
  · have : y ^ 3 < 0 := Odd.pow_neg (by decide) hy
    linarith
  · have : 0 < y ^ 3 := pow_pos hy 3
    linarith

end SecondOrder
