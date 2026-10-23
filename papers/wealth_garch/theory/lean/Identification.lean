/-
================================================================================
  Threshold comparative statics and joint local identification
  Companion to PROOFS_v2.tex: prop:tcs, prop:kcs, cor:idn.
================================================================================

Every quantity is a real number at a boundary point: W, N and their
derivatives at y*, x_L, y_post; V'', V''' there; σ², ρ_L, μ'. The analytic
inputs enter as hypotheses in raw form: the envelope identities
∂_θ V = −W (θ = λ_S) and ∂_θ V = −N (θ = K) and their x-derivatives; the
chain rule along each boundary (the implicit function theorem); the
differentiated interior equation at y*; and the signs of W, N, N', W'(x_L),
V''(x_L) and V''(y_post), which come from hitting-time arguments and the
second-order conditions and are not proved here.
-/

import Mathlib

namespace Identification

theorem vppp_at_barrier {s2 s2p mu mup rhoL Vpp Vp Vppp Lp : ℝ}
    (hint : (1 / 2) * s2 * Vppp + (1 / 2) * s2p * Vpp + mu * Vpp + mup * Vp - rhoL * Vp - Lp = 0)
    (hVpp : Vpp = 0) (hVp : Vp = 1) (hLp : Lp = 0) :
    (1 / 2) * s2 * Vppp = rhoL - mup := by
  subst hVpp hVp hLp
  linear_combination hint

theorem fpp_at_barrier {s2 mu rhoL F Fp Fpp g : ℝ}
    (hode : (1 / 2) * s2 * Fpp + mu * Fp - rhoL * F + g = 0) (hFp : Fp = 0) (hg : g = 0) :
    (1 / 2) * s2 * Fpp = rhoL * F := by
  subst hFp hg
  linear_combination hode

theorem barrier_slope {s2 rhoL mup F Fpp Vppp dVpp dy : ℝ} (hs : 0 < s2) (hdeg : rhoL ≠ mup)
    (hchain : Vppp * dy + dVpp = 0) (henv : dVpp = -Fpp)
    (hF : (1 / 2) * s2 * Fpp = rhoL * F) (hV : (1 / 2) * s2 * Vppp = rhoL - mup) :
    dy = rhoL * F / (rhoL - mup) := by
  subst henv
  have hd : rhoL - mup ≠ 0 := sub_ne_zero.mpr hdeg
  have hVppp : Vppp = 2 * (rhoL - mup) / s2 := by field_simp; linarith
  have hFpp : Fpp = 2 * (rhoL * F) / s2 := by field_simp; linarith
  rw [hVppp, hFpp] at hchain
  field_simp at hchain ⊢
  linarith

theorem barrier_slope_pos {rhoL mup F : ℝ} (hrho : 0 < rhoL) (hgap : mup < rhoL) (hF : 0 < F) :
    0 < rhoL * F / (rhoL - mup) :=
  div_pos (mul_pos hrho hF) (by linarith)

theorem boundary_slope {Vpp Fp dx dVp : ℝ} (hVpp : Vpp ≠ 0)
    (hchain : Vpp * dx + dVp = 0) (henv : dVp = -Fp) :
    dx = Fp / Vpp := by
  subst henv
  field_simp
  linarith

theorem trigger_rises_in_lambda {Wp Vpp : ℝ} (hW : 0 < Wp) (hV : 0 < Vpp) : 0 < Wp / Vpp :=
  div_pos hW hV

theorem trigger_falls_in_K {Np Vpp : ℝ} (hN : Np < 0) (hV : 0 < Vpp) : Np / Vpp < 0 :=
  div_neg_of_neg_of_pos hN hV

theorem target_rises_in_K {Np Vpp : ℝ} (hN : Np < 0) (hV : Vpp < 0) : 0 < Np / Vpp :=
  div_pos_of_neg_of_neg hN hV

theorem gap_increasing_in_K {Np_x Vpp_x Np_y Vpp_y : ℝ} (hNx : Np_x < 0) (hVx : 0 < Vpp_x)
    (hNy : Np_y < 0) (hVy : Vpp_y < 0) :
    0 < Np_y / Vpp_y - Np_x / Vpp_x := by
  have h1 := target_rises_in_K hNy hVy
  have h2 := trigger_falls_in_K hNx hVx
  linarith

theorem det_formula {rhoL mup W N Wp Np Vpp : ℝ} (hd : rhoL - mup ≠ 0) (hV : Vpp ≠ 0) :
    (rhoL * W / (rhoL - mup)) * (Np / Vpp) - (rhoL * N / (rhoL - mup)) * (Wp / Vpp)
      = rhoL / ((rhoL - mup) * Vpp) * (W * Np - N * Wp) := by
  field_simp

theorem det_neg_of_opposite {a11 a12 a21 a22 : ℝ} (h11 : 0 < a11) (h12 : 0 < a12)
    (h21 : 0 < a21) (h22 : a22 < 0) : a11 * a22 - a12 * a21 < 0 := by
  nlinarith [mul_pos h12 h21, mul_neg_of_pos_of_neg h11 h22]

theorem det_neg {rhoL mup W N Wp Np Vpp : ℝ} (hrho : 0 < rhoL) (hgap : mup < rhoL)
    (hW : 0 < W) (hN : 0 < N) (hWp : 0 < Wp) (hNp : Np < 0) (hV : 0 < Vpp) :
    (rhoL * W / (rhoL - mup)) * (Np / Vpp) - (rhoL * N / (rhoL - mup)) * (Wp / Vpp) < 0 :=
  det_neg_of_opposite (barrier_slope_pos hrho hgap hW) (barrier_slope_pos hrho hgap hN)
    (trigger_rises_in_lambda hWp hV) (trigger_falls_in_K hNp hV)

theorem control_same_direction_can_fail :
    ∃ a11 a12 a21 a22 : ℝ, 0 < a11 ∧ 0 < a12 ∧ 0 < a21 ∧ 0 < a22 ∧ a11 * a22 - a12 * a21 = 0 :=
  ⟨1, 1, 1, 1, by norm_num, by norm_num, by norm_num, by norm_num, by norm_num⟩

theorem control_barrier_sign_needs_gap :
    ∃ rhoL mup F : ℝ, 0 < rhoL ∧ 0 < F ∧ rhoL < mup ∧ rhoL * F / (rhoL - mup) < 0 :=
  ⟨1, 2, 1, by norm_num, by norm_num, by norm_num, by norm_num⟩

theorem control_trigger_slope_needs_curvature :
    ∃ dx₁ dx₂ : ℝ, dx₁ ≠ dx₂ ∧ (0 : ℝ) * dx₁ + 0 = 0 ∧ (0 : ℝ) * dx₂ + 0 = 0 :=
  ⟨0, 1, by norm_num, by norm_num, by norm_num⟩

end Identification
