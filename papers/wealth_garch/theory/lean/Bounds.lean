/-
================================================================================
  Bounds on the z-volatility for every policy that complies with the cap
  Companion to MANUSCRIPT.tex, the bound form of K1.
================================================================================

The z-volatility is a convex quadratic in the exposure ratio ω = πx/(x − 1).
Compliance, 0 ≤ π and πx ≤ u(x), confines ω to [0, 1/a₁] everywhere and to
[0, (x − a₂)/(a₃(x − 1))] deep in surplus, so the volatility is at most the
larger of its values at the ends of that interval. No optimality and no
binding of the cap is assumed.
-/

import CapGeometry

open Real Filter Topology Set

namespace Bounds

noncomputable def qv (σ σL c ω : ℝ) : ℝ := ω ^ 2 * σ ^ 2 - 2 * ω * c * σ * σL + σL ^ 2

theorem qv_zero (σ σL c : ℝ) : qv σ σL c 0 = σL ^ 2 := by
  unfold qv
  ring

theorem nu2_eq_qv {σ σL c a : ℝ} (ha : a ≠ 0) : CapGeometry.nu2 σ σL c a = qv σ σL c (1 / a) := by
  unfold CapGeometry.nu2 qv
  field_simp

theorem qv_le_max {σ σL c M ω : ℝ} (hM : 0 < M) (h0 : 0 ≤ ω) (h1 : ω ≤ M) :
    qv σ σL c ω ≤ max (qv σ σL c 0) (qv σ σL c M) := by
  set t := ω / M with ht
  have ht0 : 0 ≤ t := div_nonneg h0 hM.le
  have ht1 : t ≤ 1 := (div_le_one hM).mpr h1
  have hω : ω = t * M := by rw [ht]; field_simp
  have key : qv σ σL c ω = (1 - t) * qv σ σL c 0 + t * qv σ σL c M + σ ^ 2 * M ^ 2 * (t ^ 2 - t) := by
    rw [hω]
    unfold qv
    ring
  have hneg : σ ^ 2 * M ^ 2 * (t ^ 2 - t) ≤ 0 := by
    have : t ^ 2 - t ≤ 0 := by nlinarith
    have : 0 ≤ σ ^ 2 * M ^ 2 := by positivity
    nlinarith
  have hcomb : (1 - t) * qv σ σL c 0 + t * qv σ σL c M ≤ max (qv σ σL c 0) (qv σ σL c M) := by
    have ha := le_max_left (qv σ σL c 0) (qv σ σL c M)
    have hb := le_max_right (qv σ σL c 0) (qv σ σL c M)
    nlinarith
  linarith

theorem zvol2_eq_qv {σ σL c q x : ℝ} {pol : ℝ → ℝ} (hq : 0 < q) (hx : 1 < x) :
    CapGeometry.zvol2 σ σL c q pol x = qv σ σL c (pol x * x / (x - 1)) := by
  rw [CapGeometry.zvol2_eq hq hx]
  unfold qv
  ring

theorem deficit_bound {σ σL c q a1 a2 a3 x : ℝ} {pol : ℝ → ℝ} (hq : 0 < q) (ha1 : 0 < a1)
    (hx : 1 < x) (hpos : 0 ≤ pol x) (hcap : pol x * x ≤ CapGeometry.capU a1 a2 a3 x) :
    CapGeometry.zvol2 σ σL c q pol x ≤ max (σL ^ 2) (CapGeometry.nu2 σ σL c a1) := by
  have hx1 : 0 < x - 1 := by linarith
  have hω0 : 0 ≤ pol x * x / (x - 1) := div_nonneg (mul_nonneg hpos (by linarith)) hx1.le
  have hω1 : pol x * x / (x - 1) ≤ 1 / a1 := by
    have h := hcap.trans (min_le_left _ _)
    rw [div_le_div_iff₀ hx1 ha1]
    have := (le_div_iff₀ ha1).mp h
    nlinarith
  rw [zvol2_eq_qv hq hx, nu2_eq_qv ha1.ne', ← qv_zero σ σL c]
  exact qv_le_max (by positivity) hω0 hω1

theorem surplus_bound {σ σL c q a1 a2 a3 ε : ℝ} {pol : ℝ → ℝ} (hq : 0 < q) (ha3 : 0 < a3)
    (ha2 : a2 < 1) (hε : 0 < ε)
    (hcomp : ∀ᶠ x in atTop, 0 ≤ pol x ∧ pol x * x ≤ CapGeometry.capU a1 a2 a3 x) :
    ∀ᶠ x in atTop, CapGeometry.zvol2 σ σL c q pol x ≤ max (σL ^ 2) (CapGeometry.nu2 σ σL c a3) + ε := by
  set w : ℝ → ℝ := fun x => 1 / a3 + (1 - a2) / a3 * (x - 1)⁻¹ with hw
  have hinv : Tendsto (fun x : ℝ => (x - 1)⁻¹) atTop (𝓝 0) :=
    tendsto_inv_atTop_zero.comp (tendsto_atTop_add_const_right _ (-1) tendsto_id |>.congr
      (fun x => by simp [sub_eq_add_neg]))
  have hwlim : Tendsto w atTop (𝓝 (1 / a3)) := by
    have := (hinv.const_mul ((1 - a2) / a3)).const_add (1 / a3)
    simpa [hw] using this
  have hc : Continuous (fun t : ℝ => qv σ σL c t) := by
    unfold qv
    fun_prop
  have hpoly : Tendsto (fun x => qv σ σL c (w x)) atTop (𝓝 (CapGeometry.nu2 σ σL c a3)) := by
    rw [nu2_eq_qv ha3.ne']
    exact (hc.tendsto _).comp hwlim
  have hev : ∀ᶠ x in atTop, qv σ σL c (w x) < CapGeometry.nu2 σ σL c a3 + ε :=
    hpoly.eventually (gt_mem_nhds (by linarith))
  filter_upwards [hcomp, hev, eventually_gt_atTop (1 : ℝ)] with x hcx hlt hx
  obtain ⟨hpos, hcap⟩ := hcx
  have hx1 : 0 < x - 1 := by linarith
  have hwpos : 0 < w x := by
    simp only [hw]
    have : 0 < (1 - a2) / a3 * (x - 1)⁻¹ := by
      apply mul_pos (div_pos (by linarith) ha3) (inv_pos.mpr hx1)
    have : 0 < 1 / a3 := by positivity
    linarith
  have hω0 : 0 ≤ pol x * x / (x - 1) := div_nonneg (mul_nonneg hpos (by linarith)) hx1.le
  have hω1 : pol x * x / (x - 1) ≤ w x := by
    have h := hcap.trans (min_le_right _ _)
    have hwx : (x - a2) / a3 / (x - 1) = w x := by
      simp only [hw]
      field_simp
      ring
    rw [← hwx]
    exact div_le_div_of_nonneg_right h hx1.le
  rw [zvol2_eq_qv hq hx]
  have hb := qv_le_max (σ := σ) (σL := σL) (c := c) hwpos hω0 hω1
  rw [qv_zero] at hb
  calc qv σ σL c (pol x * x / (x - 1)) ≤ max (σL ^ 2) (qv σ σL c (w x)) := hb
    _ ≤ max (σL ^ 2) (CapGeometry.nu2 σ σL c a3 + ε) := max_le_max le_rfl hlt.le
    _ ≤ max (σL ^ 2) (CapGeometry.nu2 σ σL c a3) + ε := by
      rcases le_total (σL ^ 2) (CapGeometry.nu2 σ σL c a3) with h | h
      · rw [max_eq_right (by linarith), max_eq_right h]
      · rw [max_eq_left h]
        exact max_le (by linarith) (by linarith)

theorem control_bound_needs_long_exposure :
    ¬ (qv 1 1 1 (-1) ≤ max (qv 1 1 1 0) (qv 1 1 1 1)) := by
  unfold qv
  norm_num

theorem control_bound_needs_compliance :
    ¬ (qv 1 0 0 2 ≤ max (qv 1 0 0 0) (qv 1 0 0 1)) := by
  unfold qv
  norm_num

end Bounds
