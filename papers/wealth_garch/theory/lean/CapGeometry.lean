/-
================================================================================
  Saturation limits of the z-volatility under the regulatory cap
  Companion to PROOFS_v2.tex: prop:statemap (ii), prop:satlimits.
================================================================================

The z-volatility is the squared diffusion coefficient of z = log((x-1)/q),
taken as (dz/dx)^2 * sigma^2(x, pi). That this is the diffusion coefficient of
z is Ito's formula, which enters as the definition `zvol2` and is not proved.

The exposure policy enters as a hypothesis: the theorems hold for any policy
that coincides with the capped exposure u near the end in question. Whether
the optimal policy binds the cap there is not proved here.
-/

import Mathlib

open Real Filter Topology Set

namespace CapGeometry

noncomputable def sig2 (σ σL c x p : ℝ) : ℝ :=
  p ^ 2 * σ ^ 2 * x ^ 2 + 2 * p * c * σ * σL * x * (1 - x) + σL ^ 2 * (1 - x) ^ 2

noncomputable def capU (a1 a2 a3 x : ℝ) : ℝ := min ((x - 1) / a1) ((x - a2) / a3)

noncomputable def nu2 (σ σL c a : ℝ) : ℝ := σ ^ 2 / a ^ 2 - 2 * c * σ * σL / a + σL ^ 2

noncomputable def xbar (a1 a2 a3 : ℝ) : ℝ := (a3 - a1 * a2) / (a3 - a1)

noncomputable def zmap (q x : ℝ) : ℝ := Real.log ((x - 1) / q)

noncomputable def zvol2 (σ σL c q : ℝ) (pol : ℝ → ℝ) (x : ℝ) : ℝ :=
  (deriv (zmap q) x) ^ 2 * sig2 σ σL c x (pol x)

theorem hasDerivAt_zmap {q x : ℝ} (hq : 0 < q) (hx : 1 < x) :
    HasDerivAt (zmap q) (1 / (x - 1)) x := by
  have hx1 : x - 1 ≠ 0 := by linarith
  have hpos : 0 < (x - 1) / q := div_pos (by linarith) hq
  have hin : HasDerivAt (fun y => (y - 1) / q) (1 / q) x := by
    simpa using ((hasDerivAt_id x).sub_const 1).div_const q
  have := hin.log hpos.ne'
  unfold zmap
  convert this using 1
  field_simp

theorem zmap_inverse {q x : ℝ} (hq : 0 < q) (hx : 1 < x) : 1 + q * Real.exp (zmap q x) = x := by
  unfold zmap
  rw [Real.exp_log (div_pos (by linarith) hq)]
  field_simp
  ring

theorem zmap_of_inverse {q z : ℝ} (hq : 0 < q) : zmap q (1 + q * Real.exp z) = z := by
  unfold zmap
  rw [show 1 + q * Real.exp z - 1 = q * Real.exp z by ring, mul_div_cancel_left₀ _ hq.ne']
  exact Real.log_exp z

theorem xbar_sub_one {a1 a2 a3 : ℝ} (h13 : a1 < a3) :
    xbar a1 a2 a3 - 1 = a1 * (1 - a2) / (a3 - a1) := by
  unfold xbar
  have : a3 - a1 ≠ 0 := by linarith
  field_simp
  ring

theorem one_lt_xbar {a1 a2 a3 : ℝ} (ha1 : 0 < a1) (h13 : a1 < a3) (ha2 : a2 < 1) :
    1 < xbar a1 a2 a3 := by
  have h := xbar_sub_one (a2 := a2) h13
  have : 0 < a1 * (1 - a2) / (a3 - a1) := div_pos (mul_pos ha1 (by linarith)) (by linarith)
  linarith

theorem solvency_binds {a1 a2 a3 x : ℝ} (ha1 : 0 < a1) (h13 : a1 < a3)
    (hx : x ≤ xbar a1 a2 a3) : capU a1 a2 a3 x = (x - 1) / a1 := by
  unfold capU
  apply min_eq_left
  have ha3 : 0 < a3 := by linarith
  have hd : 0 < a3 - a1 := by linarith
  unfold xbar at hx
  rw [le_div_iff₀ hd] at hx
  rw [div_le_div_iff₀ ha1 ha3]
  nlinarith

theorem liquidity_binds {a1 a2 a3 x : ℝ} (ha1 : 0 < a1) (h13 : a1 < a3)
    (hx : xbar a1 a2 a3 ≤ x) : capU a1 a2 a3 x = (x - a2) / a3 := by
  unfold capU
  apply min_eq_right
  have ha3 : 0 < a3 := by linarith
  have hd : 0 < a3 - a1 := by linarith
  unfold xbar at hx
  rw [div_le_iff₀ hd] at hx
  rw [div_le_div_iff₀ ha3 ha1]
  nlinarith

theorem zvol2_eq {σ σL c q x : ℝ} {pol : ℝ → ℝ} (hq : 0 < q) (hx : 1 < x) :
    zvol2 σ σL c q pol x =
      (pol x * x / (x - 1)) ^ 2 * σ ^ 2 - 2 * (pol x * x / (x - 1)) * c * σ * σL + σL ^ 2 := by
  unfold zvol2 sig2
  rw [(hasDerivAt_zmap hq hx).deriv]
  have hx1 : x - 1 ≠ 0 := by linarith
  field_simp
  ring

theorem zvol2_solvency {σ σL c q a1 a2 a3 x : ℝ} {pol : ℝ → ℝ} (hq : 0 < q) (ha1 : 0 < a1)
    (h13 : a1 < a3) (hx : 1 < x) (hxb : x ≤ xbar a1 a2 a3) (hpol : pol x * x = capU a1 a2 a3 x) :
    zvol2 σ σL c q pol x = nu2 σ σL c a1 := by
  rw [zvol2_eq hq hx, hpol, solvency_binds ha1 h13 hxb]
  unfold nu2
  have hx1 : x - 1 ≠ 0 := by linarith
  field_simp

theorem deficit_limit {σ σL c q a1 a2 a3 : ℝ} {pol : ℝ → ℝ} (hq : 0 < q) (ha1 : 0 < a1)
    (h13 : a1 < a3) (ha2 : a2 < 1)
    (hpol : ∀ᶠ x in 𝓝[>] 1, pol x * x = capU a1 a2 a3 x) :
    Tendsto (zvol2 σ σL c q pol) (𝓝[>] 1) (𝓝 (nu2 σ σL c a1)) := by
  have hlt : ∀ᶠ x in 𝓝[>] (1:ℝ), x < xbar a1 a2 a3 :=
    (eventually_lt_nhds (one_lt_xbar ha1 h13 ha2)).filter_mono nhdsWithin_le_nhds
  have hgt : ∀ᶠ x in 𝓝[>] (1:ℝ), 1 < x := eventually_nhdsWithin_of_forall fun x hx => hx
  refine tendsto_const_nhds.congr' ?_
  filter_upwards [hlt, hgt, hpol] with x h1 h2 h3
  exact (zvol2_solvency hq ha1 h13 h2 h1.le h3).symm

theorem surplus_limit {σ σL c q a1 a2 a3 : ℝ} {pol : ℝ → ℝ} (hq : 0 < q) (ha1 : 0 < a1)
    (h13 : a1 < a3) (hpol : ∀ᶠ x in atTop, pol x * x = capU a1 a2 a3 x) :
    Tendsto (zvol2 σ σL c q pol) atTop (𝓝 (nu2 σ σL c a3)) := by
  have ha3 : 0 < a3 := by linarith
  set w : ℝ → ℝ := fun x => 1 / a3 + (1 - a2) / a3 * (x - 1)⁻¹ with hw
  have hinv : Tendsto (fun x : ℝ => (x - 1)⁻¹) atTop (𝓝 0) :=
    tendsto_inv_atTop_zero.comp (tendsto_atTop_add_const_right _ (-1) tendsto_id |>.congr
      (fun x => by simp [sub_eq_add_neg]))
  have hwlim : Tendsto w atTop (𝓝 (1 / a3)) := by
    have := (hinv.const_mul ((1 - a2) / a3)).const_add (1 / a3)
    simpa [hw] using this
  have hpoly : Tendsto (fun x => w x ^ 2 * σ ^ 2 - 2 * w x * c * σ * σL + σL ^ 2) atTop
      (𝓝 ((1 / a3) ^ 2 * σ ^ 2 - 2 * (1 / a3) * c * σ * σL + σL ^ 2)) := by
    have hc : Continuous (fun t : ℝ => t ^ 2 * σ ^ 2 - 2 * t * c * σ * σL + σL ^ 2) := by
      fun_prop
    exact (hc.tendsto _).comp hwlim
  have hk : (1 / a3) ^ 2 * σ ^ 2 - 2 * (1 / a3) * c * σ * σL + σL ^ 2 = nu2 σ σL c a3 := by
    unfold nu2
    field_simp
  rw [hk] at hpoly
  refine hpoly.congr' ?_
  filter_upwards [hpol, eventually_ge_atTop (xbar a1 a2 a3), eventually_gt_atTop (1:ℝ)]
    with x h1 h2 h3
  rw [zvol2_eq hq h3, h1, liquidity_binds ha1 h13 h2]
  have hx1 : x - 1 ≠ 0 := by linarith
  have hwx : (x - a2) / a3 / (x - 1) = w x := by
    simp only [hw]
    field_simp
    ring
  rw [hwx]

theorem variance_ratio_is_cap_geometry {σ σL c q a1 a2 a3 : ℝ} {pol : ℝ → ℝ}
    (hq : 0 < q) (ha1 : 0 < a1) (h13 : a1 < a3) (ha2 : a2 < 1)
    (hlo : ∀ᶠ x in 𝓝[>] 1, pol x * x = capU a1 a2 a3 x)
    (hhi : ∀ᶠ x in atTop, pol x * x = capU a1 a2 a3 x) :
    Tendsto (zvol2 σ σL c q pol) (𝓝[>] 1) (𝓝 (nu2 σ σL c a1)) ∧
      Tendsto (zvol2 σ σL c q pol) atTop (𝓝 (nu2 σ σL c a3)) :=
  ⟨deficit_limit hq ha1 h13 ha2 hlo, surplus_limit hq ha1 h13 hhi⟩

theorem control_surplus_limit_needs_a1_lt_a3 :
    ¬ Tendsto (zvol2 1 0 0 1 (fun x => capU (1/2) 0 (1/4) x / x)) atTop
      (𝓝 (nu2 1 0 0 (1/4))) := by
  intro h
  have hsolv : ∀ᶠ x in atTop, zvol2 1 0 0 1 (fun x => capU (1/2) 0 (1/4) x / x) x = 4 := by
    filter_upwards [eventually_gt_atTop (1:ℝ)] with x hx
    rw [zvol2_eq one_pos hx]
    have hx0 : x ≠ 0 := by linarith
    have hx1 : x - 1 ≠ 0 := by linarith
    have hmin : capU (1/2) 0 (1/4) x = (x - 1) / (1/2) := by
      unfold capU
      apply min_eq_left
      rw [div_le_div_iff₀ (by norm_num) (by norm_num)]
      nlinarith
    rw [div_mul_cancel₀ _ hx0, hmin]
    field_simp
    ring
  have h4 : Tendsto (zvol2 1 0 0 1 (fun x => capU (1/2) 0 (1/4) x / x)) atTop (𝓝 4) :=
    tendsto_const_nhds.congr' (hsolv.mono fun x hx => hx.symm)
  have := tendsto_nhds_unique h h4
  unfold nu2 at this
  norm_num at this

theorem control_constancy_needs_binding :
    zvol2 1 0 0 1 (fun _ => 0) 2 ≠ nu2 1 0 0 (1/2) := by
  rw [zvol2_eq one_pos (by norm_num : (1:ℝ) < 2)]
  unfold nu2
  norm_num

end CapGeometry
