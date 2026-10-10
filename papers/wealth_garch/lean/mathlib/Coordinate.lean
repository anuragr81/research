/-
================================================================================
  The log coordinate and the distress boundary
  Companion to PROOFS_v2.tex: prop:statemap (i), (iii), prop:rrL.
================================================================================

z = log((x − 1)/q) maps (1, ∞) onto ℝ. The drift of z is taken as
B(x)/(x − 1) − ν₁²/2, its Ito form, which is not proved here. The boundary
classification of x = 1 (whether it is reached) is not formalised.
-/

import CapGeometry
import BCVW

open Real Filter Topology Set

namespace Coordinate

noncomputable def cappedDrift (r μs μL γ a1 a2 a3 x : ℝ) : ℝ :=
  CapGeometry.capU a1 a2 a3 x * (μs - r) + (r - μL) * x + γ

theorem zmap_strictMonoOn {q : ℝ} (hq : 0 < q) : StrictMonoOn (CapGeometry.zmap q) (Ioi 1) := by
  intro x hx y hy hxy
  simp only [mem_Ioi] at hx hy
  unfold CapGeometry.zmap
  apply Real.log_lt_log (div_pos (by linarith) hq)
  exact div_lt_div_of_pos_right (by linarith) hq

theorem zmap_at_requirement {q : ℝ} (hq : 0 < q) : CapGeometry.zmap q (1 + q) = 0 := by
  unfold CapGeometry.zmap
  rw [show 1 + q - 1 = q by ring, div_self hq.ne', Real.log_one]

theorem zmap_tendsto_atBot {q : ℝ} (hq : 0 < q) :
    Tendsto (CapGeometry.zmap q) (𝓝[>] 1) atBot := by
  have h1 : Tendsto (fun x : ℝ => (x - 1) / q) (𝓝[>] 1) (𝓝[>] 0) := by
    apply tendsto_nhdsWithin_of_tendsto_nhds_of_eventually_within
    · have : Tendsto (fun x : ℝ => (x - 1) / q) (𝓝 1) (𝓝 ((1 - 1) / q)) :=
        ((continuous_id.sub continuous_const).div_const q).tendsto 1
      simpa using this.mono_left nhdsWithin_le_nhds
    · filter_upwards [self_mem_nhdsWithin] with x hx
      exact div_pos (by simp only [mem_Ioi] at hx; linarith) hq
  exact Real.tendsto_log_nhdsGT_zero.comp h1

theorem zmap_tendsto_atTop {q : ℝ} (hq : 0 < q) :
    Tendsto (CapGeometry.zmap q) atTop atTop := by
  have h1 : Tendsto (fun x : ℝ => (x - 1) / q) atTop atTop :=
    (tendsto_atTop_add_const_right _ (-1) tendsto_id |>.congr
      (fun x => by simp [sub_eq_add_neg])).atTop_div_const hq
  exact Real.tendsto_log_atTop.comp h1

theorem cappedDrift_at_one {r μs μL γ rL a1 a2 a3 : ℝ} (ha3 : 0 < a3) (ha2 : a2 < 1)
    (ha1 : 0 < a1) (hμL : μL = γ + rL) :
    cappedDrift r μs μL γ a1 a2 a3 1 = r - rL := by
  unfold cappedDrift
  have hu : CapGeometry.capU a1 a2 a3 1 = 0 := by
    have := BCVW.piBar_mul_y (a2 := a2) ha1 ha3 one_pos
    rw [← this, BCVW.piBar_at_one ha3 ha2]
    ring
  rw [hu, hμL]
  ring

theorem zdrift_tendsto_atTop {B : ℝ → ℝ} {b1 : ℝ} (hB : Tendsto B (𝓝[>] 1) (𝓝 b1)) (hb : 0 < b1) :
    Tendsto (fun x => B x / (x - 1)) (𝓝[>] 1) atTop := by
  have hinv : Tendsto (fun x : ℝ => (x - 1)⁻¹) (𝓝[>] 1) atTop := by
    have h1 : Tendsto (fun x : ℝ => x - 1) (𝓝[>] 1) (𝓝[>] 0) := by
      apply tendsto_nhdsWithin_of_tendsto_nhds_of_eventually_within
      · have : Tendsto (fun x : ℝ => x - 1) (𝓝 1) (𝓝 (1 - 1)) :=
          (continuous_id.sub continuous_const).tendsto 1
        simpa using this.mono_left nhdsWithin_le_nhds
      · filter_upwards [self_mem_nhdsWithin] with x hx
        simp only [mem_Ioi] at hx ⊢
        linarith
    exact tendsto_inv_nhdsGT_zero.comp h1
  simpa [div_eq_mul_inv] using hB.pos_mul_atTop hb hinv

theorem control_zdrift_needs_positive_limit :
    Tendsto (fun x : ℝ => (-1 : ℝ) / (x - 1)) (𝓝[>] 1) atBot := by
  have hinv : Tendsto (fun x : ℝ => (x - 1)⁻¹) (𝓝[>] 1) atTop := by
    have h1 : Tendsto (fun x : ℝ => x - 1) (𝓝[>] 1) (𝓝[>] 0) := by
      apply tendsto_nhdsWithin_of_tendsto_nhds_of_eventually_within
      · have : Tendsto (fun x : ℝ => x - 1) (𝓝 1) (𝓝 (1 - 1)) :=
          (continuous_id.sub continuous_const).tendsto 1
        simpa using this.mono_left nhdsWithin_le_nhds
      · filter_upwards [self_mem_nhdsWithin] with x hx
        simp only [mem_Ioi] at hx ⊢
        linarith
    exact tendsto_inv_nhdsGT_zero.comp h1
  have := tendsto_neg_atTop_atBot.comp hinv
  refine this.congr (fun x => ?_)
  simp [div_eq_mul_inv]

end Coordinate
