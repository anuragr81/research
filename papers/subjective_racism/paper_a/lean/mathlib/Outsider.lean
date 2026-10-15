import Bifurcation

open Filter Topology

namespace Outsider

open ClosureMap Bifurcation

/-- Two groups, an outsider `O` and a host `I`, each moved by its own composed
map. -/
def step (μ γ b μI γI bI : ℝ) : ℝ × ℝ → ℝ × ℝ :=
  Prod.map (H μ γ b) (H μI γI bI)

/-- The joint orbit is the pair of separate orbits. Neither group's standing
enters the other's map. -/
theorem decoupled (μ γ b μI γI bI : ℝ) (n : ℕ) (x : ℝ × ℝ) :
    (step μ γ b μI γI bI)^[n] x = ((H μ γ b)^[n] x.1, (H μI γI bI)^[n] x.2) := by
  unfold step
  rw [Prod.map_iterate]
  rfl

/-- Row M11 (i). Below the mobility threshold, and under the bounds of M6, an
outsider entering below the tipping point converges to its own closure trap,
whatever the host does, and the host's orbit is its own. -/
theorem outsider_trapped {μ γ b : ℝ} (hμ : 0 < μ) (hγ : 0 ≤ γ) (hb : 0 < b)
    (hthr : γ < gammaStar μ b) (hA : b < 1 - μ + γ) (hμγ : μ + γ < 1) :
    ∃ cL cM, 0 < cL ∧ cL < cM ∧ cM < 1 ∧ H μ γ b cL = cL ∧ H μ γ b cM = cM ∧
      ∀ μI γI bI cI cO, 0 ≤ cO → cO < cM →
        Tendsto (fun n => ((step μ γ b μI γI bI)^[n] (cO, cI)).1) atTop (𝓝 cL) ∧
        ∀ n, ((step μ γ b μI γI bI)^[n] (cO, cI)).2 = (H μI γI bI)^[n] cI := by
  obtain ⟨cL, cM, h0, h12, h1, hf1, hf2⟩ :=
    (two_interior_iff hμ hγ hb).2 ((trap_iff_gamma hμ hb).2 hthr)
  refine ⟨cL, cM, h0, h12, h1, hf1, hf2, fun μI γI bI cI cO hc0 hcM => ⟨?_, ?_⟩⟩
  · simp_rw [decoupled]
    exact (basins hb hA hμγ h0 h12 h1 hf1 hf2).1 cO hc0 hcM
  · intro n; rw [decoupled]

/-- Row M11 (ii). Above the mobility threshold, and under the bounds of M6, every
standing in `[0, 1]` converges to integration. -/
theorem integrates_above {μ γ b : ℝ} (hμ : 0 < μ) (hb : 0 < b)
    (hthr : gammaStar μ b < γ) (hA : b < 1 - μ + γ) (hμγ : μ + γ < 1) :
    ∀ c₀, 0 ≤ c₀ → c₀ ≤ 1 → Tendsto (fun n => (H μ γ b)^[n] c₀) atTop (𝓝 1) := by
  intro c₀ h0 h1
  apply tendsto_up (H_continuous μ γ b) h1 (H_one μ γ b)
  · intro x y hx hxy hy
    rcases eq_or_lt_of_le hxy with h | h
    · rw [h]
    · exact (H_strictMonoOn hb hA hμγ (by linarith) h hy).le
  · intro x hx hx1
    have hQ := Q_pos_above hμ hb hthr (by linarith : 0 ≤ x)
    have hs := H_sub_self μ γ b x
    have : 0 < (1 - x) * Q μ γ b x := mul_pos (by linarith) hQ
    linarith

/-- The hypotheses of `integrates_above` can all hold, at `μ = 1/20`,
`γ = 1/10`, `b = 1/5`. -/
theorem integrates_hypotheses_example :
    gammaStar (1 / 20) (1 / 5) < 1 / 10 ∧ (1 / 5 : ℝ) < 1 - 1 / 20 + 1 / 10 ∧
      (1 / 20 : ℝ) + 1 / 10 < 1 := by
  rw [control_threshold_zero]; norm_num

/-- The hypotheses of `outsider_trapped` can all hold, at `μ = 1/20`,
`γ = 1/10`, `b = 1/2`. -/
theorem trapped_hypotheses_example : (1 / 10 : ℝ) < gammaStar (1 / 20) (1 / 2) := by
  rw [← trap_iff_gamma (by norm_num) (by norm_num)]
  norm_num

/-- Control: below the threshold integration is not reached from every
standing. At `μ = 1/20`, `γ = 1/10`, `b = 1/2`, the orbit of 0 does not
converge to 1. -/
theorem control_threshold_needed :
    ¬ Tendsto (fun n => (H (1 / 20) (1 / 10) (1 / 2))^[n] 0) atTop (𝓝 1) := by
  intro h1
  obtain ⟨cL, cM, h0, h12, h2, hf1, hf2, hall⟩ :=
    outsider_trapped (μ := 1 / 20) (γ := 1 / 10) (b := 1 / 2) (by norm_num) (by norm_num)
      (by norm_num) trapped_hypotheses_example (by norm_num) (by norm_num)
  have hL := (hall 0 0 0 0 0 le_rfl (h0.trans h12)).1
  simp_rw [decoupled] at hL
  have := tendsto_nhds_unique hL h1
  linarith

end Outsider
