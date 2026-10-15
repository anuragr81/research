import Intensity
import ClassPosterior

open Filter Topology

namespace Figures

open ClosureMap Bifurcation

/-! ### Row M17. The predecessor's figures and worked example

Figure 1 of the predecessor (`fig:closure-map`, script
`figures/generate_figures.py`) uses `μ = 0.05`, `δ = 0.5`, `s = 1`, the start
`0.55`, and `γ = 0.05` in panel (a), `γ = 0.30` in panel (b). Figure 2
(`fig:bifurcation-delta`) uses `μ = γ = 0.05`, `s = 1`. -/

/-- At panel (a), the start `11/20` lies strictly between the two interior
fixed points. -/
theorem fig1a_start_between {c₁ c₂ : ℝ} (h12 : c₁ < c₂)
    (hf1 : H (1 / 20) (1 / 20) (1 / 2) c₁ = c₁) (hf2 : H (1 / 20) (1 / 20) (1 / 2) c₂ = c₂)
    (h2 : c₂ < 1) : c₁ < 11 / 20 ∧ 11 / 20 < c₂ := by
  have q1 := (fixed_iff_Q (by linarith : c₁ ≠ 1)).mp hf1
  have q2 := (fixed_iff_Q (by linarith : c₂ ≠ 1)).mp hf2
  obtain ⟨hsum, hprod⟩ := vieta q1 q2 (ne_of_lt h12)
  have hQ : Q (1 / 20) (1 / 20) (1 / 2) (11 / 20) < 0 := by norm_num [Q]
  rw [Q_factor hsum hprod] at hQ
  have hneg : (11 / 20 - c₁) * (11 / 20 - c₂) < 0 := by nlinarith
  rcases lt_or_ge c₁ (11 / 20) with h | h
  · refine ⟨h, ?_⟩
    by_contra h'; push_neg at h'
    nlinarith [mul_nonneg (by linarith : (0 : ℝ) ≤ 11 / 20 - c₁) (by linarith : (0 : ℝ) ≤ 11 / 20 - c₂)]
  · nlinarith [mul_nonneg (by linarith : (0 : ℝ) ≤ c₁ - 11 / 20) (by linarith : (0 : ℝ) ≤ c₂ - 11 / 20)]

/-- Figure 1, panel (a). The orbit of `0.55` converges to the closure trap,
a fixed point below `0.55`. -/
theorem fig1a_trapped :
    ∃ cL, 0 < cL ∧ cL < 11 / 20 ∧ H (1 / 20) (1 / 20) (1 / 2) cL = cL ∧
      Tendsto (fun n => (H (1 / 20) (1 / 20) (1 / 2))^[n] (11 / 20)) atTop (𝓝 cL) := by
  obtain ⟨c₁, c₂, h0, h12, h2, hf1, hf2⟩ :=
    (two_interior_iff (μ := 1 / 20) (γ := 1 / 20) (b := 1 / 2) (by norm_num) (by norm_num)
      (by norm_num)).2 (by norm_num)
  obtain ⟨hlo, hhi⟩ := fig1a_start_between h12 hf1 hf2 h2
  exact ⟨c₁, h0, hlo, hf1,
    (basins (by norm_num) (by norm_num) (by norm_num) h0 h12 h2 hf1 hf2).1 _ (by norm_num) hhi⟩

/-- Figure 1, panel (b). The orbit of `0.55` converges to integration. -/
theorem fig1b_integrates :
    Tendsto (fun n => (H (1 / 20) (3 / 10) (1 / 2))^[n] (11 / 20)) atTop (𝓝 1) := by
  have hthr : gammaStar (1 / 20) (1 / 2) < 3 / 10 := by
    unfold gammaStar
    have : (1 / 10 : ℝ) < Real.sqrt (1 / 20 * (1 / 2)) := by
      rw [show (1 / 10 : ℝ) = Real.sqrt ((1 / 10) ^ 2) by rw [Real.sqrt_sq (by norm_num)]]
      exact Real.sqrt_lt_sqrt (by norm_num) (by norm_num)
    linarith
  exact Outsider.integrates_above (by norm_num) (by norm_num) hthr (by norm_num) (by norm_num)
    _ (by norm_num) (by norm_num)

/-- Control: in panel (a) the same start does not reach integration. -/
theorem control_fig1a_not_integrated :
    ¬ Tendsto (fun n => (H (1 / 20) (1 / 20) (1 / 2))^[n] (11 / 20)) atTop (𝓝 1) := by
  intro h1
  obtain ⟨cL, _, hL, _, hT⟩ := fig1a_trapped
  have := tendsto_nhds_unique hT h1
  linarith

/-- The intensity threshold at Figure 1's panel (a), `ρ* = b*/(s δ)` with
`s δ = 1/2`, lies between `0.58` and `0.60`. The predecessor reports
`ρ* ≈ 0.31` "under base parameters" it does not state. -/
theorem rhoStar_at_fig1 :
    29 / 50 < Intensity.rhoStar (1 / 20) (1 / 20) (1 / 2) 1 ∧
      Intensity.rhoStar (1 / 20) (1 / 20) (1 / 2) 1 < 3 / 5 := by
  unfold Intensity.rhoStar
  obtain ⟨h1, h2⟩ := bStar_at_figure
  constructor
  · rw [lt_div_iff₀ (by norm_num)]; linarith
  · rw [div_lt_iff₀ (by norm_num)]; linarith

/-- The predecessor's worked example of the stranger and the accent. Group
weights `(1/2, 1/2)` mix standings `7/10` and `3/10` to `1/2`, and weights
`(1/5, 4/5)` mix them to `19/50`, the mixture of M2 and M3. -/
theorem stranger_example :
    ClassPosterior.mixture (fun g (_ : Fin 1) => (![7 / 10, 3 / 10] : Fin 2 → ℝ) g)
        ![1 / 2, 1 / 2] 0 = 1 / 2 ∧
      ClassPosterior.mixture (fun g (_ : Fin 1) => (![7 / 10, 3 / 10] : Fin 2 → ℝ) g)
        ![1 / 5, 4 / 5] 0 = 19 / 50 := by
  constructor <;> norm_num [ClassPosterior.mixture, Fin.sum_univ_two]

end Figures
