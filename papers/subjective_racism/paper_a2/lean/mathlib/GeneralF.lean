import ClosureMap

open Filter Topology

namespace GeneralF

open ClosureMap

/-! ### Row M12. The trap for a general discrimination-rate function -/

theorem Hf_sub_self (F : ℝ → ℝ) (μ γ δ s c : ℝ) :
    Hf F μ γ δ s c - c = (1 - c) * (μ + γ * c) - s * δ * c * F c := by
  unfold Hf; ring

/-- With `F 1 = 0` and `F' 1 = 0`, the map lies above the diagonal at some
standing between any `c < 1` and 1. -/
theorem rise_near_one {F : ℝ → ℝ} {μ γ δ s c : ℝ} (hμ : 0 < μ) (hγ : 0 ≤ γ)
    (hsd : 0 ≤ s * δ) (hF1 : F 1 = 0) (hF' : HasDerivAt F 0 1)
    (hc0 : 0 ≤ c) (hc : c < 1) :
    ∃ d, c < d ∧ d < 1 ∧ d < Hf F μ γ δ s d := by
  rw [hasDerivAt_iff_tendsto_slope] at hF'
  have hε : 0 < μ / (2 * (s * δ + 1)) := by positivity
  obtain ⟨r, hr, hall⟩ := Metric.tendsto_nhdsWithin_nhds.1 hF' _ hε
  set d := (max c (1 - r) + 1) / 2 with hd
  have hmax1 : max c (1 - r) < 1 := max_lt hc (by linarith)
  have hcd : c < d := by
    have := le_max_left c (1 - r); rw [hd]; linarith
  have hd1 : d < 1 := by rw [hd]; linarith
  have hd0 : 0 ≤ d := by linarith
  have hdr : dist d 1 < r := by
    rw [Real.dist_eq, abs_of_neg (by linarith)]
    have := le_max_right c (1 - r); rw [hd]; linarith
  have hslope := hall (Set.mem_compl_singleton_iff.2 (ne_of_lt hd1)) hdr
  rw [Real.dist_eq, sub_zero, slope_def_field, hF1, sub_zero, abs_div,
    abs_of_neg (by linarith : d - 1 < 0), neg_sub] at hslope
  have hFd : |F d| < μ / (2 * (s * δ + 1)) * (1 - d) :=
    (div_lt_iff₀ (by linarith)).1 hslope
  refine ⟨d, hcd, hd1, ?_⟩
  have hsub := Hf_sub_self F μ γ δ s d
  have h1 : s * δ * d * F d ≤ s * δ * |F d| := by
    have : F d ≤ |F d| := le_abs_self _
    have : 0 ≤ s * δ * d := mul_nonneg hsd hd0
    nlinarith [abs_nonneg (F d), mul_nonneg hsd (abs_nonneg (F d))]
  have h2 : s * δ * |F d| ≤ (s * δ + 1) * |F d| := by nlinarith [abs_nonneg (F d)]
  have h3 : (s * δ + 1) * |F d| < (s * δ + 1) * (μ / (2 * (s * δ + 1)) * (1 - d)) :=
    mul_lt_mul_of_pos_left hFd (by linarith)
  have h4 : (s * δ + 1) * (μ / (2 * (s * δ + 1)) * (1 - d)) = μ * (1 - d) / 2 := by
    field_simp
  have h5 : 0 ≤ (1 - d) * (γ * d) := mul_nonneg (by linarith) (mul_nonneg hγ hd0)
  nlinarith

/-- Row M12. A dip below the diagonal at `c`, and a rise above it at `d > c`,
give a fixed point in `(0, c)` and one in `(c, d)`, for every `F` continuous
on `[0, 1]`. -/
theorem two_fixed_of_dip {F : ℝ → ℝ} {μ γ δ s c d : ℝ}
    (hF : ContinuousOn F (Set.Icc 0 1)) (hμ : 0 < μ)
    (hc0 : 0 < c) (hcd : c < d) (hd1 : d < 1)
    (hdip : Hf F μ γ δ s c < c) (hrise : d < Hf F μ γ δ s d) :
    ∃ c₁ c₂, 0 < c₁ ∧ c₁ < c ∧ c < c₂ ∧ c₂ < d ∧
      Hf F μ γ δ s c₁ = c₁ ∧ Hf F μ γ δ s c₂ = c₂ := by
  have hgc : ContinuousOn (fun x => Hf F μ γ δ s x - x) (Set.Icc 0 1) := by
    unfold Hf; fun_prop
  have hg0 : Hf F μ γ δ s 0 - 0 = μ := by rw [Hf_zero]; ring
  have hmem1 : (0 : ℝ) ∈ Set.Ioo (Hf F μ γ δ s c - c) (Hf F μ γ δ s 0 - 0) := by
    rw [hg0]; exact ⟨by linarith, hμ⟩
  obtain ⟨c₁, ⟨h10, h1c⟩, hg1⟩ := intermediate_value_Ioo' hc0.le
    (hgc.mono (Set.Icc_subset_Icc le_rfl (by linarith))) hmem1
  have hmem2 : (0 : ℝ) ∈ Set.Ioo (Hf F μ γ δ s c - c) (Hf F μ γ δ s d - d) :=
    ⟨by linarith, by linarith⟩
  obtain ⟨c₂, ⟨h2c, h2d⟩, hg2⟩ := intermediate_value_Ioo hcd.le
    (hgc.mono (Set.Icc_subset_Icc hc0.le hd1.le)) hmem2
  simp only at hg1 hg2
  exact ⟨c₁, c₂, h10, h1c, h2c, h2d, by linarith, by linarith⟩

/-- For the canonical `F` the trap condition supplies the dip, at the midpoint
of the two interior fixed points. -/
theorem dip_of_trap {μ γ b c₁ c₂ : ℝ} (hb : 0 < b)
    (h12 : c₁ < c₂) (h2 : c₂ < 1)
    (hf1 : H μ γ b c₁ = c₁) (hf2 : H μ γ b c₂ = c₂) :
    H μ γ b ((c₁ + c₂) / 2) < (c₁ + c₂) / 2 := by
  have q1 := (fixed_iff_Q (by linarith : c₁ ≠ 1)).mp hf1
  have q2 := (fixed_iff_Q (by linarith : c₂ ≠ 1)).mp hf2
  obtain ⟨hsum, hprod⟩ := vieta q1 q2 (ne_of_lt h12)
  have key := H_sub_self μ γ b ((c₁ + c₂) / 2)
  rw [Q_factor hsum hprod] at key
  have : (1 - (c₁ + c₂) / 2) * (b * ((c₁ + c₂) / 2 - c₁) * ((c₁ + c₂) / 2 - c₂)) < 0 := by
    apply mul_neg_of_pos_of_neg (by linarith)
    apply mul_neg_of_pos_of_neg (mul_pos hb (by linarith)) (by linarith)
  linarith

/-- Control: without a dip there is no interior fixed point. With `F ≡ 0` the
map lies above the diagonal on `[0, 1)`. -/
theorem control_dip_needed {μ γ δ s c : ℝ} (hμ : 0 < μ) (hγ : 0 ≤ γ)
    (hc0 : 0 ≤ c) (hc1 : c < 1) :
    c < Hf (fun _ => 0) μ γ δ s c := by
  have h := Hf_sub_self (fun _ => 0) μ γ δ s c
  simp only [mul_zero, sub_zero] at h
  have : 0 < (1 - c) * (μ + γ * c) :=
    mul_pos (by linarith) (by nlinarith [mul_nonneg hγ hc0])
  linarith

end GeneralF
