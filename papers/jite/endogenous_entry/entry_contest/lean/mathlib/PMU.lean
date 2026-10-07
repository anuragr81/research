import Mathlib
import EntryContestModel
import RepresentationFOSD

open MeasureTheory ProbabilityTheory Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestPMU

open EntryContestModel EntryContestP1P2

theorem integrable_bdd {Ω : Type*} [MeasurableSpace Ω] (ρ : Measure Ω) [IsFiniteMeasure ρ]
    (f : Ω → ℝ) (hf : Measurable f) (B : ℝ) (hB : ∀ x, |f x| ≤ B) : Integrable f ρ :=
  (integrable_const B).mono' hf.aestronglyMeasurable
    (ae_of_all _ (fun x => by rw [Real.norm_eq_abs]; exact hB x))

section Kernel

/-- **The kernel is hump-shaped.** `g ↦ g^n (1 − g)` rises on `[0, n/(n+1)]`. -/
theorem kernel_rises (n : ℕ) :
    MonotoneOn (fun g : ℝ => g ^ n * (1 - g)) (Icc 0 ((n : ℝ) / (n + 1))) := by
  have hd : ∀ g : ℝ, HasDerivAt (fun g : ℝ => g ^ n * (1 - g))
      ((n : ℝ) * g ^ (n - 1) * (1 - g) + g ^ n * (0 - 1)) g := fun g =>
    (hasDerivAt_pow n g).mul ((hasDerivAt_const g (1 : ℝ)).sub (hasDerivAt_id g))
  refine monotoneOn_of_deriv_nonneg (convex_Icc _ _)
    (fun g _ => (hd g).continuousAt.continuousWithinAt)
    (fun g _ => (hd g).differentiableAt.differentiableWithinAt) (fun g hg => ?_)
  rw [interior_Icc] at hg
  obtain ⟨hg0, hg1⟩ := hg
  rw [(hd g).deriv]
  rcases n with _ | k
  · simp at hg1
    linarith
  · have hn1 : (0 : ℝ) < (k : ℝ) + 1 + 1 := by positivity
    rw [lt_div_iff₀ (by push_cast; linarith)] at hg1
    push_cast at hg1 ⊢
    simp only [pow_succ]
    have hp : 0 ≤ g ^ k := pow_nonneg hg0.le k
    have e : ((k : ℝ) + 1) * g ^ k * (1 - g) + g ^ k * g * (0 - 1)
        = g ^ k * (((k : ℝ) + 1) - ((k : ℝ) + 1 + 1) * g) := by ring
    rw [e]
    exact mul_nonneg hp (by linarith)

/-- **The kernel is hump-shaped.** `g ↦ g^n (1 − g)` falls on `[n/(n+1), 1]`. -/
theorem kernel_falls (n : ℕ) :
    AntitoneOn (fun g : ℝ => g ^ n * (1 - g)) (Icc ((n : ℝ) / (n + 1)) 1) := by
  have hd : ∀ g : ℝ, HasDerivAt (fun g : ℝ => g ^ n * (1 - g))
      ((n : ℝ) * g ^ (n - 1) * (1 - g) + g ^ n * (0 - 1)) g := fun g =>
    (hasDerivAt_pow n g).mul ((hasDerivAt_const g (1 : ℝ)).sub (hasDerivAt_id g))
  refine antitoneOn_of_deriv_nonpos (convex_Icc _ _)
    (fun g _ => (hd g).continuousAt.continuousWithinAt)
    (fun g _ => (hd g).differentiableAt.differentiableWithinAt) (fun g hg => ?_)
  rw [interior_Icc] at hg
  obtain ⟨hg0, hg1⟩ := hg
  rw [(hd g).deriv]
  have hgpos : 0 < g := lt_of_le_of_lt (div_nonneg (Nat.cast_nonneg n) (by positivity)) hg0
  rcases n with _ | k
  · simp
  · rw [div_lt_iff₀ (by positivity)] at hg0
    push_cast at hg0 ⊢
    simp only [pow_succ]
    have hp : 0 ≤ g ^ k := pow_nonneg hgpos.le k
    have e : ((k : ℝ) + 1) * g ^ k * (1 - g) + g ^ k * g * (0 - 1)
        = g ^ k * (((k : ℝ) + 1) - ((k : ℝ) + 1 + 1) * g) := by ring
    rw [e]
    exact mul_nonpos_of_nonneg_of_nonpos hp (by linarith)

/-- The kernel peaks at `n/(n+1)` on `[0, 1]`, which is `(Q−1)/Q` for `n = Q − 1`. -/
theorem kernel_max (n : ℕ) (g : ℝ) (hg : g ∈ Icc (0 : ℝ) 1) :
    g ^ n * (1 - g) ≤ ((n : ℝ) / (n + 1)) ^ n * (1 - (n : ℝ) / (n + 1)) := by
  have hpk0 : (0 : ℝ) ≤ (n : ℝ) / (n + 1) := div_nonneg (Nat.cast_nonneg n) (by positivity)
  have hpk1 : (n : ℝ) / (n + 1) ≤ 1 := by rw [div_le_one (by positivity)]; linarith
  rcases le_total g ((n : ℝ) / (n + 1)) with h | h
  · exact kernel_rises n ⟨hg.1, h⟩ ⟨hpk0, le_refl _⟩ h
  · exact kernel_falls n ⟨le_refl _, hpk1⟩ ⟨h, hg.2⟩ h

end Kernel

section Identity

variable (V : ℝ) (α β C : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]
  [IsProbabilityMeasure C]

/-- The P-MU weight `W = C · G^(Q−1) · (1 − G)`. -/
noncomputable def weight (Q : ℕ) (x : ℝ) : ℝ := cdf C x * cdf β x ^ (Q - 1) * (1 - cdf β x)

/-- The step in `Q` of the first entrant's gain, `D(Q) = Δ(0, Q+1) − Δ(0, Q)`. -/
noncomputable def stepQ (Q : ℕ) : ℝ := Delta V α β C (Q + 1) 0 - Delta V α β C Q 0

theorem weight_eq (Q : ℕ) (hQ : 1 ≤ Q) (x : ℝ) :
    weight β C Q x = cdf (rivals C α β 0 (Q - 1)) x - cdf (rivals C α β 0 Q) x := by
  obtain ⟨n, rfl⟩ : ∃ n, Q = n + 1 := ⟨Q - 1, by omega⟩
  rw [cdf_rivals, cdf_rivals]
  unfold weight
  simp only [Nat.add_sub_cancel, pow_zero, mul_one, pow_succ]
  ring

theorem weight_nonneg (Q : ℕ) (x : ℝ) : 0 ≤ weight β C Q x :=
  mul_nonneg (mul_nonneg (cdf_nonneg C x) (pow_nonneg (cdf_nonneg β x) _))
    (by linarith [cdf_le_one β x])

theorem weight_le_one (Q : ℕ) (x : ℝ) : weight β C Q x ≤ 1 := by
  unfold weight
  have h1 := cdf_le_one C x
  have h2 : cdf β x ^ (Q - 1) ≤ 1 := pow_le_one₀ (cdf_nonneg β x) (cdf_le_one β x)
  have h3 : 1 - cdf β x ≤ 1 := by linarith [cdf_nonneg β x]
  have h4 := cdf_nonneg C x
  have h5 := pow_nonneg (cdf_nonneg β x) (Q - 1)
  have h6 : 0 ≤ 1 - cdf β x := by linarith [cdf_le_one β x]
  calc cdf C x * cdf β x ^ (Q - 1) * (1 - cdf β x) ≤ 1 * 1 * 1 := by gcongr
    _ = 1 := by ring

theorem measurable_weight (Q : ℕ) : Measurable (weight β C Q) :=
  (((cdf C).mono.measurable.mul ((cdf β).mono.measurable.pow_const _))).mul
    (measurable_const.sub (cdf β).mono.measurable)

theorem integral_weight (ρ : Measure ℝ) [IsProbabilityMeasure ρ] (Q : ℕ) (hQ : 1 ≤ Q) :
    ∫ x, weight β C Q x ∂ρ
      = (∫ x, cdf (rivals C α β 0 (Q - 1)) x ∂ρ) - ∫ x, cdf (rivals C α β 0 Q) x ∂ρ := by
  rw [← integral_sub (integrable_cdf _ _) (integrable_cdf _ _)]
  exact integral_congr_ae (ae_of_all _ (fun x => weight_eq α β C Q hQ x))

/-- **The P-MU identity.** `Δ(0, Q+1) − Δ(0, Q) = −V (∫ W dF − ∫ W dG)`, for every incumbent
    law `C`. -/
theorem pmu_identity (Q : ℕ) (hQ : 1 ≤ Q) :
    stepQ V α β C Q = -V * ((∫ x, weight β C Q x ∂α) - ∫ x, weight β C Q x ∂β) := by
  rw [integral_weight α β C α Q hQ, integral_weight α β C β Q hQ]
  unfold stepQ Delta
  rw [show Q + 1 - 1 - 0 = Q by omega, show Q - 1 - 0 = Q - 1 by omega]
  ring

/-- **P-MU.** With `V > 0`, the first entrant's gain rises from `Q` to `Q + 1` exactly when
    `∫ W dF < ∫ W dG`. -/
theorem pmu_sign_iff (hV : 0 < V) (Q : ℕ) (hQ : 1 ≤ Q) :
    0 < stepQ V α β C Q ↔ (∫ x, weight β C Q x ∂α) < ∫ x, weight β C Q x ∂β := by
  rw [pmu_identity V α β C Q hQ,
    show -V * ((∫ x, weight β C Q x ∂α) - ∫ x, weight β C Q x ∂β)
      = V * ((∫ x, weight β C Q x ∂β) - ∫ x, weight β C Q x ∂α) by ring,
    mul_pos_iff_of_pos_left hV, sub_pos]

end Identity

section SingleCrossing

variable (V : ℝ) (α β C : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]
  [IsProbabilityMeasure C]

theorem weight_shift (Q1 Q2 : ℕ) (h1 : 1 ≤ Q1) (h12 : Q1 ≤ Q2) (x : ℝ) :
    weight β C Q2 x = cdf β x ^ (Q2 - Q1) * weight β C Q1 x := by
  unfold weight
  rw [show Q2 - 1 = (Q2 - Q1) + (Q1 - 1) by omega, pow_add]
  ring

/-- **The corrected single-crossing step.** Suppose `dF − dG` is nonpositive on `(−∞, x0]` and
    nonnegative on `(x0, ∞)`, which is the measure form of `φ = G − F` being single-peaked.
    Then `D(Q2) ≤ G(x0)^(Q2−Q1) D(Q1)` for `1 ≤ Q1 ≤ Q2`. -/
theorem pmu_single_crossing (hV : 0 ≤ V) (x0 : ℝ)
    (hleft : α.restrict (Iic x0) ≤ β.restrict (Iic x0))
    (hright : β.restrict (Ioi x0) ≤ α.restrict (Ioi x0))
    (Q1 Q2 : ℕ) (h1 : 1 ≤ Q1) (h12 : Q1 ≤ Q2) :
    stepQ V α β C Q2 ≤ cdf β x0 ^ (Q2 - Q1) * stepQ V α β C Q1 := by
  set ρ0 := cdf β x0 ^ (Q2 - Q1) with hρ0
  set h : ℝ → ℝ := fun x => (cdf β x ^ (Q2 - Q1) - ρ0) * weight β C Q1 x with hh
  have hmeas : Measurable h :=
    (((cdf β).mono.measurable.pow_const _).sub measurable_const).mul (measurable_weight β C Q1)
  have hbound : ∀ x, |h x| ≤ 1 := by
    intro x
    have a1 : 0 ≤ cdf β x ^ (Q2 - Q1) := pow_nonneg (cdf_nonneg β x) _
    have a2 : cdf β x ^ (Q2 - Q1) ≤ 1 := pow_le_one₀ (cdf_nonneg β x) (cdf_le_one β x)
    have a3 : 0 ≤ ρ0 := pow_nonneg (cdf_nonneg β x0) _
    have a4 : ρ0 ≤ 1 := pow_le_one₀ (cdf_nonneg β x0) (cdf_le_one β x0)
    have b1 := weight_nonneg β C Q1 x
    have b2 := weight_le_one β C Q1 x
    rw [hh, abs_mul]
    calc |cdf β x ^ (Q2 - Q1) - ρ0| * |weight β C Q1 x| ≤ 1 * 1 := by
          gcongr
          · rw [abs_le]; constructor <;> linarith
          · rw [abs_of_nonneg b1]; exact b2
      _ = 1 := by ring
  have hint : ∀ (ρ : Measure ℝ) [IsFiniteMeasure ρ], Integrable h ρ :=
    fun ρ _ => integrable_bdd ρ h hmeas 1 hbound
  -- `h ≤ 0` up to `x0` and `h ≥ 0` beyond it
  have hneg : ∀ x ∈ Iic x0, h x ≤ 0 := by
    intro x hx
    have : cdf β x ^ (Q2 - Q1) ≤ ρ0 :=
      pow_le_pow_left₀ (cdf_nonneg β x) ((cdf β).mono hx) _
    exact mul_nonpos_of_nonpos_of_nonneg (by linarith) (weight_nonneg β C Q1 x)
  have hpos : ∀ x ∈ Ioi x0, 0 ≤ h x := by
    intro x hx
    have : ρ0 ≤ cdf β x ^ (Q2 - Q1) :=
      pow_le_pow_left₀ (cdf_nonneg β x0) ((cdf β).mono (le_of_lt hx)) _
    exact mul_nonneg (by linarith) (weight_nonneg β C Q1 x)
  have hL : ∫ x in Iic x0, h x ∂β ≤ ∫ x in Iic x0, h x ∂α := by
    have := integral_mono_measure hleft
      (ae_restrict_of_forall_mem measurableSet_Iic (fun x hx => by
        show (0 : ℝ) ≤ -h x; linarith [hneg x hx]))
      ((hint (β.restrict (Iic x0))).neg)
    simp only [Pi.neg_apply] at this
    rw [integral_neg, integral_neg] at this
    linarith
  have hR : ∫ x in Ioi x0, h x ∂β ≤ ∫ x in Ioi x0, h x ∂α :=
    integral_mono_measure hright
      (ae_restrict_of_forall_mem measurableSet_Ioi (fun x hx => hpos x hx))
      (hint (α.restrict (Ioi x0)))
  have hsplit : ∀ (ρ : Measure ℝ) [IsProbabilityMeasure ρ],
      ∫ x, h x ∂ρ = (∫ x in Iic x0, h x ∂ρ) + ∫ x in Ioi x0, h x ∂ρ := by
    intro ρ _
    rw [← integral_add_compl measurableSet_Iic (hint ρ), compl_Iic]
  have hmain : ∫ x, h x ∂β ≤ ∫ x, h x ∂α := by
    rw [hsplit α, hsplit β]
    linarith
  -- `∫ h = ∫ W_Q2 − ρ0 ∫ W_Q1`
  have hexp : ∀ (ρ : Measure ℝ) [IsProbabilityMeasure ρ],
      ∫ x, h x ∂ρ = (∫ x, weight β C Q2 x ∂ρ) - ρ0 * ∫ x, weight β C Q1 x ∂ρ := by
    intro ρ _
    have hw1 : Integrable (weight β C Q1) ρ :=
      integrable_bdd ρ _ (measurable_weight β C Q1) 1
        (fun x => by rw [abs_of_nonneg (weight_nonneg β C Q1 x)]; exact weight_le_one β C Q1 x)
    have hw2 : Integrable (weight β C Q2) ρ :=
      integrable_bdd ρ _ (measurable_weight β C Q2) 1
        (fun x => by rw [abs_of_nonneg (weight_nonneg β C Q2 x)]; exact weight_le_one β C Q2 x)
    rw [← integral_const_mul, ← integral_sub hw2 (hw1.const_mul ρ0)]
    refine integral_congr_ae (ae_of_all _ (fun x => ?_))
    simp only [hh]
    rw [weight_shift β C Q1 Q2 h1 h12 x]
    ring
  rw [pmu_identity V α β C Q2 (le_trans h1 h12), pmu_identity V α β C Q1 h1]
  rw [hexp α, hexp β] at hmain
  nlinarith

/-- Once the step in `Q` is nonpositive, it stays nonpositive. -/
theorem pmu_step_nonpos_persists (hV : 0 ≤ V) (x0 : ℝ)
    (hleft : α.restrict (Iic x0) ≤ β.restrict (Iic x0))
    (hright : β.restrict (Ioi x0) ≤ α.restrict (Ioi x0))
    (Q1 Q2 : ℕ) (h1 : 1 ≤ Q1) (h12 : Q1 ≤ Q2) (hstep : stepQ V α β C Q1 ≤ 0) :
    stepQ V α β C Q2 ≤ 0 := by
  have h := pmu_single_crossing V α β C hV x0 hleft hright Q1 Q2 h1 h12
  have hρ : 0 ≤ cdf β x0 ^ (Q2 - Q1) := pow_nonneg (cdf_nonneg β x0) _
  nlinarith

theorem quasiconcave_of_steps (a : ℕ → ℝ)
    (h : ∀ i j, 1 ≤ i → i ≤ j → a (i + 1) - a i ≤ 0 → a (j + 1) - a j ≤ 0)
    (Q1 Q2 Q3 : ℕ) (h1 : 1 ≤ Q1) (h12 : Q1 ≤ Q2) (h23 : Q2 ≤ Q3) :
    min (a Q1) (a Q3) ≤ a Q2 := by
  by_cases hex : ∃ i, Q1 ≤ i ∧ i < Q2 ∧ a (i + 1) - a i ≤ 0
  · obtain ⟨i, hi1, hi2, hi⟩ := hex
    have hdown : ∀ k, Q2 ≤ k → a k ≤ a Q2 := by
      intro k hk
      induction k, hk using Nat.le_induction with
      | base => exact le_refl _
      | succ k hk ih =>
          have := h i k (by omega) (by omega) hi
          linarith
    exact le_trans (min_le_right _ _) (hdown Q3 h23)
  · push_neg at hex
    have hup : ∀ k, Q1 ≤ k → k ≤ Q2 → a Q1 ≤ a k := by
      intro k hk
      induction k, hk using Nat.le_induction with
      | base => exact fun _ => le_refl _
      | succ k hk ih =>
          intro hk2
          have := hex k hk (by omega)
          linarith [ih (by omega)]
    exact le_trans (min_le_left _ _) (hup Q2 h12 (le_refl _))

/-- **The first entrant's gain is quasi-concave in `Q`.** Under the single-crossing hypothesis,
    `Δ(0, ·)` rises and then falls on `Q ≥ 1`, so it has no interior minimum. -/
theorem pmu_quasiconcave (hV : 0 ≤ V) (x0 : ℝ)
    (hleft : α.restrict (Iic x0) ≤ β.restrict (Iic x0))
    (hright : β.restrict (Ioi x0) ≤ α.restrict (Ioi x0))
    (Q1 Q2 Q3 : ℕ) (h1 : 1 ≤ Q1) (h12 : Q1 ≤ Q2) (h23 : Q2 ≤ Q3) :
    min (Delta V α β C Q1 0) (Delta V α β C Q3 0) ≤ Delta V α β C Q2 0 :=
  quasiconcave_of_steps (fun Q => Delta V α β C Q 0)
    (fun i j hi hij hstep =>
      pmu_step_nonpos_persists V α β C hV x0 hleft hright i j hi hij hstep)
    Q1 Q2 Q3 h1 h12 h23

end SingleCrossing

section Uniform

/-- The uniform law on `[0, 1]`. -/
noncomputable def unif : Measure ℝ := volume.restrict (Icc 0 1)

instance unif_isProb : IsProbabilityMeasure unif :=
  ⟨by simp [unif, Real.volume_Icc]⟩

/-- Shifting a uniform score up by `t ≥ 0` cannot put more mass on a set below `μ`. -/
theorem unif_shift_le (μ t : ℝ) (hμ : 0 < μ) (ht : 0 ≤ t) (B : Set ℝ) (hB : B ⊆ Iic μ) :
    unif {r | μ * r + t ∈ B} ≤ unif {r | μ * r ∈ B} := by
  unfold unif
  rw [Measure.restrict_apply' measurableSet_Icc, Measure.restrict_apply' measurableSet_Icc]
  set a := t / μ with ha_def
  have ha : 0 ≤ a := div_nonneg ht hμ.le
  have e : ∀ r, μ * (r + a) = μ * r + t := by
    intro r
    rw [ha_def, mul_add, mul_div_cancel₀ _ hμ.ne']
  have hpre : {r | μ * r + t ∈ B} ∩ Icc 0 1
      = (fun r => r + a) ⁻¹' ({r' | μ * r' ∈ B} ∩ Icc a (1 + a)) := by
    ext r
    simp only [mem_inter_iff, mem_setOf_eq, mem_preimage, mem_Icc, e r]
    constructor
    · rintro ⟨h1, h2, h3⟩
      exact ⟨h1, by linarith, by linarith⟩
    · rintro ⟨h1, h2, h3⟩
      exact ⟨h1, by linarith, by linarith⟩
  rw [hpre, measure_preimage_add_right]
  apply measure_mono
  intro r' hr'
  simp only [mem_inter_iff, mem_setOf_eq, mem_Icc] at hr' ⊢
  obtain ⟨h1, h2, _⟩ := hr'
  have h4 : μ * r' ≤ μ * 1 := by rw [mul_one]; exact hB h1
  exact ⟨h1, by linarith, le_of_mul_le_mul_left h4 hμ⟩

/-- **The single-crossing hypothesis holds for a uniform base score**, at `x0 = μ`, below the
    crossing point. -/
theorem uniform_left (μ : ℝ) (hμ0 : 0 < μ) (hμ1 : μ ≤ 1) (ρs : Measure ℝ)
    [IsProbabilityMeasure ρs] (hs : ρs (Iio 0) = 0) :
    (investorLaw μ unif ρs).restrict (Iic μ) ≤ (nonInvestorLaw μ unif).restrict (Iic μ) := by
  rw [Measure.le_iff]
  intro A hA
  rw [Measure.restrict_apply hA, Measure.restrict_apply hA]
  have hBm : MeasurableSet (A ∩ Iic μ) := hA.inter measurableSet_Iic
  have hBsub : A ∩ Iic μ ⊆ Iic μ := inter_subset_right
  rw [investorLaw, Measure.map_apply (measurable_investorScore μ) hBm,
    Measure.prod_apply_symm ((measurable_investorScore μ) hBm),
    nonInvestorLaw, Measure.map_apply (measurable_nonInvestorScore μ) hBm]
  have hae : ∀ᵐ y ∂ρs, 0 ≤ y := by
    rw [ae_iff]
    have e : {y : ℝ | ¬ 0 ≤ y} = Iio 0 := by ext y; simp
    rw [e]
    exact hs
  calc ∫⁻ y, unif ((fun x => (x, y)) ⁻¹'
          ((fun p : ℝ × ℝ => μ * p.1 + (1 - μ) * p.2) ⁻¹' (A ∩ Iic μ))) ∂ρs
      ≤ ∫⁻ _y, unif ((fun r => μ * r) ⁻¹' (A ∩ Iic μ)) ∂ρs := by
        refine lintegral_mono_ae (hae.mono (fun y hy => ?_))
        exact unif_shift_le μ ((1 - μ) * y) hμ0 (mul_nonneg (by linarith) hy) _ hBsub
    _ = unif ((fun r => μ * r) ⁻¹' (A ∩ Iic μ)) := by
        rw [lintegral_const, measure_univ, mul_one]

/-- Above the crossing point the non-investor's law has no mass. -/
theorem uniform_right (μ : ℝ) (hμ0 : 0 < μ) (ρs : Measure ℝ) [IsProbabilityMeasure ρs] :
    (nonInvestorLaw μ unif).restrict (Ioi μ) ≤ (investorLaw μ unif ρs).restrict (Ioi μ) := by
  have h0 : nonInvestorLaw μ unif (Ioi μ) = 0 := by
    rw [nonInvestorLaw, Measure.map_apply (measurable_nonInvestorScore μ) measurableSet_Ioi]
    unfold unif
    rw [Measure.restrict_apply' measurableSet_Icc]
    have hempty : (fun r : ℝ => μ * r) ⁻¹' Ioi μ ∩ Icc 0 1 = ∅ := by
      ext r
      simp only [mem_inter_iff, mem_preimage, mem_Ioi, mem_Icc, mem_empty_iff_false, iff_false,
        not_and]
      intro h1 _ h3
      nlinarith
    rw [hempty, measure_empty]
  rw [Measure.restrict_eq_zero.mpr h0]
  exact Measure.zero_le _

/-- **The first entrant's gain is quasi-concave in `Q` for a uniform base score.** For any law
    of `s ≥ 0`, any `0 < μ ≤ 1` and any incumbent law `C`, `Δ(0, ·)` rises and then falls on
    `Q ≥ 1`. -/
theorem pmu_quasiconcave_uniform (V : ℝ) (hV : 0 ≤ V) (μ : ℝ) (hμ0 : 0 < μ) (hμ1 : μ ≤ 1)
    (ρs C : Measure ℝ) [IsProbabilityMeasure ρs] [IsProbabilityMeasure C]
    (hs : ρs (Iio 0) = 0) (Q1 Q2 Q3 : ℕ) (h1 : 1 ≤ Q1) (h12 : Q1 ≤ Q2) (h23 : Q2 ≤ Q3) :
    min (Delta V (investorLaw μ unif ρs) (nonInvestorLaw μ unif) C Q1 0)
        (Delta V (investorLaw μ unif ρs) (nonInvestorLaw μ unif) C Q3 0)
      ≤ Delta V (investorLaw μ unif ρs) (nonInvestorLaw μ unif) C Q2 0 :=
  pmu_quasiconcave V _ _ C hV μ (uniform_left μ hμ0 hμ1 ρs hs) (uniform_right μ hμ0 ρs)
    Q1 Q2 Q3 h1 h12 h23

end Uniform

end EntryContestPMU
