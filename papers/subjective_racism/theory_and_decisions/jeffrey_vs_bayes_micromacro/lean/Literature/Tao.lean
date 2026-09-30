/-
# Tao (2011), *An Introduction to Measure Theory* (AMS GSM 126): a bridge file

The source is the author's preliminary version of the book, made available with
the AMS's permission (265 pp.). Its theorem and exercise numbers are assumed to
match the published book, but this was not checked. Page numbers are those of
the preliminary version (`pre-p.N`).

Paper B's proof of Theorem LOS (Appendix A.3, Steps 3-4) cites Tao twice:

* Step 3: "Tonelli's theorem [Corollary 1.7.23]". Corollary 1.7.23 (pre-p.206) is the
  *Fubini-Tonelli* theorem, stated for complex-valued `f` with an absolute
  integrability hypothesis. For the nonnegative integrand of Step 3 the
  statement actually used is **Tonelli, Theorem 1.7.15** (incomplete version,
  pre-p.200-201) or **Theorem 1.7.18** (complete version, pre-p.202).
* Step 4: "continuity from above [§1.4]". This is **Exercise 1.4.23(iii)**
  (downward monotone convergence, pre-p.93), whose hypothesis is that
  `μ(E_n) < ∞` for at least one `n`.

This file states each result in Tao's form and proves it from the matching
Mathlib theorem. It then proves the lemma Step 4 actually needs, for a family
indexed by a *real* parameter `c ↓ 0`: Tao states 1.4.23(iii) for sequences,
and the manuscript applies it to `c > 0` without saying how. Finally it proves
Step 3's marginal identity and Step 4's `L(c) = o(c)` conclusion.

Tao's σ-finite product measure is unique (his Prop. 1.7.11). Mathlib's
`Measure.prod` is that measure. Tao's "measurable with respect to the completion
of `B_X × B_Y`" (Thm 1.7.18) corresponds to Mathlib's `AEMeasurable f (μ.prod ν)`.
Every function measurable for the completed σ-algebra agrees a.e. with a
product-measurable one, and that is all `AEMeasurable` asks.
-/
import Mathlib

namespace Literature.Tao

open MeasureTheory Filter Set Topology
open scoped ENNReal

variable {X Y : Type*} [MeasurableSpace X] [MeasurableSpace Y]

/-! ## Exercise 1.4.23: monotone convergence for sets -/

/-- **Exercise 1.4.23(ii), upwards monotone convergence.** If `E₁ ⊂ E₂ ⊂ …`
then `μ(⋃ Eₙ) = lim μ(Eₙ) = sup μ(Eₙ)`. (Tao indexes from `1`, here from `0`.)
No measurability is needed in Mathlib's form. -/
theorem ex_1_4_23_ii (μ : Measure X) {E : ℕ → Set X} (hE : Monotone E) :
    μ (⋃ n, E n) = ⨆ n, μ (E n) ∧
      Tendsto (fun n => μ (E n)) atTop (𝓝 (μ (⋃ n, E n))) :=
  ⟨hE.measure_iUnion, tendsto_measure_iUnion_atTop hE⟩

/-- **Exercise 1.4.23(iii), downwards monotone convergence ("continuity from
above").** If `E₁ ⊃ E₂ ⊃ …` are measurable and `μ(Eₙ) < ∞` for at least one
`n`, then `μ(⋂ Eₙ) = lim μ(Eₙ) = inf μ(Eₙ)`. -/
theorem ex_1_4_23_iii (μ : Measure X) {E : ℕ → Set X} (hmeas : ∀ n, MeasurableSet (E n))
    (hE : Antitone E) (hfin : ∃ n, μ (E n) < ∞) :
    μ (⋂ n, E n) = ⨅ n, μ (E n) ∧
      Tendsto (fun n => μ (E n)) atTop (𝓝 (μ (⋂ n, E n))) := by
  have hfin' : ∃ n, μ (E n) ≠ ∞ := hfin.imp fun _ h => h.ne
  exact ⟨hE.measure_iInter (fun n => (hmeas n).nullMeasurableSet) hfin',
    tendsto_measure_iInter_atTop (fun n => (hmeas n).nullMeasurableSet) hE hfin'⟩

/-- **Exercise 1.4.23, last sentence: the finiteness hypothesis cannot be
dropped.** `Eₙ = [n, ∞)` in `ℝ` is decreasing, every `Eₙ` has infinite Lebesgue
measure, and the intersection is empty. So `lim μ(Eₙ) = ∞ ≠ 0 = μ(⋂ Eₙ)`. -/
theorem ex_1_4_23_iii_needs_finite :
    Antitone (fun n : ℕ => Ici (n : ℝ)) ∧ (∀ n : ℕ, volume (Ici (n : ℝ)) = ∞) ∧
      (⋂ n : ℕ, Ici (n : ℝ)) = ∅ := by
  refine ⟨fun m n hmn => Ici_subset_Ici.mpr (by exact_mod_cast hmn),
    fun n => Real.volume_Ici, ?_⟩
  ext x
  simp only [mem_iInter, mem_Ici, mem_empty_iff_false, iff_false, not_forall, not_le]
  obtain ⟨n, hn⟩ := exists_nat_gt x
  exact ⟨n, hn⟩

/-! ## Tonelli's theorem -/

/-- **Theorem 1.7.15 (Tonelli's theorem, incomplete version), pre-p.200-201.**
For σ-finite `μ_X`, `μ_Y` and `f : X × Y → [0, +∞]` measurable for the product
σ-algebra: (i) both partial integrals are measurable, and (ii) the double
integral equals both iterated integrals. -/
theorem thm_1_7_15 (μX : Measure X) (μY : Measure Y) [SigmaFinite μX] [SigmaFinite μY]
    {f : X × Y → ℝ≥0∞} (hf : Measurable f) :
    (Measurable (fun x => ∫⁻ y, f (x, y) ∂μY) ∧ Measurable (fun y => ∫⁻ x, f (x, y) ∂μX)) ∧
      (∫⁻ z, f z ∂(μX.prod μY) = ∫⁻ x, ∫⁻ y, f (x, y) ∂μY ∂μX ∧
        ∫⁻ z, f z ∂(μX.prod μY) = ∫⁻ y, ∫⁻ x, f (x, y) ∂μX ∂μY) :=
  ⟨⟨hf.lintegral_prod_right', hf.lintegral_prod_left'⟩,
    lintegral_prod f hf.aemeasurable, lintegral_prod_symm f hf.aemeasurable⟩

/-- **Theorem 1.7.18 (Tonelli's theorem, complete version), pre-p.202.** For
`f` measurable with respect to the completed product σ-algebra (in Mathlib:
a.e.-measurable for `μ_X × μ_Y`): (i) for `μ_X`-a.e. `x` the section
`y ↦ f(x,y)` is measurable (up to a null set), and `x ↦ ∫ f(x,y) dμ_Y` is
measurable; (ii) the same with the roles swapped; (iii) Eq. (1.37), the double
integral equals both iterated integrals. -/
theorem thm_1_7_18 (μX : Measure X) (μY : Measure Y) [SigmaFinite μX] [SigmaFinite μY]
    {f : X × Y → ℝ≥0∞} (hf : AEMeasurable f (μX.prod μY)) :
    ((∀ᵐ x ∂μX, AEMeasurable (fun y => f (x, y)) μY) ∧
        AEMeasurable (fun x => ∫⁻ y, f (x, y) ∂μY) μX) ∧
      ((∀ᵐ y ∂μY, AEMeasurable (fun x => f (x, y)) μX) ∧
        AEMeasurable (fun y => ∫⁻ x, f (x, y) ∂μX) μY) ∧
      (∫⁻ z, f z ∂(μX.prod μY) = ∫⁻ x, ∫⁻ y, f (x, y) ∂μY ∂μX ∧
        ∫⁻ z, f z ∂(μX.prod μY) = ∫⁻ y, ∫⁻ x, f (x, y) ∂μX ∂μY) := by
  refine ⟨⟨?_, hf.lintegral_prod_right'⟩, ⟨?_, hf.lintegral_prod_left'⟩,
    lintegral_prod f hf, lintegral_prod_symm f hf⟩
  · obtain ⟨g, hg, hfg⟩ := hf
    filter_upwards [Measure.ae_ae_of_ae_prod hfg] with x hx
    exact ⟨fun y => g (x, y), hg.comp measurable_prodMk_left, hx⟩
  · obtain ⟨g, hg, hfg⟩ := hf.prod_swap
    filter_upwards [Measure.ae_ae_of_ae_prod hfg] with y hy
    exact ⟨fun x => g (y, x), hg.comp measurable_prodMk_left, hy⟩

/-- Iterated integrals may be swapped (the form Step 3 uses). This is
`MeasureTheory.lintegral_lintegral_swap`, a corollary of Thm 1.7.15/1.7.18. -/
theorem tonelli_swap (μX : Measure X) (μY : Measure Y) [SigmaFinite μX] [SigmaFinite μY]
    {f : X → Y → ℝ≥0∞} (hf : AEMeasurable (Function.uncurry f) (μX.prod μY)) :
    ∫⁻ x, ∫⁻ y, f x y ∂μY ∂μX = ∫⁻ y, ∫⁻ x, f x y ∂μX ∂μY :=
  lintegral_lintegral_swap hf

/-! ## The lemma Theorem LOS, Step 4 uses -/

/-- **Continuity from above for a real-parameter family (what Step 4 uses).**
Let `μ` be a finite measure and let `(B_c)_{c > 0}` be measurable sets that nest
(`c' ≤ c ⇒ B_{c'} ⊆ B_c`) and exhaust (`⋂_{c>0} B_c = ∅`). Then
`μ(B_c) → 0` as `c ↓ 0`. Tao's Exercise 1.4.23(iii) is stated for sequences;
the passage to `c ↓ 0` uses that `𝓝[>] 0` is countably generated, which is
built into Mathlib's `tendsto_measure_biInter_gt`. Only `μ(B_c) < ∞` for one
`c > 0` is needed, and finiteness of `μ` gives it. -/
theorem measure_band_tendsto_zero (μ : Measure X) [IsFiniteMeasure μ] {B : ℝ → Set X}
    (hmeas : ∀ c > 0, MeasurableSet (B c))
    (hnest : ∀ c' c, 0 < c' → c' ≤ c → B c' ⊆ B c)
    (hempty : (⋂ c > (0 : ℝ), B c) = ∅) :
    Tendsto (fun c => μ (B c)) (𝓝[>] 0) (𝓝 0) := by
  have h := tendsto_measure_biInter_gt (μ := μ) (s := B) (a := (0 : ℝ))
    (fun r hr => (hmeas r hr).nullMeasurableSet) hnest ⟨1, one_pos, measure_ne_top μ _⟩
  rw [hempty, measure_empty] at h
  exact h

/-- The manuscript's band, `B_c = {u : 0 < |u| ≤ cM}` (Appendix A.3, Step 2). -/
def band (M c : ℝ) : Set ℝ := {u | 0 < |u| ∧ |u| ≤ c * M}

theorem measurableSet_band (M c : ℝ) : MeasurableSet (band M c) :=
  (measurableSet_lt measurable_const continuous_abs.measurable).inter
    (measurableSet_le continuous_abs.measurable measurable_const)

/-- The bands nest: `c' < c ⇒ B_{c'} ⊆ B_c` (for `M ≥ 0`). -/
theorem band_mono {M : ℝ} (hM : 0 ≤ M) {c' c : ℝ} (h : c' ≤ c) : band M c' ⊆ band M c :=
  fun _ ⟨h1, h2⟩ => ⟨h1, h2.trans (mul_le_mul_of_nonneg_right h hM)⟩

/-- The bands exhaust: `⋂_{c>0} B_c = ∅`, since `u ≠ 0` leaves once
`cM < |u|` and `u = 0` lies in no band. -/
theorem band_iInter_empty {M : ℝ} (hM : 0 ≤ M) : (⋂ c > (0 : ℝ), band M c) = ∅ := by
  ext u
  simp only [mem_iInter, mem_empty_iff_false, iff_false, not_forall]
  by_cases hu : u = 0
  · exact ⟨1, one_pos, fun h => by simp [band, hu] at h⟩
  · have hpos : 0 < |u| := abs_pos.mpr hu
    refine ⟨|u| / (M + 1), div_pos hpos (by linarith), fun h => ?_⟩
    have h2 := h.2
    have : |u| / (M + 1) * M < |u| := by
      rw [div_mul_eq_mul_div, div_lt_iff₀ (by linarith)]
      nlinarith
    linarith

/-- **Theorem LOS, Step 4.** For the marginal law `μ` of `u` (any finite
measure on `ℝ`, atoms at `0` allowed), `μ(B_c) ↓ 0` as `c ↓ 0`. -/
theorem los_step4 (μ : Measure ℝ) [IsFiniteMeasure μ] {M : ℝ} (hM : 0 ≤ M) :
    Tendsto (fun c => μ (band M c)) (𝓝[>] 0) (𝓝 0) :=
  measure_band_tendsto_zero μ (fun c _ => measurableSet_band M c)
    (fun _ _ _ h => band_mono hM h) (band_iInter_empty hM)

/-- **Theorem LOS, Step 4, conclusion.** If `0 ≤ L(c) ≤ cM μ(B_c)` for `c > 0`,
then `L(c)/c → 0`, i.e. `L(c) = o(c)`. -/
theorem los_step4_littleO (μ : Measure ℝ) [IsFiniteMeasure μ] {M : ℝ} (hM : 0 ≤ M)
    {L : ℝ → ℝ} (hL0 : ∀ c > 0, 0 ≤ L c)
    (hL : ∀ c > 0, L c ≤ c * M * (μ (band M c)).toReal) :
    Tendsto (fun c => L c / c) (𝓝[>] 0) (𝓝 0) := by
  have hμ : Tendsto (fun c => (μ (band M c)).toReal) (𝓝[>] 0) (𝓝 0) := by
    have := (ENNReal.tendsto_toReal ENNReal.zero_ne_top).comp (los_step4 μ hM)
    simpa [Function.comp_def] using this
  have hup : Tendsto (fun c => M * (μ (band M c)).toReal) (𝓝[>] 0) (𝓝 0) := by
    simpa using hμ.const_mul M
  refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds hup ?_ ?_
  · filter_upwards [self_mem_nhdsWithin] with c (hc : 0 < c)
    exact div_nonneg (hL0 c hc) hc.le
  · filter_upwards [self_mem_nhdsWithin] with c (hc : 0 < c)
    rw [div_le_iff₀ hc]
    calc L c ≤ c * M * (μ (band M c)).toReal := hL c hc
      _ = M * (μ (band M c)).toReal * c := by ring

/-! ## The Tonelli step of Theorem LOS, Step 3 -/

/-- **Theorem LOS, Step 3: the band mass is a marginal mass.** For a joint
density `g(u, δ)`, integrating `1_{B}(u) g(u, δ)` first in `u` and then in `δ`
gives `∫_B (∫ g(u, δ) dδ) du`, the mass that the `u`-marginal puts on `B`. The
swap is Tonelli (`lintegral_lintegral_swap`); nothing beyond Thm 1.7.15 is used,
and in particular no integrability hypothesis (Cor. 1.7.23's) is needed. -/
theorem los_step3_marginal (μ : Measure X) (ν : Measure Y) [SigmaFinite μ] [SigmaFinite ν]
    {g : X × Y → ℝ≥0∞} (hg : Measurable g) {B : Set X} (hB : MeasurableSet B) :
    ∫⁻ δ, ∫⁻ u, B.indicator (fun u => g (u, δ)) u ∂μ ∂ν =
      ∫⁻ u in B, ∫⁻ δ, g (u, δ) ∂ν ∂μ := by
  have hmeas : Measurable (Function.uncurry fun (δ : Y) (u : X) => B.indicator (fun u => g (u, δ)) u) := by
    have : (Function.uncurry fun (δ : Y) (u : X) => B.indicator (fun u => g (u, δ)) u) =
        (Prod.snd ⁻¹' B).indicator (fun p : Y × X => g (p.2, p.1)) := by
      ext ⟨δ, u⟩
      simp only [Function.uncurry_apply_pair, Set.indicator]; rfl
    rw [this]
    exact (hg.comp measurable_swap).indicator (measurable_snd hB)
  rw [lintegral_lintegral_swap hmeas.aemeasurable, ← lintegral_indicator hB]
  congr 1
  ext u
  by_cases hu : u ∈ B <;> simp [hu]

/-- **Theorem LOS, Step 3: the bound.** If every point of the flip set `F` has
its `u`-coordinate in the band `B` and a weight `w ≤ K` there (the manuscript's
`|u| ≤ cM`), then `∬_F w g ≤ K ∫_B (∫ g dδ) du = K μ(B)`. -/
theorem los_step3_bound (μ : Measure X) (ν : Measure Y) [SigmaFinite μ] [SigmaFinite ν]
    {g w : X × Y → ℝ≥0∞} (hg : Measurable g) {B : Set X} (hB : MeasurableSet B)
    {F : Set (X × Y)} {K : ℝ≥0∞} (hFB : ∀ p ∈ F, p.1 ∈ B) (hw : ∀ p ∈ F, w p ≤ K) :
    ∫⁻ δ, ∫⁻ u, F.indicator (fun p => w p * g p) (u, δ) ∂μ ∂ν ≤
      K * ∫⁻ u in B, ∫⁻ δ, g (u, δ) ∂ν ∂μ := by
  have hpt : ∀ δ u, F.indicator (fun p => w p * g p) (u, δ) ≤
      K * B.indicator (fun u => g (u, δ)) u := by
    intro δ u
    by_cases hF : (u, δ) ∈ F
    · rw [indicator_of_mem hF, indicator_of_mem (hFB _ hF)]
      exact mul_le_mul_left (hw _ hF) _
    · rw [indicator_of_notMem hF]; exact zero_le
  have hsec : ∀ δ, Measurable (fun u => B.indicator (fun u => g (u, δ)) u) := fun δ =>
    (hg.comp measurable_prodMk_right).indicator hB
  have hsec2 : Measurable (fun δ => ∫⁻ u, B.indicator (fun u => g (u, δ)) u ∂μ) := by
    have : Measurable (Function.uncurry fun (u : X) (δ : Y) => B.indicator (fun u => g (u, δ)) u) := by
      have : (Function.uncurry fun (u : X) (δ : Y) => B.indicator (fun u => g (u, δ)) u) =
          (Prod.fst ⁻¹' B).indicator g := by
        ext ⟨u, δ⟩; simp only [Function.uncurry_apply_pair, Set.indicator]; rfl
      rw [this]; exact hg.indicator (measurable_fst hB)
    exact this.lintegral_prod_left'
  calc ∫⁻ δ, ∫⁻ u, F.indicator (fun p => w p * g p) (u, δ) ∂μ ∂ν
      ≤ ∫⁻ δ, ∫⁻ u, K * B.indicator (fun u => g (u, δ)) u ∂μ ∂ν :=
        lintegral_mono fun δ => lintegral_mono fun u => hpt δ u
    _ = ∫⁻ δ, K * ∫⁻ u, B.indicator (fun u => g (u, δ)) u ∂μ ∂ν := by
        congr 1; ext δ; exact lintegral_const_mul K (hsec δ)
    _ = K * ∫⁻ δ, ∫⁻ u, B.indicator (fun u => g (u, δ)) u ∂μ ∂ν :=
        lintegral_const_mul K hsec2
    _ = K * ∫⁻ u in B, ∫⁻ δ, g (u, δ) ∂ν ∂μ := by rw [los_step3_marginal μ ν hg hB]

end Literature.Tao
