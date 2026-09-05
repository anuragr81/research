/-
# Decision statistics: Section 4.4, Theorem LOS and Proposition SHR

An evaluator has net benchmark surplus `u = s(Pᴮ) - τ` and first-order score
coefficient `δ = δ_σ`, so their Jeffrey score sits at `u + cδ` to first order.
Section 2.1 has them engage iff the score is at least the threshold, so their
action differs from the benchmark action exactly on the FLIP event

    `Flips u c δ  :  ¬((0 ≤ u) ↔ (0 ≤ u + cδ))`.

The results formalised here are the deterministic core of Section 4.4,
Appendix A.3 and Proposition SHR:

  * `flips_iff`             the flip event is `u` strictly between `0` and `-cδ`;
  * `abs_le_of_flips`       every flipping evaluator lies within `|cδ|` of the
                            threshold -- the band of width `O(c)`;
  * `not_flips_of_lt`       nobody flips while `|c| < c* = |u/δ|`, so the
                            indicator and the individual loss `|u|·1{flip}` are
                            identically zero near `c = 0` and every Taylor
                            coefficient there vanishes (Section 4.4);
  * `not_flips_of_pointing_away`
                            a perturbation pointing away from the threshold
                            never flips, however large `|c|` grows;
  * `flipSet_eq_*`, `volume_flipSet`
                            the flip set is an interval of Lebesgue measure
                            exactly `|cδ|` -- the band mass of Step 2, which
                            is what makes the SHARE first order (Prop. SHR);
  * `lintegral_stake_le`    the surplus-weighted loss is bounded by
                            `|cδ| · μ(band)`: an `O(c)` mass of individuals each
                            costing `O(c)`, hence `O(c²)` (Appendix A.3 Step 3).
                            This is the one extra factor of `c` that separates
                            intensity from incidence.
-/
import Mathlib.MeasureTheory.Integral.Lebesgue.Basic
import Mathlib.MeasureTheory.Measure.Lebesgue.Basic
import Mathlib.Tactic.Linarith

namespace JeffreyOrder

open MeasureTheory

/-- The flip event: the Jeffrey action differs from the benchmark action. -/
def Flips (u c δ : ℝ) : Prop := ¬((0 ≤ u) ↔ (0 ≤ u + c * δ))

/-- **Section 4.4: the flip condition unpacked.**  A flip happens exactly when
the perturbation carries the surplus across the threshold. -/
theorem flips_iff (u c δ : ℝ) :
    Flips u c δ ↔ (u < 0 ∧ 0 ≤ u + c * δ) ∨ (0 ≤ u ∧ u + c * δ < 0) := by
  unfold Flips
  constructor
  · intro h
    rcases le_or_gt 0 u with hu | hu
    · exact Or.inr ⟨hu, by by_contra hc; exact h ⟨fun _ => not_lt.mp hc, fun _ => hu⟩⟩
    · refine Or.inl ⟨hu, ?_⟩
      by_contra hc
      exact h ⟨fun hx => absurd hx (not_le.mpr hu), fun hx => absurd hx hc⟩
  · rintro (⟨hu, hv⟩ | ⟨hu, hv⟩) h
    · exact absurd (h.mpr hv) (not_le.mpr hu)
    · exact absurd (h.mp hu) (not_le.mpr hv)

/-- **The band (Appendix A.3, Step 2).**  A flipping evaluator lies within
`|cδ|` of the threshold: `|u| ≤ |cδ|`.  Since `δ` is bounded across the
population, only evaluators within `O(c)` of the threshold can be affected. -/
theorem abs_le_of_flips {u c δ : ℝ} (h : Flips u c δ) : |u| ≤ |c * δ| := by
  rcases (flips_iff u c δ).mp h with ⟨hu, hv⟩ | ⟨hu, hv⟩
  · rw [abs_of_neg hu]
    have : 0 ≤ c * δ := by linarith
    rw [abs_of_nonneg this]; linarith
  · rw [abs_of_nonneg hu]
    have : c * δ ≤ 0 := by linarith
    rw [abs_of_nonpos this]; linarith

/-- The same bound in the factored form used for the `O(c)` reading. -/
theorem abs_le_of_flips' {u c δ : ℝ} (h : Flips u c δ) : |u| ≤ |c| * |δ| := by
  rw [← abs_mul]; exact abs_le_of_flips h

/-- **Section 4.4: the flat interval `(-c*, c*)`.**  While `|cδ| < |u|` nobody
flips.  With `c* = |u/δ|` this says the flip indicator, and with it the
individual loss `|u|·1{flip}`, vanishes identically on `(-c*, c*)`; hence every
Taylor coefficient at `c = 0` is zero even though the indicator is not
identically zero -- it is a step in `c`, not an analytic function of it, and
there is no leading coefficient to read off. -/
theorem not_flips_of_lt {u c δ : ℝ} (h : |c * δ| < |u|) : ¬ Flips u c δ := by
  intro hf; exact absurd (abs_le_of_flips hf) (not_le.mpr h)

/-- Restated with the crossing point `c* = |u/δ|`. -/
theorem not_flips_of_lt_cstar {u c δ : ℝ} (hδ : δ ≠ 0) (h : |c| < |u| / |δ|) :
    ¬ Flips u c δ := by
  refine not_flips_of_lt ?_
  rw [abs_mul]
  have hδ' : 0 < |δ| := abs_pos.mpr hδ
  calc |c| * |δ| < (|u| / |δ|) * |δ| := by exact mul_lt_mul_of_pos_right h hδ'
    _ = |u| := by field_simp

/-- **A perturbation pointing away from the threshold never flips**, however
large `|c|` grows (Section 4.4). -/
theorem not_flips_of_pointing_away {u c δ : ℝ} (hu : 0 ≤ u) (hcd : 0 ≤ c * δ) :
    ¬ Flips u c δ := by
  intro hf
  rcases (flips_iff u c δ).mp hf with ⟨h1, _⟩ | ⟨_, h2⟩
  · linarith
  · linarith

theorem not_flips_of_pointing_away' {u c δ : ℝ} (hu : u < 0) (hcd : c * δ ≤ 0) :
    ¬ Flips u c δ := by
  intro hf
  rcases (flips_iff u c δ).mp hf with ⟨_, h2⟩ | ⟨h1, _⟩
  · linarith
  · linarith

/-- An evaluator exactly at the threshold flips only when the perturbation is
strictly negative, and in every case contributes stake `|u| = 0`.  This is the
content of Appendix A.3, Step 4's remark that the conclusion holds
"irrespective of an atom at `u = 0`, where `|u| = 0`". -/
theorem flips_at_threshold_iff (c δ : ℝ) : Flips 0 c δ ↔ c * δ < 0 := by
  rw [flips_iff]
  constructor
  · rintro (⟨h1, _⟩ | ⟨_, h2⟩) <;> linarith
  · intro h; exact Or.inr ⟨le_refl 0, by linarith⟩

/-- An atom at the threshold contributes nothing to the surplus-weighted loss,
whether or not it is counted as flipping. -/
theorem stake_at_threshold : |(0 : ℝ)| = 0 := abs_zero

/-! ### The flip set and its measure -/

/-- For `cδ > 0` the flip set is the interval `[-cδ, 0)`. -/
theorem flipSet_eq_Ico_of_pos {c δ : ℝ} (h : 0 < c * δ) :
    {u : ℝ | Flips u c δ} = Set.Ico (-(c * δ)) 0 := by
  ext u
  simp only [Set.mem_setOf_eq, Set.mem_Ico, flips_iff]
  constructor
  · rintro (⟨h1, h2⟩ | ⟨h1, h2⟩)
    · exact ⟨by linarith, h1⟩
    · linarith
  · rintro ⟨h1, h2⟩
    exact Or.inl ⟨h2, by linarith⟩

/-- For `cδ < 0` the flip set is the interval `[0, -cδ)`. -/
theorem flipSet_eq_Ico_of_neg {c δ : ℝ} (h : c * δ < 0) :
    {u : ℝ | Flips u c δ} = Set.Ico 0 (-(c * δ)) := by
  ext u
  simp only [Set.mem_setOf_eq, Set.mem_Ico, flips_iff]
  constructor
  · rintro (⟨h1, h2⟩ | ⟨h1, h2⟩)
    · linarith
    · exact ⟨h1, by linarith⟩
  · rintro ⟨h1, h2⟩
    exact Or.inr ⟨h1, by linarith⟩

/-- With no perturbation there is no flip. -/
theorem flipSet_eq_empty {c δ : ℝ} (h : c * δ = 0) : {u : ℝ | Flips u c δ} = ∅ := by
  ext u
  simp only [Set.mem_setOf_eq, Set.mem_empty_iff_false, iff_false, flips_iff, h, add_zero]
  rintro (⟨h1, h2⟩ | ⟨h1, h2⟩) <;> linarith

theorem measurableSet_flipSet (c δ : ℝ) : MeasurableSet {u : ℝ | Flips u c δ} := by
  rcases lt_trichotomy (c * δ) 0 with h | h | h
  · rw [flipSet_eq_Ico_of_neg h]; exact measurableSet_Ico
  · rw [flipSet_eq_empty h]; exact MeasurableSet.empty
  · rw [flipSet_eq_Ico_of_pos h]; exact measurableSet_Ico

/-- **Step 2: the band has width exactly `|cδ|`.**  Its Lebesgue measure is
`|cδ| = O(c)`, which is what makes the sequence-affected SHARE genuinely first
order (Proposition SHR) rather than merely `o(1)`. -/
theorem volume_flipSet (c δ : ℝ) :
    volume {u : ℝ | Flips u c δ} = ENNReal.ofReal |c * δ| := by
  rcases lt_trichotomy (c * δ) 0 with h | h | h
  · rw [flipSet_eq_Ico_of_neg h, Real.volume_Ico, abs_of_neg h]; ring_nf
  · rw [flipSet_eq_empty h, measure_empty, h]; simp
  · rw [flipSet_eq_Ico_of_pos h, Real.volume_Ico, abs_of_pos h]; ring_nf

/-! ### Step 3: incidence versus intensity -/

/-- **Appendix A.3, Step 3.**  The surplus-weighted loss over any population
measure `μ` is bounded by `|cδ| · μ(band)`: an `O(c)` mass of individuals, each
costing `O(c)`, gives an `O(c²)` loss.  The incidence `μ(band)` alone is `O(c)`
(Proposition SHR); the extra factor is the weight `|u|`, which the band itself
forces to first order. -/
theorem lintegral_stake_le (μ : Measure ℝ) (c δ : ℝ) :
    ∫⁻ u in {u : ℝ | Flips u c δ}, ENNReal.ofReal |u| ∂μ
      ≤ ENNReal.ofReal |c * δ| * μ {u : ℝ | Flips u c δ} := by
  have hbd : ∫⁻ u in {u : ℝ | Flips u c δ}, ENNReal.ofReal |u| ∂μ
      ≤ ∫⁻ _ in {u : ℝ | Flips u c δ}, ENNReal.ofReal |c * δ| ∂μ := by
    refine lintegral_mono_ae ?_
    filter_upwards [ae_restrict_mem (measurableSet_flipSet c δ)] with u hu
    exact ENNReal.ofReal_le_ofReal (abs_le_of_flips hu)
  calc ∫⁻ u in {u : ℝ | Flips u c δ}, ENNReal.ofReal |u| ∂μ
      ≤ ∫⁻ _ in {u : ℝ | Flips u c δ}, ENNReal.ofReal |c * δ| ∂μ := hbd
    _ = ENNReal.ofReal |c * δ| * μ {u : ℝ | Flips u c δ} := by
        rw [setLIntegral_const]

/-- Specialised to Lebesgue measure, this is the quadratic bound of
Theorem LOS in its bare form: the loss on the band is at most `(cδ)²`, whereas
the share is `|cδ|` -- the two decoupling by exactly one order in `c`. -/
theorem lintegral_stake_le_volume (c δ : ℝ) :
    ∫⁻ u in {u : ℝ | Flips u c δ}, ENNReal.ofReal |u| ∂volume
      ≤ ENNReal.ofReal |c * δ| * ENNReal.ofReal |c * δ| := by
  have := lintegral_stake_le volume c δ
  rwa [volume_flipSet c δ] at this

end JeffreyOrder
