/-
# Becker (1962), "Irrational Behavior and Economic Theory"

*Journal of Political Economy* 70(1), 1-13.

**Source.** The copy used is a web PDF of the JSTOR scan (14 pp., JSTOR cover
page plus journal pp. 1-13), fetched from cooperative-individualism.org. It is
not the Drive copy (there is none). Journal page numbers are printed on the
scan and are cited as `p. N`.

## The claims formalized

Section II (pp. 2-9) argues that the market demand curve slopes downward even
for households that do not maximize anything. Two models of "irrational"
households are used:

* **Impulsive households (pp. 5-6).** "every opportunity has an equal chance of
  being selected". On the budget line `p₁x + p₂y = I` the average of many
  independent households "would almost certainly be at the middle", the
  midpoint `(I/2p₁, I/2p₂)` (fn. 10). A compensated rise in `p₁` rotates the
  budget line through that point, and "always shifts the midpoint of the budget
  line upward and to the left" (p. 6). Market demand is unit-elastic (fn. 15:
  `X = k I/P_x`).
* **Inert households (pp. 6-8).** "wherever possible, households consume exactly
  what they did in the past". After a compensated rise in `p₁`, households on
  `Ap` (less `x` than at `p`) can stay, and those on `pB` "could not remain
  there … because pB would be outside the new opportunity set OCD". "If the
  average household in pB had been consuming more than OD of X, the average
  amount of X consumed by all households would necessarily decline" (p. 6).
  Fn. 14 computes a 10 % rise with a uniform initial distribution and forced
  adjusters moving to the new midpoint: `X₁ = (31/88) I/P_x`, a decline of
  "about 30 per cent", "a high elasticity of -3" (p. 8). "A smaller price
  change … would yield a still higher elasticity".
* **Weighted average (p. 7).** A mixture of the two also slopes downward.

The mechanism, in Becker's own words: the slope comes "from the change in
opportunities alone" (p. 4), and "what is simply more probable for a particular
household becomes a certainty for a large number of independent ones" (p. 6).
So there are two ingredients: the **budget constraint** moves every household's
opportunity set, and **averaging** over many households turns a shifted
distribution into a sure market response. Survival "simply refers to a
resource constraint on behavior" (p. 10). Irrational units are "'forced' by a
change in opportunities to respond rationally" (p. 12).

## What is proved

* `budgetLine_midpoint`: the midpoint of the budget segment is
  `(I/2p₁, I/2p₂)` (fn. 10).
* `impulsive_mean`: `x` uniform on `[0, I/p₁]` (the budget line with uniform
  arc length) has mean `I/(2p₁)`.
* `impulsive_market_average`: **the market level**. For pairwise-independent,
  identically distributed impulsive households, the market average of `x`
  converges almost surely to `I/(2p₁)` (strong law of large numbers).
* `impulsive_demand_strictAnti`, `impulsive_unit_elastic`: expected demand
  `I/(2p₁)` falls strictly in `p₁` and has `p₁ x̄ = I/2` (unit elasticity, fn. 15).
* `rotation_through_point`, `compensated_midpoint_shift`: a compensated rise in
  `p₁` (a line through the old midpoint) lowers `p₂`. It moves the new midpoint
  left and up (p. 6).
* `constraint_without_averaging`: a *single* impulsive household can buy more
  `x` after the price rise ("many actual individual curves would not be",
  p. 6).
* `averaging_without_constraint`: households whose choice law does not depend on
  the budget have a flat market demand, however many are averaged.
* `inert_outside_iff`: on the old line, a point is outside the new opportunity
  set iff it has more `x` than the rotation point (`pB` is forced, `Ap` is not).
* `inert_decline_of_mean_gt`: **p. 6, "necessarily decline"**, for any finite
  population.
* `inert_decline_not_necessary`: without that condition, market `x` can rise.
* `inert_fn14`, `inert_fn14_change`, `inert_fn14_elasticity`: fn. 14's
  `31/88`, `-13/44 ≈ -0.295` and elasticity `-65/22 ≈ -2.95`.
* `inert_elasticity_formula`, `inert_elasticity_smaller_change`: for a rise by
  factor `1+t`, the elasticity is `-(1+3t)/(4t(1+t))`. Its size grows as `t`
  falls (p. 8).
* `mixture_strictAnti`: the weighted-average model (p. 7).
-/
import Mathlib

namespace Literature.Becker

open MeasureTheory Filter Set Topology ProbabilityTheory Finset

/-! ## Impulsive households -/

/-- **Fn. 10.** The midpoint of the budget segment from `(0, I/p₂)` to
`(I/p₁, 0)` is `(I/2p₁, I/2p₂)`. -/
theorem budgetLine_midpoint (I p₁ p₂ : ℝ) :
    midpoint ℝ ((0 : ℝ), I / p₂) (I / p₁, (0 : ℝ)) = (I / (2 * p₁), I / (2 * p₂)) := by
  rw [midpoint_eq_smul_add]
  ext <;> simp <;> field_simp

/-- Points of the budget line, parametrized by `x ∈ [0, I/p₁]`. The map is affine
with constant speed, so "uniform along the line" is "uniform in `x`". -/
theorem budgetLine_param {I p₁ p₂ x : ℝ} (hp₂ : 0 < p₂) :
    p₁ * x + p₂ * ((I - p₁ * x) / p₂) = I := by
  field_simp; ring

/-- **p. 5.** An impulsive household picks a point of the budget line at random,
each point equally likely: `x` uniform on `[0, I/p₁]`. Its expected `x` is the
midpoint `I/(2p₁)`. -/
theorem impulsive_mean {Ω : Type*} [MeasurableSpace Ω] {P : Measure Ω} {X : Ω → ℝ}
    {I p₁ : ℝ} (hI : 0 < I) (hp : 0 < p₁) (hX : pdf.IsUniform X (Icc 0 (I / p₁)) P) :
    ∫ ω, X ω ∂P = I / (2 * p₁) := by
  have ha : 0 < I / p₁ := div_pos hI hp
  rw [pdf.IsUniform.integral_eq hX, Real.volume_Icc, sub_zero,
    integral_Icc_eq_integral_Ioc, ← intervalIntegral.integral_of_le ha.le, integral_id,
    ENNReal.toReal_inv, ENNReal.toReal_ofReal ha.le]
  field_simp
  ring

theorem impulsive_integrable {Ω : Type*} [MeasurableSpace Ω] {P : Measure Ω} {X : Ω → ℝ}
    {a : ℝ} (ha : 0 < a) (hX : pdf.IsUniform X (Icc 0 a) P) : Integrable X P := by
  have hns : volume (Icc (0 : ℝ) a) ≠ 0 := by rw [Real.volume_Icc]; simpa using ha
  have hnt : volume (Icc (0 : ℝ) a) ≠ ⊤ := by rw [Real.volume_Icc]; exact ENNReal.ofReal_ne_top
  have hXm : AEMeasurable X P := pdf.IsUniform.aemeasurable hns hnt hX
  have hid : Integrable id (Measure.map X P) := by
    rw [hX]
    unfold ProbabilityTheory.cond
    exact (show Integrable id (volume.restrict (Icc (0 : ℝ) a)) from
      continuous_id.integrableOn_Icc).smul_measure (ENNReal.inv_ne_top.mpr hns)
  exact (integrable_map_measure aestronglyMeasurable_id hXm).mp hid

/-- **pp. 5-6, the market level.** "the average consumption of a large number of
independent households would almost certainly be at the middle of the
opportunity set": for pairwise-independent, identically distributed impulsive
households, the market average of `x` converges almost surely to `I/(2p₁)`. -/
theorem impulsive_market_average {Ω : Type*} [MeasurableSpace Ω] {P : Measure Ω}
    {X : ℕ → Ω → ℝ} {I p₁ : ℝ} (hI : 0 < I) (hp : 0 < p₁)
    (hX : pdf.IsUniform (X 0) (Icc 0 (I / p₁)) P)
    (hindep : Pairwise (fun i j => X i ⟂ᵢ[P] X j)) (hident : ∀ i, IdentDistrib (X i) (X 0) P P) :
    ∀ᵐ ω ∂P, Tendsto (fun n : ℕ => (∑ i ∈ range n, X i ω) / n) atTop (𝓝 (I / (2 * p₁))) := by
  have h := strong_law_ae_real X (impulsive_integrable (div_pos hI hp) hX) hindep hident
  rwa [impulsive_mean hI hp hX] at h

/-- **p. 6.** Expected (and market) demand `I/(2p₁)` is strictly decreasing in
the own price. -/
theorem impulsive_demand_strictAnti {I : ℝ} (hI : 0 < I) :
    StrictAntiOn (fun p₁ : ℝ => I / (2 * p₁)) (Ioi 0) := by
  intro a ha b hb hab
  simp only
  exact div_lt_div_of_pos_left hI (by simp at ha; linarith) (by linarith)

/-- **Fn. 15.** Unit elasticity: expenditure on `x`, `p₁ · I/(2p₁) = I/2`, does
not depend on `p₁`. -/
theorem impulsive_unit_elastic {I p₁ : ℝ} (hp : p₁ ≠ 0) : p₁ * (I / (2 * p₁)) = I / 2 := by
  field_simp

/-- **p. 6, the compensated change.** A new line through the old point `(x₀, y₀)`
(both coordinates positive) with a higher price of `x` has a lower price of `y`. -/
theorem rotation_through_point {I p₁ p₂ q₁ q₂ x₀ y₀ : ℝ} (hx : 0 < x₀) (hy : 0 < y₀)
    (hold : p₁ * x₀ + p₂ * y₀ = I) (hnew : q₁ * x₀ + q₂ * y₀ = I) (hq : p₁ < q₁) :
    q₂ < p₂ := by
  nlinarith

/-- **p. 6 and fn. 10.** "a compensated increase in the price of X always shifts
the midpoint of the budget line upward and to the left". Rotation through the
old midpoint `(I/2p₁, I/2p₂)` to prices `q₁ > p₁`, `q₂ > 0`. -/
theorem compensated_midpoint_shift {I p₁ p₂ q₁ q₂ : ℝ} (hI : 0 < I) (hp₁ : 0 < p₁)
    (hp₂ : 0 < p₂) (hq₂ : 0 < q₂) (hq : p₁ < q₁)
    (hnew : q₁ * (I / (2 * p₁)) + q₂ * (I / (2 * p₂)) = I) :
    I / (2 * q₁) < I / (2 * p₁) ∧ I / (2 * p₂) < I / (2 * q₂) := by
  have hold : p₁ * (I / (2 * p₁)) + p₂ * (I / (2 * p₂)) = I := by field_simp; ring
  have h2 := rotation_through_point (div_pos hI (by linarith)) (div_pos hI (by linarith))
    hold hnew hq
  exact ⟨div_lt_div_of_pos_left hI (by linarith) (by linarith),
    div_lt_div_of_pos_left hI (by linarith) (by linarith)⟩

/-- **The constraint alone does not give a downward slope for one household**
(p. 6: "many actual individual curves would not be"). With `I = p₂ = 1`, `p₁`
rising from `1` to `11/10`, the choice `x = 1/10` before and `x = 9/10` after
are both affordable, and `x` rises. -/
theorem constraint_without_averaging :
    ∃ x x' : ℝ, (0 ≤ x ∧ x ≤ 1 / 1) ∧ (0 ≤ x' ∧ x' ≤ 1 / (11 / 10)) ∧ x < x' :=
  ⟨1 / 10, 9 / 10, by norm_num, by norm_num, by norm_num⟩

/-- **Averaging alone does not give a downward slope** (a remark; its content is
definitional). If each household draws
`x` from a law `μ` that ignores the budget (no constraint), the market mean
`∫ x dμ` is the same at every price vector, however many households are
averaged. The slope in `impulsive_demand_strictAnti` comes from the budget
constraint through the support `[0, I/p₁]`. -/
theorem averaging_without_constraint (μ : Measure ℝ) :
    ∀ p p' : ℝ × ℝ × ℝ, (fun _ : ℝ × ℝ × ℝ => ∫ x, x ∂μ) p = (fun _ => ∫ x, x ∂μ) p' :=
  fun _ _ => rfl

/-! ## Inert households -/

/-- **p. 6.** On the old budget line through `(x₀, y₀)`, after a compensated
rise `p₁ < q₁` (the new line also passes through `(x₀, y₀)`), a point costs
more than `I` at the new prices iff it has more `x` than `x₀`. So the households
on `pB` must move and those on `Ap` can stay. -/
theorem inert_outside_iff {I p₁ p₂ q₁ q₂ x₀ y₀ x y : ℝ} (hx₀ : 0 < x₀) (hy₀ : 0 < y₀)
    (hp₁ : 0 < p₁) (hp₂ : 0 < p₂) (hold : p₁ * x₀ + p₂ * y₀ = I) (hnew : q₁ * x₀ + q₂ * y₀ = I)
    (hq : p₁ < q₁) (hline : p₁ * x + p₂ * y = I) :
    I < q₁ * x + q₂ * y ↔ x₀ < x := by
  have h2 := rotation_through_point hx₀ hy₀ hold hnew hq
  -- `q₁ x + q₂ y - I = (x - x₀) (q₁ - q₂ p₁ / p₂)`, and the bracket is positive
  have hy : y - y₀ = -(p₁ / p₂) * (x - x₀) := by
    field_simp; linarith
  have key : q₁ * x + q₂ * y - I = (x - x₀) * (q₁ - q₂ * p₁ / p₂) := by
    have : q₁ * x + q₂ * y - I = q₁ * (x - x₀) + q₂ * (y - y₀) := by linarith
    rw [this, hy]; field_simp; ring
  have hc : 0 < q₁ - q₂ * p₁ / p₂ := by
    rw [sub_pos, div_lt_iff₀ hp₂]
    nlinarith
  constructor
  · intro h
    have : 0 < (x - x₀) * (q₁ - q₂ * p₁ / p₂) := by linarith
    have := (pos_iff_pos_of_mul_pos this).mpr hc
    linarith
  · intro h
    have : 0 < (x - x₀) * (q₁ - q₂ * p₁ / p₂) := mul_pos (by linarith) hc
    linarith

/-- **p. 6: "If the average household in pB had been consuming more than OD of X,
the average amount of X consumed by all households would necessarily
decline."** Finite population `s`; household `i` had `x i` and now has `x' i`.
Those at or below `x₀` stay; those above `x₀` (the set `pB`) end at most at
`OD`. If the `pB` households consumed more than `OD` on average, total `x`
falls. -/
theorem inert_decline_of_mean_gt {ι : Type*} (s : Finset ι) (x x' : ι → ℝ) (x₀ OD : ℝ)
    (hstay : ∀ i ∈ s, x i ≤ x₀ → x' i = x i)
    (hforced : ∀ i ∈ s, x₀ < x i → x' i ≤ OD)
    (hmean : ((s.filter (fun i => x₀ < x i)).card : ℝ) * OD <
      ∑ i ∈ s.filter (fun i => x₀ < x i), x i) :
    ∑ i ∈ s, x' i < ∑ i ∈ s, x i := by
  rw [← sum_filter_add_sum_filter_not s (fun i => x₀ < x i),
    ← sum_filter_add_sum_filter_not s (fun i => x₀ < x i) (f := x)]
  have h1 : ∑ i ∈ s.filter (fun i => x₀ < x i), x' i ≤
      ((s.filter (fun i => x₀ < x i)).card : ℝ) * OD := by
    rw [← nsmul_eq_mul, ← sum_const]
    exact sum_le_sum fun i hi => by
      rw [mem_filter] at hi; exact hforced i hi.1 hi.2
  have h2 : ∑ i ∈ s.filter (fun i => ¬ x₀ < x i), x' i =
      ∑ i ∈ s.filter (fun i => ¬ x₀ < x i), x i :=
    sum_congr rfl fun i hi => by
      rw [mem_filter] at hi; exact hstay i hi.1 (not_lt.mp hi.2)
  linarith

/-- **p. 7: "would probably decline even when not arithmetically necessary".**
Without the condition on the mean of `pB`, market `x` can rise. Take
`I = p₁ = p₂ = 1`, so `p = (1/2, 1/2)`, and the compensated prices
`(11/10, 9/10)`, whose line passes through `p` (`OD = 10/11`). A household at
`(3/5, 2/5)` on `pB` is forced out, and `(9/10, 1/90)` is on the new line. Its
`x` rises from `3/5` to `9/10`. -/
theorem inert_decline_not_necessary :
    (11 / 10 : ℝ) * (1 / 2) + 9 / 10 * (1 / 2) = 1 ∧
      (1 : ℝ) * (3 / 5) + 1 * (2 / 5) = 1 ∧ (1 / 2 : ℝ) < 3 / 5 ∧
      1 < (11 / 10 : ℝ) * (3 / 5) + 9 / 10 * (2 / 5) ∧
      (11 / 10 : ℝ) * (9 / 10) + 9 / 10 * (1 / 90) = 1 ∧ (3 / 5 : ℝ) < 9 / 10 := by
  norm_num

/-- Fn. 14's market `x` after a rise of `p₁` by the factor `1 + t`: half the
households (those on `Ap`) keep their `x`, averaging `x₀/2 = I/(4p₁)`; the other
half move to the new midpoint `I/(2(1+t)p₁)`. -/
noncomputable def inertX₁ (I p₁ t : ℝ) : ℝ :=
  1 / 2 * (I / (4 * p₁)) + 1 / 2 * (I / (2 * ((1 + t) * p₁)))

/-- **Fn. 14:** `X₁ = (31/88) I/P_x` for a 10 % rise. -/
theorem inert_fn14 {I p₁ : ℝ} (hp : p₁ ≠ 0) : inertX₁ I p₁ (1 / 10) = 31 / 88 * (I / p₁) := by
  unfold inertX₁; field_simp; ring

/-- **Fn. 14:** `(X₁ - X₀)/X₀ = (31/88 - 44/88)/(44/88) = -13/44 ≈ -.3`. -/
theorem inert_fn14_change {I p₁ : ℝ} (hI : I ≠ 0) (hp : p₁ ≠ 0) :
    (inertX₁ I p₁ (1 / 10) - I / (2 * p₁)) / (I / (2 * p₁)) = -13 / 44 := by
  rw [inert_fn14 hp]; field_simp; ring

/-- **p. 8:** "a high elasticity of -3": the arc elasticity is
`(-13/44)/(1/10) = -65/22 ≈ -2.95`. -/
theorem inert_fn14_elasticity {I p₁ : ℝ} (hI : I ≠ 0) (hp : p₁ ≠ 0) :
    (inertX₁ I p₁ (1 / 10) - I / (2 * p₁)) / (I / (2 * p₁)) / (1 / 10) = -65 / 22 := by
  rw [inert_fn14_change hI hp]; norm_num

/-- The elasticity for a rise by factor `1 + t`: `-(1 + 3t)/(4t(1+t))`. -/
theorem inert_elasticity_formula {I p₁ t : ℝ} (hI : I ≠ 0) (hp : p₁ ≠ 0) (ht : 0 < t) :
    (inertX₁ I p₁ t - I / (2 * p₁)) / (I / (2 * p₁)) / t = -(1 + 3 * t) / (4 * t * (1 + t)) := by
  have h1 : (1 + t) ≠ 0 := by linarith
  unfold inertX₁; field_simp; ring

/-- **p. 8: "A smaller price change … would yield a still higher elasticity."**
For `0 < t < t'` the elasticity at `t` is more negative. -/
theorem inert_elasticity_smaller_change {t t' : ℝ} (ht : 0 < t) (htt : t < t') :
    -(1 + 3 * t) / (4 * t * (1 + t)) < -(1 + 3 * t') / (4 * t' * (1 + t')) := by
  have ht' : 0 < t' := ht.trans htt
  rw [neg_div, neg_div, neg_lt_neg_iff, div_lt_div_iff₀ (by positivity) (by positivity)]
  nlinarith [mul_pos ht ht', mul_pos (mul_pos ht ht') (sub_pos.mpr htt)]

/-! ## The weighted average (p. 7) -/

/-- **p. 7.** "Since market demand curves at both these extremes would tend to be
negatively inclined, the market curves of any weighted average would also tend
to be." -/
theorem mixture_strictAnti {S : Set ℝ} {D₁ D₂ : ℝ → ℝ} {w : ℝ} (hw0 : 0 ≤ w) (hw1 : w ≤ 1)
    (h₁ : StrictAntiOn D₁ S) (h₂ : StrictAntiOn D₂ S) :
    StrictAntiOn (fun p => w * D₁ p + (1 - w) * D₂ p) S := by
  intro a ha b hb hab
  have e1 := h₁ ha hb hab
  have e2 := h₂ ha hb hab
  simp only
  rcases eq_or_lt_of_le hw0 with h | h
  · subst h; linarith
  · nlinarith

end Literature.Becker
