/-
# The ladder under partial adoption (Propositions ADJ and PRO, interior weight)

When the second cue is adopted with weight `δ` (the paper's `ω`), which orders
in `c` does each belief statistic carry, and which statistics stay protected at
second order for every weight?  The answer rests on one structural fact and two
evaluations at `c = 0`.

**Structural fact.**  Every Jeffrey step, damped or not, multiplies rows (or
columns) of the table by constants: `jeffreyA_eq_rescale`, `jeffreyB_eq_rescale`.
A row-and-column rescaling multiplies the association by the product of the four
factors (`assoc_rescale`) and the product of the four cells by its square
(`cellProd_rescale`), and leaves the odds ratio unchanged (`oddsRatio_rescale`).
Since `assoc (prior α β c) = c` exactly (`assoc_prior`), each route's association
is `c` times an explicit factor (`assoc_routeDamped`, `assoc_routeDampedBA`).

**At `c = 0`.**  Both routes end at product measures, `q ⊗ t` and `s ⊗ r` with
`t₀ = (1-δ)β + δr₀`, `s₀ = (1-δ)α + δq₀` (`routeDamped_at_zero`,
`routeDampedBA_at_zero`), and their gap is
`(1-δ)(β-r₀)·R₁ + (1-δ)(q₀-α)·R₂`, a vector of `span{R₁, R₂}` (`ladder_gap`).
So the marginals differ at zeroth order while every statistic whose differential
annihilates `R₁` and `R₂` does not.

**First order.**  The association's factor at `c = 0` on each route is
`q₀q₁t₀t₁ / Z` and `s₀s₁r₀r₁ / Z`, `Z = α(1-α)β(1-β)` (`kAB_at_zero`,
`kBA_at_zero`).  Their difference is `(1-δ)·H/Z` with `H` affine in `δ`
(`ladder_assoc_coeff`): zero at `δ = 1` (Proposition ORD) and nonzero at a
generic interior point (`ladder_assoc_coeff_witness`), so the association's
sequence effect is first order under partial adoption.  Divided by the product
of the four marginals, the factor is `1/Z` on both routes
(`ladder_oddsShadow_coeff`), so `assoc / mprod` has a sequence effect
`c · (bracket)` whose bracket vanishes at `c = 0` for every `δ`
(`ladder_oddsShadow_seqEffect`): it stays second order at every weight.

As in `PropPRO`, the passage from "the bracket is a rational function of `c`
that vanishes at `c = 0`" to "`O(c)`" is the elementary analytic step left to
the text; everything here is exact algebra.
-/
import JeffreyOrder.Anchoring
import JeffreyOrder.PropPRO

namespace JeffreyOrder
open Mat

variable {α β c q₀ r₀ δ : ℝ}

/-! ### Rescalings -/

/-- Multiply row `i` by `aᵢ` and column `j` by `bⱼ`. -/
def rescale (Q : Mat) (a₀ a₁ b₀ b₁ : ℝ) : Mat :=
  ⟨a₀ * b₀ * Q.a00, a₀ * b₁ * Q.a01, a₁ * b₀ * Q.a10, a₁ * b₁ * Q.a11⟩

/-- The product of the four cells. -/
def cellProd (Q : Mat) : ℝ := Q.a00 * Q.a01 * Q.a10 * Q.a11

/-- The product of the four marginals. -/
def mprod (Q : Mat) : ℝ := Q.mA0 * Q.mA1 * Q.mB0 * Q.mB1

theorem assoc_rescale (Q : Mat) (a₀ a₁ b₀ b₁ : ℝ) :
    assoc (rescale Q a₀ a₁ b₀ b₁) = (a₀ * a₁ * b₀ * b₁) * assoc Q := by
  simp only [assoc, rescale]; ring

theorem cellProd_rescale (Q : Mat) (a₀ a₁ b₀ b₁ : ℝ) :
    cellProd (rescale Q a₀ a₁ b₀ b₁) = (a₀ * a₁ * b₀ * b₁) ^ 2 * cellProd Q := by
  simp only [cellProd, rescale]; ring

/-- A rescaling with nonzero factors leaves the odds ratio unchanged. -/
theorem oddsRatio_rescale (Q : Mat) {a₀ a₁ b₀ b₁ : ℝ}
    (hK : a₀ * a₁ * b₀ * b₁ ≠ 0) :
    oddsRatio (rescale Q a₀ a₁ b₀ b₁) = oddsRatio Q := by
  simp only [oddsRatio, rescale]
  have hn : a₀ * b₀ * Q.a00 * (a₁ * b₁ * Q.a11) = (a₀ * a₁ * b₀ * b₁) * (Q.a00 * Q.a11) := by
    ring
  have hd : a₀ * b₁ * Q.a01 * (a₁ * b₀ * Q.a10) = (a₀ * a₁ * b₀ * b₁) * (Q.a01 * Q.a10) := by
    ring
  rw [hn, hd, mul_div_mul_left _ _ hK]

/-- `assoc² / cellProd`, a function of the odds ratio, is exactly invariant. -/
theorem assocSq_div_cellProd_rescale (Q : Mat) {a₀ a₁ b₀ b₁ : ℝ}
    (hK : a₀ * a₁ * b₀ * b₁ ≠ 0) :
    assoc (rescale Q a₀ a₁ b₀ b₁) ^ 2 / cellProd (rescale Q a₀ a₁ b₀ b₁)
      = assoc Q ^ 2 / cellProd Q := by
  rw [assoc_rescale, cellProd_rescale, mul_pow,
    mul_div_mul_left _ _ (pow_ne_zero 2 hK)]

/-- Every Jeffrey step on `A`, whatever its target, is a row rescaling. -/
theorem jeffreyA_eq_rescale (Q : Mat) (x : ℝ) :
    jeffreyA Q x = rescale Q (x / Q.mA0) ((1 - x) / Q.mA1) 1 1 := by
  ext <;> simp only [jeffreyA, rescale] <;> ring

/-- Every Jeffrey step on `B`, whatever its target, is a column rescaling.  A
damped step is a Jeffrey step to the damped target, so it is one too, and
Lemma SEP applies at every weight. -/
theorem jeffreyB_eq_rescale (Q : Mat) (x : ℝ) :
    jeffreyB Q x = rescale Q 1 1 (x / Q.mB0) ((1 - x) / Q.mB1) := by
  ext <;> simp only [jeffreyB, rescale] <;> ring

theorem assoc_prior : assoc (prior α β c) = c := by
  simp only [assoc, prior]; ring

/-! ### Each route's association is `c` times an explicit factor -/

/-- The factor multiplying `c` in the association of route `AB`. -/
noncomputable def kAB (α β c q₀ r₀ δ : ℝ) : ℝ :=
  (q₀ / (prior α β c).mA0) * ((1 - q₀) / (prior α β c).mA1)
    * ((1 - dampedTarget (jeffreyA (prior α β c) q₀) r₀ δ)
        / (jeffreyA (prior α β c) q₀).mB0)
    * ((1 - (1 - dampedTarget (jeffreyA (prior α β c) q₀) r₀ δ))
        / (jeffreyA (prior α β c) q₀).mB1)

/-- The factor multiplying `c` in the association of route `BA`. -/
noncomputable def kBA (α β c q₀ r₀ δ : ℝ) : ℝ :=
  (r₀ / (prior α β c).mB0) * ((1 - r₀) / (prior α β c).mB1)
    * ((1 - dampedTargetA (jeffreyB (prior α β c) r₀) q₀ δ)
        / (jeffreyB (prior α β c) r₀).mA0)
    * ((1 - (1 - dampedTargetA (jeffreyB (prior α β c) r₀) q₀ δ))
        / (jeffreyB (prior α β c) r₀).mA1)

theorem assoc_routeDamped :
    assoc (routeDamped α β c q₀ r₀ δ) = c * kAB α β c q₀ r₀ δ := by
  have h1 : assoc (jeffreyA (prior α β c) q₀)
      = (q₀ / (prior α β c).mA0) * ((1 - q₀) / (prior α β c).mA1) * c := by
    rw [jeffreyA_eq_rescale, assoc_rescale, assoc_prior]; ring
  unfold routeDamped dampedB
  rw [jeffreyB_eq_rescale, assoc_rescale, h1]
  unfold kAB; ring

theorem assoc_routeDampedBA :
    assoc (routeDampedBA α β c q₀ r₀ δ) = c * kBA α β c q₀ r₀ δ := by
  have h1 : assoc (jeffreyB (prior α β c) r₀)
      = (r₀ / (prior α β c).mB0) * ((1 - r₀) / (prior α β c).mB1) * c := by
    rw [jeffreyB_eq_rescale, assoc_rescale, assoc_prior]; ring
  unfold routeDampedBA dampedA
  rw [jeffreyA_eq_rescale, assoc_rescale, h1]
  unfold kBA; ring

/-! ### At `c = 0` both routes end at product measures -/

theorem jeffreyA_prior_zero (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0) :
    jeffreyA (prior α β 0) q₀ = indep q₀ β := by
  have e0 : (prior α β 0).mA0 = α := by simp only [prior, Mat.mA0]; ring
  have e1 : (prior α β 0).mA1 = 1 - α := by simp only [prior, Mat.mA1]; ring
  ext <;> simp only [jeffreyA, e0, e1] <;> simp only [prior, indep] <;> field_simp <;> ring

theorem jeffreyB_prior_zero (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    jeffreyB (prior α β 0) r₀ = indep α r₀ := by
  have e0 : (prior α β 0).mB0 = β := by simp only [prior, Mat.mB0]; ring
  have e1 : (prior α β 0).mB1 = 1 - β := by simp only [prior, Mat.mB1]; ring
  ext <;> simp only [jeffreyB, e0, e1] <;> simp only [prior, indep] <;> field_simp <;> ring

theorem routeDamped_at_zero (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    routeDamped α β 0 q₀ r₀ δ = indep q₀ ((1 - δ) * β + δ * r₀) := by
  unfold routeDamped dampedB dampedTarget
  rw [jeffreyA_prior_zero hα hα']
  have h0 : (indep q₀ β).mB0 = β := by simp only [indep, Mat.mB0]; ring
  have h1 : (indep q₀ β).mB1 = 1 - β := by simp only [indep, Mat.mB1]; ring
  ext <;> simp only [jeffreyB, h0, h1] <;> simp only [indep] <;> field_simp <;> ring

theorem routeDampedBA_at_zero (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    routeDampedBA α β 0 q₀ r₀ δ = indep ((1 - δ) * α + δ * q₀) r₀ := by
  unfold routeDampedBA dampedA dampedTargetA
  rw [jeffreyB_prior_zero hβ hβ']
  have h0 : (indep α r₀).mA0 = α := by simp only [indep, Mat.mA0]; ring
  have h1 : (indep α r₀).mA1 = 1 - α := by simp only [indep, Mat.mA1]; ring
  ext <;> simp only [jeffreyA, h0, h1] <;> simp only [indep] <;> field_simp <;> ring

/-- **The gap at `c = 0` lies in `span{R₁, R₂}`.**  Under partial adoption the
two sequences differ at zeroth order, but only along the two route directions,
which every protected statistic annihilates. -/
theorem ladder_gap (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    (routeDamped α β 0 q₀ r₀ δ).sub (routeDampedBA α β 0 q₀ r₀ δ)
      = (Mat.smul ((1 - δ) * (β - r₀)) (R1 q₀)).add
          (Mat.smul ((1 - δ) * (q₀ - α)) (R2 r₀)) := by
  rw [routeDamped_at_zero hα hα' hβ hβ', routeDampedBA_at_zero hα hα' hβ hβ']
  ext <;> simp only [Mat.sub, Mat.add, Mat.smul, R1, R2, indep] <;> ring

/-- The `A`-marginal gap at `c = 0` is `(1-δ)(α - q₀)`: zeroth order. -/
theorem ladder_gap_mA1 (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    (routeDamped α β 0 q₀ r₀ δ).mA1 - (routeDampedBA α β 0 q₀ r₀ δ).mA1
      = (1 - δ) * (α - q₀) := by
  rw [routeDamped_at_zero hα hα' hβ hβ', routeDampedBA_at_zero hα hα' hβ hβ']
  simp only [indep, Mat.mA1]; ring

/-! ### The first-order factors -/

theorem kAB_at_zero (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    kAB α β 0 q₀ r₀ δ
      = q₀ * (1 - q₀) * ((1 - δ) * β + δ * r₀) * (1 - ((1 - δ) * β + δ * r₀))
          / Zpar α β := by
  unfold kAB dampedTarget
  rw [jeffreyA_prior_zero hα hα']
  have e0 : (prior α β 0).mA0 = α := by simp only [prior, Mat.mA0]; ring
  have e1 : (prior α β 0).mA1 = 1 - α := by simp only [prior, Mat.mA1]; ring
  have h0 : (indep q₀ β).mB0 = β := by simp only [indep, Mat.mB0]; ring
  have h1 : (indep q₀ β).mB1 = 1 - β := by simp only [indep, Mat.mB1]; ring
  rw [e0, e1, h0, h1]
  unfold Zpar
  field_simp
  ring

theorem kBA_at_zero (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    kBA α β 0 q₀ r₀ δ
      = r₀ * (1 - r₀) * ((1 - δ) * α + δ * q₀) * (1 - ((1 - δ) * α + δ * q₀))
          / Zpar α β := by
  unfold kBA dampedTargetA
  rw [jeffreyB_prior_zero hβ hβ']
  have e0 : (prior α β 0).mB0 = β := by simp only [prior, Mat.mB0]; ring
  have e1 : (prior α β 0).mB1 = 1 - β := by simp only [prior, Mat.mB1]; ring
  have h0 : (indep α r₀).mA0 = α := by simp only [indep, Mat.mA0]; ring
  have h1 : (indep α r₀).mA1 = 1 - α := by simp only [indep, Mat.mA1]; ring
  rw [e0, e1, h0, h1]
  unfold Zpar
  field_simp
  ring

/-- The cofactor of the association's first-order sequence effect. -/
def Hcof (α β q₀ r₀ δ : ℝ) : ℝ :=
  q₀ * (1 - q₀) * (β * (1 - β) + δ * (β - r₀) ^ 2)
    - r₀ * (1 - r₀) * (α * (1 - α) + δ * (α - q₀) ^ 2)

/-- **The association moves at first order under partial adoption.**  The
difference of the two routes' first-order factors is `(1-δ)·H/Z`. -/
theorem ladder_assoc_coeff (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    kAB α β 0 q₀ r₀ δ - kBA α β 0 q₀ r₀ δ = (1 - δ) * Hcof α β q₀ r₀ δ / Zpar α β := by
  rw [kAB_at_zero hα hα' hβ hβ', kBA_at_zero hα hα' hβ hβ']
  unfold Hcof
  ring

/-- At full adoption the first-order factors agree (Proposition ORD). -/
theorem ladder_assoc_coeff_at_one (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    kAB α β 0 q₀ r₀ 1 - kBA α β 0 q₀ r₀ 1 = 0 := by
  rw [ladder_assoc_coeff hα hα' hβ hβ']; ring

/-- At the generic point of the sympy suite, `δ = 1/2`, the cofactor is nonzero. -/
theorem ladder_assoc_coeff_witness :
    (1 - (1/2 : ℝ)) * Hcof (1/3) (1/4) (2/5) (5/7) (1/2) ≠ 0 := by
  unfold Hcof; norm_num

/-- ...and at `δ = 0`. -/
theorem ladder_assoc_coeff_witness_zero :
    Hcof (1/3) (1/4) (2/5) (5/7) 0 ≠ 0 := by
  unfold Hcof; norm_num

/-! ### The first-order shadow of the odds ratio stays second order at every weight -/

theorem mprod_indep (x y : ℝ) : mprod (indep x y) = x * (1 - x) * (y * (1 - y)) := by
  simp only [mprod, indep, Mat.mA0, Mat.mA1, Mat.mB0, Mat.mB1]; ring

/-- Divided by the product of the four marginals, each route's first-order factor
is `1/Z`, the same on both routes and for every `δ`. -/
theorem ladder_oddsShadow_coeff (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0)
    (hq : q₀ * (1 - q₀) ≠ 0)
    (ht : ((1 - δ) * β + δ * r₀) * (1 - ((1 - δ) * β + δ * r₀)) ≠ 0)
    (hr : r₀ * (1 - r₀) ≠ 0)
    (hs : ((1 - δ) * α + δ * q₀) * (1 - ((1 - δ) * α + δ * q₀)) ≠ 0) :
    kAB α β 0 q₀ r₀ δ / mprod (routeDamped α β 0 q₀ r₀ δ) = 1 / Zpar α β ∧
    kBA α β 0 q₀ r₀ δ / mprod (routeDampedBA α β 0 q₀ r₀ δ) = 1 / Zpar α β := by
  have key : ∀ N Z : ℝ, N ≠ 0 → N / Z / N = 1 / Z := by
    intro N Z hN; rw [div_right_comm, div_self hN]
  refine ⟨?_, ?_⟩
  · rw [kAB_at_zero hα hα' hβ hβ', routeDamped_at_zero hα hα' hβ hβ', mprod_indep]
    have e : q₀ * (1 - q₀) * (((1 - δ) * β + δ * r₀) * (1 - ((1 - δ) * β + δ * r₀)))
        = q₀ * (1 - q₀) * ((1 - δ) * β + δ * r₀) * (1 - ((1 - δ) * β + δ * r₀)) := by ring
    have hm : q₀ * (1 - q₀) * ((1 - δ) * β + δ * r₀) * (1 - ((1 - δ) * β + δ * r₀)) ≠ 0 := by
      rw [← e]; exact mul_ne_zero hq ht
    rw [e, key _ _ hm]
  · rw [kBA_at_zero hα hα' hβ hβ', routeDampedBA_at_zero hα hα' hβ hβ', mprod_indep]
    have e : ((1 - δ) * α + δ * q₀) * (1 - ((1 - δ) * α + δ * q₀)) * (r₀ * (1 - r₀))
        = r₀ * (1 - r₀) * ((1 - δ) * α + δ * q₀) * (1 - ((1 - δ) * α + δ * q₀)) := by ring
    have hm : r₀ * (1 - r₀) * ((1 - δ) * α + δ * q₀) * (1 - ((1 - δ) * α + δ * q₀)) ≠ 0 := by
      rw [← e]; exact mul_ne_zero hs hr
    rw [e, key _ _ hm]

/-- **The odds-ratio shadow is protected at every weight.**  The sequence effect of
`assoc / mprod` is `c` times a bracket, exactly for every `c`, and the bracket
vanishes at `c = 0` for every `δ`. -/
theorem ladder_oddsShadow_seqEffect (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0)
    (hq : q₀ * (1 - q₀) ≠ 0)
    (ht : ((1 - δ) * β + δ * r₀) * (1 - ((1 - δ) * β + δ * r₀)) ≠ 0)
    (hr : r₀ * (1 - r₀) ≠ 0)
    (hs : ((1 - δ) * α + δ * q₀) * (1 - ((1 - δ) * α + δ * q₀)) ≠ 0) :
    (∀ c', assoc (routeDamped α β c' q₀ r₀ δ) / mprod (routeDamped α β c' q₀ r₀ δ)
        - assoc (routeDampedBA α β c' q₀ r₀ δ) / mprod (routeDampedBA α β c' q₀ r₀ δ)
      = c' * (kAB α β c' q₀ r₀ δ / mprod (routeDamped α β c' q₀ r₀ δ)
              - kBA α β c' q₀ r₀ δ / mprod (routeDampedBA α β c' q₀ r₀ δ))) ∧
    (kAB α β 0 q₀ r₀ δ / mprod (routeDamped α β 0 q₀ r₀ δ)
      - kBA α β 0 q₀ r₀ δ / mprod (routeDampedBA α β 0 q₀ r₀ δ) = 0) := by
  refine ⟨fun c' => ?_, ?_⟩
  · rw [assoc_routeDamped, assoc_routeDampedBA]; ring
  · obtain ⟨h1, h2⟩ := ladder_oddsShadow_coeff hα hα' hβ hβ' hq ht hr hs
    rw [h1, h2]; ring

/-! ### Factor inputs

Under the benchmark reading each cue supplies a factor on its own attribute and the
belief after both is the prior with cell `(i,j)` multiplied by `aᵢ bⱼ`, renormalised.
With both factors applied in full the two sequences coincide for every `c`
(`rescale_rescale`); this is the paper's benchmark and Hawthorne's factor-based
variants across distinct bases.  When the factor read second is adopted only in
part, replaced by some `a'` or `b'`, the two sequences end at `c = 0` at product
measures (`factorRoute_at_zero`) whose `A`-marginals agree iff the damped factor
has the same ratio as the full one (`factor_mA1_gap_iff`); the association is `c`
times an explicit factor (`assoc_factorRoute`); the odds ratio is the prior's
(`oddsRatio_factorRoute`); and the odds-ratio shadow's first-order factor is again
`1/Z` on both routes (`mprod_factorRoute_zero`, `oddsShadow_factor_coeff`).
The instantiation `a' = a^ω` is left to the text and the sympy suite. -/

/-- Both cues read as factors on their own attributes, applied and renormalised. -/
noncomputable def factorRoute (P : Mat) (a₀ a₁ b₀ b₁ : ℝ) : Mat :=
  (rescale P a₀ a₁ b₀ b₁).normalize

/-- Two factor updates on distinct attributes commute, whichever is applied first. -/
theorem rescale_rescale (P : Mat) (a₀ a₁ b₀ b₁ : ℝ) :
    rescale (rescale P a₀ a₁ 1 1) 1 1 b₀ b₁ = rescale (rescale P 1 1 b₀ b₁) a₀ a₁ 1 1 := by
  ext <;> simp only [rescale] <;> ring

theorem rescale_rescale_eq (P : Mat) (a₀ a₁ b₀ b₁ : ℝ) :
    rescale (rescale P a₀ a₁ 1 1) 1 1 b₀ b₁ = rescale P a₀ a₁ b₀ b₁ := by
  ext <;> simp only [rescale] <;> ring

theorem assoc_normalize (Q : Mat) : assoc Q.normalize = assoc Q / Q.total ^ 2 := by
  simp only [assoc, Mat.normalize]
  rw [div_mul_div_comm, div_mul_div_comm, ← sub_div, pow_two]

theorem oddsRatio_normalize (Q : Mat) (h : Q.total ≠ 0) :
    oddsRatio Q.normalize = oddsRatio Q := by
  simp only [oddsRatio, Mat.normalize]
  rw [div_mul_div_comm, div_mul_div_comm]
  have h2 : Q.total * Q.total ≠ 0 := mul_ne_zero h h
  rw [div_div_div_cancel_right₀ h2]

theorem oddsRatio_factorRoute (P : Mat) {a₀ a₁ b₀ b₁ : ℝ} (hK : a₀ * a₁ * b₀ * b₁ ≠ 0)
    (hT : (rescale P a₀ a₁ b₀ b₁).total ≠ 0) :
    oddsRatio (factorRoute P a₀ a₁ b₀ b₁) = oddsRatio P := by
  unfold factorRoute
  rw [oddsRatio_normalize _ hT, oddsRatio_rescale _ hK]

/-- The association of a factor route is `c` times the product of the four factors
over the square of the normalising sum, exactly. -/
theorem assoc_factorRoute (a₀ a₁ b₀ b₁ : ℝ) :
    assoc (factorRoute (prior α β c) a₀ a₁ b₀ b₁)
      = c * (a₀ * a₁ * b₀ * b₁) / (rescale (prior α β c) a₀ a₁ b₀ b₁).total ^ 2 := by
  unfold factorRoute
  rw [assoc_normalize, assoc_rescale, assoc_prior]
  ring

theorem total_rescale_prior_zero (a₀ a₁ b₀ b₁ : ℝ) :
    (rescale (prior α β 0) a₀ a₁ b₀ b₁).total
      = (α * a₀ + (1 - α) * a₁) * (β * b₀ + (1 - β) * b₁) := by
  simp only [Mat.total, rescale, prior]; ring

/-- At `c = 0` a factor route ends at a product measure. -/
theorem factorRoute_at_zero (a₀ a₁ b₀ b₁ : ℝ) (hA : α * a₀ + (1 - α) * a₁ ≠ 0)
    (hB : β * b₀ + (1 - β) * b₁ ≠ 0) :
    factorRoute (prior α β 0) a₀ a₁ b₀ b₁
      = indep (α * a₀ / (α * a₀ + (1 - α) * a₁)) (β * b₀ / (β * b₀ + (1 - β) * b₁)) := by
  unfold factorRoute
  simp only [Mat.normalize]
  rw [total_rescale_prior_zero]
  generalize hSA : α * a₀ + (1 - α) * a₁ = SA at hA ⊢
  generalize hSB : β * b₀ + (1 - β) * b₁ = SB at hB ⊢
  ext <;> simp only [rescale, prior, indep] <;> field_simp <;> subst hSA hSB <;> ring

theorem factorRoute_mA1_zero (a₀ a₁ b₀ b₁ : ℝ) (hA : α * a₀ + (1 - α) * a₁ ≠ 0)
    (hB : β * b₀ + (1 - β) * b₁ ≠ 0) :
    (factorRoute (prior α β 0) a₀ a₁ b₀ b₁).mA1 = (1 - α) * a₁ / (α * a₀ + (1 - α) * a₁) := by
  rw [factorRoute_at_zero a₀ a₁ b₀ b₁ hA hB]
  simp only [indep, Mat.mA1]
  field_simp
  ring

/-- The two sequences agree on the `A`-marginal at `c = 0` iff the damped factor
`a'` has the same ratio as the full factor `a`. -/
theorem factor_mA1_gap_iff {a₀ a₁ a₀' a₁' : ℝ} (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hA : α * a₀ + (1 - α) * a₁ ≠ 0) (hA' : α * a₀' + (1 - α) * a₁' ≠ 0) :
    (1 - α) * a₁ / (α * a₀ + (1 - α) * a₁) = (1 - α) * a₁' / (α * a₀' + (1 - α) * a₁')
      ↔ a₁ * a₀' = a₀ * a₁' := by
  rw [div_eq_div_iff hA hA']
  constructor
  · intro h
    have h2 : (1 - α) * α * (a₁ * a₀' - a₀ * a₁') = 0 := by linear_combination h
    rcases mul_eq_zero.1 h2 with h3 | h3
    · exact absurd h3 (mul_ne_zero hα' hα)
    · linarith
  · intro h; linear_combination (1 - α) * α * h

/-- The product of the four marginals of a factor route at `c = 0`. -/
theorem mprod_factorRoute_zero (a₀ a₁ b₀ b₁ : ℝ) (hA : α * a₀ + (1 - α) * a₁ ≠ 0)
    (hB : β * b₀ + (1 - β) * b₁ ≠ 0) :
    mprod (factorRoute (prior α β 0) a₀ a₁ b₀ b₁)
      = Zpar α β * (a₀ * a₁ * b₀ * b₁) / (rescale (prior α β 0) a₀ a₁ b₀ b₁).total ^ 2 := by
  rw [factorRoute_at_zero a₀ a₁ b₀ b₁ hA hB, mprod_indep, total_rescale_prior_zero]
  unfold Zpar
  field_simp
  ring

/-- Dividing the association's first-order factor by the marginal product gives `1/Z`
on every factor route, so the odds-ratio shadow is second order at every weight. -/
theorem oddsShadow_factor_coeff {K S Z : ℝ} (hK : K ≠ 0) (hS : S ≠ 0) (hZ : Z ≠ 0) :
    K / S ^ 2 / (Z * K / S ^ 2) = 1 / Z := by
  have hS2 : S ^ 2 ≠ 0 := pow_ne_zero 2 hS
  field_simp

end JeffreyOrder
