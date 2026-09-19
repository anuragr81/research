/-
# The anchoring family, and what the zero `c`-slope identifies

Defence material for the objection that the order effect might arise from
partial adoption of the later cue -- the belief-adjustment model of Hogarth and
Einhorn (1992) -- rather than from full adoption of each cue in turn.

The family.  Read the `A`-cue in full (a Jeffrey step to target `q`).  Then
respond to the `B`-cue by moving `B`'s marginal only a fraction `δ` of the way to
its target, that is, by a Jeffrey step to the damped target

  `(1-δ) * (current B-marginal) + δ * (1-r₀)`.

This is the averaging form `S_k = (1-w_k) S_{k-1} + w_k s(x_k)` of Hogarth and
Einhorn (1992, Eq. 4) applied to the marginal and embedded coherently in the
joint by a Jeffrey step.  Their model shares the amnestic feature of the paper's
rule: in their words, "memory is limited to the location of one's current
anchor and not how this was reached" (General Discussion).  The primacy they
predict arises from adjustment weights that decay over a long series, not from
a weight set to zero; the endpoint `δ = 0` below is this file's construction,
not theirs.  The "attention decrement" account of primacy that they discuss is
Anderson's (1981), reported by them as a rival.

`δ = 1` is amnestic pinning; `δ = 0` is the second cue ignored; `0 < δ < 1` is
anchoring.  The paper writes the weight as `ω`; `δ` is the code's name.

The results below are algebraic, and the key ones need no calculus at all:

  `dampedB_deviation`
      the post-step gap between the `B`-marginal and its delivered credence is
      exactly `(1-δ)` times the gap the first cue left.
  `routeDamped_mA1_deviation`
      the first-read marginal's gap from its delivered credence is exactly `δ`
      times its gap under the amnestic route.
  `orderEffect_damped_at_indep`
      at `c = 0` the order effect on `A`'s marginal is `(1-δ)(α - q₀)`.

Since the gap the first cue left carries the `Θ(c)` behaviour (the `A`-step
disturbs `B` through the prior covariance), the first factorisation says the
last-read marginal's `c`-dependence is scaled by `(1-δ)` and therefore vanishes
**only** at `δ = 1`.  Symmetrically, `dampedB_at_zero` shows the step is the
identity at `δ = 0`, so the *first*-read marginal is the one pinned exactly
there.

Hence the location of the `c`-invariant marginal identifies the endpoints: the
last-read attribute at `δ = 1`, the first-read attribute at `δ = 0`, and neither
in between.  Interior `δ` and the benchmark both leave neither marginal
invariant; the order effect at independence separates them, since the benchmark
has none at any `c` and interior `δ` has `(1-δ)(α - q₀)`.

Not covered: mechanisms outside this one-parameter family, in particular any
rule under which the credence a cue delivers depends on its position.
-/
import JeffreyOrder.Basic
import JeffreyOrder.PropIMM

namespace JeffreyOrder
open Mat

variable {α β c q₀ r₀ δ : ℝ}

/-- The damped target for `B`'s marginal: a fraction `δ` of the way from the
current marginal to the delivered credence `1 - r₀`. -/
noncomputable def dampedTarget (Q : Mat) (r₀ δ : ℝ) : ℝ :=
  (1 - δ) * Q.mB1 + δ * (1 - r₀)

/-- One damped response to the `B`-cue: a Jeffrey step to the damped target. -/
noncomputable def dampedB (Q : Mat) (r₀ δ : ℝ) : Mat :=
  jeffreyB Q (1 - dampedTarget Q r₀ δ)

/-- Route `AB` in the anchoring family: the `A`-cue in full, then the `B`-cue
damped by `δ`. -/
noncomputable def routeDamped (α β c q₀ r₀ δ : ℝ) : Mat :=
  dampedB (jeffreyA (prior α β c) q₀) r₀ δ

/-! ### Helper: a Jeffrey step attains its target marginal -/

/-- A Jeffrey step on `B` sets the `B`-marginal to `1 - r`. -/
theorem jeffreyB_mB1 (Q : Mat) (r : ℝ) (hB1 : Q.mB1 ≠ 0) :
    (jeffreyB Q r).mB1 = 1 - r := by
  simp only [Mat.mB1] at hB1
  unfold jeffreyB Mat.mB1
  field_simp

/-- A Jeffrey step on `A` sets the `A`-marginal to `1 - q`. -/
theorem jeffreyA_mA1 (Q : Mat) (q : ℝ) (hA1 : Q.mA1 ≠ 0) :
    (jeffreyA Q q).mA1 = 1 - q := by
  simp only [Mat.mA1] at hA1
  unfold jeffreyA Mat.mA1
  field_simp

/-! ### The damped step reaches its target -/

/-- The damped step attains the damped target. -/
theorem dampedB_mB1 (Q : Mat) (hB1 : Q.mB1 ≠ 0) :
    (dampedB Q r₀ δ).mB1 = dampedTarget Q r₀ δ := by
  unfold dampedB
  rw [jeffreyB_mB1 Q _ hB1]
  ring

/-! ### The key factorisation -/

/-- **The deviation is scaled by `(1-δ)`.**  After the damped step, the gap
between `B`'s marginal and the delivered credence `1 - r₀` is exactly `(1-δ)`
times the gap before the step.  No calculus is involved: this is an identity in
all of `Q`, `r₀` and `δ`. -/
theorem dampedB_deviation (Q : Mat) (hB1 : Q.mB1 ≠ 0) :
    (dampedB Q r₀ δ).mB1 - (1 - r₀) = (1 - δ) * (Q.mB1 - (1 - r₀)) := by
  rw [dampedB_mB1 Q hB1]
  unfold dampedTarget
  ring

/-- Consequently the whole `c`-dependence of the last-read marginal carries the
factor `(1-δ)`: across any two covariances the difference of deviations scales by
`(1-δ)`.  So the last-read marginal is `c`-invariant exactly when `δ = 1`, or
when the gap the first cue left is already `c`-invariant. -/
theorem dampedB_deviation_c_scaled (c₁ c₂ : ℝ)
    (h₁ : (jeffreyA (prior α β c₁) q₀).mB1 ≠ 0)
    (h₂ : (jeffreyA (prior α β c₂) q₀).mB1 ≠ 0) :
    (routeDamped α β c₁ q₀ r₀ δ).mB1 - (routeDamped α β c₂ q₀ r₀ δ).mB1
      = (1 - δ) * ((jeffreyA (prior α β c₁) q₀).mB1
                    - (jeffreyA (prior α β c₂) q₀).mB1) := by
  unfold routeDamped
  have e₁ := dampedB_deviation (r₀ := r₀) (δ := δ) _ h₁
  have e₂ := dampedB_deviation (r₀ := r₀) (δ := δ) _ h₂
  linarith [e₁, e₂]

/-! ### The two endpoints -/

/-- **Amnestic (`δ = 1`): the last-read marginal is pinned exactly.**  It equals
the delivered credence for every prior and every covariance, so its `c`-slope
vanishes at every order, not merely at first order. -/
theorem dampedB_at_one (Q : Mat) (hB1 : Q.mB1 ≠ 0) :
    (dampedB Q r₀ 1).mB1 = 1 - r₀ := by
  rw [dampedB_mB1 Q hB1]
  unfold dampedTarget
  ring

/-- **Pure primacy (`δ = 0`): the damped step is the identity.**  The second cue
is ignored entirely, so the belief is the one left by the first cue alone. -/
theorem dampedB_at_zero (Q : Mat) (hB0 : Q.mB0 ≠ 0) (hB1 : Q.mB1 ≠ 0)
    (htot : Q.mB0 + Q.mB1 = 1) :
    dampedB Q r₀ 0 = Q := by
  have hr : (1:ℝ) - dampedTarget Q r₀ 0 = Q.mB0 := by
    unfold dampedTarget; linarith
  have h1 : (1:ℝ) - Q.mB0 = Q.mB1 := by linarith
  unfold dampedB
  rw [hr]
  unfold jeffreyB
  ext
  · field_simp
  · rw [h1]; field_simp
  · field_simp
  · rw [h1]; field_simp

/-- Hence at `δ = 0` the **first**-read marginal is the pinned one: the `A`-step
sets `A`'s marginal to the delivered credence and the ignored `B`-cue leaves it
untouched. -/
theorem routeDamped_at_zero_pins_A
    (hB0 : (jeffreyA (prior α β c) q₀).mB0 ≠ 0)
    (hB1 : (jeffreyA (prior α β c) q₀).mB1 ≠ 0)
    (htot : (jeffreyA (prior α β c) q₀).mB0 + (jeffreyA (prior α β c) q₀).mB1 = 1)
    (hA1 : (prior α β c).mA1 ≠ 0) :
    (routeDamped α β c q₀ r₀ 0).mA1 = 1 - q₀ := by
  unfold routeDamped
  rw [dampedB_at_zero _ hB0 hB1 htot]
  exact jeffreyA_mA1 _ _ hA1

/-! ### The first-read marginal under the damped step

The damped step is a Jeffrey step, so it moves `A`'s marginal as well.  It does
so by the same convex mixture that defines the target: `A`'s marginal after the
damped `B`-step is `(1-δ)` times what it was plus `δ` times what the full step
would have made it.  Hence the first-read marginal's departure from its
delivered credence is exactly `δ` times its departure under the amnestic route
`PJab`.  With `dampedB_deviation` this gives both factors of the identification
table exactly: the last-read marginal's `c`-dependence carries `(1-δ)`, the
first-read marginal's carries `δ`. -/

/-- The damped `B`-step moves `A`'s marginal by the same convex mixture. -/
theorem dampedB_mA1_mix (Q : Mat) (hB0 : Q.mB0 ≠ 0) (hB1 : Q.mB1 ≠ 0)
    (htot : Q.mB0 + Q.mB1 = 1) :
    (dampedB Q r₀ δ).mA1 = (1 - δ) * Q.mA1 + δ * (jeffreyB Q r₀).mA1 := by
  have h0 : Q.mB0 = 1 - Q.mB1 := by linarith
  have hB0' : (1:ℝ) - Q.mB1 ≠ 0 := by rw [← h0]; exact hB0
  have key : (1:ℝ) - dampedTarget Q r₀ δ = (1 - δ) * (1 - Q.mB1) + δ * r₀ := by
    unfold dampedTarget; ring
  simp only [dampedB, jeffreyB, Mat.mA1]
  rw [key, h0]
  field_simp
  ring

/-- **The first-read marginal's departure carries the factor `δ`.**  Along the
damped route the first-read marginal differs from its delivered credence by
exactly `δ` times its difference under the amnestic route. -/
theorem routeDamped_mA1_deviation
    (hB0 : (jeffreyA (prior α β c) q₀).mB0 ≠ 0)
    (hB1 : (jeffreyA (prior α β c) q₀).mB1 ≠ 0)
    (htot : (jeffreyA (prior α β c) q₀).mB0 + (jeffreyA (prior α β c) q₀).mB1 = 1)
    (hA1 : (prior α β c).mA1 ≠ 0) :
    (routeDamped α β c q₀ r₀ δ).mA1 - (1 - q₀)
      = δ * ((PJab α β c q₀ r₀).mA1 - (1 - q₀)) := by
  unfold routeDamped
  rw [dampedB_mA1_mix _ hB0 hB1 htot, jeffreyA_mA1 _ _ hA1]
  simp only [PJab]
  ring

/-! ### The mirror route, and the order effect of a damped updater

Route `BA` in the family: the `B`-cue in full, then the `A`-cue damped by `δ`.
Comparing the two routes gives the order effect of the damped updater in closed
form.  It is a `δ`-mixture of two gaps: the amnestic order effect (Proposition
ORD, which is `Θ(c)`) and the distance the damped cue fails to travel, which is
present already at `c = 0`.  So at independence the damped updater shows an
order effect of size `(1-δ)(α - q₀)` on `A`'s marginal, where the amnestic
updater (Proposition IMM) and the benchmark show none.  This is what separates
an interior weight from the benchmark: both leave neither marginal `c`-invariant,
but only the interior weight produces an order effect at `c = 0`. -/

/-- The damped target for `A`'s marginal. -/
noncomputable def dampedTargetA (Q : Mat) (q₀ δ : ℝ) : ℝ :=
  (1 - δ) * Q.mA1 + δ * (1 - q₀)

/-- One damped response to the `A`-cue. -/
noncomputable def dampedA (Q : Mat) (q₀ δ : ℝ) : Mat :=
  jeffreyA Q (1 - dampedTargetA Q q₀ δ)

/-- Route `BA` in the anchoring family: the `B`-cue in full, then the `A`-cue
damped by `δ`. -/
noncomputable def routeDampedBA (α β c q₀ r₀ δ : ℝ) : Mat :=
  dampedA (jeffreyB (prior α β c) r₀) q₀ δ

/-- The damped `A`-step attains its damped target. -/
theorem dampedA_mA1 (Q : Mat) (hA1 : Q.mA1 ≠ 0) :
    (dampedA Q q₀ δ).mA1 = (1 - δ) * Q.mA1 + δ * (1 - q₀) := by
  unfold dampedA
  rw [jeffreyA_mA1 Q _ hA1]
  unfold dampedTargetA
  ring

/-- **The order effect of the damped updater, exactly.**  On `A`'s marginal it is
`δ` times the amnestic order effect plus `(1-δ)` times the gap the damped
`A`-cue leaves open. -/
theorem orderEffect_damped_mA1
    (hB0 : (jeffreyA (prior α β c) q₀).mB0 ≠ 0)
    (hB1 : (jeffreyA (prior α β c) q₀).mB1 ≠ 0)
    (htot : (jeffreyA (prior α β c) q₀).mB0 + (jeffreyA (prior α β c) q₀).mB1 = 1)
    (hA1 : (prior α β c).mA1 ≠ 0)
    (hA1' : (jeffreyB (prior α β c) r₀).mA1 ≠ 0) :
    (routeDamped α β c q₀ r₀ δ).mA1 - (routeDampedBA α β c q₀ r₀ δ).mA1
      = δ * ((PJab α β c q₀ r₀).mA1 - (1 - q₀))
        + (1 - δ) * ((1 - q₀) - (jeffreyB (prior α β c) r₀).mA1) := by
  have h := routeDamped_mA1_deviation (r₀ := r₀) (δ := δ) hB0 hB1 htot hA1
  unfold routeDampedBA
  rw [dampedA_mA1 _ hA1']
  linear_combination h

/-- **At independence the damped updater's order effect is `(1-δ)(α - q₀)`.**
The amnestic term vanishes there (Proposition IMM), leaving only the gap the
damped cue does not close.  It is zero exactly at `δ = 1`, or when the `A`-cue
delivers the prior marginal. -/
theorem orderEffect_damped_at_indep (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    (routeDamped α β 0 q₀ r₀ δ).mA1 - (routeDampedBA α β 0 q₀ r₀ δ).mA1
      = (1 - δ) * (α - q₀) := by
  have hstepA := jeffreyA_prior (β := β) (c := 0) (q₀ := q₀) hα hα'
  have hB0 : (jeffreyA (prior α β 0) q₀).mB0 = β := by
    rw [hstepA]; simp only [Mat.mB0]; field_simp; ring
  have hB1 : (jeffreyA (prior α β 0) q₀).mB1 = 1 - β := by
    rw [hstepA]; simp only [Mat.mB1]; field_simp; ring
  have hstepB := jeffreyB_prior (α := α) (c := 0) (r₀ := r₀) hβ hβ'
  have hA1' : (jeffreyB (prior α β 0) r₀).mA1 = 1 - α := by
    rw [hstepB]; simp only [Mat.mA1]; field_simp; ring
  have hPJ : (PJab α β 0 q₀ r₀).mA1 = 1 - q₀ := by
    rw [PJab_at_zero hα hα' hβ hβ']; simp only [indep, Mat.mA1]; ring
  rw [orderEffect_damped_mA1 (by rw [hB0]; exact hβ) (by rw [hB1]; exact hβ')
        (by rw [hB0, hB1]; ring) (by rw [prior_mA1]; exact hα')
        (by rw [hA1']; exact hα')]
  rw [hPJ, hA1']
  ring

end JeffreyOrder
