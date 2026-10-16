import IscLean.Reassignment
import Mathlib.Tactic

/-!
Test of the two-stage closure rule, option (a) for M29, in the smallest pool where closure
can act: two entrants in each status group (`two_one_pool_forces_crossing` shows a pool of
two and one gives closure nothing to do).

The rule. Stage one: each entrant, independently, goes into their own group's lottery with
chance `σ` and into the joint lottery otherwise. Stage two: every lottery reassigns its
members' former jobs at random as in M36. A lottery with fewer than two members sends its
members to the joint lottery. In the pool of two and two the joint lottery then has zero,
two or four members, so no further rule is needed.

With entrants `A1, A2` in one group and `B1, B2` in the other, the final lotteries depend only
on whether both `A`s chose their own lottery (`nA = 2`) and whether both `B`s did (`nB = 2`).
If either pair did, `A1` and `A2` end in a lottery of two, which is the exchange, and so do
`B1` and `B2`; otherwise all four are in the joint lottery, where each entrant is equally
likely to receive each other's former job (M36), chance `1/3`, and exactly one of the nine
reassignments keeps every job within its group.
-/

open Finset

namespace Isc

/-- Chance of a stage-one outcome with `k` entrants in their own lotteries out of four. -/
noncomputable def stageOne (σ : ℝ) (k : ℕ) : ℝ := σ ^ k * (1 - σ) ^ (4 - k)

/-- Number of the four entrants who chose their own lottery. -/
def ownCount (a1 a2 b1 b2 : Bool) : ℕ := a1.toNat + a2.toNat + b1.toNat + b2.toNat

/-- Whether `A1` and `A2` end in a lottery of two. -/
def pairedA (a1 a2 b1 b2 : Bool) : Bool := (a1 && a2) || (b1 && b2)

/-- Chance that `A1` receives `A2`'s former job, given the outcome. -/
noncomputable def withinChance (a1 a2 b1 b2 : Bool) : ℝ :=
  if pairedA a1 a2 b1 b2 then 1 else 1 / 3

/-- Chance that no job crosses the groups, given the outcome. -/
noncomputable def noCrossChance (a1 a2 b1 b2 : Bool) : ℝ :=
  if pairedA a1 a2 b1 b2 then 1 else 1 / 9

/-- Expected value of `f` over the sixteen stage-one outcomes. -/
noncomputable def twoStage (σ : ℝ) (f : Bool → Bool → Bool → Bool → ℝ) : ℝ :=
  ∑ a1 : Bool, ∑ a2 : Bool, ∑ b1 : Bool, ∑ b2 : Bool,
    stageOne σ (ownCount a1 a2 b1 b2) * f a1 a2 b1 b2

/-- Under the two-stage rule, `A1` receives a partner from its own group with chance
`(1 - σ²)² / 3 + σ² (2 - σ²)`. -/
theorem two_stage_within_chance (σ : ℝ) :
    twoStage σ withinChance = (1 - σ ^ 2) ^ 2 / 3 + σ ^ 2 * (2 - σ ^ 2) := by
  simp only [twoStage, Fintype.sum_bool, stageOne, ownCount, withinChance, pairedA]
  simp
  ring

/-- Under the two-stage rule, the chance that no job crosses the groups, so that the groups'
positions on the ladder are unchanged by the lottery, is `(1 - σ²)² / 9 + σ² (2 - σ²)`. -/
theorem two_stage_no_cross_chance (σ : ℝ) :
    twoStage σ noCrossChance = (1 - σ ^ 2) ^ 2 / 9 + σ ^ 2 * (2 - σ ^ 2) := by
  simp only [twoStage, Fintype.sum_bool, stageOne, ownCount, noCrossChance, pairedA]
  simp
  ring

/-- The limits: no closure gives the joint lottery's `1/3`, full closure gives `1`. -/
theorem two_stage_limits :
    twoStage 0 withinChance = 1 / 3 ∧ twoStage 1 withinChance = 1 := by
  rw [two_stage_within_chance, two_stage_within_chance]
  norm_num

/-- The chance that the groups' positions survive the lottery rises from `1/9` with no closure
to `1` with full closure. -/
theorem two_stage_persistence_limits :
    twoStage 0 noCrossChance = 1 / 9 ∧ twoStage 1 noCrossChance = 1 := by
  rw [two_stage_no_cross_chance, two_stage_no_cross_chance]
  norm_num

/-- Both chances rise with closure on `[0, 1]`. -/
theorem two_stage_chances_rise {σ₁ σ₂ : ℝ} (h0 : 0 ≤ σ₁) (h12 : σ₁ < σ₂) (h1 : σ₂ ≤ 1) :
    twoStage σ₁ withinChance < twoStage σ₂ withinChance ∧
      twoStage σ₁ noCrossChance < twoStage σ₂ noCrossChance := by
  rw [two_stage_within_chance, two_stage_within_chance, two_stage_no_cross_chance,
    two_stage_no_cross_chance]
  have hsq : σ₁ ^ 2 < σ₂ ^ 2 := by nlinarith
  have hsq1 : σ₂ ^ 2 ≤ 1 := by nlinarith
  constructor <;> nlinarith [sq_nonneg (σ₁ ^ 2 - σ₂ ^ 2)]

/-- The one-shot effect of closure under the two-stage rule is second order in `σ`: the excess
of the within-group chance over the no-closure value `1/3` is `(2/3) σ² (2 - σ²)`. -/
theorem two_stage_effect_second_order (σ : ℝ) :
    twoStage σ withinChance - 1 / 3 = 2 / 3 * σ ^ 2 * (2 - σ ^ 2) := by
  rw [two_stage_within_chance]
  ring

/-- M29's per-entrant weights in the same pool give `A1` a partner from its own group with
chance `1 / (3 - 2σ)`, which is first order in `σ`. The two rules cross: at `σ = 1/10` the
weights are more closed, at `σ = 1/2` the two-stage rule is. -/
theorem two_stage_vs_weights_cross :
    twoStage (1 / 10) withinChance < 1 / (3 - 2 * (1 / 10)) ∧
      1 / (3 - 2 * (1 / 2)) < twoStage (1 / 2) withinChance := by
  rw [two_stage_within_chance, two_stage_within_chance]
  norm_num

/-- In the joint lottery of four, nine reassignments leave nobody with their own former job,
and exactly one keeps every job within its group. -/
theorem joint_four_counts :
    (univ.filter (fun π : Equiv.Perm (Fin 4) => ∀ x, π x ≠ x)).card = 9 ∧
      (univ.filter (fun π : Equiv.Perm (Fin 4) => (∀ x, π x ≠ x) ∧
        ∀ x, decide (x.val < 2) = decide ((π x).val < 2))).card = 1 := by
  decide

/-- Control: the chances of the sixteen outcomes sum to one. -/
theorem control_stage_one_total (σ : ℝ) : twoStage σ (fun _ _ _ _ => 1) = 1 := by
  simp only [twoStage, Fintype.sum_bool, stageOne, ownCount]
  simp
  ring

/-- Control: without the merging of lone lotteries the chances would not sum to one over the
outcomes that leave every lottery with two or more members, since the outcomes with one `A`
or one `B` alone carry positive chance at interior `σ`. -/
theorem control_lone_outcomes_have_weight {σ : ℝ} (h0 : 0 < σ) (h1 : σ < 1) :
    0 < stageOne σ 1 := by
  unfold stageOne
  have : 0 < 1 - σ := by linarith
  positivity

end Isc
