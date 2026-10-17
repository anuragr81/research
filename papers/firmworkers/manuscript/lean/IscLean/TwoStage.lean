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

/-! ### The structure of the groups under the two-stage rule -/

/-- Any reassignment conserves the number of jobs each status group holds, because every
entrant receives exactly one former job. -/
theorem group_job_count_conserved {ι : Type*} [DecidableEq ι] (P : Finset ι) (g : ι → Bool)
    (π : Equiv.Perm ι) (h : Bool) :
    ((P.filter (fun i => g i = h)).image π).card = (P.filter (fun i => g i = h)).card :=
  card_image_of_injective _ π.injective

/-- The nine reassignments of a joint lottery of four. -/
def derangements4 : Finset (Equiv.Perm (Fin 4)) := univ.filter (fun π => ∀ x, π x ≠ x)

theorem derangements4_fibers : ∀ i k : Fin 4, i ≠ k →
    (derangements4.filter (fun π => π i = k)).card = 3 := by decide

theorem derangements4_self : ∀ i : Fin 4, (derangements4.filter (fun π => π i = i)).card = 0 := by
  decide

/-- Summed over the nine reassignments, an entrant's new job is worth three times the total of
the others' former jobs. -/
theorem sum_over_derangements4 (x : Fin 4 → ℝ) (i : Fin 4) :
    ∑ π ∈ derangements4, x (π i) = 3 * (∑ k, x k - x i) := by
  rw [← Finset.sum_fiberwise_of_maps_to (g := fun π => π i) (t := univ) (fun _ _ => mem_univ _)]
  have h : ∀ k, ∑ π ∈ derangements4.filter (fun π => π i = k), x (π i)
      = (derangements4.filter (fun π => π i = k)).card * x k := by
    intro k
    rw [sum_congr rfl (fun π hπ => by rw [(mem_filter.mp hπ).2]), sum_const, nsmul_eq_mul]
  simp_rw [h]
  rw [← sum_erase_add _ _ (mem_univ i), derangements4_self i]
  rw [sum_congr rfl (fun k hk => by rw [derangements4_fibers i k (ne_of_mem_erase hk).symm])]
  rw [sum_erase_eq_sub (mem_univ i), ← Finset.mul_sum]
  push_cast
  ring

/-- In the joint lottery of four, the expected total of the jobs held by entrants `0` and `1`
after the lottery is `(2S - T)/3`, with `S` the total of all four former jobs and `T` the
total of theirs. -/
theorem joint_expected_group_total (x : Fin 4 → ℝ) :
    (∑ π ∈ derangements4, (x (π 0) + x (π 1))) / 9
      = (2 * (∑ k, x k) - (x 0 + x 1)) / 3 := by
  rw [sum_add_distrib, sum_over_derangements4, sum_over_derangements4]
  ring

/-- Expected total of group `A`'s jobs after the lottery, given the stage-one outcome: unchanged
when the pairs exchange within their groups, `(2S - T)/3` in the joint lottery. -/
noncomputable def groupTotalAfter (T S : ℝ) (a1 a2 b1 b2 : Bool) : ℝ :=
  if pairedA a1 a2 b1 b2 then T else (2 * S - T) / 3

/-- The persistence coefficient of the groups' positions. -/
noncomputable def persistence (σ : ℝ) : ℝ := σ ^ 2 * (2 - σ ^ 2) - (1 - σ ^ 2) ^ 2 / 3

/-- Under the two-stage rule, the expected gap between group `A`'s total and half the pool's
total after the lottery is the gap before times `persistence σ`. -/
theorem two_stage_gap_recursion (σ T S : ℝ) :
    twoStage σ (groupTotalAfter T S) - S / 2 = persistence σ * (T - S / 2) := by
  simp only [twoStage, Fintype.sum_bool, stageOne, ownCount, groupTotalAfter, pairedA, persistence]
  simp
  ring

/-- Without closure the gap reverses and shrinks to a third, because nobody keeps their own
former job; with full closure it persists entire. -/
theorem persistence_limits : persistence 0 = -(1 / 3) ∧ persistence 1 = 1 := by
  unfold persistence
  norm_num

/-- The persistence coefficient rises with closure on `[0, 1]`. -/
theorem persistence_rises {σ₁ σ₂ : ℝ} (h0 : 0 ≤ σ₁) (h12 : σ₁ < σ₂) (h1 : σ₂ ≤ 1) :
    persistence σ₁ < persistence σ₂ := by
  unfold persistence
  have hsq : σ₁ ^ 2 < σ₂ ^ 2 := by nlinarith
  have hsq1 : σ₂ ^ 2 ≤ 1 := by nlinarith
  nlinarith [sq_nonneg (σ₁ ^ 2 - σ₂ ^ 2)]

/-- Control: the recursion is exact for the no-closure pool, the joint lottery of four. -/
theorem control_gap_recursion_no_closure (T S : ℝ) :
    twoStage 0 (groupTotalAfter T S) - S / 2 = -(1 / 3) * (T - S / 2) := by
  rw [two_stage_gap_recursion, persistence_limits.1]

end Isc
