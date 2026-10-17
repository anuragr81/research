import IscLean.Routes
import Mathlib.Logic.Relation
import Mathlib.Tactic

/-!
The mobility test for a fallback rule, before any decision on issue 12.

Under uniform displacement every set of workers is displaced together with positive
probability, in particular a pool of exactly two: the top worker and a worker of rank `i`.
In that pool the lottery is a sure exchange of jobs (M36), so the path from `i` to the top
is open as soon as both weakly prefer the lottery to their other two routes. The theorems
state the preferences for every nondecreasing utility `u`; the sure exchange means no
risk attitude enters.

Two rules are compared. Under the bottom rung (M32 as decided) the top accepts any entrant
whose income less the lottery cost is at least the bottom income. Under a proportional step
the top refuses every entrant whose income less the lottery cost is below its own reduced
pay. A third theorem gives the case common to both rules: when route choices are a fixed
function of rank, a rank that returns is sealed, nobody enters it and nobody leaves.
-/

namespace Isc

/-- Bottom rung. The top worker, whose return is worth no more than the bottom income
(`hcost`), weakly prefers the sure exchange with any entrant `i` whose income less the
lottery cost is at least the bottom income. -/
theorem bottom_rung_top_accepts_entrant {u : ℝ → ℝ} {S : Set ℝ} (hu : MonotoneOn u S)
    {x0 xtop xi L κ b : ℝ} (hx0 : x0 ∈ S) (hi : xi - L ∈ S)
    (hret : returnFromLoss κ b x0 (xtop - x0) ∈ S) (hloss : 0 < xtop - x0)
    (hcost : 1 ≤ κ * (xtop - x0) ^ (b - 1)) (hL : x0 ≤ xi - L) :
    u x0 ≤ u (xi - L) ∧ u (returnFromLoss κ b x0 (xtop - x0)) ≤ u (xi - L) := by
  have hle : returnFromLoss κ b x0 (xtop - x0) ≤ x0 := by
    unfold returnFromLoss
    have e : κ * (xtop - x0) ^ b = κ * (xtop - x0) ^ (b - 1) * (xtop - x0) := by
      rw [Real.rpow_sub_one hloss.ne']
      field_simp
    rw [e]
    nlinarith
  exact ⟨hu hx0 hi hL, hu hret hi (le_trans hle hL)⟩

/-- Bottom rung. A worker of rank `i` whose income is at least the lottery cost below the top
weakly prefers the sure exchange to the downgrade and to the return, for any upskilling
cost. -/
theorem entrant_accepts_top {u : ℝ → ℝ} {S : Set ℝ} (hu : MonotoneOn u S)
    {x0 xtop xi L κ b : ℝ} (hx0 : x0 ∈ S) (htop : xtop - L ∈ S)
    (hret : returnFromLoss κ b x0 (xi - x0) ∈ S) (hκ : 0 ≤ κ) (h0 : x0 ≤ xi)
    (hL : xi ≤ xtop - L) (hL0 : 0 ≤ L) :
    u x0 ≤ u (xtop - L) ∧ u (returnFromLoss κ b x0 (xi - x0)) ≤ u (xtop - L) := by
  have h1 : x0 ≤ xtop - L := by linarith
  have h2 : returnFromLoss κ b x0 (xi - x0) ≤ xtop - L :=
    le_trans (returnFromLoss_le_floor_plus_loss hκ (by linarith)) (by linarith)
  exact ⟨hu hx0 htop h1, hu hret htop h2⟩

/-- Proportional step. The top worker strictly prefers keeping the job at reduced pay
`(1 - δ) xtop` to a sure exchange with any entrant whose income less the lottery cost is
below that reduced pay, for every strictly increasing utility. -/
theorem proportional_top_refuses_entrant {u : ℝ → ℝ} {S : Set ℝ} (hu : StrictMonoOn u S)
    {δ xtop xi L : ℝ} (hi : xi - L ∈ S) (hred : (1 - δ) * xtop ∈ S)
    (hlow : xi - L < (1 - δ) * xtop) : u (xi - L) < u ((1 - δ) * xtop) :=
  hu hi hred hlow

/-- On the ladder of M3, the top's reduced pay exceeds the income of every rank below a
quantile that depends on `δ` and `a`, so under the proportional step the top refuses the
exchange with all of those ranks. Example: `a = 2`, `δ = 1/4`, top quantile `99/100`, the
reduced pay `7.5 x0` exceeds the income of every rank up to quantile `8/9` *(incomes
`x0 (1 - q)^(-1/a)`)*. -/
theorem control_proportional_refusal_reaches_far_down :
    (1 - 1 / 4) * pareto 1 2 (99 / 100) > pareto 1 2 (8 / 9) + 1 / 10 := by
  unfold pareto
  rw [show (1 : ℝ) - 99 / 100 = (1 / 10) ^ (2 : ℝ) by norm_num,
    show (1 : ℝ) - 8 / 9 = (1 / 3) ^ (2 : ℝ) by norm_num,
    ← Real.rpow_mul (by norm_num), ← Real.rpow_mul (by norm_num)]
  norm_num

/-- Bottom rung, control: with a cheap return the top refuses a low entrant. At `κ = 1/4`,
`b = 1`, the top with income `10` and bottom income `1` returns for `9/4` and keeps
`31/4`, which beats an entrant paying `3` less a lottery cost of `1/10`. -/
theorem control_bottom_rung_needs_cost_bound :
    returnFromLoss (1 / 4) 1 1 (10 - 1) > 3 - 1 / 10 := by
  unfold returnFromLoss
  norm_num

/-- With the same reduced pay, an entrant paying at least the top's reduced pay plus the
lottery cost is accepted under the proportional step too. -/
theorem control_proportional_top_accepts_near_entrant {u : ℝ → ℝ} {S : Set ℝ}
    (hu : MonotoneOn u S) {δ xtop xi L : ℝ} (hi : xi - L ∈ S) (hred : (1 - δ) * xtop ∈ S)
    (hhigh : (1 - δ) * xtop ≤ xi - L) : u ((1 - δ) * xtop) ≤ u (xi - L) :=
  hu hred hi hhigh

/-! ### Sealed ranks under a fixed route rule -/

inductive Route
  | down | ret | lot

/-- One-period moves of a job when routes are a fixed function of rank and only the lottery
moves jobs: a job can pass from `i` to `j ≠ i` only if both choose the lottery. -/
def lotteryMove {ι : Type*} (route : ι → Route) (i j : ι) : Prop :=
  i = j ∨ (route i = Route.lot ∧ route j = Route.lot)

/-- A rank that returns is sealed: nothing reachable from it but itself. -/
theorem return_rank_sealed {ι : Type*} (route : ι → Route) {i j : ι}
    (hi : route i = Route.ret) (h : Relation.ReflTransGen (lotteryMove route) i j) : j = i := by
  induction h with
  | refl => rfl
  | tail _ hstep ih =>
    subst ih
    rcases hstep with h | ⟨h1, _⟩
    · exact h.symm
    · rw [hi] at h1
      cases h1

/-- Nothing reaches a rank that returns, except itself. -/
theorem return_rank_unreachable {ι : Type*} (route : ι → Route) {i j : ι}
    (hj : route j = Route.ret) (h : Relation.ReflTransGen (lotteryMove route) i j) : i = j := by
  induction h with
  | refl => rfl
  | tail _ hstep ih =>
    rcases hstep with h | ⟨_, h2⟩
    · subst h
      exact ih hj
    · rw [hj] at h2
      cases h2

/-- Two lottery ranks reach each other in one move. -/
theorem lottery_ranks_communicate {ι : Type*} (route : ι → Route) {i j : ι}
    (hi : route i = Route.lot) (hj : route j = Route.lot) : lotteryMove route i j :=
  Or.inr ⟨hi, hj⟩

/-! ### What the lottery cost does -/

/-- For every nondecreasing utility, the lottery is worth less at a higher lottery cost. -/
theorem poolEU_antitone_in_L {ι : Type*} [DecidableEq ι] {P : Finset ι} (h2 : 2 ≤ P.card)
    (x : ι → ℝ) {L L' : ℝ} {u : ℝ → ℝ} {S : Set ℝ} (hu : MonotoneOn u S)
    (hS : ∀ k ∈ P, x k - L ∈ S) (hS' : ∀ k ∈ P, x k - L' ∈ S) (hLL : L ≤ L') (i : ι) :
    poolEU P x L' u i ≤ poolEU P x L u i := by
  have hpos : (0 : ℝ) ≤ (P.card : ℝ) - 1 := by
    have : (2 : ℝ) ≤ (P.card : ℝ) := by exact_mod_cast h2
    linarith
  unfold poolEU
  apply div_le_div_of_nonneg_right _ hpos
  apply Finset.sum_le_sum
  intro k hk
  exact hu (hS' k (Finset.mem_of_mem_erase hk)) (hS k (Finset.mem_of_mem_erase hk)) (by linarith)

/-- Whoever prefers the return to the lottery still does at any higher lottery cost, so the set
of returners grows with the lottery cost. -/
theorem return_keeps_beating_lottery_as_L_rises {ι : Type*} [DecidableEq ι] {P : Finset ι}
    (h2 : 2 ≤ P.card) (x : ι → ℝ) {L L' r : ℝ} {u : ℝ → ℝ} {S : Set ℝ} (hu : MonotoneOn u S)
    (hS : ∀ k ∈ P, x k - L ∈ S) (hS' : ∀ k ∈ P, x k - L' ∈ S) (hLL : L ≤ L') {i : ι}
    (h : poolEU P x L u i ≤ u r) : poolEU P x L' u i ≤ u r :=
  le_trans (poolEU_antitone_in_L h2 x hu hS hS' hLL i) h

/-- Bottom rung: the top strictly prefers the downgrade to a sure exchange with an entrant whose
former income less the lottery cost is below the bottom income, for every strictly increasing
utility. So the lottery cost excludes the poorest entrants from the exchange. -/
theorem top_refuses_below_floor {u : ℝ → ℝ} {S : Set ℝ} (hu : StrictMonoOn u S)
    {x0 xi L : ℝ} (hi : xi - L ∈ S) (hx0 : x0 ∈ S) (hlow : xi - L < x0) : u (xi - L) < u x0 :=
  hu hi hx0 hlow

/-- The exchange window: an entrant of rank `i` is accepted by the top and accepts the top (M43's
conditions) exactly when `x0 + L ≤ xi ≤ xtop - L`. The window shrinks at both ends as the
lottery cost rises. -/
theorem exchange_window_shrinks {x0 xtop L L' : ℝ} (hLL : L ≤ L') :
    Set.Icc (x0 + L') (xtop - L') ⊆ Set.Icc (x0 + L) (xtop - L) := by
  intro xi hxi
  exact ⟨by linarith [hxi.1], by linarith [hxi.2]⟩

/-- The window is empty once the lottery cost exceeds half the distance from the bottom income
to the top. -/
theorem exchange_window_empty {x0 xtop L : ℝ} (h : xtop - x0 < 2 * L) :
    Set.Icc (x0 + L) (xtop - L) = ∅ := by
  rw [Set.Icc_eq_empty_iff]
  intro hle
  linarith

/-- Control: at a zero lottery cost the window is the whole ladder from the bottom income to the
top. -/
theorem control_window_full_at_zero_cost {x0 xtop : ℝ} :
    Set.Icc (x0 + 0) (xtop - 0) = Set.Icc x0 xtop := by
  simp

end Isc
