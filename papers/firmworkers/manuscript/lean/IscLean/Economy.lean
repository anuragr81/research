import IscLean.Mobility
import Mathlib.Analysis.SpecificLimits.Normed
import Mathlib.Tactic

/-!
The large economy, the pool of one, and who pays and receives the lottery cost.

* A pool of fewer than two entrants becomes negligible as the number of workers grows
  (issue 14, option (a), M55, M56).
* With the lottery unavailable to a lone entrant, the chance of an empty pool does not change
  who enters (M57).
* The lottery cost is paid to the members of the entrant's own status group in equal shares, so
  every group's accounts balance and receipts that do not depend on one's own route leave the
  dominance results unchanged (M58, M59).
* The same choice seen by the entrant, by workers taken together and by the firm (M60).
-/

open Filter Topology Finset

namespace Isc

/-! ### A pool of fewer than two entrants -/

/-- Chance that fewer than two of `n` workers enter the lottery, each independently with chance
`q`: the first two terms of the binomial distribution. -/
noncomputable def voidPoolChance (n : ℕ) (q : ℝ) : ℝ := (1 - q) ^ n + n * q * (1 - q) ^ (n - 1)

/-- The chance is the sum of the binomial terms for no entrant and one entrant, and all the
binomial terms sum to one. -/
theorem voidPool_binomial_terms (n : ℕ) (q : ℝ) :
    voidPoolChance n q = ∑ k ∈ range 2, q ^ k * (1 - q) ^ (n - k) * (n.choose k : ℝ) ∧
      ∑ k ∈ range (n + 1), q ^ k * (1 - q) ^ (n - k) * (n.choose k : ℝ) = 1 := by
  constructor
  · simp [voidPoolChance, sum_range_succ]
    ring
  · have h : q + (1 - q) = 1 := by ring
    rw [← add_pow, h, one_pow]

/-- For any chance of entry strictly between zero and one, the chance of a pool of fewer than
two entrants tends to zero as the number of workers grows. -/
theorem voidPool_tendsto_zero {q : ℝ} (h0 : 0 < q) (h1 : q < 1) :
    Tendsto (fun n : ℕ => voidPoolChance n q) atTop (𝓝 0) := by
  have hr0 : 0 ≤ 1 - q := by linarith
  have hr1 : 1 - q < 1 := by linarith
  have hne : (1 - q) ≠ 0 := by linarith
  have hA : Tendsto (fun n : ℕ => (1 - q) ^ n) atTop (𝓝 0) :=
    tendsto_pow_atTop_nhds_zero_of_lt_one hr0 hr1
  have hB : Tendsto (fun n : ℕ => (n : ℝ) * (1 - q) ^ n) atTop (𝓝 0) :=
    tendsto_self_mul_const_pow_of_lt_one hr0 hr1
  have hsum : Tendsto (fun n : ℕ => (1 - q) ^ n + q / (1 - q) * ((n : ℝ) * (1 - q) ^ n))
      atTop (𝓝 0) := by
    simpa using hA.add (hB.const_mul (q / (1 - q)))
  refine hsum.congr' ?_
  filter_upwards [eventually_ge_atTop 1] with n hn
  unfold voidPoolChance
  obtain ⟨m, rfl⟩ : ∃ m, n = m + 1 := ⟨n - 1, by omega⟩
  simp only [Nat.add_sub_cancel, pow_succ]
  field_simp
  ring

/-- Control: in an economy of two workers, each entering with chance one half, a pool of fewer
than two entrants has chance three quarters. -/
theorem control_void_pool_likely_when_small : voidPoolChance 2 (1 / 2) = 3 / 4 := by
  norm_num [voidPoolChance]

/-! ### The rule for a pool of one does not change who enters -/

/-- A displaced worker who enters the lottery and finds no partner takes the route `A` it would
otherwise have chosen; with chance `π < 1` of an empty pool and value `D` of the draw otherwise,
entering is at least as good as `A` exactly when the draw is. -/
theorem void_rule_neutral {π A D : ℝ} (h1 : π < 1) :
    A ≤ π * A + (1 - π) * D ↔ A ≤ D := by
  have hp : 0 < 1 - π := by linarith
  constructor
  · intro h
    by_contra hc
    push_neg at hc
    nlinarith [mul_lt_mul_of_pos_left hc hp]
  · intro h
    nlinarith [mul_le_mul_of_nonneg_left h hp.le]

/-- Control: if a lone entrant received the own former job back at the lottery cost, worth `8`
against a best alternative of `5`, the lottery would be entered at an empty-pool chance of one
half although its draw, `4`, is worse than the alternative. -/
theorem control_cheap_lone_rule_distorts :
    (5 : ℝ) < 1 / 2 * 8 + (1 - 1 / 2) * 4 ∧ (4 : ℝ) < 5 := by
  norm_num

/-! ### Who receives the lottery cost -/

/-- The entrants of a status group pay the lottery cost and its members receive it in equal
shares, so the group's receipts equal its payments, in every pool and under any closure rule. -/
theorem own_group_dues_balance {L : ℝ} {members entrants : ℕ} (hm : 0 < members) :
    (members : ℝ) * (L * entrants / members) = L * entrants := by
  have : (members : ℝ) ≠ 0 := by exact_mod_cast hm.ne'
  field_simp

/-- A receipt `R` that does not depend on one's own route leaves the lottery-over-downgrade
result of M33 unchanged: shift every income by `R` and it still holds, for every nondecreasing
utility. -/
theorem receipt_preserves_dominance {ι : Type*} [DecidableEq ι] {P : Finset ι} (h2 : 2 ≤ P.card)
    (x : ι → ℝ) {L x0 R : ℝ} {u : ℝ → ℝ} {S : Set ℝ} (hu : MonotoneOn u S) (hx0 : x0 + R ∈ S)
    (hS : ∀ k ∈ P, (x k + R) - L ∈ S) (hL : ∀ k ∈ P, x0 ≤ x k - L) :
    ∀ i ∈ P, u (x0 + R) ≤ poolEU P (fun k => x k + R) L u i :=
  displaced_prefer_lottery h2 _ hu hx0 hS (fun k hk => by linarith [hL k hk])

/-- The same receipt leaves every comparison of sure routes unchanged. -/
theorem receipt_preserves_sure_comparison {a b R : ℝ} : a + R ≤ b + R ↔ a ≤ b := by
  constructor <;> intro h <;> linarith

/-! ### One choice, three interests -/

/-- The entrant compares the return, `x - C`, with the lottery valued at its average, `E - L`.
Workers taken together recover the lottery cost, so for them the lottery is worth `E`. Whenever
the entrant prefers the lottery, workers taken together do too. -/
theorem workers_prefer_lottery_whenever_entrant_does {x C E L : ℝ} (hL : 0 ≤ L)
    (h : x - C ≤ E - L) : x - C ≤ E := by
  linarith

/-- Control: an entrant can prefer the return although workers taken together would prefer the
lottery. With former income `10`, upskilling cost `3`, expected prize `8` and lottery cost `2`,
the return is worth `7`, the lottery `6` to the entrant and `8` to workers taken together. -/
theorem control_entrant_returns_where_workers_would_gamble :
    (8 : ℝ) - 2 < 10 - 3 ∧ (10 : ℝ) - 3 < 8 := by
  norm_num

/-- The firm receives the upskilling cost from a returner and nothing from an entrant, so it
prefers the return whenever the upskilling cost is positive, whatever the lottery cost. -/
theorem firm_prefers_return {C : ℝ} (hC : 0 < C) : (0 : ℝ) < C := hC

end Isc
