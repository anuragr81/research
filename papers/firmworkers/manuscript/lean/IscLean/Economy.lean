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

/-! ### Who receives the lottery cost: the job holders above the entrant (M58) -/

section Dues

variable {ι : Type*}

/-- Job holders strictly above an entrant's former rank. -/
def above (H : Finset ι) (r : ι → ℕ) (i : ι) : Finset ι := H.filter (fun k => r i < r k)

/-- What job holder `h` receives: from each entrant with holders above, an equal share of `L`
among those holders; from an entrant with no holder above, an equal share among all holders. -/
noncomputable def dues (P H : Finset ι) (r : ι → ℕ) (L : ℝ) (h : ι) : ℝ :=
  ∑ i ∈ P, (if r i < r h then L / ((above H r i).card : ℝ)
            else if (above H r i).card = 0 then L / (H.card : ℝ) else 0)

/-- Each entrant's lottery cost is received in full by the job holders. -/
theorem dues_of_entrant (H : Finset ι) (r : ι → ℕ) (L : ℝ) (hH : H.Nonempty) (i : ι) :
    ∑ h ∈ H, (if r i < r h then L / ((above H r i).card : ℝ)
            else if (above H r i).card = 0 then L / (H.card : ℝ) else 0) = L := by
  by_cases hn : (above H r i).card = 0
  · have hnone : ∀ h ∈ H, ¬ r i < r h := by
      intro h hh hlt
      have : h ∈ above H r i := mem_filter.mpr ⟨hh, hlt⟩
      rw [card_eq_zero] at hn
      simp [hn] at this
    rw [sum_congr rfl (fun h hh => by rw [if_neg (hnone h hh), if_pos hn])]
    rw [sum_const, nsmul_eq_mul]
    have : (H.card : ℝ) ≠ 0 := by exact_mod_cast (card_pos.mpr hH).ne'
    field_simp
  · rw [sum_congr rfl (fun h _ => by rw [if_neg hn])]
    rw [← sum_filter]
    show ∑ h ∈ above H r i, L / ((above H r i).card : ℝ) = L
    rw [sum_const, nsmul_eq_mul]
    have : ((above H r i).card : ℝ) ≠ 0 := by exact_mod_cast hn
    field_simp

/-- No leak: the job holders receive in total the number of entrants times the lottery cost. -/
theorem dues_total (P H : Finset ι) (r : ι → ℕ) (L : ℝ) (hH : H.Nonempty) :
    ∑ h ∈ H, dues P H r L h = L * P.card := by
  unfold dues
  rw [sum_comm, sum_congr rfl (fun i _ => dues_of_entrant H r L hH i), sum_const, nsmul_eq_mul]
  ring

/-- Receipts rise with rank: a job holder of higher rank receives at least as much. -/
theorem dues_mono (P H : Finset ι) (r : ι → ℕ) {L : ℝ} (hL : 0 ≤ L) {h h' : ι} (hh' : h' ∈ H)
    (hr : r h ≤ r h') : dues P H r L h ≤ dues P H r L h' := by
  unfold dues
  apply sum_le_sum
  intro i _
  by_cases h1 : r i < r h
  · rw [if_pos h1, if_pos (lt_of_lt_of_le h1 hr)]
  · rw [if_neg h1]
    by_cases h2 : r i < r h'
    · rw [if_pos h2]
      have hpos : (above H r i).card ≠ 0 := by
        intro hc
        rw [card_eq_zero] at hc
        have : h' ∈ above H r i := mem_filter.mpr ⟨hh', h2⟩
        simp [hc] at this
      rw [if_neg hpos]
      positivity
    · rw [if_neg h2]

/-- A job holder with no entrant below receives nothing, when every entrant has a holder
above. -/
theorem dues_bottom_zero (P H : Finset ι) (r : ι → ℕ) (L : ℝ) {h : ι}
    (hlow : ∀ i ∈ P, r h ≤ r i) (hab : ∀ i ∈ P, (above H r i).card ≠ 0) :
    dues P H r L h = 0 := by
  unfold dues
  apply sum_eq_zero
  intro i hi
  rw [if_neg (not_lt.mpr (hlow i hi)), if_neg (hab i hi)]

/-- Control: four ranks, the entrant from the bottom rank, three job holders above. Each holder
receives a third of the lottery cost, and the holder of the top rank receives the same as the
holder just above the entrant. -/
theorem control_dues_example (L : ℝ) :
    dues ({0} : Finset (Fin 4)) {1, 2, 3} (fun k => k.val) L 1 = L / 3 ∧
      dues ({0} : Finset (Fin 4)) {1, 2, 3} (fun k => k.val) L 3 = L / 3 := by
  constructor <;> simp [dues, above, Finset.filter_insert, Finset.filter_singleton]

/-- Control: the fallback. An entrant from the top rank has no holder above, so its lottery
cost goes to all three holders equally. -/
theorem control_dues_top_entrant_fallback (L : ℝ) :
    dues ({3} : Finset (Fin 4)) {0, 1, 2} (fun k => k.val) L 0 = L / 3 := by
  simp [dues, above, Finset.filter_insert, Finset.filter_singleton]

end Dues

end Isc
