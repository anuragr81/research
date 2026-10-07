import Mathlib
import MeasurementMap
import Anonymity

open MeasureTheory ProbabilityTheory Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestEq

open EntryContestModel EntryContestP1P2 EntryContestP6P7 EntryContestPMU EntryContestAnon
  EntryContestMM

/-! **Equilibrium structure, closed in Lean.** The strict part of the monotonicity corollary,
count invariance for `Q` challengers, and the three parts of the assortative-selection
proposition N4, which `PROOFS.tex` had partly argued in prose and partly checked by sampling. -/

section Strict

variable (V : ℝ) (α β C : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]
  [IsProbabilityMeasure C] [NoAtoms α] [NoAtoms β] [NoAtoms C]

/-- **Corollary (monotonicity), strict part.** The gain falls strictly from `m` to `m + 1`
    whenever `∫ φ² dK > 0`. -/
theorem Delta_step_strict (hV : 0 < V) (Q m : ℕ) (hm : m + 2 ≤ Q)
    (hpos : 0 < ∫ x, (cdf α x - cdf β x) ^ 2 ∂(rivals C α β m (Q - 2 - m))) :
    Delta V α β C Q (m + 1) < Delta V α β C Q m := by
  have h := Delta_step V α β C Q m hm
  have : 0 < V / 2 * ∫ x, (cdf α x - cdf β x) ^ 2 ∂(rivals C α β m (Q - 2 - m)) :=
    mul_pos (by linarith) hpos
  linarith

end Strict

section Selection

/-! Challengers are indexed `0, …, Q − 1` in increasing order of cost, which is decreasing order
of wealth under burden-monotonicity. `k*` is the length of the prefix on which the entry
condition holds, with the next rank failing it. -/

variable (cost gain : ℕ → ℝ) (Q kstar : ℕ)

/-- A pure-strategy equilibrium entrant set among `Q` challengers. A member compares its cost
    with the gain at `|S| − 1` other entrants and an outsider with the gain at `|S|`. -/
def IsEquilibriumR (S : Finset ℕ) : Prop :=
  S ⊆ Finset.range Q ∧ (∀ i ∈ S, cost i ≤ gain (S.card - 1))
    ∧ (∀ j ∈ Finset.range Q, j ∉ S → gain S.card < cost j)

/-- **Count invariance for `Q` challengers.** Every pure-strategy equilibrium has size `k*`. -/
theorem count_invariance_fin (hcost : Monotone cost) (hgain : Antitone gain) (hkQ : kstar ≤ Q)
    (hprefix : ∀ j, j < kstar → cost j ≤ gain j) (hfail : kstar < Q → gain kstar < cost kstar)
    (S : Finset ℕ) (hS : IsEquilibriumR cost gain Q S) : S.card = kstar := by
  obtain ⟨hsub, hmem, hout⟩ := hS
  have hcardQ : S.card ≤ Q := by
    have := Finset.card_le_card hsub
    rwa [Finset.card_range] at this
  apply le_antisymm
  · by_contra hlt
    push_neg at hlt
    obtain ⟨i, hiS, hi⟩ := exists_mem_ge S (by omega)
    have h1 : cost (S.card - 1) ≤ gain (S.card - 1) := le_trans (hcost hi) (hmem i hiS)
    have h2 : gain kstar < cost kstar := hfail (by omega)
    have h3 : cost kstar ≤ cost (S.card - 1) := hcost (by omega)
    have h4 : gain (S.card - 1) ≤ gain kstar := hgain (by omega)
    linarith
  · by_contra hlt
    push_neg at hlt
    obtain ⟨j, hj, hjS⟩ := exists_not_mem_le S
    have hjQ : j ∈ Finset.range Q := Finset.mem_range.mpr (by omega)
    have h1 := hout j hjQ hjS
    have h2 : cost j ≤ cost S.card := hcost hj
    have h3 := hprefix S.card hlt
    linarith

/-- **N4 (i).** The assortative set, the `k*` cheapest challengers, is an equilibrium. -/
theorem assortative_is_equilibrium (hcost : Monotone cost) (hkQ : kstar ≤ Q)
    (hprefix : ∀ j, j < kstar → cost j ≤ gain j) (hfail : kstar < Q → gain kstar < cost kstar) :
    IsEquilibriumR cost gain Q (Finset.range kstar) := by
  refine ⟨fun x hx => Finset.mem_range.mpr (lt_of_lt_of_le (Finset.mem_range.mp hx) hkQ),
    fun i hi => ?_, fun j hj hjS => ?_⟩
  · rw [Finset.card_range]
    have hi' := Finset.mem_range.mp hi
    exact le_trans (hcost (by omega : i ≤ kstar - 1)) (hprefix (kstar - 1) (by omega))
  · rw [Finset.card_range]
    have hj' := Finset.mem_range.mp hj
    have hk : kstar ≤ j := by
      by_contra h
      exact hjS (Finset.mem_range.mpr (by omega))
    exact lt_of_lt_of_le (hfail (by omega)) (hcost hk)

/-- **When identities are pinned.** An equilibrium of size `k*` whose members all lie below `k*`
    is the assortative set. -/
theorem assortative_unique (S : Finset ℕ) (hsub : ∀ i ∈ S, i < kstar) (hcard : S.card = kstar) :
    S = Finset.range kstar :=
  Finset.eq_of_subset_of_card_le (fun i hi => Finset.mem_range.mpr (hsub i hi))
    (by rw [Finset.card_range, hcard])

/-- **N4 (ii).** No set of `k` challengers costs less in total than the `k` cheapest. -/
theorem assortative_min_cost (hcost : Monotone cost) (S : Finset ℕ) :
    ∑ i ∈ Finset.range S.card, cost i ≤ ∑ i ∈ S, cost i := by
  induction S using Finset.induction_on_max with
  | h0 => simp
  | step a T hlt ih =>
      have haT : a ∉ T := fun h => lt_irrefl a (hlt a h)
      have hcard : T.card ≤ a := by
        have hsub : T ⊆ Finset.range a := fun x hx => Finset.mem_range.mpr (hlt x hx)
        have := Finset.card_le_card hsub
        rwa [Finset.card_range] at this
      rw [Finset.card_insert_of_notMem haT, Finset.sum_range_succ, Finset.sum_insert haT]
      linarith [hcost hcard]


/-- **Identities are not pinned without M7's condition.** With costs `(2, 5, 8)` and gains
    `(12, 10, 1)`, the sets `{0, 1}`, `{0, 2}` and `{1, 2}` are all equilibria, the last without
    the richest challenger. -/
theorem three_equilibria :
    IsEquilibriumR (fun i => if i = 0 then 2 else if i = 1 then 5 else 8)
        (fun m => if m = 0 then 12 else if m = 1 then 10 else 1) 3 {0, 1}
      ∧ IsEquilibriumR (fun i => if i = 0 then 2 else if i = 1 then 5 else 8)
        (fun m => if m = 0 then 12 else if m = 1 then 10 else 1) 3 {0, 2}
      ∧ IsEquilibriumR (fun i => if i = 0 then 2 else if i = 1 then 5 else 8)
        (fun m => if m = 0 then 12 else if m = 1 then 10 else 1) 3 {1, 2} := by
  refine ⟨⟨?_, ?_, ?_⟩, ⟨?_, ?_, ?_⟩, ⟨?_, ?_, ?_⟩⟩
  all_goals first
    | (intro x hx; simp only [Finset.mem_insert, Finset.mem_singleton] at hx;
       simp only [Finset.mem_range]; omega)
    | (intro i hi; simp only [Finset.mem_insert, Finset.mem_singleton] at hi;
       rcases hi with rfl | rfl <;> norm_num)
    | (intro j hj hjS; simp only [Finset.mem_range] at hj;
       simp only [Finset.mem_insert, Finset.mem_singleton, not_or] at hjS;
       interval_cases j <;> simp_all)

end Selection

section ComparativeStatics

/-! **P8.** `k*` rises with lower costs and higher gains, so it is non-decreasing in the prize
and non-increasing in the fee. -/

/-- `k*` does not fall when every cost weakly falls and every gain weakly rises. -/
theorem kstar_mono (cost gain cost' gain' : ℕ → ℝ) (Q k k' : ℕ) (hkQ : k ≤ Q)
    (hc : ∀ j, cost' j ≤ cost j) (hg : ∀ j, gain j ≤ gain' j)
    (hprefix : ∀ j, j < k → cost j ≤ gain j) (hfail' : k' < Q → gain' k' < cost' k') :
    k ≤ k' := by
  by_contra h
  push_neg at h
  have h1 := hfail' (by omega)
  have h2 := hprefix k' h
  linarith [hc k', hg k']

/-- The gain is non-decreasing in the prize when it is nonnegative at a unit prize. -/
theorem Delta_mono_prize (V V' : ℝ) (hVV : V ≤ V') (α β C : Measure ℝ) (Q m : ℕ)
    (h1 : 0 ≤ Delta 1 α β C Q m) : Delta V α β C Q m ≤ Delta V' α β C Q m := by
  have e : ∀ W, Delta W α β C Q m = W * Delta 1 α β C Q m := fun W => by
    rw [← Delta_scale W 1, mul_one]
  rw [e V, e V']
  exact mul_le_mul_of_nonneg_right hVV h1

/-- The cost of entry is non-decreasing in the fee when `u` is non-decreasing. -/
theorem kappa_mono_fee (u : ℝ → ℝ) (hu : Monotone u) (w c c' : ℝ) (h : c ≤ c') :
    EntryContestKappa.kappa u c w ≤ EntryContestKappa.kappa u c' w := by
  unfold EntryContestKappa.kappa
  linarith [hu (show w - c' ≤ w - c by linarith)]

end ComparativeStatics

section Payoffs

/-! **N4 (iii).** With identical score laws, the sum of expected payoffs depends on the entrant
set only through its size and its total cost. Challenger `i` has expected payoff
`u(w_i) − κ_i e_i + V P_i`, by MM9. -/

variable {Q : ℕ} (α out C : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure out]
  [IsProbabilityMeasure C]

theorem prod_profile_anonymous (S : Finset (Fin Q)) (i : Fin Q) (x : ℝ) :
    ∏ j ∈ Finset.univ.erase i, cdf (profile (fun _ => α) out S j) x
      = cdf α x ^ (S.erase i).card * cdf out x ^ (Q - 1 - (S.erase i).card) := by
  have hfilt : (Finset.univ.erase i).filter (fun j => j ∈ S) = S.erase i := by
    ext j
    simp only [Finset.mem_filter, Finset.mem_erase, Finset.mem_univ, and_true]
  have hcard : ((Finset.univ.erase i).filter (fun j => ¬ j ∈ S)).card
      = Q - 1 - (S.erase i).card := by
    have h1 := Finset.filter_card_add_filter_neg_card_eq_card (s := Finset.univ.erase i)
      (fun j => j ∈ S)
    rw [hfilt, Finset.card_erase_of_mem (Finset.mem_univ i), Finset.card_univ,
      Fintype.card_fin] at h1
    omega
  have e : ∏ j ∈ Finset.univ.erase i, cdf (profile (fun _ => α) out S j) x
      = ∏ j ∈ Finset.univ.erase i, (if j ∈ S then cdf α x else cdf out x) :=
    Finset.prod_congr rfl (fun j _ => cdf_profile _ _ S j x)
  rw [e, Finset.prod_ite, Finset.prod_const, Finset.prod_const, hfilt, hcard]

/-- The win probability of an entrant and of an outsider, when every entrant draws from `α`. -/
noncomputable def winIn (k : ℕ) : ℝ :=
  ∫ x, cdf C x * (cdf α x ^ (k - 1) * cdf out x ^ (Q - 1 - (k - 1))) ∂α

noncomputable def winOut (k : ℕ) : ℝ :=
  ∫ x, cdf C x * (cdf α x ^ k * cdf out x ^ (Q - 1 - k)) ∂out

/-- **The prize term depends on the count alone.** The challengers' win probabilities sum to
    `|S| · winIn(|S|) + (Q − |S|) · winOut(|S|)`. -/
theorem prize_term_anonymous (S : Finset (Fin Q)) :
    ∑ i, winProb (profile (fun _ => α) out S) C i
      = S.card * winIn (Q := Q) α out C S.card + (Q - S.card) * winOut (Q := Q) α out C S.card := by
  have hterm : ∀ i, winProb (profile (fun _ => α) out S) C i
      = if i ∈ S then winIn (Q := Q) α out C S.card else winOut (Q := Q) α out C S.card := by
    intro i
    unfold winProb
    simp_rw [prod_profile_anonymous α out S i]
    by_cases hi : i ∈ S
    · rw [if_pos hi, profile_mem _ _ _ i hi, Finset.card_erase_of_mem hi]
      rfl
    · rw [if_neg hi, profile_not_mem _ _ _ i hi, Finset.erase_eq_of_notMem hi]
      rfl
  simp_rw [hterm]
  rw [Finset.sum_ite, Finset.sum_const, Finset.sum_const, nsmul_eq_mul, nsmul_eq_mul]
  have h1 : (Finset.univ.filter (fun i => i ∈ S)) = S := by
    ext i
    simp
  have h2 : (Finset.univ.filter (fun i => ¬ i ∈ S)).card = Q - S.card := by
    have := Finset.filter_card_add_filter_neg_card_eq_card (s := (Finset.univ : Finset (Fin Q)))
      (fun i => i ∈ S)
    rw [h1, Finset.card_univ, Fintype.card_fin] at this
    omega
  rw [h1, h2]
  have hle : S.card ≤ Q := by
    have := Finset.card_le_univ S
    rwa [Fintype.card_fin] at this
  push_cast [hle]
  ring

/-- The sum of the challengers' expected payoffs under the entrant set `S`. -/
noncomputable def payoffSum (V : ℝ) (u κ : Fin Q → ℝ) (S : Finset (Fin Q)) : ℝ :=
  ∑ i, (u i - (if i ∈ S then κ i else 0) + V * winProb (profile (fun _ => α) out S) C i)

/-- **N4 (iii).** For two entrant sets of the same size, the sums of expected payoffs differ
    exactly by the difference of their total costs. -/
theorem payoff_gap_anonymous (V : ℝ) (u κ : Fin Q → ℝ) (S T : Finset (Fin Q))
    (hST : S.card = T.card) :
    payoffSum α out C V u κ S - payoffSum α out C V u κ T = ∑ i ∈ T, κ i - ∑ i ∈ S, κ i := by
  have e : ∀ R : Finset (Fin Q), payoffSum α out C V u κ R
      = (∑ i, u i) - (∑ i ∈ R, κ i) + V * ∑ i, winProb (profile (fun _ => α) out R) C i := by
    intro R
    unfold payoffSum
    rw [Finset.sum_add_distrib, Finset.sum_sub_distrib, ← Finset.mul_sum, Finset.sum_ite,
      Finset.sum_const_zero, add_zero]
    congr 2
    exact Finset.sum_congr (by ext i; simp) (fun _ _ => rfl)
  rw [e S, e T, prize_term_anonymous α out C S, prize_term_anonymous α out C T, hST]
  ring

end Payoffs

end EntryContestEq
