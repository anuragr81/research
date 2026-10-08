import Mathlib
import Equilibrium

open MeasureTheory ProbabilityTheory Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestInc

open EntryContestModel EntryContestEq

/-! **The incumbent as challenger 0.** The incumbent of the `Q`-challenger game is read as
player `0` of a symmetric game with `N = Q + 1` players: she has the lowest cost, which
`Monotone cost` gives, and the same entry choice. A referee asked whether an equilibrium can
have the incumbent out while some challenger is in. It can, exactly when `k*` lies in a band
(`incumbent_out_iff`); the band is empty when identities are pinned
(`incumbent_in_of_pinned`); and whenever such an equilibrium exists, the assortative set, which
contains the incumbent, is another equilibrium of the same size (`incumbent_out_not_unique`).
The `three_equilibria` numbers witness the band, and raising one cost closes it. -/

section Abstract

variable (cost gain : ℕ → ℝ) (N kstar : ℕ)

/-- **The incumbent-out band.** Under the hypotheses of `count_invariance_fin`, a nonempty
    equilibrium without player `0` exists if and only if `1 ≤ k* < N`, the richest player strictly
    prefers to stay out of a `k*`-entrant set, and the `k*`-th player is weakly willing to be its
    `k*`-th member. -/
theorem incumbent_out_iff (hcost : Monotone cost) (hgain : Antitone gain) (hkN : kstar ≤ N)
    (hprefix : ∀ j, j < kstar → cost j ≤ gain j) (hfail : kstar < N → gain kstar < cost kstar) :
    (∃ S : Finset ℕ, IsEquilibriumR cost gain N S ∧ 0 ∉ S ∧ S.Nonempty)
      ↔ (1 ≤ kstar ∧ kstar < N ∧ gain kstar < cost 0 ∧ cost kstar ≤ gain (kstar - 1)) := by
  constructor
  · rintro ⟨S, hS, h0, hne⟩
    have hcard : S.card = kstar :=
      count_invariance_fin cost gain N kstar hcost hgain hkN hprefix hfail S hS
    obtain ⟨hsub, hmem, hout⟩ := hS
    have h1 : 1 ≤ kstar := by
      have := Finset.card_pos.mpr hne
      omega
    have hN : 1 ≤ N := by
      obtain ⟨i, hi⟩ := hne
      have := Finset.mem_range.mp (hsub hi)
      omega
    have h0N : 0 ∈ Finset.range N := Finset.mem_range.mpr hN
    have hg : gain kstar < cost 0 := by
      have := hout 0 h0N h0
      rwa [hcard] at this
    have hsub' : S ⊆ (Finset.range N).erase 0 := fun i hi =>
      Finset.mem_erase.mpr ⟨fun h => h0 (by rw [← h]; exact hi), hsub hi⟩
    have hkN' : kstar < N := by
      have := Finset.card_le_card hsub'
      rw [Finset.card_erase_of_mem h0N, Finset.card_range, hcard] at this
      omega
    have hex : ∃ i ∈ S, kstar ≤ i := by
      by_contra h
      push_neg at h
      have hsub'' : S ⊆ Finset.Ico 1 kstar := by
        intro i hi
        have hi0 : i ≠ 0 := fun h' => h0 (by rw [← h']; exact hi)
        exact Finset.mem_Ico.mpr ⟨by omega, h i hi⟩
      have := Finset.card_le_card hsub''
      rw [Nat.card_Ico, hcard] at this
      omega
    obtain ⟨i, hiS, hi⟩ := hex
    refine ⟨h1, hkN', hg, ?_⟩
    have := hmem i hiS
    rw [hcard] at this
    exact le_trans (hcost hi) this
  · rintro ⟨h1, hkN', hg, hc⟩
    refine ⟨Finset.Icc 1 kstar, ⟨?_, ?_, ?_⟩, ?_, ?_⟩
    · intro i hi
      have := Finset.mem_Icc.mp hi
      exact Finset.mem_range.mpr (by omega)
    · intro i hi
      rw [Nat.card_Icc, show kstar + 1 - 1 - 1 = kstar - 1 by omega]
      exact le_trans (hcost (Finset.mem_Icc.mp hi).2) hc
    · intro j _ _
      rw [Nat.card_Icc, show kstar + 1 - 1 = kstar by omega]
      exact lt_of_lt_of_le hg (hcost (Nat.zero_le j))
    · intro h
      have := (Finset.mem_Icc.mp h).1
      omega
    · exact ⟨1, Finset.mem_Icc.mpr ⟨le_refl 1, h1⟩⟩

/-- **When identities are pinned, the incumbent enters.** If the `k*`-th player is not willing
    to be the `k*`-th member, every nonempty equilibrium contains player `0`. -/
theorem incumbent_in_of_pinned (hcost : Monotone cost) (hgain : Antitone gain) (hkN : kstar ≤ N)
    (hprefix : ∀ j, j < kstar → cost j ≤ gain j) (hfail : kstar < N → gain kstar < cost kstar)
    (hpin : gain (kstar - 1) < cost kstar) (S : Finset ℕ) (hS : IsEquilibriumR cost gain N S)
    (hne : S.Nonempty) : 0 ∈ S := by
  by_contra h0
  have h := (incumbent_out_iff cost gain N kstar hcost hgain hkN hprefix hfail).mp ⟨S, hS, h0, hne⟩
  linarith [h.2.2.2]

/-- **An incumbent-out equilibrium is never the only one.** Whenever one exists, the assortative
    set `range k*`, which contains player `0`, is an equilibrium of the same size. -/
theorem incumbent_out_not_unique (hcost : Monotone cost) (hgain : Antitone gain) (hkN : kstar ≤ N)
    (hprefix : ∀ j, j < kstar → cost j ≤ gain j) (hfail : kstar < N → gain kstar < cost kstar) :
    (∃ S : Finset ℕ, IsEquilibriumR cost gain N S ∧ 0 ∉ S ∧ S.Nonempty)
      → ∃ T : Finset ℕ, IsEquilibriumR cost gain N T ∧ 0 ∈ T ∧ T.card = kstar := by
  intro h
  obtain ⟨h1, -, -, -⟩ := (incumbent_out_iff cost gain N kstar hcost hgain hkN hprefix hfail).mp h
  exact ⟨Finset.range kstar, assortative_is_equilibrium cost gain N kstar hcost hkN hprefix hfail,
    Finset.mem_range.mpr (by omega), Finset.card_range kstar⟩

end Abstract

section Witness

/-! The numbers of `three_equilibria`: costs `(2, 5, 8)`, gains `(12, 10, 1)`, three players,
`k* = 2`. Player `0` is the incumbent. -/

/-- The costs `(2, 5, 8)`. -/
noncomputable def cost3 : ℕ → ℝ := fun i => if i = 0 then 2 else if i = 1 then 5 else 8

/-- The gains `(12, 10, 1)`. -/
noncomputable def gain3 : ℕ → ℝ := fun m => if m = 0 then 12 else if m = 1 then 10 else 1

/-- The control: player `2`'s cost raised from `8` to `11`, above `gain 1 = 10`. -/
noncomputable def cost3' : ℕ → ℝ := fun i => if i = 0 then 2 else if i = 1 then 5 else 11

theorem cost3_mono : Monotone cost3 := by
  intro a b hab
  unfold cost3
  split_ifs <;> norm_num <;> omega

theorem cost3'_mono : Monotone cost3' := by
  intro a b hab
  unfold cost3'
  split_ifs <;> norm_num <;> omega

theorem gain3_anti : Antitone gain3 := by
  intro a b hab
  unfold gain3
  split_ifs <;> norm_num <;> omega

theorem cost3_prefix : ∀ j, j < 2 → cost3 j ≤ gain3 j := by
  intro j hj
  interval_cases j <;> norm_num [cost3, gain3]

theorem cost3'_prefix : ∀ j, j < 2 → cost3' j ≤ gain3 j := by
  intro j hj
  interval_cases j <;> norm_num [cost3', gain3]

/-- **The band is witnessed.** `{1, 2}` is an equilibrium without the incumbent, and the band
    condition of `incumbent_out_iff` holds: `1 ≤ 2 < 3`, `gain 2 = 1 < 2 = cost 0`, and
    `cost 2 = 8 ≤ 10 = gain 1`. -/
theorem band_witness :
    IsEquilibriumR cost3 gain3 3 {1, 2}
      ∧ (1 ≤ 2 ∧ 2 < 3 ∧ gain3 2 < cost3 0 ∧ cost3 2 ≤ gain3 (2 - 1)) :=
  ⟨three_equilibria.2.2, by norm_num [cost3, gain3]⟩

/-- **The control.** With `cost 2 = 11 > 10 = gain 1`, still `k* = 2` (`2 ≤ 12`, `5 ≤ 10`,
    `1 < 11`), but no nonempty equilibrium avoids the incumbent. -/
theorem control_no_band :
    ¬ ∃ S : Finset ℕ, IsEquilibriumR cost3' gain3 3 S ∧ 0 ∉ S ∧ S.Nonempty := by
  intro h
  have hband := (incumbent_out_iff cost3' gain3 3 2 cost3'_mono gain3_anti (by norm_num)
    cost3'_prefix (fun _ => by norm_num [cost3', gain3])).mp h
  have h4 := hband.2.2.2
  norm_num [cost3', gain3] at h4

/-- In the control every nonempty equilibrium contains the incumbent. -/
theorem control_incumbent_in (S : Finset ℕ) (hS : IsEquilibriumR cost3' gain3 3 S)
    (hne : S.Nonempty) : 0 ∈ S :=
  incumbent_in_of_pinned cost3' gain3 3 2 cost3'_mono gain3_anti (by norm_num) cost3'_prefix
    (fun _ => by norm_num [cost3', gain3]) (by norm_num [cost3', gain3]) S hS hne

end Witness

section Model

/-! **The gains of the `(Q + 1)`-player symmetric game.** Entrants draw from `α`, non-entrants
from `β`. The `Q`-challenger gain with an outsider incumbent, `C = β`, is the symmetric gain of
a player facing `m` other entrants among `Q` rivals; the `Q`-challenger gain with the incumbent
in, `C = α`, is that gain at `m + 1`. -/

variable (V : ℝ) (α β : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]

/-- The symmetric gain of a player facing `m` entrants among `Q` rivals. -/
noncomputable def symGain (Q m : ℕ) : ℝ := Delta V α β β Q m

/-- **The incumbent in is one more entrant.** The challenger gain with the incumbent in and `m`
    other challengers in equals the challenger gain with the incumbent out and `m + 1` other
    challengers in. -/
theorem Delta_incumbent_in_eq (Q m : ℕ) (hm : m + 2 ≤ Q) :
    Delta V α β α Q m = Delta V α β β Q (m + 1) := by
  have h : ∀ x, cdf (rivals α α β m (Q - 1 - m)) x
      = cdf (rivals β α β (m + 1) (Q - 1 - (m + 1))) x := by
    intro x
    rw [cdf_rivals, cdf_rivals, show Q - 1 - m = (Q - 1 - (m + 1)) + 1 by omega, pow_succ,
      pow_succ]
    ring
  unfold Delta
  simp_rw [h]

/-- The same identity in terms of the symmetric gain. -/
theorem challenger_gain_incumbent_in (Q m : ℕ) (hm : m + 2 ≤ Q) :
    Delta V α β α Q m = symGain V α β Q (m + 1) :=
  Delta_incumbent_in_eq V α β Q m hm

/-- The symmetric gain falls weakly with each further entrant, from M4. -/
theorem symGain_step [NoAtoms α] [NoAtoms β] (hV : 0 ≤ V) (Q m : ℕ) (hm : m + 2 ≤ Q) :
    symGain V α β Q (m + 1) ≤ symGain V α β Q m :=
  Delta_step_nonpos V α β β hV Q m hm

/-- The symmetric gain is antitone on the counts `0, …, Q − 1` that the model covers. -/
theorem symGain_antitoneOn [NoAtoms α] [NoAtoms β] (hV : 0 ≤ V) (Q : ℕ) :
    AntitoneOn (symGain V α β Q) (Set.Iic (Q - 1)) := by
  intro a ha b hb hab
  have hmono : Antitone (DeltaUpTo V α β β Q) :=
    antitone_nat_of_succ_le (DeltaUpTo_step V α β β hV Q)
  have h := hmono hab
  unfold DeltaUpTo at h
  rw [min_eq_left (Set.mem_Iic.mp ha), min_eq_left (Set.mem_Iic.mp hb)] at h
  exact h

end Model

end EntryContestInc
