import Mathlib
import Anonymity

open MeasureTheory ProbabilityTheory Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestAligned

open EntryContestModel EntryContestP1P2 EntryContestP6P7 EntryContestPMU EntryContestPMUWitness
  EntryContestRefute EntryContestKappa EntryContestAnon

/-! **The anonymity boundary with ability rising in wealth.** `Anonymity.lean` exhibits two
equilibria of different sizes when the richest challenger has the weakest ability law. The same
failure occurs when ability is aligned with wealth: the richest challenger draws the largest of
12 uniforms, the next of 10, the poorest of 9, and the costs come from CRRA(2) utility at wealths
`2.15, 2.08, 2.075` with fee 1, so cost rises as wealth falls. At `μ = 0` the richest entering
alone and the two poorer entering together are both equilibria, and by continuity so for every
small `μ > 0`. -/

section Witness

/-- Abilities `12, 10, 9` from the richest to the poorest, so ability rises with wealth. -/
def kkA : Fin 3 → ℕ := ![11, 9, 8]

/-- Wealth `43/20, 52/25, 83/40` (`2.15, 2.08, 2.075`), richest first. -/
noncomputable def wA : Fin 3 → ℝ := ![43 / 20, 52 / 25, 83 / 40]

/-- The cost of entry under CRRA utility with `γ = 2` and fee `c = 1`. -/
noncomputable def κA (j : Fin 3) : ℝ := kappa (crra 2) 1 (wA j)

theorem wA_val : wA 0 = 43 / 20 ∧ wA 1 = 52 / 25 ∧ wA 2 = 83 / 40 := ⟨rfl, rfl, rfl⟩

theorem kkA_val : kkA 0 = 11 ∧ kkA 1 = 9 ∧ kkA 2 = 8 := ⟨rfl, rfl, rfl⟩

theorem κA_values : κA 0 = 400 / 989 ∧ κA 1 = 625 / 1404 ∧ κA 2 = 1600 / 3569 := by
  obtain ⟨h0, h1, h2⟩ := wA_val
  refine ⟨?_, ?_, ?_⟩
  · simp only [κA, kappa, crra_two, h0]
    norm_num
  · simp only [κA, kappa, crra_two, h1]
    norm_num
  · simp only [κA, kappa, crra_two, h2]
    norm_num

/-- The costs at the three wealths, under an ASCII name for the manuscript. -/
theorem kappaA_values : κA 0 = 400 / 989 ∧ κA 1 = 625 / 1404 ∧ κA 2 = 1600 / 3569 := κA_values

/-- Wealth falls and ability falls along the ranking: ability is aligned with wealth. -/
theorem ability_with_wealth : wA 1 < wA 0 ∧ wA 2 < wA 1 ∧ kkA 1 < kkA 0 ∧ kkA 2 < kkA 1 := by
  obtain ⟨h0, h1, h2⟩ := wA_val
  obtain ⟨k0, k1, k2⟩ := kkA_val
  rw [h0, h1, h2, k0, k1, k2]
  norm_num

/-- The cost of entry rises as wealth falls. -/
theorem costs_ordered : κA 0 < κA 1 ∧ κA 1 < κA 2 := by
  obtain ⟨c0, c1, c2⟩ := κA_values
  rw [c0, c1, c2]
  norm_num

/-- The gains at `V = 1` that the two equilibria use: `n_i / (n_i + 1 + Σ_{j ∈ T} n_j)` with
    `n = (12, 10, 9)`. -/
theorem gainsA :
    gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 0 ∅ = 12 / 13
    ∧ gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 0 {1, 2} = 3 / 8
    ∧ gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 1 {0} = 10 / 23
    ∧ gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 1 {2} = 1 / 2
    ∧ gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 2 {0} = 9 / 22
    ∧ gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 2 {1} = 9 / 20 := by
  obtain ⟨k0, k1, k2⟩ := kkA_val
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_⟩
  · rw [gain_zero kkA 1 0 ∅ (by decide), Finset.sum_empty, k0]
    norm_num
  · rw [gain_zero kkA 1 0 {1, 2} (by decide), Finset.sum_pair (by decide), k0, k1, k2]
    norm_num
  · rw [gain_zero kkA 1 1 {0} (by decide), Finset.sum_singleton, k1, k0]
    norm_num
  · rw [gain_zero kkA 1 1 {2} (by decide), Finset.sum_singleton, k1, k2]
    norm_num
  · rw [gain_zero kkA 1 2 {0} (by decide), Finset.sum_singleton, k2, k0]
    norm_num
  · rw [gain_zero kkA 1 2 {1} (by decide), Finset.sum_singleton, k2, k1]
    norm_num

/-- **The anonymity boundary at `μ = 0` with ability rising in wealth, exact.** The richest
    challenger entering alone and the two poorer challengers entering together are both
    equilibria, so equilibria of different sizes coexist. -/
theorem aligned_witness_zero :
    IsEquilibrium 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif κA {0}
      ∧ IsEquilibrium 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif κA {1, 2} := by
  obtain ⟨c0, c1, c2⟩ := κA_values
  obtain ⟨g0, g012, g10, g12, g20, g21⟩ := gainsA
  have e0 : ({0} : Finset (Fin 3)).erase 0 = ∅ := by decide
  have e1 : ({1, 2} : Finset (Fin 3)).erase 1 = {2} := by decide
  have e2 : ({1, 2} : Finset (Fin 3)).erase 2 = {1} := by decide
  refine ⟨⟨fun i hi => ?_, fun i hi => ?_⟩, ⟨fun i hi => ?_, fun i hi => ?_⟩⟩
  · rw [Finset.mem_singleton] at hi
    subst hi
    rw [e0, g0, c0]
    norm_num
  · fin_cases i
    · exact absurd (Finset.mem_singleton_self _) hi
    · show gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 1 {0} < κA 1
      rw [g10, c1]
      norm_num
    · show gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 2 {0} < κA 2
      rw [g20, c2]
      norm_num
  · rw [Finset.mem_insert, Finset.mem_singleton] at hi
    rcases hi with rfl | rfl
    · rw [e1, g12, c1]
      norm_num
    · rw [e2, g21, c2]
      norm_num
  · fin_cases i
    · show gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 0 {1, 2} < κA 0
      rw [g012, c0]
      norm_num
    · exact absurd (Finset.mem_insert_self _ _) hi
    · exact absurd (Finset.mem_insert_of_mem (Finset.mem_singleton_self _)) hi

end Witness

section Positive

/-- **The anonymity boundary inside the primitives, with ability rising in wealth.** For every
    small enough `μ > 0`, the richest challenger entering alone and the two poorer challengers
    entering together are both equilibria. -/
theorem aligned_witness_positive :
    ∀ᶠ μ in 𝓝[>] 0, IsEquilibrium 1 (entμ kkA μ) (outμ μ) (incμ μ) κA {0}
      ∧ IsEquilibrium 1 (entμ kkA μ) (outμ μ) (incμ μ) κA {1, 2} := by
  obtain ⟨c0, c1, c2⟩ := κA_values
  obtain ⟨g0, g012, g10, g12, g20, g21⟩ := gainsA
  have e0 : ({0} : Finset (Fin 3)).erase 0 = ∅ := by decide
  have e1 : ({1, 2} : Finset (Fin 3)).erase 1 = {2} := by decide
  have e2 : ({1, 2} : Finset (Fin 3)).erase 2 = {1} := by decide
  have t1 := (gain_tendsto kkA 1 0 ∅).eventually (eventually_gt_nhds
    (show κA 0 < gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 0 ∅ by rw [g0, c0]; norm_num))
  have t2 := (gain_tendsto kkA 1 1 {0}).eventually (eventually_lt_nhds
    (show gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 1 {0} < κA 1 by
      rw [g10, c1]; norm_num))
  have t3 := (gain_tendsto kkA 1 2 {0}).eventually (eventually_lt_nhds
    (show gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 2 {0} < κA 2 by
      rw [g20, c2]; norm_num))
  have t4 := (gain_tendsto kkA 1 1 {2}).eventually (eventually_gt_nhds
    (show κA 1 < gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 1 {2} by
      rw [g12, c1]; norm_num))
  have t5 := (gain_tendsto kkA 1 2 {1}).eventually (eventually_gt_nhds
    (show κA 2 < gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 2 {1} by
      rw [g21, c2]; norm_num))
  have t6 := (gain_tendsto kkA 1 0 {1, 2}).eventually (eventually_lt_nhds
    (show gain 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif 0 {1, 2} < κA 0 by
      rw [g012, c0]; norm_num))
  filter_upwards [nhdsWithin_le_nhds t1, nhdsWithin_le_nhds t2, nhdsWithin_le_nhds t3,
    nhdsWithin_le_nhds t4, nhdsWithin_le_nhds t5, nhdsWithin_le_nhds t6]
    with μ h1 h2 h3 h4 h5 h6
  refine ⟨⟨fun i hi => ?_, fun i hi => ?_⟩, ⟨fun i hi => ?_, fun i hi => ?_⟩⟩
  · rw [Finset.mem_singleton] at hi
    subst hi
    rw [e0]
    exact h1.le
  · fin_cases i
    · exact absurd (Finset.mem_singleton_self _) hi
    · exact h2
    · exact h3
  · rw [Finset.mem_insert, Finset.mem_singleton] at hi
    rcases hi with rfl | rfl
    · rw [e1]
      exact h4.le
    · rw [e2]
      exact h5.le
  · fin_cases i
    · exact h6
    · exact absurd (Finset.mem_insert_self _ _) hi
    · exact absurd (Finset.mem_insert_of_mem (Finset.mem_singleton_self _)) hi

/-- Some `μ > 0` carries both equilibria, of sizes 1 and 2. -/
theorem aligned_witness_exists :
    ∃ μ : ℝ, 0 < μ ∧ IsEquilibrium 1 (entμ kkA μ) (outμ μ) (incμ μ) κA {0}
      ∧ IsEquilibrium 1 (entμ kkA μ) (outμ μ) (incμ μ) κA {1, 2}
      ∧ ({0} : Finset (Fin 3)).card ≠ ({1, 2} : Finset (Fin 3)).card := by
  obtain ⟨μ, h, hμ⟩ := (aligned_witness_positive.and self_mem_nhdsWithin).exists
  exact ⟨μ, hμ, h.1, h.2, by decide⟩

end Positive

section Controls

/-- The cost vector with the poorest challenger's cost raised to `23/50`, above its gain `9/20`
    in `{1, 2}`; the other two costs are unchanged. -/
noncomputable def κA' : Fin 3 → ℝ := ![κA 0, κA 1, 23 / 50]

theorem κA'_val : κA' 0 = κA 0 ∧ κA' 1 = κA 1 ∧ κA' 2 = 23 / 50 := ⟨rfl, rfl, rfl⟩

/-- **Control: the window of costs is load-bearing.** Raising the poorest challenger's cost
    just above `9/20` breaks the two-entrant equilibrium. -/
theorem control_window :
    ¬ IsEquilibrium 1 (fun j => abil (kkA j)) (Measure.dirac 0) unif κA' {1, 2} := by
  obtain ⟨-, -, c2⟩ := κA'_val
  obtain ⟨-, -, -, -, -, g21⟩ := gainsA
  have e2 : ({1, 2} : Finset (Fin 3)).erase 2 = {1} := by decide
  intro h
  have h2 := h.1 2 (Finset.mem_insert_of_mem (Finset.mem_singleton_self _))
  rw [e2, g21, c2] at h2
  norm_num at h2

/-- Equal abilities `10, 10, 10`. -/
def kkE : Fin 3 → ℕ := ![9, 9, 9]

theorem kkE_val : kkE 0 = 9 ∧ kkE 1 = 9 ∧ kkE 2 = 9 := ⟨rfl, rfl, rfl⟩

/-- **Control: the ability differences are load-bearing.** With equal abilities and the same
    costs, `{0}` and `{1, 2}` are not both equilibria: the second-richest challenger's gain from
    joining `{0}`, `10/21`, exceeds its cost. -/
theorem control_equal_abilities :
    ¬ (IsEquilibrium 1 (fun j => abil (kkE j)) (Measure.dirac 0) unif κA {0}
      ∧ IsEquilibrium 1 (fun j => abil (kkE j)) (Measure.dirac 0) unif κA {1, 2}) := by
  obtain ⟨-, c1, -⟩ := κA_values
  obtain ⟨k0, k1, -⟩ := kkE_val
  intro h
  have h1 := h.1.2 1 (by decide)
  rw [gain_zero kkE 1 1 {0} (by decide), Finset.sum_singleton, k1, k0, c1] at h1
  norm_num at h1

end Controls

end EntryContestAligned
