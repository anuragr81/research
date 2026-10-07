import Mathlib
import Refutations
import KappaSpread

open MeasureTheory ProbabilityTheory Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestAnon

open EntryContestModel EntryContestP1P2 EntryContestP6P7 EntryContestPMU EntryContestPMUWitness
  EntryContestRefute EntryContestKappa

/-! **The anonymity boundary.** Challengers may draw from different laws. The probability that
challenger `i` wins is the integral, against `i`'s law, of the product of the incumbent's CDF and
every other challenger's CDF, which is the formula `Delta` uses for identical laws. -/

section Framework

variable {Q : ℕ}

/-- The probability that challenger `i` wins, when challenger `j` draws from `L j` and the
    incumbent from `C`. -/
noncomputable def winProb (L : Fin Q → Measure ℝ) (C : Measure ℝ) (i : Fin Q) : ℝ :=
  ∫ x, cdf C x * ∏ j ∈ Finset.univ.erase i, cdf (L j) x ∂(L i)

/-- Entrants draw from `ent`, non-entrants from `out`. -/
noncomputable def profile (ent : Fin Q → Measure ℝ) (out : Measure ℝ) (S : Finset (Fin Q)) :
    Fin Q → Measure ℝ := fun j => if j ∈ S then ent j else out

/-- The gain to challenger `i` from entering when the other entrants are `T`. -/
noncomputable def gain (V : ℝ) (ent : Fin Q → Measure ℝ) (out C : Measure ℝ) (i : Fin Q)
    (T : Finset (Fin Q)) : ℝ :=
  V * (winProb (profile ent out (insert i T)) C i - winProb (profile ent out T) C i)

/-- A pure-strategy equilibrium entrant set. -/
def IsEquilibrium (V : ℝ) (ent : Fin Q → Measure ℝ) (out C : Measure ℝ) (κ : Fin Q → ℝ)
    (S : Finset (Fin Q)) : Prop :=
  (∀ i ∈ S, κ i ≤ gain V ent out C i (S.erase i)) ∧ (∀ i ∉ S, gain V ent out C i S < κ i)

theorem profile_mem (ent : Fin Q → Measure ℝ) (out : Measure ℝ) (S : Finset (Fin Q)) (i : Fin Q)
    (hi : i ∈ S) : profile ent out S i = ent i := by
  unfold profile
  rw [if_pos hi]

theorem profile_not_mem (ent : Fin Q → Measure ℝ) (out : Measure ℝ) (S : Finset (Fin Q))
    (i : Fin Q) (hi : i ∉ S) : profile ent out S i = out := by
  unfold profile
  rw [if_neg hi]

theorem cdf_profile (ent : Fin Q → Measure ℝ) (out : Measure ℝ) (S : Finset (Fin Q)) (j : Fin Q)
    (x : ℝ) : cdf (profile ent out S j) x = if j ∈ S then cdf (ent j) x else cdf out x := by
  unfold profile
  split_ifs <;> rfl

/-- **Proposition (anonymity), formally.** When every entrant draws from the same law, the gain
    depends on the other entrants only through their number, and equals `Δ(|T|)`. -/
theorem gain_anonymous (V : ℝ) (α out C : Measure ℝ) [IsProbabilityMeasure α]
    [IsProbabilityMeasure out] [IsProbabilityMeasure C] (i : Fin Q) (T : Finset (Fin Q))
    (hi : i ∉ T) :
    gain V (fun _ => α) out C i T = Delta V α out C Q T.card := by
  have hfilt : (Finset.univ.erase i).filter (fun j => j ∈ T) = T := by
    ext j
    simp only [Finset.mem_filter, Finset.mem_erase, Finset.mem_univ, and_true]
    constructor
    · exact fun h => h.2
    · intro h
      exact ⟨fun hj => hi (hj ▸ h), h⟩
  have hcard : ((Finset.univ.erase i).filter (fun j => ¬ j ∈ T)).card = Q - 1 - T.card := by
    have h1 := Finset.filter_card_add_filter_neg_card_eq_card (s := Finset.univ.erase i)
      (fun j => j ∈ T)
    rw [hfilt, Finset.card_erase_of_mem (Finset.mem_univ i), Finset.card_univ,
      Fintype.card_fin] at h1
    omega
  have hprod : ∀ (S : Finset (Fin Q)), (∀ j, j ≠ i → (j ∈ S ↔ j ∈ T)) → ∀ x,
      ∏ j ∈ Finset.univ.erase i, cdf (profile (fun _ => α) out S j) x
        = cdf α x ^ T.card * cdf out x ^ (Q - 1 - T.card) := by
    intro S hS x
    have e : ∏ j ∈ Finset.univ.erase i, cdf (profile (fun _ => α) out S j) x
        = ∏ j ∈ Finset.univ.erase i, (if j ∈ T then cdf α x else cdf out x) := by
      refine Finset.prod_congr rfl (fun j hj => ?_)
      rw [cdf_profile]
      have hji : j ≠ i := Finset.ne_of_mem_erase hj
      by_cases hjT : j ∈ T
      · rw [if_pos ((hS j hji).mpr hjT), if_pos hjT]
      · rw [if_neg (fun h => hjT ((hS j hji).mp h)), if_neg hjT]
    rw [e, Finset.prod_ite, Finset.prod_const, Finset.prod_const, hfilt, hcard]
  have hins : ∀ j, j ≠ i → (j ∈ insert i T ↔ j ∈ T) := by
    intro j hj
    rw [Finset.mem_insert]
    constructor
    · rintro (h | h)
      · exact absurd h hj
      · exact h
    · exact fun h => Or.inr h
  unfold gain winProb Delta
  rw [profile_mem _ _ _ i (Finset.mem_insert_self i T), profile_not_mem _ _ _ i hi]
  simp_rw [hprod (insert i T) hins, hprod T (fun j _ => Iff.rfl), cdf_rivals, mul_assoc]

end Framework

section Zero

/-- A challenger of ability `k + 1` scores the largest of `k + 1` uniform draws, with CDF
    `u^(k+1)` on `[0, 1]`. -/
noncomputable abbrev abil (k : ℕ) : Measure ℝ := powLaw unif unif k

theorem cdf_abil (k : ℕ) (x : ℝ) : cdf (abil k) x = cdf unif x ^ (k + 1) := by
  rw [cdf_powLaw, pow_succ]
  ring

theorem abil_Iic_zero (k : ℕ) : abil k (Iic 0) = 0 := by
  have h := cdf_abil k 0
  rw [cdf_unif_of_mem 0 le_rfl zero_le_one, zero_pow (Nat.succ_ne_zero k), cdf_eq_real,
    measureReal_def] at h
  exact ((ENNReal.toReal_eq_zero_iff _).mp h).resolve_right (measure_ne_top _ _)

theorem abil_ae_pos (k : ℕ) : ∀ᵐ x ∂(abil k), 0 < x := by
  rw [ae_iff]
  have e : {x : ℝ | ¬ 0 < x} = Iic 0 := by ext x; simp
  rw [e]
  exact abil_Iic_zero k

variable {Q : ℕ} (kk : Fin Q → ℕ)

/-- At `μ = 0` an entrant of ability `n_i` wins with probability `n_i / (1 + Σ_{j ∈ S} n_j)`, the
    incumbent having ability 1. -/
theorem winProb_zero_entrant (S : Finset (Fin Q)) (i : Fin Q) (hi : i ∈ S) :
    winProb (profile (fun j => abil (kk j)) (Measure.dirac 0) S) unif i
      = ((kk i : ℝ) + 1) / (1 + ∑ j ∈ S, ((kk j : ℝ) + 1)) := by
  unfold winProb
  rw [profile_mem _ _ _ i hi]
  set N : ℕ := ∑ j ∈ S.erase i, (kk j + 1) with hN
  have hfilt : (Finset.univ.erase i).filter (fun j => j ∈ S) = S.erase i := by
    ext j
    simp only [Finset.mem_filter, Finset.mem_erase, Finset.mem_univ, and_true]
  have e : ∀ᵐ x ∂(abil (kk i)), cdf unif x * ∏ j ∈ Finset.univ.erase i,
      cdf (profile (fun j => abil (kk j)) (Measure.dirac 0) S j) x = cdf unif x ^ (N + 1) := by
    refine (abil_ae_pos (kk i)).mono (fun x hx => ?_)
    have h1 : ∏ j ∈ Finset.univ.erase i,
        cdf (profile (fun j => abil (kk j)) (Measure.dirac 0) S j) x
        = ∏ j ∈ Finset.univ.erase i, (if j ∈ S then cdf unif x ^ (kk j + 1) else 1) := by
      refine Finset.prod_congr rfl (fun j _ => ?_)
      rw [cdf_profile, cdf_abil, cdf_dirac_zero x hx.le]
    rw [h1, ← Finset.prod_filter, hfilt, Finset.prod_pow_eq_pow_sum, ← hN, pow_succ]
    ring
  rw [integral_congr_ae e,
    integral_iid unif (kk i) (fun x => cdf unif x ^ (N + 1))
      ((cdf unif).mono.measurable.pow_const _) 1 (fun x => by
        rw [abs_of_nonneg (pow_nonneg (cdf_nonneg unif x) _)]
        exact pow_le_one₀ (cdf_nonneg unif x) (cdf_le_one unif x))]
  have e2 : ∀ x, cdf unif x ^ (N + 1) * cdf unif x ^ (kk i) = cdf unif x ^ (N + 1 + kk i) :=
    fun x => (pow_add _ _ _).symm
  simp_rw [e2]
  rw [integral_cdf_pow unif, ← Finset.add_sum_erase S _ hi]
  have hNr : (N : ℝ) = ∑ j ∈ S.erase i, ((kk j : ℝ) + 1) := by
    rw [hN]
    push_cast
    rfl
  rw [← hNr]
  push_cast
  field_simp
  ring

/-- At `μ = 0` a non-entrant scores 0 and never beats the investing incumbent. -/
theorem winProb_zero_outsider (S : Finset (Fin Q)) (i : Fin Q) (hi : i ∉ S) :
    winProb (profile (fun j => abil (kk j)) (Measure.dirac 0) S) unif i = 0 := by
  unfold winProb
  rw [profile_not_mem _ _ _ i hi, integral_dirac, cdf_unif_of_mem 0 le_rfl zero_le_one, zero_mul]

/-- At `μ = 0` the gain is `V n_i / (1 + n_i + Σ_{j ∈ T} n_j)`, which depends on who the other
    entrants are, not only on how many. -/
theorem gain_zero (V : ℝ) (i : Fin Q) (T : Finset (Fin Q)) (hi : i ∉ T) :
    gain V (fun j => abil (kk j)) (Measure.dirac 0) unif i T
      = V * (((kk i : ℝ) + 1) / (1 + (((kk i : ℝ) + 1) + ∑ j ∈ T, ((kk j : ℝ) + 1)))) := by
  unfold gain
  rw [winProb_zero_entrant kk (insert i T) i (Finset.mem_insert_self i T),
    winProb_zero_outsider kk T i hi, Finset.sum_insert hi, sub_zero]

end Zero

section Witness

theorem crra_two (x : ℝ) : crra 2 x = -x⁻¹ := by
  unfold crra
  rw [show (1 : ℝ) - 2 = -1 by norm_num, Real.rpow_neg_one]
  ring

/-- Abilities `1, 2, 3` from the richest to the poorest, so ability falls with wealth. -/
def kk3 : Fin 3 → ℕ := ![0, 1, 2]

/-- Wealth `11/4, 9/4, 2`, richest first. -/
noncomputable def w3 : Fin 3 → ℝ := ![11 / 4, 9 / 4, 2]

/-- The cost of entry under CRRA utility with `γ = 2` and fee `c = 1`. -/
noncomputable def κ3 (j : Fin 3) : ℝ := kappa (crra 2) 1 (w3 j)

theorem w3_val : w3 0 = 11 / 4 ∧ w3 1 = 9 / 4 ∧ w3 2 = 2 := ⟨rfl, rfl, rfl⟩

theorem kk3_val : kk3 0 = 0 ∧ kk3 1 = 1 ∧ kk3 2 = 2 := ⟨rfl, rfl, rfl⟩

theorem κ3_values : κ3 0 = 16 / 77 ∧ κ3 1 = 16 / 45 ∧ κ3 2 = 1 / 2 := by
  obtain ⟨h0, h1, h2⟩ := w3_val
  refine ⟨?_, ?_, ?_⟩
  · simp only [κ3, kappa, crra_two, h0]
    norm_num
  · simp only [κ3, kappa, crra_two, h1]
    norm_num
  · simp only [κ3, kappa, crra_two, h2]
    norm_num

/-- Wealth falls and ability rises along the ranking. -/
theorem ability_against_wealth : w3 1 < w3 0 ∧ w3 2 < w3 1 ∧ kk3 0 < kk3 1 ∧ kk3 1 < kk3 2 := by
  obtain ⟨h0, h1, h2⟩ := w3_val
  obtain ⟨k0, k1, k2⟩ := kk3_val
  rw [h0, h1, h2, k0, k1, k2]
  norm_num

/-- The gains at `V = 1` that the two equilibria use. -/
theorem gains3 :
    gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 2 ∅ = 3 / 4
    ∧ gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 0 {2} = 1 / 5
    ∧ gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 1 {2} = 1 / 3
    ∧ gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 0 {1} = 1 / 4
    ∧ gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 1 {0} = 1 / 2
    ∧ gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 2 {0, 1} = 3 / 7 := by
  obtain ⟨k0, k1, k2⟩ := kk3_val
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_⟩
  · rw [gain_zero kk3 1 2 ∅ (by decide), Finset.sum_empty, k2]
    norm_num
  · rw [gain_zero kk3 1 0 {2} (by decide), Finset.sum_singleton, k0, k2]
    norm_num
  · rw [gain_zero kk3 1 1 {2} (by decide), Finset.sum_singleton, k1, k2]
    norm_num
  · rw [gain_zero kk3 1 0 {1} (by decide), Finset.sum_singleton, k0, k1]
    norm_num
  · rw [gain_zero kk3 1 1 {0} (by decide), Finset.sum_singleton, k1, k0]
    norm_num
  · rw [gain_zero kk3 1 2 {0, 1} (by decide), Finset.sum_pair (by decide), k2, k0, k1]
    norm_num

/-- **The anonymity boundary at `μ = 0`, exact.** With ability falling in wealth, the poorest
    challenger entering alone and the two richest entering together are both equilibria, so
    equilibria of different sizes coexist. -/
theorem anon_witness_zero :
    IsEquilibrium 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif κ3 {2}
      ∧ IsEquilibrium 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif κ3 {0, 1} := by
  obtain ⟨c0, c1, c2⟩ := κ3_values
  obtain ⟨g2, g02, g12, g01, g10, g201⟩ := gains3
  have e2 : ({2} : Finset (Fin 3)).erase 2 = ∅ := by decide
  have e0 : ({0, 1} : Finset (Fin 3)).erase 0 = {1} := by decide
  have e1 : ({0, 1} : Finset (Fin 3)).erase 1 = {0} := by decide
  refine ⟨⟨fun i hi => ?_, fun i hi => ?_⟩, ⟨fun i hi => ?_, fun i hi => ?_⟩⟩
  · rw [Finset.mem_singleton] at hi
    subst hi
    rw [e2, g2, c2]
    norm_num
  · fin_cases i
    · show gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 0 {2} < κ3 0
      rw [g02, c0]
      norm_num
    · show gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 1 {2} < κ3 1
      rw [g12, c1]
      norm_num
    · exact absurd (Finset.mem_singleton_self _) hi
  · rw [Finset.mem_insert, Finset.mem_singleton] at hi
    rcases hi with rfl | rfl
    · rw [e0, g01, c0]
      norm_num
    · rw [e1, g10, c1]
      norm_num
  · fin_cases i
    · exact absurd (Finset.mem_insert_self _ _) hi
    · exact absurd (Finset.mem_insert_of_mem (Finset.mem_singleton_self _)) hi
    · show gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 2 {0, 1} < κ3 2
      rw [g201, c2]
      norm_num

end Witness

section Positive

variable {Q : ℕ} (kk : Fin Q → ℕ)

/-- The model's laws at `μ`. Entrant `j` scores `μ r + (1 − μ) s_j` with `r` uniform and `s_j` of
    ability `kk j + 1`. A non-entrant scores `μ r`. The incumbent invests, with ability 1. -/
noncomputable abbrev entμ (μ : ℝ) (j : Fin Q) : Measure ℝ := investorLaw μ unif (abil (kk j))
noncomputable abbrev outμ (μ : ℝ) : Measure ℝ := nonInvestorLaw μ unif
noncomputable abbrev incμ (μ : ℝ) : Measure ℝ := investorLaw μ unif unif

theorem prod_cdf_bound (L : Fin Q → Measure ℝ) (C : Measure ℝ) (s : Finset (Fin Q)) (y : ℝ) :
    |cdf C y * ∏ j ∈ s, cdf (L j) y| ≤ 1 := by
  have h0 : 0 ≤ ∏ j ∈ s, cdf (L j) y := Finset.prod_nonneg (fun j _ => cdf_nonneg _ _)
  have h1 : ∏ j ∈ s, cdf (L j) y ≤ 1 :=
    Finset.prod_le_one (fun j _ => cdf_nonneg _ _) (fun j _ => cdf_le_one _ _)
  rw [abs_of_nonneg (mul_nonneg (cdf_nonneg _ _) h0)]
  exact mul_le_one₀ (cdf_le_one _ _) h0 h1

theorem measurable_prod_cdf (L : Fin Q → Measure ℝ) (C : Measure ℝ) (s : Finset (Fin Q)) :
    Measurable (fun y => cdf C y * ∏ j ∈ s, cdf (L j) y) :=
  (cdf C).mono.measurable.mul (Finset.measurable_prod s (fun j _ => (cdf (L j)).mono.measurable))

theorem ae_snd_pos (ν : Measure ℝ) [IsProbabilityMeasure ν] (hν : ν (Iic 0) = 0) :
    ∀ᵐ p ∂(unif.prod ν), 0 < p.2 := by
  rw [ae_iff]
  have e : {p : ℝ × ℝ | ¬ 0 < p.2} = univ ×ˢ Iic 0 := by ext p; simp
  rw [e, Measure.prod_prod, hν, mul_zero]

/-- **The win probabilities are continuous at `μ = 0`.** -/
theorem winProb_tendsto (S : Finset (Fin Q)) (i : Fin Q) :
    Tendsto (fun μ => winProb (profile (entμ kk μ) (outμ μ) S) (incμ μ) i) (𝓝 0)
      (𝓝 (winProb (profile (fun j => abil (kk j)) (Measure.dirac 0) S) unif i)) := by
  by_cases hi : i ∈ S
  · -- an entrant
    set g : ℝ → ℝ → ℝ := fun μ y => cdf (incμ μ) y
      * ∏ j ∈ Finset.univ.erase i, cdf (profile (entμ kk μ) (outμ μ) S j) y with hg
    set g0 : ℝ → ℝ := fun y => cdf unif y
      * ∏ j ∈ Finset.univ.erase i, cdf (profile (fun j => abil (kk j)) (Measure.dirac 0) S j) y
      with hg0
    have e : ∀ μ, winProb (profile (entμ kk μ) (outμ μ) S) (incμ μ) i
        = ∫ p, g μ (μ * p.1 + (1 - μ) * p.2) ∂(unif.prod (abil (kk i))) := by
      intro μ
      unfold winProb
      rw [profile_mem _ _ _ i hi]
      exact integral_investorLaw unif (abil (kk i)) μ (g μ) (measurable_prod_cdf _ _ _)
    have e0 : winProb (profile (fun j => abil (kk j)) (Measure.dirac 0) S) unif i
        = ∫ p, g0 p.2 ∂(unif.prod (abil (kk i))) := by
      unfold winProb
      rw [profile_mem _ _ _ i hi, integral_snd_prod unif (abil (kk i)) g0 (measurable_prod_cdf _ _ _)]
    rw [e0]
    refine (tendsto_congr e).mpr ?_
    refine tendsto_integral_filter_of_dominated_convergence (fun _ => (1 : ℝ)) ?_ ?_
      (integrable_const 1) ?_
    · exact Eventually.of_forall (fun μ =>
        ((measurable_prod_cdf _ _ _).comp (measurable_investorScore μ)).aestronglyMeasurable)
    · exact Eventually.of_forall (fun μ => ae_of_all _ (fun p => prod_cdf_bound _ _ _ _))
    · refine (ae_snd_pos (abil (kk i)) (abil_Iic_zero (kk i))).mono (fun p hp => ?_)
      have hc : Continuous (fun t : ℝ => t * p.1 + (1 - t) * p.2) := by fun_prop
      have hT : Tendsto (fun t : ℝ => t * p.1 + (1 - t) * p.2) (𝓝 0) (𝓝 p.2) := by
        simpa using hc.tendsto 0
      have hC := tendsto_cdf_investorLaw unif unif p.2 (measure_singleton _) _ hT
      have hP : Tendsto (fun μ => ∏ j ∈ Finset.univ.erase i,
          cdf (profile (entμ kk μ) (outμ μ) S j) (μ * p.1 + (1 - μ) * p.2)) (𝓝 0)
          (𝓝 (∏ j ∈ Finset.univ.erase i,
            cdf (profile (fun j => abil (kk j)) (Measure.dirac 0) S j) p.2)) := by
        refine tendsto_finset_prod _ (fun j _ => ?_)
        by_cases hj : j ∈ S
        · simp only [profile_mem _ _ _ j hj]
          exact tendsto_cdf_investorLaw unif (abil (kk j)) p.2 (measure_singleton _) _ hT
        · simp only [profile_not_mem _ _ _ j hj]
          rw [cdf_dirac_zero p.2 hp.le]
          exact tendsto_cdf_nonInvestorLaw unif p.2 hp _ hT
      exact hC.mul hP
  · -- a non-entrant
    set g : ℝ → ℝ → ℝ := fun μ y => cdf (incμ μ) y
      * ∏ j ∈ Finset.univ.erase i, cdf (profile (entμ kk μ) (outμ μ) S j) y with hg
    have e : ∀ μ, winProb (profile (entμ kk μ) (outμ μ) S) (incμ μ) i
        = ∫ r, g μ (μ * r) ∂unif := by
      intro μ
      unfold winProb
      rw [profile_not_mem _ _ _ i hi]
      exact integral_nonInvestorLaw unif μ (g μ) (measurable_prod_cdf _ _ _)
    rw [winProb_zero_outsider kk S i hi]
    refine (tendsto_congr e).mpr ?_
    have hlim : Tendsto (fun μ => ∫ r, g μ (μ * r) ∂unif) (𝓝 0) (𝓝 (∫ _r, (0 : ℝ) ∂unif)) := by
      refine tendsto_integral_filter_of_dominated_convergence (fun _ => (1 : ℝ)) ?_ ?_
        (integrable_const 1) ?_
      · exact Eventually.of_forall (fun μ =>
          ((measurable_prod_cdf _ _ _).comp (measurable_nonInvestorScore μ)).aestronglyMeasurable)
      · exact Eventually.of_forall (fun μ => ae_of_all _ (fun r => prod_cdf_bound _ _ _ _))
      · refine ae_of_all _ (fun r => ?_)
        have hc : Continuous (fun t : ℝ => t * r) := by fun_prop
        have hT : Tendsto (fun t : ℝ => t * r) (𝓝 0) (𝓝 0) := by simpa using hc.tendsto 0
        have hC := tendsto_cdf_investorLaw unif unif 0 (measure_singleton _) _ hT
        rw [cdf_unif_of_mem 0 le_rfl zero_le_one] at hC
        refine squeeze_zero (fun μ => ?_) (fun μ => ?_) hC
        · exact mul_nonneg (cdf_nonneg _ _) (Finset.prod_nonneg (fun j _ => cdf_nonneg _ _))
        · exact mul_le_of_le_one_right (cdf_nonneg _ _)
            (Finset.prod_le_one (fun j _ => cdf_nonneg _ _) (fun j _ => cdf_le_one _ _))
    rw [integral_zero] at hlim
    exact hlim

theorem gain_tendsto (V : ℝ) (i : Fin Q) (T : Finset (Fin Q)) :
    Tendsto (fun μ => gain V (entμ kk μ) (outμ μ) (incμ μ) i T) (𝓝 0)
      (𝓝 (gain V (fun j => abil (kk j)) (Measure.dirac 0) unif i T)) := by
  unfold gain
  exact ((winProb_tendsto kk (insert i T) i).sub (winProb_tendsto kk T i)).const_mul V

/-- For `μ ≠ 0` every law in the economy has no atoms, as the primitives require. -/
theorem laws_noAtoms (μ : ℝ) (hμ : μ ≠ 0) (j : Fin Q) :
    NoAtoms (entμ kk μ j) ∧ NoAtoms (outμ μ) ∧ NoAtoms (incμ μ) :=
  ⟨investorLaw_noAtoms' μ hμ unif _, nonInvestorLaw_noAtoms' μ hμ unif,
    investorLaw_noAtoms' μ hμ unif unif⟩

/-- **The anonymity boundary inside the primitives.** For every small enough `μ > 0`, with ability
    falling in wealth, the poorest challenger entering alone and the two richest entering
    together are both equilibria. -/
theorem anon_witness_positive :
    ∀ᶠ μ in 𝓝[>] 0, IsEquilibrium 1 (entμ kk3 μ) (outμ μ) (incμ μ) κ3 {2}
      ∧ IsEquilibrium 1 (entμ kk3 μ) (outμ μ) (incμ μ) κ3 {0, 1} := by
  obtain ⟨c0, c1, c2⟩ := κ3_values
  obtain ⟨g2, g02, g12, g01, g10, g201⟩ := gains3
  have e2 : ({2} : Finset (Fin 3)).erase 2 = ∅ := by decide
  have e0 : ({0, 1} : Finset (Fin 3)).erase 0 = {1} := by decide
  have e1 : ({0, 1} : Finset (Fin 3)).erase 1 = {0} := by decide
  have t1 := (gain_tendsto kk3 1 2 ∅).eventually (eventually_gt_nhds
    (show κ3 2 < gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 2 ∅ by rw [g2, c2]; norm_num))
  have t2 := (gain_tendsto kk3 1 0 {2}).eventually (eventually_lt_nhds
    (show gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 0 {2} < κ3 0 by
      rw [g02, c0]; norm_num))
  have t3 := (gain_tendsto kk3 1 1 {2}).eventually (eventually_lt_nhds
    (show gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 1 {2} < κ3 1 by
      rw [g12, c1]; norm_num))
  have t4 := (gain_tendsto kk3 1 0 {1}).eventually (eventually_gt_nhds
    (show κ3 0 < gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 0 {1} by
      rw [g01, c0]; norm_num))
  have t5 := (gain_tendsto kk3 1 1 {0}).eventually (eventually_gt_nhds
    (show κ3 1 < gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 1 {0} by
      rw [g10, c1]; norm_num))
  have t6 := (gain_tendsto kk3 1 2 {0, 1}).eventually (eventually_lt_nhds
    (show gain 1 (fun j => abil (kk3 j)) (Measure.dirac 0) unif 2 {0, 1} < κ3 2 by
      rw [g201, c2]; norm_num))
  filter_upwards [nhdsWithin_le_nhds t1, nhdsWithin_le_nhds t2, nhdsWithin_le_nhds t3,
    nhdsWithin_le_nhds t4, nhdsWithin_le_nhds t5, nhdsWithin_le_nhds t6]
    with μ h1 h2 h3 h4 h5 h6
  refine ⟨⟨fun i hi => ?_, fun i hi => ?_⟩, ⟨fun i hi => ?_, fun i hi => ?_⟩⟩
  · rw [Finset.mem_singleton] at hi
    subst hi
    rw [e2]
    exact h1.le
  · fin_cases i
    · exact h2
    · exact h3
    · exact absurd (Finset.mem_singleton_self _) hi
  · rw [Finset.mem_insert, Finset.mem_singleton] at hi
    rcases hi with rfl | rfl
    · rw [e0]
      exact h4.le
    · rw [e1]
      exact h5.le
  · fin_cases i
    · exact absurd (Finset.mem_insert_self _ _) hi
    · exact absurd (Finset.mem_insert_of_mem (Finset.mem_singleton_self _)) hi
    · exact h6

/-- Some `μ > 0` carries both equilibria, of sizes 1 and 2. -/
theorem anon_witness_exists :
    ∃ μ : ℝ, 0 < μ ∧ IsEquilibrium 1 (entμ kk3 μ) (outμ μ) (incμ μ) κ3 {2}
      ∧ IsEquilibrium 1 (entμ kk3 μ) (outμ μ) (incμ μ) κ3 {0, 1}
      ∧ ({2} : Finset (Fin 3)).card ≠ ({0, 1} : Finset (Fin 3)).card := by
  obtain ⟨μ, h, hμ⟩ := (anon_witness_positive.and self_mem_nhdsWithin).exists
  exact ⟨μ, hμ, h.1, h.2, by decide⟩

end Positive

end EntryContestAnon
