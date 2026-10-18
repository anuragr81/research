import Mathlib

/-!
# Selection loss (manuscript row M34)

The selection loss is the difference between the highest base component among the players
and the winner's base component. This file proves two things about it.

1. Selection-neutrality of reordering. Under anonymity the draws `(r_i, s_i)` are independent
   and identically distributed across players, so the law of the winner's base component
   under an entrant set depends on the set only through its size: permuting the entry
   vector leaves the law unchanged (`law_perm`), and two entry vectors with the same number
   of entrants give the same law (`law_count`). The winner is the strict maximiser of the
   score `μ r_i + (1 − μ) s_i e_i`; where no strict maximiser exists the convention is `0`,
   which the atomless laws of the paper make a null event.
2. The endpoint at `μ = 1`. With a unique highest base component the loss is zero
   (`loss_zero_at_one`). At `μ = 0` the score reads the bought component alone
   (`score_zero`), so the winner's base component is a single draw of `r`
   (`WeightEndpoints.winner_base_law`).

Control: at `μ = 1/2` the loss can be positive (`loss_pos_at_half`).
-/

open MeasureTheory Set
open scoped Classical

namespace SelectionLoss

variable {n : ℕ}

/-- The score of player `i`: `μ r_i + (1 − μ) s_i e_i`, with `x i = (r_i, s_i)`. -/
def score (μ : ℝ) (e : Fin n → Bool) (x : Fin n → ℝ × ℝ) (i : Fin n) : ℝ :=
  μ * (x i).1 + (1 - μ) * (x i).2 * (if e i then 1 else 0)

/-- Player `i` is the strict maximiser of the score. -/
def IsWinner (μ : ℝ) (e : Fin n → Bool) (x : Fin n → ℝ × ℝ) (i : Fin n) : Prop :=
  ∀ j, j ≠ i → score μ e x j < score μ e x i

/-- The winner's base component, `0` when there is no strict maximiser. -/
noncomputable def winnerBase (μ : ℝ) (e : Fin n → Bool) (x : Fin n → ℝ × ℝ) : ℝ :=
  ∑ i, if IsWinner μ e x i then (x i).1 else 0

/-- The highest base component among the players. -/
noncomputable def topBase (x : Fin (n + 1) → ℝ × ℝ) : ℝ :=
  Finset.univ.sup' Finset.univ_nonempty fun i => (x i).1

/-- The selection loss. -/
noncomputable def loss (μ : ℝ) (e : Fin (n + 1) → Bool) (x : Fin (n + 1) → ℝ × ℝ) : ℝ :=
  topBase x - winnerBase μ e x

/-! ## 1. Reordering is selection-neutral -/

theorem score_perm (μ : ℝ) (e : Fin n → Bool) (x : Fin n → ℝ × ℝ) (σ : Equiv.Perm (Fin n))
    (k : Fin n) : score μ e (x ∘ σ.symm) (σ k) = score μ (e ∘ σ) x k := by
  simp [score]

theorem isWinner_perm (μ : ℝ) (e : Fin n → Bool) (x : Fin n → ℝ × ℝ) (σ : Equiv.Perm (Fin n))
    (i : Fin n) : IsWinner μ e (x ∘ σ.symm) (σ i) ↔ IsWinner μ (e ∘ σ) x i := by
  constructor
  · intro h k hk
    have := h (σ k) (fun h' => hk (σ.injective h'))
    rwa [score_perm, score_perm] at this
  · intro h j hj
    obtain ⟨k, rfl⟩ := σ.surjective j
    rw [score_perm, score_perm]
    exact h k (fun h' => hj (by rw [h']))

theorem winnerBase_perm (μ : ℝ) (e : Fin n → Bool) (x : Fin n → ℝ × ℝ) (σ : Equiv.Perm (Fin n)) :
    winnerBase μ (e ∘ σ) x = winnerBase μ e (x ∘ σ.symm) := by
  unfold winnerBase
  rw [← Equiv.sum_comp σ (fun i => if IsWinner μ e (x ∘ σ.symm) i then (x ∘ σ.symm) i |>.1 else 0)]
  refine Finset.sum_congr rfl fun i _ => ?_
  simp [isWinner_perm]

theorem measurable_score (μ : ℝ) (e : Fin n → Bool) (i : Fin n) :
    Measurable fun x : Fin n → ℝ × ℝ => score μ e x i := by
  unfold score
  exact (measurable_const.mul (measurable_pi_apply i).fst).add
    ((measurable_const.mul (measurable_pi_apply i).snd).mul measurable_const)

theorem measurableSet_isWinner (μ : ℝ) (e : Fin n → Bool) (i : Fin n) :
    MeasurableSet {x : Fin n → ℝ × ℝ | IsWinner μ e x i} := by
  have : {x : Fin n → ℝ × ℝ | IsWinner μ e x i}
      = ⋂ j, {x | j ≠ i → score μ e x j < score μ e x i} := by
    ext x; simp [IsWinner]
  rw [this]
  refine MeasurableSet.iInter fun j => ?_
  by_cases hj : j = i
  · simp [hj]
  · simpa [hj] using measurableSet_lt (measurable_score μ e j) (measurable_score μ e i)

theorem winnerBase_measurable (μ : ℝ) (e : Fin n → Bool) : Measurable (winnerBase μ e) := by
  unfold winnerBase
  exact Finset.measurable_sum _ fun i _ =>
    Measurable.ite (measurableSet_isWinner μ e i) (measurable_pi_apply i).fst measurable_const

/-- Independent and identically distributed draws are exchangeable: the product law is
    invariant under a permutation of the players. -/
theorem pi_map_perm {β : Type*} [MeasurableSpace β] (ν : Measure β) [IsProbabilityMeasure ν]
    (σ : Equiv.Perm (Fin n)) :
    (Measure.pi fun _ : Fin n => ν).map (fun x => x ∘ σ) = Measure.pi fun _ => ν := by
  have hm : Measurable (fun x : Fin n → β => x ∘ σ) :=
    measurable_pi_lambda _ fun i => measurable_pi_apply (σ i)
  refine (Measure.pi_eq (μ := fun _ : Fin n => ν) fun s hs => ?_).symm
  rw [Measure.map_apply hm (MeasurableSet.univ_pi hs)]
  have : (fun x : Fin n → β => x ∘ σ) ⁻¹' Set.pi univ s = Set.pi univ fun j => s (σ.symm j) := by
    ext x
    simp only [mem_preimage, mem_univ_pi, Function.comp_apply]
    constructor
    · intro h j; simpa using h (σ.symm j)
    · intro h i; simpa using h (σ i)
  rw [this, Measure.pi_pi]
  exact Equiv.prod_comp σ.symm fun j => ν (s j)

/-- Permuting the entry vector leaves the law of the winner's base component unchanged. -/
theorem law_perm {Ω : Type*} [MeasurableSpace Ω] (P : Measure Ω) (X : Ω → Fin n → ℝ × ℝ)
    (hX : Measurable X) (ν : Measure (ℝ × ℝ)) [IsProbabilityMeasure ν]
    (hlaw : P.map X = Measure.pi fun _ => ν) (μ : ℝ) (e : Fin n → Bool) (σ : Equiv.Perm (Fin n)) :
    P.map (fun ω => winnerBase μ (e ∘ σ) (X ω)) = P.map (fun ω => winnerBase μ e (X ω)) := by
  have hw : Measurable (winnerBase μ e) := winnerBase_measurable μ e
  have hperm : Measurable (fun x : Fin n → ℝ × ℝ => x ∘ σ.symm) :=
    measurable_pi_lambda _ fun i => measurable_pi_apply _
  have hXσ : Measurable (fun ω => X ω ∘ σ.symm) := hperm.comp hX
  calc P.map (fun ω => winnerBase μ (e ∘ σ) (X ω))
      = P.map (fun ω => winnerBase μ e (X ω ∘ σ.symm)) := by simp_rw [winnerBase_perm]
    _ = (P.map (fun ω => X ω ∘ σ.symm)).map (winnerBase μ e) := by
        rw [Measure.map_map hw hXσ]; rfl
    _ = ((P.map X).map (fun x => x ∘ σ.symm)).map (winnerBase μ e) := by
        rw [Measure.map_map hperm hX]; rfl
    _ = (Measure.pi fun _ => ν).map (winnerBase μ e) := by rw [hlaw, pi_map_perm]
    _ = (P.map X).map (winnerBase μ e) := by rw [hlaw]
    _ = P.map (fun ω => winnerBase μ e (X ω)) := by rw [Measure.map_map hw hX]; rfl

/-- Two entry vectors with the same number of entrants are permutations of each other. -/
theorem exists_perm_of_card_eq (e e' : Fin n → Bool)
    (hc : (Finset.univ.filter fun i => e i = true).card
        = (Finset.univ.filter fun i => e' i = true).card) :
    ∃ σ : Equiv.Perm (Fin n), e = e' ∘ σ := by
  have h : Fintype.card {i // e i = true} = Fintype.card {i // e' i = true} := by
    rw [Fintype.card_subtype, Fintype.card_subtype]; exact hc
  let f := Fintype.equivOfCardEq h
  refine ⟨f.extendSubtype, funext fun i => ?_⟩
  by_cases hi : e i = true
  · have := (f ⟨i, hi⟩).2
    simp only [Function.comp_apply, f.extendSubtype_apply_of_mem i hi]
    rw [hi]; exact this.symm
  · have := (f.toCompl ⟨i, hi⟩).2
    simp only [Function.comp_apply, f.extendSubtype_apply_of_not_mem i hi]
    simp only [Bool.not_eq_true] at hi this
    rw [hi, this]

/-- The law of the winner's base component depends on the entrant set only through its size. -/
theorem law_count {Ω : Type*} [MeasurableSpace Ω] (P : Measure Ω) (X : Ω → Fin n → ℝ × ℝ)
    (hX : Measurable X) (ν : Measure (ℝ × ℝ)) [IsProbabilityMeasure ν]
    (hlaw : P.map X = Measure.pi fun _ => ν) (μ : ℝ) (e e' : Fin n → Bool)
    (hc : (Finset.univ.filter fun i => e i = true).card
        = (Finset.univ.filter fun i => e' i = true).card) :
    P.map (fun ω => winnerBase μ e (X ω)) = P.map (fun ω => winnerBase μ e' (X ω)) := by
  obtain ⟨σ, hσ⟩ := exists_perm_of_card_eq e e' hc
  rw [hσ]
  exact law_perm P X hX ν hlaw μ e' σ

/-! ## 2. The endpoints of the weight -/

theorem score_one (e : Fin n → Bool) (x : Fin n → ℝ × ℝ) (i : Fin n) : score 1 e x i = (x i).1 := by
  simp [score]

theorem score_zero (e : Fin n → Bool) (x : Fin n → ℝ × ℝ) (i : Fin n) :
    score 0 e x i = (x i).2 * (if e i then 1 else 0) := by
  simp [score]

/-- At `μ = 1`, with a unique highest base component, the loss is zero. -/
theorem loss_zero_at_one (e : Fin (n + 1) → Bool) (x : Fin (n + 1) → ℝ × ℝ) (i : Fin (n + 1))
    (hi : ∀ j, j ≠ i → (x j).1 < (x i).1) : loss 1 e x = 0 := by
  have hwin : winnerBase 1 e x = (x i).1 := by
    unfold winnerBase
    rw [Finset.sum_eq_single i]
    · have : IsWinner 1 e x i := fun j hj => by rw [score_one, score_one]; exact hi j hj
      simp [this]
    · intro j _ hj
      have : ¬ IsWinner 1 e x j := fun h => by
        have := h i (Ne.symm hj)
        rw [score_one, score_one] at this
        exact lt_asymm this (hi j hj)
      simp [this]
    · intro h; exact absurd (Finset.mem_univ i) h
  have htop : topBase x = (x i).1 := by
    unfold topBase
    apply le_antisymm
    · exact Finset.sup'_le _ _ fun j _ => by
        by_cases hj : j = i
        · rw [hj]
        · exact le_of_lt (hi j hj)
    · exact Finset.le_sup' (fun j => (x j).1) (Finset.mem_univ i)
  unfold loss
  rw [hwin, htop, sub_self]

/-- Control: at `μ = 1/2` the loss can be positive. The bought component `4` carries player
    `1`, whose base component is `0`, past player `0`, whose base component is `1`. -/
theorem loss_pos_at_half : 0 < loss (1/2) ![true, true] ![((1 : ℝ), (0 : ℝ)), (0, 4)] := by
  have h0 : ¬ IsWinner (1/2) ![true, true] ![((1 : ℝ), (0 : ℝ)), (0, 4)] 0 := by
    intro h
    have := h 1 (by decide)
    norm_num [score] at this
  have h1 : IsWinner (1/2) ![true, true] ![((1 : ℝ), (0 : ℝ)), (0, 4)] 1 := by
    intro j hj
    fin_cases j
    · norm_num [score]
    · exact absurd rfl hj
  have hwin : winnerBase (1/2) ![true, true] ![((1 : ℝ), (0 : ℝ)), (0, 4)] = 0 := by
    unfold winnerBase
    rw [Fin.sum_univ_two, if_neg h0, if_pos h1]
    norm_num
  have htop : (1 : ℝ) ≤ topBase ![((1 : ℝ), (0 : ℝ)), (0, 4)] :=
    Finset.le_sup' (fun j => (![((1 : ℝ), (0 : ℝ)), (0, 4)] j).1) (Finset.mem_univ 0)
  unfold loss
  rw [hwin]
  linarith

end SelectionLoss
