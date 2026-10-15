import Mathlib
import RepresentationFOSD
import KappaSpread

/-!
# The two endpoints of the institution's weight (WEIGHT_ADDENDUM.tex)

The manuscript takes the weight `μ` on the base component as given. The addendum sets out an
extension in which the institution chooses it, and needs the benefit side at the two ends of
`[0, 1]`. This file proves the three statements the addendum marks as resting on one step.

## What is proved

1. Entry buys nothing at `μ = 1` (the addendum's table, "one step from M1"): the entrant's law
   and the outsider's law are both the law of `r` (`investorLaw_one`, `nonInvestorLaw_one`), so
   the gain is zero at every `m` (`Delta_at_one`); with `c > 0` and `u` strictly increasing the
   cost `κ` is positive (`kappa_pos`), so no challenger enters (`no_entry_at_one`).
2. W-1: at `μ = 1` every score is the base component, so any winner, a player whose score is
   highest, has the highest base component (`winner_base_at_one`), and its base component is the
   maximum over the players (`winner_base_eq_sup_at_one`).
3. W-2: at `μ = 0` every score is `s_i e_i`, a function of the bought components and the fixed
   entry decisions alone (`score_at_zero`). For any measurable rule that picks the winner from the
   bought components alone, and base components independent of the bought ones with a common law
   `ν`, the winner's base component has the law `ν` (`winner_base_law`): at `μ = 0` the rule
   selects nothing on the base component.  Controls: at `μ = 1/2` the winner need not have the
   highest base component (`winner_at_one_needs_one`), and the score reads the base component
   (`score_needs_zero`).

## Not formalised

W-3, the conjecture that the winner's expected base component does not fall in `μ` between the
endpoints, is open.
-/

open MeasureTheory ProbabilityTheory Set

set_option linter.unusedSectionVars false

namespace WeightEndpoints

/-! ## 1. Entry buys nothing at `μ = 1` -/

variable (ρr ρs : Measure ℝ) [IsProbabilityMeasure ρr] [IsProbabilityMeasure ρs]

theorem investorLaw_one : EntryContestP1P2.investorLaw 1 ρr ρs = ρr := by
  unfold EntryContestP1P2.investorLaw
  have e : (fun p : ℝ × ℝ => (1:ℝ) * p.1 + (1 - 1) * p.2) = Prod.fst := by
    funext p; simp
  rw [e, Measure.map_fst_prod]
  simp

theorem nonInvestorLaw_one : EntryContestP1P2.nonInvestorLaw 1 ρr = ρr := by
  unfold EntryContestP1P2.nonInvestorLaw
  have e : (fun r : ℝ => (1:ℝ) * r) = id := by funext r; simp
  rw [e, Measure.map_id]

/-- At `μ = 1` the gain from entering is zero at every number of other entrants, for any law of
    the incumbent. -/
theorem Delta_at_one (V : ℝ) (C : Measure ℝ) (Q m : ℕ) :
    EntryContestModel.Delta V (EntryContestP1P2.investorLaw 1 ρr ρs)
      (EntryContestP1P2.nonInvestorLaw 1 ρr) C Q m = 0 := by
  rw [investorLaw_one, nonInvestorLaw_one]
  unfold EntryContestModel.Delta
  simp

/-- With a positive fee and `u` strictly increasing, the cost of the fee in utility is positive. -/
theorem kappa_pos {u : ℝ → ℝ} (hu : StrictMono u) {c : ℝ} (hc : 0 < c) (w : ℝ) :
    0 < EntryContestKappa.kappa u c w := by
  unfold EntryContestKappa.kappa
  have := hu (show w - c < w by linarith)
  linarith

/-- No challenger enters at `μ = 1`: the entry condition `κ(w) ≤ Δ(m)` fails at every `m`. -/
theorem no_entry_at_one {u : ℝ → ℝ} (hu : StrictMono u) {c : ℝ} (hc : 0 < c) (w V : ℝ)
    (C : Measure ℝ) (Q m : ℕ) :
    ¬ EntryContestKappa.kappa u c w ≤ EntryContestModel.Delta V
      (EntryContestP1P2.investorLaw 1 ρr ρs) (EntryContestP1P2.nonInvestorLaw 1 ρr) C Q m := by
  rw [Delta_at_one]
  exact not_le.mpr (kappa_pos hu hc w)

/-! ## 2. W-1: the winner at `μ = 1` -/

/-- The score of player `i`: `μ r_i + (1 − μ) s_i e_i`. -/
def score (μ : ℝ) {n : ℕ} (r s : Fin n → ℝ) (e : Fin n → ℝ) (i : Fin n) : ℝ :=
  μ * r i + (1 - μ) * s i * e i

theorem score_at_one {n : ℕ} (r s e : Fin n → ℝ) (i : Fin n) : score 1 r s e i = r i := by
  unfold score; ring

/-- W-1: at `μ = 1` a winner has the highest base component. -/
theorem winner_base_at_one {n : ℕ} (r s e : Fin n → ℝ) (W : Fin n)
    (hW : ∀ j, score 1 r s e j ≤ score 1 r s e W) (j : Fin n) : r j ≤ r W := by
  have := hW j
  rwa [score_at_one, score_at_one] at this

/-- W-1: at `μ = 1` the winner's base component is the maximum of the base components, so it has
    the law of the maximum of the players' draws. -/
theorem winner_base_eq_sup_at_one {n : ℕ} (r s e : Fin (n + 1) → ℝ) (W : Fin (n + 1))
    (hW : ∀ j, score 1 r s e j ≤ score 1 r s e W) :
    r W = Finset.univ.sup' Finset.univ_nonempty r := by
  apply le_antisymm
  · exact Finset.le_sup' r (Finset.mem_univ W)
  · exact Finset.sup'_le _ _ (fun j _ => winner_base_at_one r s e W hW j)

/-! ## 3. W-2: the winner at `μ = 0` -/

/-- At `μ = 0` the score does not involve the base component. -/
theorem score_at_zero {n : ℕ} (r r' s e : Fin n → ℝ) (i : Fin n) :
    score 0 r s e i = score 0 r' s e i := by
  unfold score; ring

/-- W-2: if the winner is picked by a measurable rule `g` that reads only the bought components,
    and the base components are independent of the bought ones and share the law `ν`, then the
    winner's base component has the law `ν`. -/
theorem winner_base_law {Ω : Type*} [MeasurableSpace Ω] {P : Measure Ω} [IsProbabilityMeasure P]
    {n : ℕ} (r s : Fin n → Ω → ℝ) (hr : ∀ i, Measurable (r i)) (hs : ∀ i, Measurable (s i))
    (hind : IndepFun (fun ω i => r i ω) (fun ω i => s i ω) P)
    (ν : Measure ℝ) (hlaw : ∀ i, P.map (r i) = ν)
    (g : (Fin n → ℝ) → Fin n) (hg : Measurable g) :
    P.map (fun ω => r (g (fun i => s i ω)) ω) = ν := by
  have hS : Measurable (fun ω i => s i ω) := measurable_pi_lambda _ hs
  have hW : Measurable (fun ω => g (fun i => s i ω)) := hg.comp hS
  have hjoint : Measurable (fun p : Ω × Fin n => r p.2 p.1) :=
    measurable_from_prod_countable_left (fun j => hr j)
  have hRW : Measurable (fun ω => r (g (fun i => s i ω)) ω) :=
    hjoint.comp (measurable_id.prodMk hW)
  ext A hA
  rw [Measure.map_apply hRW hA]
  have hset : (fun ω => r (g (fun i => s i ω)) ω) ⁻¹' A
      = ⋃ j ∈ (Finset.univ : Finset (Fin n)),
          ((fun ω => g (fun i => s i ω)) ⁻¹' {j} ∩ r j ⁻¹' A) := by
    ext ω; simp
  have hdisj : PairwiseDisjoint (↑(Finset.univ : Finset (Fin n)))
      (fun j => (fun ω => g (fun i => s i ω)) ⁻¹' {j} ∩ r j ⁻¹' A) := by
    intro i _ j _ hij
    exact Set.disjoint_left.mpr (fun ω hi hj => hij (by
      have h1 : g (fun k => s k ω) = i := hi.1
      have h2 : g (fun k => s k ω) = j := hj.1
      rw [← h1, ← h2]))
  have hmeas : ∀ j ∈ (Finset.univ : Finset (Fin n)),
      MeasurableSet ((fun ω => g (fun i => s i ω)) ⁻¹' {j} ∩ r j ⁻¹' A) :=
    fun j _ => (hW (measurableSet_singleton j)).inter (hr j hA)
  rw [hset, measure_biUnion_finset hdisj hmeas]
  have hterm : ∀ j, P ((fun ω => g (fun i => s i ω)) ⁻¹' {j} ∩ r j ⁻¹' A)
      = P ((fun ω => g (fun i => s i ω)) ⁻¹' {j}) * ν A := by
    intro j
    have hA' : MeasurableSet {x : Fin n → ℝ | x j ∈ A} := measurable_pi_apply j hA
    have hB : MeasurableSet (g ⁻¹' {j}) := hg (measurableSet_singleton j)
    have h := hind.measure_inter_preimage_eq_mul _ _ hA' hB
    have e1 : (fun ω i => r i ω) ⁻¹' {x : Fin n → ℝ | x j ∈ A} = r j ⁻¹' A := rfl
    have e2 : (fun ω i => s i ω) ⁻¹' (g ⁻¹' {j}) = (fun ω => g (fun i => s i ω)) ⁻¹' {j} := rfl
    rw [e1, e2] at h
    rw [Set.inter_comm, h, mul_comm, ← hlaw j, Measure.map_apply (hr j) hA]
  simp_rw [hterm]
  rw [← Finset.sum_mul]
  have htot : ∑ j, P ((fun ω => g (fun i => s i ω)) ⁻¹' {j}) = 1 := by
    have hd2 : PairwiseDisjoint (↑(Finset.univ : Finset (Fin n)))
        (fun j => (fun ω => g (fun i => s i ω)) ⁻¹' {j}) := by
      intro i _ j _ hij
      exact Set.disjoint_left.mpr (fun ω hi hj => hij (by
        have h1 : g (fun k => s k ω) = i := hi
        have h2 : g (fun k => s k ω) = j := hj
        rw [← h1, ← h2]))
    rw [← measure_biUnion_finset hd2 (fun j _ => hW (measurableSet_singleton j))]
    have : (⋃ j ∈ (Finset.univ : Finset (Fin n)), (fun ω => g (fun i => s i ω)) ⁻¹' {j})
        = univ := by ext ω; simp
    rw [this, measure_univ]
  rw [htot, one_mul]

/-- Control for W-1: at `μ = 1/2` the winner need not have the highest base component. -/
theorem winner_at_one_needs_one :
    score (1/2) ![1, 0] ![0, 4] ![1, 1] 0 < score (1/2) ![1, 0] ![0, 4] ![1, 1] 1 ∧
      (![1, 0] : Fin 2 → ℝ) 1 < (![1, 0] : Fin 2 → ℝ) 0 := by
  unfold score; norm_num

/-- Control for W-2: away from `μ = 0` the score reads the base component, so a rule that picks
    the winner by score is not a rule on the bought components alone. -/
theorem score_needs_zero :
    score (1/2) ![1] ![0] ![1] 0 ≠ score (1/2) ![0] ![0] ![1] 0 := by
  unfold score; norm_num

end WeightEndpoints
