import IscLean.Economy
import IscLean.TwoStage
import Lit.CurrariniJacksonPin2009
import Mathlib.Tactic
import Mathlib.Data.Complex.ExponentialBounds

/-!
The two-stage closure rule of M64 for any pool (issue 17, items 17a to 17d).

The rule. Stage one: each entrant, independently, draws the own status group's lottery with
chance `σ` and the joint lottery otherwise. An own lottery with fewer than two members merges
into the joint one; if the joint one is then left with a single member, all entrants form one
lottery. Stage two: every lottery reassigns its members' former jobs at random as in M36.

17b. For any group sizes and any stage-one outcome, every lottery has two or more members when
the pool has, and some reassignment moves every entrant to another member's former job in the
entrant's lottery; the stage-one chances sum to one.

17a. M58 refined: an entrant pays the job holders strictly above whom the entrant's connections
reach, the own group's holders after drawing the own lottery and all holders after drawing the
joint one. No leak; receipts rise with rank among holders reached alike; under full closure each
group's holders receive exactly what the group's entrants pay.

17c. In a large pool, where a lottery's jobs have the mean of the jobs vacated into it and own
lotteries have two or more members, the gap between the groups' mean incomes is multiplied by
`1 - (1 - σ) d` each period, with `d` the rate of entry into the lottery; its exact half-life is
`ln 2 / (-ln (1 - (1 - σ) d))`, within `ln 2` periods below `ln 2 / ((1 - σ) d)`.

17d. In a large pool an entrant receives a former job of the own group with chance
`σ + (1 - σ) w`, `w` the group's share among entrants, and Coleman's inbreeding homophily index
of that chance is exactly `σ`; in a joint lottery of `n` members the own-group share among the
others tends to `w`, so the index tends to `σ`.
-/

open Finset Filter

namespace Isc

/-! ### 17b: feasibility for any group sizes -/

section Feasibility

variable {ι G : Type*} [Fintype ι] [DecidableEq ι] [DecidableEq G]

/-- The entrants of group `γ` who drew the own group's lottery in stage one. -/
def ownLottery (g : ι → G) (S : Finset ι) (γ : G) : Finset ι := S.filter (fun j => g j = γ)

/-- An entrant stays in the own group's lottery: drew it, and it has two or more members. -/
def staysOwn (g : ι → G) (S : Finset ι) (i : ι) : Prop :=
  i ∈ S ∧ 2 ≤ (ownLottery g S (g i)).card

instance (g : ι → G) (S : Finset ι) (i : ι) : Decidable (staysOwn g S i) :=
  inferInstanceAs (Decidable (_ ∧ _))

/-- The joint lottery before the last merge: every entrant not staying in an own lottery. -/
def jointLottery (g : ι → G) (S : Finset ι) : Finset ι :=
  univ.filter (fun i => ¬ staysOwn g S i)

/-- The lottery an entrant ends in: `some γ` for group `γ`'s own lottery, `none` for the joint
one. A joint lottery of one member brings every entrant into one lottery. -/
def lotteryOf (g : ι → G) (S : Finset ι) (i : ι) : Option G :=
  if (jointLottery g S).card = 1 then none else if staysOwn g S i then some (g i) else none

/-- The members of the lottery an entrant ends in. -/
def lotteryMates (g : ι → G) (S : Finset ι) (i : ι) : Finset ι :=
  univ.filter (fun j => lotteryOf g S j = lotteryOf g S i)

/-- Every lottery has two or more members when the pool has. -/
theorem lottery_has_two (g : ι → G) (S : Finset ι) (h2 : 2 ≤ Fintype.card ι) (i : ι) :
    2 ≤ (lotteryMates g S i).card := by
  by_cases hJ : (jointLottery g S).card = 1
  · have : lotteryMates g S i = univ := by
      ext j
      simp [lotteryMates, lotteryOf, hJ]
    rw [this, card_univ]
    exact h2
  · by_cases hi : staysOwn g S i
    · have hsub : ownLottery g S (g i) ⊆ lotteryMates g S i := by
        intro j hj
        have hjS : j ∈ S := (mem_filter.mp hj).1
        have hg : g j = g i := (mem_filter.mp hj).2
        have hj' : staysOwn g S j := ⟨hjS, by rw [hg]; exact hi.2⟩
        simp [lotteryMates, lotteryOf, hJ, hi, hj', hg]
      exact le_trans hi.2 (card_le_card hsub)
    · have hsub : jointLottery g S ⊆ lotteryMates g S i := by
        intro j hj
        have hj' : ¬ staysOwn g S j := (mem_filter.mp hj).2
        simp [lotteryMates, lotteryOf, hJ, hi, hj']
      have hmem : i ∈ jointLottery g S := mem_filter.mpr ⟨mem_univ _, hi⟩
      have h1 : 1 ≤ (jointLottery g S).card := card_pos.mpr ⟨i, hmem⟩
      have : 2 ≤ (jointLottery g S).card := by omega
      exact le_trans this (card_le_card hsub)

/-- In any division of a pool into lotteries of two or more members, some reassignment moves
every entrant to another member's former job within the entrant's lottery (each lottery's
members in a cycle). -/
theorem exists_reassignment_within {β : Type*} [DecidableEq β] (lab : ι → β)
    (h : ∀ i, 2 ≤ (univ.filter (fun j => lab j = lab i)).card) :
    ∃ π : Equiv.Perm ι, ∀ i, lab (π i) = lab i ∧ π i ≠ i := by
  let l : β → List ι := fun b => (univ.filter (fun j => lab j = b)).toList
  have hnd : ∀ b, (l b).Nodup := fun b => nodup_toList _
  have hmem : ∀ b j, j ∈ l b ↔ lab j = b := by
    intro b j
    simp [l]
  let f : ι → ι := fun i => (l (lab i)).formPerm i
  have hlab : ∀ i, lab (f i) = lab i := fun i =>
    (hmem _ _).mp (List.formPerm_apply_mem_of_mem ((hmem _ _).mpr rfl))
  have hinj : Function.Injective f := by
    intro i j hij
    have hl : lab i = lab j := by rw [← hlab i, hij, hlab j]
    have h' : (l (lab j)).formPerm i = (l (lab j)).formPerm j := by
      have e : f i = f j := hij
      simp only [f] at e
      rw [hl] at e
      exact e
    exact (l (lab j)).formPerm.injective h'
  refine ⟨Equiv.ofBijective f (Finite.injective_iff_bijective.mp hinj), fun i => ⟨hlab i, ?_⟩⟩
  show f i ≠ i
  have hi : i ∈ l (lab i) := (hmem _ _).mpr rfl
  refine (List.formPerm_apply_mem_ne_self_iff _ (hnd _) i hi).mpr ?_
  simpa [l, length_toList] using h i

/-- Feasibility of the two-stage rule for any group sizes: whatever the stage-one outcome, some
reassignment gives every entrant another member's former job in the entrant's own lottery. -/
theorem two_stage_feasible (g : ι → G) (S : Finset ι) (h2 : 2 ≤ Fintype.card ι) :
    ∃ π : Equiv.Perm ι, ∀ i, lotteryOf g S (π i) = lotteryOf g S i ∧ π i ≠ i :=
  exists_reassignment_within (lotteryOf g S) (lottery_has_two g S h2)

/-- An own lottery holds only the group's entrants, so its reassignments keep jobs in the group. -/
theorem own_lottery_within_group (g : ι → G) (S : Finset ι) {i : ι} {γ : G}
    (h : lotteryOf g S i = some γ) : g i = γ := by
  unfold lotteryOf at h
  split_ifs at h
  simp_all

omit [DecidableEq ι] in
/-- The stage-one chances, `σ` for each entrant drawing the own lottery, sum to one over all
outcomes. -/
theorem stage_one_chances_sum (σ : ℝ) :
    ∑ S : Finset ι, σ ^ S.card * (1 - σ) ^ (Fintype.card ι - S.card) = 1 := by
  have h := Finset.sum_pow_mul_eq_add_pow σ (1 - σ) (univ : Finset ι)
  rw [powerset_univ, card_univ] at h
  rw [h]
  norm_num

/-- Control: the last merge is needed. Three entrants, two of one group who both draw the own
lottery and one of the other: the joint lottery would hold a single member, so all three form
one lottery. -/
theorem control_last_merge_needed :
    (jointLottery (fun k : Fin 3 => decide (k.val < 2)) {0, 1}).card = 1 ∧
      lotteryMates (fun k : Fin 3 => decide (k.val < 2)) {0, 1} 2 = univ := by
  decide

/-- Control: a lone entrant can be reassigned only its own former job. -/
theorem control_feasibility_needs_two (π : Equiv.Perm (Fin 1)) : π 0 = 0 :=
  Subsingleton.elim _ _

end Feasibility

/-! ### 17a: the lottery cost goes to the job holders above whom the entrant reaches -/

section Reach

variable {ι : Type*} [DecidableEq ι]

/-- Entrant `i`'s lottery cost as received by job holder `h` when the entrant reaches the job
holders `R`: M58's equal shares among those of `R` strictly above, or among all of `R` when
none is above. -/
noncomputable def shareOf (R : Finset ι) (r : ι → ℕ) (L : ℝ) (i h : ι) : ℝ :=
  if r i < r h then L / ((above R r i).card : ℝ)
  else if (above R r i).card = 0 then L / (R.card : ℝ) else 0

/-- What job holder `h` receives when each entrant pays only the job holders it reaches. -/
noncomputable def duesReach (P : Finset ι) (reach : ι → Finset ι) (r : ι → ℕ) (L : ℝ)
    (h : ι) : ℝ :=
  ∑ i ∈ P, if h ∈ reach i then shareOf (reach i) r L i h else 0

/-- When every entrant reaches all job holders, the refined rule is M58. -/
theorem duesReach_eq_dues {P H : Finset ι} {reach : ι → Finset ι} (r : ι → ℕ) (L : ℝ)
    (hr : ∀ i ∈ P, reach i = H) {h : ι} (hh : h ∈ H) :
    duesReach P reach r L h = dues P H r L h := by
  unfold duesReach dues
  apply sum_congr rfl
  intro i hi
  rw [hr i hi, if_pos hh]
  rfl

theorem reach_share_total (R K : Finset ι) (r : ι → ℕ) (L : ℝ) (hR : R.Nonempty) (i : ι)
    (hRK : R ⊆ K) : ∑ h ∈ K, (if h ∈ R then shareOf R r L i h else 0) = L := by
  rw [sum_ite_mem, inter_eq_right.mpr hRK]
  exact dues_of_entrant R r L hR i

theorem reach_share_none (R K : Finset ι) (r : ι → ℕ) (L : ℝ) (i : ι) (hd : Disjoint K R) :
    ∑ h ∈ K, (if h ∈ R then shareOf R r L i h else 0) = 0 := by
  rw [sum_ite_mem, disjoint_iff_inter_eq_empty.mp hd, sum_empty]

/-- No leak: when every entrant reaches some job holders, the job holders receive in total the
number of entrants times the lottery cost. -/
theorem duesReach_total (P H : Finset ι) (reach : ι → Finset ι) (r : ι → ℕ) (L : ℝ)
    (hne : ∀ i ∈ P, (reach i).Nonempty) (hsub : ∀ i ∈ P, reach i ⊆ H) :
    ∑ h ∈ H, duesReach P reach r L h = L * P.card := by
  unfold duesReach
  rw [sum_comm, sum_congr rfl (fun i hi => reach_share_total _ _ r L (hne i hi) i (hsub i hi)),
    sum_const, nsmul_eq_mul]
  ring

/-- A set of job holders `K` that each entrant reaches wholly or not at all receives exactly the
lottery cost of the entrants reaching it. -/
theorem duesReach_group (P K : Finset ι) (reach : ι → Finset ι) (r : ι → ℕ) (L : ℝ)
    (hne : ∀ i ∈ P, (reach i).Nonempty)
    (hsplit : ∀ i ∈ P, reach i ⊆ K ∨ Disjoint K (reach i)) :
    ∑ h ∈ K, duesReach P reach r L h = L * (P.filter (fun i => reach i ⊆ K)).card := by
  unfold duesReach
  have hterm : ∀ i ∈ P, ∑ h ∈ K, (if h ∈ reach i then shareOf (reach i) r L i h else 0) =
      if reach i ⊆ K then L else 0 := by
    intro i hi
    by_cases hK : reach i ⊆ K
    · rw [if_pos hK]
      exact reach_share_total _ _ r L (hne i hi) i hK
    · rw [if_neg hK]
      exact reach_share_none _ _ r L i ((hsplit i hi).resolve_left hK)
  rw [sum_comm, sum_congr rfl hterm, ← sum_filter, sum_const, nsmul_eq_mul]
  ring

/-- Receipts rise with rank among job holders reached alike: if every entrant reaching `h` also
reaches `h'`, and `h'` ranks at least as high, `h'` receives at least as much. -/
theorem duesReach_mono (P : Finset ι) (reach : ι → Finset ι) (r : ι → ℕ) {L : ℝ} (hL : 0 ≤ L)
    {h h' : ι} (hreach : ∀ i ∈ P, h ∈ reach i → h' ∈ reach i) (hr : r h ≤ r h') :
    duesReach P reach r L h ≤ duesReach P reach r L h' := by
  unfold duesReach
  apply sum_le_sum
  intro i hi
  by_cases hm : h ∈ reach i
  · rw [if_pos hm, if_pos (hreach i hi hm)]
    have e := dues_mono {i} (reach i) r hL (hreach i hi hm) hr
    simp only [dues, sum_singleton] at e
    exact e
  · rw [if_neg hm]
    split_ifs
    · unfold shareOf
      split_ifs <;> positivity
    · exact le_rfl

/-- Full closure: when every entrant reaches only the own group's job holders, each group's
holders receive exactly the number of the group's entrants times the lottery cost, so under
full closure the lottery cost never crosses the groups. -/
theorem full_closure_dues_within_group {G : Type*} [DecidableEq G] (P H : Finset ι) (g : ι → G)
    (r : ι → ℕ) (L : ℝ) (γ : G)
    (hne : ∀ i ∈ P, (H.filter (fun k => g k = g i)).Nonempty) :
    ∑ h ∈ H.filter (fun k => g k = γ), duesReach P (fun i => H.filter (fun k => g k = g i)) r L h
      = L * (P.filter (fun i => g i = γ)).card := by
  rw [duesReach_group P _ _ r L hne]
  · congr 2
    apply congrArg
    apply filter_congr
    intro i hi
    constructor
    · intro hs
      obtain ⟨k, hk⟩ := hne i hi
      have h1 := (mem_filter.mp hk).2
      have h2 := (mem_filter.mp (hs hk)).2
      rw [← h1, h2]
    · intro hg
      intro k hk
      rw [mem_filter] at hk ⊢
      exact ⟨hk.1, hk.2.trans hg⟩
  · intro i _
    by_cases hg : g i = γ
    · left
      intro k hk
      rw [mem_filter] at hk ⊢
      exact ⟨hk.1, hk.2.trans hg⟩
    · right
      rw [disjoint_left]
      intro k hk hk'
      exact hg ((mem_filter.mp hk').2.symm.trans (mem_filter.mp hk).2)

/-- Control: who is reached decides who is paid. The entrant at rank 0; job holders at ranks 1
(own group) and 2 (other group). Reaching only the own group, the rank-1 holder receives the
whole cost and the rank-2 holder nothing; reaching both, each receives half. -/
theorem control_reach_decides_who_is_paid (L : ℝ) :
    duesReach ({0} : Finset (Fin 3)) (fun _ => {1}) (fun k => k.val) L 1 = L ∧
      duesReach ({0} : Finset (Fin 3)) (fun _ => {1}) (fun k => k.val) L 2 = 0 ∧
      duesReach ({0} : Finset (Fin 3)) (fun _ => {1, 2}) (fun k => k.val) L 2 = L / 2 := by
  refine ⟨?_, ?_, ?_⟩ <;>
    simp [duesReach, shareOf, above, Finset.filter_insert, Finset.filter_singleton]

end Reach

/-! ### 17c: the gap between the groups in a large pool, and its half-life -/

section LargePool

/-- The pool's mean income, group `A` holding share `w` of the jobs (M50 keeps it fixed). -/
noncomputable def poolMean (w μA μB : ℝ) : ℝ := w * μA + (1 - w) * μB

/-- A group's mean income after one period in a large pool: a share `1 - d` of its jobs is not
entered into the lottery; a share `dσ` is reassigned in the own lottery, which keeps the group's
former jobs; a share `d(1 - σ)` is reassigned in the joint lottery, whose jobs have the pool's
mean `m` when entry into the lottery does not depend on rank. -/
noncomputable def nextMean (σ d μ m : ℝ) : ℝ := (1 - d) * μ + d * σ * μ + d * (1 - σ) * m

/-- The pool's mean is unchanged by the period. -/
theorem large_pool_mean_conserved (σ d w μA μB : ℝ) :
    poolMean w (nextMean σ d μA (poolMean w μA μB)) (nextMean σ d μB (poolMean w μA μB))
      = poolMean w μA μB := by
  unfold poolMean nextMean
  ring

/-- The gap recursion in a large pool: the gap between the groups' mean incomes is multiplied
by `1 - (1 - σ) d`, whatever the groups' sizes. -/
theorem large_pool_gap_step (σ d w μA μB : ℝ) :
    nextMean σ d μA (poolMean w μA μB) - nextMean σ d μB (poolMean w μA μB)
      = (1 - (1 - σ) * d) * (μA - μB) := by
  unfold poolMean nextMean
  ring

/-- The groups' mean incomes after `t` periods. -/
noncomputable def meansAfter (σ d w μA μB : ℝ) : ℕ → ℝ × ℝ
  | 0 => (μA, μB)
  | t + 1 =>
    ((nextMean σ d (meansAfter σ d w μA μB t).1
        (poolMean w (meansAfter σ d w μA μB t).1 (meansAfter σ d w μA μB t).2)),
      (nextMean σ d (meansAfter σ d w μA μB t).2
        (poolMean w (meansAfter σ d w μA μB t).1 (meansAfter σ d w μA μB t).2)))

theorem large_pool_gap (σ d w μA μB : ℝ) (t : ℕ) :
    (meansAfter σ d w μA μB t).1 - (meansAfter σ d w μA μB t).2
      = (1 - (1 - σ) * d) ^ t * (μA - μB) := by
  induction t with
  | zero => simp [meansAfter]
  | succ t ih =>
    show nextMean σ d _ (poolMean w _ _) - nextMean σ d _ (poolMean w _ _) = _
    rw [large_pool_gap_step, ih, pow_succ]
    ring

/-- Below full closure, with entry into the lottery, the gap vanishes. -/
theorem large_pool_gap_vanishes {σ d w μA μB : ℝ} (h0 : 0 < (1 - σ) * d) (h1 : (1 - σ) * d ≤ 1) :
    Tendsto (fun t => (meansAfter σ d w μA μB t).1 - (meansAfter σ d w μA μB t).2) atTop
      (nhds 0) := by
  simp_rw [large_pool_gap]
  have := (tendsto_pow_atTop_nhds_zero_of_lt_one (by linarith : (0 : ℝ) ≤ 1 - (1 - σ) * d)
    (by linarith : 1 - (1 - σ) * d < 1)).mul_const (μA - μB)
  simpa using this

/-- Under full closure the gap persists entire. -/
theorem large_pool_gap_full_closure (d w μA μB : ℝ) (t : ℕ) :
    (meansAfter 1 d w μA μB t).1 - (meansAfter 1 d w μA μB t).2 = μA - μB := by
  rw [large_pool_gap]
  simp

/-- The exact half-life of a gap multiplied by `1 - u` each period. -/
noncomputable def halfLife (u : ℝ) : ℝ := Real.log 2 / (-Real.log (1 - u))

theorem halfLife_halves {u : ℝ} (h0 : 0 < u) (h1 : u < 1) : (1 - u) ^ halfLife u = 1 / 2 := by
  have hpos : 0 < 1 - u := by linarith
  have hlog : Real.log (1 - u) ≠ 0 := (Real.log_neg hpos (by linarith)).ne
  rw [Real.rpow_def_of_pos hpos, halfLife]
  have hlog' : -Real.log (1 - u) ≠ 0 := neg_ne_zero.mpr hlog
  have : Real.log (1 - u) * (Real.log 2 / -Real.log (1 - u)) = -Real.log 2 := by
    field_simp
    ring
  rw [this, Real.exp_neg, Real.exp_log (by norm_num)]
  norm_num

/-- The approximation `ln 2 / u` overstates the exact half-life by at most `ln 2` periods. -/
theorem halfLife_bounds {u : ℝ} (h0 : 0 < u) (h1 : u < 1) :
    Real.log 2 / u - Real.log 2 ≤ halfLife u ∧ halfLife u ≤ Real.log 2 / u := by
  have hpos : 0 < 1 - u := by linarith
  have hl2 : 0 ≤ Real.log 2 := Real.log_nonneg (by norm_num)
  have hup : Real.log (1 - u) ≤ -u := by linarith [Real.log_le_sub_one_of_pos hpos]
  have hlow : 1 - (1 - u)⁻¹ ≤ Real.log (1 - u) := Real.one_sub_inv_le_log_of_pos hpos
  have ha : u ≤ -Real.log (1 - u) := by linarith
  have hb : -Real.log (1 - u) ≤ u / (1 - u) := by
    have : (1 - u)⁻¹ - 1 = u / (1 - u) := by field_simp
    linarith
  unfold halfLife
  constructor
  · have e : Real.log 2 / u - Real.log 2 = Real.log 2 / (u / (1 - u)) := by
      field_simp
      ring
    rw [e]
    exact div_le_div_of_nonneg_left hl2 (by linarith) hb
  · exact div_le_div_of_nonneg_left hl2 h0 ha

/-- Control: the approximation is not exact. A gap halved each period (`u = 1/2`) has
half-life one period, while `ln 2 / u` gives about `1.39`. -/
theorem control_halfLife_approximation_overstates :
    halfLife (1 / 2) = 1 ∧ 1 < Real.log 2 / (1 / 2) := by
  constructor
  · unfold halfLife
    have : Real.log (1 - 1 / 2) = -Real.log 2 := by
      rw [show (1 : ℝ) - 1 / 2 = 2⁻¹ by norm_num, Real.log_inv]
    rw [this, neg_neg, div_self (Real.log_pos (by norm_num)).ne']
  · have := Real.log_two_gt_d9
    rw [div_div_eq_mul_div]
    linarith

/-- Control: the large-pool recursion is not the small-pool one. With every worker entering and
no closure the large pool removes the gap in one period, while in the pool of two and two the
gap reverses to a third (M51), because nobody can draw the own former job. -/
theorem control_small_pool_differs : (1 - (1 - 0) * 1 : ℝ) ≠ persistence 0 := by
  unfold persistence
  norm_num

end LargePool

/-! ### 17d: closure as inbreeding homophily -/

section Homophily

/-- In a large pool, the chance that an entrant of a group with share `w` among entrants
receives a former job of the own group: `σ` from the own lottery, and otherwise the group's
share of the joint lottery. -/
noncomputable def ownGroupChance (σ w : ℝ) : ℝ := σ + (1 - σ) * w

/-- Closure is Coleman's inbreeding homophily index of the own-group chance, for every group
share. -/
theorem closure_is_inbreeding_homophily {σ w : ℝ} (hw : w < 1) :
    CurrariniJacksonPin2009.inbreeding (ownGroupChance σ w) w = σ := by
  unfold CurrariniJacksonPin2009.inbreeding ownGroupChance
  have : (1 - w) ≠ 0 := by linarith
  field_simp
  ring

/-- In a joint lottery of `n` members of whom a share `w` belong to the entrant's group, the
own-group share among the other members tends to `w`. -/
theorem joint_own_share_tendsto (w : ℝ) :
    Tendsto (fun n : ℕ => (w * n - 1) / ((n : ℝ) - 1)) atTop (nhds w) := by
  have hden : Tendsto (fun n : ℕ => (n : ℝ) - 1) atTop atTop :=
    tendsto_atTop_add_const_right _ (-1) tendsto_natCast_atTop_atTop
  have hfrac : Tendsto (fun n : ℕ => (w - 1) / ((n : ℝ) - 1)) atTop (nhds 0) :=
    tendsto_const_nhds.div_atTop hden
  have hsum := hfrac.const_add w
  rw [add_zero] at hsum
  refine hsum.congr' ?_
  filter_upwards [eventually_ge_atTop 2] with n hn
  have : (n : ℝ) - 1 ≠ 0 := by
    have : (2 : ℝ) ≤ n := by exact_mod_cast hn
    linarith
  field_simp
  ring

/-- So the index of the own-group chance tends to `σ` as the joint lottery grows. -/
theorem inbreeding_tendsto_closure {σ w : ℝ} (hw : w < 1) :
    Tendsto (fun n : ℕ => CurrariniJacksonPin2009.inbreeding
      (σ + (1 - σ) * ((w * n - 1) / ((n : ℝ) - 1))) w) atTop (nhds σ) := by
  have h := (((joint_own_share_tendsto w).const_mul (1 - σ)).const_add σ |>.sub_const w).div_const
    (1 - w)
  have e := closure_is_inbreeding_homophily (σ := σ) hw
  unfold CurrariniJacksonPin2009.inbreeding ownGroupChance at e
  rw [e] at h
  exact h

/-- Control: closure is not the own-group share. With `σ = 1/2`, a group of a quarter of the
entrants draws own-group jobs with chance `5/8` and a group of half with chance `3/4`; the
index is `1/2` for both. -/
theorem control_share_differs_index_same :
    ownGroupChance (1 / 2) (1 / 4) = 5 / 8 ∧ ownGroupChance (1 / 2) (1 / 2) = 3 / 4 ∧
      CurrariniJacksonPin2009.inbreeding (ownGroupChance (1 / 2) (1 / 4)) (1 / 4) = 1 / 2 ∧
      CurrariniJacksonPin2009.inbreeding (ownGroupChance (1 / 2) (1 / 2)) (1 / 2) = 1 / 2 := by
  refine ⟨?_, ?_, closure_is_inbreeding_homophily (by norm_num),
    closure_is_inbreeding_homophily (by norm_num)⟩ <;>
    (unfold ownGroupChance; norm_num)

end Homophily

end Isc
