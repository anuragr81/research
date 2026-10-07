import Mathlib
import Equilibrium
import Anonymity

open MeasureTheory ProbabilityTheory Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestSpread

open EntryContestModel EntryContestP1P2 EntryContestP6P7 EntryContestKappa EntryContestAnon
  EntryContestPMU EntryContestPMUWitness EntryContestRefute

/-! **Wealth spreads and the entrant count.** Profiles are sorted with the richest challenger at
rank 0. A challenger at rank `j` enters when its wealth exceeds the fee `c` and its cost is within
the gain at its rank, which is the support-floor rule the author adopted for P9. The count `k`
of a profile is the length of the prefix of entrants, with rank `k` failing when `k < Q`. -/

section Counts

variable (κ : ℝ → ℝ) (c : ℝ) (Δ : ℕ → ℝ) (Q : ℕ)

/-- Rank `j` of the profile `w` enters. -/
def EntersP (w : ℕ → ℝ) (j : ℕ) : Prop := c < w j ∧ κ (w j) ≤ Δ j

/-- `k` is the entrant count of the profile `w` among `Q` challengers. -/
def IsCount (w : ℕ → ℝ) (k : ℕ) : Prop :=
  k ≤ Q ∧ (∀ j, j < k → EntersP κ c Δ w j) ∧ (k < Q → ¬ EntersP κ c Δ w k)

/-- In a sorted profile the entrants form a prefix. -/
theorem enters_down (hκ : AntitoneOn κ (Ioi c)) (hΔ : Antitone Δ) (w : ℕ → ℝ) (hw : Antitone w)
    (i j : ℕ) (hij : i ≤ j) (hj : EntersP κ c Δ w j) : EntersP κ c Δ w i := by
  obtain ⟨hc, hk⟩ := hj
  have hwi : w j ≤ w i := hw hij
  have hci : c < w i := lt_of_lt_of_le hc hwi
  exact ⟨hci, (hκ hc hci hwi).trans (hk.trans (hΔ hij))⟩

theorem count_ge_of_enters (w' : ℕ → ℝ) (k k' : ℕ) (hk : k ≤ Q)
    (hk' : IsCount κ c Δ Q w' k') (h : ∀ j, j < k → EntersP κ c Δ w' j) : k ≤ k' := by
  by_contra hlt
  push_neg at hlt
  exact hk'.2.2 (by omega) (h k' hlt)

theorem count_le_of_fails (w' : ℕ → ℝ) (k k' : ℕ) (hk' : IsCount κ c Δ Q w' k')
    (h : ¬ EntersP κ c Δ w' k) : k' ≤ k := by
  by_contra hlt
  push_neg at hlt
  exact h (hk'.2.1 k hlt)

/-- **The margin condition, rise branch.** If the marginal entrant's wealth does not fall, the
    count does not fall. Nothing is assumed about any other rank. -/
theorem margin_rise (hκ : AntitoneOn κ (Ioi c)) (hΔ : Antitone Δ) (w w' : ℕ → ℝ)
    (hw' : Antitone w') (k k' : ℕ) (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q w' k')
    (hm : 1 ≤ k → w (k - 1) ≤ w' (k - 1)) : k ≤ k' := by
  rcases Nat.eq_zero_or_pos k with h0 | hpos
  · omega
  have hmarg : EntersP κ c Δ w' (k - 1) := by
    obtain ⟨hc, hκk⟩ := hk.2.1 (k - 1) (by omega)
    have h1 := hm hpos
    have hc' : c < w' (k - 1) := lt_of_lt_of_le hc h1
    exact ⟨hc', (hκ hc hc' h1).trans hκk⟩
  exact count_ge_of_enters κ c Δ Q w' k k' hk.1 hk'
    (fun j hj => enters_down κ c Δ hκ hΔ w' hw' j (k - 1) (by omega) hmarg)

/-- **The margin condition, fall branch.** If the first outsider's wealth does not rise, the
    count does not rise. Nothing is assumed about any other rank. -/
theorem margin_fall (hκ : AntitoneOn κ (Ioi c)) (w w' : ℕ → ℝ) (k k' : ℕ)
    (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q w' k') (hm : k < Q → w' k ≤ w k) :
    k' ≤ k := by
  rcases lt_or_ge k Q with hkQ | hkQ
  · refine count_le_of_fails κ c Δ Q w' k k' hk' (fun ⟨hc', hκ'⟩ => ?_)
    have h1 := hm hkQ
    have hc : c < w k := lt_of_lt_of_le hc' h1
    exact hk.2.2 hkQ ⟨hc, (hκ hc' hc h1).trans hκ'⟩
  · exact le_trans hk'.1 hkQ

/-- The rank-by-rank tail condition of `PROOFS.tex`, rise branch, as a special case. -/
theorem tail_rise (hκ : AntitoneOn κ (Ioi c)) (hΔ : Antitone Δ) (w w' : ℕ → ℝ)
    (hw' : Antitone w') (k k' : ℕ) (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q w' k')
    (h : ∀ j, j < k → w j ≤ w' j) : k ≤ k' :=
  margin_rise κ c Δ Q hκ hΔ w w' hw' k k' hk hk' (fun h1 => h (k - 1) (by omega))

/-- The rank-by-rank tail condition of `PROOFS.tex`, fall branch, as a special case. -/
theorem tail_fall (hκ : AntitoneOn κ (Ioi c)) (w w' : ℕ → ℝ) (k k' : ℕ)
    (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q w' k')
    (h : ∀ j, k ≤ j → j < Q → w' j ≤ w j) : k' ≤ k :=
  margin_fall κ c Δ Q hκ w w' k k' hk hk' (fun hkQ => h k le_rfl hkQ)

end Counts

section Pivot

/-! **P9-gen.** A monotone map `T` is a pivot-spread about `x0` when it moves wealth up above
`x0` and down below it. -/

def IsPivotSpread (T : ℝ → ℝ) (x0 : ℝ) : Prop :=
  Monotone T ∧ (∀ v, x0 ≤ v → v ≤ T v) ∧ (∀ v, v ≤ x0 → T v ≤ v)

variable (κ : ℝ → ℝ) (c : ℝ) (Δ : ℕ → ℝ) (Q : ℕ)

/-- **P9-gen, rise branch.** With the marginal entrant at or above the pivot, the count does not
    fall. -/
theorem pivot_rise (hκ : AntitoneOn κ (Ioi c)) (hΔ : Antitone Δ) (T : ℝ → ℝ) (x0 : ℝ)
    (hT : IsPivotSpread T x0) (w : ℕ → ℝ) (hw : Antitone w) (k k' : ℕ)
    (hk : IsCount κ c Δ Q w k) (hk' : IsCount κ c Δ Q (fun j => T (w j)) k')
    (hm : 1 ≤ k → x0 ≤ w (k - 1)) : k ≤ k' :=
  margin_rise κ c Δ Q hκ hΔ w (fun j => T (w j)) (fun _ _ hij => hT.1 (hw hij)) k k' hk hk'
    (fun h1 => hT.2.1 _ (hm h1))

/-- **P9-gen, fall branch.** With the marginal entrant at or below the pivot, the count does not
    rise. -/
theorem pivot_fall (hκ : AntitoneOn κ (Ioi c)) (T : ℝ → ℝ) (x0 : ℝ) (hT : IsPivotSpread T x0)
    (w : ℕ → ℝ) (hw : Antitone w) (k k' : ℕ) (hk : IsCount κ c Δ Q w k)
    (hk' : IsCount κ c Δ Q (fun j => T (w j)) k') (h1 : 1 ≤ k) (hm : w (k - 1) ≤ x0) :
    k' ≤ k :=
  margin_fall κ c Δ Q hκ w (fun j => T (w j)) k k' hk hk'
    (fun _ => hT.2.2 _ (le_trans (hw (by omega : k - 1 ≤ k)) hm))

/-- **P9.** The linear spread `x0 + λ(w − x0)` with `λ ≥ 1` is a pivot-spread about `x0`. -/
theorem spread_isPivot (x0 lam : ℝ) (hlam : 1 ≤ lam) : IsPivotSpread (spread x0 lam) x0 := by
  refine ⟨fun a b hab => ?_, fun v hv => ?_, fun v hv => ?_⟩
  · unfold spread
    nlinarith
  · unfold spread
    nlinarith
  · unfold spread
    nlinarith

end Pivot

section Band

/-! **The band.** With `d j = w' j − w j`, the margin condition settles the direction unless the
displacement is negative at the marginal entrant and positive at the first outsider. -/

/-- The margin `k` lies in the band of the displacement `d`. -/
def InBand (d : ℕ → ℝ) (k : ℕ) : Prop := 1 ≤ k ∧ d (k - 1) < 0 ∧ 0 < d k

/-- Outside the band one branch of the margin condition holds. -/
theorem branch_of_not_band (d : ℕ → ℝ) (k : ℕ) (h : ¬ InBand d k) :
    (1 ≤ k → 0 ≤ d (k - 1)) ∨ d k ≤ 0 := by
  by_contra h'
  push_neg at h'
  obtain ⟨⟨h1, h2⟩, h3⟩ := h'
  exact h ⟨h1, h2, h3⟩

/-- A displacement that changes sign once, from nonnegative to nonpositive down the ranks, never
    puts a margin in the band. -/
theorem not_band_of_single_crossing (d : ℕ → ℝ) (hd : ∀ i j, i ≤ j → 0 < d j → 0 ≤ d i)
    (k : ℕ) : ¬ InBand d k := by
  rintro ⟨h1, h2, h3⟩
  have := hd (k - 1) k (by omega) h3
  linarith

/-- A pivot-spread of a sorted profile changes sign once. -/
theorem pivot_single_crossing (T : ℝ → ℝ) (x0 : ℝ) (hT : IsPivotSpread T x0) (w : ℕ → ℝ)
    (hw : Antitone w) (i j : ℕ) (hij : i ≤ j) (hj : 0 < T (w j) - w j) :
    0 ≤ T (w i) - w i := by
  have hxj : x0 < w j := by
    by_contra h
    push_neg at h
    have := hT.2.2 _ h
    linarith
  have := hT.2.1 _ (le_of_lt (lt_of_lt_of_le hxj (hw hij)))
  linarith

end Band

section Witness

/-! **Inside the band the signs settle nothing.** Three challengers, CRRA utility with `γ = 2`
and `c = 1`. Before the spread the wealth is `(4, 27/10, 21/10)` and the count is 2. Two spreads
with the same displacement signs `(0, −, +)` move the count to 3 and to 1. -/

/-- The cost of entry, `κ(w) = 1/(w − 1) − 1/w`. -/
noncomputable def κc (w : ℝ) : ℝ := kappa (crra 2) 1 w

theorem κc_eq (w : ℝ) : κc w = (w - 1)⁻¹ - w⁻¹ := by
  simp only [κc, kappa, crra_two]
  ring

theorem κc_anti : AntitoneOn κc (Ioi 1) :=
  (crra_burden_strictAnti 2 (by norm_num) (by norm_num) 1 one_pos).antitoneOn

/-- Profiles, richest first, padded with zeros beyond the three challengers. -/
noncomputable def wPre (j : ℕ) : ℝ := if j = 0 then 4 else if j = 1 then 27 / 10 else if j = 2 then 21 / 10 else 0
noncomputable def wUp (j : ℕ) : ℝ := if j = 0 then 4 else if j = 1 then 53 / 20 else if j = 2 then 13 / 5 else 0
noncomputable def wDown (j : ℕ) : ℝ := if j = 0 then 4 else if j = 1 then 9 / 4 else if j = 2 then 11 / 5 else 0

theorem antitone_of_three (w : ℕ → ℝ) (a b c' : ℝ) (h0 : w 0 = a) (h1 : w 1 = b) (h2 : w 2 = c')
    (hrest : ∀ j, 3 ≤ j → w j = 0) (hab : b ≤ a) (hbc : c' ≤ b) (hc : 0 ≤ c') : Antitone w := by
  intro i j hij
  have key : ∀ n, w n = if n = 0 then a else if n = 1 then b else if n = 2 then c' else 0 := by
    intro n
    rcases n with _ | _ | _ | n
    · simp [h0]
    · simp [h1]
    · simp [h2]
    · simp [hrest (n + 3) (by omega)]
  rw [key i, key j]
  split_ifs <;> first | omega | linarith

theorem wPre_anti : Antitone wPre :=
  antitone_of_three wPre 4 (27 / 10) (21 / 10) (by simp [wPre]) (by simp [wPre]) (by simp [wPre])
    (fun j hj => by
      simp [wPre, show j ≠ 0 by omega, show j ≠ 1 by omega, show j ≠ 2 by omega]) (by norm_num) (by norm_num) (by norm_num)
theorem wUp_anti : Antitone wUp :=
  antitone_of_three wUp 4 (53 / 20) (13 / 5) (by simp [wUp]) (by simp [wUp]) (by simp [wUp])
    (fun j hj => by
      simp [wUp, show j ≠ 0 by omega, show j ≠ 1 by omega, show j ≠ 2 by omega]) (by norm_num) (by norm_num) (by norm_num)
theorem wDown_anti : Antitone wDown :=
  antitone_of_three wDown 4 (9 / 4) (11 / 5) (by simp [wDown]) (by simp [wDown]) (by simp [wDown])
    (fun j hj => by
      simp [wDown, show j ≠ 0 by omega, show j ≠ 1 by omega, show j ≠ 2 by omega]) (by norm_num) (by norm_num) (by norm_num)

/-- The counts for any gain schedule close enough to the benchmark `Δ(m) = 1/(m + 2)`. -/
theorem witness_counts (Δ : ℕ → ℝ) (h0 : 1 / 12 ≤ Δ 0) (h1 : 400 / 1749 ≤ Δ 1)
    (h1' : Δ 1 < 16 / 45) (h2 : 25 / 104 ≤ Δ 2) (h2' : Δ 2 < 100 / 231) :
    IsCount κc 1 Δ 3 wPre 2 ∧ IsCount κc 1 Δ 3 wUp 3 ∧ IsCount κc 1 Δ 3 wDown 1 := by
  have k4 : κc 4 = 1 / 12 := by rw [κc_eq]; norm_num
  refine ⟨⟨by norm_num, fun j hj => ?_, fun _ => ?_⟩, ⟨le_rfl, fun j hj => ?_, fun h => ?_⟩,
    ⟨by norm_num, fun j hj => ?_, fun _ => ?_⟩⟩
  · interval_cases j
    · exact ⟨by simp [wPre], by simp [wPre]; rw [k4]; exact h0⟩
    · refine ⟨by simp [wPre]; norm_num, ?_⟩
      simp [wPre]
      rw [κc_eq]
      norm_num
      linarith
  · rintro ⟨_, h⟩
    simp [wPre] at h
    rw [κc_eq] at h
    norm_num at h
    linarith
  · interval_cases j
    · exact ⟨by simp [wUp], by simp [wUp]; rw [k4]; exact h0⟩
    · refine ⟨by simp [wUp]; norm_num, ?_⟩
      simp [wUp]
      rw [κc_eq]
      norm_num
      linarith
    · refine ⟨by simp [wUp]; norm_num, ?_⟩
      simp [wUp]
      rw [κc_eq]
      norm_num
      linarith
  · exact absurd h (lt_irrefl 3)
  · interval_cases j
    exact ⟨by simp [wDown], by simp [wDown]; rw [k4]; exact h0⟩
  · rintro ⟨_, h⟩
    simp [wDown] at h
    rw [κc_eq] at h
    norm_num at h
    linarith

/-- Both spreads put the margin in the band, with the same signs at every rank. -/
theorem witness_signs :
    wUp 0 - wPre 0 = 0 ∧ wDown 0 - wPre 0 = 0
      ∧ wUp 1 - wPre 1 < 0 ∧ wDown 1 - wPre 1 < 0
      ∧ 0 < wUp 2 - wPre 2 ∧ 0 < wDown 2 - wPre 2
      ∧ InBand (fun j => wUp j - wPre j) 2 ∧ InBand (fun j => wDown j - wPre j) 2 := by
  simp only [InBand, wUp, wDown, wPre]
  norm_num

/-- **At the benchmark gains, the same signs move the count up in one case and down in the
    other.** -/
theorem band_both_directions :
    IsCount κc 1 (fun m => 1 / ((m : ℝ) + 2)) 3 wPre 2
      ∧ IsCount κc 1 (fun m => 1 / ((m : ℝ) + 2)) 3 wUp 3
      ∧ IsCount κc 1 (fun m => 1 / ((m : ℝ) + 2)) 3 wDown 1 :=
  witness_counts _ (by norm_num) (by norm_num) (by norm_num) (by norm_num) (by norm_num)

/-- **Inside the primitives.** For every small `μ > 0`, with `r` and `s` uniform and the
    incumbent investing, the model's gains give the same three counts. -/
theorem band_both_directions_positive :
    ∀ᶠ μ in 𝓝[>] 0,
      IsCount κc 1 (DeltaUpTo 1 (investorLaw μ unif unif) (nonInvestorLaw μ unif)
          (investorLaw μ unif unif) 3) 3 wPre 2
      ∧ IsCount κc 1 (DeltaUpTo 1 (investorLaw μ unif unif) (nonInvestorLaw μ unif)
          (investorLaw μ unif unif) 3) 3 wUp 3
      ∧ IsCount κc 1 (DeltaUpTo 1 (investorLaw μ unif unif) (nonInvestorLaw μ unif)
          (investorLaw μ unif unif) 3) 3 wDown 1 := by
  have hs : unif (Iio 0) = 0 := by
    have h := cdf_unif_of_mem 0 le_rfl zero_le_one
    rw [cdf_eq_real, measureReal_def] at h
    refine measure_mono_null Iio_subset_Iic_self ?_
    exact ((ENNReal.toReal_eq_zero_iff _).mp h).resolve_right (measure_ne_top _ _)
  have lim : ∀ m : ℕ, Tendsto (fun t => Delta 1 (investorLaw t unif unif) (nonInvestorLaw t unif)
      (investorLaw t unif unif) 3 m) (𝓝 0) (𝓝 (1 / ((m : ℝ) + 2))) := fun m => by
    have := p6_limit unif unif hs 1 3 m
    simpa using this
  have e : ∀ t (m : ℕ), m ≤ 2 → DeltaUpTo 1 (investorLaw t unif unif) (nonInvestorLaw t unif)
      (investorLaw t unif unif) 3 m = Delta 1 (investorLaw t unif unif) (nonInvestorLaw t unif)
      (investorLaw t unif unif) 3 m := fun t m hm => by
    unfold DeltaUpTo
    rw [min_eq_left (by omega)]
  have t0 := (lim 0).eventually (eventually_gt_nhds (show (1 : ℝ) / 12 < 1 / ((0 : ℕ) + 2) by norm_num))
  have t1 := (lim 1).eventually (eventually_gt_nhds (show (400 : ℝ) / 1749 < 1 / ((1 : ℕ) + 2) by norm_num))
  have t1' := (lim 1).eventually (eventually_lt_nhds (show 1 / (((1 : ℕ) : ℝ) + 2) < 16 / 45 by norm_num))
  have t2 := (lim 2).eventually (eventually_gt_nhds (show (25 : ℝ) / 104 < 1 / ((2 : ℕ) + 2) by norm_num))
  have t2' := (lim 2).eventually (eventually_lt_nhds (show 1 / (((2 : ℕ) : ℝ) + 2) < 100 / 231 by norm_num))
  filter_upwards [nhdsWithin_le_nhds t0, nhdsWithin_le_nhds t1, nhdsWithin_le_nhds t1',
    nhdsWithin_le_nhds t2, nhdsWithin_le_nhds t2'] with μ a0 a1 a1' a2 a2'
  exact witness_counts _ (by rw [e μ 0 (by norm_num)]; exact a0.le)
    (by rw [e μ 1 (by norm_num)]; exact a1.le) (by rw [e μ 1 (by norm_num)]; exact a1')
    (by rw [e μ 2 (by norm_num)]; exact a2.le) (by rw [e μ 2 (by norm_num)]; exact a2')

/-- A profile `(2.9, 2.8, 2.0)` and a mean-preserving spread of it, `(3.6, 2.2, 1.9)`. -/
noncomputable def wMpsPre (j : ℕ) : ℝ :=
  if j = 0 then 29 / 10 else if j = 1 then 28 / 10 else if j = 2 then 2 else 0
noncomputable def wMpsPost (j : ℕ) : ℝ :=
  if j = 0 then 36 / 10 else if j = 1 then 22 / 10 else if j = 2 then 19 / 10 else 0

/-- **A mean-preserving spread can lower the count with the marginal entrant above the mean.**
    The two profiles have the same total, the second majorizes the first (its partial sums from
    the poorest up are no larger), the marginal entrant is above the mean before the spread, and
    the count falls from 2 to 1 at the benchmark gains. -/
theorem mps_lowers_count :
    (wMpsPre 0 + wMpsPre 1 + wMpsPre 2 = wMpsPost 0 + wMpsPost 1 + wMpsPost 2)
      ∧ wMpsPost 2 ≤ wMpsPre 2 ∧ wMpsPost 2 + wMpsPost 1 ≤ wMpsPre 2 + wMpsPre 1
      ∧ (wMpsPre 0 + wMpsPre 1 + wMpsPre 2) / 3 < wMpsPre 1
      ∧ IsCount κc 1 (fun m => 1 / ((m : ℝ) + 2)) 3 wMpsPre 2
      ∧ IsCount κc 1 (fun m => 1 / ((m : ℝ) + 2)) 3 wMpsPost 1 := by
  refine ⟨by simp [wMpsPre, wMpsPost]; norm_num, by simp [wMpsPre, wMpsPost]; norm_num,
    by simp [wMpsPre, wMpsPost]; norm_num, by simp [wMpsPre]; norm_num,
    ⟨by norm_num, fun j hj => ?_, fun _ => ?_⟩, ⟨by norm_num, fun j hj => ?_, fun _ => ?_⟩⟩
  · interval_cases j
    · refine ⟨by simp [wMpsPre]; norm_num, ?_⟩
      simp [wMpsPre]
      rw [κc_eq]
      norm_num
    · refine ⟨by simp [wMpsPre]; norm_num, ?_⟩
      simp [wMpsPre]
      rw [κc_eq]
      norm_num
  · rintro ⟨_, h⟩
    simp [wMpsPre] at h
    rw [κc_eq] at h
    norm_num at h
  · interval_cases j
    refine ⟨by simp [wMpsPost]; norm_num, ?_⟩
    simp [wMpsPost]
    rw [κc_eq]
    norm_num
  · rintro ⟨_, h⟩
    simp [wMpsPost] at h
    rw [κc_eq] at h
    norm_num at h

/-! **Mean-preserving spreads inside the band.** Four challengers. Before the spread the wealth is
`(4, 14/5, 11/5, 2)` and the count is 2. Two spreads, each preserving the total and majorizing
the original profile, have the displacement signs `(+, −, +, −)` and move the count to 3 and
to 1. -/

noncomputable def mPre (j : ℕ) : ℝ :=
  if j = 0 then 4 else if j = 1 then 14 / 5 else if j = 2 then 11 / 5 else if j = 3 then 2 else 0
noncomputable def mUp (j : ℕ) : ℝ :=
  if j = 0 then 41 / 10 else if j = 1 then 27 / 10 else if j = 2 then 13 / 5 else if j = 3 then 8 / 5
    else 0
noncomputable def mDown (j : ℕ) : ℝ :=
  if j = 0 then 23 / 5 else if j = 1 then 9 / 4 else if j = 2 then 9 / 4 else if j = 3 then 19 / 10
    else 0

theorem antitone_of_four (w : ℕ → ℝ) (a b c' e : ℝ) (h0 : w 0 = a) (h1 : w 1 = b) (h2 : w 2 = c')
    (h3 : w 3 = e) (hrest : ∀ j, 4 ≤ j → w j = 0) (hab : b ≤ a) (hbc : c' ≤ b) (hce : e ≤ c')
    (he : 0 ≤ e) : Antitone w := by
  intro i j hij
  have key : ∀ n, w n =
      if n = 0 then a else if n = 1 then b else if n = 2 then c' else if n = 3 then e else 0 := by
    intro n
    rcases n with _ | _ | _ | _ | n
    · simp [h0]
    · simp [h1]
    · simp [h2]
    · simp [h3]
    · simp [hrest (n + 4) (by omega)]
  rw [key i, key j]
  split_ifs <;> first | omega | linarith

theorem mPre_anti : Antitone mPre :=
  antitone_of_four mPre 4 (14 / 5) (11 / 5) 2 (by simp [mPre]) (by simp [mPre]) (by simp [mPre])
    (by simp [mPre]) (fun j hj => by
      simp [mPre, show j ≠ 0 by omega, show j ≠ 1 by omega, show j ≠ 2 by omega,
        show j ≠ 3 by omega]) (by norm_num) (by norm_num) (by norm_num) (by norm_num)
theorem mUp_anti : Antitone mUp :=
  antitone_of_four mUp (41 / 10) (27 / 10) (13 / 5) (8 / 5) (by simp [mUp]) (by simp [mUp])
    (by simp [mUp]) (by simp [mUp]) (fun j hj => by
      simp [mUp, show j ≠ 0 by omega, show j ≠ 1 by omega, show j ≠ 2 by omega,
        show j ≠ 3 by omega]) (by norm_num) (by norm_num) (by norm_num) (by norm_num)
theorem mDown_anti : Antitone mDown :=
  antitone_of_four mDown (23 / 5) (9 / 4) (9 / 4) (19 / 10) (by simp [mDown]) (by simp [mDown])
    (by simp [mDown]) (by simp [mDown]) (fun j hj => by
      simp [mDown, show j ≠ 0 by omega, show j ≠ 1 by omega, show j ≠ 2 by omega,
        show j ≠ 3 by omega]) (by norm_num) (by norm_num) (by norm_num) (by norm_num)

/-- Both spreads preserve the total, majorize the original profile (partial sums from the poorest
    up are no larger), have the signs `(+, −, +, −)`, and put the margin `k = 2` in the band. -/
theorem mps_band_signs :
    (mUp 0 + mUp 1 + mUp 2 + mUp 3 = mPre 0 + mPre 1 + mPre 2 + mPre 3)
      ∧ mUp 3 ≤ mPre 3 ∧ mUp 3 + mUp 2 ≤ mPre 3 + mPre 2
      ∧ mUp 3 + mUp 2 + mUp 1 ≤ mPre 3 + mPre 2 + mPre 1
      ∧ (mDown 0 + mDown 1 + mDown 2 + mDown 3 = mPre 0 + mPre 1 + mPre 2 + mPre 3)
      ∧ mDown 3 ≤ mPre 3 ∧ mDown 3 + mDown 2 ≤ mPre 3 + mPre 2
      ∧ mDown 3 + mDown 2 + mDown 1 ≤ mPre 3 + mPre 2 + mPre 1
      ∧ 0 < mUp 0 - mPre 0 ∧ mUp 1 - mPre 1 < 0 ∧ 0 < mUp 2 - mPre 2 ∧ mUp 3 - mPre 3 < 0
      ∧ 0 < mDown 0 - mPre 0 ∧ mDown 1 - mPre 1 < 0 ∧ 0 < mDown 2 - mPre 2
      ∧ mDown 3 - mPre 3 < 0
      ∧ InBand (fun j => mUp j - mPre j) 2 ∧ InBand (fun j => mDown j - mPre j) 2 := by
  simp only [InBand, mUp, mDown, mPre]
  norm_num

/-- The counts for any gain schedule close enough to the benchmark `Δ(m) = 1/(m + 2)`. -/
theorem mps_witness_counts (Δ : ℕ → ℝ) (h0 : 1 / 12 ≤ Δ 0) (h1 : 100 / 459 ≤ Δ 1)
    (h1' : Δ 1 < 16 / 45) (h2 : 25 / 104 ≤ Δ 2) (h2' : Δ 2 < 25 / 66) (h3' : Δ 3 < 25 / 24) :
    IsCount κc 1 Δ 4 mPre 2 ∧ IsCount κc 1 Δ 4 mUp 3 ∧ IsCount κc 1 Δ 4 mDown 1 := by
  have k4 : κc 4 = 1 / 12 := by rw [κc_eq]; norm_num
  refine ⟨⟨by norm_num, fun j hj => ?_, fun _ => ?_⟩, ⟨by norm_num, fun j hj => ?_, fun _ => ?_⟩,
    ⟨by norm_num, fun j hj => ?_, fun _ => ?_⟩⟩
  · interval_cases j
    · exact ⟨by simp [mPre], by simp [mPre]; rw [k4]; exact h0⟩
    · refine ⟨by simp [mPre]; norm_num, ?_⟩
      simp [mPre]
      rw [κc_eq]
      norm_num
      linarith
  · rintro ⟨_, h⟩
    simp [mPre] at h
    rw [κc_eq] at h
    norm_num at h
    linarith
  · interval_cases j
    · refine ⟨by simp [mUp]; norm_num, ?_⟩
      simp [mUp]
      rw [κc_eq]
      norm_num
      linarith
    · refine ⟨by simp [mUp]; norm_num, ?_⟩
      simp [mUp]
      rw [κc_eq]
      norm_num
      linarith
    · refine ⟨by simp [mUp]; norm_num, ?_⟩
      simp [mUp]
      rw [κc_eq]
      norm_num
      linarith
  · rintro ⟨_, h⟩
    simp [mUp] at h
    rw [κc_eq] at h
    norm_num at h
    linarith
  · interval_cases j
    refine ⟨by simp [mDown]; norm_num, ?_⟩
    simp [mDown]
    rw [κc_eq]
    norm_num
    linarith
  · rintro ⟨_, h⟩
    simp [mDown] at h
    rw [κc_eq] at h
    norm_num at h
    linarith

/-- **At the benchmark gains, two mean-preserving spreads with the same signs move the count up
    in one case and down in the other.** -/
theorem mps_band_both_directions :
    IsCount κc 1 (fun m => 1 / ((m : ℝ) + 2)) 4 mPre 2
      ∧ IsCount κc 1 (fun m => 1 / ((m : ℝ) + 2)) 4 mUp 3
      ∧ IsCount κc 1 (fun m => 1 / ((m : ℝ) + 2)) 4 mDown 1 :=
  mps_witness_counts _ (by norm_num) (by norm_num) (by norm_num) (by norm_num) (by norm_num)
    (by norm_num)

/-- **Inside the primitives.** For every small `μ > 0`, with `r` and `s` uniform and the
    incumbent investing, the model's gains give the same three counts. -/
theorem mps_band_both_directions_positive :
    ∀ᶠ μ in 𝓝[>] 0,
      IsCount κc 1 (DeltaUpTo 1 (investorLaw μ unif unif) (nonInvestorLaw μ unif)
          (investorLaw μ unif unif) 4) 4 mPre 2
      ∧ IsCount κc 1 (DeltaUpTo 1 (investorLaw μ unif unif) (nonInvestorLaw μ unif)
          (investorLaw μ unif unif) 4) 4 mUp 3
      ∧ IsCount κc 1 (DeltaUpTo 1 (investorLaw μ unif unif) (nonInvestorLaw μ unif)
          (investorLaw μ unif unif) 4) 4 mDown 1 := by
  have hs : unif (Iio 0) = 0 := by
    have h := cdf_unif_of_mem 0 le_rfl zero_le_one
    rw [cdf_eq_real, measureReal_def] at h
    refine measure_mono_null Iio_subset_Iic_self ?_
    exact ((ENNReal.toReal_eq_zero_iff _).mp h).resolve_right (measure_ne_top _ _)
  have lim : ∀ m : ℕ, Tendsto (fun t => Delta 1 (investorLaw t unif unif) (nonInvestorLaw t unif)
      (investorLaw t unif unif) 4 m) (𝓝 0) (𝓝 (1 / ((m : ℝ) + 2))) := fun m => by
    have := p6_limit unif unif hs 1 4 m
    simpa using this
  have e : ∀ t (m : ℕ), m ≤ 3 → DeltaUpTo 1 (investorLaw t unif unif) (nonInvestorLaw t unif)
      (investorLaw t unif unif) 4 m = Delta 1 (investorLaw t unif unif) (nonInvestorLaw t unif)
      (investorLaw t unif unif) 4 m := fun t m hm => by
    unfold DeltaUpTo
    rw [min_eq_left (by omega)]
  have t0 := (lim 0).eventually (eventually_gt_nhds (show (1 : ℝ) / 12 < 1 / ((0 : ℕ) + 2) by norm_num))
  have t1 := (lim 1).eventually (eventually_gt_nhds (show (100 : ℝ) / 459 < 1 / ((1 : ℕ) + 2) by norm_num))
  have t1' := (lim 1).eventually (eventually_lt_nhds (show 1 / (((1 : ℕ) : ℝ) + 2) < 16 / 45 by norm_num))
  have t2 := (lim 2).eventually (eventually_gt_nhds (show (25 : ℝ) / 104 < 1 / ((2 : ℕ) + 2) by norm_num))
  have t2' := (lim 2).eventually (eventually_lt_nhds (show 1 / (((2 : ℕ) : ℝ) + 2) < 25 / 66 by norm_num))
  have t3' := (lim 3).eventually (eventually_lt_nhds (show 1 / (((3 : ℕ) : ℝ) + 2) < 25 / 24 by norm_num))
  filter_upwards [nhdsWithin_le_nhds t0, nhdsWithin_le_nhds t1, nhdsWithin_le_nhds t1',
    nhdsWithin_le_nhds t2, nhdsWithin_le_nhds t2', nhdsWithin_le_nhds t3'] with μ a0 a1 a1' a2 a2' a3'
  exact mps_witness_counts _ (by rw [e μ 0 (by norm_num)]; exact a0.le)
    (by rw [e μ 1 (by norm_num)]; exact a1.le) (by rw [e μ 1 (by norm_num)]; exact a1')
    (by rw [e μ 2 (by norm_num)]; exact a2.le) (by rw [e μ 2 (by norm_num)]; exact a2')
    (by rw [e μ 3 (by norm_num)]; exact a3')

end Witness

end EntryContestSpread
