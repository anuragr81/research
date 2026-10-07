import Mathlib
import EntryContestModel

open MeasureTheory ProbabilityTheory Real Set Filter Topology

set_option linter.unusedSectionVars false

namespace EntryContestKappa

/-- The utility cost of entry, `κ(w) = u(w) − u(w − c)`. -/
def kappa (u : ℝ → ℝ) (c w : ℝ) : ℝ := u w - u (w - c)

section Burden

/-- **Strict concavity gives strict burden-monotonicity.** For `c < a < b`, the slope from
    `a − c` falls when its far end moves from `a` to `b`, and the slope into `b` falls when its
    near end moves from `a − c` to `b − c`. -/
theorem burden_strictAnti_of_strictConcave (u : ℝ → ℝ) (c : ℝ) (hc : 0 < c)
    (hu : StrictConcaveOn ℝ (Ioi 0) u) : StrictAntiOn (kappa u c) (Ioi c) := by
  intro a ha b hb hab
  rw [mem_Ioi] at ha hb
  have ha0 : a - c ∈ Ioi (0 : ℝ) := by rw [mem_Ioi]; linarith
  have hb0 : b - c ∈ Ioi (0 : ℝ) := by rw [mem_Ioi]; linarith
  have haI : a ∈ Ioi (0 : ℝ) := by rw [mem_Ioi]; linarith
  have hbI : b ∈ Ioi (0 : ℝ) := by rw [mem_Ioi]; linarith
  have h1 := hu.secant_strict_mono ha0 haI hbI (ne_of_gt (by linarith)) (ne_of_gt (by linarith))
    hab
  have h2 := hu.secant_strict_mono hbI ha0 hb0 (ne_of_lt (by linarith)) (ne_of_lt (by linarith))
    (by linarith : a - c < b - c)
  have hL : (0 : ℝ) < b - (a - c) := by linarith
  have q1 : (u a - u (a - c)) / (a - (a - c)) = (u a - u (a - c)) / c := by
    rw [show a - (a - c) = c by ring]
  have q2 : (u (b - c) - u b) / (b - c - b) = (u b - u (b - c)) / c := by
    rw [div_eq_div_iff (ne_of_lt (by linarith)) hc.ne']
    ring
  have q3 : (u (a - c) - u b) / (a - c - b) = (u b - u (a - c)) / (b - (a - c)) := by
    rw [div_eq_div_iff (ne_of_lt (by linarith)) hL.ne']
    ring
  rw [q1] at h1
  rw [q2, q3] at h2
  unfold kappa
  exact (div_lt_div_iff_of_pos_right hc).mp (h2.trans h1)

theorem tendsto_sub_nhdsGT (c : ℝ) : Tendsto (fun w => w - c) (𝓝[>] c) (𝓝[>] 0) := by
  refine tendsto_nhdsWithin_iff.mpr ⟨?_, eventually_nhdsWithin_of_forall (fun w hw => ?_)⟩
  · exact ((continuous_sub_right c).tendsto' c 0 (sub_self c)).mono_left nhdsWithin_le_nhds
  · exact sub_pos.mpr (mem_Ioi.mp hw)

theorem tendsto_add_nhdsGT (c : ℝ) : Tendsto (fun v => v + c) (𝓝[>] 0) (𝓝[>] c) := by
  refine tendsto_nhdsWithin_iff.mpr ⟨?_, eventually_nhdsWithin_of_forall (fun v hv => ?_)⟩
  · exact ((continuous_add_right c).tendsto' 0 c (zero_add c)).mono_left nhdsWithin_le_nhds
  · exact lt_add_of_pos_left c hv

/-- **The divergence primitive is `u(0+) = −∞`.** With `u` continuous at `c`, the cost of
    entry diverges as wealth falls to `c` exactly when utility falls without bound as residual
    wealth falls to 0. -/
theorem kappa_diverges_iff (u : ℝ → ℝ) (c : ℝ) (hu : ContinuousAt u c) :
    Tendsto (kappa u c) (𝓝[>] c) atTop ↔ Tendsto u (𝓝[>] 0) atBot := by
  have hcont : Tendsto u (𝓝[>] c) (𝓝 (u c)) := hu.tendsto.mono_left nhdsWithin_le_nhds
  constructor
  · intro h
    have h1 : Tendsto (fun w => u (w - c)) (𝓝[>] c) atBot := by
      refine (hcont.add_atBot (tendsto_neg_atTop_atBot.comp h)).congr (fun w => ?_)
      simp [kappa]
    exact (h1.comp (tendsto_add_nhdsGT c)).congr (fun v => by simp)
  · intro h
    have h1 : Tendsto (fun w => u (w - c)) (𝓝[>] c) atBot := h.comp (tendsto_sub_nhdsGT c)
    refine (hcont.add_atTop (tendsto_neg_atBot_atTop.comp h1)).congr (fun w => ?_)
    simp [kappa, sub_eq_add_neg]

/-- A utility that stays bounded as residual wealth falls to 0 violates the divergence
    primitive. -/
theorem not_diverges_of_continuousWithinAt_zero (u : ℝ → ℝ) (c : ℝ) (hu : ContinuousAt u c)
    (h0 : ContinuousWithinAt u (Ioi 0) 0) : ¬ Tendsto (kappa u c) (𝓝[>] c) atTop := by
  rw [kappa_diverges_iff u c hu]
  exact not_tendsto_atBot_of_tendsto_nhds h0.tendsto

end Burden

section Families

/-- Log utility. -/
theorem log_burden_strictAnti (c : ℝ) (hc : 0 < c) : StrictAntiOn (kappa log c) (Ioi c) :=
  burden_strictAnti_of_strictConcave log c hc strictConcaveOn_log_Ioi

theorem log_kappa_diverges (c : ℝ) (hc : 0 < c) : Tendsto (kappa log c) (𝓝[>] c) atTop :=
  (kappa_diverges_iff log c (continuousAt_log hc.ne')).mpr tendsto_log_nhdsGT_zero

/-- CRRA utility with coefficient `γ ≠ 1`, `u(x) = x^(1−γ)/(1−γ)`. -/
noncomputable def crra (γ : ℝ) (x : ℝ) : ℝ := x ^ (1 - γ) / (1 - γ)

theorem crra_hasDerivAt (γ : ℝ) (hγ : γ ≠ 1) (x : ℝ) (hx : 0 < x) :
    HasDerivAt (crra γ) (x ^ (-γ)) x := by
  have h := (hasDerivAt_rpow_const (p := 1 - γ) (Or.inl hx.ne')).div_const (1 - γ)
  have hne : (1 : ℝ) - γ ≠ 0 := sub_ne_zero.mpr (Ne.symm hγ)
  have e : (1 - γ) * x ^ (1 - γ - 1) / (1 - γ) = x ^ (-γ) := by
    rw [show (1 : ℝ) - γ - 1 = -γ by ring, mul_div_cancel_left₀ _ hne]
  rw [e] at h
  exact h

theorem crra_strictConcave (γ : ℝ) (hγ0 : 0 < γ) (hγ : γ ≠ 1) :
    StrictConcaveOn ℝ (Ioi 0) (crra γ) := by
  refine StrictAntiOn.strictConcaveOn_of_deriv (convex_Ioi 0)
    (fun x hx => (crra_hasDerivAt γ hγ x hx).continuousAt.continuousWithinAt) ?_
  rw [interior_Ioi]
  intro x hx y hy hxy
  rw [(crra_hasDerivAt γ hγ x hx).deriv, (crra_hasDerivAt γ hγ y hy).deriv]
  exact rpow_lt_rpow_of_neg hx hxy (by linarith)

theorem crra_burden_strictAnti (γ : ℝ) (hγ0 : 0 < γ) (hγ : γ ≠ 1) (c : ℝ) (hc : 0 < c) :
    StrictAntiOn (kappa (crra γ) c) (Ioi c) :=
  burden_strictAnti_of_strictConcave (crra γ) c hc (crra_strictConcave γ hγ0 hγ)

theorem crra_continuousAt (γ : ℝ) (c : ℝ) (hc : 0 < c) : ContinuousAt (crra γ) c :=
  (continuousAt_rpow_const c (1 - γ) (Or.inl hc.ne')).div_const (1 - γ)

/-- For `γ > 1`, CRRA utility falls without bound at 0, so the cost diverges. -/
theorem crra_kappa_diverges (γ : ℝ) (hγ : 1 < γ) (c : ℝ) (hc : 0 < c) :
    Tendsto (kappa (crra γ) c) (𝓝[>] c) atTop := by
  rw [kappa_diverges_iff (crra γ) c (crra_continuousAt γ c hc)]
  have hq : 0 < γ - 1 := by linarith
  have hpow : Tendsto (fun x : ℝ => x ^ (1 - γ)) (𝓝[>] 0) atTop := by
    refine ((tendsto_rpow_atTop hq).comp tendsto_inv_nhdsGT_zero).congr'
      (eventually_nhdsWithin_of_forall (fun x hx => ?_))
    have hx0 : (0 : ℝ) ≤ x := le_of_lt hx
    simp only [Function.comp_apply]
    rw [inv_rpow hx0, ← rpow_neg hx0, neg_sub]
  have h1 := tendsto_neg_atTop_atBot.comp (hpow.atTop_div_const hq)
  refine h1.congr (fun x => ?_)
  have hne1 : γ - 1 ≠ 0 := hq.ne'
  have hne2 : (1 : ℝ) - γ ≠ 0 := by linarith
  simp only [Function.comp_apply, crra]
  field_simp
  ring

/-- For `0 < γ < 1`, CRRA utility is bounded at 0, so the divergence primitive fails. This
    family includes the square root. -/
theorem crra_kappa_not_diverges (γ : ℝ) (hγ1 : γ < 1) (c : ℝ) (hc : 0 < c) :
    ¬ Tendsto (kappa (crra γ) c) (𝓝[>] c) atTop := by
  refine not_diverges_of_continuousWithinAt_zero (crra γ) c (crra_continuousAt γ c hc) ?_
  exact ((continuous_rpow_const (by linarith : (0 : ℝ) ≤ 1 - γ)).continuousAt.div_const
    (1 - γ)).continuousWithinAt

/-- The separating example of `PROOFS.tex` (Claim BM), `u(x) = √x + ε sin(kx)`, is continuous
    at 0, so it violates the divergence primitive for every `ε`, `k` and `c > 0`. -/
theorem bm_example_not_diverges (ε k c : ℝ) :
    ¬ Tendsto (kappa (fun x => Real.sqrt x + ε * Real.sin (k * x)) c) (𝓝[>] c) atTop := by
  have hcont : Continuous (fun x => Real.sqrt x + ε * Real.sin (k * x)) := by fun_prop
  exact not_diverges_of_continuousWithinAt_zero _ c hcont.continuousAt
    hcont.continuousAt.continuousWithinAt

end Families

section Spread

/-- The linear spread about the pivot `x0` with factor `lam`. -/
def spread (x0 lam w : ℝ) : ℝ := x0 + lam * (w - x0)

variable (κ : ℝ → ℝ) (c x0 : ℝ) (w Δ : ℕ → ℝ)

/-- After the spread `lam`, the challenger at index `j` (the `(j+1)`-th richest) enters when
    its wealth exceeds the fee `c` and its cost is within the gain at its index. -/
def Enters (lam : ℝ) (j : ℕ) : Prop :=
  c < spread x0 lam (w j) ∧ κ (spread x0 lam (w j)) ≤ Δ j

open Classical in
/-- The number of entrants among `n` challengers after the spread `lam`. -/
noncomputable def count (n : ℕ) (lam : ℝ) : ℕ :=
  ((Finset.range n).filter (fun j => Enters κ c x0 w Δ lam j)).card

theorem spread_one (x0 v : ℝ) : spread x0 1 v = v := by unfold spread; ring

theorem spread_mono_w (x0 lam : ℝ) (hlam : 0 ≤ lam) (a b : ℝ) (hab : a ≤ b) :
    spread x0 lam a ≤ spread x0 lam b := by
  unfold spread
  nlinarith

/-- **The entry set is a prefix**, at every spread factor `lam ≥ 0`. -/
theorem enters_downward (hκ : AntitoneOn κ (Ioi c)) (hw : Antitone w) (hΔ : Antitone Δ)
    (lam : ℝ) (hlam : 0 ≤ lam) (i j : ℕ) (hij : i ≤ j) (hj : Enters κ c x0 w Δ lam j) :
    Enters κ c x0 w Δ lam i := by
  obtain ⟨hcj, hkj⟩ := hj
  have hs := spread_mono_w x0 lam hlam (w j) (w i) (hw hij)
  have hci : c < spread x0 lam (w i) := lt_of_lt_of_le hcj hs
  exact ⟨hci, (hκ hcj hci hs).trans (hkj.trans (hΔ hij))⟩

/-- **The marginal entrant exits under a strong enough spread.** Below the pivot, a large
    `lam` pushes wealth into the band above `c` where the cost exceeds the gain, or to `c` and
    below, where the fee cannot be paid. -/
theorem exit_below_pivot (hdiv : Tendsto κ (𝓝[>] c) atTop) (j : ℕ) (hwj : w j < x0) :
    ∃ lamBar, 1 ≤ lamBar ∧ ∀ lam, lamBar < lam → ¬ Enters κ c x0 w Δ lam j := by
  have hev : ∀ᶠ v in 𝓝[>] c, Δ j < κ v := hdiv.eventually (eventually_gt_atTop (Δ j))
  obtain ⟨u, hu, hsub⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp hev
  have hd : 0 < x0 - w j := by linarith
  refine ⟨max 1 ((x0 - u) / (x0 - w j)), le_max_left _ _, fun lam hlam hent => ?_⟩
  have h1 : (x0 - u) / (x0 - w j) < lam := lt_of_le_of_lt (le_max_right _ _) hlam
  rw [div_lt_iff₀ hd] at h1
  have hlt : spread x0 lam (w j) < u := by
    unfold spread
    have e : lam * (w j - x0) = -(lam * (x0 - w j)) := by ring
    linarith
  have hgt := hsub ⟨hent.1, hlt⟩
  exact absurd hent.2 (not_le.mpr hgt)

open Classical in
/-- **P9 strictness.** When some entrant at `lam = 1` is below the pivot, the count after a
    strong enough linear spread is strictly smaller. -/
theorem p9_strict (hκ : AntitoneOn κ (Ioi c)) (hdiv : Tendsto κ (𝓝[>] c) atTop)
    (hw : Antitone w) (hΔ : Antitone Δ) (n j : ℕ) (hjn : j < n)
    (hj : Enters κ c x0 w Δ 1 j) (hwj : w j < x0) :
    ∃ lamBar, 1 ≤ lamBar ∧ ∀ lam, lamBar < lam → count κ c x0 w Δ n lam < count κ c x0 w Δ n 1 := by
  obtain ⟨lamBar, h1, hexit⟩ := exit_below_pivot κ c x0 w Δ hdiv j hwj
  refine ⟨lamBar, h1, fun lam hlam => ?_⟩
  have hlam0 : 0 ≤ lam := by linarith
  have hafter : count κ c x0 w Δ n lam ≤ j := by
    unfold count
    calc ((Finset.range n).filter (fun i => Enters κ c x0 w Δ lam i)).card
        ≤ (Finset.range j).card := by
          refine Finset.card_le_card (fun i hi => ?_)
          rw [Finset.mem_filter] at hi
          rw [Finset.mem_range]
          by_contra hij
          exact hexit lam hlam
            (enters_downward κ c x0 w Δ hκ hw hΔ lam hlam0 j i (not_lt.mp hij) hi.2)
      _ = j := Finset.card_range j
  have hbefore : j + 1 ≤ count κ c x0 w Δ n 1 := by
    unfold count
    calc j + 1 = (Finset.range (j + 1)).card := (Finset.card_range _).symm
      _ ≤ ((Finset.range n).filter (fun i => Enters κ c x0 w Δ 1 i)).card := by
          refine Finset.card_le_card (fun i hi => ?_)
          rw [Finset.mem_range] at hi
          rw [Finset.mem_filter, Finset.mem_range]
          exact ⟨by omega,
            enters_downward κ c x0 w Δ hκ hw hΔ 1 zero_le_one i j (by omega) hj⟩
  omega

open Classical in
/-- **P9 monotonicity.** When some entrant at `lam = 1` is below the pivot, the count does not
    rise with `lam`. -/
theorem p9_monotone (hκ : AntitoneOn κ (Ioi c)) (hw : Antitone w) (hΔ : Antitone Δ)
    (n j : ℕ) (hj : Enters κ c x0 w Δ 1 j) (hwj : w j < x0)
    (lam1 lam2 : ℝ) (h1 : 1 ≤ lam1) (h12 : lam1 ≤ lam2) :
    count κ c x0 w Δ n lam2 ≤ count κ c x0 w Δ n lam1 := by
  unfold count
  refine Finset.card_le_card (fun i hi => ?_)
  rw [Finset.mem_filter] at hi ⊢
  refine ⟨hi.1, ?_⟩
  rcases le_or_gt x0 (w i) with hi0 | hi0
  · have hij : i ≤ j := by
      by_contra h
      have := hw (le_of_lt (not_le.mp h))
      linarith
    obtain ⟨hc1, hk1⟩ := enters_downward κ c x0 w Δ hκ hw hΔ 1 zero_le_one i j hij hj
    rw [spread_one] at hc1 hk1
    have hup : w i ≤ spread x0 lam1 (w i) := by
      unfold spread
      nlinarith
    have hc' : c < spread x0 lam1 (w i) := lt_of_lt_of_le hc1 hup
    exact ⟨hc', (hκ hc1 hc' hup).trans hk1⟩
  · obtain ⟨hc2, hk2⟩ := hi.2
    have hdown : spread x0 lam2 (w i) ≤ spread x0 lam1 (w i) := by
      unfold spread
      nlinarith
    have hc' : c < spread x0 lam1 (w i) := lt_of_lt_of_le hc2 hdown
    exact ⟨hc', (hκ hc2 hc' hdown).trans hk2⟩

end Spread

section Model

open EntryContestModel

variable (V : ℝ) (α β C : Measure ℝ) [IsProbabilityMeasure α] [IsProbabilityMeasure β]
  [IsProbabilityMeasure C] [NoAtoms α] [NoAtoms β] [NoAtoms C]

theorem DeltaUpTo_antitone (hV : 0 ≤ V) (Q : ℕ) : Antitone (DeltaUpTo V α β C Q) :=
  antitone_nat_of_succ_le (fun n => DeltaUpTo_step V α β C hV Q n)

open Classical in
/-- **P9 strictness from the primitives.** The gain is the model's `Δ`, built from the score
    laws, and the cost comes from any `u` strictly concave on the positive reals, continuous at
    `c`, with `u(0+) = −∞`. -/
theorem p9_strict_model (hV : 0 ≤ V) (Q : ℕ) (u : ℝ → ℝ) (c : ℝ) (hc : 0 < c)
    (hu : StrictConcaveOn ℝ (Ioi 0) u) (huc : ContinuousAt u c)
    (hu0 : Tendsto u (𝓝[>] 0) atBot) (x0 : ℝ) (w : ℕ → ℝ) (hw : Antitone w)
    (n j : ℕ) (hjn : j < n) (hj : Enters (kappa u c) c x0 w (DeltaUpTo V α β C Q) 1 j)
    (hwj : w j < x0) :
    ∃ lamBar, 1 ≤ lamBar ∧ ∀ lam, lamBar < lam →
      count (kappa u c) c x0 w (DeltaUpTo V α β C Q) n lam
        < count (kappa u c) c x0 w (DeltaUpTo V α β C Q) n 1 :=
  p9_strict (kappa u c) c x0 w (DeltaUpTo V α β C Q)
    (burden_strictAnti_of_strictConcave u c hc hu).antitoneOn
    ((kappa_diverges_iff u c huc).mpr hu0) hw (DeltaUpTo_antitone V α β C hV Q) n j hjn hj hwj

end Model

end EntryContestKappa
