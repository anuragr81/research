import ClosureMap

open Filter Topology

/-! Coate and Loury (1993), the structure Paper A relies on, in the paper's own
notation. `π` is the employer's prior that a group member is qualified, `fq`
and `fu` the signal densities of qualified and unqualified workers at `θ`,
`φ = fu / fq` the likelihood ratio, `xq` and `xu` the employer's gain and
loss, and `r = xq / xu`. -/

namespace CoateLoury1993

/-- Eq. (1), Bayes' rule for the posterior that the worker is qualified. -/
noncomputable def xi (π fq fu : ℝ) : ℝ := π * fq / (π * fq + (1 - π) * fu)

/-- Eq. (1), second form. -/
theorem xi_eq {π fq fu : ℝ} (hπ0 : 0 < π) (hπ1 : π < 1) (hq : 0 < fq) (hu : 0 ≤ fu) :
    xi π fq fu = 1 / (1 + (1 - π) / π * (fu / fq)) := by
  unfold xi
  have h1 : 0 < 1 - π := by linarith
  have hden : 0 < π * fq + (1 - π) * fu := by positivity
  field_simp

/-- The assignment rule. Task one is optimal exactly when
`r ≥ [(1 − π)/π] φ(θ)`. -/
theorem assign_iff {π fq fu xq xu : ℝ} (hπ0 : 0 < π) (hπ1 : π < 1) (hq : 0 < fq)
    (hu : 0 ≤ fu) (hxu : 0 < xu) :
    0 ≤ xi π fq fu * xq - (1 - xi π fq fu) * xu ↔ (1 - π) / π * (fu / fq) ≤ xq / xu := by
  unfold xi
  have h1 : 0 < 1 - π := by linarith
  have hden : 0 < π * fq + (1 - π) * fu := by positivity
  have e : π * fq / (π * fq + (1 - π) * fu) * xq - (1 - π * fq / (π * fq + (1 - π) * fu)) * xu
      = (π * fq * xq - (1 - π) * fu * xu) / (π * fq + (1 - π) * fu) := by
    field_simp; ring
  rw [e, le_div_iff₀ hden, zero_mul, div_mul_div_comm, div_le_div_iff₀ (by positivity) hxu]
  constructor <;> intro h <;> nlinarith [h]

/-- The EE locus in the proof of Proposition 1, `π = φ/(r + φ)`. -/
theorem EE_locus {π r φ : ℝ} (hπ0 : 0 < π) (hr : 0 < r) (hφ : 0 ≤ φ) :
    r = (1 - π) / π * φ ↔ π = φ / (r + φ) := by
  have hrφ : 0 < r + φ := by linarith
  constructor
  · intro h
    rw [eq_div_iff hrφ.ne', h]
    field_simp
    ring
  · intro h
    have hφ0 : φ ≠ 0 := by
      intro h0; rw [h0, zero_div] at h; linarith
    rw [h]
    field_simp
    ring

/-- The no-investment belief is self-confirming when `G 0 = 0` and the benefit
of investing at the standard `1` is `0`. -/
theorem zero_self_confirming {G β : ℝ → ℝ} (hG : G 0 = 0) (hβ : β 1 = 0) :
    G (β 1) = 0 := by
  rw [hβ, hG]

/-! ### The uniform example, eq. (9)

Under affirmative action, beliefs about B's follow
`π_b^{t+1} = [(1 − π_ℓ)/(1 − π_b^t)] π_ℓ`. -/

/-- Eq. (9). -/
noncomputable def g (πl π : ℝ) : ℝ := (1 - πl) / (1 - π) * πl

/-- The same map with its argument capped at `π_ℓ`, continuous on all of `ℝ`. -/
noncomputable def gc (πl π : ℝ) : ℝ := (1 - πl) / (1 - min π πl) * πl

theorem g_sub_self {πl π : ℝ} (h : π ≠ 1) :
    g πl π - π = (π - πl) * (π - (1 - πl)) / (1 - π) := by
  unfold g
  have : 1 - π ≠ 0 := sub_ne_zero.2 (Ne.symm h)
  field_simp
  ring

/-- The two equilibrium beliefs about B's, the color-blind `π_ℓ` and the
patronizing `1 − π_ℓ`. -/
theorem g_fixed_iff {πl π : ℝ} (h : π ≠ 1) :
    g πl π = π ↔ π = πl ∨ π = 1 - πl := by
  have hs := g_sub_self (πl := πl) h
  have h1 : 1 - π ≠ 0 := sub_ne_zero.2 (Ne.symm h)
  constructor
  · intro hf
    rw [hf, sub_self, eq_comm, div_eq_zero_iff] at hs
    rcases hs with hs | hs
    · rcases mul_eq_zero.1 hs with h2 | h2
      · left; linarith
      · right; linarith
    · exact absurd hs h1
  · rintro (rfl | rfl)
    · unfold g; field_simp
    · unfold g
      have : (1 : ℝ) - (1 - πl) = πl := by ring
      rw [this]
      have : πl ≠ 0 := by
        intro h0; apply h; rw [h0]; ring
      field_simp

theorem gc_continuous {πl : ℝ} (h : πl < 1) : Continuous (gc πl) := by
  unfold gc
  apply Continuous.mul _ continuous_const
  apply Continuous.div continuous_const
  · exact continuous_const.sub (continuous_id.min continuous_const)
  · intro x
    have : min x πl ≤ πl := min_le_right _ _
    linarith

theorem gc_eq_g {πl π : ℝ} (h : π ≤ πl) : gc πl π = g πl π := by
  unfold gc g; rw [min_eq_left h]

/-- Starting below `π_ℓ`, with `π_ℓ > 1/2`, beliefs about B's converge to the
patronizing equilibrium `1 − π_ℓ`, as the paper states after eq. (9) for the
start `π_c`. The compiler showed `0 ≤ π₀` is not needed. -/
theorem converges_to_patronizing {πl π₀ : ℝ} (hl : 1 / 2 < πl) (hl1 : πl < 1)
    (h1 : π₀ < πl) :
    Tendsto (fun n => (g πl)^[n] π₀) atTop (𝓝 (1 - πl)) := by
  have hcont := gc_continuous hl1
  have hp : 1 - πl < πl := by linarith
  -- the capped map is nondecreasing below π_ℓ
  have mono : ∀ x y, x ≤ y → y ≤ πl → gc πl x ≤ gc πl y := by
    intro x y hxy hy
    rw [gc_eq_g (le_trans hxy hy), gc_eq_g hy]
    unfold g
    have h1 : 0 < 1 - y := by linarith
    have h2 : 0 < 1 - x := by linarith
    have hnum : 0 ≤ (1 - πl) * πl := by nlinarith
    rw [div_mul_eq_mul_div, div_mul_eq_mul_div]
    exact div_le_div_of_nonneg_left hnum h1 (by linarith)
  have hfix : gc πl (1 - πl) = 1 - πl := by
    rw [gc_eq_g hp.le, g_fixed_iff (by linarith)]; right; rfl
  have sub : ∀ x, x ≤ πl → gc πl x - x = (x - πl) * (x - (1 - πl)) / (1 - x) := by
    intro x hx; rw [gc_eq_g hx, g_sub_self (by linarith)]
  have orbit : ∀ n, (g πl)^[n] π₀ = (gc πl)^[n] π₀ ∧ (gc πl)^[n] π₀ < πl := by
    intro n
    induction n with
    | zero => exact ⟨rfl, h1⟩
    | succ n ih =>
      obtain ⟨he, hlt⟩ := ih
      rw [Function.iterate_succ_apply', Function.iterate_succ_apply', he, gc_eq_g hlt.le]
      refine ⟨rfl, ?_⟩
      have := sub _ hlt.le
      rw [gc_eq_g hlt.le] at this
      rcases le_or_gt ((gc πl)^[n] π₀) (1 - πl) with hle | hgt
      · have hm := mono _ _ hle hp.le
        rw [hfix, gc_eq_g hlt.le] at hm
        linarith
      · have hneg : (gc πl)^[n] π₀ - πl < 0 := by linarith
        have hpos : 0 < (gc πl)^[n] π₀ - (1 - πl) := by linarith
        have hd : 0 < 1 - (gc πl)^[n] π₀ := by linarith
        have : (( gc πl)^[n] π₀ - πl) * ((gc πl)^[n] π₀ - (1 - πl)) / (1 - (gc πl)^[n] π₀) < 0 :=
          div_neg_of_neg_of_pos (mul_neg_of_neg_of_pos hneg hpos) hd
        linarith
  have key : Tendsto (fun n => (gc πl)^[n] π₀) atTop (𝓝 (1 - πl)) := by
    rcases le_or_gt π₀ (1 - πl) with hle | hgt
    · apply ClosureMap.tendsto_up hcont hle hfix
      · intro x y _ hxy hy; exact mono x y hxy (le_trans hy hp.le)
      · intro x hx hxp
        have hs := sub x (by linarith)
        have : 0 < (x - πl) * (x - (1 - πl)) / (1 - x) :=
          div_pos (mul_pos_of_neg_of_neg (by linarith) (by linarith)) (by linarith)
        linarith
    · apply ClosureMap.tendsto_down hcont hgt.le hfix
      · intro x y _ hxy hy; exact mono x y hxy (le_trans hy h1.le)
      · intro x hx hxp
        have hs := sub x (by linarith)
        have : (x - πl) * (x - (1 - πl)) / (1 - x) < 0 :=
          div_neg_of_neg_of_pos (mul_neg_of_neg_of_pos (by linarith) (by linarith)) (by linarith)
        linarith
  have : (fun n => (g πl)^[n] π₀) = fun n => (gc πl)^[n] π₀ := funext fun n => (orbit n).1
  rw [this]; exact key

/-- Control: the start matters. From `π_ℓ` itself beliefs stay at the
color-blind equilibrium. -/
theorem control_start_at_colorblind {πl : ℝ} (hl : 1 / 2 < πl) (hl1 : πl < 1) :
    ∀ n, (g πl)^[n] πl = πl ∧ πl ≠ 1 - πl := by
  intro n
  refine ⟨?_, by intro h; linarith⟩
  induction n with
  | zero => rfl
  | succ n ih =>
    rw [Function.iterate_succ_apply', ih, g_fixed_iff (by linarith)]; left; rfl

/-- Control: without `π_ℓ > 1/2` the limit is not `1 − π_ℓ`. At `π_ℓ = 1/4`
the orbit of `0` stays at or below `1/4`, so it cannot converge to `3/4`. -/
theorem control_half_needed :
    ¬ Tendsto (fun n => (g (1 / 4))^[n] 0) atTop (𝓝 (1 - 1 / 4)) := by
  have inv : ∀ n, 0 ≤ (g (1 / 4))^[n] 0 ∧ (g (1 / 4))^[n] 0 ≤ 1 / 4 := by
    intro n
    induction n with
    | zero => norm_num
    | succ n ih =>
      obtain ⟨a, b⟩ := ih
      rw [Function.iterate_succ_apply']
      have hg : ∀ x, g (1 / 4) x = (3 / 16) / (1 - x) := by intro x; unfold g; ring
      rw [hg]
      have hd : 0 < 1 - (g (1 / 4))^[n] 0 := by linarith
      constructor
      · positivity
      · rw [div_le_iff₀ hd]; nlinarith
  intro h
  have hle : ∀ᶠ n in atTop, (g (1 / 4))^[n] 0 ≤ 1 / 4 := Filter.Eventually.of_forall fun n => (inv n).2
  have := le_of_tendsto h hle
  norm_num at this

end CoateLoury1993
