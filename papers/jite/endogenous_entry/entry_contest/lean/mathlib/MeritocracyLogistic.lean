import Mathlib

/-!
# Morgan, Tumlinson and Vardy (2018), "The Limits of Meritocracy", IMF WP/18/231, Section III

An exact instance of their Proposition 1, in their notation. A unit mass of homogeneous agents
chooses log-output `x` or drops out. A mass `m ∈ (0, 1)` of prizes of value `V > 0` goes to those
whose measured log-performance `x + ε` beats the standard `θ`; `ε/σ` has CDF `F` and density `f`,
and `1/σ` is meritocracy. The cost is `c(x) = k exp(α x)`, their leading example `C(X) = k X^α`.
With `z = (θ − x)/σ`, the system on p.13 in the mixed region is

* FOC (3): `(V/σ) f(z) = c'(x) = α c(x)`;
* PC, binding when participation `γ ∈ (0, 1)`: `V (1 − F(z)) = c(x)`;
* MCC: `γ (1 − F(z)) = m`;
* SOC (4), p.11: `c''/c' > −(1/σ) f'(z)/f(z)`.

FOC over PC gives the hazard `f(z)/(1 − F(z)) = σ α`. For the logistic `F`, `f = F (1 − F)` and
the hazard is `F`, so `F(z) = σ α` and `γ = m/(1 − σ α)`: participation rises with the noise `σ`,
tends to `m` as `σ → 0`, and reaches `1` at `σ = (1 − m)/α`. The SOC holds there because
`f'/f = 1 − 2F`, so it reads `α > −(1/σ)(1 − 2σα)`, that is `σ α < 1`. In the code the
participation probability is `p` and the standard enters only through `z`.
-/

open Real Set Filter Topology

namespace MeritocracyLogistic

/-! ## The logistic distribution -/

/-- The logistic CDF. -/
noncomputable def logistic (z : ℝ) : ℝ := 1 / (1 + Real.exp (-z))

/-- The logistic density, written as `F (1 − F)`. -/
noncomputable def logisticDensity (z : ℝ) : ℝ := logistic z * (1 - logistic z)

/-- **The density is the derivative of the CDF.** -/
theorem hasDerivAt_logistic (z : ℝ) : HasDerivAt logistic (logisticDensity z) z := by
  have h1 : HasDerivAt (fun x => 1 + Real.exp (-x)) (-Real.exp (-z)) z := by
    have := ((Real.hasDerivAt_exp (-z)).comp z (hasDerivAt_neg z)).const_add 1
    simpa using this
  have hne : (1 + Real.exp (-z)) ≠ 0 := by positivity
  have h2 := h1.inv hne
  have e : logistic = (fun x => 1 + Real.exp (-x))⁻¹ := by
    funext x
    simp [logistic]
  rw [e]
  convert h2 using 1
  simp only [logisticDensity, logistic]
  field_simp
  ring

/-- The logistic CDF takes values in `(0, 1)`. -/
theorem logistic_mem (z : ℝ) : logistic z ∈ Ioo 0 1 := by
  have he := Real.exp_pos (-z)
  constructor
  · unfold logistic
    positivity
  · unfold logistic
    rw [div_lt_one (by positivity)]
    linarith

theorem one_sub_logistic_pos (z : ℝ) : 0 < 1 - logistic z := by
  linarith [(logistic_mem z).2]

/-- **The logistic hazard is the CDF itself**: `f(z)/(1 − F(z)) = F(z)`. -/
theorem hazard_eq (z : ℝ) : logisticDensity z / (1 - logistic z) = logistic z := by
  unfold logisticDensity
  rw [mul_div_assoc, div_self (one_sub_logistic_pos z).ne', mul_one]

/-! ## Equilibrium algebra in the mixed region -/

/-- **FOC, binding PC and market clearing pin down the mixed region**: `F(z) = σ α` and
`p = m/(1 − σ α)`. -/
theorem mixed_region (V σ α m k y z p : ℝ) (hV : 0 < V) (hσ : 0 < σ)
    (hfoc : V / σ * logisticDensity z = α * (k * Real.exp (α * y)))
    (hpc : V * (1 - logistic z) = k * Real.exp (α * y))
    (hmcc : p * (1 - logistic z) = m) :
    logistic z = σ * α ∧ p = m / (1 - σ * α) := by
  have h1 := one_sub_logistic_pos z
  have hX : V * (1 - logistic z) ≠ 0 := (mul_pos hV h1).ne'
  have key : logistic z / σ * (V * (1 - logistic z)) = α * (V * (1 - logistic z)) := by
    have e : logistic z / σ * (V * (1 - logistic z)) = V / σ * logisticDensity z := by
      rw [logisticDensity]
      ring
    rw [e, hfoc, hpc]
  have hL : logistic z / σ = α := mul_right_cancel₀ hX key
  have hL' : logistic z = σ * α := by
    rw [div_eq_iff hσ.ne'] at hL
    rw [hL]
    ring
  refine ⟨hL', ?_⟩
  rw [← hL', eq_div_iff h1.ne']
  exact hmcc

/-! ## Comparative statics of participation -/

/-- Participation in the mixed region, `p = m/(1 − σ α)`. -/
noncomputable def participation (m α σ : ℝ) : ℝ := m / (1 - σ * α)

/-- **Participation rises with noise** on `σ ∈ (0, 1/α)`. -/
theorem participation_strictMonoOn {m α : ℝ} (hm : 0 < m) (hα : 0 < α) :
    StrictMonoOn (fun σ => participation m α σ) (Ioo 0 (1 / α)) := by
  intro a _ b hb hab
  simp only [participation]
  have hb' : b * α < 1 := by
    have := hb.2
    rwa [lt_div_iff₀ hα] at this
  have hab' : a * α < b * α := mul_lt_mul_of_pos_right hab hα
  exact div_lt_div_of_pos_left hm (by linarith) (by linarith)

/-- The critical noise level `σ̄ = (1 − m)/α` lies in `(0, 1/α)` for `m ∈ (0, 1)`. -/
theorem critical_mem {m α : ℝ} (hm0 : 0 < m) (hm : m < 1) (hα : 0 < α) :
    (1 - m) / α ∈ Ioo 0 (1 / α) :=
  ⟨div_pos (by linarith) hα, (div_lt_div_iff_of_pos_right hα).2 (by linarith)⟩

/-- **Everyone participates at the critical noise level** `σ̄ = (1 − m)/α`. -/
theorem participation_at_critical {m α : ℝ} (hm0 : 0 < m) (hα : 0 < α) :
    participation m α ((1 - m) / α) = 1 := by
  unfold participation
  have h : 1 - (1 - m) / α * α = m := by
    field_simp
    ring
  rw [h, div_self hm0.ne']

/-- **Participation tends to `m` as noise vanishes.** -/
theorem participation_tendsto (m α : ℝ) :
    Tendsto (fun σ => participation m α σ) (𝓝[>] 0) (𝓝 m) := by
  have hg : Tendsto (fun σ : ℝ => 1 - σ * α) (𝓝 0) (𝓝 1) := by
    have := (continuous_const.sub (continuous_id.mul continuous_const)).tendsto (0 : ℝ)
      (f := fun σ : ℝ => 1 - id σ * α)
    simpa using this
  have h := (tendsto_const_nhds (x := m)).div hg one_ne_zero
  simpa [participation] using h.mono_left nhdsWithin_le_nhds

/-- **Participation is below one** below the critical noise level. -/
theorem participation_lt_one {m α : ℝ} (hm0 : 0 < m) (hα : 0 < α) (σ : ℝ)
    (hσ : σ < (1 - m) / α) : participation m α σ < 1 := by
  unfold participation
  have h1 : σ * α < 1 - m := by rwa [lt_div_iff₀ hα] at hσ
  rw [div_lt_one (by linarith)]
  linarith

/-- **In the mixed region `p ∈ (m, 1)`.** -/
theorem participation_mem_Ioo {m α : ℝ} (hm0 : 0 < m) (hα : 0 < α) (σ : ℝ) (hσ0 : 0 < σ)
    (hσ : σ < (1 - m) / α) : participation m α σ ∈ Ioo m 1 := by
  refine ⟨?_, participation_lt_one hm0 hα σ hσ⟩
  have h1 : σ * α < 1 - m := by rwa [lt_div_iff₀ hα] at hσ
  have h2 : 0 < m * (σ * α) := by positivity
  unfold participation
  rw [lt_div_iff₀ (by linarith)]
  linarith

/-- The slope of participation in noise, `m α/(1 − σ α)²`. -/
theorem hasDerivAt_participation (m α σ : ℝ) (h : 1 - σ * α ≠ 0) :
    HasDerivAt (fun s => participation m α s) (m * α / (1 - σ * α) ^ 2) σ := by
  have hg : HasDerivAt (fun s : ℝ => 1 - s * α) (-α) σ := by
    simpa using ((hasDerivAt_id σ).mul_const α).const_sub 1
  have := (hasDerivAt_const σ m).div hg h
  convert this using 1
  ring

/-- The slope is positive wherever `σ α ≠ 1`. -/
theorem participation_deriv_pos {m α σ : ℝ} (hm : 0 < m) (hα : 0 < α) (h : 1 - σ * α ≠ 0) :
    0 < m * α / (1 - σ * α) ^ 2 := by
  have := pow_pos (abs_pos.2 h) 2
  rw [sq_abs] at this
  positivity

/-! ## Existence: the mixed region is not vacuous -/

/-- **An explicit mixed-region solution.** For `V, σ, α, m, k > 0` with `σ α < 1 − m`, the cutoff
`z = log(σ α/(1 − σ α))`, the log-output `y = log(V (1 − σ α)/k)/α` and the participation
`p = m/(1 − σ α)` satisfy FOC, binding PC and market clearing, and `p ∈ (m, 1)`. -/
theorem mixed_region_exists (V σ α m k : ℝ) (hV : 0 < V) (hσ : 0 < σ) (hα : 0 < α) (hm : 0 < m)
    (hk : 0 < k) (hσα : σ * α < 1 - m) :
    ∃ z y p : ℝ, z = Real.log (σ * α / (1 - σ * α)) ∧ y = Real.log (V * (1 - σ * α) / k) / α ∧
      p = m / (1 - σ * α) ∧
      V / σ * logisticDensity z = α * (k * Real.exp (α * y)) ∧
      V * (1 - logistic z) = k * Real.exp (α * y) ∧
      p * (1 - logistic z) = m ∧ p ∈ Ioo m 1 := by
  have ha0 : 0 < σ * α := mul_pos hσ hα
  have ha1 : 0 < 1 - σ * α := by linarith
  have hlog : logistic (Real.log (σ * α / (1 - σ * α))) = σ * α := by
    unfold logistic
    rw [Real.exp_neg, Real.exp_log (div_pos ha0 ha1)]
    field_simp
    ring
  have hexp : k * Real.exp (α * (Real.log (V * (1 - σ * α) / k) / α)) = V * (1 - σ * α) := by
    have e : α * (Real.log (V * (1 - σ * α) / k) / α) = Real.log (V * (1 - σ * α) / k) := by
      field_simp
    rw [e, Real.exp_log (by positivity)]
    field_simp
  refine ⟨_, _, _, rfl, rfl, rfl, ?_, ?_, ?_, ?_⟩
  · rw [hexp, logisticDensity, hlog]
    field_simp
  · rw [hexp, hlog]
  · rw [hlog]
    field_simp
  · exact participation_mem_Ioo hm hα σ hσ (by rw [lt_div_iff₀ hα]; linarith)

/-! ## Controls -/

/-- **Control: a constant hazard breaks the mixed region.** If `f = λ (1 − F)` then FOC and
binding PC force `σ α = λ`, whatever the cutoff. -/
theorem control_constant_hazard (V σ α lam Fz C : ℝ) (hσ : 0 < σ) (hC : 0 < C)
    (hfoc : V / σ * (lam * (1 - Fz)) = α * C) (hpc : V * (1 - Fz) = C) :
    σ * α = lam := by
  have key : lam / σ * C = α * C := by
    rw [← hfoc, ← hpc]
    ring
  have h := mul_right_cancel₀ hC.ne' key
  rw [div_eq_iff hσ.ne'] at h
  rw [h]
  ring

/-- **Control: with constant hazard `λ` and `σ α ≠ λ` there is no mixed-region solution**, for
any CDF `F` and density `f = λ (1 − F)`. The increasing hazard of the logistic is load-bearing. -/
theorem control_constant_hazard_no_solution (F f : ℝ → ℝ) (lam V σ α k : ℝ)
    (hf : ∀ z, f z = lam * (1 - F z)) (hσ : 0 < σ) (hk : 0 < k) (hne : σ * α ≠ lam) :
    ¬ ∃ y z : ℝ, V / σ * f z = α * (k * Real.exp (α * y)) ∧
      V * (1 - F z) = k * Real.exp (α * y) := by
  rintro ⟨y, z, hfoc, hpc⟩
  rw [hf] at hfoc
  exact hne (control_constant_hazard V σ α lam (F z) _ hσ (by positivity) hfoc hpc)

/-- **Control: participation rises with noise**, at `α = 1`, `m = 1/2`: `5/9` at `σ = 1/10`
and `5/8` at `σ = 1/5`. -/
theorem control_entry_direction :
    participation (1 / 2) 1 (1 / 10) = 5 / 9 ∧ participation (1 / 2) 1 (1 / 5) = 5 / 8 ∧
      participation (1 / 2) 1 (1 / 10) < participation (1 / 2) 1 (1 / 5) := by
  refine ⟨?_, ?_, ?_⟩ <;> norm_num [participation]

/-! ## The second-order condition (4) -/

/-- The logistic density is positive. -/
theorem logisticDensity_pos (z : ℝ) : 0 < logisticDensity z :=
  mul_pos (logistic_mem z).1 (one_sub_logistic_pos z)

/-- **The derivative of the logistic density** is `f (1 − 2F)`, so `f'/f = 1 − 2F`. -/
theorem hasDerivAt_logisticDensity (z : ℝ) :
    HasDerivAt logisticDensity (logisticDensity z * (1 - 2 * logistic z)) z := by
  have h := (hasDerivAt_logistic z).mul ((hasDerivAt_logistic z).const_sub 1)
  have e : logisticDensity = fun x => logistic x * (1 - logistic x) := rfl
  rw [e]
  convert h using 1
  unfold logisticDensity
  ring

/-- **The SOC (4) holds in the mixed region**: with `F(z) = σ α` and `σ α < 1`,
`−(1/σ) f'(z)/f(z) < α = c''/c'`. -/
theorem soc_mixed (σ α z : ℝ) (hσ : 0 < σ) (hz : logistic z = σ * α) (h1 : σ * α < 1) :
    -(1 / σ) * (logisticDensity z * (1 - 2 * logistic z) / logisticDensity z) < α := by
  have hd := (logisticDensity_pos z).ne'
  rw [mul_div_right_comm, div_self hd, one_mul, hz]
  have : -(1 / σ) * (1 - 2 * (σ * α)) = 2 * α - 1 / σ := by
    field_simp
    ring
  rw [this]
  have hα : α < 1 / σ := by
    rw [lt_div_iff₀ hσ]
    linarith
  linarith

/-- **Control: the SOC fails once `σ α ≥ 1`**, so the bound `σ α < 1` is load-bearing. -/
theorem control_soc_fails (σ α z : ℝ) (hσ : 0 < σ) (hz : logistic z = σ * α) (h1 : 1 ≤ σ * α) :
    ¬ (-(1 / σ) * (logisticDensity z * (1 - 2 * logistic z) / logisticDensity z) < α) := by
  have hd := (logisticDensity_pos z).ne'
  rw [mul_div_right_comm, div_self hd, one_mul, hz, not_lt]
  have : -(1 / σ) * (1 - 2 * (σ * α)) = 2 * α - 1 / σ := by
    field_simp
    ring
  rw [this]
  have hα : 1 / σ ≤ α := by
    rw [div_le_iff₀ hσ]
    linarith
  linarith

end MeritocracyLogistic
