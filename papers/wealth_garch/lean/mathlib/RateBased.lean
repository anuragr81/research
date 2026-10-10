/-
================================================================================
  A Rate-Based Reference Point for Loss-Averse Capital Management
  -- formal verification

  Companion to proofs_rate_based.tex.  Lean 4 + Mathlib.
================================================================================

SCOPE.  This covers the elementary real-analysis content of the persistence
and saturation results: limits, monotonicity, and the Wald first-passage
relation used in the robustness appendix.  It does NOT attempt the Gaussian
moment identities underlying the EGARCH linearisation (E1a in
verify_rate_based.py) -- those stay with SymPy, on the same scope boundary
as the predecessor project's BoomBust.lean.

MAINTAINED MODEL.  As in proofs_rate_based.tex, `phi_minus` below is the
MAINTAINED-MODEL persistence coefficient (Assumption B2: proportional
control, calibrated), not a first-principles derivation from full
retention.  The exact accounting alternative (additive drift) is covered
separately by `sojourn_time` and its properties, which do not depend on
`phi_minus` at all.
-/

import Mathlib

open Real

namespace RateBased

/-! ############################################################
    ## 1.  Persistence under the maintained model
    ############################################################ -/

/-- Surplus persistence: exact by definition of the payout policy. -/
noncomputable def phi_plus (delta_p : ℝ) : ℝ := 1 - delta_p

/-- Deficit persistence under the MAINTAINED model (SymPy tags P1a-P1e).
    delta_m is calibrated to g(1-b)/b via B1 (rho = g/b) and B2. -/
noncomputable def phi_minus (g b : ℝ) : ℝ := 1 - g * (1 - b) / b

/-- **Exact unit root as g → 0.**  Corresponds to SymPy tag P1b. -/
theorem phi_minus_tendsto_one {b : ℝ} (_hb : 0 < b) :
    Filter.Tendsto (fun g => phi_minus g b) (nhdsWithin 0 (Set.Ioi 0)) (nhds 1) := by
  unfold phi_minus
  have : Filter.Tendsto (fun g : ℝ => g * (1 - b) / b) (nhdsWithin 0 (Set.Ioi 0)) (nhds 0) := by
    have h1 : Filter.Tendsto (fun g : ℝ => g * (1 - b) / b)
        (nhdsWithin 0 (Set.Ioi 0)) (nhds (0 * (1 - b) / b)) := by
      have hcont : Continuous (fun g : ℝ => g * (1 - b) / b) := by fun_prop
      exact (hcont.tendsto 0).mono_left nhdsWithin_le_nhds
    simpa using h1
  simpa using Filter.Tendsto.const_sub 1 this

/-- **Persistence falls strictly as growth rises.**  Corresponds to P1c. -/
theorem phi_minus_strictAnti {b : ℝ} (hb0 : 0 < b) (hb1 : b < 1) :
    StrictAnti (fun g => phi_minus g b) := by
  intro g1 g2 hg
  simp only [phi_minus]
  have h1b : (0:ℝ) < 1 - b := by linarith
  have hstep : g1 * (1 - b) / b < g2 * (1 - b) / b := by
    gcongr
  linarith

/-- The persistence asymmetry $\phi^- - \phi^+$, and its limit as $g\to0$
    (P1d, P1e). -/
noncomputable def persistence_asymmetry (g b delta_p : ℝ) : ℝ :=
  phi_minus g b - phi_plus delta_p

theorem persistence_asymmetry_eq (g b delta_p : ℝ) (hb : b ≠ 0) :
    persistence_asymmetry g b delta_p = delta_p + g - g / b := by
  unfold persistence_asymmetry phi_minus phi_plus
  field_simp
  ring

theorem persistence_asymmetry_tendsto {b delta_p : ℝ} (hb : 0 < b) :
    Filter.Tendsto (fun g => persistence_asymmetry g b delta_p)
      (nhdsWithin 0 (Set.Ioi 0)) (nhds delta_p) := by
  have heq : ∀ g, persistence_asymmetry g b delta_p = delta_p + g - g / b := by
    intro g; exact persistence_asymmetry_eq g b delta_p (ne_of_gt hb)
  simp only [heq]
  have h1 : Filter.Tendsto (fun g : ℝ => delta_p + g - g / b)
      (nhdsWithin 0 (Set.Ioi 0)) (nhds (delta_p + 0 - 0 / b)) := by
    have hcont : Continuous (fun g : ℝ => delta_p + g - g / b) := by fun_prop
    exact (hcont.tendsto 0).mono_left nhdsWithin_le_nhds
  simpa using h1

/-! ############################################################
    ## 2.  Variance saturation -- recursion-independent
    ############################################################ -/

/-- The variance-response function to the buffer level.  c = log(lambda). -/
noncomputable def Vresp (V0 c theta z : ℝ) : ℝ :=
  V0 * Real.exp (-2 * c * Real.tanh (z / theta))

/-- **Exact pointwise ratio identity** (SymPy-confirmed): for EVERY z, not
    merely in a limit, `Vresp(-z)/Vresp(z) = exp(4c*tanh(z/theta))`. This is
    the identity that carries the "recursion-independent" claim -- it is
    pure algebra on `Vresp`, with no transition-dynamics parameter
    (phi_plus, phi_minus, delta_p, delta_m) anywhere in its statement. -/
theorem Vresp_ratio_eq (V0 c theta z : ℝ) (hV0 : 0 < V0) :
    Vresp V0 c theta (-z) / Vresp V0 c theta z
      = Real.exp (4 * c * Real.tanh (z / theta)) := by
  have hB : Real.exp (-2 * c * Real.tanh (z / theta)) ≠ 0 := (Real.exp_pos _).ne'
  unfold Vresp
  rw [show (-z) / theta = -(z / theta) by ring, Real.tanh_neg]
  rw [div_eq_iff (by positivity)]
  rw [show (4:ℝ) * c * Real.tanh (z / theta)
        = (-2 * c * -Real.tanh (z / theta)) - (-2 * c * Real.tanh (z / theta)) by ring,
      Real.exp_sub]
  field_simp

theorem tanh_eq_one_sub (y : ℝ) : Real.tanh y = 1 - 2 / (Real.exp (2 * y) + 1) := by
  rw [Real.tanh_eq, Real.exp_neg, show 2 * y = y + y by ring, Real.exp_add]
  have h : 0 < Real.exp y := Real.exp_pos y
  field_simp
  ring

theorem tendsto_tanh_atTop : Filter.Tendsto Real.tanh Filter.atTop (nhds 1) := by
  have he : Filter.Tendsto (fun y : ℝ => Real.exp (2 * y) + 1) Filter.atTop Filter.atTop :=
    Filter.tendsto_atTop_add_const_right _ 1
      (Real.tendsto_exp_atTop.comp (Filter.tendsto_id.const_mul_atTop two_pos))
  have h2 : Filter.Tendsto (fun y : ℝ => 2 / (Real.exp (2 * y) + 1)) Filter.atTop (nhds 0) :=
    tendsto_const_nhds.div_atTop he
  have h3 := (tendsto_const_nhds (x := (1:ℝ))).sub h2
  simp only [sub_zero] at h3
  exact h3.congr (fun y => (tanh_eq_one_sub y).symm)

/-- **Saturated ratio equals exp(4c) as z → ∞.** Combines the exact
    identity above with the standard fact that tanh → 1 at infinity and
    continuity of exp. Corresponds to SymPy tags S1a-S1b: the limit
    depends only on `Vresp`, i.e. only on sign(z) as |z| → ∞, matching
    S1c's "no dependence on delta_p or delta_m". -/
theorem saturated_ratio_tendsto {V0 c theta : ℝ} (hV0 : 0 < V0) (htheta : 0 < theta) :
    Filter.Tendsto (fun z => Vresp V0 c theta (-z) / Vresp V0 c theta z)
      Filter.atTop (nhds (Real.exp (4 * c))) := by
  have hcongr : (fun z => Vresp V0 c theta (-z) / Vresp V0 c theta z)
      = (fun z => Real.exp (4 * c * Real.tanh (z / theta))) := by
    funext z; exact Vresp_ratio_eq V0 c theta z hV0
  rw [hcongr]
  -- z/theta → ∞ as z → ∞, since theta > 0: dividing by a positive constant
  -- preserves divergence. `tendsto_id` is not available unqualified in
  -- current Mathlib, so `hdiv` is built directly from the defining
  -- characterization of atTop (`Filter.tendsto_atTop`) plus `le_div_iff₀`
  -- (current name; `le_div_iff` was deprecated to it, alongside
  -- `lt_div_iff₀` and `div_lt_iff₀`, in the 2024-10-02 renaming pass).
  have hdiv : Filter.Tendsto (fun z : ℝ => z / theta) Filter.atTop Filter.atTop := by
    rw [Filter.tendsto_atTop]
    intro b
    filter_upwards [Filter.eventually_ge_atTop (b * theta)] with z hz
    rw [le_div_iff₀ htheta]
    linarith
  have htanh : Filter.Tendsto (fun z : ℝ => Real.tanh (z / theta))
      Filter.atTop (nhds 1) :=
    tendsto_tanh_atTop.comp hdiv
  have hmul : Filter.Tendsto (fun z : ℝ => 4 * c * Real.tanh (z / theta))
      Filter.atTop (nhds (4 * c)) := by
    have := htanh.const_mul (4 * c)
    simpa using this
  -- `Real.exp ∘ f` and `fun z => Real.exp (f z)` are definitionally equal
  -- (Function.comp unfolds by delta+beta), so `show` accepts the comp form
  -- of the goal directly -- this avoids depending on `simp` recognising
  -- and unfolding `Function.comp`, which is what broke last time.
  change Filter.Tendsto (Real.exp ∘ fun z : ℝ => 4 * c * Real.tanh (z / theta))
      Filter.atTop (nhds (Real.exp (4 * c)))
  exact (Real.continuous_exp.tendsto (4 * c)).comp hmul

/-- G-analogue: with c = log(lambda), the saturated ratio is exactly
    lambda^4.  Purely algebraic once saturated_ratio_tendsto is in hand. -/
theorem saturated_ratio_lambda4 {lam : ℝ} (hlam : 0 < lam) :
    Real.exp (4 * Real.log lam) = lam ^ 4 := by
  rw [show (4:ℝ) * Real.log lam = Real.log (lam^4) by
        rw [Real.log_pow]; ring]
  exact Real.exp_log (by positivity)

/-! ############################################################
    ## 3.  Sojourn time under the exact accounting alternative
    ############################################################ -/

/-- Wald first-passage expectation for a random walk with drift mu,
    entry depth d.  R1a-R1c. -/
noncomputable def expected_sojourn (d mu : ℝ) : ℝ := d / mu

theorem sojourn_strictAnti {d : ℝ} (hd : 0 < d) :
    StrictAntiOn (fun mu => expected_sojourn d mu) (Set.Ioi 0) := by
  intro mu1 h1 mu2 h2 hlt
  simp only [Set.mem_Ioi] at h1 h2
  unfold expected_sojourn
  exact div_lt_div_of_pos_left hd h1 hlt

/-- **Expected sojourn time diverges as the drift vanishes.** Corresponds
    to SymPy tag R1c. Written via the reciprocal to keep the tactic proof
    to standard Mathlib lemmas about `1/x → ∞` as `x → 0⁺`. -/
theorem sojourn_tendsto_atTop {d : ℝ} (hd : 0 < d) :
    Filter.Tendsto (fun mu => expected_sojourn d mu)
      (nhdsWithin 0 (Set.Ioi 0)) Filter.atTop := by
  unfold expected_sojourn
  -- VERIFIED against current Mathlib docs: `tendsto_inv_zero_atTop` is not
  -- the current name; it is `tendsto_inv_nhdsGT_zero`.
  have hrecip : Filter.Tendsto (fun mu : ℝ => mu⁻¹)
      (nhdsWithin 0 (Set.Ioi 0)) Filter.atTop := tendsto_inv_nhdsGT_zero
  have : Filter.Tendsto (fun mu : ℝ => d * mu⁻¹)
      (nhdsWithin 0 (Set.Ioi 0)) Filter.atTop :=
    Filter.Tendsto.const_mul_atTop hd hrecip
  simpa [div_eq_mul_inv] using this

end RateBased
