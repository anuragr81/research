/-
================================================================================
  Buffer variance under time-varying asset volatility (row M15)
  Written for the theory paper on 30 October 2026.
================================================================================

The identities of CapGeometry hold at one value of the asset volatility. They are
instantaneous, so they hold date by date along any path of the asset volatility
t ↦ σ t, and of the exposure policy t ↦ pol t, at every date where the exposure
binds the cap. Buffer variance is then the deficit limit at that date's asset
volatility, whatever policy produced the binding exposure, and so whatever the
asymmetry parameter behind it. Written in the asset variance h = σ², the deficit
limit is h/a² - 2c√h σL/a + σL², the structural relation of equity variance and
asset variance with the multiplier fixed by the cap.

That the optimal exposure binds the cap at each date, once the asset volatility
moves, is a hypothesis here, as it is in CapGeometry.
-/

import Mathlib
import CapGeometry

open Real Filter Topology

namespace TimeVarying

open CapGeometry

/-- On the solvency branch, at any date where the exposure binds the cap, buffer
variance is the deficit limit at that date's asset volatility. -/
theorem deficit_limit_at_date {σ : ℝ → ℝ} {σL c q a1 a2 a3 : ℝ} {pol : ℝ → ℝ → ℝ} {X : ℝ → ℝ}
    (hq : 0 < q) (ha1 : 0 < a1) (h13 : a1 < a3) (t : ℝ) (hx : 1 < X t)
    (hxb : X t ≤ xbar a1 a2 a3) (hpol : pol t (X t) * X t = capU a1 a2 a3 (X t)) :
    zvol2 (σ t) σL c q (pol t) (X t) = nu2 (σ t) σL c a1 :=
  zvol2_solvency hq ha1 h13 hx hxb hpol

/-- Two exposure policies that both bind the cap at a state of the solvency branch give
the same buffer variance there at every date, so the policy, and the asymmetry
parameter behind it, does not enter. -/
theorem same_variance_any_binding_policy {σ : ℝ → ℝ} {σL c q a1 a2 a3 x : ℝ}
    {pol₁ pol₂ : ℝ → ℝ → ℝ} (hq : 0 < q) (ha1 : 0 < a1) (h13 : a1 < a3) (t : ℝ)
    (hx : 1 < x) (hxb : x ≤ xbar a1 a2 a3) (h₁ : pol₁ t x * x = capU a1 a2 a3 x)
    (h₂ : pol₂ t x * x = capU a1 a2 a3 x) :
    zvol2 (σ t) σL c q (pol₁ t) x = zvol2 (σ t) σL c q (pol₂ t) x := by
  rw [zvol2_solvency hq ha1 h13 hx hxb h₁, zvol2_solvency hq ha1 h13 hx hxb h₂]

/-- At each date, a policy that binds the cap deep in surplus gives the surplus limit at
that date's asset volatility. -/
theorem surplus_limit_at_date {σ : ℝ → ℝ} {σL c q a1 a2 a3 : ℝ} {pol : ℝ → ℝ → ℝ}
    (hq : 0 < q) (ha1 : 0 < a1) (h13 : a1 < a3)
    (hpol : ∀ t, ∀ᶠ x in atTop, pol t x * x = capU a1 a2 a3 x) (t : ℝ) :
    Tendsto (zvol2 (σ t) σL c q (pol t)) atTop (𝓝 (nu2 (σ t) σL c a3)) :=
  surplus_limit hq ha1 h13 (hpol t)

/-- The limit in the asset variance h = σ²: h/a² - 2c√h σL/a + σL². -/
theorem nu2_of_variance {h σL c a : ℝ} (hh : 0 ≤ h) :
    nu2 (Real.sqrt h) σL c a = h / a ^ 2 - 2 * c * Real.sqrt h * σL / a + σL ^ 2 := by
  unfold nu2
  rw [Real.sq_sqrt hh]

/-- Control. Without the binding hypothesis the date-t identity fails: with no risky
exposure buffer variance is the liability term alone, here 1, against a deficit limit
of 5. -/
theorem control_date_identity_needs_binding :
    zvol2 1 1 0 1 (fun _ => 0) 2 ≠ nu2 1 1 0 (1 / 2) := by
  rw [zvol2_eq (by norm_num) (by norm_num)]
  unfold nu2
  norm_num

end TimeVarying
