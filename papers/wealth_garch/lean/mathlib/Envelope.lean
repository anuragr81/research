/-
================================================================================
  Envelope identities and the smooth-fit discrimination check
  Companion to PROOFS_v2.tex: lem:envelope (ENV), lem:envelopeK (ENVK),
  rem:smfn (SMFN).  Lean 4 + Mathlib.
================================================================================

SCOPE, in the same spirit as SmoothFit.lean.

Each result below has an analytic step (Danskin / the envelope theorem for a
pointwise supremum of parameter-affine payoffs, and the interior HJB equation)
and an algebraic step (the ODE or boundary condition that the envelope
function then satisfies).  Mathlib has no envelope-of-a-value-function library,
so — exactly as SmoothFit.lean took the viscosity-supersolution property as a
hypothesis rather than deriving it — the analytic facts here enter as
hypotheses (the envelope substitution `∂V/∂param = -W`/`-N`, the differentiated
interior equation, the FOC and smooth-fit slope, the value-matching relation).
What is machine-checked is that GIVEN those, the paper's stated ODEs and
boundary conditions follow.  The algebra is the part these lemmas actually
author; the analytic inputs are standard (Danskin; differentiating the HJB).

All quantities are values at a single point (of x, and of the parameter),
so each statement is an identity among reals.

Confidence, per declaration:
  `smf_permits`        -- reuses SmoothFit.OperatorLe; one sign fact
                          (σ²>0, q≤0 ⇒ ½σ²q ≤ 0) then linear.  `nlinarith`.
  `wode_of_envelope`   -- ENV eq:Wode.  Substitute the envelope identities into
                          the differentiated interior equation and negate.
                          `subst` + `linear_combination`.
  `node_of_envelope`   -- ENVK eq:Node, the homogeneous case (no running term).
                          Same shape as `wode_of_envelope`.
  `neumann_at_ystar`   -- ENVK N'(y*)=0 from V''(y*)=0 and the chain rule.
  `trigger_jump`       -- ENVK N(x_L)=N(y_post)+1: differentiate value matching
                          along the boundary; FOC and smooth fit cancel the
                          boundary-derivative terms, the explicit -1 survives.
  If any `linear_combination`/`nlinarith` line fails, it is a tactic fix, not a
  statement problem — the by-hand derivation for each is in the comment above
  it, and the hypotheses encode exactly the paper's premises.
-/

import SmoothFit

open SmoothFit

namespace Envelope

/-! ############################################################
    ## SMFN (rem:smfn):  smooth fit is CONSISTENT with the
    ##   supersolution property — the discrimination check.
    ############################################################ -/

/--
**rem:smfn.**  Where `no_convex_kink` shows that a *jump* frees the test-
function curvature `q` to range over all of ℝ and thereby breaks the
supersolution inequality, this shows the complementary fact: under smooth fit
the admissible curvatures are exactly the `q ≤ 0` (a `C²` test function
touching the affine intervention side from below, with matching value and
slope, must be concave there), and for those the operator inequality HOLDS.
So the argument discriminates rather than merely destroys.

`d.b - d.rhs = (1+κ)μ - (ρ_L V + Λ) = -Ψ(x_L)` at the trigger, so the
hypothesis `d.b ≤ d.rhs` is exactly `Ψ(x_L) ≥ 0`, the intervention-region
inequality (Prop soc(ii)) the remark cites. -/
theorem smf_permits (d : TriggerData) (hb : d.b ≤ d.rhs) :
    ∀ q : ℝ, q ≤ 0 → OperatorLe d q := by
  intro q hq
  unfold OperatorLe
  -- ½ σ² q ≤ 0 since σ² > 0 and q ≤ 0; then b ≤ rhs closes it.
  nlinarith [mul_nonneg (le_of_lt d.hσ2) (neg_nonneg.2 hq), hb]

/-! ############################################################
    ## ENV (lem:envelope):  the envelope function W solves eq:Wode
    ##   on the inaction region.
    ############################################################ -/

/--
**lem:envelope, eq:Wode.**  Analytic inputs, as hypotheses:
* the envelope identity `∂_λ V = -W` and its `x`-derivatives
  (`Vλ = -W`, `Vλ' = -W'`, `Vλ'' = -W''`), from Danskin;
* the interior equation differentiated in `λ_S`,
  `½σ² Vλ'' + μ Vλ' - ρ_L Vλ - (R-x)^+ = 0`
  (the envelope theorem sends `∂_λ (sup_π ℒ^π V)` to `ℒ^{u} (∂_λ V)`, and
  `∂_λ Λ = (R-x)^+`).

Conclusion: `W` satisfies the inhomogeneous ODE
`½σ² W'' + μ W' - ρ_L W + (R-x)^+ = 0`.

By hand: substitute `Vλ = -W` etc. into the differentiated equation; every
term flips sign, and negating the whole line is eq:Wode. -/
theorem wode_of_envelope
    (s2 mu rhoL g W Wp Wpp Vl Vlp Vlpp : ℝ)
    (he   : Vl   = -W)
    (hep  : Vlp  = -Wp)
    (hepp : Vlpp = -Wpp)
    (hdiff : (1/2) * s2 * Vlpp + mu * Vlp - rhoL * Vl - g = 0) :
    (1/2) * s2 * Wpp + mu * Wp - rhoL * W + g = 0 := by
  subst he hep hepp
  linear_combination -hdiff

/-! ############################################################
    ## ENVK (lem:envelopeK):  the injection-count envelope N.
    ############################################################ -/

/--
**lem:envelopeK, eq:Node** — the homogeneous case.  Same inputs as ENV with
`∂_K V = -N`, but the differentiated interior equation has NO running term
(injections accrue only at boundary hits, none in the interior). -/
theorem node_of_envelope
    (s2 mu rhoL N Np Npp VK VKp VKpp : ℝ)
    (he   : VK   = -N)
    (hep  : VKp  = -Np)
    (hepp : VKpp = -Npp)
    (hdiff : (1/2) * s2 * VKpp + mu * VKp - rhoL * VK = 0) :
    (1/2) * s2 * Npp + mu * Np - rhoL * N = 0 := by
  subst he hep hepp
  linear_combination -hdiff

/--
**lem:envelopeK, Neumann datum `N'(y*) = 0`.**  Differentiate `V'(y*(K);K) = 1`
in `K`: `V''(y*)·y*' + ∂_K V'(y*) = 0`.  With `V''(y*) = 0` (second-order
smooth fit at the barrier) the first term drops, so `∂_K V'(y*) = 0`, and with
`∂_K V' = -N'` this is `N'(y*) = 0`. -/
theorem neumann_at_ystar
    (Vpp_ys ystar_K dKVp Np : ℝ)
    (hVpp   : Vpp_ys = 0)
    (hchain : Vpp_ys * ystar_K + dKVp = 0)
    (henvp  : dKVp = -Np) :
    Np = 0 := by
  subst hVpp henvp
  linear_combination -hchain

/--
**lem:envelopeK, trigger jump `N(x_L) = N(y_post) + 1`.**  Differentiate the
value-matching relation
`V(x_L) = V(y_post) - K - (1+κ)(y_post - x_L)` along the boundary in `K`.
The `x_L'` terms cancel by the smooth fit `V'(x_L) = 1+κ`, the `y_post'` terms
cancel by the FOC `V'(y_post) = 1+κ`, and the explicit `-∂K/∂K = -1` survives:
`∂_K V(x_L) = ∂_K V(y_post) - 1`.  With `∂_K V = -N` at both points this is the
stated jump. -/
theorem trigger_jump
    (kap xL_K yp_K Vp_xL Vp_yp dKV_xL dKV_yp N_xL N_yp : ℝ)
    (hsf  : Vp_xL = 1 + kap)
    (hfoc : Vp_yp = 1 + kap)
    (hmatch : Vp_xL * xL_K + dKV_xL
              = Vp_yp * yp_K + dKV_yp - 1 - (1 + kap) * (yp_K - xL_K))
    (henvL : dKV_xL = -N_xL)
    (henvY : dKV_yp = -N_yp) :
    N_xL = N_yp + 1 := by
  subst hsf hfoc henvL henvY
  linear_combination -hmatch

end Envelope
