# Coate, S. & Loury, G.C. (1993), "Will Affirmative-Action Policies Eliminate Negative Stereotypes?"

*American Economic Review* 83(5), 1220-1240. The Drive copy is the JSTOR PDF of the
published article.

## Claims formalized

**Section I (pp. 1223-1226).**
* **Setup.** There are two observable groups. Qualification for task one is binary
  and requires an unobservable investment with cost `c ~ G`. The employer sees the
  worker's group and a signal `θ ∈ [0,1]` with densities `f_q`, `f_u`. The
  likelihood ratio `φ = f_u/f_q` is nonincreasing.
* **Bayes' Rule** (eq. 1): `ξ(π, θ) = 1/{1 + [(1-π)/π] φ(θ)}`.
* **Assignment rule.** Assign the worker iff `r ≥ [(1-π)/π] φ(θ)`. The standard
  `s*(π)` of eq. (2) is decreasing in `π`.
* **Worker response.** A worker facing standard `s` gains `β(s) = ω[F_u(s) - F_q(s)]`
  from investing, so the fraction that invests is `G(β(s))`.
* **Definition 1 (equilibrium).** An equilibrium is a pair of beliefs with
  `π_i = G(β(s*(π_i)))` (eq. 3). A discriminatory equilibrium is one with
  `π_b < π_w`.

**Stereotype (p. 1221).** "Employers form beliefs about the correlation between group
identity and productivity which, in the equilibria of our model, must be correct. If
workers in one group are seen as less productive, we say that employers have negative
stereotypes about that group."

**Proposition 1 (p. 1226).** Two or more nonzero solutions of (3) exist if
`G(β(s)) > φ(s)/[r + φ(s)]` for some `s`.

**Proposition 2 (p. 1229).** If `ρ̂` is decreasing, every equilibrium under
affirmative action has homogeneous beliefs.

**The uniform example of II.B (pp. 1230-1232).**
* The example's primitives: `p_q`, `p_u`, `π̂ = φ/(r + φ)`, `π_ℓ = ω(1 - p_u)` and
  `π_c = ω(1 - p_q)`, together with eq. (6).
* Eqs. (7)-(9).
* **Proposition 3:** for `π_ℓ > 1/2` the only stable equilibrium under affirmative
  action is the patronizing one, `π_b = 1 - π_ℓ`.
* Footnotes 20-21.

## Result

`lean/CoateLoury.lean` is a symlink to `lean/Literature/CoateLoury.lean`. It checks
with `lake env lean`, contains no `sorry`, and uses only the axioms `[propext,
Classical.choice, Quot.sound]`; all 25 theorems were checked with `#print axioms` in a
scratch copy.

**Bayes' rule and the standard.**
* `posterior_eq_odds`: eq. (1).
* `posterior_strictMono`: the posterior rises with the prior.
* `assign_iff`: the payoff rule is equivalent to `r ≥ [(1-π)/π] φ`.
* `assign_iff_threshold`: the same rule written as `π ≥ φ/(r + φ)`.
* `acceptSet_mono` and `threshold_antitone`: `s*` is decreasing in `π`.
* `acceptSet_upper`: under MLRP the optimal policy is a threshold rule.
* `isLeast_of_EE`: on the EE locus the standard is exactly `s`.

**The stereotype.**
* `cov_group_qualified`: the believed covariance is
  `Cov(1_W, 1_q) = λ(1-λ)(π_w - π_b)`.
* `negativeStereotype_iff_cov_pos`: a negative stereotype about B's is exactly a
  believed positive correlation between being W and being qualified.

**Proposition 1.**
* `prop1_two_equilibria`: Proposition 1 in full. It is proved with the intermediate
  value theorem on both sides of `s₀`, and yields two distinct beliefs in `(0,1)`,
  each with its own optimal standard, each self-confirming.
* `exφ_strictAntiOn`, `exφ_pos` and `smooth_example`: an explicit smooth instance.
  * Primitives: `f_q = 2(1+θ)/3`, `f_u = 2(2-θ)/3`, so `φ = (2-θ)/(1+θ)`; `ω = 3`;
    uniform costs; `r = 1`.
  * The self-confirming beliefs are `1/2` and `4/9`, with standards `1/2` and
    `2/3`, so `(π_b, π_w) = (4/9, 1/2)` is a discriminatory equilibrium between ex
    ante identical groups.

**Proposition 2.**
* `prop2_homogeneous`: Proposition 2.

**Example II.B.**
* `pihat_eq6`: the middle term of eq. (6).
* `example_selfConfirming`: `π_c` and `π_ℓ` are self-confirming, and they are the
  only self-confirming beliefs.
* `alpha_eq7`: eq. (7).
* `eq8_solutions`: the roots of eq. (8) are `π_ℓ` and `1 - π_ℓ`.
* `lambdaHat_iff`: footnote 20.
* `patronSeq_tendsto`: the dynamics of Proposition 3. From any start `x₀ < π_ℓ`,
  process (9) converges to `1 - π_ℓ`. The proof is a genuine `Tendsto` with an
  explicit geometric rate.
* `colorBlind_unstable`: the slope of the step map is `π_ℓ/(1-π_ℓ) > 1` at `π_ℓ` and
  `(1-π_ℓ)/π_ℓ < 1` at `1 - π_ℓ`.
* `stereotype_worsens`.
* `fn21_instance`: one exact parameter point from footnote 21.

**Sympy.** `sympy/check_coate_loury.py` passes 35/35 checks, all exact. It covers:
* eq. (1) and the assignment boundary;
* the covariance identity;
* the smooth instance, derived from the densities, with the local-stability slopes of
  p. 1226 (`1/2` is stable, `4/9` is unstable) and exact adjustment-process
  iterations;
* the example's CDFs, so that `β(θ_q) = π_ℓ` and `β(θ_u) = π_c`;
* eqs. (6)-(8), footnote 20, and 60 exact iterations of (9);
* **all of footnote 21**. With `p_u = .2`, `p_q = .3` and `r = 2/3`, patronization
  occurs exactly for `0.5/(λ - 0.2) < ω < 5/7`. This region is nonempty iff
  `λ > 0.9`, and the stereotype worsens iff `ω > 2/3`, as printed.

## What formalizing revealed

**Of the three classical sources, CL has the closest evaluator.** The employer is a
Bayesian (eq. 1). He holds a belief about a binary latent attribute (qualified or
not) and updates it on a noisy signal, conditional on an observed binary group.
Arrow's §4 employer never sees a signal, and Phelps's unknown is continuous.

**CL's stereotype is a believed cross-attribute association.**
`negativeStereotype_iff_cov_pos` makes "believed correlation between group identity
and productivity" (p. 1221) literal: `π_b < π_w` exactly when the believed
covariance of `1_W` and `1_q` is positive. That is the same kind of object as
Paper B's association statistic. The difference lies in where the association comes
from. In CL it is correct in equilibrium: "in the equilibria of our model, must be
correct" (p. 1221), and employers "correctly perceive group identity to be correlated
with worker productivity" (p. 1227). In Paper B it is produced by coherent updating on
impoverished input.

**Proposition 1 is the intermediate value theorem, and multiplicity is stable only in
a limited sense.**
* The formal proof needs exactly the paper's hypotheses. They give `EE > WW` at both
  ends because `WW(0) = WW(1) = 0` and `φ > 0`.
* Both the smooth instance and the example show that the *interior* discriminatory
  root need not be stable. In the smooth instance `4/9` is unstable, so the stable
  discriminatory pair is `(0, 1/2)`. This matches CL's footnote 11: "A little more
  structure is required to guarantee the existence of multiple locally stable
  equilibria."

**Proposition 2 needs *strict* monotonicity.** The argument "`ρ̂(s_b) = ρ̂(s_w)`, so
`s_b = s_w`" uses injectivity. With a merely nonincreasing `ρ̂`, a flat stretch would
allow `s_b ≠ s_w`. `prop2_homogeneous` assumes `StrictAntiOn`.

**CL have dynamics as well as fixed points.** The adjustment process
`π_{t+1} = G(β(s*(π_t)))` (p. 1226) and process (9) are belief dynamics. They are
iterated best responses to realized qualification rates, not an individual evaluator
updating on a sequence of cues.

## Bearing on Paper B

MS:280-281's contrast, "Unlike \citet{CoateLoury1993} ... the stereotype is a
self-confirming steady state of best responses", is accurate as a statement about
*mechanism*: equilibrium with correct beliefs, as against kinematics. But it should
not suggest a difference in *concept*. CL's stereotype is a believed group–attribute
correlation, as Paper B's is.

CL also fit MS:279's "evaluator-with-binary-attributes frame" better than Phelps
does. Their affirmative action is "a government-mandated constraint on employers"
(p. 1221). This is the natural referent for the "enforced constraint on behaviour" in
the Becker sentence (MS:282-283). The manuscript does not draw that link.

## Not formalized

* The MLRP ⇒ FOSD step (`F_q ≤ F_u`), which needs integrals.
* The Lagrangian analysis of Section III and **Proposition 4**, which rests on the
  implicit function theorem plus a figure argument.
* Section IV's subsidy comparisons, Figures 5-9.
* The Pareto ranking of equilibria.
* The general local-stability criterion of p. 1226. It is checked on the smooth
  instance in sympy and proved for process (9) only.

## Audit findings (2026-09-29)

Checked against `scratchpad/verify_discrimination_econ.md` §3.

* **C1, MS:280-281, "self-confirming steady state of best responses": verified,
  confirmed.** Definition 1 and footnote 9 ("each strategy is a best response to the
  other").
* **C2, the contrast holds with a caveat: confirmed and made formal.**
  * CL's stereotype is literally a believed covariance (`negativeStereotype_iff_cov_pos`).
  * CL's employer is a Bayesian with a binary latent attribute, a binary observed
    group and a signal. That is closer to MS:279's frame than Phelps.
  * CL do study adjustment dynamics (process (9) converges; `patronSeq_tendsto`).
    Those dynamics are best-response iteration on group-level beliefs, not individual
    updating.
* **C3, correct beliefs: confirmed.** CL pp. 1221 and 1227.
* **C4, bibliography: confirmed.** The JSTOR cover gives Vol. 83, No. 5 (Dec. 1993),
  pp. 1220-1240.
* **Relative to audit §2(c)(ii): qualified.** The audit says Arrow's §4 "already
  contains" the self-confirming equilibrium. CL prove the existence of discriminatory
  equilibria (Proposition 1) that Arrow only conjectured (Arrow WP p. 30). CL
  themselves credit Arrow with the idea, not the result (p. 1222: "He noted that ...
  employers' prejudicial beliefs can be self-fulfilling").
* **For MS:282-283 (Becker sentence).** CL's "government-mandated constraint on
  employers" (p. 1221) supports reading "enforced constraint on behaviour" as a
  regulatory constraint. The audit's K1 recommendation to say so explicitly stands.

**Proposed wording (text only, not applied).** For MS:279, see the Phelps README.
For MS:280-282:

> However, the object of interest here is kinematic rather than an equilibrium fixed
> point. In \citet{CoateLoury1993} a stereotype is, as here, a believed correlation
> between group identity and a productive attribute, but it is a self-confirming
> steady state of employers' and workers' best responses (an idea sketched in
> \citet[§4]{Arrow1973}), and in equilibrium it is correct. Here the believed
> cross-attribute association is produced by coherent updating on impoverished
> input, and the paper characterises the channel-free baseline ...
