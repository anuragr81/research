# Phelps, E.S. (1972), "The Statistical Theory of Racism and Sexism"

*American Economic Review* 62(4), 659-661. (The Drive copy is a scan of the journal
pages. The article is on PDF pages 1-3; pages 4-7 are blank.)

## Claims formalized

The paper's model:

* **Test score** (eq. 1, p. 659): `y_i = q_i + μ_i`. Qualification `q_i` is
  **continuous** ("promise or degree of qualification") and `μ` is normal with mean zero.
* **No group information** (eq. 2, p. 660): the employer uses the least-squares
  "regression-type relation" `q' = a₁ y' + u'`, with `a₁ = var q / (var q + var μ)`
  and `0 < a₁ < 1`. By footnote 3, `a₁` is the probability limit of the OLS
  coefficient.
* **Race observed** (eqs. 3, 3a, 4, 5, 5', p. 660):
  * `q = α + x + η`, with `x = (-β + ε)c`, `β > 0`, and `c = 1` iff black;
  * `λ = η + cε`, `z = -βc`;
  * the prediction is `q' - z' = a₁ (y' - z')`, with `a₁ = var λ / (var λ + var μ)`.

  So the predicted qualification is `(1 - a₁)(α + z) + a₁ y`: the group's believed
  mean, shrunk toward the score.
* **Case 1** (p. 660): `ε ≡ 0`. The two groups have the same slope, and the black
  line "lies parallel and below".
* **Case 2** (eq. 6, pp. 660-661): `var λ = var η + c² var ε`, so the
  *qualification* variance differs by group. The black slope is greater and tends
  to 1 as `var ε → ∞`. Above some score, blacks are predicted to excel whites who
  have the same score.
* **Further Case** (eq. 7, p. 661): `μ = ξ + cρ`, so the *test reliability* differs
  by group. The white line is steeper, and at low scores whites are predicted below
  equally scoring blacks.

## Result

`lean/Phelps.lean` is a symlink to `lean/Literature/Phelps.lean`. It checks with
`lake env lean`, has no `sorry`, and uses only the axioms `[propext,
Classical.choice, Quot.sound]` (all 18 theorems checked with `#print axioms` in a
scratch copy).

* **Least squares:**
  * `relRatio_pos`, `relRatio_lt_one` and `one_sub_relRatio`: eq. (2).
  * `mse_sub_mse_opt` and `relRatio_minimizes_mse`: `a₁` is the unique population
    least-squares slope, with `MSE(a) - MSE(a₁) = (var q + var μ)(a - a₁)²`.
  * `ssr_sub_ssr_ols`: the finite-sample OLS statement behind footnote 3.
* **The race-observed prediction:**
  * `predict_eq5` and `predict_eq5'`: eqs. (5) and (5'), including the shrinkage
    form `(1 - γ)·m + γ·y`.
  * `gap_identity`: `pred_B - pred_W = (a_B - a_W)(y - α) - (1 - a_B)β`.
* **The three cases:**
  * `case1_parallel`: Case 1.
  * `case2_slope_gt`, `case2_slope_tendsto_one` (a genuine `Tendsto`) and
    `case2_crossing`, which proves the crossing score is
    `α + (1 - a_B)β/(a_B - a_W)`: Case 2.
  * `further_slope_lt` and `further_crossing`, with threshold
    `α - (1 - a_B)β/(a_W - a_B)`: the Further Case.
  * `relRatio_lt_iff`: the steeper line belongs to the group with the higher
    signal-to-noise ratio `var λ / var μ`. This is the paper's "greater reliability
    ... might overcome any tendency for them to have less credibility".
* **No mean gap:** `no_mean_gap_still_differential`. With `β = 0` but unequal
  coefficients, equally scoring applicants are still treated differently at every
  score except `α`.

`sympy/check_phelps.py` passes 14/14 checks, all exact. Exit status is 0 iff every
check passes. It covers:

* the two least-squares identities;
* that under normality `argmax/mean of q | y = (1 - a₁)m + a₁y`. This is the step
  from Phelps's "least-squares predictor" to "posterior mean", which the paper never
  states;
* the gap identity;
* each case at a concrete point. At `α = 1`, `β = 1/2` and unit variances, Case 2
  has `a_B = 2/3 > a_W = 1/2` and crossing `y* = 2`, and the Further Case has
  `a_W = 1/2 > a_B = 1/3` and crossing `y = -1`;
* the `var ε → ∞` limit.

## What formalizing revealed

**The model contains no binary unknown.** The only binary variable is the race
dummy `c`, which is observed. The unknown `q` is continuous and Gaussian. The
formal object is a scalar shrinkage estimator, not a belief over attributes.

**The cases differ in what the employer believes about the groups, not only in
the mean.**
* In Case 1 the groups differ in believed mean (`β`).
* In Case 2 they differ in believed *dispersion* of qualification.
* In the Further Case they differ in believed *test reliability*.

Phelps layers Case 2 and the Further Case on top of `β > 0`. The paper never
isolates them. `no_mean_gap_still_differential` shows that the mean gap is not
needed: with `β = 0`, a dispersion or reliability difference alone makes the
employer treat equally scoring applicants differently.

**"Posterior mean" is our label, not Phelps's.** He writes a least-squares
regression and cites its plim (fn 3). Normality makes it the posterior mean
(checked in sympy), but Phelps also allows the prior to come from "prevailing
sociological beliefs ... (in which latter case the discrimination is
self-perpetuating)" (p. 659). So the employer's model is not asserted to be correct.

## Bearing on Paper B

Phelps is the Gaussian, continuous-attribute ancestor of the statistical
discrimination literature, and he explicitly frames it as a theory of *beliefs*.
He is not a source for a binary-attribute evaluator.

For the plan's 1.C contrast, Phelps's gaps come from the employer's *model of the
groups* differing: prior mean, prior dispersion, or signal reliability, conditional
on `c`. Paper B's sequence gap arises with identical priors and identical evidence.
That is the clean way to state the contrast.

## Not formalized

* Gaussian conditioning as a measure-theoretic statement (only its algebra is
  checked, in sympy).
* Figure 1.
* The verbal remarks on policy and on where beliefs come from.

## Audit findings (2026-09-29)

Checked against `scratchpad/verify_discrimination_econ.md` §1.

* **P1 (MS:279), "evaluator-with-binary-attributes frame" from Phelps: UNSUPPORTED
  — confirmed.**
  * Eq. (1) has a continuous `q` and normal `μ`.
  * The only binary variable is the observed dummy of eq. (3a).
  * The formalization has no binary unknown anywhere.
* **P3 (plan 1.C, lines 310-311), "rests on a difference in beliefs about the
  groups": verified with caveat — confirmed and sharpened.**
  * Every Phelps case *is* a difference in the employer's beliefs about the groups,
    in the broad sense: mean (Case 1), dispersion (Case 2), or test reliability
    (Further Case).
  * The phrase is literally true. What it fails to do is separate Phelps from the
    plan's own sequence gap, which also yields "different mean beliefs" (posteriors).
  * The distinguishing feature is that Phelps's differences sit in the **prior /
    group-level model**, whereas Paper B holds priors and evidence fixed.
* **Qualification of the audit's evidence for P3.**
  * The audit quotes CL (p. 1222), "Phelps assumed available measures of
    productivity to be noisier for minority workers", as a summary of Phelps.
  * That describes only the Further Case, eq. (7). Cases 1 and 2 have equally noisy
    tests.
  * Also, Phelps never presents Case 2 or the Further Case without `β > 0`. The
    no-mean-gap reading is ours (`no_mean_gap_still_differential`), not his.
* **Terminology in the task brief.** Case 2 is a difference in *qualification
  variance*, eq. (6). The *test-reliability* case is the "Further Case", eq. (7).
* **P5, bibliography: consistent.** The scan shows pp. 659-661. Phelps's own
  reference list cites Arrow's RAND memo RM-6253-RC (1971), not Arrow (1973).

**Proposed wording (text only, not applied).**

MS:279, replace the first sentence with:

> The paper's evaluator, who holds a joint belief over two binary attributes,
> generalises the evaluator of the statistical-discrimination lineage, who observes
> a binary group marker and is uncertain about a binary qualification
> \citep[§4]{Arrow1973}, \citep{CoateLoury1993}; \citet{Phelps1972} is the
> continuous-attribute, Gaussian signal-extraction version of the same idea.

Plan 1.C, lines 310-311:

> ... is not statistical discrimination in the sense of \citet{Phelps1972} and
> \citet{Arrow1973}, which rests on the evaluator holding different prior beliefs
> about the groups (a lower believed mean or qualification rate, or in
> \citet{Phelps1972} a different believed dispersion or test reliability); here
> priors and evidence are the same, and the gap arises from a difference in
> sequence alone.
