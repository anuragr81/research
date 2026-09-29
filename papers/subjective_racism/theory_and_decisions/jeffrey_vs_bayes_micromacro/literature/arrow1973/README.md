# Arrow, K.J. (1973), "The Theory of Discrimination"

In O. Ashenfelter and A. Rees (eds.), *Discrimination in Labor Markets*, Princeton
University Press, 1973, pp. 3-33.

**Version and pagination.**
* The Drive copy is **Princeton Industrial Relations Section Working Paper
  No. 30A**, "Presented at Conference on 'Discrimination in Labor Markets' October
  7-8, 1971". It is not the book chapter.
* It has its own pagination: text pp. 1-31, then two pages of references, then a
  two-page Appendix.
* All page numbers below and in `lean/Arrow.lean` are **working-paper pages**. WP
  page n is PDF page n + 1.
* The book pagination (pp. 3-33) cannot be checked against this copy.

## Claims formalized

**§1, employer taste discrimination (WP pp. 4-9)**
* Profit is `π = f(W+B) - w_W W - w_B B` (1).
* `MP_B = w_B + d_B` (2) and `MP_W = w_W + d_W` (3), where `d_B` is Becker's
  discrimination coefficient, "the negative of the marginal rate of substitution of
  profits for B labor". Hence `w_W - w_B = d_B - d_W > 0` (4).
* `π - π₀ = d_W W + d_B B` (5-6).
* With ratio-only utility, `d_W W + d_B B = 0` (7).
* `W/L = d_B/(w_W - w_B)` and `B/L = -d_W/(w_W - w_B)` (p. 8).

**§2, nonconvexities (WP pp. 13-20 and Appendix)**
* Co-worker discrimination by perfect substitutes, with cost `C = w_W(W/L) W + w_B B`
  (12): any mixed labour force costs more than at least one of the two segregated
  ones (p. 17).
* Appendix: a ratio-dependent utility that is increasing in `π` cannot have convex
  indifference surfaces.

**§4, imperfect information (WP pp. 25-31)**
* There are two groups W and B, and a binary qualification for skilled jobs.
* The employer "does believe that the probability that a random W worker is
  qualified is `p_W` and that a random B worker is qualified is `p_B`" (p. 26). He
  observes nothing else about the worker before hiring.
* The personnel investment earns zero expected return:
  `r = (MP_S - w_i) p_i` (13). Hence `w_W = q w_B + (1 - q) MP_S` with
  `q = p_B/p_W` (14), and `p_B < p_W` implies `w_W > w_B` (p. 27).
* Qualification is endogenous: `p_i = S(w_i - w_U)` (15), with `MP_U = w_U` (16)
  (pp. 28-29).
* A symmetric equilibrium exists, "it is clear" (p. 29). Uniqueness is left "an open
  question".
* The dynamics are Marshallian, `dp_W/dt = k[S(v_W) - p_W]`.
* Stability condition: `E (MP_S - w_S)/(w_S - w_U) < 1` (p. 31). The proof is
  deferred to Arrow (1971), Technical Note F, which is not in the Drive copy.
  Instability "strongly suggests, though it does not prove, that there are
  equilibria other than the symmetric, non-discriminatory, one" (p. 30).

## Result

`lean/Arrow.lean` is a symlink to `lean/Literature/Arrow.lean`. It checks with
`lake env lean`, has no `sorry`, and uses only the axioms `[propext,
Classical.choice, Quot.sound]` (all 16 theorems checked).

**§1**
* `wage_gap`: eq. (4).
* `profit_change`: eqs. (5)-(6).
* `ratio_utility_euler`: the two partial derivatives of `h(B/W)` as `HasDerivAt`
  statements, together with Euler's relation `B U_B + W U_W = 0`.
* `eq7_of_ratio_utility`: eq. (7).
* `segregation_shares`: p. 8.

**§2**
* `mixed_costlier`: p. 17.
* `appendix_nonconvex`: the Appendix. Homogeneity, strict monotonicity in `π`,
  continuity at `(B₀, W₀)`, and one lower-profit indifferent bundle together rule
  out midpoint-convex upper contour sets. The proof takes a genuine `t → 0⁺` limit.

**§4**
* `eq14` and `wage_differential`: p. 27.
* `exists_symmetric`: the intermediate-value hypothesis that the p. 29 "it is clear"
  needs.
* `desired_hasDerivAt` and `stability_iff`: the p. 31 condition is exactly
  `S'(v) r/p² - 1 < 0`.
* `example_two_fixed_points`, `example_only_fixed_points`, `example_discriminatory`
  and `example_stability`: an explicit instance of §4 with `MP_S` and `w_U` held
  fixed, `MP_S - w_U = 4`, `r = 1` and `S(v) = v/(v+2)`.
  * The self-confirming shares are exactly `{1/2, 1/3}`.
  * `(p_W, p_B) = (1/2, 1/3)` is a discriminatory equilibrium with
    `w_W = MP_S - 2 > w_B = MP_S - 3`.
  * Arrow's criterion is `1/2` at `p = 1/2` (stable) and `2` at `p = 1/3`
    (unstable).

`sympy/check_arrow.py` passes 14/14 checks, all exact:
* eqs. (4)-(7), p. 8, and eq. (14);
* the full **two-group Jacobian** at a symmetric equilibrium, with general smooth
  `S`, `MP_S(P)` and `w_U(P)` and group weights `n_W`, `n_B`:
  * `(n_B, -n_W)` is an eigenvector, with eigenvalue `S'(v) r/p² - 1`;
  * the other eigenvalue adds `S'(v)(M' - U')(n_W + n_B)`;
  * Arrow's `E (MP_S - w_S)/(w_S - w_U)` equals `S'(v) r/p²` at `S(v) = p`;
* the explicit instance.

## What formalizing revealed

**Arrow's stability condition is the antisymmetric-mode eigenvalue.** The p. 31
condition is exactly the requirement that a perturbation moving the two groups'
qualified shares in opposite directions (weighted by group size) dies out. The
other mode carries `S'(M' - U')`, which is negative under diminishing returns
(`M' < 0`, `U' > 0`). So Arrow's single condition is necessary and sufficient for
local stability. This reconstruction is ours: Arrow's proof is in a RAND technical
note that is not in the Drive copy. It agrees with his reading of the condition as
the one under which a discriminatory deviation reinforces itself.

**Multiplicity is conjectured in the text, and it holds.** Arrow proves neither
uniqueness nor multiplicity. The explicit instance shows that his §4 system does
admit a discriminatory equilibrium with identical supply schedules, at least with
`MP_S` and `w_U` fixed. In that instance the discriminatory pair puts B on the
unstable root. If `S` is truncated at zero for `v ≤ 0`, the corner `p_B → 0`
becomes the stable discriminatory state. That corresponds to Arrow's own "refusal
to hire B workers at all for skilled jobs" (p. 27).

**Arrow's §4 employer never updates on individual evidence.** He sees only group
membership before hiring. Beliefs enter as group-level priors `p_W`, `p_B`, and the
wage (eq. 14) is set on the prior alone. There is no signal and no Bayes step:
Coate-Loury add both.

**Two small defects in the text.**
* The Appendix's last display prints `U(π', B₀, W₀) = U(π₀, B₀, W₀)`. The argument
  yields `≥`, which suffices. `appendix_nonconvex` proves the corrected step.
* The p. 29 "it is clear that there is a symmetric equilibrium" needs an
  intermediate-value condition (`exists_symmetric`). Symmetry alone does not give
  existence.

## Bearing on Paper B

§4 is the nearest classical source for an evaluator facing a binary qualification
across two groups. But the group is observed, and the only uncertain attribute is
qualification, so there is no joint belief over two uncertain binary attributes.
It is also where the self-confirming-belief idea originates, as a conjecture backed
by a stability computation; CL credit Arrow for it (CL pp. 1222, 1227). The paper
is otherwise mainly about tastes: §§1-3 fill WP pp. 1-24 of 31.

## Not formalized

* §1: the long-run capital-mobility argument (p. 9) and the long-run segregation
  proof (p. 20).
* The foremen model: eq. (10) is quoted from Arrow (1971), Technical Note B, and not
  derived.
* §2: the niche and convexification discussion.
* §3 (costs of adjustment and personnel investment): verbal, with results quoted from
  Technical Note E.
* The cognitive-dissonance account of beliefs (p. 28).

## Audit findings (2026-09-29)

Checked against `scratchpad/verify_discrimination_econ.md` §2.

* **A1 (MS:279), binary frame from Arrow: verified with caveat — confirmed.**
  * §4 has an observed binary group and an uncertain binary qualification.
  * Additional caveat: the employer receives no signal at all, so there is no updating
    on individual evidence.
* **A2 / problem (i): confirmed.**
  * The paper is mostly taste-based.
  * WP p. 25: "an alternative interpretation ... not tastes, but perception of
    reality".
* **A3 (plan 1.C): verified — qualified as in the Phelps README.**
  * Arrow's §4 difference is in group-level prior beliefs `p_W`, `p_B`.
  * The plan's phrase is true but does not separate Arrow from Paper B's gap. Only
    "different *prior* beliefs about the groups" does.
* **Problem (ii), "Arrow's §4 already contains the self-confirming equilibrium the
  manuscript attributes to CL": qualified.**
  * Arrow has the idea, the equations (13), (15), (16), a symmetric equilibrium, and
    a stability condition.
  * He does **not** establish that a discriminatory equilibrium exists (p. 30: "does
    not prove").
  * CL's Proposition 1 proves it. MS:280 introduces CL with "for example", so the
    attribution stands. A precise version would credit the idea to Arrow and the
    existence result to CL.
* **A4: confirmed.** Arrow himself allows beliefs that are not correct (p. 28,
  subjective probabilities adopted to justify conduct). "Correct beliefs" is BIR's
  classification.
* **A5 and problem (iii): confirmed.**
  * The Drive copy is the 1971 working paper, and all pages here are WP pages.
  * Any page-specific citation of Arrow1973 in the manuscript must not use them.
    Cite by section (`\citep[§4]{Arrow1973}`) instead.

**Proposed wording (text only, not applied).** For MS:279-281, see the Phelps
README for the first sentence. Continue with:

> However, the object of interest here is kinematic rather than an equilibrium fixed
> point. The self-confirming stereotype sketched in \citet[§4]{Arrow1973} and
> established by \citet{CoateLoury1993} is a steady state of employers' and
> workers' best responses; here ...
