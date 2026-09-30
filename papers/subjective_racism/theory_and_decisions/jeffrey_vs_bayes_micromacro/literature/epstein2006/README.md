# Epstein, L. G. (2006), "An Axiomatic Model of Non-Bayesian Updating"

*Review of Economic Studies* 73, 413-436.

**Source.** The copy used is the published version (24 pp., `epstein2006.pdf`;
on Drive as `Epstein-Updating-RESTUD-2006.pdf`). Page references are to the
journal's printed pages. The journal page is the PDF page plus 412. The page
header prints "Review of Economic Studies (2006) 73, 413-436". It does not print
the issue number (2) or the DOI. Read in full on 2026-09-30, including
Appendix A (the proofs of Theorem 1 and Corollaries 1 and 3) and Appendix B
(Theorem 2). Every formula used below was checked on the rendered pages. The
paper gives **no numerical illustrations**. The numbers in the checks are built
here to instantiate its displayed claims, and are labelled as built.

## Claims formalized

**Setting (pp. 413, 416-418).** There are three periods. At `t = 0` the agent
ranks *contingent menus* `F : S1 → M(S2)`. At `t = 1` **one** signal `s1 ∈ S1`
is realized, and she updates and picks an act from `F(s1)`. At `t = 2` the state
`s2 ∈ S2` is realized. `S1` and `S2` are finite.

**The Gul-Pesendorfer form (1)** (p. 415):
`U(A) = max_{x∈A} {U(x) + V(x)} - max_{y∈A} V(y)`.

**The representation** (pp. 418-419):

* (4) `U(F) = ∫_{S1} U(F(s1); s1) dp1`.
* (6) `U(F(s1); s1) = max_f {∫u(f)dp(·|s1) + α(s1)∫u(f)dq(·|s1)} - max_{f'} α(s1)∫u(f')dq(·|s1)`.
* Reg1-Reg4:
  * `u` is mixture linear and non-constant;
  * `q(·|s1) ≪ p(·|s1)`;
  * `α ≥ 0`;
  * `p1` has full support.

**Theorem 1 (p. 425).** Order, Continuity, Independence, Nondegeneracy,
Set-Betweenness, SRL, State Independence and `S1`-Full Support hold iff `≽` has
the representation (4)-(6) with Reg1-Reg4.

* `p` is the *commitment prior* (7).
* `q` is the retroactively revised prior that tempts the agent.
* `α(s1)` is the marginal cost of self-control (Corollary 2, (22), pp. 427-428).

**Compromise prior (pp. 416, 419).**

* Interim choice maximizes (8), `∫u(f)dp(·|s1) + α(s1)∫u(f)dq(·|s1)`.
* This is Bayesian updating of the compromise prior (9),
  `p*(s1,s2) = [p(s2|s1) + α(s1)q(s2|s1)]/(1+α(s1)) · p1(s1)`.
* Its conditional is (10), `p*(·|s1) = [p(·|s1) + α(s1)q(·|s1)]/(1+α(s1))`.
  Each `s1`-conditional of `p*` is "a mixture of the conditionals of `p` and
  `q`, where the mixture weights may vary with the signal" (p. 416).

**Prior-Bias (pp. 428-429).**

* A signal `s1` is *neutral* iff `p(·|s1) = p2` (p. 428).
* Axiom 9 (Prior-Bias): `{f} ≻_{s1} {g}` implies `{f} ~_{s1} {f,g}` if `s1` is
  neutral or `{f}‾ ~ {g}‾`.
* Axiom 10 (Positive Prior-Bias) replaces the second condition with
  `{f}‾ ≽ {g}‾`.
* Axiom 11 (Negative Prior-Bias) replaces it with `{f}‾ ≼ {g}‾`.

**Corollary 3 (p. 429).** Under the axioms of Theorem 1, Prior-Bias (Positive,
Negative) holds iff, for each `s1`, either `α(s1) = 0`, or `α(s1) > 0` and

  (23) `q(·|s1) = (1-λ(s1)) p(·|s1) + λ(s1) p2(·)`,

with `λ(s1) ≤ 1` (respectively `0 < λ(s1) ≤ 1`, `λ(s1) ≤ 0`). The display is
identical to (11) on p. 420.

The paper adds: "Note finally that under Prior-Bias, `q(·|s1) = p(·|s1)` and
updating is standard if `S1` is a singleton, or more generally, if `p` is a
product measure" (p. 429).

**Section 2.3 examples (pp. 420-421).** The section has exactly four
subsections:

* 2.3.1 **Underreaction and overreaction.** This covers (11) and the rewrite
  `q(s2|s1) = p(s2|s1) - λ(s1)(p(s2|s1) - p2(s2))`. It also gives
  (12) `p*(·|s1) = (1 - αλ/(1+α)) p(·|s1) + (αλ/(1+α)) p2(·)`. With constant
  `λ` and `α`, `p*` is less sensitive to `s1` than `p(·|s1)` if `λ > 0`, and
  more sensitive if `λ < 0`.
* 2.3.2 **Confirmatory bias.** Here `γ = αλ/(1+α)` varies with `s1`. Under (13),
  `S1 = {a,b}`, `S2 = {A,B}`, `p(a|A) = p(b|B) > 1/2`, and "`p2(B) > 1/2` iff
  `p1(b) > 1/2`". The weight is `γ(s1) = γ*(p1(s1)/max p1)` with `γ*` decreasing.
* 2.3.3 **Representativeness.** With (13) and `q(A|a) = q(B|b) = 1`, the paper
  claims `p*(A|a) > p(A|a)` and `p*(B|b) > p(B|b)`.
* 2.3.4 **Sample-Bias.** Here `S1 = S2 = S` and
  `q(·|s) = (1-λ)p(·|s) + λδ_s`. `λ > 0` gives the hot hand and `λ < 0` the
  gambler's fallacy. Footnote 10: nonnegativity needs `p(s|s) ≥ -λ/(1-λ)`.

There is **no base-rate-neglect example**. See Audit findings.

**Section 4 (pp. 429-430).** "Our non-Bayesian agent violates the law of
iterated expectations, because she uses the compromise measure `p*` … at
`t = 1` but uses `p` for choice at `t = 0`. Thus, there exist acts `f` such that
`{f} ≻ {-f}` at time 0 and yet … at each `s1`, the agent would strictly prefer
to choose `-f` out of `{f, -f}`." The paper calls this a violation of the
"sure-thing principle for action rules" and links it to no-trade theorems.

## Result

`lean/Epstein.lean` is a symlink to `lean/Literature/Epstein.lean`.

* It is standalone on Mathlib (`import Mathlib`, namespace
  `Literature.Epstein`).
* It checks with `lake env lean` and contains no `sorry`.
* All 50 of its theorems use only the axioms `[propext, Classical.choice,
  Quot.sound]`, checked with `#print axioms` in a scratch copy.

The objects are measures on finite `S1 × S2`, written as real tables.

| Lean theorem | Content |
|---|---|
| `cond_sum`, `joint_eq_marg_mul_cond`, `tower` | Bayesian conditionals; `p` is generated by `p1` and its conditionals (p. 419); `∑ p1 p(·|s1) = p2` |
| `gp`, `menuU`, `menuU_singleton` | (1) and (6) for a finite menu; on a singleton menu (6) is conditional expected utility under `p` (the commitment case behind (7)) |
| `marg1_pstar`, `cond_pstar`, `cond_pstar_mix` | (9) keeps `p1`; **(10)**, the `s1`-dependent mixture `(1/(1+α)) p(·|s1) + (α/(1+α)) q(·|s1)` |
| `compromise_objective` | (8) `= (1+α) E_{p*(·|s1)} u`: interim choice is expected-utility maximization under `p*(·|s1)` |
| `priorBias_sum`, `priorBias_nonneg`, `overreaction_form` | (23)/(11) is a probability for `0 ≤ λ ≤ 1`, plus the p. 420 rewrite |
| `cor3_case1` | Corollary 3's proof, Case 1 (p. 433): solving (A.11) for `q(·|s1)` gives (23) with `λ = y₂/y_q ≤ 1` |
| `cond_pstar_priorBias`, `pstar_deviation` | **(12)** with `s1`-dependent `α`, `λ`: `p*(·|s1) = (1-γ) p(·|s1) + γ p2`, `γ = αλ/(1+α)`; deviation from Bayes `= γ (p2 - p(·|s1))` |
| `gam_lt_one`, `gam_nonneg`, `gam_nonpos`, `gam_eq_zero_iff` | `γ < 1` whenever `α ≥ 0`, `λ ≤ 1`; sign of `γ` is the sign of `λ`; `γ = 0` iff `α = 0` or `λ = 0` |
| `bayes_of_alpha_zero`, `bayes_of_lam_zero` | `α(s1) = 0` or `λ(s1) = 0` gives Bayesian interim choice |
| `cond_eq_marg2_of_product`, `priorBias_of_neutral`, `bayes_of_product` | **why a product `p` gives Bayes (p. 429):** every signal is neutral, `p(·|s1) = p2`, so (23) collapses to `p(·|s1)` for every `λ` |
| `cond_eq_marg2_of_subsingleton` | `S1` a singleton: the one signal is neutral |
| `not_bayes_of_nonneutral` | conversely, `γ ≠ 0` and a non-neutral signal move `p*(·|s1)` off `p(·|s1)` |
| `priorBias_absCont` | Reg2 plus (23) with `λ(s1) ≠ 0` force `p2(s2) = 0` wherever `p(s2|s1) = 0` (not stated in the paper) |
| `sensitivity_scaled` | 2.3.1: with equal `γ` at two signals, `p*` differences are `(1-γ)` times the Bayesian ones |
| `confirm_marginal`, `confirm_iff` | 2.3.2 / (13): `p1(b) - 1/2 = (2θ-1)(p2(B) - 1/2)`, so `p2(B) > 1/2` iff `p1(b) > 1/2` for `θ > 1/2` |
| `representativeness` | 2.3.3: `q(A|a) = 1`, `p(A|a) < 1`, `α > 0` give `p*(A|a) > p(A|a)` |
| `sampleBias_nonneg_iff` | fn 10: `(1-λ)p(s|s) + λ ≥ 0` iff `p(s|s) ≥ -λ/(1-λ)` (`λ < 1`) |
| `lie_violation_example` | **Section 4, explicit, `norm_num`:** `p = [[2/5,1/10],[1/5,3/10]]`, `q(·|s1) = δ_B`, `α ≡ 1`, `f = (1,-1)`: `q ≪ p`, `E_{p2} f = 1/5 > 0`, and (8) strictly prefers `-f` at both signals (`E_{p*(·|a)} f = -1/5`, `E_{p*(·|b)} f = -3/5`) |
| `pstar_average`, `pstar_average_const` | under (23), `E_{p1}[p*(·|s1)] = p2 + ∑ p1 γ (p2 - p(·|s1))`; **with `γ` constant the LIE holds** |
| `priorBias_no_uniform_reversal` | **under (23), for every act some signal has `E_{p*(·|s1)} v ≥ E_{p2} v`**, so the Section 4 reversal is impossible for a Prior-Bias agent |
| `dampedTarget`, `priorBias_eq_damped`, `compromise_eq_damped` | **Paper B:** (23) and (12) are the damped target `(1-ω)·current + ω·delivered` with `current = p2(s2)`, `delivered = p(s2|s1)`; `ω = 1-λ` for `q`, `ω = 1 - αλ/(1+α)` (`omegaEff`) for `p*` |
| `omegaEff_eq`, `omegaEff_pos`, `omegaEff_positive`, `omegaEff_negative`, `omegaEff_surj` | `ω = (1+α(1-λ))/(1+α)`; always `> 0`; Positive Prior-Bias with `α > 0`: `1/(1+α) ≤ ω < 1`; Negative: `ω ≥ 1`; every `ω > 0` is attained |
| `damped_deviation`, `omega_identified` | `target - delivered = (1-ω)(current - delivered)` (the scalar content of `JeffreyOrder.dampedB_deviation`); `ω` is identified whenever `current ≠ delivered` |
| `dampedJeffrey_eq_mix` | the project's damped Jeffrey step equals `(1-ω)·Q + ω·(full Jeffrey update)` on the whole joint |

`sympy/check_epstein.py` (41/41 PASS, exit 0, about 18 s) checks all of the
above symbolically (a free 2×3 joint, `s1`-dependent `α`, `λ`) or in exact
rationals. It also includes:

* a built (13) instance (`θ = 3/4`, `p2(B) = 3/5`, so `p1(b) = 11/20`, with
  `γ*(t) = (1-t)/2`). The conflicting signal `a` gets `γ(a) = 1/11 > γ(b) = 0`,
  so `p(A|a) = 2/3 > p*(A|a) > p2(A) = 2/5`. With `γ` varying by signal,
  `E_{p1}[p*(·|s1)] = (107/275, 168/275) ≠ p2 = (2/5, 3/5)`, a Prior-Bias
  violation of the LIE in the weak sense;
* representativeness on the same instance: `p*(A|a) = 5/6 > p(A|a) = 2/3` at
  `α = 1`;
* a 3000-draw exact random sweep (`|S1|, |S2| ∈ {2,3,4}`, `λ ≤ 1`, `α ≥ 0`),
  with no draw showing a uniform reversal under (23);
* a Reg2 counterexample: `p(B|a) = 0 < p2(B) = 1/4` with `λ(a) = 1/2` gives
  `q(B|a) = 1/8`, which is not `≪ p`.

## What formalizing revealed

1. **The Section 4 violation needs a `q` outside Prior-Bias, or a `γ` that
   varies with the signal.** The p. 430 claim is that "there exist acts `f`"
   with `{f} ≻ {-f}` at `t = 0` and `-f` chosen at every `s1`. It is true for
   some specifications (`lie_violation_example`). It is not true of every
   non-Bayesian agent in the model. Under (23), with `λ ≤ 1` and `α ≥ 0`, some
   signal always has an interim value at least the time-0 value
   (`priorBias_no_uniform_reversal`). The key fact is `γ < 1`. With `γ`
   constant across signals, the interim posteriors average back to `p2`, so
   the law of iterated expectations holds exactly (`pstar_average_const`).
   A Prior-Bias agent violates it only when `γ` depends on `s1`, as in the
   confirmatory-bias example. So "our non-Bayesian agent violates the law of
   iterated expectations" should be read as "can violate". This qualifies the
   paper, not Paper B.
2. **(23) is the temptation posterior. The posterior the agent acts on is
   (12).** Its weight on the prior marginal is `γ = αλ/(1+α)`, not `λ`. The
   two agree only as `α → ∞`. Since `γ < 1`, the acting posterior always
   keeps some weight on the Bayesian update.
3. **Reg2 constrains (23).** `q(·|s1) ≪ p(·|s1)` with `λ(s1) ≠ 0` means that
   `p(·|s1)` must charge every state that `p2` charges (`priorBias_absCont`).
   A Prior-Bias agent with `λ(s1) ≠ 0` therefore never receives a signal that
   rules out an a priori possible state. After such a signal she must have
   `λ(s1) = 0` (Bayes) or `α(s1) = 0`. The paper does not say this. Its
   examples respect it when `p(a|A) < 1`, and in 2.3.4 when `p(s|s) > 0`.
4. **The product-measure remark means an uninformative signal.** A product `p`
   makes every signal neutral, `p(·|s1) = p2` (`cond_eq_marg2_of_product`).
   Then (23) returns `p(·|s1)` for every `λ`. There is no updating to distort.
5. **The paper's `γ*` has range `[0,1]` (p. 421).** Under the representation,
   `γ = αλ/(1+α) ∈ [0,1)` for `λ ∈ [0,1]`. The value `γ = 1` is a limit that
   no representation reaches. This is minor.
6. **(10) is the dual of Jeffrey conditioning.** `p*` keeps the partition
   marginal `p1` and replaces the conditionals `p(·|s1)` (`marg1_pstar`,
   `cond_pstar`). A Jeffrey step on the `S1` partition does the opposite: it
   keeps the conditionals and replaces the marginal.

## Bearing on Paper B

**Question.** Does Epstein's (23) coincide with the project's partial-adoption
step on a single marginal, and which of his quantities plays `ω`?

**Answer: yes, exactly, state by state** (`priorBias_eq_damped`,
`compromise_eq_damped`). The project's target is
`(1-ω)·current + ω·delivered`, written in `JeffreyOrder/Anchoring.lean` as
`dampedTarget Q r₀ δ = (1-δ)·Q.mB1 + δ·(1-r₀)`. Set `current = p2(s2)`, the
prior marginal, and `delivered = p(s2|s1)`, the Bayesian posterior. Then:

* the temptation posterior `q(·|s1)` of (23) is the damped target with
  `ω = 1 - λ(s1)`;
* the posterior that governs choice, `p*(·|s1)` of (12), is the damped target
  with `ω = 1 - α(s1)λ(s1)/(1+α(s1)) = (1 + α(1-λ))/(1+α)`.

The second is the behavioural counterpart of `ω`, because (8) is expected
utility under `p*(·|s1)`.

The damped-deviation identity carries over as
`p*(·|s1) - p(·|s1) = (1-ω)(p2 - p(·|s1))`, the scalar content of
`dampedB_deviation`. The project's damped Jeffrey step is itself a (23)-type
mixture of "no update" and "full Jeffrey update" on the whole joint
(`dampedJeffrey_eq_mix`).

**Differences.**

* **Input.** Epstein's input is a signal `s1` with likelihoods, and his
  "delivered" belief is *computed* from a joint prior on `S1 × S2` by Bayes. In
  Paper B the credence is delivered directly, as a Jeffrey input, with no
  likelihood.
* **What is mixed in.** Epstein's mixing target is the time-0 prior marginal
  `p2`. Paper B's is the marginal in force when the cue arrives, which after
  the first cue is no longer the prior. With one signal the two readings
  coincide.
* **Number of signals.** Epstein has one interim signal. Nothing in the paper
  concerns the order of several signals, so it has no order effect to compare.
* **Range of `ω`.**
  * Positive Prior-Bias with `α > 0` gives `ω ∈ [1/(1+α), 1)`, which is
    interior.
  * Negative Prior-Bias gives `ω ≥ 1`, overshooting the delivered belief. This
    is outside Paper B's `[0,1]`.
  * `ω = 0` (the cue ignored, the project's `δ = 0` endpoint) is never
    attained by the acting posterior (`omegaEff_pos`). It is attained by the
    temptation posterior at `λ = 1`, and by `p*` only in the limit `α → ∞`.
  * Every `ω > 0` is attained by some `(α ≥ 0, λ ≤ 1)` (`omegaEff_surj`).
* **Source of the weight.** Epstein's weight is derived from preferences over
  contingent menus: `α` from (22), and `λ` from `q`, which is unique when
  `s1` violates strategic rationality (Lemma 1, Corollary 1 (20)). At
  `α(s1) = 0`, `λ(s1)` is not identified. The project's `ω` is a free
  parameter.

Epstein is therefore a legitimate citation for the *averaging form* of partial
adoption as an axiomatized non-Bayesian rule. He is not a citation for anything
about sequences or order.

## Audit findings (2026-09-29/30)

This section summarises `notes/citation_audit.md` items M6, M8 and the two
2026-09-30 Epstein entries, together with
`notes/citation_audit/verify_updating_theory.md` §5 (E1, OE1, E2). It sets them
against the primary.

* **M6 / E1: MS:276, "\citet{Epstein2006} makes the updating rule subjective".
  VERIFIED.** The survey-based NOT-CHECKABLE verdict is overturned.
  * The abstract (p. 413) says the main result "generalizes (the dynamic
    version of) Anscombe-Aumann's theorem so that both the prior and the way in
    which it is updated are subjective".
  * The introduction repeats it (p. 413): "render it more fully
    subjective—both the prior and the way in which it is updated are
    subjective".
  * The review's description of the paper as a temptation model is also
    accurate (pp. 413-416). The two descriptions do not conflict.
* **M8 / OE1: the framing "used ... to explain whether sequential conditioning
  is sequence-independent or not". UNSUPPORTED for Epstein.** Confirmed.
  * The model has three periods and a **single** interim signal (abstract
    p. 413; p. 416; primitives p. 417). Theorem 1 concerns that one signal.
  * The repeated-trials reading of 2.3.4 (`S1 = S2 = S`, fn 15) is still one
    observation.
  * Section 4 says the model "has nothing to say about how behaviour is
    connected across state spaces" (p. 429).
  * No passage concerns the order of signals.
* **Adoption-weight entry, M6-M8 (c): "the same averaging form as the paper's
  partial adoption (weight 1 - omega on what was held before)". Confirmed,
  with two qualifications.**
  * The identification `λ = 1 - ω` holds for the **temptation** posterior `q`
    of (23). The posterior that governs choice is (12), with
    `ω = 1 - αλ/(1+α)`. Cite (12), or say "the tempting posterior", when
    drawing the parallel.
  * `ω = 0` is not attained by the acting posterior, and Negative Prior-Bias
    gives `ω > 1`.
  * "Axiomatized from preferences over contingent menus" is right: this is
    Corollary 3 via Axioms 9-11. `λ(s1)` is identified only where
    `α(s1) > 0` and `q(·|s1) ≠ p(·|s1)` (Corollary 1, (19)-(20)).
* **The "product measure" caution. Confirmed.** A product `p` makes every
  signal neutral (`cond_eq_marg2_of_product`), so the case is "the signal
  carries no information about `S2`". It is not an analogue of Proposition IMM
  at `c = 0`.
* **Qualification to the Ortoleva-based entry, M6-M8 re-read (b), and to
  verify_updating_theory.md §5.** Both repeat the review's list: "under- and
  overreaction, base-rate neglect, sample bias, and representativeness"
  (Ortoleva 2024, p. 558).
  * The primary has **no base-rate-neglect example**. Section 2.3 has four
    subsections: underreaction and overreaction, **confirmatory bias**,
    representativeness, and sample bias (pp. 420-421).
  * Any text that lists Epstein's biases should use the primary's list.
  * The review's other suggestion, "models departures from Bayes' rule as
    temptation", is accurate but is no longer needed now that M6 is verified.
* **E2 / M18: bibliographic entry.** Confirmed: RES 73, pp. 413-436, as
  printed on p. 413. The copy does not print the issue number (2) or the DOI,
  so M18 stands for the DOI.
* **New, affecting the paper and not Paper B.** The Section 4 LIE remark holds
  for some specifications but not for every non-Bayesian agent in the model
  (point 1 of "What formalizing revealed"). Paper B does not cite it.

### Proposed corrected wording (not applied)

* **MS:276**, replace "Several alternatives to Bayesian updating have been used
  in the decision theory literature to explain whether sequential conditioning
  is sequence-independent or not. For example, \citet{Epstein2006} makes the
  updating rule subjective, and \citet{Ortoleva2012} axiomatises departures
  triggered by unexpected news." with:
  > Decision theory has axiomatised several alternatives to Bayesian updating,
  > each for a single piece of news. \citet{Epstein2006} makes the updating
  > rule, and not only the prior, subjective. His agent is tempted by a revised
  > view of the world after a signal, and acts on a compromise between the
  > Bayesian posterior and the tempting one. \citet{Ortoleva2012} axiomatises
  > departures triggered by unexpected news. Neither concerns the order in which
  > several pieces of news arrive.

  (The Cripps sentence that follows is item M9 and is not addressed here.)
* **Optional footnote at the same place**, if the adoption weight is to be
  linked to Epstein:
  > Under Epstein's Prior-Bias axiom (his Corollary 3, eq. 23), the posterior
  > that guides interim choice is $(1-\gamma)\,p(\cdot\mid s_1) + \gamma\,p_2$,
  > with $\gamma = \alpha\lambda/(1+\alpha)$ (his eq. 12). This is the averaging
  > form of partial adoption. The Bayesian posterior plays the delivered belief,
  > the prior marginal plays the current one, and the adoption weight is
  > $\omega = 1-\gamma$. His input is a single signal with likelihoods rather
  > than a delivered credence. His weight is derived from preferences, and it
  > never reaches $\omega = 0$.
* **Wherever Epstein's examples are listed**, replace "base-rate neglect" with
  "confirmatory bias", or cite the list to Ortoleva (2024) rather than to
  Epstein.

## Not formalized

* Theorem 1 in either direction, together with the preference side: the axioms,
  contingent menus and the Hausdorff topology. Only the belief objects that
  the representation produces are formalized.
* Lemma 1 and Corollaries 1-2.
* Corollary 3's Motzkin step and its Case 2. Only Case 1's algebra is
  formalized (`cor3_case1`).
* Theorem 2 (Appendix B).
* The introduction's portfolio example (3), which is qualitative with no
  numbers.
