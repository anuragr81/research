# Bohren, J. A., Imas, A. & Rosenberg, M. (2019), "The Dynamics of Discrimination: Theory and Evidence"

*American Economic Review* 109(10), 3395-3436. (Drive copy `bohren3.pdf` is the
**January 2019 working paper**, 83 PDF pages. All page numbers below are that paper's
printed pages; printed page = PDF page - 1. They are not AER pages.)

## Claims formalized

The normal-normal model of Section 2 and Appendix A.1. Ability `a ~ N(μ_g, 1/τa)`,
quality `q_t = a + ε_t`, signal `s_t = q_t + η_t` (pp.9-10). An evaluator of type `i`
has subjective prior mean `μ̂_g` and taste `c_g` (`c_M = 0`), and updates by Bayes'
rule (p.11, "Belief-Updating").

* **Eq. (2)** (p.11): `v_i(h,s,g) = Ê_i[q|h,s,g] - c^i_g`.
* **Eq. (3)** (p.12): `D_i(h,s) = v_i(h,s,M) - v_i(h,s,F)`. The unsubscripted `D(h,s)`
  means the aggregate `E_π[D_i]`.
* **Eqs. (4)-(5), Proposition 1** (p.16):
  `D(h₁,s₁) = τq/(τq+τη) (μ̂_M - μ̂_F) + c_F`, with `τq = τaτε/(τa+τε)` (p.15).
  Belief-based discrimination decreases in `τη`. Preference-based discrimination is
  constant in it, and survives `τη → ∞`.
* **Aggregate formula** (p.17, introduced as an extension):
  `D = τq/(τq+τη) E_π[μ̂_M - μ̂_F] + E_π[c_F]`. **Eq. (7)** (p.20) is the two-type case.
* **Footnote 9** (p.16): observational equivalence of the two sources at one parameter
  set.
* **Footnote 10** (p.19): with *exogenous* informational content, no reversal
  "would follow immediately".
* **Proposition 2** (p.18; Lemma 1 pp.47-49, Lemma 2 pp.49-50, Lemma 3 pp.50-51):
  with a single type, belief-based partiality and no taste, "fixing an evaluation
  history, discrimination decreases across periods but never reverses".
* **Proposition 3** (p.21; proof pp.51-54): with a heuristic type (share `p`) and an
  impartial type, aggregate discrimination reverses in period 2 for small enough `p`.

## Result

`lean/BohrenImasRosenberg.lean` is a symlink to `lean/Literature/BohrenImasRosenberg.lean`.
It compiles as a single file. It has no `sorry`, and its headline theorems use only the axioms
`[propext, Classical.choice, Quot.sound]`.

* `complete_square`, `posterior_kernel` -- the conjugacy step (pp.15-16), stated as
  the completing-the-square identity of the Gaussian kernels. After this step the
  posterior mean is used as a definition (`postMean`).
* `disc_initial` (eq. 5), `disc_aggregate` (p.17), `disc_eq7` (eq. 7),
  `fn9_equivalent`, `eval_strictMono_prior`.
* `prop1_decreasing`, `prop1_constant`, `prop1_limit` -- Proposition 1. The limit is a
  `Filter.Tendsto` statement.
* `exo_gap` -- footnote 10. With the evaluator reporting `s`, the posterior-mean gap
  is `τa/(τa+τεη)` times the prior gap.
* `endo_gap_eq12` -- eq. (12) exactly as printed, with channel (i) `τa/(τa+τεη)` and
  channel (ii) `-τεητq/((τa+τεη)τη)`.
* `endo_gap`, `channels`, `endo_strictMono` -- **the eq. (12) coefficient equals
  `τa/(τa+τε)`**. The two channels always net to this value.
* `endo_faster` -- `τa/(τa+τε) < τa/(τa+τεη)`.
* `belief_gap`, `discAt_eq` (Lemma 3 / eq. 14), `prop2_no_reversal`, `prop2_decreasing`
  -- **Proposition 2 in full**, along an arbitrary history `v : ℕ → ℝ`. After `n`
  evaluations the gap is `∏_{j<n} τa(j)/(τa(j)+τε)` times the prior gap, with
  `τa(j) = τa + jτεη`.
* `impartial_favors_female`, `m1_gt_m2`, `disc2_factor`, `disc2_endpoints`,
  `prop3_sign`, `prop3_cutoff`, `prop3_condition_iff` -- the structure of
  Proposition 3. The mixture weights `A = C₁D₁`, `B = C₂D₂` are left abstract and
  positive.

`sympy/check_normal_model.py` passes 37/37 checks (symbolic ones are exact; the Proposition 3 instance
is evaluated to 50 digits):

* conjugacy, eq. (5), Proposition 1 derivative and limit;
* eq. (12) as printed, and its simplification to `τa/(τa+τε)`;
* the endogenous-minus-exogenous contraction, `τaτε²/((τa+τε)(τaτε+τaτη+τετη)) > 0`;
* `D_t - D_{t+1} > 0` as a ratio of positive polynomials;
* the p.54 exponent of `C₁D₁/(C₂D₂)` matches the normalising constants;
* the derivative condition in the Lean form equals the paper's form;
* **the monotonicity directions stated on p.54 are reversed** (see below);
* a full-model reversal. At unit precisions, `μ̂_M = 1`, `μ̂¹_F = 0`, `v₁ = -6` and
  `s₂ = 4`, we get `C₁D₁/C₂D₂ = 8.24 > 3`. Then `D = -0.0229` at `p = 1/20` and
  `D = +0.434` at `p = 9/10`.
* Tables 1-6: every interaction coefficient equals the difference of the separate
  regressions, as it must in a saturated model, within 0.02. So do the level
  coefficients. Also checked: the p.28 reputations (155.23 is the mean of 155.89 and
  154.57) and the observation counts (273 = 280 - 7 = 135 + 138; 135 = 140 - 5).

The two pre-existing scripts, as found (not modified):

* `sympy/check_pinning_kills_partiality.py` -- all checks pass.
* `sympy/check_reversal.py` -- **exits 1 (4/8 checks).** It uses the repo's `bayes`,
  which is Paper B's Bayes-factor benchmark `P^B`. `P^B` matches its likelihoods to
  each group's prior, so it is not BIR's evaluator, whose likelihood `s|q` is
  group-free (p.10). Under `P^B` a lower prior gets a larger Bayes factor. As a
  result, the "novice" comparison already favours women (`D = -0.0154`), and the
  checks labelled "(their Proposition 2)" fail. This is the audit's point B20 showing
  up in the code: the benchmark in that script is not the object of BIR's
  Proposition 2.

## What formalizing revealed

1. **Eq. (12)'s contraction factor is `τa/(τa+τε)`.** It does not depend on the
   evaluation `v` or on the signal precision `τη`. For an evaluator who inverts the
   evaluation through the prior, the belief gap after an evaluation contracts
   exactly as if quality itself had been observed.
2. **The exogenous case contracts more slowly.** Its factor is `τa/(τa+τεη)`
   (footnote 10), and `τεη < τε`. This gives a formal sense to p.2's "this speeds up
   the mitigation of discrimination". "Posterior mean increasing in prior mean" holds
   in both cases, but with different coefficients. The exogenous case is the
   immediate one; the endogenous case is Proposition 2's.
3. **The proof of Proposition 2 is incomplete as printed.** On p.51 the proof reads
   "It remains to show that discrimination decreases", displays the period-`t+1`
   expression, and ends. `prop2_decreasing` supplies the missing comparison. It
   holds for any positive next-period precision, not only `τa + τεη`. The closed
   form of the difference is in the sympy output.
4. **The p.54 text has both monotonicity directions reversed.** It says
   `C₁D₁/(C₂D₂)` "is increasing in `v₁` and decreasing in `s₂`, and becomes
   arbitrarily large as `v₁` approaches negative infinity or `s₂` approaches
   infinity". The derivatives show the ratio is *decreasing* in `v₁` and
   *increasing* in `s₂`. The limits and Proposition 3 itself are right; only the two
   monotonicity words are wrong.
5. **Proposition 3 has an exact sign characterization.** With the mixture weights
   `A, B > 0` held fixed, `D(p)` has the sign of `(1-p)L₀ + pL₁`, where
   `L₀ = B(m₂-f₁) - A(m₁-m₂)` and `L₁ = A(m₂-f₁) > 0`. The consequences:
   - If `L₀ < 0`, the reversal holds exactly on `p ∈ (0, L₀/(L₀-L₁))`, and `D > 0`
     above that cut-off. This is the non-monotonicity of Figure 1.
   - If `L₀ ≥ 0`, no `p` reverses.
   - `L₀ < 0` is exactly the paper's condition
     `1 < τε²/((τε+τη)(τa+τε))(1 + C₁D₁/C₂D₂)` (`prop3_condition_iff`).
6. **The impartial type discriminates against men for every `v₁` and every
   `p ∈ (0,1)`** (`impartial_favors_female`). This evaluator has no partiality, a
   correct model and Bayes' rule. Its discrimination is produced by the endogenous
   informational content of the history.

## Bearing on Paper B

* **Proposition 2 is a theorem about Bayesian updating on endogenous histories.**
  In the normal model it holds with a *uniform* contraction factor (points 1-3 above).
  Paper B's non-Bayesian (Jeffrey, reading-order) mechanism therefore needs no
  "scope condition" in BIR. BIR assume Bayes' rule explicitly (p.11), and a
  reading-order effect sits outside their model rather than in a gap within it.
* **BIR's framework contains process-produced discrimination.** The impartial type
  (point 6) has correct beliefs, no animus and Bayesian coherence, yet it
  discriminates. So "person-based versus process-based" cannot be what distinguishes
  Paper B from BIR. What does distinguish it: BIR's process runs through *other
  evaluators' past evaluations* (a social-learning history). Paper B's runs through
  *one evaluator's reading order* of two correlated cues, and BIR's signal
  `s = q + η` is group-free (p.10).
* **The Bayes-factor benchmark `P^B` is not BIR's evaluator** (see
  `check_reversal.py` above). Any comparison of Paper B's routes with "BIR's
  Bayesian" has to use a group-free likelihood (as `check_pinning_kills_partiality.py`
  does), not `P^B`.

## Not formalized

* The general-distribution robustness of Proposition 2 (Appendix A.2, pp.54-59). It
  rests on MLRP preservation and on numerical grids.
* Coarse evaluations, Proposition 4 and Lemmas 4-5 (pp.59-65). These use the
  Keilson-Sumita convolution theorem and a Mills-ratio inequality that the paper
  cites from Stack Exchange (fn 22, p.63).
* Shifting standards (pp.65-66).
* The exact Gaussian forms of `C₁, C₂, D₁, D₂` inside Lean. They are checked in sympy;
  Lean treats them as positive weights.
* All econometrics, and Appendix C's Figure 7. The "triples to nearly half a
  quintile" claim (p.81) cannot be recomputed, because the quintile distributions are
  not reported.

## Audit findings (2026-09-29)

These are from `verify_bir_heckman.md` §1 and concern the project's notes (`notes/paper_review_log.md`,
`notes/the_discrimination_problem.tex`, `notes/positioning_economics.tex`,
`notes/manuscript_change_plan_asof_2026-09-30.md`), not the manuscript itself: `PAPER_B_MANUSCRIPT.tex`
does not cite BIR. Proposed wording is text only; nothing has been edited.

**Five CONTRADICTED items.**

1. **"Unstated scope condition"** (log Entry 9, kept as a "scope observation" in Entry
   10). Bayes' rule is stated as a model assumption (p.11, "derived using Bayes rule
   ... also using Bayes rule"). p.22 restricts result (iii) to "a correctly-specified
   model of belief-based partiality".
   *Proposed:* "Proposition 2 is proved for evaluators who update by Bayes' rule, as
   BIR assume explicitly (p.11); a reading-order effect under Jeffrey updating lies
   outside their model."
2. **Exogenous-likelihoods docstring** (`check_pinning_kills_partiality.py:17-20, 28,
   123`: "Bayesian evaluator with EXOGENOUS likelihoods (what BIR assume) ... This is
   the object their Proposition 2 is about"). Proposition 2 concerns the *endogenous*
   informational content of evaluations. The exogenous case is the one footnote 10
   (p.19) calls immediate. The contraction factors differ (`exo_gap` vs `endo_gap`).
   *Proposed:* "Bayesian evaluator with a group-free signal likelihood, as in BIR's
   first period (eqs. 4-5, p.16) and their footnote 10 (p.19). BIR's Proposition 2
   concerns the endogenous case, in which the evaluation history's informational
   content depends on the prior."
3. **"Exhaustive trichotomy"** (TDP:279-280, live). BIR "allow for three potential
   sources" (p.2). They call their reversal mechanism "one possible way ... Other forms
   of misspecification can also lead to reversals" (p.19). §2.5.2 (pp.24-25) adds
   attrition, variance differences, heterogeneous preferences and self-fulfilling
   beliefs.
   *Proposed:* "BIR distinguish three sources -- correct beliefs, biased beliefs and
   preferences (p.2) -- and discuss further alternatives (pp.24-25)."
4. **"Defect of the evaluator: set the belief gap and the preference gap to zero and
   nothing remains"** (TDP:150-156, 279-285). BIR's impartial type has zero belief gap
   and zero taste, yet "discriminates against males in the second period" (p.20;
   `impartial_favors_female`). "Nothing remains" holds only if *every* type has zero
   gaps (eqs. 5, 7).
   *Proposed:* "In BIR, discrimination arises from evaluators' partiality or from the
   informational content of a social-learning history (the impartial type, p.20). In
   Paper B it arises within a single evaluator, from the order in which two correlated
   cues are read."
5. **"Biased priors overshooting as signals accumulate"** (log Entry 5, uncorrected).
   Proposition 2 says a single biased type's beliefs never reverse (`prop2_no_reversal`).
   The reversal comes from the impartial type's inference about the heuristic type
   (pp.19-21). The advanced accounts' history is reputation ≥ 100, mean 155.23 (p.28);
   calling it "long" is a gloss.
   *Proposed:* "no history versus a high-reputation history (≥ 100, p.28) ... in BIR's
   model the reversal comes from impartial evaluators discounting women's histories for
   the stricter standard applied by biased evaluators (pp.19-21), not from biased
   priors overshooting, which Proposition 2 rules out."

**Page and version errors.**

* All pinpoints are **working-paper** pages but are paired with the AER citation
  (3395-3436). *Proposed:* cite "Bohren, Imas & Rosenberg (2019, working paper,
  January)" with these pages, or re-map every pinpoint to the AER text.
* The aggregate formula `D = τq/(τq+τη) E_π[μ̂_M - μ̂_F] + E_π[c_F]` is on **p.17**, not
  p.16, where it is an extension ("an analogue to Proposition 1 immediately
  follows"). p.16 has eq. (5) and Proposition 1. Also keep the `π` subscript.
* "The posterior mean is increasing in the prior mean" is on **p.19**, not p.18.
* The Proposition 3 statement is on p.21; the mechanism is on pp.19-20.

**Overstatements to soften** (VERIFIED-WITH-CAVEAT in the audit):

* "The only way to extinguish the belief channel is perfect objectivity" (plan 3.D).
  *Proposed:* "at a given history, the belief channel vanishes only as `τη → ∞`;
  along a history it also shrinks every period (Proposition 2)".
* "Requires two types". *Proposed:* "BIR exhibit one mechanism, with two types (p.19:
  'a possibility result')".
* "An observed reversal identifies bias". *Proposed:* "... indicates some form of
  misspecification (pp.18-19), once attrition, variance differences and
  autocorrelation are ruled out (pp.24, 37-39)".
* "Their entire empirical inference rests on Proposition 2's corollary". The
  preference-versus-belief inference rests on Proposition 1 (pp.31-33).
