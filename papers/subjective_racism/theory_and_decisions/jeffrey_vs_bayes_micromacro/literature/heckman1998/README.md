# Heckman, J. J. (1998), "Detecting Discrimination"

*Journal of Economic Perspectives* 12(2), 101-116. (The Drive copy `heckman_detecting.pdf`
is the publisher-typeset article. Its pages are the journal's own, 101-116. The "2011"
in the file metadata is a download date.)

## Claims formalized

The formal model in the Appendix, "Implicit Identifying Assumptions In The Audit
Method" (pp.112-115), and the claim it is built to support (p.102): "The audit method
can find discrimination when in fact none exists; it can also disguise discrimination
when it is present". The argument for this claim is on pp.108-111 and is "made more
precise in the Appendix" (p.111).

* Productivity is `P = X₁ + X₂ + f`, and race `r ∈ {1 black, 0 white}` does not
  affect it (p.112). The audit pair is matched on `X₁ = X₁*` and sent to the same firm.
  `X₂` is unobserved by the auditor but acted on by the firm.
* **Linear treatment** `T(P,r) = P + γr` (p.112). The pair difference is
  `X₂¹ - X₂⁰ + γ` (p.113). Averages estimate `γ` without bias only if
  `E X₂¹ = E X₂⁰`, which Heckman calls "the crucial identifying assumption" (p.113).
  In the text this is assumption (b), about which he says "Nothing guarantees that it
  will be satisfied" (p.109).
* **The two-component example** (p.108). Mean productivity is equal, but blacks lead
  on one component and whites on the other. An audit that matches the black-leading
  component is "biased toward a finding of discrimination".
* **Threshold treatment**: hire iff `P ≥ c` (p.113), with `c₁ > c₀` under
  discrimination (p.114). If the unobservables differ in dispersion, the audit can
  show discrimination, reverse discrimination or equal treatment when there is none
  (p.111). With discrimination it "can find no discrimination at all" (p.111), or
  appear to favour blacks (p.115).

## Result

`lean/Heckman.lean` is a symlink to `lean/Literature/Heckman.lean`. It compiles as a
single file, has no `sorry`, and uses only `[propext, Classical.choice, Quot.sound]`.

* `audit_diff_linear`, `audit_mean_linear`, `audit_unbiased_iff` -- the linear case.
  It covers any finite population of pairs with weights summing to one.
* `two_component_bias` -- the p.108 example. With `γ = 0` the expected audit
  difference is `-1`.
* Threshold hiring with two-point unobservables of mean `0`: spread `1` for blacks and
  `3/2` for whites. These have variances `1` and `9/4`, which is Figures 1-2's ratio
  `Var(X₂⁰) = 2.25 Var(X₂¹)`. All examples are proved by `norm_num`.
  - With a common cut-off `0` (no discrimination):
    - `no_disc_finds_disc`: at `x₁ = -5/4` blacks are hired at rate 0 and whites at 1/2;
    - `no_disc_finds_reverse`: at `x₁ = 5/4` the rates are 1 and 1/2;
    - `no_disc_finds_equal`: at `x₁ = 2` both rates are 1.
  - With cut-offs `c₁ = 1/4 > c₀ = 0` (Figure 2's values):
    - `disc_disguised`: at `x₁ = 3` both rates are 1;
    - `disc_reversed`: at `x₁ = 5/4` blacks are hired at 1 and whites at 1/2;
    - `disc_shown`: at `x₁ = -5/4` the audit shows the discrimination correctly.
  - `phase_high`, `phase_low` -- the general pattern for any spreads `0 ≤ d₁ < d₀`.
    The standardization level relative to the cut-off decides which group the audit
    favours.
  - `equal_means_not_enough` -- equal means do not rescue the threshold case (p.109).

`sympy/check_audit_model.py` passes 16/16 checks:

* the linear identity, the two-component example and all the threshold examples;
* **Figures 1-2 recomputed** with normal unobservables, `f = 0`, from sympy's `erf` at
  30 digits. The captions' parameters reproduce the plotted curves: ratio 0.059 at
  `X₁* = -3`, 1.126 at `X₁* = 1` and 1.022 at `X₁* = 3` for Figure 1; 1.056 at
  `X₁* = 2` for Figure 2;
* **Table 1 (p.105) recomputed** cell by cell (see below).

## What formalizing revealed

1. **Figures 1-2's titles contradict their captions.** Both titles say "Blacks Have
   More Dispersion", but both captions set `Var(X₂⁰) = 2.25 Var(X₂¹)`: *whites* are more
   dispersed. The captions are the ones that reproduce the plotted curves. With the
   titles' parameters, Figure 1's ratio at `X₁* = -3` would be 16.9, far off the
   plotted axis, which stops at 1.2. The p.115 text ("the black hire rate falls short
   of the white rate if `X₁* < 0`") also matches the captions. Figure 2's title also
   says "No Discrimination against Blacks", although its caption has `c₁ = 0.25 > c₀ = 0`
   and p.115 describes it as the discrimination case.
2. **Table 1 has a count error.** Denver pair 4, "White Yes, Black No", is printed
   "(2) 6.7%". With a count of 2 the row sums to 16 of 15 audits, and 2/15 = 13.3%. A
   count of "(1)" reconciles the row (1/15 = 6.7%) and the Denver column total (7).
   Washington pair 5's "Equal Treatment" is printed 77.6%, but (7 + 26)/42 = 78.6%.
   Denver pair 1's 72.1% (13/18 = 72.2%) is a rounding slip. The table is reproduced
   from Heckman & Siegelman (1993), so these may be transcription errors.
3. **Two separate arguments, both formal and both elementary.** One is a mean shift
   under linear treatment (`audit_unbiased_iff`, `two_component_bias`). The other is a
   dispersion difference under threshold treatment (`phase_high`, `phase_low`). Neither
   involves sample size: each is a statement about the estimand. Averaging more pairs
   drawn from the same distributions converges to the same biased value.
4. With two-point unobservables, every sign pattern Heckman describes is realized
   *exactly*, at finitely many standardization levels. Normal tails are not needed.

## Bearing on Paper B

* Paper B's abstract closes on a point about audits: "whether the effect of order is
  detectable or not depends on the kind of question being asked rather [than] on the
  number of evaluators considered". Heckman is a natural economics precedent for
  *structural* limits on audits, because his biases sit in the estimand. But he never
  mentions sample size (see the audit finding below). "Not repaired by sample size" is
  an inference from his framing, and should be worded as the author's.
* The two papers make different kinds of objection. Heckman's concern the *design*
  (what is matched, and where the audit standardizes), under a threshold or linear
  hiring rule. Paper B's concern which *statistic* of a belief or decision population
  registers a reading-order effect. The shared structure is that an aggregate can be
  uninformative, or wrongly signed, for reasons unrelated to precision.

## Not formalized

* The market-versus-firm argument (pp.102-103, Becker's marginal firm).
* The empirical discussion of Darity-Mason and Neal-Johnson (pp.103-107), and the
  Becker-model remark (pp.111-112).
* The normal-distribution Figures are checked numerically in sympy, not in Lean.
* The claim that matching "in general reduces" bias under equal means (p.113) is
  stated there without a formal comparison, and is not formalized.

## Audit findings (2026-09-29)

These are from `verify_bir_heckman.md` §2 (`notes/positioning_economics.tex`,
`notes/the_discrimination_problem.tex`, `notes/manuscript_change_plan_asof_2026-09-30.md`,
`notes/paper_review_log.md`). Proposed wording is text only.

1. **"No sample size repairs" is the project's gloss, not Heckman's** (UNSUPPORTED as
   an attribution; TDP:242-244, plan 3.C lines 894-899, log:688-690). Heckman never
   discusses the number of audits. His one size-related remark, "the small pools of
   applicants from which matched pairs are constructed" (p.108), explains why matching
   is imperfect. The inference itself is sound (point 3 above): the bias is in the
   estimand.
   *Proposed:* "Heckman (1998) showed that audit-pair estimates rest on assumptions
   about unobserved productivity -- equal means under linear treatment, equal
   distributions under threshold hiring (pp.108-115). Because the resulting bias is in
   the estimand, it does not diminish with more audits."
2. **"Nothing guarantees" is on p.109, not p.102** (WRONG-LOCATION, PE:42-45, 93-94).
   It refers to assumption (b), equal means of the unobserved productivity. The
   bibliography note "quotation from p.102" should list both pages, pp.102 and 109.
3. **The p.102 sentence is a general summary, not a claim specific to threshold
   hiring.** Cite pp.109-111 and 113-115 for the threshold mechanism.
4. **"Turns on unobserved heterogeneity meeting a threshold, and is repaired by
   assumptions"** (PE:59-60) leaves out the mean-shift argument (p.108, p.113).
   "Repaired" should be qualified: Heckman calls the needed assumptions "untested and
   unverifiable" (p.111), requiring "a priori knowledge that is typically not
   available" (p.108).
   *Proposed:* "turns on unobserved productivity differing across groups -- in mean,
   or in dispersion when hiring is by threshold -- and can be avoided only by
   assumptions Heckman calls untested and unverifiable (p.111)."
5. **"Concerns a design rather than a formal model"** (TDP:244-245). The target is a
   design, but Heckman does give a formal model (Appendix, pp.112-115), which is what
   this record formalizes.
