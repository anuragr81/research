# Citation audit: gaps, overclaims and mistakes, with a todo list

Opened 2026-09-29. Every claim the manuscript, the notes, the change plan, the
literature READMEs and the Lean/SymPy comments make about a cited paper is
being checked against the full text of that paper, read from the Drive folder
`temp` (id `14Jw8TW7gJ7kKSVuPSTvhRh1rW-z1FXTC`). One agent per group of
papers; each wrote a per-claim table with page-cited evidence, now in
`notes/citation_audit/verify_*.md` (file:line locations there are as of
commit cf7f70e2). Paths written "Drive:temp/<file>" are the local names the
Drive files were downloaded under.

Result of the pass (2026-09-29): about 400 claims checked across 32 papers.
No quotation in the manuscript is misquoted; every number (Asch Table 7,
Garber, Hawthorne, Diaconis-Zabell, Augenblick-Rabin, Zhao) reproduces. The
failures are attribution and characterisation: the load-bearing ones are
M22-M24 (Diaconis-Zabell), M9 (Cripps), M5 (Dietrich), P10-P12
(Hogarth-Einhorn primacy/recency and the position channel), D1 (Doring is
normative), D13 (BIR trichotomy) and M17 (missing Hawthorne bib entry). The
paper's own mathematics is unaffected: SymPy 15/15, Lean builds with no
`sorry` and standard axioms only.

**Nothing here has been applied.** Each entry names the location, the
verdict, the evidence, and what to do. Status: `[ ]` open, `[x]` done,
`[~]` decided not to act. "MS" is `PAPER_B_MANUSCRIPT.tex`, "PLAN" is
`notes/manuscript_change_plan.md`, line numbers as of commit cf7f70e2.

Verdicts: VERIFIED, VERIFIED-WITH-CAVEAT, MISQUOTED, WRONG-LOCATION,
WRONG-NUMBER, UNSUPPORTED (not found in the paper), CONTRADICTED (the paper
says otherwise), NOT-CHECKABLE (source not available).

## Groups and status

| Group | Papers | Report | Status |
|---|---|---|---|
| A | Hawthorne 2004, Weisberg 2009 | verify_hawthorne_weisberg.md | done, logged below |
| B | Hogarth-Einhorn 1992, Asch 1946 | verify_hogarth_asch.md | done, logged below |
| C | Diaconis-Zabell, Field, Garber, Wagner 2002/2003, Pettigrew-Weisberg | verify_kinematics.md | done, logged below |
| D | Bohren-Imas-Rosenberg 2019, Heckman 1998 | verify_bir_heckman.md | done, logged below |
| E | Doring 1999, Domotor 1980, Good-Mittal 1987 | verify_scanned.md | done, logged below |
| F | Phelps, Arrow, Coate-Loury, BCGS, Becker, FGT | verify_discrimination_econ.md | done, logged below |
| G | Cripps, Dietrich, BHW, Banerjee, Ortoleva, Epstein | verify_updating_theory.md | done, logged below |
| H | Augenblick-Rabin, Shmaya-Yariv, Zhao 2010/2012, Wilson, Cassell | verify_record_papers.md | done, logged below |
| -- | Tao 2011 | checked by hand | done |
| J | Jeffrey 1983, Jeffrey 2004 (2002 draft) | verify_jeffrey.md | done 2026-09-30, logged below |

## 1. Manuscript text (PAPER_B_MANUSCRIPT.tex)

### Section 1, Introduction

- [ ] **M1. MS:120, Domotor citation. UNSUPPORTED.** "the likelihood a
  delivered credence implies must be read against the marginal in force when
  the cue arrives \citep{Domotor1980}". Domotor never mentions likelihoods or
  marginals; he supports only bare non-commutativity (pp. 386, 395, 399) and
  treats it as making Jeffrey machines "inadequate" (p. 395), the opposite
  lean to the manuscript's. Closest passage is the Field-to-Jeffrey embedding
  on p. 397. **Do:** either cite Domotor only for non-commutativity, or drop
  the citation and let the mechanism sentence stand on Prop. DIV.
  **Confirmed formally (lean/Literature/Domotor.lean):** the Field-to-Jeffrey
  embedding makes the Jeffrey input depend on the current state
  (`field_eq_jeffrey_embed`, `embed_depends_on_state`); Domotor never frames
  it as a likelihood read against a marginal. Also: two sequential Jeffrey
  steps always are one Jeffrey step on the meet (`jeffrey_seq_on_meet`), so
  his p. 395-396 claim needs reading as "the joint input depends on P"; his
  p. 397 clause (ii) is false as stated (`field_not_convex`, corrected
  coefficient in `field_mix`). Do not cite clause (ii).

- [ ] **M22. MS:117 (also papers_dialectic.tex:64, two_horn_motivation_body.tex
  :24, PLAN :381, :404), Diaconis-Zabell. UNSUPPORTED, load-bearing.**
  "Jeffrey updating is the unique coherent revision for credence delivered on
  cue's partition". Not in D-Z: they list four legitimate routes, and their
  only uniqueness results concern distance measures (Thms 5.1, 6.1).
  "equivalent to making the minimal change of the prior consistent with the
  delivered marginal" holds for Hellinger and Kullback-Leibler; the
  variation-distance minimiser is not unique (Remark a, p. 828). **Do:** drop
  "unique coherent"; say "minimises Kullback-Leibler (and Hellinger) distance
  to the prior among distributions with the delivered marginal (D-Z,
  section 6)". This sentence carries the modelling premise, so it matters.
- [ ] **M22-M23 settled formally (2026-09-30,
  lean/Literature/DiaconisZabell.lean).** D-Z prove: (i) given (J), the
  delivered marginal determines the posterior (`jcond_iff_jeffrey`);
  (ii) Jeffrey's posterior uniquely minimizes I(Q,P) = sum Q log(Q/P), i.e.
  KL(Q||P) with the candidate first, among Q with the delivered marginal
  (`thm51_KL_le`, `thm51_KL_eq_iff`), likewise Hellinger, and every
  f-divergence for convex f with uniqueness for strictly convex f
  (`thm61_le`, `thm61_unique`); for variation distance it is a minimizer but
  not unique (`remark_a_tv_not_unique`). No coherence or Dutch-book
  uniqueness result exists in D-Z, and no axioms (section 5 is "mechanical
  updating"). Sufficiency: (J) iff P*/P constant on cells
  (`jcond_iff_ratio_const`); the likelihood-ratio partition is the minimal
  (J)-partition and every refinement also satisfies (J)
  (`thm22_lr_minimal`, `jcond_of_refines`). Replacement wording for MS ~117
  and ~190-195 is in literature/diaconis_zabell1982/README.md. **Gap in D-Z
  Theorem 3.2 as printed** (same issue as M26): "commute => Jeffrey
  independent" needs every E_i F_j nonempty (`thm32_needs_qualitative_independence`);
  holds for Paper B's full-support prior. Exact 2x2 condition: J-independence
  iff (p = alpha or c = 0) and (q = beta or c = 0) (`remark826`).
- [ ] **M23. MS:190-195, Diaconis-Zabell. MISQUOTED.** "The partition
  satisfying the invariance condition is the minimal sufficient statistic for
  revising the prior to any candidate posterior on that partition". D-Z
  (p. 824, Thm 2.2) say such a partition is *sufficient*; the minimal one is
  the likelihood-ratio partition, every refinement also satisfies the
  condition, and sufficiency is relative to one pair {P, P*}. "The axiomatic
  grounding is twofold" mislabels what D-Z call "mechanical updating". **Do:**
  restate as Thm 2.2 states it.
- [ ] **M24. MS:78-79, Diaconis-Zabell. CONTRADICTED.** The sentence has D-Z
  (with Hawthorne) "treating order dependence as a defect to repair". D-Z
  Remarks 1-2 (p. 827): "There is no reason to require P_EF = P_FE";
  "noncommutativity is not a real problem". **Do:** attribute the defect view
  to Hawthorne (and Doring) only; D-Z are on the paper's side here.
- [ ] **M25. MS:277, Pettigrew-Weisberg. CAVEAT.** "changing how successive
  inputs are pooled": P-W pool the prior with each source's opinion by upco,
  not successive inputs with each other. P_B equals Field/upco only when each
  factor is computed against the prior marginal. **Do:** "by pooling the prior
  with each new input multiplicatively (upco)".
- [ ] **M26. Commutation criterion needs positive cells (2026-09-30,
  lean/Literature/Hawthorne.lean).** The "only if" half of the criterion
  Hawthorne states (p. 97) and credits to D-Z Thm 3.2 (two Jeffrey updates
  commute only if neither moves the other's basis marginal) needs every joint
  cell positive: D-Z's step "choose A = E_i0 F_j0" divides by that cell.
  Counterexample `criterion_needs_positive_cells`: block-diagonal support,
  the updates commute, yet each moves the other's marginal. Paper B's
  interior 2x2 prior satisfies the condition, so no result is affected; any
  "iff" statement of the criterion (MS, PLAN 1.A/3.A, D-Z README) should say
  "for a prior with all cells positive". Hawthorne's own Section 9 covers
  the zero-cell case. Also (M21 refined): the manuscript's
  l proportional to q_i/P(A=i) are NL factors against the prior; since only
  ratios matter they are equally his LR factors, which he (after Jeffrey and
  Good, note 20) calls Bayes factors, so PLAN 3.A's parenthetical can be
  reworded rather than retracted. The p. 115 quotation runs onto p. 116.
  Corrected wording for M19, M20, P8, P9, 3.A, D5, D7(c) is in
  literature/hawthorne2004/README.md.
- [ ] **M19. MS:80 footnote, Hawthorne. CAVEAT.** "Hawthorne2004 repairs
  this by letting a cue multiply the old belief instead of replacing it, and
  under his order-free variants the sequence effect disappears". Order-freedom
  holds only across distinct bases, and full order-freedom only in his
  Basis-Commuting Version (Section 8); "repairs" overstates, he offers
  alternatives and prefers them. **Do:** "offers factor-based alternatives
  under which updates on distinct bases commute".
- [ ] **M20. MS:97, Hawthorne. NOT-CHECKABLE (project's own inference).**
  "Hawthorne's variants leave the association just as uninformative": he
  says nothing about association. It follows from Lemma SEP (every
  factor-based variant is separable), which is the paper's result, not his.
  **Do:** attribute to Lemma SEP explicitly.
- [ ] **M21. Terminology, "Bayes factor".** Hawthorne's note 20 gives the
  name "Bayes factor" to his likelihood-ratio factors; the manuscript uses it
  for his normed-likelihood (NL) factor. His taxonomy has three models with NL
  the middle one, so PLAN 3.A's "the two ends of his taxonomy" is wrong
  (amnestic is one end, LR the other). The identification of P^B with his
  extended update formula omits his normalising denominator. **Do:** fix the
  3.A parenthetical and the "two ends" phrase.

### Section 3, Related literature

- [ ] **M2. MS:266-267, BHW and Banerjee. UNSUPPORTED.** "the sequence effect
  in \citep{BHW1992, Banerjee1992} lives on the analogue of the marginals".
  Both models have a single-variable state, so there is no marginal-versus-
  association distinction to map onto. This is the paper's own analogy.
  **Do:** state it as an analogy or remove.
- [ ] **M3. MS:257-259, Banerjee versus BHW contrast. CAVEAT.** "In BHW, on
  the other hand, a cascade is an action that conveys no private signal": the
  "on the other hand" is misleading, BHW rest on the same coarse-action
  mechanism (their fn 17 and the Banerjee remark, p. 1002). Banerjee p. 809
  gives the sufficient-statistic failure as the cause of herding and
  inefficiency, not of "the sequence persists". **Do:** reword as one
  mechanism described twice.
  **Settled formally (lean/Literature/BHW.lean, Banerjee.lean):** "coarse" is
  on the wrong paper. Banerjee's actions live in the continuum [0,1]; his
  non-invertibility comes from the equilibrium rule under a discontinuous
  payoff. BHW's binary action is the coarse one, and it loses information
  even before a cascade (`lik_ratio`: at d = 2 the public ratio is
  p(1+p)/((1-p)(2-p)), below (p/(1-p))^2). The shared mechanism is "the
  equilibrium action is not a sufficient statistic". "Persists" is defensible
  for the herd: joining is uninformative, so a wrong herd is absorbing and
  P(no one correct) >= Pi for every N (`no_one_correct_ge_Pi`). Wording in
  literature/bhw1992/README.md and banerjee1992/README.md.
- [ ] **M4. MS:273, Dietrich "Def. 1". WRONG-LOCATION.** Def. 1 (p. 7) is
  linear-geometric *preference* aggregation. Geometric belief pooling is
  App. A (p. 16), characterised by Thm 2 (p. 11, section 4). Drive copy is
  the HAL January 2021 extended version, not the JET typeset article; check
  numbering there too. **Do:** cite Thm 2 or section 4.
- [ ] **M5. MS:273-274, Dietrich criterion applied to the population mean.
  UNSUPPORTED.** Dietrich's criterion (pool then condition on E, versus
  condition each member on the same E then pool; p. 8, External Bayesianity
  p. 15) is defined for Bayesian conditioning on events or likelihoods. The
  paper's members share a prior and differ only in the order of Jeffrey
  steps, so no event conditioning produces the profile, and "generically
  fails the criterion" is not a statement his framework makes. Geometric
  pooling "satisfies the criterion" is true only for event/likelihood
  updating. **Do:** either restrict the sentence to what Dietrich proves
  and say the Jeffrey case is outside it, or cut the paragraph. Hayashi
  (2024, JME, in Drive, uncited) bears on this paragraph.
  **Settled formally (lean/Literature/Dietrich.lean):** geometric pooling is
  dynamically rational and externally Bayesian (`geoPool_dynRational`,
  `geoPool_externallyBayesian`); linear pooling commutes only under
  dictatorship, equal p(E) or equal conditionals (`linPool_comm_iff`). The
  manuscript's application is worse than unevaluable: with a common prior,
  whenever the criterion's premise holds every member ends with the same
  posterior, so linear pooling *passes*
  (`linPool_dynRational_on_common_prior`). Against P^B, linear and geometric
  pooling of (PJ_AB, PJ_BA) miss at order c with the same coefficient and
  differ only at O(c^2), so the linear-versus-geometric contrast plays no role
  in the first-order results. **Do:** cut the "generically fails the
  criterion" clause. Wording in literature/dietrich2021/README.md.
- [ ] **M6. MS:276, Epstein 2006. NOT-CHECKABLE / doubtful.** "Epstein2006
  makes the updating rule subjective". Primary not in Drive. Ortoleva's 2024
  review (p. 558) describes it as a temptation/self-control model
  (Gul-Pesendorfer menus, tempted to deviate from Bayes), not as a subjective
  updating rule. **Do:** obtain the RES paper; reword to what its
  representation says.
- [ ] **M7. MS:276, Ortoleva 2012. CAVEAT.** "axiomatises departures
  triggered by unexpected news" is supported only via the 2024 review
  (pp. 558-560, HT model, Theorem 2). Primary not in Drive. **Do:** obtain
  the AER paper.
- [ ] **M8. MS:276, framing of Epstein and Ortoleva. UNSUPPORTED.** The
  sentence presents them as alternatives used "to explain whether sequential
  conditioning is sequence-independent or not". The review presents both as
  single-step updating on events, and its fn 2 (p. 546) leaves Jeffrey's rule
  out of scope. Neither is about order. The framing fits Cripps only.
  **Do:** reframe as "non-Bayesian updating rules" and separate Cripps.
- [ ] **M6 and M8 against the primary, 2026-09-30.** Epstein, "An Axiomatic
  Model of Non-Bayesian Updating", Rev. Econ. Stud. 73 (2006) 413-436 (Drive:
  Epstein-Updating-RESTUD-2006.pdf, published version, 24 pp.). (a) M6 is
  **VERIFIED, overturning the survey-based verdict below**: the abstract
  (p. 413) says the main result generalizes Anscombe-Aumann "so that both the
  prior and the way in which it is updated are subjective"; the introduction
  (p. 413) repeats it. Keep "makes the updating rule subjective". (b) M8
  stays **UNSUPPORTED** for Epstein: the model has three periods and a single
  interim signal s_1 (p. 414); nothing concerns the order of several
  signals. (c) Worth citing for the adoption weight: under Prior-Bias,
  Corollary 3, eq. (23) (p. 429), the temptation posterior is
  q(.|s_1) = (1 - lambda(s_1)) p(.|s_1) + lambda(s_1) p_2(.), a mixture of
  the Bayesian update and the prior marginal, the same averaging form as the
  paper's partial adoption (weight 1 - omega on what was held before), here
  axiomatized from preferences over contingent menus. Caution: his "updating
  is standard ... if p is a product measure" (p. 429) is the case where the
  signal carries no information about S_2, not an analogue of Proposition IMM
  at c = 0.
  **Corrected by the Lean record (lean/Literature/Epstein.lean, 50
  theorems):** lambda = 1 - omega only for the *tempting* posterior q in
  (23) (`priorBias_eq_damped`). The posterior the agent acts on is the
  compromise (12), which is the damped target with
  omega = 1 - alpha lambda/(1 + alpha) (`compromise_eq_damped`); that omega
  is never 0, lies in [1/(1+alpha), 1) under Positive Prior-Bias and exceeds
  1 under Negative Prior-Bias. Epstein's mixing target is the time-0 prior
  marginal p_2, not the marginal in force, his "delivered" belief is a Bayes
  update, and he has one signal. "Base-rate neglect" is not in the paper
  (Ortoleva's survey lists it; section 2.3 covers under/overreaction,
  confirmatory bias, representativeness, sample bias), so strike it from the
  entry below and from verify_updating_theory.md section 5. Epstein's own
  law-of-iterated-expectations remark (pp. 429-430) is too broad: under
  Prior-Bias no act reverses at every signal (`priorBias_no_uniform_reversal`).
  Unstated constraint: Reg2 with (23) and lambda != 0 forces p(.|s_1) to
  charge every state p_2 charges (`priorBias_absCont`).
- [ ] **M6-M8 re-read 2026-09-30 against Ortoleva (2024), "Alternatives to
  Bayesian Updating", Annu. Rev. Econ. 16:545-570 (Drive:
  ortoleva_2024_annurev-economics-100223-050352.pdf).** (a) Ortoleva 2012:
  pp. 558-560 give the Hypothesis Testing model (keep the prior and apply
  Bayes unless pi(A) <= epsilon, then update a prior over priors and pick the
  most likely prior) and Theorem 2 ("proved by Ortoleva (2012)"):
  Consequentialism + Dynamic Coherence iff a minimal HT representation;
  epsilon = 0 iff Dynamic Consistency also holds. M7's wording is accurate
  (second-hand, by the paper's own author). (b) Epstein 2006, p. 558: a
  temptation model in the Gul-Pesendorfer (2001) self-control framework
  (three periods, preferences over menus, agents tempted to over- or
  under-react), accommodating under/overreaction, base-rate neglect, sample
  bias and representativeness. p. 548 cites Epstein (2006) for the weaker
  normative appeal of Bayes when the *prior* is subjective. "makes the
  updating rule subjective" is loose; suggested: "models departures from
  Bayes' rule as temptation (Epstein 2006)". (c) The framing ("to explain
  whether sequential conditioning is sequence-independent") is unsupported:
  neither is a theory of order effects, and fn 2 (p. 546) puts Jeffrey's rule
  out of scope, citing Diaconis & Zabell (1986), a different paper from the
  1982 one. (d) Possible citation for an order effect in a non-Bayesian
  model: Benjamin, Bodoh-Creed & Rabin (2019) on base-rate neglect;
  "more recent messages are given more weight" (p. 557) is *Ortoleva's
  paraphrase*, not their text. Quote their p. 21 ("recency bias ... she draws
  stronger inferences from signals observed recently") or p. 3. Now in Drive
  and formalized, see P12. **Do:** keep M7's sentence; reword M6; drop
  or rewrite the M8 framing; consider Benjamin et al. (2019).
- [ ] **M7-M8, M13, M16 formal records (2026-09-30).** Ortoleva
  (lean/Literature/Ortoleva.lean, 49 theorems, from the 2024 survey; Theorem
  1 is on p. 550): the "if" half of Theorem 2 for every HT model and the
  epsilon = 0 case; the "only if" half needs the 2012 construction, not in
  the survey. HT is one-step updating from the original prior, and Dynamic
  Coherence's "sequence of events" is a cycle of alternative events, not
  successive updates, confirming M8 for Ortoleva. The model needs an
  unstated support condition for the prior-over-priors update. Becker
  (lean/Literature/Becker.lean, 20 theorems, web copy): impulsive demand
  averages to I/(2 p_1) and slopes down (`impulsive_mean`,
  `impulsive_market_average` via the strong law); each ingredient alone
  fails (`constraint_without_averaging`, `averaging_without_constraint`).
  M13 is stronger than logged: Becker says "Our statement goes beyond
  arithmetic" (p. 7), so on the manuscript's own dichotomy he sits on the
  constraint side; the 1962 paper mentions discrimination only in a cited
  book title (fn 21). His fn 14 example does not meet his own sufficient
  condition for a "necessary decline". Tao (lean/Literature/Tao.lean, 14
  theorems): Exercise 1.4.23(iii) and Theorems 1.7.15/1.7.18 proved from
  Mathlib; `measure_band_tendsto_zero` and `los_step4` give the manuscript's
  Step 4 (mu(B_c) -> 0, L(c) = o(c)); Tao states the exercise for sequences,
  the manuscript takes c down to 0 over a continuum (standard, not literally
  Tao's statement); Step 3 needs only Tonelli. JeffreyOrder/Decision.lean
  itself uses neither Tonelli nor continuity from above. The Tao copy is the
  author's preliminary version, so printed numbering is unverified.
- [ ] **M9. MS:276-277, Cripps. CAVEAT + UNSUPPORTED x2.** (a) "shows that
  symmetry and divisibility jointly force sequence-independence" rests on a
  one-sentence remark after Axiom 3 (p. 9) about reversing the nested
  revelation of one experiment's signals, not a proposition, and not two cues
  on different attributes. (b) "the correlated two-cue composite fails
  divisibility": nothing in Cripps or the repo shows it; by contraposition an
  order-dependent rule fails Divisibility *or* Symmetry, and only if it is in
  his domain at all (rules on a prior plus an experiment with signal
  probabilities; the Jeffrey composite takes credences). (c) "of the four
  axioms, only divisibility": no derivation anywhere that the composite
  satisfies Uninformativeness, Symmetry and Non-Dogmatic; Non-Dogmatic is
  doubtful for an attribute-marginal step. The footnote calling the statement
  "behavioural" concedes the domain mismatch and contradicts the main text.
  Cripps's four axioms: 1 Uninformativeness, 2 Symmetry, 3 Divisibility,
  4 Non-Dogmatic; Prop. 1 (p. 11) characterises rules satisfying all four as
  "divisible" (shadow prior, Bayes, map back). **Do:** either supply the
  translation and the derivation (verify in sympy first), or reduce to
  "Cripps remarks that Symmetry and Divisibility together make the order of
  revelation irrelevant; the present composite is outside his domain".
  **Settled formally (lean/Literature/Cripps.lean):** (a) is true and now a
  theorem, `order_invariance`, from Symmetry and Divisibility alone, for any
  two conditionally independent experiments (so it does cover an A- and a
  B-measurable experiment, qualifying the audit's "nested revelation only").
  (b)-(c) are **false**: under the matched-likelihood reading (the paper's
  own Prop. IMM) the composite is a Bayes rule satisfying all four axioms,
  and the order effect comes from the second likelihood being re-matched to
  the intermediate belief (`composite_AB_eq_bayes`; the B-marginal shifts by
  c(q_0 - alpha)/(alpha(1 - alpha)) after the first step); under the
  rigid-credence reading Axioms 1 and 4 fail for every choice of evidence map
  (`rigid_not_uninformative`, `rigid_not_nonDogmatic`). No reading makes it
  fail Divisibility alone. The honest sentence is that Cripps's
  order-invariance needs the *same* experiments in both orders, and
  delivered credences re-matched to the current belief are not the same
  experiments. Wording in literature/cripps2021/README.md.
- [ ] **M10. MS:279, Phelps. UNSUPPORTED for Phelps; CAVEAT for Arrow.**
  "borrows the evaluator-with-binary-attributes frame from ... Phelps1972,
  Arrow1973". Phelps's qualification is continuous with a normal test error;
  his only binary variable is the race dummy (pp. 659-660). Arrow section 4
  (p. 26) has binary qualification x binary group, but the group is observed,
  not a second uncertain attribute; and Arrow 1973 is taste-based in
  sections 1-3, statistical only in section 4 (pp. 25-31). Coate-Loury
  (p. 1224) is closer to the frame than Phelps. **Do:** cite Arrow section 4
  and Coate-Loury for the frame; cite Phelps for statistical discrimination
  in general only.
  **Settled formally (lean/Literature/Phelps.lean, Arrow.lean,
  CoateLoury.lean):** no binary unknown appears anywhere in Phelps. Phelps's
  Case 2 is a difference in the *variance of qualification* (eq. 6); the
  test-reliability case is the Further Case (eq. 7); both keep a mean
  difference, and the point that dispersion or reliability alone suffices is
  ours (`no_mean_gap_still_differential`). Arrow section 4 sketches the
  self-confirming idea but says it "does not prove" discriminatory equilibria
  exist (WP p. 30), and his employer receives no individual signal;
  Coate-Loury prove existence (`prop1_two_equilibria`) and add the signal and
  Bayes step. CL's stereotype is literally a believed positive covariance
  between group and qualification (`negativeStereotype_iff_cov_pos`), so the
  contrast with the paper is mechanism (correct-in-equilibrium best responses
  versus coherent updating), not concept. For P4, every Phelps/Arrow case is a
  difference in the employer's prior model of the groups, so the wording
  should be "different *prior* beliefs about the groups". Wording in the three
  READMEs.
- [ ] **M11. MS:280-281, Coate-Loury. CAVEAT.** Contrast "kinematic rather
  than an equilibrium fixed point" holds, but CL also have adjustment
  dynamics (p. 1226), and CL define a stereotype as a believed correlation
  between group identity and productivity (p. 1221), close to the
  manuscript's own definition. **Do:** acknowledge the CL definition when the
  paper defines its own sense of stereotype.
- [ ] **M12. MS:282, BCGS. CAVEAT x2.** "representativeness-distortion or
  selective recall": one mechanism, not two ("Selective recall is driven by
  representativeness", WP p. 12). "distortion of memory or sampling":
  sampling is not a BCGS mechanism. BCGS section 4.3 (p. 27) also produces an
  exaggerated cross-attribute correlation, so the contrast with the
  manuscript is in mechanism, not in the object. Drive copy is the May 2015
  working paper; QJE 131(4):1753-1794 confirmed externally. **Do:** reword
  to one mechanism; drop "sampling"; say the contrast is mechanism.
  **Settled formally (lean/Literature/BCGS.lean):** in the section 4.3
  instance the education-welfare correlation is exaggerated only in the
  population pooled across groups; within each group the stereotype has zero
  association against a true -6/125 (`welfare_correlation_*`,
  `welfare_within_group`). For a 2x2 law their covariance is exactly the
  paper's `assoc`. So the contrast is mechanism *and* location: BCGS's
  association sits across groups, the paper's within one evaluator's belief.
- [ ] **M13. MS:282-283, Becker 1962. UNSUPPORTED cross-reference + CAVEAT.**
  "The lineage distinction the concluding section trades on ... goes back at
  least as far as Becker1962": the Concluding remarks (MS:900-967) never draw
  an aggregation-versus-constraint distinction and never mention Becker.
  Becker's own mechanism is averaging *plus* "a resource constraint on
  behavior" (p. 10; irrational units "forced by a change in opportunities to
  respond rationally", p. 12), so "rather than imposed by an enforced
  constraint on behaviour" nearly inverts him unless "enforced" means
  regulatory. Becker 1962 is not in Drive (agent used a JSTOR copy). PLAN:877
  lists Becker in the "discrimination lineage", conflating Becker 1962 with
  *The Economics of Discrimination* (the Becker that Phelps, Arrow and CL
  cite). **Do:** either write the distinction into the conclusion and fix
  the Becker gloss, or drop the sentence; fix PLAN:877.
- [ ] **M14. MS:285-288, Good-Mittal amalgamation paradox. CAVEAT x2.**
  (a) "a real effect present in every subpopulation is erased or reversed" is
  narrower than their Def. 1.1 (p. 695): the aggregate lies outside the
  interval of the subpopulation measures, which includes amplification and
  the Yule case where no subpopulation shows an effect but the aggregate does
  (the case the name "amalgamation" was chosen for). (b) "a confound in how
  the subpopulations are weighted together": they never use "confound" and
  say the paradox "can happen even though N_i is proportional to p_i"
  (p. 696); the cause is non-uniform row or column ratios across
  subpopulations (Defs 2.1-2.2, Thms 4.1-4.3). **Do:** restate the paradox in
  their terms; the manuscript's contrast (erasure within one population)
  survives but the description of theirs must change.
  **Settled formally (lean/Literature/GoodMittal.lean):** `piR_amalg_general`
  shows the aggregate difference is a treated-row average minus an
  untreated-row average under *different* subpopulation weights, so it is a
  weighting effect, but not by population shares N_i/N (`equalSize_reversal`:
  N_1 = N_2, reversal from treatment imbalance). Row-uniform designs rule the
  paradox out for pi_R, Yule's Q and the pi_C analogue (`no_paradox_*`), but
  not for the odds ratio (`note_p702_kappa_paradox`, their p. 702 tables: both
  27, aggregate 26.991, a dilution, neither erased nor reversed). Yule's case
  (`yule_case_paradox`) creates an effect from none. Proposed wording
  ("treatment unevenly allocated across subpopulations") is in
  literature/goodmittal1987/README.md. For M15, FGT.lean shows alpha = 0 and
  alpha = 1 are exactly where monotonicity and the transfer axiom switch on
  (`P0_not_monotone`, `P1_transfer_neutral`), which gives a principled reason
  to single out the first two members, and that the mean gap among the poor
  is not decomposable (`I_not_decomposable`); wording in
  literature/fgt1984/README.md.

### Section 4

- [ ] **M15. MS:297, Foster-Greer-Thorbecke. MISQUOTED + CAVEAT x3.**
  "a headcount (a count of those past a threshold) and a mean shortfall
  (average of their distances from it) are first two members of a single
  parametrised family". P_0 is the headcount *ratio* q/n, a share. P_1 =
  H*I = (1/n) sum g_i / z (p. 763) is normalised by the poverty line and
  averaged over the whole population; "average of their distances" is
  (1/q) sum g_i, the income-gap measure, which is *not* in the family.
  alpha ranges over all reals >= 0, so 0 and 1 are the first two integer
  members, and FGT's headline measure is alpha = 2. FGT never say
  "incidence versus intensity". The manuscript's L(c) does have the P_1
  shape; only the wording is wrong. **Do:** "the headcount ratio and the
  population-average normalised shortfall, the non-poor contributing zero";
  drop "first two members"; attribute the incidence/intensity pairing to the
  later literature or to nobody.

### Appendix, Theorem LOS proof

- [ ] **M16. MS:1136 and 1150, Tao 2011. VERIFIED-WITH-CAVEAT.** Corollary
  1.7.23 is the Fubini-Tonelli theorem; for the nonnegative integrand the
  exact reference is Tonelli, Theorem 1.7.15 (incomplete) or 1.7.18
  (complete version). "Continuity from above, section 1.4" is Exercise
  1.4.23(iii), downward monotone convergence, whose finite-measure hypothesis
  the proof satisfies. Tao is not in Drive; checked against the author's
  online preprint. **Do:** cite Theorem 1.7.18 and Exercise 1.4.23(iii).

### Bibliography

- [ ] **M17. Hawthorne2004 is cited twice in the manuscript (MS:78, 80) but
  is not in bibliography.bib.** Undefined citation in the committed
  manuscript. PLAN B.A supplies the entry. **Do:** apply B.A's Hawthorne
  entry now.
- [ ] **M18. DOIs.** BHW, Ortoleva 2012, Epstein 2006 DOIs are not
  verifiable from Drive (not printed on the copies). Good-Mittal volume,
  number, year not printed on the scan. Arrow chapter pages 3-33 confirmed
  only via the CL and BCGS reference lists (Drive copy is the 1971 Princeton
  working paper 30A, own pagination; never cite Arrow by page from it).
  **Do:** confirm from publisher pages before submission.

## 2. Change plan (notes/manuscript_change_plan.md)

- [ ] **P1. PLAN:791 (3.A verification note). UNSUPPORTED.** "All five papers
  read in full and formalized in literature/": there is no Doring directory
  in `literature/`. `notes/verification_coverage.md:61-62` also points to
  `literature/` for Doring. **Do:** correct the note; add a Doring README if
  the read is to be on record.
- [ ] **P2. PLAN:686-687 (3.A text on Doring). CAVEAT.** "an adjustment that
  Jeffrey's rule cannot supply": Doring's own remedy is a single Jeffrey
  update from the original prior on the merged partition (S384-S385); what
  Jeffrey's rule cannot do is the *incremental* version. **Do:** say
  "cannot be reached by successive Jeffrey steps".
- [ ] **P3. PLAN:668-669 (3.A text). CAVEAT, see D1.** "The premise the paper
  adopts is disputed on psychological grounds": true of Hawthorne, not of
  Doring, whose objection is normative. **Do:** attribute the psychological
  objection to Hawthorne alone.
- [ ] **P4. PLAN:310-311 (1.C). CAVEAT.** "statistical discrimination in the
  sense of Phelps and Arrow, which rests on a difference in beliefs about the
  groups" clashes with the paragraph's own "receive different mean beliefs";
  Phelps's Case 2 and Further Case rest on variance and test reliability,
  not mean. **Do:** "prior beliefs about the groups".
- [ ] **P5. PLAN:877 (3.C). CAVEAT.** Lists Becker in the discrimination
  lineage; see M13.
- [ ] **P8. PLAN:418-420 (1.D). UNSUPPORTED.** "Hawthorne names the premise
  and objects ... so that the alternative to a weight of one on the later cue
  is a weight below one": his alternatives are factor models, not a partial
  weight. The weight is the paper's own construction. **Do:** say so.
- [ ] **P9. PLAN 6.A (~1283, ~1324) and papers_dialectic.tex:91-95.
  CONTRADICTED in part.** Hawthorne's illustration is said to be "one basis
  cued twice" and so to miss a one-cue-per-attribute model. The car example
  is single-basis, but his pp. 98-99 objection follows directly on the
  two-basis medical example, where the overwrite is explicit
  (Q_e[E] = Q_fe[E] = .90). The objection therefore reaches the paper's model
  through his own two-basis example. **Do:** drop the claim that his
  illustration misses; the 6.A reply (the weight is measured) stands without
  it.
- [ ] **P10. Hogarth-Einhorn, primacy versus recency. CONTRADICTED, the most
  consequential finding so far.** PLAN 6.A (~1300-1303), papers_dialectic.tex
  :164-166 and review log :802-803 say partial adjustment "in the manner of
  Hogarth and Einhorn" protects the *first* impression. HE's Appendix B shows
  the opposite: with R = S_{k-1} under Step-by-Step processing, "recency
  always obtains". Their primacy comes mainly from the End-of-Sequence
  "force toward primacy" (Eq. 8, Table 2 row 1, 19 of 27 studies), not from
  weights decaying over a long series (the claim in Anchoring.lean:18-20 and
  PLAN :447-448, 787-788, 1266). The paper's damped family damps only the
  second cue, so protecting the first impression is a property of that
  one-sided construction, not of HE's model, which damps every cue with a
  state-dependent weight (their 6a/6b, called "critical"); the constant-weight
  version is Anderson-Hovland's. **Do:** restate 6.A, the dialectic's
  "realism complaint" paragraph and interior_omega's position channel so the
  rival is "the one-sided damped family" with HE cited only for the averaging
  equation; do not attribute first-impression protection to HE.
  **Settled formally (2026-09-29, lean/Literature/HogarthEinhorn.lean, 43
  theorems):** HE's Step-by-Step rule damping every cue gives recency on
  mixed evidence under Anderson-Hovland weights (`appB_recency`, Eq. B.3) and
  under HE's own contrast weights (`contrast_mixed_recency`); consistent
  evidence can give primacy under their weights (`contrast_consistent_primacy_*`,
  about 5% of an exact sweep), a gap HE admit (T-p.35). The paper's one-sided
  construction (first cue in full, second damped by w) *is* HE's
  End-of-Sequence Eq. 8 (`oneSided_eq_eq8`), which on a single scalar gives
  primacy only for w < 1/2 (`oneSided_primacy_iff`). So (i) the one-sided form
  is theirs, not new; the two-attribute Jeffrey embedding is what is new
  (revises P11); (ii) "interior omega is the primacy regime" is false in HE's
  sense; (iii) in the two-attribute model the first-read attribute is
  "protected" because it is adopted in full, not because anything is damped.
  Proposed rewordings are in literature/hogarth_einhorn1992/README.md.
- [ ] **P11. omega = 0 "is ours, not theirs". CONTRADICTED.** (Anchoring.lean
  :20-21; PLAN :446-448, 1267, 1277-1278.) HE define 0 <= w_k <= 1, name the
  insensitive corner (Fig. 7, the "advocate"), and say evidence can be
  "completely ignored"; Asch p. 273 has subjects who "completely excluded" the
  late trait. Only the one-sided construction is new. **Do:** say "the
  one-sided construction is ours; the zero weight is in HE's range".
- [ ] **P12. interior_omega.tex:62-65 (prelude, approved 2026-09-27), carried
  into PLAN 1.A2 :203-205 and writing_discipline.md:84. CONTRADICTED as
  cited.** "a position channel, in which the observer weights the later cue
  less ... \citep{HogarthEinhorn1992,Asch1946}". HE's Step-by-Step partial
  adjustment gives recency; Asch p. 272 says "It is not the sheer temporal
  position of the item ...". Neither source supports "weights the later cue
  less". interior_omega :323, 448 also calls interior omega "the primacy
  regime" in HE's vocabulary (UNSUPPORTED; the one-sided scheme gives primacy
  on one attribute only for omega < 1/2). **Do:** cite neither for the
  direction; define the position channel by the model, not by the sources.
  **Candidate source for the position channel (2026-09-30):** Benjamin,
  Bodoh-Creed & Rabin, "Base-Rate Neglect: Foundations and Implications",
  working paper, 19 July 2019 (Drive: baserateneglect-2019-07.pdf). Their
  rule p_a(theta|s) proportional to p(s|theta) p(theta)^a, a in [0,1), is
  applied at every step, so earlier signals are progressively down-weighted:
  a recency effect (Section 3; abstract: "beliefs will reflect the most
  recent signals"). In the paper's 2x2 model with attribute-local likelihoods
  (scratch sympy, to be formalized): the A-marginal differs between orders
  already at c = 0 (the position channel, with Bayes-factor inputs); the log
  odds ratio is identical across orders and equals a^2 times the prior's, so
  the association is order-blind but shrunk against P^B at every c. It
  supports interior_omega's claim that the position channel is not specific
  to delivered credences. Direction caveat: BRN down-weights the *earlier*
  evidence, so it is recency, not "the later cue weighted less".
  **Formalized (lean/Literature/BenjaminBodohCreedRabin.lean, 41 theorems;
  sympy 55/55):** closed form after n signals, eq. 5 (p. 21); weight
  alpha^{n-1-k} on signal k in log odds, eq. 6 (p. 21); reversing two
  signals shifts the log odds by (1-alpha)(l_2 - l_1) (`twoSignal_logOdds_order`).
  On the 2x2 joint: log OR = alpha^2 assoc(P) in either order and =
  alpha^2 assoc(P^B)-relative (`assoc_brn_orders_eq`, `assoc_brnAB_vs_bayes`),
  a gap at every c (first order in c against P^B); A-marginal differs by
  orders at independence (`margOddsA_orders_ne`; exact witness 7/8 versus
  49/50). **Qualifications:** (i) the c = 0 effect needs BRN on the joint
  cells with posterior-becomes-prior (their "major modeling gambit", p. 19,
  untested); applied per attribute, or with both cues pooled in one update
  (pp. 20-21), there is no c = 0 effect. (ii) The prelude's next sentence,
  "which channels operate is fixed by how fully the later cue is adopted",
  fails as a general claim: under BRN the later cue is adopted in full and
  the position channel still operates; keep it only as a statement about
  the paper's model. (iii) Their fn 7 (p. 13) says the model predicts that
  whatever comes first is down-weighted (recency). Proposed prelude wording
  in literature/benjamin2019/README.md. Not yet in bibliography.bib
  (working paper; @unpublished entry proposed there).
- [ ] **P13. Asch, "the joint is never elicited". CONTRADICTED.** (review log
  :539-540; PLAN 3.B :810-811.) Each subject's check-list is an 18-item joint
  response; Asch conditions on the warm/cold item in Experiment II (p. 265)
  and notes individual consistency (pp. 264-265). What he never collected is
  a *prior* association (that part holds). **Do:** 3.B should say "no prior
  association is elicited", not "the joint is never elicited".
- [ ] **P14. Asch Experiment VI, "one cue per trait across eighteen traits".
  CONTRADICTED.** (PLAN 6.A :1314-1315.) Experiment VI has six stimulus terms;
  the eighteen traits are check-list response items, none of them a
  stimulus. **Do:** "six stimulus terms, eighteen response traits".
- [ ] **P15. Asch caveats.** "A broad, uncrystallized ..." is on p. 272 (the
  sentence begins p. 271), not pp. 272-273. Footnote 5 gives two conditions,
  not only centrality. "envious 6th versus 1st" are modal ranks (39% and 29%
  of subjects). "early terms dominate": 10 of 24 subjects reported no change.
  "no account offered" of uneven effects: Asch gives a content-based account
  in Experiment I (p. 264). Asch's own mechanism is the relation of content,
  not position. All Table 7 numbers quoted in PLAN 3.B are correct and in the
  right columns.
- [ ] **P16. Hogarth-Einhorn caveats.** "HE find primacy, recency or no
  effect": their own five experiments found only recency or no effect (the
  76 data points are other authors' studies, 5 of them no-effect). "their
  eq. (1) with R = S_{k-1}" is their Eq. 3 with a constant weight. "memory is
  limited to ... current anchor" means only the anchor is remembered, not
  full adoption. HE's Limitations section does raise dependencies among
  evidence. **Source caveat:** `hogarth_einhorn_1992.pdf` in Drive is a
  compiled LaTeX transcription with reconstructed equations, not the journal
  article; journal page numbers, issue and DOI are unverifiable from it, and
  PLAN's "checked against hogarth_einhorn_1992.pdf" means the transcription.
  **Do:** obtain the Cognitive Psychology PDF.
- [ ] **P17. BIR and Heckman in PLAN 3.C/3.D.** (a) "no sample size repairs"
  (PLAN :894-899, the_discrimination_problem.tex:242-244, review log :688-690)
  is the project's gloss; Heckman never discusses sample size. Keep only as
  the author's inference, not attributed. (b) BIR's "map ... vanishes only as
  judgment becomes perfectly objective" (PLAN :940-943) ignores that
  Proposition 2 attenuates discrimination along histories and that the
  coefficient also vanishes as tau_q -> 0. (c) Every BIR page reference in
  the notes is to the January 2019 working paper (printed page = PDF page
  - 1), not the AER pages the bibliography entry gives. **Do:** cite AER
  pages or say "working paper".
- [ ] **P6. PLAN 1.A and 1.D are stale.** Their BEFORE blocks quote intro
  paragraphs that the manuscript commits of 2026-09-12 and 09-15 already
  rewrote; much of both edits is already in. **Do:** merge by hand against
  the current paragraphs 1 and 3.
- [ ] **P7. PLAN 1.B records an approved sentence (2026-09-21) that is not in
  the committed manuscript.** MS:113 still reads "Whether arrival sequence
  matters or not in the aggregate...". **Do:** confirm whether a newer
  working copy exists; the Drive copy of the manuscript is from July.

## 3. Notes (notes/*.tex, *.md)

- [ ] **D1. papers_dialectic.tex:32-34, 54-56. CONTRADICTED for Doring.**
  "The attack says the model is unrealistic. The attack is about psychology.
  Nobody in the exchange measures anything." Doring's claim is normative:
  "an exercise in Bayesian *rational* psychology ... cannot be a complete
  account of *rational* belief change" (S379); the order dependence "seems
  wholly unjustified" (S383); "Jeffrey conditionalization alone cannot be all
  there is to rational belief change" (S386). Hawthorne's objection is the
  psychological one. **Do:** split the standoff into a normative prong
  (Doring) and a psychological prong (Hawthorne); the dialectic's step 3
  already separates Hawthorne's two prongs, so the structure can absorb it.
  Same correction in PLAN 3.A (P3).
- [ ] **D2. papers_dialectic.tex:155-158. CAVEAT, borderline CONTRADICTED.**
  "cells are exactly what an observer of a population does not get to read,
  and the step from his tables to observable statistics is the step this
  paper supplies". Doring himself reads the effect off P(A given not-B)
  (1/6 versus 5/6) and argues a third update pushes the *unconditional* P(A)
  near 0 versus 1 in the two sequences (S383). What he lacks is a population
  or observer statistic, not a move off the cells. His cues are disjunctive
  (raise P(A or B), then P(not-A or B), to .99), not attribute-local.
  **Do:** reword to "what he lacks is the population statistic".
  **Refined formally (lean/Literature/Doring.lean):** "1/6 versus 5/6" and
  "one fifth" are his roundings of 19/118 versus 99/118 and 19/99. With his
  own numbers the third step gives P(A) = 97/590 versus 493/590 (.164 versus
  .836), not "near 0 and 1"; that needs his "playing with the numbers"
  limit (`gap_tends_to_one`). The A-marginal already differs after two steps
  (91/190 versus 99/190), so the D2 conclusion stands. His remedy is one
  Jeffrey update on the original prior (`fig2`), confirming P2. Paper slips:
  the Dempster gap is 1/10100 per cell (about 1/100 of a point, not 1/1000),
  and Figure 3's 42.2 should be 42.3.
- [ ] **D3. verification_coverage.md is out of date.** Counts 13 sympy
  scripts; run_all.py has 15 (verify_example, verify_interior_omega added).
  Line 61-62 points to literature/ for Doring (see P1). **Do:** refresh.
- [ ] **D4. literature/bohren_imas_rosenberg2019/sympy/check_reversal.py**
  still asserts a discrimination reversal that review log Entry 10 says was
  blocked; kept deliberately "with its failures intact". **Confirmed
  2026-09-29:** it exits 1 at 4/8, because it uses the paper's P^B as "BIR's
  Bayesian" and P^B matches likelihoods to each group's prior, so even the
  novice comparison comes out negative (the D12 point, in code). The correct
  BIR evaluator is now `literature/bohren_imas_rosenberg2019/sympy/
  check_normal_model.py` (37/37) and `lean/Literature/BohrenImasRosenberg.lean`,
  which proves Proposition 2 in full along any history (`prop2_no_reversal`,
  `prop2_decreasing`, the latter supplying a step BIR's own proof on p. 51
  leaves out). **Do:** add a header line saying the script is a record of a
  blocked derivation and exits 1 by design, and exclude it from any runner.

- [ ] **D5. papers_dialectic.tex:91, 213. WRONG-LOCATION.** "it seems
  implausible that the most recent experience ..." starts on p. 98, not
  p. 99.
- [ ] **D6. paper_review_log.md:804. MISQUOTED.** "how completely we dismiss
  previous experiences"; the text is "dismiss previous experiences so
  completely" (p. 115).
- [ ] **D7. papers_dialectic.tex, Hawthorne. CAVEAT x3.** (a) Note 15 cites
  Lange as well as Diaconis-Zabell and Doring; the dialectic drops Lange.
  (b) "Hawthorne ... agrees with the verdict" of Doring, but his notes 2 and
  10 list Doring as a defender of standard sequential updating. (c) The
  bibliography says the Update Reordering Theorem was "verified"; only one
  instance was checked.
- [ ] **D8. literature/hawthorne2004/README.md. CONTRADICTED + WRONG-LOCATION
  + CAVEAT.** (a) :101-106 says the likelihood-ratio example uses "exactly the
  same likelihoods and reports" read as Bayes factors; Hawthorne changes the
  reports to LR .50 and 2 (the Bayes-factor reading of the .90 reports would
  be 1/9 and 9). (b) :83 puts Extended Rigidity in Section 8; it is Sections
  6-7 (only Basis-Overwrite and Basis-Commuting are Section 8). (c) The
  Reordering Theorem description treats r as free; the theorem fixes
  r = NL[Q_alpha-epsilon,d,D_i] / NL[Q_alpha,d,D_i], and
  `sympy/check_reordering_theorem.py` prints t_j/w_j, which is not his r.
  **Do:** correct the README and the script's printed quantity.
- [ ] **D9. interior_omega.tex:381-384 and literature/weisberg2009/README.md:49.
  CAVEAT, substantive.** Weisberg is said to "set aside" non-commutativity on
  input distributions (p. 9), with the omega = 1 model placed there. He calls
  that non-commutativity a *desirable feature* (Lange's point: the same
  experiences in reverse order should yield different inputs). The omega = 1
  model fixes the same inputs in either order, so its non-commutativity is a
  failure of commutativity on *experiences*, which Weisberg keeps as a
  desideratum. **Do:** re-place the model in the Weisberg table; this changes
  the "on neither horn" cell.
- [ ] **D10. interior_omega.tex:89-91. CAVEAT.** The "order dependence in the
  rule versus in the inputs" distinction is credited to Weisberg; he never
  frames it that way, and the correlated-partitions mechanism in that row is
  Diaconis-Zabell's. The undercut/rebut vocabulary is analogy (his F is a
  defeater proposition, not a second attribute's cue). Lange claims are all
  second-hand through Weisberg. Weisberg's Bayes factor is an odds ratio, not
  the NL form. **Do:** reword the attributions in Table 1 and Section 5.

- [ ] **D11. BIR "unstated scope condition". CONTRADICTED.** review log
  :358, 387-391 (Entry 9) calls Bayesian updating an unstated scope
  condition of Proposition 2; BIR state Bayes' rule explicitly as a model
  assumption (p. 11). **Do:** annotate Entry 9 ("stated assumption, not
  unstated").
- [ ] **D12. check_pinning_kills_partiality.py:17-20, 28, 123. CONTRADICTED.**
  Docstring says a Bayesian evaluator with exogenous likelihoods "is the
  object their Proposition 2 is about"; Proposition 2 concerns histories
  whose informativeness is endogenous, and their fn 10 (p. 19) calls the
  exogenous case immediate. **Do:** fix the docstring (the check itself
  stands).
- [ ] **D13. the_discrimination_problem.tex:150-156, 279-285. CONTRADICTED x2.**
  (a) "exhaustive trichotomy": BIR "allow for three potential sources" (p. 2),
  say other misspecifications can also produce reversals (p. 19), and discuss
  attrition, variance differences and self-fulfilling beliefs (pp. 24-25).
  (b) All three sources are "a defect of the evaluator" and "set the belief
  gap and the preference gap to zero and nothing remains": BIR's impartial
  type has correct beliefs, no animus and Bayesian updating, yet
  "discriminates against males in the second period" (p. 20). This undercuts
  the note's "person-based versus process-based" framing. **Do:** rewrite
  both paragraphs; drop the trichotomy-is-exhaustive framing.
- [ ] **D14. review log Entry 5 :186-188. CONTRADICTED, never corrected.**
  "reversal comes from biased priors overshooting": Proposition 2 says a
  single biased type never reverses; the reversal comes from the impartial
  type's inference about the heuristic type (pp. 19-21). Entry 10 has the
  right account. **Do:** annotate Entry 5.
- [ ] **D15. BIR/Heckman page errors.** Aggregate Proposition 1 formula is on
  p. 17, not p. 16 (the_discrimination_problem.tex:114, review log :469).
  "posterior mean is increasing in the prior mean" is on p. 19, not p. 18.
  Heckman's "nothing guarantees" is on p. 109, but positioning_economics.tex
  :42-45, 93-94 gives only p. 102. BIR's appendix does impose conditional
  independence (shocks independent pp. 9-10; signal given ability
  independent of the prior mean, p. 47), which closes review log Entry 7's
  open question. Reading order of reputation versus content is never
  discussed; closest is p. 3, "Both the username and the level of reputation
  are prominently displayed adjacent to any post."
- [ ] **D16. question_and_answer.tex:63-65.** Still says the studies of
  Hogarth-Einhorn and Asch "elicit a single evaluative level"; the review log
  (:522) already calls this wrong for Asch. **Do:** fix (the file is drafting
  history per PLAN, but it is still wrong).

### Literature READMEs and scripts, kinematics papers (group C)

- [x] **L1. literature/field1978/sympy/check_commutativity.py. BUG.** (Fixed 2026-09-30; eq. (7) commutation now passes, with a regression check.)
  `tilt_step1` ignores its input argument, so the commutativity check prints
  non-zero differences; the README (:18-20) says eq. (7) was verified to
  commute. The theorem is true (checked independently). **Do:** fix the
  function to reweight its argument, rerun, and add the script to a runner.
- [ ] **L2. literature/field1978/README.md:33-36. CONTRADICTED.** "e^{2 alpha}
  is a squared likelihood ratio": by eq. (4), e^{2 alpha} *is* the likelihood
  ratio; e^{alpha} is its square root, and alpha is half the log-odds shift.
- [ ] **L3. literature/diaconis_zabell1982/README.md.** (a) :39-41 says Thm 3.2
  is proved via Csiszar; it is proved by direct algebra (pp. 825-826); Csiszar
  is the omitted proof of Thm 3.1. CONTRADICTED. (b) Example 5.1 is in
  section 5.3, not 5.2. (c) "When is successive updating reasonable?" is
  followed by D-Z's own proposal, not left open. (d) The c = 0 equals
  Jeffrey-independence claim is supported by the p. 826 Remark, which the
  README should cite. (e) "footnote at line ~81" is stale.
- [ ] **L4. literature/garber1980/README.md.** "far more interesting (and far
  more difficult)" loses its parentheses (p. 145; MISQUOTED). "neither
  correct nor necessary" is his opening thesis (p. 142), not his conclusion.
  The text layer garbles eqs. (3)-(4). Garber's prose says ".5019", a typo
  in the paper; his table and the computation give .5091.
- [ ] **L5. literature/wagner2002/README.md.** (a) :68-72 says Thm 4.1 makes
  P^B "the" benchmark; Thm 4.1 does not single P^B out, and the README's own
  check found a one-parameter family of commuting schemas (UNSUPPORTED).
  (b) :62-64 calls matching-target routes "Field's simpler special case";
  Field's case is the general finite schema, matched targets are the D-Z case
  (Remark 3.4) (CONTRADICTED). (c) Full support also needs qualitative
  independence of the partitions for (4.3)-(4.4) (Remark 4.1); D-Z already
  had the matched-case necessity (Remark 4.3). Wagner Remark 5.1 is the
  explicit answer to Garber.
- [ ] **L5 settled formally (2026-09-30, lean/Literature/Wagner2002.lean,
  75 theorems).** Theorem 3.1 makes P^B the common endpoint of Wagner's
  two-route schema when each cue's first-position update is its delivered
  credence against the prior and each second-position update carries the same
  Bayes factor (`PB_endpoint`), so "combining Bayes-factor content is
  sequence-invariant (Wagner 2002)" is accurate. Theorem 4.1 (conditions hold
  on the paper's full-support grid, `grid_43_44`) makes P^B unique *given that
  anchoring* (`PB_unique`). Not licensed: P^B as *the* benchmark; every route
  has a Bayes-factor-consistent partner ending elsewhere (`completion`; the
  B-first Jeffrey sequence gives 36/55 at cell EF against P^B's 27/40,
  `wagner_does_not_single_out_PB`). What picks out P^B is the paper's choice
  to read each cue's factor against the prior; the README's old
  "one-parameter family" evidence changed the cue itself. Corrected wording
  for MS:171, 216-217, 277 in the README. Pettigrew-Weisberg
  (PettigrewWeisberg.lean, 54): Theorem 1 for regular P on finite partitions;
  Theorem 2 needs an unstated hypothesis (pooling regular distributions gives
  a regular one) that their proof uses in (6) and (8)
  (`RegularityPreserving`; upco satisfies it, `upco_*`); P^B is upco-then-
  Jeffrey pooling only when the pooled opinion is the likelihood matched
  against the prior, not the delivered credence (`PB_is_upco_pooling`; on
  their p. 4 numbers the delivered credences give 18/29 at EF against 27/40).
  Wagner 2003 (Wagner2003.lean, 31): note 5's "unless Q = p there is no r"
  also needs q != p (`absurdity_needs_learning`); d fails criteria II and III.
- [ ] **L6. literature/wagner2003/README.md.** (a) :3-4 "considered experiences
  ... Lange/Cassell exchange": the phrase is Wagner 2002 note 9; Wagner 2003
  mentions neither Lange nor Cassell (WRONG-LOCATION). (b) Three indices
  (d, D, pi), not two; criterion II is "on the same partition" (MISQUOTED).
  (c) :40-42 says the d-index failure of criterion II is not checkable; note
  4 (p. 363) gives a numeric counterexample, q'(H|E) = .6286 versus
  p'(H|E) = .5 (CONTRADICTED). (d) Thm 2.1 is for purely atomic algebras;
  the generalisation of the 2002 result is Thm 3.2 in section 3, which the
  README files under old evidence.
- [ ] **L7. literature/pettigrew_weisberg2025/README.md:45-47. WRONG-LOCATION.**
  The "no prior opinion" gloss on beta/(beta+1) is P-W's own main text (p. 7),
  not "Field's own gloss (footnote 9)"; P-W fn 9 only says Field's alpha is a
  log-scaled beta; Field never writes beta/(beta+1). Thm 2 as stated omits
  "regular P". question_and_answer_doc.tex:52-53 lists P-W as "preprint /
  forthcoming"; published Phil. Imprint 25(8), July 2025.

### Literature READMEs and Lean, record papers (group H)

- [ ] **L8. Augenblick-Rabin record. CONTRADICTED x2.** (a) review log
  :86-91 and README "What formalizing revealed": that `excess_step` is a pure
  algebraic identity is stated by AR themselves (p. 3 "can be simplified as
  (2 pi_t - 1)(pi_t - pi_{t+1})", fn 21, p. 11), not revealed by
  formalizing. (b) review log :106-111 "No alternative rule is proposed": AR
  section 3.1 (p. 24) gives LR[pi_{t+1}] = LR[pi_t]^alpha LR[s]^beta.
  UNSUPPORTED: Prop 4 "measure-theoretic" (its proof is a finite
  construction); a belief stream "could not express" permutation
  non-commutativity; PRO "completes a classification AR decline to attempt"
  (AR only disclaim optimality). Lean header says definitions are "verbatim"
  but the paper writes pi, not theta. Drive copy is the Nov 2020 working
  paper; all numbering is working-paper numbering.
- [x] **L9. Shmaya-Yariv record.** (Resolved 2026-09-30: `ConjExp.Restricted` encodes Definition 2; `no_reversal_of_restricted` now concludes sigma(s) = a*, the real necessity direction of Theorem 1; notation is the paper's (alpha, tau, zeta); `alpha_depends_on_nu` proves non-restriction; `reversal_example` added. Sufficiency half and Theorems 3-4 not formalized.) (a) README:8 "Definitions 1-3 formalized":
  Definition 2 (restricted) is not encoded in the Lean (CONTRADICTED).
  (b) `no_reversal_of_restricted` is described as the necessity direction; it
  assumes the convex-combination step and concludes only equal scores, not
  sigma(s) = a (UNSUPPORTED). (c) Def 1 notation: the paper has
  (alpha, tau, zeta) valued in A, bold N = {0..N}, S^N (MISQUOTED).
  (d) "events don't overlap when nu is a function of history": they are
  disjoint in every conjectured experiment (UNSUPPORTED). `alpha_depends_on_nu`
  does not prove non-independence. Drive copy is the 2008 working paper.
  **Do:** encode Definition 2 and prove the necessity direction properly, or
  narrow the README.
- [ ] **L10. measurement_susceptibility_survey.md.** (a) :67 says Jeffrey's
  term is "rigidity": ZO attribute "invariance" to Jeffrey (2004, section 3.2);
  "rigidity" is Oaksford-Chater's and Over-Hadjichristidis's (ZO fn 1)
  (CONTRADICTED). (b) :101-102 rewrites the ineffability parenthetical, which
  reads "(as stressed by Jeffrey, 1983, section 11.1)" (MISQUOTED). (c) :126
  "single judgment per subject" in Zhao 2012: each gave five (WRONG-NUMBER).
  (d) :152-153 the swing-state result is "per-subject, not an aggregate
  audit": it is a between-group comparison against a control mean
  (CONTRADICTED). (e) ZO "stability": 22 of 40 changed Pr(G|B).
- [ ] **L10 confirmed formally (2026-09-30; lean/Literature/ZhaoOsherson.lean,
  Zhao2012.lean).** (a)-(e) hold. Qualifications: (a) Jeffrey himself uses
  "rigidity condition" in the 2002 draft, ch. 2 (J6), so report what ZO say
  rather than calling the word foreign to Jeffrey; (e) what holds is
  relative: Pr(G|B) moved less than its converse (33.0% versus 73.0%; 22
  versus 32 of 40 changed), against a normative baseline of zero versus a
  positive amount (`converse_invariant_iff`); (d) Zhao 2012's groups are
  yoked triples with paired tests t(19), so "between-group (yoked)" is exact
  and "per-subject" wrong. P15: Asch's I->E modal share is 13/34 = 38%,
  printed 39% only by forcing the column to 100 (`table8_ie_rounding`).
  Paper slips not used by the project: ZO p. 304 "18.45%" should be 18.05%;
  Zhao 2012's 17-of-20 binomial p is .0026, printed .01.
- [ ] **L11 formal records (2026-09-30).** Wilson (lean/Literature/
  Wilson.lean, 85 theorems, April 2003 draft numbering): Lemma 1, eq. (4),
  the N = 3 optimal rule gamma*(eta) = (sqrt(2 eta - eta^2) - eta)/(1 - eta),
  Corollary (i) lower half and (ii) at N = 3, Lemma 4, Theorem 6 (i), (iii),
  Theorem 7 (ii) core, Theorem 4 (i) pathwise step. **Draft Theorem 5 (i) is
  false as stated:** N = 5, j = 2, k = 4, t = 2 meets its hypothesis yet
  polarization has probability 0 (`thm5_draft_counterexample`; also
  (7,2,6,2), (7,3,5,4), (9,2,8,2), (9,3,7,4)); exact threshold
  min(2j - 1, 2(N - k) + 1) (`thm5_i_corrected`). Check against the 2014
  published version before relying on it. Project question: with one binary
  state the association's sign is fixed by the kernel
  (`assoc_sign_fixed`), so Wilson makes no association prediction by
  construction. Cassell (lean/Literature/Cassell.lean, 33 theorems): later
  input wins; her raven Figures 1-2; same posterior from different priors
  needs different Bayes factors; reversal of inputs and of Bayes factors
  coincide only when nothing moves; Bayes-factor updates commute; ECJC is
  reorder-invariant. The jellybean example is Weisberg's (pp. 3-4), not
  Cassell's; the IO:329-330 form of Lange's point is a separate theorem, not
  the contrapositive of Cassell's; her fn 5 "just in case" fails on a single
  partition (as Wagner 2002's section 4 example). Proposed wording for
  IO:329-330, IO:382, WBR:49, LOG:699-701, 771-772, 828 and PLAN:1337-1338 is
  in the two READMEs.
- [ ] **L11. Wilson and Cassell identities.** `AndreaWilson.pdf` is the
  April 29, 2003 draft, not the 2014 Econometrica paper. `lisa_cassell.pdf` is
  Cassell, "Commutativity, Normativity, and Holism: Lange Revisited", Can. J.
  Phil. 50(2) 159-173 (online 2019, volume year 2020). All Lange claims in the
  notes are second-hand via Weisberg and Cassell.

## 4. Drive folder and sources

- [ ] **S1. `goodmittal1987.pdf` is not Good-Mittal.** It is I. J. Good
  (1960), "Weight of Evidence, Corroboration, Explanatory Power, Information
  and the Utility of Experiments", JRSS B 22(2), 319-331. The real paper is
  `goodmittal1987_1.pdf` (Ann. Statist. 694-711, complete). **Do:** rename
  or remove the mislabelled file.
- [ ] **S2. `domotor1980.pdf`:** journal p. 387 scanned twice; PDF pp. 22-27
  blank; journal pp. 384-403 complete. `phelps1972.pdf`: paper on 3 pages,
  4 blank. `BCGS_stereotypes_june_6.pdf` is the May 2015 working paper.
  `heckman` file is named 2011 (group D will say what it is).
  `Arrow_1973` is the 1971 Princeton working paper 30A.
- [ ] **S3. Not in the `temp` folder:** Becker 1962, Epstein 2006, Ortoleva
  2012 (only the 2024 Annual Review is there), Tao 2011, Jeffrey 1988,
  Doring is there but has no `literature/` record. Jeffrey 1983 *The Logic
  of Decision* and *Subjective Probability* are in the books folder
  (`1-REBD20dZB84hLl0udakW1EImR1xiMAn`), which is not link-shared, so they
  cannot be downloaded from this session. **Do:** copy the two Jeffrey books
  (or at least ch. 11 of The Logic of Decision) into `temp`; add Epstein,
  Ortoleva 2012, Becker 1962.
- [ ] **S4. `phelps_slides.pdf`, `hayashi.pdf`, `thoma_mistakes.pdf`,
  `quantum_nature_of_human_perception`, `sen_poverty.pdf`, `lisa_cassell.pdf`,
  `AndreaWilson.pdf`** are in Drive but cited nowhere in the manuscript.
  Wilson and Cassell appear in the notes (group H will report).

### Jeffrey 1983 and 2004 (group J, 2026-09-30; verify_jeffrey.md)

Sources: *The Logic of Decision*, 2nd ed., 1983 (Drive scan; Chapter 11,
pp. 164-183, read in full); *Subjective Probability: The Real Thing*, 2002
draft of the 2004 CUP book (Chapter 3, pp. 55-65). Draft section and page
numbers may differ from the published book. 19 claims: 7 VERIFIED, 7
CAVEAT, 1 MISQUOTED, 3 CONTRADICTED, 1 NOT-CHECKABLE.

- [ ] **J1. Framing, the strongest anchor is Jeffrey himself.** 1983,
  pp. 182-183: non-commutativity "is as it should be"; the demand for
  commutativity "stems from a conflation", and he names Domotor (1980,
  p. 395) as its source. **Do:** cite Jeffrey (1983, pp. 182-183) for "the
  order effect is not a defect", beside D-Z Remark 2 (M24); this also
  settles how to cite Domotor (M1): as the source of the commutativity
  demand Jeffrey rejects.
- [ ] **J2. MS:115-117, the two-horn argument, papers_dialectic.tex,
  two_horn_motivation_body.tex. CONTRADICTED.** "A Bayes factor requires the
  probability of the same credential for a candidate who is not competent"
  (a counterfactual likelihood). Jeffrey's Bayes factor (2002 draft, p. 61)
  is new odds over old odds and needs no likelihood; the likelihood-ratio
  reading holds only for conditioning on a certainty "assuming rigidity"
  (p. 42). In one step the credence and Bayes-factor readings are the same
  update; they differ in what stays fixed when the input meets a different
  prior. Jeffrey draws the line by provenance: one's own experience yields
  credences that are "data" (p. 59); another's report should be converted to
  factors because it is mixed with that person's prior. **Do:** rebuild the
  premise paragraph on the provenance argument (own impression = credence),
  which supports modelling a panelist's impression as a credence, and drop
  the counterfactual-likelihood argument.
- [ ] **J3. MS:190-191. CONTRADICTED (with M23).** "The partition satisfying
  the invariance condition": Jeffrey p. 174 allows "a certain latitude in the
  choice" of that partition.
- [ ] **J4. MS:184-186. CAVEAT.** Chapter 11 never uses "invariance"; it says
  the change "originates in" the partition (pp. 168, 174). "Invariance" is
  the 2004 book's term (section 3.1-3.2, pp. 56-58). MS:111: Jeffrey says
  "uncertain evidence", not "soft evidence" (pp. xii, 167); "impression" is
  his word (p. 165). **Do:** cite the 2004 book for "invariance" (add it to
  bibliography.bib) or use "originates in".
- [ ] **J5. papers_dialectic.tex:70. CAVEAT.** "even Jeffrey took [the
  factor escape]": in 1983 he rejected Field's parameters as "epistemological
  geegaws that do no work. I prefer to make do with the a_i and b_j"
  (p. 183); the 2002 draft adopts factors only for other people's reports
  (p. 59). **Do:** date and scope the claim.
- [ ] **J6. Survey and audit terminology.** survey :67 "Jeffrey's is
  'rigidity'" is CONTRADICTED: his kinematics term is "invariance" (2002) or
  "originates" (1983); but Jeffrey himself uses "rigidity condition" in
  Ch. 2 of the 2002 draft (pp. 40, 42, 49), which qualifies L10(a). ZO's
  "(Jeffrey, 1983, section 11.1)": exact quotation, but the fullest statement
  is section 11.2 (p. 166) and he never says "ineffable". Jeffrey p. 181
  derives Chapter 11 from Ch. 3 of his 1957 dissertation (D-Z cite Ch. 4).
- [ ] **J7. Draft slips.** 2002 draft Example 5 (p. 60) prints factors that
  do not normalise (correct 8/7, 4/7, 12/7; new(H) = 3/7, not 1/2), and
  formula (2) prints pi-prime for pi. Do not quote those numbers.
- Lean: `lean/Literature/Jeffrey.lean` (18 theorems): kinematic updates on
  independent finite partitions commute; factor updates compose to one
  update on the product partition and commute; one factor update equals one
  Jeffrey update written two ways; kinematics as conditioning on a richer
  space; the relevance identity (11-5); the book's worked numbers.

## 5. Formal records (literature/ and lean/Literature/)

- [ ] **R1. Papers with no record at all** get a `literature/<paper>/` record
  (README with page-cited claims, Lean file in `lean/Literature/`, SymPy
  check where numeric): Doring, Domotor, Good-Mittal, FGT, Hogarth-Einhorn,
  Asch, Cripps, Dietrich, BHW, Banerjee, Phelps, Arrow, Coate-Loury, BIR
  (README missing), Heckman, BCGS; Weisberg (README only). **Done
  2026-09-29:** 16 new Lean files in `lean/Literature/`, 443 literature
  theorems in all, full `lake build` clean, every theorem on the standard
  axioms or a subset (in `check_axioms.lean`); `literature/run_all.py` 29/29.
  Asch has sympy only (no formal claim).
- [x] **R2 (done 2026-09-30: all seven now have Lean and asserting scripts). Sympy-only records with empty `lean/` directories:**
  Diaconis-Zabell, Field, Garber, Hawthorne, Pettigrew-Weisberg, Wagner 2002,
  Wagner 2003. Add Lean for the closed-form identities (Field eq. 7
  commutativity, Wagner Thm 3.1, P-W's upco/Field identity, Garber's
  recurrence, Hawthorne's LR-model commutation).
- [ ] **R3. Not in Drive, no record possible yet:** (Jeffrey 1983, the 2002
  draft of Jeffrey 2004, and Epstein 2006 were added to Drive 2026-09-30 and
  are now checked) Ortoleva 2012 (via the 2024 survey only), Becker 1962 (web copy),
  Jeffrey 1988.
- [x] **R4. Literature scripts are in no runner.** `literature/run_all.py`
  added 2026-09-29 (29/29). Remaining gap: the seven sympy-only scripts print
  but do not assert, so their exit status says nothing; convert them to
  assert (with L1's Field fix) when R2 is done. Consider a `make literature`
  target.

- [ ] **R5. Standing requirement (author, 2026-09-30): every cited paper has
  a Lean formalization of the mathematics the project relies on, built clean
  with standard axioms, so the paper stands on verified foundations.**
  Coverage, updated as passes land:

  | Paper | Lean | Pass |
  |---|---|---|
  | Augenblick-Rabin, Shmaya-Yariv | yes | 0 / C |
  | Weisberg, Doring, Domotor, Good-Mittal, FGT, Hogarth-Einhorn | yes | 1 |
  | Cripps, Dietrich, BHW, Banerjee, Phelps, Arrow, Coate-Loury | yes | 1 |
  | Bohren-Imas-Rosenberg, Heckman, BCGS | yes | 1 |
  | Jeffrey 1983/2004, Benjamin-Bodoh-Creed-Rabin 2019 | yes | 2 |
  | Hawthorne, Diaconis-Zabell, Field, Garber | yes | A |
  | Wagner 2002, Wagner 2003, Pettigrew-Weisberg | yes | A |
  | Epstein 2006, Ortoleva 2012 (via 2024 survey), Tao 2011, Becker 1962 (web copy) | yes | 2/B |
  | Asch (data), Zhao-Osherson 2010, Zhao et al. 2012 | yes | B/C |
  | Wilson 2014 (2003 draft), Cassell 2020, Shmaya-Yariv Def. 2 | yes | C |
  | Jeffrey 1988 | blocked, not in Drive | -- |

## 6. Cross-document consistency (to do after the pass)

- [ ] **X1.** Check that the literature edits in PLAN 3.A-3.D carry what
  `papers_dialectic.tex` argues, and that both agree with the sources once
  the corrections above are made. Known already: D1/P3 (Doring's objection
  is normative, not psychological) affects both.
- [ ] **X2.** Table 2 says "share of candidates"; the text and Prop. SHR say
  evaluators (noted in PLAN W.A).
- [ ] **X3.** Whether to retire `positioning_economics.tex` (review log
  Entry 16 recommendation).
- [ ] **X4.** Weisberg 2009 citation unconfirmed from the preprint (group A
  will report).
- [ ] **X5. Incorporate the exploration documents (author, 2026-09-30; after
  R5 is complete).** Every underlying point in the exploration documents must
  be covered by the manuscript or by a change in
  `notes/manuscript_change_plan.md`, or be recorded as deliberately left out
  with the reason. Documents: `papers_dialectic.tex`, `interior_omega.tex`,
  `the_discrimination_problem.tex`, `positioning_economics.tex`,
  `question_and_answer.tex`, `two_horn_motivation_body.tex`,
  `empirical_analytics.tex`, `worked_example.tex`, and the substantive
  entries of `paper_review_log.md` (e.g. Entry 17, the zero-slope test).
  Method: (1) extract each document's points, one line each; (2) map each to
  a manuscript line or plan item, marking covered / partly covered / missing;
  (3) before any point is carried over, apply this audit's corrections to it
  (e.g. D1 Doring normative, P10-P12 Hogarth-Einhorn and the position
  channel, D13 BIR trichotomy, J2 the counterfactual-likelihood argument), so
  no corrected claim re-enters; (4) for missing points, draft new plan
  entries in the plan's BEFORE/AFTER format under
  `notes/writing_discipline.md`; (5) record points left out and why. X1 (do
  the plan's literature edits carry the dialectic?) is the first case.
  Output: a coverage matrix in the plan's front matter, plus new plan
  entries.
