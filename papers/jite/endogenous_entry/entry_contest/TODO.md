# TODO queue — entry_contest

Working queue. Maintained across sessions. **Check this and
`lit/TRACEABILITY.md` before starting work.**

Conventions: items are ordered by consequence, not by effort. `[ ]` open,
`[~]` in progress, `[x]` done this cycle (cleared on the next pass), `[!]`
blocked or needs a decision from Anurag. Each item records what would count
as done, so nothing gets marked complete on a plausible-looking partial.

Last updated: 2 September 2026, after the N1/N4/R1 pass and an independent
review of it (Lean 4.33.1 recompiled from scratch, 47 theorems, 0 sorry, ten
new theorems axiom-audited; both changed suites re-run). F1-F7 were that
review's findings; N5, U2 and A1 are candidate directions and a queued gap.
**All of F1-F7 CLEARED 2 Sep** — suites re-run, `VERIFICATION.md`
regenerated, `PROOFS.pdf` rebuilt (22pp), every count and every Lean name
cited in the new propositions audited against the generated evidence and the
source. Open queue is now N5, W1, R2, R3, M1 and the Later items. **A1 and V1 closed
2 Sep**, along with open item 3 (the tail-condition band) and the
burden-monotonicity restatement; see Recently closed. Lean is at 52 theorems,
0 sorry, 52/52 axiom-audited, 36 axiom-free; `verify.sh` runs 10 suites (suite 10 is doc consistency, added on review).

---

## Now

- [x] **N0. Identity-pinning claim withdrawn; count-invariance proved.**
  `SOUNDNESS_20260902.md` Finding 1. The claim that complete information pins
  entrant *identities* was **false** — counterexample `Delta=(12,10,1)`,
  `kappa=(2,5,8)` gives three equilibria including one with the richest
  challenger absent; identity multiplicity in 17–22% of sampled instances.
  Independently reproduced. What replaced it is stronger than what was
  claimed for the assortative construction alone: the **count** is the same
  in every pure-strategy equilibrium (`equilibrium_count_unique`), with
  identity uniqueness under the stated condition (`members_below_kstar`).
  Lean 35 theorems, `checks/verify_equilibria.py` 6 checks.
  **Lesson to carry:** the Lean layer could not have caught this — its
  `Enters`/`EntersAt` machinery takes the assortative assignment as given, so
  no theorem quantified over arbitrary entrant sets. Same failure class as
  the silent Lean skip: *the prose generalised beyond the layer that was
  verified.* Before asserting any structural claim, ask which layer would
  fail if it were false.

- [x] **N1. Endogenous-margin pivot rule. RESOLVED 2 Sep — positively.** The sign rule does survive the margin moving, and the reason is the descending wealth order: a pivot comparison at the marginal index automatically covers all inframarginal entrants (richer, hence also above pivot) or all extramarginal agents (poorer, hence also below). No post-spread margin need be located. Machine-checked (`entry_preserved_below_marginal_index`, `nonentry_preserved_above_marginal_index`, `descending_preserved`); 20,000 economies with `k*` recomputed both sides, 0 violations, count moving in 393. Control: comparing at the richest index instead gives 154/2744 violations, so the marginal index is load-bearing, not slack. Stated as Proposition (endogenous margin) in `PROOFS.tex` and folded into §Contribution. **This is now a claimable strengthening over Costrell–Loury, whose marginal quantile is fixed by technology.**
  - What the gap was: in Costrell-Loury (2004) the marginal worker's quantile
    `theta` is fixed by technology, so it does not move when `F` does; only
    `mu-hat = F^-1(theta)` shifts. Here the marginal index is itself the
    object the comparative static is about. That is the respect in which the
    result differs from its closest structural precedent.
  - Superseded 2 Sep by R1: the pivot hypothesis is not needed at all, only
    a one-sided condition on the quantile difference. N1 is now the special
    case of the tail condition in which the pivot property supplies that sign
    (`pivot_imp_upToMargin`, `pivot_imp_downFromMargin`).
  - Independently re-verified 2 Sep: Lean 4.33.1 compiles clean, 47 theorems,
    0 sorry; the three N1 theorems are axiom-free. Suite re-run green.
  - Carries over to F4 and F6 below: the fall branch's proof text has an
    endpoint error, and the whole proposition inherits the anonymity
    dependence tracked in A1.

- [x] **N4. Assortative selection. RESOLVED 2 Sep — argument made, not dropped.** The assortative set is always an equilibrium, minimises total realised entry cost among all equilibria, and — because Delta depends on the count alone (anonymity) and the count is invariant (P5-inv) — is therefore the surplus-maximising equilibrium: the prize term is common, so aggregate surplus differs across equilibria only through total entry cost. Exchange step machine-checked (`cost_exchange_le`, `totalCost_cons`, `surplus_gap_is_cost_gap`); full minimisation is the standard sorting fact, checked numerically (6000 economies, 1335 with genuine multiplicity, 0 failures; control: non-assortative count-k* sets strictly costlier in 3892 cases). Stated as Proposition (assortative selection) and folded into §Contribution. **Recorded limits:** it is a normative selection, not a strategic refinement — nothing says players coordinate on it — and it is silent on distribution.

- [x] **F1. DONE 2 Sep. `PROOFS.tex` overstated the R1 evidence by 4x.**
  Fixed by raising the draw budget, not by loosening the generator: the
  sampler is byte-identical and the first 758 draws are the same, so the
  tested population is unchanged. 808,535 draws are needed for 3000 survivors
  (~4s), budget set to 1,000,000. Suite now prints 3000 spreads / 977
  multi-crossing / 2276 genuine MPS / 0 violations, and `PROOFS.tex` cites
  exactly those. Every count in the R1, N1 and N4 blocks re-audited against
  the regenerated `VERIFICATION.md`; all match. **Original finding:** The results table and §R1 both cite "3000 spreads"; the
  generated `VERIFICATION.md` says **758**. The loop in `verify_p9gen.py`
  draws 200,000 candidates and breaks at `ntest >= 3000`, but the filters
  (`k0 >= Q-2`, min-wealth, sorted-preservation) reject enough that the break
  never fires. `TODO.md`'s own R1 entry repeats 3000. Independently
  reproduced.
  - Done when either the draw budget is raised until 3000 survive the filters,
    or every occurrence of 3000 is corrected to whatever the suite actually
    prints. A number in `PROOFS.tex` that `VERIFICATION.md` contradicts is the
    worst class of error this bundle can carry.

- [x] **F2. DONE 2 Sep. "General mean-preserving spreads are covered" was
  false as written.**
  The §Contribution sentence now reads "covered when, and only when, its
  quantile difference is signed down to the margin"; the R1 subsection states
  the non-implication with the witness; the open item says the class is
  proper and cites the check. New suite block **R1-SCOPE** makes it evidence
  rather than a caveat: 37 of 4000 genuine MPS instances fail the tail
  condition with `k*` strictly falling, witness `Q=10`, `k*` 8->7. The
  genuine-MPS subcount is now reported separately in R1 (2276 of 3000; 253 of
  the 977 multi-crossing). **Original finding:** MPS does **not** imply the tail condition. Checked directly
  (second-order dominance tested, not merely `sum(d)=0`): in 4,000 genuine MPS
  instances, **37 fail the tail condition and `k*` strictly falls** — witness
  `Q=10`, `k*` 8 -> 7. The scope caveat is not removed; it is relocated to a
  hypothesis stated in terms of an endogenous object (`k*`).
  - `PROOFS.tex`'s own new open item ("When does the tail condition hold?")
    says exactly this and is correct. The offender is `TODO.md`'s R1 entry
    ("so general mean-preserving spreads are covered") and the novelty-ledger
    row ("Removes the scope caveat the claim previously carried"). They
    contradict `PROOFS.tex`.
  - Also, at the pre-fix sample of 758: 577 were genuine MPS and only 64 of
    the 245 multi-crossing cases were. (Superseded by the F1 fix — the suite
    now runs 3000, of which 2276 are genuine MPS and 253 of the 977
    multi-crossing cases are. These 758/577/64 figures are the historical
    record of what was found, not current counts.)

- [x] **F3. DONE 2 Sep. Mean-preservation is not load-bearing in R1 at all.**
  `prop:tailprop` restated on two *arbitrary* sorted profiles with no
  relation assumed between them, and the hypotheses named for what they omit:
  the only properties used are that `kappa` is non-increasing in wealth
  (concavity, nothing more) and that both profiles are sorted so the
  single-crossing lemma defines each count. Added explicitly: `w'` need not
  be obtained from `w` by a spread, a mean-preserving transformation, or any
  transformation at all. Noted that the Lean layer was always at this
  generality — `entry_preserved_of_up_to_margin` quantifies over arbitrary
  `w w'` — so what was corrected is the surrounding prose, not the formal
  content.
  - New check **R1-FREE**: 4000 profile pairs satisfying the tail condition
    and deliberately not mean-preserving (median `|sum(w'-w)| = 0.267`), 0
    violations. The "not a spread" claim now has a layer that would fail if
    it were false.
  - Framing consequence recorded in the text: the proposition is not a
    comparative static about `Lambda`, it is a monotonicity result about a
    threshold statistic, which is then *used* to get comparative statics when
    a spread satisfies its condition. Section retitled accordingly.
  - **Original finding:** 755
  deliberately non-mean-preserving displacements satisfying the tail
  condition: 0 violations. `sum(d)=0` never enters the proof — the theorem is
  about rank-wise dominance above the margin. Cleaner as a result, but the
  connection to the Rothschild-Stiglitz literature is thinner than the
  framing implies.
  - Done when the proposition is stated for what it actually needs (a signed
    quantile difference down to the margin, mean-preserving or not), with MPS
    presented as the case of interest rather than as a hypothesis.

- [x] **F4. DONE 2 Sep. Off-by-one in both fall-branch proofs, and the
  hypothesis was stronger than needed.** Both proofs corrected: the argument
  is now confined to `j > k*`, with an explicit note that at `j = k*` the
  entry condition *holds* — that is what makes her the margin — so no
  conclusion is drawn there and none is needed. Both propositions restated on
  the weaker hypothesis (strict outsiders only; the marginal entrant's wealth
  may move either way).
  - Lean: three new theorems, all axiom-free —
    `nonentry_preserved_of_beyond_margin` (tail condition on `BeyondMargin`),
    `nonentry_preserved_beyond_marginal_index` (endogenous margin, same
    weakening), and `downFromMargin_imp_beyondMargin`, which records that the
    earlier formulation assumed more than the conclusion needs. 50 theorems,
    0 sorry, 34 axiom-free.
  - New check **R1-WEAK**: margin and all inframarginal ranks left free, only
    ranks `> k*` signed — 4000 cases, 0 violations; control signing nothing at
    all gives `k*` rising in 362 of 4000, so the outsider ranks are the
    load-bearing ones.
  - **Original finding:** `prop:tailprop` (ii) and
  `prop:endogenous` (ii) both argue "for every `j >= k*` the entry condition
  continues to fail". At `j = k*` it does **not** fail — that agent is the
  marginal entrant. Lean is unaffected (`hout` is a hypothesis, so the
  theorems only fire on actual outsiders) and both conclusions hold.
  - The hypothesis is also stronger than needed: constraining only ranks
    `>= k*+1` suffices, with the marginal entrant free to move either way —
    4,000 cases, 0 violations.
  - Done when both proof texts are corrected to `j >= k*+1` and the
    propositions are restated on the weaker hypothesis.

- [x] **F5. DONE 2 Sep. The N4 surplus check was vacuous; replaced with one
  that can fail.** The old check compared `(-cost_S) - (-cost_assort)` against
  `cost_assort - cost_S` — the same expression rearranged, with `gain` absent
  — so no input could have broken it. It now computes the payoff gap against
  an explicit gain schedule and tests the hypothesis that actually does the
  work, anonymity: across **9022 equilibrium pairs** the payoff gap equals the
  cost gap when gain is a function of the count alone, and a control that
  lets gain depend on *which* agents entered breaks it in **3022** of the same
  pairs.
  - So the check now names the layer that would fail: identity-dependent
    gain. `PROOFS.tex` cites both numbers where it previously cited a
    tautology, and says where anonymity enters (`|S| = |T|` in
    `surplus_gap_is_cost_gap`).
  - Cross-links to A1: this is a third place where anonymity is shown
    load-bearing rather than assumed.
  - **Original finding:** It
  computes `lhs = (-costs[S]) - (-costs[assort])` and
  `rhs = costs[assort] - costs[S]` — the same expression rearranged — and
  `gain` never appears, so the check cannot fail. By the standing rule below
  ("name the layer that would fail if it were false"), it establishes
  nothing. The Lean `surplus_gap_is_cost_gap` does encode anonymity properly
  (`gain` applied to `.length`, plus `hlen`), so the content exists.
  - Done when the Python leg either tests anonymity (vary the entrant set at
    fixed count and confirm the gain term is unchanged) or is deleted, and
    `PROOFS.tex` stops citing it as evidence.

- [x] **F6. DONE 2 Sep. N4's welfare framing corrected; no collision with W1
  after all.** Resolution: the two operate at different margins and are
  logically independent — N4 ranks equilibria *at* the count `k*`, W1 asks
  whether `k*` is itself too large. Neither answers the other, and
  `PROOFS.tex` now says so rather than leaving a referee to notice. Part (iii)
  restated as maximising *the unweighted sum of payoffs*, not "surplus";
  "planner indifferent to identity" removed as wrong (such a planner strictly
  prefers wealthy entrants); resource-cost invariance and the direction of the
  ranking now carried by a new check, **N4-WELFARE**: over 1344 multiplicity
  economies, all equilibria share size (hence resource cost `k*c`) in 1344
  cases, total `kappa` differs strictly in 1344, and the assortative set has
  the strictly wealthiest members in 1344. Also noted in the text: cardinality
  is already assumed by the privilege-contest form, so what N4 adds is
  interpersonal *comparability*, which is a normative commitment.
  **Original finding:**
  Three distinct problems:
  - In **resource** terms every count-`k*` equilibrium costs exactly `k*c` —
    identical. The ranking exists only because `kappa` is a utility
    increment, so N4 sums cardinal utilities across agents of different
    wealth. That is a utilitarian interpersonal aggregation and it is
    currently unstated.
  - Its **direction** is that entry should be concentrated among the rich,
    because their `kappa` is smaller. So "the one a planner indifferent to
    identity would pick" is the wrong description — that planner is not
    identity-indifferent; it strictly prefers wealthy entrants. Reword.
  - It **conflicts with W1**: W1 is built on Levin-Smith excessive entry,
    where the planner wants *less* entry, and R2 Comment 3 calls the
    expenditure wasteful. The bundle would carry two incompatible objectives.
  - Done when one objective is adopted, or both are stated with their
    relation made explicit — settled **before** W1 is drafted, not after.

- [x] **F7. DONE 2 Sep. Positioning corrected in both places.** The N1
  subsection and §Contribution now say explicitly that the comparison with
  Costrell-Loury is not a claim of dominance: they sign a smooth aggregate
  under a general spread, which is the harder object and is why their
  argument needs machinery this one does not. What is being compared is two
  *formulations* — the extensive margin replaces the aggregate with a
  threshold statistic, and an endogenous margin is tractable in the second
  setting and not the first. Stated in the text: the modelling choice is the
  contribution, the proposition is what the choice buys, and it is
  correspondingly easy once the choice is made.
  - The internal comparison ("stronger than the fixed-index reading of the
    same rule") is kept — that one is a like-for-like statement and is true.
  - **Original finding:** The bundle now says openly that the count is *easier*
  than the aggregates the assignment literature signs, which is the honest
  reading and should stay. But phrased as a strengthening, a referee answers
  that a less demanding question was asked. The defensible framing is that
  choosing the extensive margin is what makes the question tractable — the
  modelling choice is the contribution, not the theorem's strength.
  - Done when §Contribution and the N1 note in `PROOFS.tex` are reworded on
    that framing, and the ledger row for the endogenous margin says so too.

- [x] **N2. State the comparability defence in `PROOFS.tex`.** DONE 2 Sep — §Contribution, "Why the extensive margin is the point", incl. CL Prop 10-vs-6. The contribution
  sentence compares hypotheses across two different models — intensive vs
  extensive margin, two players vs `Q`. A referee can reply that `u'''` is
  obviously not needed because it is not the same comparative static, and that
  objection should be met in the text rather than waited for.
  - The defence: both models answer *what does a mean-preserving spread of the
    resource distribution do to competitive expenditure*, and the margin is
    precisely the modelling choice being defended. Say so explicitly.
  - Attach Costrell–Loury (2004) Proposition 10, which **reverses** their
    Proposition 6 once the assignment re-optimises: the direction of the
    inequality effect in this literature is not robust to whether the
    allocation re-optimises. This cuts both ways and belongs in the discussion
    rather than being left for a referee to raise.

- [x] **N3. Stop presenting the contribution as a combination.** DONE 2 Sep — §Contribution rewritten: leads with the two-branch economic claim (cited to CMP92/HK04/HK10), P5-inv and the dispersive bridge as well-posedness, u''/u''' as support; closes with "What is not claimed" demoting P7 and P-MU to apparatus with their sources. §Contribution
  currently closes on "the combination of results that require the `Q`-player
  binary-entry setting jointly: P7, P9-gen and its strictness, and P-MU".
  Combination claims are routinely discounted, and on the evidence two of the
  three are individually not novel:
  - **P7 is not a novel inequality.** `Accounting.lean` proves one abstract
    lemma with FJL's expected-count bound and P7's cap as *instances* — which
    is itself the demonstration that the accounting logic is theirs. What
    remains is a restatement in a different setting.
  - **P-MU's kernel is Ryvkin–Drugov's.** RD-4: S11's hump at `(Q-1)/Q` *is*
    their log-supermodular kernel at `(k-1)/k` under `G<->F`, `Q<->k`. The
    "no universal sign" message is theirs, in print. S12 constrains what is
    left: across fifteen admissible power-family pairs the sign never flipped,
    so the both-signs claim rests on the induced `F,G` numerics, not on
    anything symbolic.
  - Rewrite so the paper leads with the hypothesis-weakening claim (which is
    verified at both ends) and presents P7 and P-MU as supporting apparatus,
    not as co-equal contributions.

- [ ] **N5. Skew-robustness check on the u''-alone claim.** Not yet tried;
  proposed 2 Sep 2026. The paper's central surviving claim is that the sign is
  determinate under `u'' < 0` alone, where the intensive margin needs `u'''`
  (ST Thm 3). That separation is verified against the admissible spread
  families, but not against a mean-preserving spread that also concentrates
  probability in one tail (more negatively skewed). If it survives a skewed
  perturbation family the way it survives `T_lambda`, the claim is materially
  stronger; if not, that is a boundary worth knowing before a referee finds it.
  - Method: same style as S12 — an admissible parametric family of
    MPS-preserving, skew-varying spreads (e.g. a two-parameter tilt of
    `T_lambda` adding a third moment at fixed first two), re-running the
    hypotheses across the family in SymPy.
  - Now cheaper than when proposed: R1 means the hypothesis to re-check is the
    tail condition, not the pivot property. Note F2 — whether a skewed MPS
    satisfies the tail condition is exactly the open characterisation, so N5
    and the `item:tailopen` successor are the same question approached from
    two directions. Consider merging them.
  - Done when either the family is checked and the sign survives (strengthen
    the contribution sentence, cite the checked family), or a counterexample
    is found and the boundary stated explicitly.

- [ ] **W1. Welfare section.** The only consolidated referee finding (R2
  Comment 3) with nothing written. Inputs are staged and mutually
  consistent; this is drafting and proving, not reading.
  - Frame: Levin–Smith Prop 3 (excessive entry by business-stealing), reached
    via the **LS-7 branch criterion** — a fixed prize gives `V_n ≡ V`, so
    their eq. (18) fails by exactly `V − W_n` and the model is in the
    common-value branch *by construction*. Cite LS p.596 directly, not by
    analogy.
  - Use the **corrected** reading of LS eq. (9): planner's FOC characterising
    `q^s`, obtained from eq. (8). It is *not* a reservation condition. Same
    algebra as FJL Definition 1, opposite economic role.
  - Pre-empt the heterogeneity objection with MW Prop 3: heterogeneous private
    costs leave free entry optimal *in the IPV branch*; the branch is set by
    `V_n`, not by the cost distribution.
  - CMP 1995 §4 as motivation only — it is Concluding Comments, prose, not a
    theorem.
  - Deliverable: an objective; `mu = 1` and prohibitive `c` shown as first-best
    corners; an argument for why they are unattainable; then interior
    comparison. Done when it carries an L or S tier leg like everything else,
    not prose alone.

- [x] **V1. DONE 2 Sep — both runners exit zero on Anurag's machine.**
  `./verify.sh` → `ALL SUITES COMPLETED`, exit 0, 9 suites: numerical, sympy
  (29 checks), P6/P7/P9-partial (6), P8/P9-full (10), P9-gen (23),
  equilibrium enumeration (14), anonymity (8), tail band (6), Lean.
  `lit/verify_lit.sh` → 12 papers, 70 checks, `ALL LITERATURE SUITES
  COMPLETED`, exit 0. Lean on the system toolchain
  (`~/.elan`, active `leanprover/lean4:v4.33.1`, the version `PROOFS.tex`
  cites): compiles clean, 52 theorems, 0 sorry, 52/52 axiom-audited, 36
  axiom-free, rest on `propext`/`Quot.sound` only.
  - **One caveat before this counts as fully third-environment.** `scipy`,
    `numpy` and `sympy` were not installed system-wide; they were supplied
    from a throwaway virtualenv placed on `PATH` for the run. Lean was the
    real system installation. Re-run after
    `pip install --user sympy scipy numpy` to close that gap.
  - The 4.14.0 half of the toolchain-latitude claim was **not** retested; only
    4.33.1 was exercised. `PROOFS.tex` still asserts both, on the earlier
    session's evidence.

## Next

- [x] **R1. General spreads. RESOLVED 2 Sep — and the recorded obstacle ran the other way.** The pivot hypothesis is stronger than the argument needs. Comparing the two *sorted* profiles rank by rank (always available — that comparison IS the quantile coupling), the conclusion needs only the sign of the displacement on ONE side of the margin: non-negative on ranks <= k* gives k* cannot fall; non-positive on ranks >= k* gives it cannot rise. Below the margin the displacement may cross zero any number of times. **CORRECTED 2 Sep (see F2): this does NOT mean general mean-preserving spreads are covered** — the tail condition is an extra hypothesis that MPS does not imply, and 37 of 4,000 genuine MPS instances fail it with `k*` strictly falling. `PROOFS.tex`'s open item `item:tailopen` states the residual question correctly; this entry previously did not. Machine-checked (`entry_preserved_of_up_to_margin`, `nonentry_preserved_of_down_from_margin`), with `pivot_imp_upToMargin`/`pivot_imp_downFromMargin` showing P9-gen and N1 are special cases. Explicit non-pivot witness (Q=8, displacement crosses sign 3 times, mean-preserving, k* 3->4) plus 3000 randomised spreads, 977 multi-crossing, 2276 genuine mean-preserving spreads in the second-order-dominance sense, 0 violations (counts corrected and re-audited under F1). Scope corrected under F2: R1-SCOPE exhibits 37/4000 genuine MPS that fail the tail condition with `k*` strictly falling.
  **Why the planned route was unnecessary:** Suen and Costrell-Loury need integration by parts and a log-concavity or monotone-weight condition because they sign a smooth AGGREGATE, which must control the quantile difference at every rank. `k*` is a threshold statistic reading the quantile function only down to the margin. The 'count vs aggregate' obstacle recorded in TRACEABILITY was real but inverted — the count is EASIER. The rescaling trap and cross-side/same-side obstacles never arise.
  **Narrowed successor (now open item 2 in PROOFS.tex):** characterise which mean-preserving spreads have a quantile difference signed down to the margin, in terms of Lambda and the position of k*. Smaller question; that is where the CL monotone-weight technique would belong if an aggregate version is ever wanted.

- [ ] **R2. P-MU shape.** The sign is characterised; the *shape* of
  `Δ(0,·)` is not. Log-supermodularity transfers from Ryvkin–Drugov; the
  single-crossing orientation does not (ours crosses `−+`, Karlin needs `+−`).
  - Two routes: rework the argument for the reversed orientation (plausible
    target: an interior **minimum** of `Δ(0,Q)` in `Q`, consistent with both
    refutations), or read RD Appendix A.2's `TP_r` machinery, still unread,
    which covers multimodal cases.

- [ ] **R3. Strictness away from the linear family.** P9-strict is stated for
  `T_λ` as `λ → ∞`. A characterisation of which spreads move `k*` strictly —
  rather than an asymptotic sufficient condition — is not attempted.

- [ ] **M1. Endogenise the incumbent's entry.** `C` is fixed throughout
  `H_m = F^m G^(Q−1−m) C`. This is R2.1's open question: does an equilibrium
  exist in which the incumbent abstains and a challenger invests? Lazear–Rosen
  §IV handicap algebra (`h* = Δμ/2`) is the tool. Modelling extension, not a
  gap in what is currently claimed.

- [x] **A1. RESOLVED 2 Sep — written up as a scope boundary, not a
  sufficiency theorem.** `checks/verify_anonymity.py` (8 checks, suite 7 of
  `verify.sh`); scope paragraph in `PROOFS.tex` §Primitives; index row ANON.
  - **The probe is now deterministic.** The earlier ad hoc probe computed
    `Delta` by Monte Carlo, which is why its rate was "indicative, not a
    result": the Nash conditions compare `kappa_i` against `Delta_i`, so a
    noisy `Delta` flips near-ties at random and manufactures multiplicity that
    is not there. `Delta` is now computed by quadrature and A1-1 bounds the
    discretisation error (`EPS = 1.7e-06`, by grid refinement), so every
    reported violation is checked against it. **This is what made the negative
    result claimable**; it was not a matter of raising the sample size.
  - **`prop:anon` is now verified rather than assumed.** At `theta = 0` the
    spread of `Delta` within a fixed count class is *exactly* 0 (A1-2); it is
    order `1e-1` once `theta != 0` (A1-3 control). The previous test used a
    synthetic gain (`gain_anon`/`gain_id`) and so could not speak to whether
    the model's own primitives deliver anonymity.
  - **Negative direction ESTABLISHED.** For `theta < 0` count invariance is
    false. Witness (A1-5), `Q=4, mu=1/2, c=1, gamma=2, theta=-1.5`:
    `w = (4.2197, 2.9982, 1.9339, 1.8760)`, `kappa = (0.0736, 0.1669, 0.5537,
    0.6085)` has exactly two equilibria — the *poorest* alone, and the two
    *richest* together, counts 1 and 2. Binding margin `9.4e-03`, i.e. 5440x
    `EPS`. Rates rise monotonically with the tilt: 0/0/0/0 for
    `theta = 0, +0.3, +0.6, +0.9, +1.5`; 0, 1, 5, 11 for
    `theta = -0.3, -0.6, -0.9, -1.5` (4000 draws each).
  - **Positive direction NOT established, and is not claimed.** 0 violations
    across 20,000 economies with `theta > 0` is tier N evidence *for* a
    universal claim, i.e. support only. Stated as such in `PROOFS.tex` and in
    the suite's own closing note.
  - **Mechanism, which is the interpretable part:** under a negative tilt the
    ablest challenger is also the one facing the highest `kappa`, so "how
    many enter" and "which enter" stop being separable — and their
    separability is exactly what `prop:count-inv` converts into a shared
    count. Since every comparative static is about that count, anonymity is
    load-bearing for all of §Proofs, not just for `prop:anon`.
  - **Nothing previously claimed is threatened**: the model assumes
    wealth-independent draws and so sits at `theta = 0` by construction. What
    changed is that the boundary is demonstrated instead of asserted.
  - Still open, inherited from the old item: `E3` draws `Delta`/`kappa` as
    independent sorted vectors, so nothing confirms the model's induced
    primitives fall inside the class E3 tests — though `verify_anonymity.py`
    now tests induced primitives directly, which covers most of that concern.
    Mixed-strategy equilibria remain unenumerated throughout.

- [ ] **A1-old (superseded, kept for the record).** Original framing below.
  `verify_equilibria.py`'s `nash_sets` indexes `Delta` by count
  alone (`prop:anon`, flagged implicit in `SOUNDNESS_20260902.md` Finding 3);
  nothing in the suite varies that assumption, so count invariance is proved
  *conditional on* the property that would hide identity effects if it failed.
  - **Reach widened by the 2 Sep work:** all three new propositions lean on
    `prop:anon` — N1's proof invokes it to hold `Delta` fixed under the
    spread, N4 (iii) invokes it explicitly for the prize term to cancel, and
    R1 inherits it. When queued this morning it conditioned P5-inv; it now
    conditions the endogenous-margin proposition, the tail condition and the
    assortative selection as well.
  - Ad hoc probe (not in `checks/`, script not in the bundle): enumerator
    rebuilt on model-induced primitives (real score draws `mu*r + (1-mu)*s`,
    incumbent present, per-set win probabilities by Monte Carlo, so `Delta`
    depends on entrant identity rather than count alone), with a tilt `theta`
    correlating ability with wealth (`theta=0` recovers anonymity). 300
    instances/setting, `Q=4`:
    | theta | count unique | count multiplicity | identity multiplicity |
    |---|---|---|---|
    | 0 (anonymity) | 300/300 | 0 | 80 (26.7%) |
    | +0.9 (rich abler) | 300/300 | 0 | 0 |
    | -0.5 (poor abler) | 294/300 | 6 | 67 |
    | -0.9 (poor abler) | 298/300 | 1 | 70 |
  - Reading: positive `theta` is harmless (reinforces the cost ordering);
    negative `theta` (ability running against wealth) produces equilibria of
    genuinely different sizes, i.e. count invariance fails. Effect small at
    this sample; Monte Carlo noise in `Delta` not characterised — the rate is
    indicative, not a result.
  - Threatens nothing currently claimed: the paper assumes draws independent
    of wealth, so it sits inside the `theta=0` column by construction. What it
    does is give Finding 3 a demonstrated boundary instead of a caveat
    sentence.
  - Also exposed: `E3` draws `Delta`/`kappa` as independent sorted random
    vectors, so nothing confirms the model's induced primitives fall inside
    the class E3 tests. Mixed-strategy equilibria remain unenumerated
    throughout.
  - Done when the probe is cleaned up, folded into `checks/`, and run at a
    sample size where the noise in `Delta` is characterised; and either a
    stated condition on the ability-wealth correlation is shown sufficient for
    count invariance (the way E5's condition is for identity uniqueness), or
    the negative result is written up as a scope boundary in `PROOFS.tex`
    §Primitives alongside Finding 3's fix. **Not claimable either way until
    then.**
  - **DISCHARGED 2 Sep by the A1 entry above**, via the second branch (scope
    boundary), not the first. The noise question was answered by removing the
    noise — deterministic quadrature plus an error bound — rather than by
    raising the sample size, which would not have sufficed: a Monte Carlo
    `Delta` cannot separate a genuine near-tie from a simulated one at any
    sample size that leaves the tie inside the confidence interval.

## Later / conditional

- [ ] **U1. Utility-function experiments.** Anurag's own direction, deferred
  until W1 and R1 settle. **Design constraint discovered in the literature
  work:** making `V` money added to the resource moves the model from
  Schroyen–Treich's *privilege* contest to their *rent-seeking* contest, where
  the two wealth effects oppose and, under CARA, exactly cancel. That is a
  substantive model change, not a robustness tweak — treat it as a new model
  with its own verification pass.

- [ ] **U2. SUPERSEDED 2 Sep by `STARTER_utility_weakening_20260902.md`.**
  U2 asked whether the two-branch rule survives a kink at a reference point.
  It does, trivially — the kinked burden is weakly decreasing, so
  single crossing, the prefix entrant set and count invariance all hold. That
  is not the interesting question, and the starter document replaces it with
  the one that is: what does the model actually need from `u`?
  - Short answer, verified by inspection: only that the burden decreases in
    wealth. Concavity is sufficient, not necessary, and
    `lean/EntryContest.lean` never assumed it — `hkap` is
    burden-monotonicity on `kap` directly, and `u` does not appear in the
    Lean at all. Same prose-vs-formal gap as F3, same direction.
  - The starter document carries the full programme: restate the primitive on
    burden-monotonicity (main text, cheap, do regardless); the
    participation-rate corollary; the kinked-collapse conjecture (the fall
    branch may disappear entirely under a reference-pivoted spread, which
    would make the two-branch rule a curvature result rather than a
    loss-aversion one); the scale condition governing how much non-concavity
    is tolerated; and a lognormal — not normal — parametrisation last.
  - **Item 1 of that programme is now done** (2 Sep, see Recently closed).
    The remaining items are unproved: the kinked collapse is hand algebra, the
    participation-rate corollary is unchecked, the scale condition is not
    started. Status table at §0 of that document, updated to match.

- [ ] **T1. Two-pivot generalisation.** HK 2004's ULR order permits *two*
  crossing points and three sign regions (poor / middle / rich). P9-gen is
  single-crossing. Natural extension of `Dispersive.lean`; untouched. Would
  cover spreads that compress the middle while expanding the tails, which a
  single pivot cannot represent.

- [ ] **X1. Mathlib.** Optional; not required for any current claim. Would let
  `step_nonpos` and the `κ` divergence be proved rather than assumed,
  collapsing the S tier into L and making the chain machine-checked from
  primitives. The core-only design is a deliberate trade — the
  explicit-hypothesis style is what makes the dependency on analytic inputs
  visible, and it is what lets `#print axioms` audit all 52 theorems. Do not
  change casually.
  - **Availability re-measured 2 Sep, and the previous assumption was wrong.**
    `verify.sh` now probes for Mathlib on every run instead of asserting its
    absence; the generated note in `VERIFICATION.md` records the finding. On
    this machine Mathlib is **present and fully built** —
    `~/development/lean4/testmathlib1`, 6977 compiled modules — and an import
    test compiles, resolving `intervalIntegral.integral_deriv_mul_eq_sub` and
    `MeasureTheory.integral_nonneg`, i.e. exactly the two facilities a Lean
    proof of `step_nonpos` would need. So the cost line above ("a much slower
    build") is no longer the obstacle; the obstacle is the work itself.
  - **Two real frictions remain, both now documented rather than guessed.**
    (i) That Mathlib is pinned to `leanprover/lean4:v4.24.0-rc1` while
    `EntryContest.lean` is verified on `4.33.1`, so the Mathlib-dependent part
    cannot simply be appended — it needs its own project, or a rebuild.
    (ii) `∫ φ² dK` is a Stieltjes integral, so the route is
    Lebesgue–Stieltjes rather than `intervalIntegral` alone. Estimate: days,
    not a session.
  - **Priority judgement (2 Sep):** still not next. It raises the assurance
    grade of a result already established with abstract links at tier S and
    declared honestly in §tiers, and it does nothing for the novelty question,
    which is the binding constraint. See the novelty ledger.

## Source hygiene

- [ ] **S1. Published-version checks.** Working papers or drafts were read for:
  Schroyen–Treich (*GEB* — Theorem 3 and the three payoff specifications),
  Fu–Lu (*EI* — the title differs between MPRA and FJL's citation of it),
  Cole–Mailath–Postlewaite 1995 (*FRB Minneapolis QR* — "Concern" singular on
  the WP vs "Concerns" plural in HK's reference list), Costrell–Loury,
  Becker–Murphy–Werning, Moldovanu–Sela, Cornes–Hartley, Treich,
  Drugov–Ryvkin. Proposition numbering may differ from what is cited.

- [ ] **S2. Re-verify Levin–Smith quotations.** Transcribed by eye from a
  scan. Must be re-checked against the published text before anything is
  quoted in print.

- [ ] **S3. Remaining abstract-level sources.** `GrossmannDietl2015` (entry
  condition vs P5) and `ChungLee2017` (welfare template, feeds W1) are still
  [A]. Upgrade if either is used for a load-bearing claim.

## Novelty ledger (assessment, revised 2 Sep 2026 — revise only against evidence)

Where each candidate claim stands. Anything not listed as SURVIVES is not to
be claimed as novel.

| Claim | Verdict | Ground |
|---|---|---|
| MPS sign determined with `u''` alone, where the intensive-margin result needs `u'''` | **SURVIVES** | Both ends verified: ST Thm 3's `u'''`-dependence exhibited by exact separator (CARA `a=1` vs log at `w=1`: same `A=1`, `P=1` vs `2`, opposite signs at `m=1/2`); P9-gen's independence machine-checked; quadratic utility separates the channels (`kappa'=-c/4<0` while `A'>0`, IARA) |
| One-sided tail condition, fall branch free at the margin | **SURVIVES (minor)** | `nonentry_preserved_of_beyond_margin` and `nonentry_preserved_beyond_marginal_index`, axiom-free; `downFromMargin_imp_beyondMargin` shows the earlier hypothesis was strictly stronger. R1-WEAK: 4000 cases with the margin free, 0 violations; control 362/4000. A sharpening of the statement, not a new result |
| Dispersive order + crossing ⇔ pivot-spread class, crossing necessary | **SURVIVES (minor)** | `Dispersive.lean`, 7 theorems, axiom-audited. Converts the pivot class from ad hoc to HK's own stochastic order. A lemma about stochastic orders, not about contests |
| Assortative set is the cheapest equilibrium | **SURVIVES as a fact; NOT as a welfare claim** | Existence and cost-minimisation correct, machine-checked and re-run. The welfare reading was corrected under F6 and is now stated as what it is: an unweighted sum of the model's utility units, carrying no resource-efficiency content (all equilibria cost `k*c`) and favouring the wealthiest entrants, verified 1344/1344 in N4-WELFARE. It ranks equilibria at a fixed count and is independent of whether `k*` is too large. **Do not present it as a welfare result.** Selection-grade, not novelty-grade |
| Count invariance across all pure equilibria (P5-inv) | **SURVIVES (conditional on anonymity — see A1)** | `equilibrium_count_unique` machine-checked; `verify_equilibria.py` E2/E3 (20,000 instances) plus an independent re-test (8,000, different seed and code); E6 control shows antitonicity of `Delta` is load-bearing, so it is not a tautology. Stronger than the original P5 claim, which only constructed the assortative equilibrium |
| Identity pinning by the wealth ordering | **WITHDRAWN — was false** | Counterexample in `SOUNDNESS_20260902.md` Finding 1; multiplicity in 17–22% of instances. Holds only under `kappa_(k*+1) > Delta(k*-1)` (`members_below_kstar`) |
| One-sided rank condition on two sorted profiles, with non-dispersive spreads inside its scope | **SURVIVES, narrower than first written** | Proposition (tail condition), machine-checked and independently recompiled; pivot-spread shown to be a special case; explicit multi-crossing witness plus 3000 randomised spreads (977 multi-crossing, 2276 genuine MPS), 0 violations. **Does not remove the scope caveat** — MPS does not imply the tail condition (37/4,000 genuine MPS fail it with `k*` strictly falling, F2), and mean-preservation is not used in the proof at all (F3). Weakens the caveat and relocates it to an endogenous object |
| Endogenous marginal agent in the pivot rule | **SURVIVES** | Proposition (endogenous margin), machine-checked and independently recompiled (axiom-free); 20,000 economies with `k*` recomputed on both sides, 0 violations; control at the wrong index gives 154/2744, so the hypothesis is not slack. Differs from CL's fixed `theta`; framing settled under F7 — offered as a consequence of the extensive-margin formulation, not as a stronger theorem than theirs |
| P7 uniform cap | **DOES NOT SURVIVE** | `Accounting.lean`: FJL's bound and P7's cap are instances of one lemma |
| P-MU | **DOES NOT SURVIVE as such** | Kernel is RD's (RD-4); "no universal sign" is theirs; S12 limits the both-signs claim to induced `F,G` numerics |
| The mechanism (wealth sorts entry via concavity) | **DOES NOT SURVIVE** | Lazear–Rosen (1981) §III; Schroyen–Treich (2016) privilege contest |
| "The combination of P7, P9-gen, P-MU" | **WEAK — see N3** | Combination claims are discounted; two of three components are not individually novel |

## Standing rules (not tasks — do not remove)

- Do **not** add a second Lean compile pass to `lit/verify_lit.sh`. Each paper
  suite compiles and axiom-audits its own Lean file and fails if `lean` is
  absent. `verify_lit.sh` only runs an orphan guard on top. A duplicate pass
  was added and removed once already.
- `VERIFICATION.md` is generated by `verify.sh`. Never hand-edit.
- Any axiom-audit `sed` must match `[A-Za-z0-9_']*`. The original
  `[a-zA-Z_]*` silently truncated digit-containing theorem names, leaving
  three theorems unaudited while the run still reported full coverage.
- Every count cited in `PROOFS.tex` must be the number `VERIFICATION.md`
  actually prints, not the number the loop was written to reach. State it in
  DIGITS: the F1 recurrence on 2 Sep survived a numeric audit purely by being
  spelled out in words. `checks/verify_docs.py` D1 now enforces both.
- Never assert an environment fact that a suite could measure. If it matters
  enough to state, measure it on every run and let the generated file carry
  the finding; `verify_docs.py` D2 fails the build otherwise. A sampling
  loop with filters and a `break` target will silently deliver fewer; read the
  printed line, not the source. F1 is this error and it reached the PDF.
- A check whose two sides are the same expression rearranged proves nothing.
  Before adding a check, name the input that would make it fail. F5 is this
  error.
- Before asserting a structural claim, name the layer that would fail if it
  were false. If no test or theorem quantifies over the objects the claim is
  about, the claim is prose, not a result. The identity-pinning error passed
  every green suite because nothing enumerated arbitrary entrant sets.
- One self-contained bundle per delivery. No overlays, no patch tarballs, no
  duplicate copies of a file.
- Code carries no comments; explanation belongs in `PROOFS.tex` or in the
  handover.

## Now

- [ ] **A2. Compile `lean/mathlib/StepNonpos.lean` and report the result.**
  DRAFTED 2 Sep, **NOT COMPILED** — written where the Mathlib cache returns
  HTTP 403 (`cache.mathlib.org` outside that sandbox's egress allowlist) and
  building 8849 modules from source exceeded available disk. Every statement
  in it is a conjecture about Mathlib's API until it compiles.
  - Purpose: close the one link that keeps every count claim from being
    machine-checked from primitives to conclusion. `step_nonpos` — the
    antitonicity of `Delta`, the engine of P5 and hence of every comparative
    static — is assumed in `EntryContest.lean` and discharged only at tier S
    over stated families (S4 on five concrete pairs, S5 the boundary term).
  - Three theorems: `step_integral_eq` (integration by parts with the boundary
    term killed by `phi a = phi b = 0`), `step_integral_nonpos` (the sign, for
    arbitrary non-decreasing `K` and arbitrary `phi` vanishing at both ends —
    no family assumed), `step_nonpos_of_representation` (bridges to the form
    `EntryContest.lean` assumes).
  - Deliberately OUTSIDE `EntryContest.lean` and outside `verify.sh`: a
    Mathlib import brings `Classical.choice` into the axiom profile, and the
    core development stays core-only so `#print axioms` certifies every
    theorem at the granularity `PROOFS.tex` claims. Separate obligation,
    separate declared profile.
  - `lean/mathlib/README.md` lists the four places it is most likely to break
    (the IBP lemma's name/namespace, the `HasDerivAt` block for `phi^2/2`,
    `integral_nonneg`'s interval, `mul_nonpos_of_nonneg_of_nonpos`'s spelling).
  - **Done when** it compiles and the three axiom profiles are recorded. Only
    then may the abstract's sentence change. What it still would NOT close:
    the algebraic identity S1 and the P1 representation S7, and the fact that
    the smoothness hypotheses hold for the model's induced `F, G, C` is not
    itself formalised.

## Recently closed

- [x] Reconciliation review of the 2 Sep complete bundle (2 Sep, later).
  All new content re-run independently: Lean 4.33.1 compiled from scratch
  (52 theorems, 0 sorry, 52/52 audited, 36 axiom-free), all suites green with
  system-installed sympy/scipy/numpy rather than a venv, so the V1 caveat in
  the manifest is now closed on this environment. The A1-5 anonymity witness
  was independently recomputed at ngrid 2001 and 8001: same two equilibria,
  sizes 1 and 2, binding margin 9.43e-03 against inter-grid drift 1.28e-05,
  ~700x, so the negative result is not a discretisation artefact. Four
  defects found and fixed:
  - **D-a (hard, and the one flagged in conversation).** `PROOFS.tex` still
    asserted "Mathlib is unreachable in this environment (its build cache is
    blocked and building from source is infeasible here)" at the top of the
    verification overview, while the tiers section 46 lines later stated that
    exact claim had been false. The correction had been made in one place and
    not the other. Both now say the reason is auditability, not availability.
  - **D-b.** `lean/EntryContest.lean`'s header scope note carried the same
    false claim; corrected.
  - **D-c (F1 recurrence).** `PROOFS.tex` cited "thirty-one theorems, twenty
    ... eleven" against an actual 52 / 36 / 16. **The counts were spelled out
    as words, which is why the F1-era numeric audit missed them.** Now in
    digits and correct.
  - **D-d.** The Lean 4.14.0 half of the toolchain-latitude claim was stated
    flatly though only 4.33.1 was exercised; now attributed to the earlier
    session and marked not retested.
  - **New suite 10, `checks/verify_docs.py`**, so this class stops recurring:
    D1 cross-checks the three Lean counts in `PROOFS.tex` against the
    generated `VERIFICATION.md` and requires them in digits; D2 fails if any
    authored file asserts an environment fact that `verify.sh` measures; D3
    fails on orphaned check scripts. Negative-controlled — reintroducing D-b
    and D-c makes it report 4 failures.
  - Standing rule added: a count stated in words is not machine-checkable.
    Numbers that a check could compare must appear as digits.
  - D2 refined after it fired on this very TODO entry, which quotes the old
    claim two lines above its correction: the scan now allows a +/-2 line
    window, so a file may quote a retracted assertion while correcting it
    nearby. Negative control re-run and still reports 4 failures.



- [x] **Open item 3 CLOSED 2 Sep — with a negative result, not a stronger
  theorem.** `checks/verify_tailband.py` (6 checks, suite 8); new §"The band"
  in `PROOFS.tex` (§P9-gen material); Lean section `TailBand`; index row BAND.
  - **The characterisation it asked for is near-definitional.** With
    `L = min{j : d_j < 0}` (richest rank that falls) and `M = max{j : d_j > 0}`
    (poorest rank that rises), the two hypotheses are exactly `k* <= L` and
    `M < k*` — verified equivalent to the stated form over 20,000 instances,
    0 mismatches (T1). So "which MPS satisfy the tail condition" has an exact
    answer that is a restatement, not a theorem. That is worth knowing but is
    not the interesting part.
  - **What dispersiveness buys, as an iff:** every margin is covered iff
    `M <= L` iff the displacement crosses zero at most once. Machine-checked,
    **axiom-free**: `band_empty_iff`, `band_nonempty_of_lt`. A pivot-spread is
    not a convenient special case — it is *the* class on which the rule is
    margin-free, and the band is the exact price of leaving it.
  - **The substantive finding: the hypothesis is SHARP.** Over 40,000 genuine
    MPS, 0 violations in 20,889 covered instances — including 1982/1982 where
    both branches apply and the rule forces `k*` fixed, which is its sharpest
    prediction and not a tautology. Inside the band `k*` rises in 4,785 and
    falls in 130, with two displayed witnesses of the same sign class and
    opposite conclusions. **One such pair refutes any purported strengthening,
    so this is established, not sampled.** The tail condition is the whole of
    what the displacement signs support.
  - **A number NOT to quote.** The band covers 47.2% of admissible spreads
    under the unstructured generator — but that describes the generator, where
    multi-crossing is typical by construction. Under a linear MPS and under a
    mean-preserving lognormal dispersion increase it is **0.0%** of 8,000 each,
    both families being dispersive. Reporting "silent on half of MPS" without
    that qualifier would overclaim in the pessimistic direction, which is as
    much a failure as overclaiming the other way. `PROOFS.tex` states the
    conditional form only.
  - **Still open, and now much narrower:** whether some statistic *other* than
    the displacement signs — magnitudes, or a Costrell–Loury monotone-weight
    functional — predicts the direction inside the band. The result says the
    signs do not; it says nothing about anything else.
  - Method note worth carrying: the first draft of the suite **hung**, because
    the lognormal generator had a 0% acceptance rate (preserving the
    lognormal's own mean does not preserve the mean of the discrete profile
    read at fixed rank fractions, which is what `_is_mps` tests). Fixed by
    additive re-centring — safe here because `d_j` is monotone in rank, so a
    constant shift moves the single crossing without creating a second one —
    and every sampling loop now carries an attempt guard that fails loudly
    rather than spinning.

- [x] **§Contribution scope corrected — the headline claim was an ordering
  problem, not a dishonesty** (2 Sep). "The claim" paragraph stated the sign
  rule for "a mean-preserving spread" without qualification, while the
  qualification — that MPS is covered *only* when its quantile difference is
  signed down to the margin — sat ~120 lines later in the same section. A
  referee reading the claim paragraph would have carried away the
  unqualified version. **The later passage was already correct and is
  unchanged**; the headline now names the class (dispersive spreads, with the
  linear MPS as leading case), states that the proof actually uses the weaker
  tail condition with no mean-preservation among the hypotheses, and carries
  the 37/4000 R1-SCOPE figure with a forward pointer to the scope discussion
  and to open item 3. No result changed; what changed is that the claim is now
  accurate read on its own.
  - Also fixed, latent and pre-existing: `\label{prop:P9}` sits on the
    *subsection*, so `Theorem~\ref{prop:P9}` rendered as "Theorem 6.10" — a
    section number wearing a theorem's name — and line 1650 called P9 a
    *Proposition* on top of that. Added `\label{thm:P9}` inside the theorem
    environment; theorem-sense references now resolve to Theorem 4, and the
    `\S\ref{prop:P9}` section-sense references are left alone. The original
    headline had the same bug ("Theorems 6.10 and 5"); it surfaced only
    because the rewrite put the reference where the rendered text was read.

- [x] **Primitive restated on burden-monotonicity** (2 Sep, item 1 of
  `STARTER_utility_weakening_20260902.md`). `PROOFS.tex` §Primitives now
  assumes burden-monotonicity — `kappa` strictly decreasing — with strict
  concavity demoted to a sufficient condition; §Terminology gains a
  `burden-monotonicity` row and the `entry cost (utility)` row no longer
  derives monotonicity from concavity; §Contribution no longer understates the
  hypothesis. No proof changed: they all already ran on `kappa`. The text now
  says explicitly that `lean/EntryContest.lean` was always at this generality
  (`hkap`, and `u` does not occur in the file), so this closed a prose-vs-formal
  gap in the same direction as F3.
  - **The separating example is now stronger than the starter claimed.** It was
    recorded there as numerically witnessed; it is exact. The oscillation
    identity `sin(kw) - sin(k(w-c)) = 2 sin(kc/2) cos(k(w-c/2))` is verified
    *abstractly* in `k`, `c`, `w` (S22a), the amplitude is exactly zero at the
    admissible periods (S22b), so `kappa(w) = sqrt(w) - sqrt(w-1)` identically
    rather than approximately. Tier S, not N. Now in `checks/verify_sympy.py`
    as S21–S23, with S23 as the non-vacuity control (`p=0.7` breaks the
    property at `w=1.3`).
  - Carries index row **BM**, the first row in the table about the primitives
    rather than about a result derived within the model. §What the three
    evidence tiers mean gains a paragraph justifying that, and distinguishing
    the *assumption* (which stays in §Primitives, unlisted, like `V>0` and
    `kappa -> infinity`) from the *claim that the assumption is strictly
    weaker than concavity* (which is what the row records).
  - **Still open, and the starter is right that it matters:** the construction
    is knife-edge — the amplitude vanishes only on a measure-zero set of
    periods. The containment is proper; whether the extra room is economically
    substantial is the scale condition, untouched.

- [x] Starter written for the utility-weakening / reference-point stream
  (2 Sep, `STARTER_utility_weakening_20260902.md`). Supersedes U2. Records
  the one durable finding from the session's exploration — concavity is used
  nowhere except to obtain burden-monotonicity, and the Lean never assumed it
  — plus five sequenced items, what is out of scope and why, and a per-claim
  status table making clear that none of it is proved.

- [x] Terminology table added to `PROOFS.tex` (2 Sep, §Terminology, between
  Primitives and Proofs). Binds every English term used in a claim or proof to
  a symbol built from the primitives, grouped as players/actions, wealth and
  cost, scores and gains, equilibrium objects, comparative statics,
  aggregates. No new objects are introduced — each entry names something
  already defined, per the definitions-table discipline. Closes with the three
  pairs earlier drafts conflated: utility vs resource entry cost, count
  invariance vs identity multiplicity, spread vs tail condition.

- [x] F5 and F7 cleared (2 Sep). The vacuous surplus check replaced with an
  anonymity test plus identity-dependent control (9022 pairs; 3022 break);
  the Costrell-Loury positioning restated as a comparison of formulations,
  not of theorem strength.

- [x] F3 cleared (2 Sep). `prop:tailprop` now stands on two arbitrary sorted
  profiles; mean-preservation removed from the framing and shown by R1-FREE
  not to be needed. The result is a monotonicity statement about a threshold
  statistic, applied to spreads, not a statement about spreads.

- [x] F4 cleared (2 Sep). Both fall-branch proofs no longer claim the entry
  condition fails at the margin, and both propositions now stand on the
  strict-outsider hypothesis, which is what they need. Three axiom-free Lean
  theorems and the R1-WEAK check with control.

- [x] F1, F2, F6 cleared (2 Sep). Draw budget raised so the R1 sampler
  actually delivers the 3000 it claimed, generator untouched; R1-SCOPE added,
  exhibiting a genuine MPS that fails the tail condition with `k*` falling;
  N4-WELFARE added, separating the cost ranking from resource efficiency and
  pinning its direction. `PROOFS.tex` corrected in five places. Suites:
  22 / 6 / 10 / 19 / 13 checks, 0 failures; Lean 47/47, 0 `sorry`, 31
  axiom-free.

- [x] N1, N4, R1 resolved (2 Sep) and independently reviewed the same day:
  Lean 4.33.1 fetched and recompiled from scratch — 47 theorems, 0 `sorry`,
  the ten new theorems axiom-audited (seven axiom-free, three on core axioms
  only); `verify_p9gen.py` 16/16 and `verify_equilibria.py` 10/10 re-run
  green. The mathematics of all three holds. Seven write-up and framing
  defects found and queued as F1-F7; two of them (F1, F2) are claims already
  in the PDF that the bundle's own generated evidence contradicts.

- [x] Soundness pass (`SOUNDNESS_20260902.md`): identity claim withdrawn,
  count-invariance proved (P5-inv), payoff function displayed, draws stated
  independent of wealth. Verified independently here.

- [x] Literature position settled; contribution stated in `PROOFS.tex`
  §Contribution. Was the gating open item.
- [x] All claims off tier N. Only the two refutations remain N, which is
  correct — one counterexample settles a refutation.
- [x] Complete-information assumption stated in §Primitives.
- [x] `lit/` verification pass: 12 paper directories, 2 Lean artifacts.
- [x] TRACEABILITY action lists A and B applied to `LITERATURE.tex` and
  `PROOFS.tex`.
- [x] `READING_NOTES_20260901.md` corrected — six synthesis errors that had
  propagated into the survey, each marked in place.
- [x] `verify.sh` hardened: `pipefail`, fail-on-missing-Lean, audit regex.
- [x] `verify_lit.sh` orphan guard for unreferenced `.lean` files.
- [x] P-MU ↔ Ryvkin–Drugov correspondence computed; kernel identity exact,
  log-supermodularity transfers, orientation reversed. No unimodality claim
  available.
