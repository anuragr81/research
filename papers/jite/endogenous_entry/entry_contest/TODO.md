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
0 sorry, 52/52 axiom-audited, 36 axiom-free; `verify.sh` runs 11 suites
since 6 Oct (suite 9 is the measurement map, suite 11 doc consistency), and
fails on sorry or on any axiom outside propext and Quot.sound.

Updated 6 October 2026 after the first literature Lean pass (item L1). The
goal order is fixed by the author. First establish novelty, then verify the
paper's claims, and only then write the manuscript.

---

## Now

- [!] **L1. Literature Lean pass, 6 Oct 2026. Decisions for Anurag.** Ten
  paper directories gained a core-only Lean file and a measuring audit
  (`lit/COVERAGE.md` has the counts). Each source was read in full, and each
  directory's `NOTES.md` carries the quotes, pages and findings. The findings
  below are against our own documents, which were left unedited. "Checked"
  means I read the passage in the source myself; "agent" means it rests on the
  agent's reading and its Lean or SymPy check, without my re-reading.
  Novelty-bearing, in order of consequence.
  0. **P-MU's single-crossing orientation is a dropped minus sign** (our own
     mathematics, not a reading of a source). `PROOFS.tex` l.1545 gives
     D(Q) = -int G^(Q-1)(1-G) F (f-g). Since f-g = -phi', that is
     D(Q) = +int G^(Q-1)(1-G) (F phi'), and with phi hump-shaped F phi'
     crosses +- , the orientation Ryvkin-Drugov's Karlin step uses (their
     p.1615). The "reversed orientation" sentence (l.1568-1571, l.1825,
     `LITERATURE.tex` l.1096-1101) is therefore wrong, and the "interior
     minimum" target of open item 5 and of R2 points the wrong way. Checked
     independently with F = x^4, G = 1-(1-x)^5 (F <= G, one interior peak of
     phi). D(Q) is +1/9009, +106/2909907, then negative for Q = 3 to 6, so
     Delta(0,.) rises then falls. Not proven, and needed before any
     unimodality claim, is that phi is single-peaked for the model's induced
     F and G. Lean `pmu_orientation` and SymPy RD-8 in
     `lit/ryvkin_drugov_2020/`.
     Settled by pass 3e, 7 Oct 2026, in `lean/mathlib/PMU.lean`. With the
     orientation corrected, the single-crossing step goes through and yields
     unimodality, as item L3 records.
  1. **The reconciliation paragraph** (`PROOFS.tex` §Contribution, and the
     P9 "Why this matters" paragraph). CMP 1992 §IV.A compares men "with the
     same initial income level in two different economies", finds savings
     falling for "men in the top half of the distribution", and states the
     result as "will tend to" with "all other things being equal" (p.1103).
     The effect runs through rivals' wealth via matching, which
     Proposition (anonymity) excludes from our model. What is shared is the
     direction, not the mechanism, so "recovered as the below-pivot branch"
     overclaims. Checked.
  2. **Uncited entry papers found in MOS 2012 p.442.** Fullerton and McAfee
     (1999), "Auctioning entry into tournaments", JPE 107, 573-605, with
     heterogeneous agents and entry by auction. Mathews and Namoro (2008),
     "Participation incentives in rank-order tournaments with endogenous
     entry". Corcoran (1984) and Corcoran and Karels (1985), the long-run
     rent-seeking entry model MOS call theirs a version of. None is in
     `refs.bib`. Any of them may bear on what P5 and the sign rule can claim.
     PDFs needed. Citation checked.
  3. **Costrell-Loury Proposition 10 reverses Proposition 6 only for concave
     beta** (draft p.28, "under concave b(.)", "for concave beta(.)"). Under
     convex beta both narrow the span. `PROOFS.tex` l.1250-1252 and
     l.1678-1680 state the reversal without the condition. Checked.
  4. **What Costrell-Loury sign.** A non-decreasing weight signs output
     (Proposition 5); the wage span (Proposition 6) needs the curvature of
     beta. `PROOFS.tex` l.1316-1318 and l.1433-1439 describe them as signing
     a wage schedule under a general spread. Agent.
  5. **P7's distinctions from Fu-Lu.** In Fu-Lu's own model N C <= Gamma_0
     holds at every feasible contest, and their count is pure-strategy and
     deterministic (pp.6-7, fn.6). Two of the three things `PROOFS.tex` §P7
     says distinguish P7 therefore separate it from FJL only. The ledger
     already says P7 does not survive; the prose overclaims. Agent.
  6. **The Schroyen-Treich separator holds for m in (0, 2^(-1/2)) and fails
     above it** (at m = 4/5 CARA and log agree). The u'' against u''' contrast
     should carry the range. Checked by hand from the Theorem 3 condition.
  7. **Moreno-Wooders.** The common threshold comes from the symmetric
     equilibrium, not from private information (p.320); `PROOFS.tex`
     l.557-560 says "because under private information". Their Proposition 3
     is a constrained optimum (p.320, "W* is a constrained maximum"), which
     matters for the welfare pre-emption in W1. Both checked. "No comparative
     statics in H anywhere" in `LITERATURE.tex` is false as worded, though the
     paper never signs a spread of H at fixed N (agent).
  8. **Levin-Smith, for W1.** "Proposition 6 holds exactly when (18) holds" is
     stronger than the paper, which shows sufficiency. A fixed prize where an
     entrant meeting a rival keeps no rent also makes free entry optimal, so
     W1 needs V - W_n > 0 for some n >= 2 as a hypothesis. Business stealing
     is their intuition, not their proof (p.590). `PROOFS.tex` l.572-575
     should say pure-strategy count, since fn.6 has a hybrid equilibrium with
     a random count. Agent.
  Wording and locator fixes, listed in each directory's `NOTES.md`. Suen
  (`PROOFS.tex` l.1795-1796 names log-concavity and omits concavity; the
  source has a sign misprint in eq. (6), p.154). CMP (qualifiers and
  conditions dropped in `LITERATURE.tex`, two wrong locators). Costrell-Loury
  ("crossing point" where the paper says "tail", Lemma 1's location). MOS
  (conditions P > F > P/N^2 and sqrt(P/F) non-integer dropped; observability
  of entry omitted at `PROOFS.tex` l.575). Lazear-Rosen ("makes the rich
  prefer" where the paper says "more likely"; "asymmetric information"
  attached to a section with known types; an agent recomputation of Table 1
  that I have not reproduced). Levin-Smith (eq. (9) carries a star; one eye
  transcription corrected). Moreno-Wooders ("when interior" dropped;
  "proportional" where we say "equals"; stale paragraphs in `LITERATURE.tex`
  l.531-534, l.1044-1047, l.1124-1126). `lit/TRACEABILITY.md` rows 5 and 9
  are stale.
  Coverage gaps carried to the next pass. The Hopkins-Kornienko Lean covers
  HK 2010 Definition 1 only, so HK 2004 and 2009 claims have no Lean.
  `Shaked1982` is cited twice in `PROOFS.tex` with no source read. The 41
  works cited only in `LITERATURE.tex` have no directory, and most have no
  PDF on Drive. Moreno-Wooders checks MW-3 and MW-5 cannot fail (flagged by
  the agent, left unchanged).
  - Done when each numbered item has the author's decision recorded and,
    where accepted, the edit applied and the suites re-run.
  - Since PROOFS.tex is to be retired (pass plan below), accepted fixes to it
    are applied in the manuscript skeleton instead.

- [!] **L2. Lean pass 3d, 7 Oct 2026. Decisions for Anurag.** Each item
  below rests on a theorem in `lean/mathlib/KappaSpread.lean`, except where
  it says otherwise.
  0. **The separating example of Claim BM violates the divergence
     primitive.** `PROOFS.tex` uses `u(x) = sqrt(x) + eps sin(kx)`, which is
     continuous at 0, so its `kappa` stays bounded as `w` falls to `c`
     (`bm_example_not_diverges`). The example lies outside the admissible
     class, and the claim that the admissible class is strictly larger than
     the concave class is therefore not yet established. A replacement for
     pass 3f is `log x + eps sin(2 pi x / p)` with `p` dividing `c`, whose
     `kappa` equals that of `log` and so diverges. A second problem, checked by
     hand and not yet in Lean, is that the stated example is not increasing.
     With `eps = 3/50` and period `p = 1`, so `k = 2 pi`, at `x = 5/2`
     `u'(x) = 1/(2 sqrt(5/2)) - 6 pi/50`, and `1/(2 sqrt(5/2)) < 1/3 < 18/50 <
     6 pi/50`, so `u' < 0` there. Pass 3f settles both in Lean.
     Settled by pass 3f, 7 Oct 2026 (`lean/mathlib/BurdenWeaker.lean`). The
     example is not increasing for any `eps > 0` and `k > 0`
     (`bm_example_not_monotone`), so it fails two admissibility conditions.
     The periodic replacement proposed above fails too, because a fixed
     amplitude eventually dominates the falling marginal utility of `log`
     (checked by hand with the argument of `bm_example_not_monotone`, not in
     Lean, since the candidate was dropped). The
     corrected example is `u(x) = log x + x + eta int_0^x ramp`, where the
     ramp rises from 0 to 1 over `[x0, x0 + sigma]`. That `u` is strictly
     increasing, strictly convex on `[x0, x0 + sigma]`, and admissible, since
     its burden is strictly decreasing and diverges at `c`
     (`rampU_admissible`). Claim BM therefore holds as stated
     (`bm_strictly_weaker`). The example is not knife-edge, because its
     admissible parameters form an open set (`admissibleParams_open`). The
     open question in `PROOFS.tex` on how much room burden-monotonicity buys is
     answered for convex stretches. Every width below `c` is admissible
     (`admissible_width`), and a convex stretch wider than `c` rules
     burden-monotonicity out (`not_burden_of_convex_stretch`). The example is
     continuously differentiable but not twice differentiable at the ends of
     the ramp. Decision recorded 7 Oct 2026. The author asked for pass 3f to
     settle the claim before pass 3e.
  1. **P9 strictness needs a convention at the support floor.** A large `lam`
     pushes the poorest challengers below `c` before the marginal entrant
     exits, which leaves the primitives' wealth support. The Lean statement
     (`p9_strict`) adopts the convention that a challenger whose wealth is at
     or below `c` does not enter. The manuscript should either state that
     convention or restrict `lam` to spreads that keep every challenger
     above `c`, and the second option may lose the strict drop.
     Decision recorded 7 Oct 2026. The author accepts the convention. The
     manuscript states it as `kappa(w) = +infinity` for `w <= c`, which
     continues `kappa` under the divergence primitive, matches a challenger's
     inability to pay a fee above the challenger's wealth, and leaves every
     challenger above `c` unaffected.
  2. **P9 strictness uses less than `PROOFS.tex` says.** The proof needs
     weak burden-monotonicity, the divergence of `kappa` at `c` and a
     non-increasing `Delta`. The continuity of `kappa` and the strictness of
     burden-monotonicity are not used.
     Decision recorded 7 Oct 2026. The manuscript keeps its stated
     assumptions, and proofs under weaker hypotheses go to a proofs-addendum
     document kept for future reference.
  3. **The divergence primitive is `u(0+) = -infinity`.** For `u` continuous
     at `c`, `kappa` diverges as `w` falls to `c` exactly when `u` falls
     without bound at 0 (`kappa_diverges_iff`). Log and CRRA with `gamma > 1`
     satisfy the primitive. CRRA with `gamma < 1`, the square root among them,
     does not (`crra_kappa_not_diverges`). S16 tested log and three exponents
     above 1, so the exclusion was never visible. The primitives section
     should say which families the assumption admits.
     Decision recorded 7 Oct 2026. Restricting or excluding families is
     acceptable when the manuscript gives an economic or intuitive reason.
     The reason to state here is that the primitive makes being left with no
     wealth after the fee unboundedly costly. Under a utility with finite
     `u(0+)`, a challenger at the floor would face a bounded cost and could
     enter with everything staked.
  4. **S17 tested the sign of `kappa'` at one point per family.** Lean now
     proves `kappa` strictly decreasing on `(c, infinity)` for every `u`
     strictly concave on the positive reals
     (`burden_strictAnti_of_strictConcave`), which covers log and CRRA for
     every `gamma > 0`. No derivative of `u` is used.
  - Done when each item has the author's decision recorded and, where
    accepted, the edit applied in the manuscript skeleton.

- [!] **L3. Lean pass 3e, 7 Oct 2026. Decisions for Anurag.** Each item rests
  on a theorem in `lean/mathlib/PMU.lean`, except where it says otherwise.
  Write `D(Q) = Delta(0, Q+1) - Delta(0, Q)` and
  `W = C G^(Q-1) (1 - G)`.
  0. **The first entrant's gain is quasi-concave in `Q`.** Suppose
     `dF - dG` is nonpositive up to some `x0` and nonnegative beyond it, the
     measure form of `phi = G - F` being single-peaked. Then
     `D(Q2) <= G(x0)^(Q2-Q1) D(Q1)` for `1 <= Q1 <= Q2`
     (`pmu_single_crossing`). Once `D` is nonpositive it stays nonpositive,
     so `Delta(0, .)` rises and then falls on `Q >= 1`
     (`pmu_quasiconcave`). The argument needs no densities, and the factor
     `G(x0)^(Q2-Q1)` is the likelihood ratio of the two kernels, so the
     variation-diminishing step of Ryvkin and Drugov reduces here to one
     comparison at `x0`. `PROOFS.tex` claims no unimodality (l.1570) and
     conjectures an interior minimum (open item 5, l.1827). Under the
     hypothesis an interior minimum is impossible.
  1. **The hypothesis holds for a uniform base score.** With `r` uniform on
     `[0, 1]`, any law of `s >= 0` and `0 < mu <= 1`, the crossing point is
     `x0 = mu` (`uniform_left`, `uniform_right`), so `Delta(0, .)` is
     quasi-concave in `Q` for every incumbent law (`pmu_quasiconcave_uniform`).
     Whether the hypothesis holds for other laws of `r` is open. A density of
     `r` that does not fall on `[0, 1]` is a plausible sufficient condition,
     not proved. The author should decide whether the paper claims the
     unimodality and for which laws of `r`. Its novelty against Ryvkin and
     Drugov is a question for pass 7.
     Decision recorded 7 Oct 2026. The author adopts the rise-then-fall result
     as a claim, stated under the single-peaked hypothesis with the uniform
     base score as the case proved from the primitives. The claim is a
     candidate row of the novelty ledger until pass 7.
  2. **The identity and the hump are proved as stated, with the factor `V`.**
     `D(Q) = -V (int W dF - int W dG)` for `Q >= 1` and every incumbent law
     (`pmu_identity`, `pmu_sign_iff`). `PROOFS.tex` writes it with `V = 1`.
     The kernel `g^(Q-1) (1 - g)` rises up to `(Q-1)/Q` and falls after it
     (`kernel_rises`, `kernel_falls`, `kernel_max`).
     Decision recorded 7 Oct 2026. The author asked for the two passages to
     be rewritten. `PROOFS.tex` §P-MU now states the identity with `V` and
     `C`, corrects the orientation and states the quasi-concavity with its
     hypothesis, and open item 5 now names what remains open. The tier caveat
     on P-MU, the P-MU row of the verification index and the matching item of
     `LITERATURE.tex` were brought into line in the same edit.
  3. **"First-order stochastic dominance alone cannot sign the comparison"
     is not yet proved.** The hump does not show it. A proof needs two
     admissible pairs with `F <= G` and opposite signs of `D(Q)` at one `Q`,
     which pass 3g can supply as exact witnesses.
     Proved 7 Oct 2026 at the author's request
     (`lean/mathlib/PMUWitness.lean`, `fosd_does_not_sign`). The pair
     `F = x`, `G = 1 - (1-x)^2` gives `D(1) = V/60` and `D(2) = V/420`, and
     the pair `F = x^2`, `G = x` gives `D(Q) = -V Q/((Q+2)(Q+3)(Q+4))` for
     every `Q` (`pairB_step`). Both pairs have `F <= G`, no atoms and an
     investing incumbent. The claim is therefore true, at `Q = 1` and at
     `Q = 2`.
  - Done when each item has the author's decision recorded and, where
    accepted, the edit applied in the manuscript skeleton.

- [!] **L4. Lean pass 3g, 7 Oct 2026. Decisions for Anurag.** Items 0 to 2
  rest on `lean/mathlib/Refutations.lean`, item 3 on
  `lean/mathlib/Anonymity.lean`. The success-or-failure family has `r`
  uniform on `[0, 1]`, an investment that fails (`s = 0`) with probability `p`
  and succeeds (`s = 1`) otherwise, `0 < mu <= 1/2` and an investing
  incumbent. In that family
  `Delta(0, Q) = V ((1 - p^2)/2 - p(1 - p)/(Q + 1))` for every `Q >= 1`
  (`bern_Delta`).
  0. **R1 is refuted exactly.** `Delta(0, Q) >= V (1 - p)/2` at every `Q`
     (`bern_Delta_lower`), so a challenger whose cost is at most that enters
     however many challengers compete, and `Delta(0, Q)` converges to
     `V (1 - p^2)/2` (`bern_Delta_limit`). The mechanism holds in general.
     If non-investor scores never exceed `xG`, then
     `Delta(m, Q) >= V (int_{x >= xG} C F^m dF - 1/(Q - m))`
     (`r1_lower_bound`). The limit formula of `PROOFS.tex` §R1, the product of
     `Pr(X > mu)` and `Pr(beat incumbent)`, is wrong in this family. There
     `Pr(X > mu) = 1 - p` (`bern_investor_above`) and an investor beats an
     investing incumbent with probability `1/2`, so the product is
     `V (1 - p)/2`, whereas the limit is `V (1 - p)(1 + p)/2`, larger by the
     factor `1 + p`. A plausible reading, not proved, is that scoring above
     `mu` and beating the incumbent are positively related, since an
     investor above `mu` also beats an incumbent whose investment failed.
  1. **R2 is refuted exactly.** `Delta(0, .)` rises strictly at every `Q`, for
     every `0 < mu <= 1/2` and `0 < p < 1` (`bern_Delta_strictMono`). With a
     cost between `Delta(0, Q)` and `Delta(0, Q + 1)`, the richest challenger
     enters with `Q + 1` challengers and stays out with `Q`, so the entrant
     count rises with `Q`. The further claim of `PROOFS.tex` §R2, that the sign
     reverses at a crossover `mu*` in `[0.1, 0.7]`, rests on one family,
     `r` and `s` both Beta(2,2), in `checks/run_all.py`, and has no Lean
     witness. The referee's direction has not been exhibited exactly in the
     model.
     Decision recorded 7 Oct 2026. The author asked for the passages to be
     rewritten with verified content. `PROOFS.tex` §R1 now carries the exact
     family and the general bound and withdraws the product formula, §R2
     carries the exact strict rise and the uniform monotonicity and marks the
     crossover as an illustration, and the index rows R1 and R2 cite the Lean.
     `LITERATURE.tex` §Refuted conjectures was brought into line.
     The referee's direction is now exhibited exactly (`fall_from_one` in
     `lean/mathlib/FallWitness.lean`, 7 Oct 2026), so §R2 now says that the
     sign depends on the primitives and that the law of `r` sets it.
  2. **With a uniform base score the first entrant's gain never falls in
     `Q`.** For any law of `s >= 0`, any `0 < mu <= 1` and any incumbent,
     `Delta(0, .)` is non-decreasing (`pmu_step_nonneg_uniform`,
     `Delta_mono_uniform`). The weight `W` vanishes above `mu`, where the
     non-investor's score cannot reach, and below `mu` the investor's law lies
     under the non-investor's. The rise-then-fall claim the author adopted on
     7 Oct 2026 is therefore monotone in the one case proved from the
     primitives. A fall needs the crossing point `x0` strictly inside the
     support of `G`, which plausibly requires a density of `r` that falls
     toward the top of its support, as Beta(2,2) does. That is not proved.
     The author should decide whether the claim is stated as "rises, and
     never has an interior minimum" with the uniform case monotone, or
     whether a witness with a fall is sought first.
     Decision recorded 7 Oct 2026. The witness is sought first, in Lean. The
     fall needs the investment to lift the score by less than `mu`, so that
     the crossing point sits inside `(0, mu)`, and a density of `mu r` that
     falls above the crossing point. With `s` two-point, `0` or `t/(1 - mu)`,
     `dF - dG = (1 - p)(g(x - t) - g(x))`, negative below `t` and positive
     above `t` when `g` falls, so the single-peaked hypothesis holds at
     `x0 = t`. A SymPy exploration, which is not evidence, found the gain
     rising twice and then falling for `r` the smaller of two uniform draws,
     `s` equal to `0` or `1/2` with probability `1/2` each, `mu = 1/2` and an
     investing incumbent, with steps `23/960`, `491/86016` and
     `-1277/2580480`. The same `r` with `s` equal to `0` or `1/4` gave a fall
     from `Q = 1`, the referee's direction. The Lean proof is the next task.
     Done 7 Oct 2026, in `lean/mathlib/FallWitness.lean`. The cheapest
     parameters are `s` in `{0, 1}`, `mu = 3/4` and an investing incumbent,
     so that only `Q = 1` and `Q = 2` are needed. At `p = 1/2` the gain rises
     from `Q = 1` to `Q = 2` and falls from `Q = 2` to `Q = 3`
     (`rise_then_fall`). At `p = 1/4` it falls from `Q = 1`
     (`fall_from_one`). The claim can be stated with the fall exhibited. The
     general statement that the single-peaked hypothesis holds for a base
     density that does not rise and a single-step lift below `mu` is the next
     proof to add, at the author's request for proofs beyond witnesses.
     Done 7 Oct 2026, in `lean/mathlib/ShiftClass.lean`. For any base law
     with no mass at or below 0 that never loses mass when a set above `c`
     moves down by `c`, any `0 < mu < 1`, any failure probability and any
     incumbent, `Delta(0, .)` is quasi-concave in `Q`
     (`quasiconcave_of_shiftMono`). A density that does not rise on the
     positive reals gives that property (`shiftMono_of_density`), and the
     witness's base law has density `2(1 - r)` and lies in the class
     (`witness_quasiconcave`). Not proved is a condition under which the gain
     must fall, and laws of `s` beyond a single step.
     The must-fall condition is proved 7 Oct 2026, at the author's request,
     in `lean/mathlib/MustFall.lean`. Let `G_t` be the non-investor's law
     shifted up by the lift `t = 1 - mu`. If `G_t` puts strictly more mass
     than `G` on some `(a, b]` with `t < a < b`, `G(t) < G(a)`, `G(b) < 1`,
     `C(a) > 0` and `p < 1`, the gain falls strictly at every `Q` from some
     `Q0` on (`must_fall`). The witness's base law meets the condition for
     every `p < 1` (`witness_must_fall`), and a uniform base law meets it on no
     interval (`uniform_fails_gain`). Open are the location of the peak in
     `Q` and laws of `s` beyond a single step.
  3. **The anonymity boundary has an exact witness inside the primitives.**
     Three challengers have wealth `11/4, 9/4, 2`, abilities `1, 2, 3` (the
     largest of `n` uniform draws, so ability falls with wealth), CRRA
     utility with `gamma = 2`, `c = 1` and `V = 1`, and the incumbent has
     ability 1. At `mu = 0` the poorest challenger entering alone and the two
     richest entering together are both equilibria (`anon_witness_zero`). All
     six equilibrium conditions hold strictly, every win probability is
     continuous at `mu = 0` (`winProb_tendsto`), so both equilibria persist
     for every small `mu > 0`, where every law has no atoms
     (`anon_witness_positive`, `anon_witness_exists`). With identical laws the
     gain is `Delta(|T|)` (`gain_anonymous`), which is Proposition (anonymity)
     in the setting with different laws. The witness in `PROOFS.tex`
     (`Q = 4`, `mu = 1/2`, `theta = -3/2`) remains numerical, and the claim
     that count invariance holds when ability rises with wealth remains
     support from sampling, as `PROOFS.tex` already says.
     Decision recorded 7 Oct 2026. The primitives paragraph of `PROOFS.tex`
     now carries the exact witness and `gain_anonymous`, keeps the sampling
     support for ability rising with wealth as support, and keeps the
     quadrature witness as an illustration. The index row ANON cites the
     Lean.
  - Done when each item has the author's decision recorded and, where
    accepted, the edit applied in the manuscript skeleton or in `PROOFS.tex`.

- [x] **L5. Pass 4c, 7 Oct 2026. Decided by Anurag 7 Oct 2026.** Each item rests on
  `lean/mathlib/Spreads.lean`. Profiles are sorted, richest first, and a
  challenger at or below the fee does not enter.
  0. **The tail condition needs only two ranks.** `PROOFS.tex`
     §R1 (Proposition tail condition) asks that wealth weakly rise at every
     rank up to the margin for the count not to fall, and weakly fall at
     every rank beyond it for the count not to rise. With both profiles
     sorted, the rise branch needs only the marginal entrant's wealth not to
     fall (`margin_rise`), because every richer challenger after the spread
     is at least as rich as the marginal entrant is. The fall branch needs
     only the first outsider's wealth not to rise (`margin_fall`). The
     rank-by-rank conditions are special cases (`tail_rise`, `tail_fall`).
  1. **The band moves, and the sharpness claim of `PROOFS.tex` is false as
     stated.** With `d = w' - w`, the signs of `d` leave the direction open
     only when `d` is negative at the marginal entrant and positive at the
     first outsider (`branch_of_not_band`). `PROOFS.tex` §The band defines the
     band as `L < k* <= M` from the whole sign pattern and says the tail
     condition "is the whole of what the sign pattern supports". The margin
     condition is strictly weaker and still settles the direction, so that
     sentence is wrong. What survives is the corrected band. A pivot-spread
     never enters it (`not_band_of_single_crossing`,
     `pivot_single_crossing`), and inside it both directions occur with the
     same signs, exactly, for every small `mu > 0`
     (`band_both_directions_positive`). The frequencies of
     `checks/verify_tailband.py` (T1 to T5, the 47.2% figure) describe the old
     band and are sampling in any case.
  2. **The novelty ledger row for the tail condition** ("One-sided tail
     condition, fall branch free at the margin", SURVIVES minor) should say
     that each branch reads one rank. Pass 7 should weigh whether that
     changes the comparison with Costrell and Loury.
  - The manuscript rows M12 to M14 already state the margin version, since
    it is the verified statement.
  - **Decisions, 7 Oct 2026.** "Entrant" and "entry count" stay, with no
    rename to "field" for now. Item 1 is done now. `PROOFS.tex` §R1 states
    Proposition (margin condition) at ranks `k*` and `k*+1` (1-indexed), with
    the support-floor rule, and §The band states the band `d_{k*} < 0 <
    d_{k*+1}`, with no "iff" and the sharpness sentence withdrawn. The index
    rows R1 and BAND, the terminology table, the contribution passage, open
    item `item:tailopen`, `MEASUREMENT_MAP.tex` and `LITERATURE.tex` follow.
    Item 2 is done now. The two ledger rows are revised below.
  - **Added during the rewrite, all in `lean/mathlib/Spreads.lean` and
    axiom-audited.** `mps_lowers_count`: the profiles (2.9, 2.8, 2.0) and
    (3.6, 2.2, 1.9) have the same total, the second majorizes the first, the
    marginal entrant is above the mean, and the count falls from 2 to 1 at the
    benchmark gains. The mean therefore cannot replace the pivot, and this
    replaces the sampled R1-SCOPE. The fall branch applies to this pair and
    allows the fall. `mps_band_signs`, `mps_witness_counts`,
    `mps_band_both_directions`, `mps_band_both_directions_positive`: with
    four challengers, (4, 2.8, 2.2, 2) has count 2, and two changes that keep
    the total and majorize it, with signs (+, -, +, -) at every rank, put the
    margin in the band and move the count to 3 and to 1, at the benchmark gains
    and for every small `mu > 0`. Mean-preservation therefore does not keep a
    margin out of the band, and the earlier band witnesses
    (`band_both_directions`) did not keep the total. M13 and M14 of
    `MANUSCRIPT.tex` now cite the new theorems.

- [!] **L6. Fullerton and McAfee (1999) read, 7 Oct 2026. Decisions for Anurag.**
  Read in full from `auction_entry.pdf` (printed pages 573 to 603, the last two
  pages absent from the upload). Everything below rests on
  `lean/mathlib/FullertonMcAfee.lean` (35 theorems, axiom-audited) and
  `lit/fullerton_mcafee_1999/`.
  0. **"Differs in kind" narrows.** Their efficient entry equilibrium admits the
     lowest-cost firms (Theorem 2) and their contestant selection auction
     admits the best types (Theorem 5), so entrant identity is a property of
     the agents there too. `LITERATURE.tex` §sec:entry made the claim
     conditional on exactly this and now states the narrower version. What
     separates P5 is the sorting variable. Their types enter the contest, and
     our wealth enters only `kappa`.
  1. **Their entry stage has equilibria of different sizes.** With costs
     `(2.1, 2.3, 2.5, 2.6)`, prize `1` and fixed cost `19/250`, `{2.1, 2.3}`
     and `{2.1, 2.5, 2.6}` are both entry equilibria (`two_sizes`). The proof
     solves their effort subgame from its primitives (existence, uniqueness,
     the active prefix and eq. (4)). An entrant's profit there depends on its
     rivals' costs, so the gain is not anonymous. This is an exact external
     witness that anonymity carries our count invariance (M6). The author
     should decide whether the manuscript cites it where count invariance is
     stated.
  2. **Their Lemma 1 is a precedent for identity multiplicity** under
     heterogeneous types. The novelty ledger's identity rows now cite it.
  3. **Discrepancies in the source, recorded and not ours to fix.** Lemma 2 as
     printed is false for a single `m` (`lemma2_single_m_fails`), Theorem 4's
     hypothesis as printed holds for every `Psi` (`thm4_hyp_always`), Theorem
     2's first display needs a sign condition (`thm2_iff_needs_sign`), and
     Theorem 3's "positive" is "nonnegative". `NOTES.md` §2 has the detail.
  4. **Convention.** The paper's claims need real analysis, so its Lean file
     lives in `lean/mathlib/` under the Mathlib audit, and the paper directory
     holds a `LEAN` pointer that `lit/coverage.py` now reads.
  - Done when the author decides item 1.

- [ ] **O1. Open items carried from `PROOFS.tex` §Open items (pass 5, 7 Oct 2026).**
  1. **The incumbent's own entry** is exogenous throughout (`C` is fixed in
     `H_m`). Endogenising it, and asking whether an equilibrium exists in which
     the incumbent abstains while a challenger enters, is a modelling extension
     and not a gap in what is claimed.
  2. **Inside the band** (M14) the displacement signs settle nothing. Whether
     some other statistic of the change, its magnitudes or a weighted
     functional of the quantile difference as in Costrell and Loury, predicts
     the direction is open.
  3. **Strictness away from the linear family.** M15 is stated for the linear
     spread with `λ` large. Which spreads move `k*` strictly is not attempted.
  4. **The competitor-number effect.** The location of the peak of
     `Δ_Q(0)` in `Q` (M17 to M21), and laws of `s` beyond a single step, are
     open.
  5. **Ability rising with wealth.** Count invariance held in every sampled
     economy (`checks/verify_anonymity.py`, A1-4b). This is not claimed, and a
     proof or a counterexample is open.
  6. **A comparative static in `μ`.** Both referees asked how the count moves
     with the weight of the investment in the score (R1.a7, R2.1). Nothing in
     Lean addresses it, and the coverage appendix marks it open.
  - Closed since `PROOFS.tex` listed it: how much room burden-monotonicity
    leaves beyond concavity. A convex stretch longer than `c` rules it out
    (`not_burden_of_convex_stretch`), and every length below `c` occurs
    (`admissible_width`).

- [ ] **PLAN. Pass plan agreed 6 Oct 2026.** One manuscript skeleton in LaTeX
  replaces `PROOFS.tex`. It has four tables (introduction, model, literature,
  conclusions), and appendices hold every proof and everything else
  `PROOFS.tex` carries. Each appendix proof names the Lean theorem it
  corresponds to, and analytic steps move into Lean with Mathlib. Plain
  English is at most 30% of each proof, checked by script. Passes run in the
  order below. Passes 1 to 4 do not depend on the novelty verdict and can run
  now. Passes 7 to 9 wait for it.
  Status 6 Oct 2026. Passes 1 and 2 are done (eace8d3e, d369cd86). Pass 3 is
  next.
  Revised the same day. Verification depends on Lean alone; SymPy and
  numerical sampling no longer count as evidence for any claim. The Lean
  files are for our verification and are not submitted, so Lean names leave
  the printed manuscript before submission. Every statement in the paper is
  a Lean theorem, a Lean-checked counterexample, a stated model assumption, or
  a quoted literature claim. Numbers may appear as illustrations only.
  Pass 3a done 6 Oct 2026. `lean/mathlib/StepIdentity.lean` proves the step
  identity for any score laws without atoms, with no densities and no
  integration by parts, through the fact that exactly one of three
  independent draws is the largest. `lean/mathlib/EntryContestModel.lean`
  builds `H_m` and `Delta` from the score laws and discharges the hypothesis
  `step_nonpos` of `EntryContest.equilibrium_is_threshold`. The P5 threshold
  structure is therefore machine-checked from the model's assumptions, and the
  abstract's "no claim is machine-checked end to end" no longer holds for P5.
  `StepNonpos.lean`, which assumed densities, was removed as superseded.
  Pass 3b done 7 Oct 2026. `lean/mathlib/RepresentationFOSD.lean` proves P1,
  `Delta(m) = V E[phi(M_m)]`, by the two-draw version of the same argument;
  P2, `F <= G`, from score laws built from the laws of `r` and `s`; the strict
  part of P2 on an interval `[q, u)`, as `PROOFS.tex` states it; and
  `Delta >= 0` from the primitives. P2 needs only `mu <= 1`, since the
  compiler reported `0 <= mu` unused.
  Pass 3c done 7 Oct 2026. `lean/mathlib/SaturationBenchmark.lean` proves P7,
  `Delta(m) <= V/(m+1)` for every `Q`, and the cap `(m+1) kappa <= V` through
  `EntryContest.entry_index_bounded`. The bound needs no atoms in the
  investor's law only. The non-investor's law and the incumbent's law can be
  any probability laws, so P7 holds for every incumbent, not only for
  `C = F` or `C = G`. `PROOFS.tex` writes `int F^m dF = V/(m+1)` in the P7
  proof, where the factor `V` belongs to `Delta` and not to the integral.
  The same file proves P6 twice. `p6_at_zero` evaluates `Delta` at `mu = 0`,
  which is the argument `PROOFS.tex` gives. `p6_limit` proves
  `Delta -> V/(m+2)` as `mu -> 0`, which is the statement `PROOFS.tex` makes
  and does not prove. The limit matters because at `mu = 0` the non-investor's
  score is the point mass at 0, so `G` has an atom and lies outside the
  primitives' continuity assumption. For every `mu` near 0, `G` is the law of
  `mu r`, and the limit holds for any law of `r`. Both theorems assume
  `s >= 0` almost surely and no atoms in the law of `s`, and the incumbent
  invests (`C = F`), as in `PROOFS.tex`. The proviso `s > 0` almost surely in
  `PROOFS.tex` follows from those two assumptions (`Iic_zero_null`).
  Pass 3d done 7 Oct 2026. `lean/mathlib/KappaSpread.lean` proves that strict
  concavity of `u` gives strict burden-monotonicity, that `kappa` diverges at
  `c` exactly when `u(0+) = -infinity`, and both facts for log and CRRA. It
  then proves P9 strictness over the reals with an explicit entrant count
  (`p9_strict`), the monotone part of the same proposition (`p9_monotone`),
  and P9 strictness from the primitives with the model's `Delta`
  (`p9_strict_model`). The analytic step (b) that `lean/EntryContest.lean`
  took as a hypothesis is therefore proved. Findings are in item L2.
  Pass 3f done 7 Oct 2026, ahead of pass 3e at the author's request.
  `lean/mathlib/BurdenWeaker.lean` proves Claim BM with an admissible example,
  shows the example of `PROOFS.tex` inadmissible, proves that
  burden-monotonicity follows when `u'` falls across every span of length `c`,
  and bounds the admissible convex width at `c` from both sides. Details are
  in item L2.0. Proofs that go beyond what the manuscript states are to be
  kept in a proofs-addendum document, created with pass 4.
  Pass 3e done 7 Oct 2026. `lean/mathlib/PMU.lean` proves the P-MU identity
  and sign condition, the hump of the kernel, the corrected single-crossing
  step, the quasi-concavity of `Delta(0, .)` in `Q` that follows from it, and
  the single-crossing hypothesis for a uniform base score. Findings are in
  item L3.
  Pass 3g done 7 Oct 2026. `lean/mathlib/Refutations.lean` refutes R1 and R2
  in an exact family of the model and proves the R1 mechanism in general.
  `lean/mathlib/Anonymity.lean` states win probabilities for challengers with
  different laws, proves Proposition (anonymity) in that setting, and gives
  an exact two-size witness that holds for every small `mu > 0`. Findings are
  in item L4.
  Pass 3g addendum, 7 Oct 2026. `lean/mathlib/FallWitness.lean` exhibits the
  rise-then-fall shape and the referee's direction exactly, inside the
  primitives, with `r` the smaller of two uniform draws. The closed forms of
  the first two steps in `Q` are polynomials in the failure probability `p`.
  Pass 3h done 7 Oct 2026. `lean/mathlib/MeasurementMap.lean` proves MM1 to
  MM10 over the reals, in general where the SymPy fixed a size or a family,
  and cites the earlier theorems for MM3 to MM5. The pass also closed a gap in
  P5-inv. The two counting facts that `lean/EntryContest.lean` took as
  hypotheses, left to enumeration in `checks/verify_equilibria.py`, are now
  proved (`exists_mem_ge`, `exists_not_mem_le`), and `count_invariance`
  holds with no hypothesis beyond the model's. `MEASUREMENT_MAP.tex` and the
  P5-inv passage and row of `PROOFS.tex` cite the Lean. Pass 3 is complete.
  Pass 4a done 7 Oct 2026. `MANUSCRIPT.tex` now carries model rows M1 to M8
  (P1, P2, P3, the monotonicity corollary, P5, count invariance, identities
  pinned, N4) with a notation block and eight proofs in Appendix A, each
  naming its Lean theorems and under the 30% prose limit. Writing the rows
  found three statements of `PROOFS.tex` not machine-checked, now proved in
  `lean/mathlib/Equilibrium.lean`. The strict part of the monotonicity
  corollary (`Delta_step_strict`). N4 (i), that the assortative set is an
  equilibrium (`assortative_is_equilibrium`). N4 (ii), that it minimises total
  cost, which `PROOFS.tex` said was "checked numerically rather than proved in
  Lean" (`assortative_min_cost`). N4 (iii) is now derived from the win
  probabilities (`prize_term_anonymous`, `payoff_gap_anonymous`) instead of an
  integer-list abstraction, and count invariance is stated for `Q`
  challengers (`count_invariance_fin`). Three corrections carried into the
  rows. P5 needs only a non-increasing `Delta`, where `PROOFS.tex` says
  "strictly decreasing". The strict part of P2 needs `mu < 1`, which
  `PROOFS.tex` omits. The step identity needs only atomless laws, so the
  regularity assumption of `PROOFS.tex` (common lower endpoint, `phi`
  vanishing at the endpoints) is unused and is for pass 5 to drop.
  `PROOFS_ADDENDUM.tex` is created with eight results more general than the
  manuscript states. `checks/verify_manuscript.py` now accepts theorem names
  with non-ASCII letters. Next is pass 4b, P6 to P8.
  Pass 4b done 7 Oct 2026. Model rows M9 (P6), M10 (P7) and M11 (P8) with
  their proofs. M9 states both the value at `mu = 0` and the limit as
  `mu -> 0`, with the factor `V`, where `PROOFS.tex` states only "as
  `mu -> 0`, `Delta(m) = 1/(m+2)`". M10 carries `V` correctly, where the P7
  proof of `PROOFS.tex` writes `int F^m dF = V/(m+1)`. For P8, the step from
  monotone entry sets to a monotone `k*` was not in Lean and is now
  (`kstar_mono`), with the prize effect (`Delta_mono_prize`) and the fee
  effect for any non-decreasing `u` (`kappa_mono_fee`), where `PROOFS.tex`
  differentiated `u`. Next is pass 4c, the P9 family with the band.
  Pass 4c done 7 Oct 2026. Model rows M12 (the margin condition), M13 (pivot
  spreads, P9-gen and P9), M14 (the band) and M15 (strictness under a spread)
  with their proofs, on `lean/mathlib/Spreads.lean` and
  `lean/mathlib/KappaSpread.lean`. The notation block states the support-floor
  rule. Writing the rows found that the tail condition needs only two ranks
  and that the band of `PROOFS.tex` is the wrong one, recorded as item L5.
  L5 decided and applied 7 Oct 2026. Next is pass 4d, P-MU.
  Pass 4d done 7 Oct 2026. Model rows M16 to M22 with their proofs, on
  `PMU.lean`, `Refutations.lean`, `ShiftClass.lean`, `FallWitness.lean`,
  `MustFall.lean` and `PMUWitness.lean`. M16 is the identity and the kernel,
  M17 the rise-then-fall result under single crossing, M18 the uniform base
  score (non-decreasing in `Q`), M19 the shift-monotone class, M20 the fall
  exhibited, M21 the must-fall condition and M22 the dominance witnesses. The
  notation block defines `Delta_Q(0)`, `D(Q)` and `W_Q`. Writing M21 found that
  the uniform case assumed the uniform law shift-monotone without proof, and
  `MustFall.lean` now proves it (`unif_shiftMono`, `uniform_never_gains`).
  Next is pass 4e, BM and ANON.
  Pass 4e done 7 Oct 2026. Model rows M23 (burden-monotonicity), M24 (strictly
  weaker than concavity, the corrected ramp construction), M25 (divergence at
  the fee), M26 (anonymity) and M27 (the anonymity boundary) with their proofs,
  on `KappaSpread.lean`, `BurdenWeaker.lean` and `Anonymity.lean`.
  `Anonymity.lean` gained the ASCII alias `kappa3_values` so the manuscript can
  cite the costs. Pass 4 is complete. Next is pass 5.
  Pass 5 done 7 Oct 2026. The five appendix stubs of `MANUSCRIPT.tex` are
  filled. Evidence and verification is rewritten for the current state (every
  row a Lean theorem, the axiom audit, the manuscript checker, the `lit/`
  records, the tiers retired, and the one assumption Lean does not derive,
  atomless `F` and `G`). Terminology carries the `PROOFS.tex` table into
  manuscript notation, with the margin condition, the band and ordering in
  dispersion. Primitives and scope drops the unused regularity assumption and
  the inadmissible sine example, and cites the multiplicity example to a new
  theorem, `EntryContestEq.three_equilibria`. Refuted conjectures R1 and R2
  and the referee responses point to Lean and to model rows. The open items of
  `PROOFS.tex` are below as item O1. Still in `PROOFS.tex` and not yet carried:
  the section "Contribution and its precedents", which is input to the
  conclusions table and goes to `LITERATURE.tex` at pass 6, and the
  verification index, which the model table's Lean column supersedes.
  Next is pass 6.
  Pass 9 and the headlines done 7 Oct 2026, ahead of passes 6 to 8 at the
  author's request ("just do it for what you have and mark any unverified
  claims as unverified"; "You're welcome to put headline as the first
  section"). The literature table has L1 to L13, one row per paper read, each
  quote verbatim in the paper's `lit/` record. Fu-Lu, Costrell-Loury and
  Schroyen-Treich are marked unverified for their published versions, and
  Shaked, Mathews-Namoro, Corcoran and Corcoran-Karels are listed below the
  table as unread. Two one-line quotation records were added (Lazear-Rosen,
  Hopkins-Kornienko), copied from quotations already recorded. The
  introduction table has K1 to K6, each with a scope note saying what it does
  not claim. Next are readability (pass 10) and the referee comments (pass 11),
  with the coverage of the referee comments as a separate appendix.
  Pass 10, first round, done 7 Oct 2026. The notation block now glosses `M_m`
  and `K_m`, M8 (iii) no longer reuses `T`, M10 names the entrant's cost, M11
  defines `Delta_V`, M6 opens by naming the equilibrium, and M12 and the
  notation say "change" where any change of profile is meant. Every proof is
  at most 21% prose. Further rounds may follow.
  Pass 11 done 7 Oct 2026. Appendix G, "Coverage of the referee comments",
  maps every substantive comment of both reports
  (`HANDOVER_referee_reports_20260831.md`) to the model rows or appendices that
  answer it, marked answered, partly answered, open or moot. Open are a
  comparative static in `μ` (O1.6) and the incumbent abstaining while a
  challenger enters (O1.1). Partly answered are locating the contribution
  (waits on pass 7), the welfare objective, and a reduced-form contest success
  function. Remaining from the plan are pass 6 (retire `PROOFS.tex`), pass 7
  (four PDFs) and the conclusions table, which waits on pass 7.
  Pass 3 therefore splits into 3a step identity (in full generality, no
  densities, decided by the author), 3b P1 and P2, 3c P7 and P6, 3d kappa
  divergence and monotonicity, 3e P-MU, 3f burden-monotonicity weaker than
  concavity, 3g R1, R2 and the anonymity counterexample as exact
  counterexamples, 3h the measurement-map checks. Pass 6 drops statements
  that only sampling supports (the theta > 0 support claim, band frequencies,
  violation counts).
  1. **Skeleton scaffold.** `MANUSCRIPT.tex` with the four tables empty, the ID
     scheme (K, M, L, C rows), appendix stubs, a link checker (K points to M; C
     points to M or L; every M row names a Lean theorem that exists and is
     audited) and the 30% prose checker. Both checkers wired into `verify.sh`.
     Done when the empty skeleton builds and both checkers run and can fail.
  2. **Mathlib project.** A lake project under `lean/mathlib/` pinned to a
     Mathlib that builds here, the drafted `StepNonpos.lean` (item A2)
     compiled, and an audit rule for Mathlib files that allows the three
     standard axioms (propext, Quot.sound, Classical.choice) and nothing else,
     while core files keep propext and Quot.sound only. Done when `verify.sh`
     builds and audits it.
  3. **Analytic steps into Lean**, one cluster per pass. `step_nonpos` (P3,
     the engine of P5); the P1 representation; P2's probability monotonicity;
     P7's integral monotonicity; P6's limit; the divergence of kappa at the
     support floor (P9-strict); the P-MU identity. Each SymPy-tier claim moves
     to tier L, and the abstract's "no claim is machine-checked end to end"
     is revised only when the chain is closed.
  4. **Proofs into the appendix**, one result family per pass (P1 to P5 with
     count invariance and N4; P6 to P8; the P9 family with the band; P-MU; BM
     and ANON). Each proof names its Lean theorem, carries the L1 corrections
     the author accepts, and passes the 30% checker. Model-table rows are
     filled in the same pass, with the mathematics shown.
  5. **Non-proof content of `PROOFS.tex` into appendices.** Evidence tiers and
     the verification index, terminology, scope and the anonymity boundary,
     refuted conjectures R1 and R2, referee responses. Open items go to this
     file.
  6. **Retire `PROOFS.tex`.** A completeness check confirms every claim,
     number and Lean name in `PROOFS.tex` is present in the skeleton.
     `checks/verify_docs.py` and `verify.sh` then point at the skeleton, and
     `PROOFS.tex` is removed. Done when `verify.sh` is green without it.
  7. **Novelty reading.** Fullerton-McAfee (1999), Mathews-Namoro (2008),
     Corcoran (1984), Corcoran-Karels (1985) and Shaked (1982), each with a
     `lit/` directory and Lean, then the novelty ledger revised. Blocked on the
     PDFs. Fullerton-McAfee read 7 Oct 2026 (item L6). The upload expected to
     be Shaked turned out to be Lewis and Thompson (1981), *J. Appl. Prob.* 18,
     76-90, which the author decided on 7 Oct 2026 to use as an earlier source
     for the dispersive order. Read in full, `lit/lewis_thompson_1981/`,
     `lean/mathlib/LewisThompson.lean` (21 theorems). Their definition is
     Hopkins and Kornienko's Definition 1 (`cdf_iff_spacing`,
     `spacing_iff_diff`), and their claim that `X` and `kX` are ordered for
     every `k ≠ 1` fails at `k = −1` (`neg_not_ordered`). `PROOFS.tex` and
     `LITERATURE.tex` now cite it beside Shaked. Shaked (1982) itself remains
     unread.
  8. **Conclusions, then introduction.** Conclusions carry only the novel
     claims from the revised ledger, each linked to model or literature rows.
     The introduction states the headlines and their number, each traced to a
     model row and assessed for overreach.
  9. **Literature table.** Only the papers a conclusion or model row depends
     on, with verbatim quotes and pages from `lit/<paper>/CLAIMS.md`.
  10. **Readability passes** on the appendix proofs, as many as needed, under
     the 30% checker.
  11. **Referee comments, again.** Agreed 7 Oct 2026. Once passes 1 to 10 are
     done, re-read every referee comment against the verified results and
     record, for each, whether the skeleton answers it, with the Lean theorem
     or quote that does. The author asked for no rush, with every step
     verified before the next.
  Running alongside, as PDFs arrive. Lean for the remaining cited works,
  Hopkins-Kornienko 2004 and 2009 claims, published-version checks (S1), and
  the welfare result W1 if the conclusions are to claim it.

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

## Novelty ledger (assessment, revised 2 Sep 2026, rows marked revised 7 Oct 2026 — revise only against evidence)

Where each candidate claim stands. Anything not listed as SURVIVES is not to
be claimed as novel.

| Claim | Verdict | Ground |
|---|---|---|
| MPS sign determined with `u''` alone, where the intensive-margin result needs `u'''` | **SURVIVES** | Both ends verified: ST Thm 3's `u'''`-dependence exhibited by exact separator (CARA `a=1` vs log at `w=1`: same `A=1`, `P=1` vs `2`, opposite signs at `m=1/2`); P9-gen's independence machine-checked; quadratic utility separates the channels (`kappa'=-c/4<0` while `A'>0`, IARA) |
| Margin condition, each branch reading one rank: the marginal entrant's for the rise branch, the first outsider's for the fall branch (revised 7 Oct 2026, L5; was "One-sided tail condition, fall branch free at the margin") | **SURVIVES (minor), pending pass 7** | `margin_rise`, `margin_fall` in `lean/mathlib/Spreads.lean`, axiom-audited. The rank-by-rank form (`tail_rise`, `tail_fall`) and the pivot rule (`pivot_rise`, `pivot_fall`) are special cases, and the earlier grounds (`nonentry_preserved_of_beyond_margin`, R1-WEAK sampling) are subsumed. The proof is the prefix structure of the new profile. A sharpening of the hypothesis, not a new result. Pass 7 weighs it against Costrell and Loury (2004), whose sign also turns on the quantile of one marginal agent, before anything is claimed |
| Dispersive order + crossing ⇔ pivot-spread class, crossing necessary | **SURVIVES (minor)** | `Dispersive.lean`, 7 theorems, axiom-audited. Converts the pivot class from ad hoc to HK's own stochastic order. A lemma about stochastic orders, not about contests |
| Assortative set is the cheapest equilibrium | **SURVIVES as a fact; NOT as a welfare claim** | Existence and cost-minimisation correct, machine-checked and re-run. The welfare reading was corrected under F6 and is now stated as what it is: an unweighted sum of the model's utility units, carrying no resource-efficiency content (all equilibria cost `k*c`) and favouring the wealthiest entrants, verified 1344/1344 in N4-WELFARE. It ranks equilibria at a fixed count and is independent of whether `k*` is too large. **Do not present it as a welfare result.** Selection-grade, not novelty-grade |
| Count invariance across all pure equilibria (P5-inv) | **SURVIVES (conditional on anonymity — see A1)** | `equilibrium_count_unique` machine-checked; `verify_equilibria.py` E2/E3 (20,000 instances) plus an independent re-test (8,000, different seed and code); E6 control shows antitonicity of `Delta` is load-bearing, so it is not a tautology. Stronger than the original P5 claim, which only constructed the assortative equilibrium. Fullerton and McAfee (1999) give an exact external witness that anonymity is load-bearing: in their entry stage, where an entrant's profit depends on its rivals' costs, entry sets of two and three firms are both equilibria (`FullertonMcAfee.two_sizes`, 7 Oct 2026, L6) |
| Identity pinning by the wealth ordering | **WITHDRAWN — was false** | Counterexample in `SOUNDNESS_20260902.md` Finding 1; multiplicity in 17–22% of instances. Holds only under `kappa_(k*+1) > Delta(k*-1)` (`members_below_kstar`). Fullerton and McAfee's Lemma 1 (1999, p.579) is a precedent for non-assortative entry equilibria under heterogeneous types (L6) |
| Margin condition on two sorted profiles, with non-dispersive changes inside its scope, and its band (revised 7 Oct 2026, L5; was "One-sided rank condition on two sorted profiles") | **SURVIVES, narrower than first written; the sharpness claim WITHDRAWN** | Proposition (margin condition) in `lean/mathlib/Spreads.lean`. The statements quantify over arbitrary sorted profiles, so the displacement may cross zero freely away from the two ranks read, and mean-preservation is not used (F3). **The scope caveat stands and is now exact.** The condition is silent exactly on the band `d_{k*} < 0 < d_{k*+1}` (1-indexed; `branch_of_not_band`). Dispersive changes never reach the band (`not_band_of_single_crossing`, `pivot_single_crossing`). Inside the band two mean-preserving spreads with the same signs at every rank move `k*` in opposite directions, exactly, for every small `mu > 0` (`mps_band_both_directions_positive`). A total-preserving majorizing change can lower `k*` with the marginal entrant above the mean (`mps_lowers_count`), so the mean cannot replace the pivot. The 2 Sep claim that the rank-by-rank condition "is the whole of what the sign pattern supports" was false and is withdrawn. The sampled grounds (R1, R1-SCOPE, `verify_tailband.py`) are illustrations, and the band frequencies describe the superseded band |
| Endogenous marginal agent in the pivot rule | **SURVIVES** | Proposition (endogenous margin), machine-checked and independently recompiled (axiom-free); 20,000 economies with `k*` recomputed on both sides, 0 violations; control at the wrong index gives 154/2744, so the hypothesis is not slack. Differs from CL's fixed `theta`; framing settled under F7 — offered as a consequence of the extensive-margin formulation, not as a stronger theorem than theirs |
| P7 uniform cap | **DOES NOT SURVIVE** | `Accounting.lean`: FJL's bound and P7's cap are instances of one lemma |
| P-MU | **DOES NOT SURVIVE as such** | Kernel is RD's (RD-4); "no universal sign" is theirs; S12 limits the both-signs claim to induced `F,G` numerics. Both signs are now exhibited exactly in Lean (`fosd_does_not_sign`, 7 Oct 2026), which leaves the verdict unchanged |
| `Delta(0, .)` rises and then falls in `Q` when `phi` is single-peaked, which holds for a uniform base score | **CANDIDATE, adopted as a claim by the author on 7 Oct 2026, novelty pending pass 7** | `pmu_single_crossing`, `pmu_quasiconcave`, `pmu_quasiconcave_uniform` in `lean/mathlib/PMU.lean`; the fall is exhibited exactly in `lean/mathlib/FallWitness.lean` (`rise_then_fall`). Ryvkin and Drugov (2020) obtain unimodality of individual effort in the number of players with the same kernel through Karlin's step, so pass 7 must decide whether the result is an instance of theirs or a new statement about the entry gain |
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

- [x] **A2. DONE 6 Oct 2026. `StepNonpos.lean` compiles** in the lake
  project `lean/mathlib/` (Lean v4.24.0-rc1, Mathlib 14871d5), after one fix
  to the derivative of `φ²/2`. All three theorems depend on `propext`,
  `Classical.choice` and `Quot.sound` only, audited by suite 12
  (`checks/verify_mathlib.py`). What remains open is in
  `lean/mathlib/README.md`. The record below is the 2 Sep entry.
- [ ] **A2 (2 Sep record). Compile `lean/mathlib/StepNonpos.lean` and report the result.**
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
    what the displacement signs support. **CORRECTED 7 Oct 2026 (L5): false.**
    The margin condition reads two ranks and still settles the direction, so
    the band is `d_{k*} < 0 < d_{k*+1}`, not `L < k* <= M`.
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
