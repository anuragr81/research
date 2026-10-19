# TODO

## Literature, by purpose (rule of 10 Oct 2026)

A paper needs its primary source, a `lit/` record and Lean only when a claim rests on it (verification) or a novelty check needs it. Exploration needs neither; exploratory rows stay at abstract level, marked unverified, and `check.py` rejects any claim that rests on a row not read in full.

### Read, recorded, with Lean

- [x] Borjas (1992) — L4, `lit/borjas_1992`
- [x] Cunha and Heckman (2007), NBER WP 12840 — L1, `lit/cunha_heckman_2007`
- [x] Gibbons and Waldman, NBER WP 6454 (1998) — L2, `lit/gibbons_waldman_1998`
- [x] Cole, Mailath and Postlewaite (1992, JPE) and (1995, CARESS WP 95-14) — L14, L15, `lit/cole_mailath_postlewaite`, read 11 Oct 2026
- [x] Robson (1992), Econometrica — L16, `lit/robson_1992`, read 12 Oct 2026, novelty for the reference-dependent sub-section
- [x] Becker, Murphy and Werning (2005), JPE, read in the authors' December 2003 manuscript — L17, `lit/becker_murphy_werning_2005`, read 12 Oct 2026, novelty for the reference-dependent sub-section
- [x] Ray and Robson (2012), Econometrica — L18, `lit/ray_robson_2012`, read 12 Oct 2026, novelty for the reference-dependent sub-section
- [x] Kőszegi and Rabin (2006), QJE — L19, `lit/koszegi_rabin_2006`, read 12 Oct 2026, contrast case for $R_h$
- [x] Genicot and Ray (2017), Econometrica — L20, `lit/genicot_ray_2017`, read 12 Oct 2026. Group-based aspirations are named as an extension (GR-13, GR-14); cross-group interaction is left open (GR-15). The novelty of the sub-section moves from $R_h$ itself to the interaction of a group reference with closure $\sigma$ in the cross-group pool
- [x] Clark and Oswald (1996), JPubE, read in the August 1995 revision — L25, `lit/clark_oswald_1996`, read 12 Oct 2026; comparison income as the pay of comparable workers, a referent for $R_h$, but evidence from reported satisfaction, not choice
- [x] Currarini, Jackson and Pin (2009), Econometrica, read in the July 2008 working paper — L24, `lit/currarini_jackson_pin_2009`, read 12 Oct 2026; under the closure weighting $\sigma = IH/H$ (Coleman's index), so X25 should cite it
- [x] Markowitz (1952), JPE (Cowles reprint) — L23, `lit/markowitz_1952`, read 12 Oct 2026; reference at own customary wealth, shape near it the reverse of prospect theory's
- [x] Jacobson, LaLonde and Sullivan (1993), AER, read in the Chicago Fed WP-92-28 (revised November 1992) — L26, `lit/jacobson_lalonde_sullivan_1993`, read 12 Oct 2026; losses of about 25 per cent of former earnings, persistent and beginning before separation; reported as shares, closer to a proportional step than to M32's common fallback (flag for the author); an observation for a row for the downgrade
- [x] Kahneman and Tversky (1979), Econometrica — L21, `lit/kahneman_tversky_1979`, read 12 Oct 2026 through a local OCR; source of the shape assumed below $R_h$ and of the unadapted-loss mechanism

### Needed for verification

Cole–Mailath–Postlewaite (1992, 1995) are read and recorded; a claim about $h$ will rest on CMP92 once the law of motion is decided. Otherwise none today: no model, map, headline or concluding row rests on a paper. The source manuscript (`inputs/firmworkers_model.pdf`) invokes these by name where a future claim would need them:

- [x] Friedman and Savage (1948), JPE — L22, `lit/friedman_savage_1948`, read 12 Oct 2026. Their convex region lies between classes, not below a group norm; the appendix's "S-shaped Friedman-Savage utility" must say which reading it means
- [ ] Spence (1973), QJE — "labour-queue theory" (Section 2). Macro model, deferred
- [ ] Tullock lottery with r = 1 (footnote 2) — macro shared-pool abstraction, deferred; reference not given in the source

### Needed for novelty

To be fixed once definite claims exist (after the M14, M19, M23 decisions): every paper that could already contain a claimed result.

- [x] Lazear and Rosen (1981), JPE — L27, `lit/lazear_rosen_1981`, read 12 Oct 2026 (not copied from entry_contest, whose record uses SymPy). Wealth sorting into a rank-order gamble, in the opposite direction to M34 and through DARA; the paper's own approximations favour the piece rate at a common `s` (LR-D3); segregated contests by productivity against closure by `h`
- [x] Hopkins and Kornienko (2004), AER — L28, `lit/hopkins_kornienko_2004`, read 12 Oct 2026. Status spending against the income distribution, the closest precedent for the source's question on inequality and investment; exogenous status removes the race, which supports the reading of `h`
- [ ] Friedman (1953), "Choice, Chance, and the Personal Distribution of Income", JPE 61(4), 277–290 — named by Lazear–Rosen as the earlier work on lotteries in occupations under Friedman–Savage utility, with assignment by endowed wealth; a direct novelty threat for the reference-dependent sub-section. Not on Drive (searched 12 Oct 2026)
- [ ] Cooper, García-Peñalosa and Funk (2001), "Status Effects and Negative Utility Growth", EJ 111(473), 642–665 — cited by Hopkins–Kornienko as status with interaction only within the own wealth group, the full-closure limit; triage from the abstract first

### Exploration only (no primary source needed)

Weber (1978) and Tilly (1998), cited by the source manuscript, exploration only by the author's decision of 11 Oct 2026 (records exist in entry_contest, not adapted). L3 Loury (1977), L5 Bénabou (1996), L6 Bowles, Loury and Sethi (2014), L7 Calvó-Armengol and Jackson (2004), L8 Rosen (1986), L9 Ghiglino and Goyal (2010), L10 Immorlica et al. (2017), L11 Luttmer (2005), L12 Altonji and Pierret (2001), L13 Fershtman, Murphy and Weiss (1996); Alós-Ferrer and Prat (2012). None is cited by the source manuscript; they were added as candidates for the pending micro extensions. A row moves to verification, and needs its PDF, when a decision makes a claim rest on it.

## Decisions recorded

- 10 Oct 2026 (Q1): the micro state is rank plus human capital, (r, h).
- 10 Oct 2026 (Q2): categorical friction enters the rank kernel, not the accumulation of h, because the friction of interest arises from non-contractual relationships in Weber's framework.
- 10 Oct 2026 (Q4): u is not observed; preferences over income lotteries take expected-utility form, so u is a representation unique up to a positive affine transformation and its curvature is meaningful (M16, `premium_sign_affine_invariant`).
- 10 Oct 2026 (Q3): the swap pool is the lottery entrants only (M14 DECIDED). M11–M13 restated over the pool; M24 proves risk-neutral entry unravels when L > 0. Corrected 12 Oct 2026: M30 proves it unravels for every strictly increasing u, and M31 under closure, so the convex region of u does not sustain an entrant-only pool.
- 10 Oct 2026 (Q5): choice set and equilibrium left to tractability (proposal: a one-shot comparison of the three routes for a displaced worker at each rank). Superseded 14 Oct 2026 by the steady state with lifetime values (M39).
- 10 Oct 2026 (Q6): global rank on the ladder, with the source's three channels.
- 10 Oct 2026 (Q9): headlines and conclusions revisited once Q1–Q8 are settled.
- 11 Oct 2026: $h$ is social standing, non-contractual and outside the market; firms observe it but do not price it. Merging $b$ into $h$ is rejected because it would let the firm's price schedule govern social rank. Cole–Mailath–Postlewaite (1992) is the formal reference: the aristocratic equilibrium (status inherited, market decisions undistorted) is the case adopted, wealth-is-status the case rejected.
- 11 Oct 2026: $h$ is a categorical label fixed within a working life; its evolution belongs to the intergenerational layer (M28 DECIDED).
- 11 Oct 2026: friction is one closure parameter $\sigma$, symmetric between two groups, acting on partner draws in the entrant pool, with its limitations stated in M29 (DECIDED). Results M25–M27 in `IscLean/Closure.lean`.
- 12 Oct 2026: lottery entrants are displaced workers, and a displaced worker who does nothing falls to the bottom rung $x_0$ (M32 DECIDED, reading (a)). Results M33–M35 in `IscLean/Displacement.lean`. Specific forms of $u$ go in a separate sub-section with a reference income $R_h$, the income norm of the own status group; main claims stay form-free.
- 13 Oct 2026: the firm is extended simply. Pay tied to results is excluded, because it needs a model of how employees perform differently and a rule for judging them. Wages absorbing firm shocks and learning about ability over a career are the grounds for enhancement, with displacement already in the model. Open choices in ISSUES issue 11.
- 13 Oct 2026: principle of the firm's incentive, to be highlighted as its justification: the firm wants workers to upskill rather than choose the lottery. It is the source's own (lines 126, 130, 258: upskilling spending flows back to the firm, lottery spending is diverted from it).
- 13 Oct 2026: a section Idea summaries, one sub-section per idea, each written to the writing discipline and kept in step with the claims at all times. First idea: why displaced workers choose the lottery. Enforced by `summary_stamps.yaml` and `check.py --restamp` (writing discipline, rule 6).
- 14 Oct 2026: the return is defined (M38 DECIDED): a displaced worker regains the former job, which stays on the ladder, vacant, by paying the upskilling cost of climbing from the income of the downgrade back to the former income; the firm receives the payment. Map row X27 (observation UNVERIFIED); the term 'displaced worker' now says the job survives.
- 14 Oct 2026: the model section has a sub-section Players, with a table of player definitions (firm, job holder, displaced worker) and a table of the choice each makes and what it maximises, each row with a status and the issue that would decide it.
- 14 Oct 2026: steady state (M39 DECIDED). One type of player, the worker, in two states, job holder and displaced; every job holder displaced each period with the same chance $d$, whatever the rank, so nothing protects a job and the source's "less likelihood of being replaced" is dropped; the steady state of this process is the micro model's object, superseding Q5's one-shot comparison; routes are compared by lifetime value (M40, M41 in `IscLean/SteadyState.lean`). Open: the refill rule for vacated jobs (issue 12) and whether route costs are paid once (issue 13).
- 15 Oct 2026: the fallback stays the bottom rung (M32 as decided; issue 11a option (a)) and jobs vacated by downgraders go into the lottery pool (issue 12 option (a)), after the mobility test of `IscLean/Mobility.lean`: the bottom rung keeps the bottom-to-top path open through the lottery for every nondecreasing utility, the proportional step closes it below the top's reduced pay. The specific-skill loss δ is kept as what JLS measure, not as the fallback. Line 375 of the source is a reading of simulations, not a feature.
- 15 Oct 2026: a base case of two displaced workers heads the model section (`base_case` in `claims.yaml`; rows M42 to M46 in `IscLean/BaseCase.lean` and `IscLean/Mobility.lean`). It states the poor-versus-rich mechanism without the macro model: with two entrants the lottery is a sure exchange, both prefer it under the bottom rung for every nondecreasing utility, it moves income from the richer to the poorer, and closure cannot act. Pools of three bring risk and closure.
- 16 Oct 2026: the status group is defined by what it governs (M47 DECIDED): the manuscript's categorisation dimension (line 96), the order outside the market, acting only on access to lottery partners, which is the one allocation of jobs the firm does not make. The idea is Cole, Mailath and Postlewaite's, that the social order touches the economy only through what markets do not allocate, without their marriage model. Weber stays background. Rank is the market order; neither is a function of the other.
- 17 Oct 2026: a lone entrant has no lottery and chooses between the return and the downgrade; the steady state assumes a large economy, proved to make pools of fewer than two negligible (M55, M56); the rule does not change who enters (M57). The lottery cost stays among workers, paid to the members of the entrant's own status group in equal shares (M58; the receiving group left to tractability by the author). A section of questions and answers follows the concluding remarks, each answer grounded and stamped like the summaries.
- 18 Oct 2026: a section Decisions and their reasons follows the FAQ (`decisions` in `claims.yaml` for decisions without a row; DECIDED rows carry theirs). The meaning of the lottery is under discussion (issue 16); M58 is under review.
- 18 Oct 2026: the lottery is read as sponsored mobility (issue 16): displaced workers make themselves favourable to those above, the lottery cost is that spending, and the allocation of vacated jobs stays the random reassignment as the worker's view of favour the rich do not choose. The lottery cost goes to the job holders strictly above the entrant's former rank in equal shares (M58 restated; own-group rule superseded); an entrant with nobody above pays all job holders equally (fallback, open to veto). Spending stays in two channels, upskilling to the firm and the lottery to those above; no third channel. M60 names the two parties, the firm and the job holders above.
- 18 Oct 2026: closure is read as the inbreeding homophily of job-finding connections by status group (idea summary `closure_meaning`); X25's observation to be redrafted on these lines once the closure rule is decided.
- 11 Oct 2026: Weber (1978) and Tilly (1998) stay exploration only; the entry_contest records are not adapted.
- Open (superseded items struck): Q7 (meaning of stakes), Q8 (repairs). Earlier open items, now decided: whether $h$ is fixed within a working life (the aristocratic status rule (12) as its law of motion, with any evolution intergenerational), how friction enters the kernel (CMP92 p.1111: the block on crossing status lines comes from feasibility, not preference, so friction is that block), whether the Weber and Tilly records of entry_contest are adapted.
- 10 Oct 2026: primary sources (with record and Lean) are required for papers needed for verification or novelty; exploration does not need them.
- 10 Oct 2026: proofs stay Lean-only for now. Informal proofs under the skeleton's 30% prose limit (MS-6) are deferred, not dropped.
- 10 Oct 2026: no paper prose for now. Work is on claims; prose will be written from the claims later.
- 10 Oct 2026: the working papers of Cunha–Heckman (NBER WP 12840) and Gibbons–Waldman (NBER WP 6454) are cited as the versions read; the published AER and QJE versions are not needed for now.
- 10 Oct 2026: headlines rest on the measurement map (X rows) as the empirical key, not on model rows alone. Deliberate departure from skeleton MS-2.

## Thinking items (author, 19 Oct 2026; not for urgent resolution)

- **Social rank in the model.** The model's social position is the pair (rank, status group): an income order set by the firm and a binary inherited order outside the market. Since M58 the courting relation pays by income and reaches by status. Limits to state in the paper: no graded honour within a group (status inconsistency appears only as the cross-classification); "the rich" are a gradient, not a class; the two parties are asymmetric (one firm against a passive distribution of receivers). None contradicts the ledger.
- **Language as a case.** Proficiency is upskilling (book-learnable, credentialable); register and accent are acquired only through contact with those above, so they are what the lottery stands for, with closure as the reach of one's circles. Implications to test, literature unread: the register stays a marker of the bottom (M54; Labov's class stratification of speech); closure slows linguistic convergence with half-life ln 2/((1 − σ)d) (M51; Milroy's network theory, Lazear 1999 on enclaves, Chiswick and Miller on linguistic concentration); M47 implies social markers affect income only through access to jobs, never pay within a job, so a speech penalty within the same job (or in open hiring rather than referral) would refute it, while a penalty across jobs conditional on skills is consistent with it; Grogger's speech-pattern wage results (unread, recalled) are the place to check whether the within-job question is answered, so this is the test to pursue first; accents fixed after adolescence and children acquiring the parents' new register support M28's within-life fixity and intergenerational layer. Researched 19 Oct: the model's prediction is a U-shape of contact use in rank (bottom and top gamble, middle returns), not "credentials cannot bridge the gap"; the literature read gives both ends (less educated and blue-collar use contacts more; contacts fill managerial and professional positions), and a computation on Ioannides and Datcher Loury's 2004 appendix table shows the U for one PSID year with small top cells; see `notes/contact_use_by_rank.md`. A claim would need a record of the review and a test on the displaced by pre-displacement wage. Corcoran, Datcher and Duncan (1980) read in full on 19 Oct: contact use falls almost monotonically with schooling and rank, no rise at the top; the three-band pattern is not supported and is in any case a parameter region, not the model's content, and its bands are regions of rank, not classes. The author's intent (19 Oct): the language detour becomes a vignette eventually; no more papers for it now.

## Measurement map

- [ ] Confirm or revise the PROPOSED observation entries (X1–X26, except X12), added 10 Oct 2026
- [x] Support for each observation, 11 Oct 2026: PROVEN by `IscLean/Identification.lean` or Closure/Lottery theorems (X2, X3, X4, X5, X13, X15, X18, X25, X26), DEFINITIONAL (X1, X6, X10, X11, X16, X17, X22, X23), CONSTRUCT (X9, X20, X21), UNVERIFIED (X7, X8, X14, X19, X24); `check.py` rejects a headline on an UNVERIFIED observation
- [ ] Raise the UNVERIFIED observations: X7, X14, X24 need an empirical paper read in full that measures the object; X8 needs M14 decided and proven; X19 needs an identification of the threshold under curved $u$
- [ ] Open for the author (12 Oct 2026): the cost of M7 has no scale, so the return-versus-downgrade comparison depends on the unit of income (M35); options include measuring income in units of $x_0$
- [ ] Add rows for the reference income $R_h$, the downgrade, the rank kernel and a mobility measure as the decisions below are made

## Model decisions pending (micro)

Each option is weighed by whether its objects are observable, since headlines rest on the measurement map.


- [ ] State space: rank plus human capital stock $(r, h)$
- [ ] Law of motion for $h$: does categorical friction enter $f$ (Borjas / Bowles–Loury–Sethi route), the rank kernel only, or both
- [ ] Rank kernel: global rank or reference-set rank
- [ ] Rank kernel: channels (own effort, propagation through contacts, observer learning)
- [ ] Rank kernel: where categorical friction enters (homophily, cross-group pass-through, observer weights)
- [ ] M14: is the swap partner drawn from lottery participants or from all workers
- [ ] M19 and M20: functional form of $u$ and the reference income $r$

## Lean

- [x] M15 for general distributions (Moments.lean, 10 Oct 2026)
- [x] Lean evidence for ILL_POSED rows M1, M6, M17
- [x] SymPy replaced by Lean throughout the micro ledger (M1, M4, M5, M6, M17)

## References

- [ ] Lean implementation for every cited paper (rule of 10 Oct 2026, empirical papers included). Done: Borjas (1992), Cunha and Heckman (2007), Gibbons and Waldman (1999), in `lean/Lit/`
- [ ] Confirm `math_content` for Loury (1977), Altonji and Pierret (2001)

## Deferred to macro sync

- [ ] M18: the risk-premium coefficient scales with $m^2$, so $p_L, p_H$ vary with $\mu$; the macro model treats them as constants

## Differences from displacement_upskilling_v4.tex (sanity check, 10 Oct 2026)

- Ladder: v4 uses $q=i/(N+1)$, the same repair as M2; M2–M4 apply to v4
- Lottery: v4 is a two-agent pool won with probability 1/2 and no entry cost; the micro model is a uniform swap among $N$ workers with cost $L$
- Human capital: v4 has $f(\kappa)=\kappa^\gamma$, depreciation and a subsistence floor; the micro model has an upward-move cost only
- Utility: v4 gives each group its own quadratic utility around subsistence ($\theta_L>0>\theta_H$); the micro appendix assumes identical S-shaped preferences
- v4 inconsistencies found: $\gamma\in(0,1)$ vs $\gamma=2$; $\lambda^*=\kappa^{\min}/M$ is the floor, not the optimum (argmax $\lambda=1$ at the test point); $\mu$ integral needs $\alpha>1$; $W\ge1$ is a barrier choice; maintenance cost treated as linear despite $\xi>1$
- v4 $\Phi(\mu)$ equals the firmworkers $\varphi(\mu)$ under v4's $\lambda^*$ (SymPy identity)
