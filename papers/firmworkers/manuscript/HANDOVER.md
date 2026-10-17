# Handover for a Lean session — Inequality and Social Contests

Date: 10 October 2026. Owner: Anurag Srivastava. Read this whole file before touching anything.

## 1. What this project is

A claims-table manuscript for the paper "Inequality and Social Contests". The manuscript has four sections, each a table of claims: headline claims, model claims, literature claims, concluding remarks. A references table and appendices follow. Model claims are to be proved in Lean 4. Literature claims are verified by verbatim quotes from primary texts. Headline and concluding claims may rest only on model claims (M), literature claims (L) and measurement-map rows (X).

A second, independent document, the measurement map, maps every micro-model symbol to its real-world referent.

Scope today: micro model only (the N-player appendix of the source manuscript). The macro model is deferred until the micro model is settled.

## 2. Standing rules from the owner

- No comments in code: no `#`, no docstrings, no `--` explanation blocks in Lean. Identifiers carry meaning. Explanations go in the response, never in files.
- Proofs in the manuscript are Lean only. Do not write informal proofs. The owner converts Lean to informal proofs later.
- Do not draft paper prose. Do not add headline or concluding claims. Suggest only when asked.
- Untagged messages mean explain only. `<edit>` means hand back content. `<job>` means edit files directly, verified.
- One self-contained bundle per delivery. No overlays, no loose files, no duplicate copies of a file.
- Claim IDs (M*, L*, X*) are permanent. Never renumber.
- Never report verification infrastructure as an achievement. Report only substantive results.
- When a Mathlib lemma name is not confirmed by grep against the pinned Mathlib source, say so. Never guess a third name after two failures. Leave an honest `sorry` and report it.
- When a fetch or search returns nothing, say so. Never reconstruct source content.
- Verify a new check by mutation before trusting it.
- For LaTeX tables, render pages to PNG and inspect. Overfull warnings do not localise problems.

## 3. Files

| Path | Role |
|---|---|
| `claims.yaml` | single source for headline, model, literature, concluding claims and references |
| `measurement_map.yaml` | X rows: symbol, role, meaning, referent, support, direction |
| `check.py` | validator. `python3 check.py --lean` audits every declared model theorem via `#print axioms` |
| `verify.sh` | builds Lean, runs `check.py --lean` and `lit/verify_lit.sh`, writes `VERIFICATION.md` (generated, never edit) |
| `refs.bib` | bibliography for literature rows read in full |
| `lit/` | one record per paper read, see `lit/README.md`; Lean in `lean/Lit/<Paper>.lean` (library `Lit`) |
| `build_manuscript.py` | writes `manuscript.tex` |
| `build_map.py` | writes `measurement_map.tex` |
| `manuscript.pdf`, `measurement_map.pdf` | compiled with `xelatex` twice each |
| `lean/` | Lean project. `IscLean/Lottery.lean`, `IscLean/Ladder.lean`, `IscLean/Gap.lean`, `IscLean/Moments.lean` |
| `~/.cache/firmworkers/` | source PDFs and extracted text, outside the repo, pinned by sha256 in each record's suite |
| `TODO.md` | pending items, including differences from `displacement_upskilling_v4.tex` |
| `inputs/firmworkers_model.pdf` | source manuscript, 15 March 2026. Model claim anchors quote its appendix |
| `inputs/firmworkers_corrected.tex` | corrected LaTeX of the macro model from an earlier session |
| `inputs/inequality_social_contests_sympy_v2.py` | SymPy checks of the macro model, 49/49 passing. Macro is deferred |
| `inputs/displacement_upskilling_v4.tex` | an alternative three-player model, supplied only as a sanity check. Not part of the ledger |

## 4. Lean environment

- Toolchain `leanprover/lean4:v4.21.0`. Mathlib pinned in `lake-manifest.json` to tag `v4.21.0`, commit `308445d7985027f538e281e18df29ca16ede2ba3`.
- On a normal machine: `cd lean && lake exe cache get && lake build`.
- In the Claude cloud sandbox the Mathlib cache hosts and `release.lean-lang.org` are blocked by the proxy. What worked:
  1. Download `https://github.com/leanprover/lean4/releases/download/v4.21.0/lean-4.21.0-linux.tar.zst`. Unpack with Python `zstandard` (no `unzstd` binary).
  2. `git clone --depth 1 --branch v4.21.0 https://github.com/leanprover-community/mathlib4.git /opt/mathlib4`.
  3. Symlink `lean/.lake/packages/mathlib -> /opt/mathlib4`. The git manifest then resolves without fetching.
  4. Set `MATHLIB_NO_CACHE_ON_UPDATE=1`. Never run `lake update` without it in the sandbox, because the cache hook retries thousands of times.
  5. Build only the needed targets. `IscLean.Lottery` needs about 940 modules. `IscLean.Ladder` and `IscLean.Gap` need about 1,860 (`Mathlib.Analysis.MeanInequalitiesPow`). On 2 cores the full build took about 2.5 hours.
- Check axioms: `check.py --lean` writes a temporary `AxiomProbe.lean` importing each module and prints `#print axioms` for every claimed theorem. A claim is `LEAN_PROVED` only if `sorryAx` is absent.

### Lemma names confirmed against Mathlib v4.21.0

`Real.rpow_lt_rpow_of_exponent_neg`, `Real.rpow_lt_rpow_of_exponent_gt`, `Real.add_rpow_le_rpow_add`, `Real.rpow_add_le_add_rpow`, `Real.rpow_def_of_pos`, `Real.one_lt_exp_iff` (not `Real.one_lt_exp`), `Real.exp_lt_exp`, `Real.exp_le_exp`, `Real.log_nonpos`, `Real.log_lt_log`, `one_div_lt_one_div_of_lt`, `Equiv.sum_comp`, `Finset.add_sum_erase`, `Finset.card_erase_of_mem`, `Finset.sum_div` (needs `import Mathlib.Algebra.BigOperators.Field`), `lt_div_iff₀`, `MeasureTheory.integral_add`, `MeasureTheory.integral_const`, `MeasureTheory.integral_const_mul`, `MeasureTheory.integrable_const`, `MeasureTheory.Integrable.const_mul`, `MeasureTheory.measureReal_univ_eq_one`, `ProbabilityTheory.variance_nonneg`.

## 5. Lean status

Thirteen model claims are `LEAN_PROVED`, all axiom-clean (`propext`, `Classical.choice`, `Quot.sound` only):

| Claim | Theorem | File |
|---|---|---|
| M2 | `Isc.pareto_strictMono_quantile` | Ladder.lean |
| M3 | `Isc.pareto_strictAnti_tail` | Ladder.lean |
| M4 | `Isc.pareto_gap_strictAnti_tail` | Gap.lean |
| M7 | `Isc.upCost_down_free` | Ladder.lean |
| M8 | `Isc.upCost_steps_le_jump` | Ladder.lean |
| M9 | `Isc.upCost_jump_le_steps` | Ladder.lean |
| M10 | `Isc.swap_preserves_total` | Lottery.lean |
| M11 | `Isc.lotteryGain_closed` | Lottery.lean |
| M12 | `Isc.lotteryGain_total` | Lottery.lean |
| M13 | `Isc.lotteryGain_pos_iff` | Lottery.lean |
| M15 | `Isc.taylor_expectation_integral` (probability measure, integrability hypotheses); finite-support case `Isc.taylor_expectation` kept in Lottery.lean | Moments.lean |
| M16 | `Isc.premium_pos_iff` | Lottery.lean |
| M18 | `Isc.premium_scales_with_income` | Lottery.lean |

M14 is `DECIDED` (entrant pool, 10 Oct 2026; Appendix D shows the dated decision); M11–M13 are over the pool and M24 proves risk-neutral unravelling against keeping one's job; M30–M31 (`IscLean/Unravelling.lean`, 12 Oct 2026) extend it to every strictly increasing $u$ and to closure. M32 is `DECIDED` (12 Oct 2026): entrants are displaced workers with fallback $x_0$. M33–M35 (`IscLean/Displacement.lean`): a displaced pool can be stable for every nondecreasing $u$; lottery-against-downgrade sorting by rank holds for every nondecreasing $u$ but shrinks as $1/(|P|-1)$; the return route beats the downgrade iff the loss is under one income unit when $b>1$, so the cost of M7 needs a scale (open for the author). Specific forms of $u$ go in a separate sub-section with reference $R_h$, the income norm of the own status group. M1, M6, M17 stay `ILL_POSED` (the printed claim is ill-posed) with Lean evidence, axiom-clean: M1 `Isc.printed_index_base_nonpos` and M6 `Isc.printed_charged_branch_base_neg` (Ladder.lean), M17 `Isc.variance_ne_neg_multiple` (Moments.lean). `check.py --lean` probes every row with a `lean:` field, whatever its status, and fails if the theorem is undeclared, uncompiled or uses `sorryAx` (mutation-tested). M5 `REFUTED` cites `Isc.pareto_top_decile` ($x(0.9)/x_0=10^{1/a}$, exact) besides M3 and M4. M6's theorem also proves no real number squares to $m_k-m_i$, which replaces the SymPy complex-branch check. SymPy was removed from the ledger on 10 Oct 2026: no `SYMPY` status, no `sympy/` folder, no SymPy appendix. Every model row with formal evidence is Lean.

Other statuses: M5 `REFUTED` by M3 and M4. M14, M19 `UNDERSPECIFIED`. M20 to M23 `OPEN`.

## 6. Lean work queue, in priority order

1. Done 10 Oct 2026: Lean evidence for M1, M6, M17; M15 for general distributions. SymPy removed from the micro ledger.
2. Literature records, started 10 Oct 2026 at the owner's request: every cited paper gets a `lit/` record with Lean, committed and pushed. Done: Borjas (1992); Cunha–Heckman and Gibbons–Waldman from their NBER working papers (cited as the working papers, author's decision of 10 Oct 2026; `version_read` remains available for future rows read in a version other than the one cited). Rule of 10 Oct 2026: primary sources are needed only for papers a claim rests on or a novelty check needs; exploration rows stay at abstract level and `check.py` rejects any claim resting on a row not read in full. Model anchors are checked verbatim against `inputs/firmworkers_model.pdf` (sha256 pinned). Every measurement-map row with a referent carries `observation` (how an observer could obtain it) and `observation_status` (PROPOSED until the owner confirms); a row may be `pending` on an undecided model claim (X8 on M14), and a headline may not rest on a pending row. Since 11 Oct 2026 each observation also carries `observation_support`: PROVEN (with `observation_lean`, the Lean theorems showing the observed quantity determines the object, mostly in `IscLean/Identification.lean`), DEFINITIONAL, LITERATURE (with `observation_literature`, VERBATIM rows only), CONSTRUCT or UNVERIFIED; a headline may not rest on an UNVERIFIED observation and gets a note on a CONSTRUCT. See TODO.md, Literature by purpose. Gibbons–Waldman is a scan; its text is Drive's OCR, pinned by sha256. Cole–Mailath–Postlewaite (1992, 1995) read 11 Oct 2026 as the formal reference for $h$ as inherited social standing (`lit/cole_mailath_postlewaite`); the entry_contest record of the same papers covers §IV.A and Property 1, this one covers §III.D. Sources are in the shared Drive folder `1r7J3xJ2rotpmlRaBANCNhviy2d8a1bx_` (page through the whole listing before asking the owner for a paper). On 12 Oct 2026 four novelty records for the reference-dependent sub-section were added: Robson (1992) L16, Becker–Murphy–Werning (2005, read in the authors' 2003 manuscript) L17, Ray–Robson (2012) L18, Kőszegi–Rabin (2006) L19. Later the same day: Genicot–Ray (2017) L20, Kahneman–Tversky (1979) L21, Friedman–Savage (1948) L22, Markowitz (1952) L23. Scans without a usable text layer are read through a local OCR made by `lit/ocr_pages.py` (rapidocr-onnxruntime 1.2.3 in a scratch virtual environment, never the user site; `single` or `columns` mode), which reproduces the cached text page for page. Third pass, 12 Oct 2026: Currarini–Jackson–Pin (2009, working paper) L24, Clark–Oswald (1996, 1995 revision) L25, Jacobson–LaLonde–Sullivan (1993, Chicago Fed WP-92-28) L26, Lazear–Rosen (1981) L27, Hopkins–Kornienko (2004) L28. The last two were read afresh, not copied from entry_contest (whose records serve that paper and use SymPy). Two quotation-matching variants were added to `lit/README.md` rule 3: spaces removed where the text layer drops them (Lazear–Rosen, the JLS footnotes), and several renderings where the text layer mixes columns (Hopkins–Kornienko: layout, reading order, columns rebuilt from word boxes by `lit/bbox_columns.py`, local OCR). Findings for the owner from this pass: JLS report losses as about 25 per cent of former earnings, closer to a proportional step than to M32's common fallback (flag only; M32 stays DECIDED), and support "no unemployment spell" only within their every-year-earnings sample; Lazear–Rosen's wealth sorting runs the opposite way to M34 (richer workers take the gamble, through DARA, and only in an example their own approximations do not produce); Hopkins–Kornienko are the closest precedent for the source's question on inequality and investment, and their exogenous-status benchmark supports the reading of $h$. Candidates named by these papers, not on Drive or not triaged: Friedman (1953, JPE 61(4)), a direct novelty threat for the reference-dependent sub-section; Cooper, García-Peñalosa and Funk (2001, EJ), status with interaction only within the own wealth group. All literature rows planned before the reassessment are now recorded.
3. Micro review, 12 Oct 2026 (owner's request: judge the source's micro claims by micro dynamics alone, macro deferred): `MICRO_REVIEW.md` gives a verdict per source claim, resting on `lean/IscLean/Routes.lean` (one-shot route choice of a displaced worker; 28 theorems, audited by `check.py --lean`, not yet cited by rows nor listed in the manuscript). Main results: M20 fails under M32's bottom rung for every increasing $u$ and holds under a proportional step; M21's cost part fails under the return-to-own-job reading (the costly route is the immobile one); "lottery play rises with inequality" is false in general (exact counterexample at $b=21/20$) and true at the top and below the pool mean; M22 holds reading upskilled as higher rank. New open issue 10: the reading of route (ii). Ledger changes are proposed, not applied, pending discussion.
4. Plain-English layer, 12 Oct 2026 (owner's request): a 300-word model summary in `claims.yaml` (`summary`, one entry per sentence with the rows, Lean theorems or verbatim source premise it rests on), rendered at the head of the Model claims section; a Terminology section in the measurement map (a `term` for every X row, a list `terms` for objects without a row, an `avoid` list each, all PROPOSED until the owner agrees). `check.py` enforces the summary's grounds, length (270 to 310 words), surface rules (no colons, dashes or symbols) and the avoided names, and lists the ledger rows that still use avoided names (nine, including M28's "human capital" for the status group); mutation-tested. `notes/writing_discipline.md` is the newest discipline (from wealth_garch) with a firmworkers rule 6.
4a. The firm and the cause of the lottery, 13 Oct 2026 (owner's discussion). The lottery stays the connections channel the firm does not manage (source line 110), paid by workers (line 158); the firm ranks workers by perceived earning potential (line 96), for which L2 (Gibbons–Waldman, GW-9) is the read precedent. Owner's decisions in `TODO.md`: pay tied to results excluded; wage pass-through and learning over a career are the grounds for enhancement; the principle "the firm wants workers to upskill rather than choose the lottery" (lines 126, 130, 258) is to be highlighted as the justification of the firm's incentive. Open choices are ISSUES issue 11a–e (what a displaced worker keeps, with `δ_p` as the loss of firm-specific skills resting on JLS-3, JLS-4, JLS-43; pass-through `θ`; the firm's instruments, proposal `a` only with the wage bill fixed; the lottery cost as a share or a fixed amount; the firm's micro objective). Correction found the same day: under a scale-free cost and `b > 1` the return beats a proportional step at every rank (`scale_free_return_beats_step`), so taking `κ = 1` as a scale-free cost would lose the source's M20 pattern, which needs the unit-dependent cost. The skeleton has a section Idea summaries (`claims.yaml`, `idea_summaries`: id, title, word range, sentences with `rests_on`), rendered after Model claims; the first, `lottery_cause` (180 to 220 words), states why displaced workers choose the lottery: the bottom income, not a taste for risk. It was revised against the writing discipline the same day (conditions carried, referents named, one name for the income climbed). Summaries stay in step with the claims: `summary_stamps.yaml` fingerprints every row (status, claim, lean, decision, quote, page) and theorem statement each summary cites, and `check.py` fails when one changes or a new one is cited, until the summary is reviewed and `check.py --restamp` is run; mutation-tested. `Routes.lean` now has 31 theorems (`cost_exceeds_regain_upward`, `scale_free_return_beats_step`, `control_step_beats_scale_free_return_below_unit` added).
4b. The return defined, 14 Oct 2026 (owner's decision): the term rested only on a result (M35) and no decided row defined it. M38 (DECIDED) defines it: the job survives displacement vacant, the climb starts at the income of the downgrade, the firm receives the payment. Map row X27 carries the term (observation UNVERIFIED; recall to the former employer is the nearest measured event, unread leads); 'displaced worker' now says the job stays on the ladder. Both summaries cite M38 and were re-stamped after review.
4c. Players, 14 Oct 2026 (owner's request): `players`, `choices` and `choice_status` in `claims.yaml`, rendered as the sub-section Players of Model claims; checked by `check_players` (each player a term, each choice for a listed player with a status, every cell grounded and free of avoided terms) and stamped as block `players`; mutation-tested. New terms 'firm' and 'job holder'. As it stands, only the displaced worker chooses in the micro model; the firm's micro choice is PROPOSED (issues 11c, 11e) and the source's macro choices are DEFERRED.
4d. Steady state, 14 Oct 2026 (owner's decision): one worker type in two states, uniform displacement at rate `d`, the steady state as the object, routes compared by lifetime value. `lean/IscLean/SteadyState.lean` (M39 DECIDED with `holdValue_recursion`; M40 the lifetime return threshold with multiplier `1/(1 − β(1 − d)) ≥ 1`; M41 M33 carried over with a one-time lottery cost, the top case and the cost ratio rising with rank). New X28 (`d`, UNVERIFIED) and X29 (`β`, CONSTRUCT); terms worker, lifetime value, lifetime multiplier; job holder and displaced worker are states. Players table rewritten (worker, firm). Summaries revised and re-stamped. `MICRO_REVIEW.md` pass 2 lists what carries over and what must be redone (the inequality counterexample). ISSUES 12 (refill rule) and 13 (costs once or per period). `Routes.lean` and `SteadyState.lean` now in the manuscript's Lean listings.
4e. Mobility test, 15 Oct 2026 (owner: test before deciding issue 12, since a rule that closes the bottom-to-top path would build in the immobility the paper wants to explain through closure). `lean/IscLean/Mobility.lean` (10 theorems, audited): in a two-entrant pool the bottom rung keeps the path open for every nondecreasing `u` (the top accepts any entrant at least `L` above `x_0`; any rank at least `L` below the top accepts the top), the proportional step closes it for every rank below the top's reduced pay, and under either rule a rank that always returns is sealed. Results in `MICRO_REVIEW.md`, Mobility test. No decision taken; issue 12 and 11a remain open, with this evidence. Line 375 of the source ("churning at the bottom and stickiness at the top") is the author's reading of simulations, not a feature to build in.
4f. Base case, 15 Oct 2026 (owner's request): the two-entrant test became the sub-section 'Base case, two displaced workers' at the head of Model claims, after Players. `base_case` in `claims.yaml` (title, rows, word range, summary, payoff table), checked by `check_base_case` and stamped as `base_case` and `base_case:payoffs`; mutation-tested. Rows M42 (exchange), M43 (both accept under the bottom rung, every nondecreasing u), M44 (the proportional step refuses; why the bottom rung was kept), M45 (closure inert with two, acts with three), M46 (zero-sum, the richer loses, the poorer gains iff L < gap). `lean/IscLean/BaseCase.lean` (7 theorems). The base rows are shown in the base section and left out of 'The model in general'. Decided the same day: bottom rung kept (11a (a)), vacated jobs into the pool (12 (a)).
4g. Status group defined and the closure rule tested, 16 Oct 2026. M47 (DECIDED) defines `h` as the categorisation dimension of line 96, governing access to lottery partners and nothing else, on CMP's idea (CMP92-1, -3, -19 in the L14 record); X24 and X2 carry the rank-versus-status contrast; the CMP 1992 reference's impact text updated since M47 rests on it. Tests of option (a) for M29 in `lean/IscLean/TwoStage.lean` (M48, M49, 11 theorems): closed forms for the within-group and no-crossing chances in the two-and-two pool, limits, monotonicity, the second-order finding and the crossover with M29's weights, the counts 9 and 1 by exhaustion. A false claim (that (a) is more closed than the weights at every interior σ) was caught by Lean and replaced. Finding for the a-versus-c discussion: with identical income distributions no symmetric rule changes the lottery's value by group, so closure's effect is the persistence of an initial gap between the groups' positions, which (a) measures and (c) cannot for unequal pools.
4h. The groups' structure under option (a), 17 Oct 2026: M50 (any reassignment conserves each group's job count) and M51 (expected-gap recursion in the two-and-two pool, `ρ(σ)` from `−1/3` to `1`, resting on the derangement fibre counts by exhaustion). Large-pool half-life `ln 2/((1 − σ)d)` derived in MICRO_REVIEW, not in Lean. `TwoStage.lean` now 22 theorems.
5. Micro-model extensions, decided 11 Oct 2026: $h$ is a categorical status label fixed within a working life (M28), friction is one symmetric closure parameter σ on partner draws in the entrant pool (M29, limitations in the decision), results M25–M27 in `IscLean/Closure.lean`. Weber and Tilly are exploration only. Still open: Q7, Q8.

## 7. Workflow for adding or changing a Lean claim

1. Add the theorem to the right file under `namespace Isc`. No comments.
2. In `claims.yaml` set the row's `lean:` field to the full name, point `evidence:` at `lean/IscLean/<File>.lean::Isc.<name>`, and set `status: LEAN_WRITTEN`.
3. `lake build`. Then `python3 check.py --lean`. The checker refuses `LEAN_WRITTEN` once a theorem is sorry-free and demands `LEAN_PROVED`. Promote it.
4. If a new file is added, add it to `LEAN_FILES` in `build_manuscript.py` and to `lean/IscLean.lean`.
5. `python3 build_manuscript.py && python3 build_map.py`, then `xelatex` each twice. Render to PNG and inspect the changed pages.
6. Purge `__pycache__`. Package one zip.

## 8. Checker rules (all mutation-tested)

- Lean claims must name a declared theorem. Axiom probe decides `LEAN_PROVED` vs `LEAN_WRITTEN`.
- Any row with a `lean:` field, whatever its status, must name a declared theorem that compiles without `sorryAx` under `--lean`.
- `lean:` may be one name or a list. Every name is checked.
- Placement by status (10 Oct 2026, following `papers/MANUSCRIPT_SKELETON.md`). `LEAN_PROVED`/`LEAN_WRITTEN` rows form the model table. `OPEN` rows go to the Pending table under Concluding remarks and need `waits_on` (optional `rows`). `UNDERSPECIFIED` rows go to Appendix D and need `affects`. `ILL_POSED` and `REFUTED` rows go to Appendix E and need a Lean counterexample; `ILL_POSED` also needs `repaired_by` naming proved rows. A placement field on a row of another status is an error, so promoting a row forces its old fields out. Every rule mutation-tested.
- `REFUTED` must cite refuting M claims that are proved or checked.
- `VERBATIM` literature rows need a quote and page, the key in `refs.bib`, a `lit/` record whose `KEYS` lists it, and the quote on that page of the record's `CLAIMS.md` quotation table.
- References may not be `NOT_APPLICABLE` (every cited paper needs Lean, 10 Oct 2026). `LEAN_PARTIAL`/`LEAN_PROVED` need a record whose `LEAN` names an existing file; a paper read in full must have one of them.
- `--lean` audits every declared theorem of each `IscLean` module: coverage must be complete and no axiom outside `propext`, `Classical.choice`, `Quot.sound` (`sorryAx` only for theorems of `LEAN_WRITTEN` rows). Every model Lean file must declare a `control_*` theorem.
- Every model claim declares `symbols` (X ids). Every X row is used by some claim. Output rows name `produced_by` claims that declare them.
- Headline and concluding claims rest only on M, L or X ids. Resting on an `OPEN` model claim is an error. Resting on a `POSIT` row is flagged.
- References: every L row links to a reference key. `impact_if_removed` must begin "No current claim rests on this paper" exactly when the computed cited-by list is empty.

## 9. Decisions only the owner can make

- Micro state: rank plus human capital $(r,h)$, and the law of motion for $h$.
- Where categorical friction σ enters: accumulation of $h$, the rank kernel, or both.
- Rank kernel (M23): global or reference-set rank; channels (effort, propagation, observer learning); where σ enters.
- M14: swap partner drawn from lottery players or from all workers.
- M19 and M20: functional form of the S-shaped utility and the reference income.
- Which headline and concluding claims to admit.
- Whether any part of `displacement_upskilling_v4.tex` enters the ledger. Currently it does not.
