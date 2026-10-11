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

M14 is `DECIDED` (entrant pool, 10 Oct 2026; Appendix D shows the dated decision); M11–M13 are over the pool and M24 proves risk-neutral unravelling. M1, M6, M17 stay `ILL_POSED` (the printed claim is ill-posed) with Lean evidence, axiom-clean: M1 `Isc.printed_index_base_nonpos` and M6 `Isc.printed_charged_branch_base_neg` (Ladder.lean), M17 `Isc.variance_ne_neg_multiple` (Moments.lean). `check.py --lean` probes every row with a `lean:` field, whatever its status, and fails if the theorem is undeclared, uncompiled or uses `sorryAx` (mutation-tested). M5 `REFUTED` cites `Isc.pareto_top_decile` ($x(0.9)/x_0=10^{1/a}$, exact) besides M3 and M4. M6's theorem also proves no real number squares to $m_k-m_i$, which replaces the SymPy complex-branch check. SymPy was removed from the ledger on 10 Oct 2026: no `SYMPY` status, no `sympy/` folder, no SymPy appendix. Every model row with formal evidence is Lean.

Other statuses: M5 `REFUTED` by M3 and M4. M14, M19 `UNDERSPECIFIED`. M20 to M23 `OPEN`.

## 6. Lean work queue, in priority order

1. Done 10 Oct 2026: Lean evidence for M1, M6, M17; M15 for general distributions. SymPy removed from the micro ledger.
2. Literature records, started 10 Oct 2026 at the owner's request: every cited paper gets a `lit/` record with Lean, committed and pushed. Done: Borjas (1992); Cunha–Heckman and Gibbons–Waldman from their NBER working papers (cited as the working papers, author's decision of 10 Oct 2026; `version_read` remains available for future rows read in a version other than the one cited). Rule of 10 Oct 2026: primary sources are needed only for papers a claim rests on or a novelty check needs; exploration rows stay at abstract level and `check.py` rejects any claim resting on a row not read in full. Model anchors are checked verbatim against `inputs/firmworkers_model.pdf` (sha256 pinned). Every measurement-map row with a referent carries `observation` (how an observer could obtain it) and `observation_status` (PROPOSED until the owner confirms); a row may be `pending` on an undecided model claim (X8 on M14), and a headline may not rest on a pending row. See TODO.md, Literature by purpose. Gibbons–Waldman is a scan; its text is Drive's OCR, pinned by sha256. Cole–Mailath–Postlewaite (1992, 1995) read 11 Oct 2026 as the formal reference for $h$ as inherited social standing (`lit/cole_mailath_postlewaite`); the entry_contest record of the same papers covers §IV.A and Property 1, this one covers §III.D. Sources are in the shared Drive folder `1r7J3xJ2rotpmlRaBANCNhviy2d8a1bx_`.
3. Micro-model extensions, decided 11 Oct 2026: $h$ is a categorical status label fixed within a working life (M28), friction is one symmetric closure parameter σ on partner draws in the entrant pool (M29, limitations in the decision), results M25–M27 in `IscLean/Closure.lean`. Weber and Tilly are exploration only. Still open: Q7, Q8.

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
