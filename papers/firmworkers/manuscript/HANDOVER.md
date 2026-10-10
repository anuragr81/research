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
| `check.py` | validator. `python3 check.py --lean` re-verifies every Lean claim via `#print axioms` |
| `build_manuscript.py` | writes `manuscript.tex` |
| `build_map.py` | writes `measurement_map.tex` |
| `manuscript.pdf`, `measurement_map.pdf` | compiled with `xelatex` twice each |
| `lean/` | Lean project. `IscLean/Lottery.lean`, `IscLean/Ladder.lean`, `IscLean/Gap.lean` |
| `sympy/micro_checks.py` | SymPy evidence for M1, M4 (cross-check), M5, M6, M17 |
| `sources/` | plain text of primary PDFs as `L<n>.txt`, used by VERBATIM checks. Empty so far |
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

`Real.rpow_lt_rpow_of_exponent_neg`, `Real.rpow_lt_rpow_of_exponent_gt`, `Real.add_rpow_le_rpow_add`, `Real.rpow_add_le_add_rpow`, `Real.rpow_def_of_pos`, `Real.one_lt_exp_iff` (not `Real.one_lt_exp`), `Real.exp_lt_exp`, `Real.exp_le_exp`, `Real.log_nonpos`, `Real.log_lt_log`, `one_div_lt_one_div_of_lt`, `Equiv.sum_comp`, `Finset.add_sum_erase`, `Finset.card_erase_of_mem`, `Finset.sum_div` (needs `import Mathlib.Algebra.BigOperators.Field`), `lt_div_iff₀`.

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
| M15 | `Isc.taylor_expectation` (finite support only) | Lottery.lean |
| M16 | `Isc.premium_pos_iff` | Lottery.lean |
| M18 | `Isc.premium_scales_with_income` | Lottery.lean |

Other statuses: M1, M6, M17 `ILL_POSED` (SymPy evidence). M5 `REFUTED` by M3 and M4. M14, M19 `UNDERSPECIFIED`. M20 to M23 `OPEN`.

## 6. Lean work queue, in priority order

1. Give the SymPy-only rows Lean evidence, so each row has one canonical verifier, Lean where it reaches:
   - M1: for every $i \ge 1$, $1 - i \le 0$, so $(1-i)^{-1/a}$ is not a positive real power. State the exact property used.
   - M6: on the printed charged branch $m_k - m_i < 0$. State it in Lean as a sign fact. Do not attempt complex powers unless asked.
   - M17: a variance $\ge 0$ cannot equal $p(1-\lambda)^2$ with $p<0$ and $\lambda<1$.
   - After each, keep the SymPy function as a cross-check or remove it. Ask the owner which.
2. M15 for general distributions. A measure-theoretic version with `MeasureTheory.integral` linearity, under explicit integrability hypotheses. Keep the finite-support theorem.
3. Literature papers with mathematical content (references table, all `NOT_STARTED`). The owner plans this for a later pass. Do not start without being asked.
4. Micro-model extensions (rank plus human capital state, rank kernel, categorical friction σ). Blocked on the owner's decisions. Do not choose them.

## 7. Workflow for adding or changing a Lean claim

1. Add the theorem to the right file under `namespace Isc`. No comments.
2. In `claims.yaml` set the row's `lean:` field to the full name, point `evidence:` at `lean/IscLean/<File>.lean::Isc.<name>`, and set `status: LEAN_WRITTEN`.
3. `lake build`. Then `python3 check.py --lean`. The checker refuses `LEAN_WRITTEN` once a theorem is sorry-free and demands `LEAN_PROVED`. Promote it.
4. If a new file is added, add it to `LEAN_FILES` in `build_manuscript.py` and to `lean/IscLean.lean`.
5. `python3 build_manuscript.py && python3 build_map.py`, then `xelatex` each twice. Render to PNG and inspect the changed pages.
6. Purge `__pycache__`. Package one zip.

## 8. Checker rules (all mutation-tested)

- Lean claims must name a declared theorem. Axiom probe decides `LEAN_PROVED` vs `LEAN_WRITTEN`.
- `SYMPY`, `ILL_POSED` and `REFUTED` rows citing `micro_checks.py::fn` must PASS that function.
- `REFUTED` must cite refuting M claims that are proved or checked.
- `VERBATIM` literature rows need a quote, a page and `sources/L<n>.txt` containing the quote.
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
