# asymmetric_capital_control -- v2 (complete, pruned) -- 10 Aug 2026

## Start here

`MANUSCRIPT.tex` is the canonical statement of the theory: headlines, model
rows each proved in Lean (`lean/mathlib/`), literature rows each quoting a
paper read (`lit/`), and conclusions. `./verify.sh` checks it and writes
`VERIFICATION.md`; `lit/verify_lit.sh` checks the literature records;
`MEASUREMENT_MAP.tex` says how each model object is observed, against the
Basel standards and the Call Report (`checks/verify_measurement.py`);
`TODO.md` holds the plan and the author's decisions. `PROOFS_v2.tex` was
retired into it on 10 Oct 2026; `RETIREMENT.md` maps every part of it to its
new home. The rest of this file describes the numerical and empirical layer.

This is the whole surviving project in one bundle: the clean document, every
canonical verifier including the Lean files, the pruned numerical layer, the
empirical layer with your solved sweep results, and a harness that runs
everything a plain machine can run. Nothing historical is in here -- the
original bundle remains available separately as a frozen archive.

Selection rules this tree satisfies: between entities covering the same
ground, only the one that can be proven is kept; one canonical verifier per
claim (Lean where it exists, SymPy otherwise); no retractions, supersessions,
or correction narratives anywhere.

## BLOCKING: country universe fixed, panel not yet rerun

`FINAL_77`, the old hardcoded 77-country list, excluded roughly half of
all areas reporting the capital ratio with no recorded or reconstructable
rationale -- confirmed by `universe_diagnostic.py` (75 available areas
never queried, several with more history than included countries) and by
`universe_provenance_test.py` (ruled out the hypothesis that it was an
inherited data requirement from a different test: FSKRC_PT and FSERE_PT
are reported by the identical 152 areas). No further reconstruction was
attempted; per instruction, it was replaced rather than explained.

`03_empirical/design/panel_universe.py` is the replacement, imported by
both `expanding_mean_panel.py` and `segmented_panel.py`. The rule: every
sovereign or dependent-territory area (ISO 3166-1 alpha-2, plus `XK` as a
documented named exception already relied on by this project) that the
source reports for the indicator. No history-based pre-screen -- folding
feasibility into universe construction is what made `FINAL_77`
unauditable in the first place. Feasibility is applied and reported as
attrition at the estimation stage, exactly as `BURNIN + 2*MIN_OBS`
already worked inside the panel script. Non-sovereign/aggregate codes
(e.g. `5Y`) are excluded by name, not by suspicion. Self-tested against
the actual 152-area fetch from 11 Aug 2026 (`python3 panel_universe.py`).

`expanding_mean_panel.py` now separates the two `MIN_OBS=15`-adjacent
exclusion mechanisms -- structural (zero quarters in one regime,
unfixable by threshold choice) versus near-miss (one or more quarters
short of the cutoff) -- rather than reporting one undifferentiated
"degenerate split" figure.

The Eastern Caribbean- and CEMAC-adjacent clusters, provisionally
flagged because their members returned near-identical history lengths,
were tested with `panel_currency_union_check.py` and the suspicion was
REJECTED: zero numerically identical quarters across all 43 pairs, with
correlations scattered from -0.77 to +0.74 and many negative. They are
independent national series whose sample windows begin together. The
flag has been removed and the result recorded as a documented
non-exclusion in `empirical_design_exclusions.tex`, which now also
carries the universe rule itself as a stated section rather than leaving
it in this README alone.

**Still required, and gated in `ledger_empirical.py`
(`PANEL_RERUN_ON_CORRECTED_UNIVERSE_REFLECTED_IN_DOCUMENT`, currently
`False`):** none of this has been run against real data in this sandbox
(`db.nomics` is unreachable here). `EMPIRICAL_v2.tex` Section 3 is marked
stale in-document and must not be cited until `expanding_mean_panel.py`
is rerun on the corrected universe and every affected figure -- the
country table, the median/mean/IQR, the attrition breakdown, the
near-miss/structural split, the currency-union check's verdict -- is
updated to match. Only then do the two flags flip to `True`.

## Layout

    00_document/   EMPIRICAL_v2.tex/.pdf -- the empirical companion:
                     estimator properties, the lambda_V(lambda_S) null,
                     the pre-registered panel as run (median 1.040, n=26,
                     attrition stated), and the specified-but-pending
                     secondary reference analysis.
    01_theory/     canonical verifiers
                   RateBased.lean        Lean: Props 1-2, Thm 1
                   QVI_Part1.lean        Lean: Props 6-7
                   verify_rate_based.py  SymPy: Phase 1 remainder (Props 3-5)
                   verify_state_map.py   SymPy: Props 8, 10, Cor 1 (M1-M10)
                   verify_saturation_limits.py  SymPy: Prop 11 (L1-L3, L5*)
                   verify_egarch_recovery.py    provisional -- see below
    02_numerical/  the pruned octave layer (26 files)
                   solver call path (10): solve_bank_qvi_lambda_M + its
                     dependencies, incl. thomas_solve
                   finding generators (5): headless_lambda_single_M,
                     headless_lambda_converge_check_M (docstring's usage
                     line was wrong -- named a nonexistent file, a
                     copy-paste leftover from its H-operator sibling;
                     fixed to name itself), headless_M_refine,
                     headless_K_single, headless_K_refine
                   validation chain for Rem 1 (7): solve_bank_qvi,
                     solve_bank_qvi_lambda, compute_H_operator,
                     headless_repro, headless_lambda_nesting,
                     compute_metrics, compute_value_bounds
                   new (4): verify_M_operator.m, run_lambda_sweep.sh,
                     run_lambda_RN_curve.sh, run_convergence_recert.sh
    03_empirical/  lambda_RN_panelconv.m (the sole null routine)
                   design/   expanding_mean_panel.py + the four
                             pre-committed design scripts + the exclusions
                             pre-registration (tex+pdf)
                   results/  your six solved resultM_lambda_*.mat (+ the
                             sweep's per-run CSVs) and lambda_V_curve.csv,
                             the lambda_V(lambda_S) curve computed from them
    00_reader/     ledger_empirical.py (EMPIRICAL_v2's claims, their
                     population and support, model support resolved
                     against the rows of MANUSCRIPT.tex),
                     PITCH_AND_SUMMARY.md, README.md

    04_reproduce/  run_all.sh

## Running it

    bash 04_reproduce/run_all.sh

Current state on a machine with python3+sympy and octave:
35 pass / 0 fail / 2 expected-fail / 1 skip. The two expected-fails are
deliberate (M8* confirms the constant-volatility result is confined to the
solvency regime; L5* confirms kappa1 and kappa3 are genuinely distinct).
The skip is L4, which needs solved .mat files via $MATDATA_DIR -- point it
at 03_empirical/results/ to enable it.

Lean is NOT covered by the harness -- verify separately with a mathlib
toolchain:

    lake build      # covers RateBased.lean and QVI_Part1.lean

The heavy sweeps are also not in the harness (hours of compute). To
regenerate 03_empirical/results/ from scratch: run 02_numerical/
run_lambda_sweep.sh in a directory holding the 11 files it lists, then
run_lambda_RN_curve.sh next to lambda_RN_panelconv.m and the .mat files.

## Provenance notes

- 10 of the 25 files in 02_numerical/ are byte-identical copies from the
  benchmark's published repo (github.com/yuqiongwang/bank_capital_structure);
  the rest are project-original. They are included so the bundle is
  self-contained.
- 03_empirical/results/ holds CONVERGED solves. The first sweep, at
  max_iter=1500, stopped at the cap with converged=0 (final_err ~2e-6
  against a 1e-6 tolerance); re-solving at max_iter=3000 converges for
  every lambda_S at 1645-1669 iterations, so 1500 was marginally short.
  The converged results are stored twice here: under the plain
  resultM_lambda_<L>.mat names (variables renamed to sol/y_post/
  recap_edge, which is what lambda_RN_panelconv.m reads) and under the
  _maxiter3000 names as written by headless_lambda_converge_check_M.m
  (variables sol_new/y_post_new/recap_edge_new). 02_numerical/
  convert_converged.m performs that rename; it recomputes nothing and
  leaves lambda_RN_panelconv.m untouched. Consider raising max_iter from
  1500 to >=2000 in headless_lambda_single_M.m for future sweeps.
- verify_egarch_recovery.py is KEPT (ruling of 11 Aug 2026) and is cited
  by EMPIRICAL_v2 Section 1. It is report-style (tables, no PASS/FAIL
  tags), so run_all.sh executes it -- a crash counts as a failure -- but
  it contributes no tag counts.
- lambda_V_curve.csv: three conventions per lambda_S, computed from the
  CONVERGED solves. The two stationary-mean series decrease in lambda_S
  (slopes -0.101 and -0.129); the fixed-split-at-R series increases
  (+0.087). The empirical estimate 1.04 lies below every value in the
  table under all three conventions -- the lowest anywhere is 1.2347, a
  gap of +0.195. Recomputing on converged rather than capped solves moved
  no value by more than 0.008 and changed no sign or ordering.

## Convergence re-certification

All six sweep runs in 03_empirical/results/ report converged=0, hitting the
1500-iteration cap at final_err ~2e-6 against a 1e-6 tolerance. To check
whether that had actually settled:

    cd 02_numerical/
    cp ../03_empirical/results/resultM_lambda_*.mat .
    bash run_convergence_recert.sh              # all six, or:
    bash run_convergence_recert.sh 1.00 2.00     # a subset

Re-solves at max_iter=3000 and diffs against the existing result; verdict
per lambda_S ("STABLE" or "had NOT settled") is printed and collected in
convergence_recert_summary.txt. Costs roughly what the original sweep
did, again, for however many lambda_S are re-run. The underlying
headless_lambda_converge_check_M.m had a copy-paste bug in its own usage
comment (named a nonexistent sibling file); fixed in this bundle.

## Split convention -- resolved

expanding_mean_panel.py classifies each country's regime by
z_t = log(ratio_t) - R_t, where R_t is a ONE-SIDED EXPANDING MEAN of that
country's own history (R[t] = mean(log_ratio[:t])) -- there is no
reference to a fixed R_ref=1.15 or any fixed constant anywhere in the
file. So the panel's convention is structurally the moving-threshold
family in lambda_V_curve.csv (the DECREASING series, slope -0.10 to
-0.13), not the fixed-split-at-R family (the increasing one) -- though
not identical to either, since the panel's threshold is a per-country
expanding empirical mean, not the model's population stationary mean.

## Lean status

See `lean/mathlib/README.md` and `VERIFICATION.md`. Every file builds with
no `sorry`, and every declared theorem passes the axiom audit.

## Open items (current)

2. The Rem 1 convergence figures are measured for the M operator this
   cycle. If the H operator's certification is also to be quoted, it needs
   its own re-run.
4. The secondary reference analysis (EMPIRICAL_v2 Section 4) is specified
   but unexecuted: it must pass its three-part simulation validation --
   including the trended null -- before touching real data. The
   pre-committed phi^- test and the bank-level feasibility check from the
   exclusions pre-registration also remain unexecuted.
