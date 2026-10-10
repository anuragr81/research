# Bundle contents — asymmetric capital control (theory paper)

Self-contained. Extract into an empty directory, or over a previous copy;
either works. Nothing here is a patch or an overlay.

    tar -xzf asymmetric_capital_control_v2_<date>.tar.gz
    bash VERIFY_NOW.sh

`VERIFY_NOW.sh` writes and prints `verification_output.txt` — send that back.

## Start here

`HANDOVER_20261010.md` at the root is the entry point for the empirical
companion. It carries the established results, the settled questions that
must not be re-opened, and the one open decision.

The FDIC panel is included at
`06_empirical_starter/fdic_panel_2015_2024_ext.csv.gz` (213,771 bank-quarters),
so the two re-derivation scripts in that directory run with no further
downloads:

    cd 06_empirical_starter
    python3 rederive_model_and_data.py \
        ../03_empirical/results/resultM_lambda_1.0000_maxiter3000.mat \
        fdic_panel_2015_2024_ext.csv.gz
    python3 rederive_shape_test.py fdic_panel_2015_2024_ext.csv.gz

Note `06_empirical_starter/fdic_institutions.csv.gz` does NOT carry
`FED_RSSD`; it would need re-pulling with that field for the market-panel
route.

## Solve data — READ THIS BEFORE EXTRACTING OVER AN EXISTING COPY

`03_empirical/results/*.mat` ARE included, so the seventeen document figure
checks run out of the box. They are the same lambda_S-sweep files you
supplied (Octave 7.1.0, 10 Aug), returned unmodified.

Because they are included, extracting over a directory where you have
re-solved anything WILL OVERWRITE your newer .mat files. If you have re-run
the solver since 10 Aug, extract into an empty directory instead, or back up
`03_empirical/results/` first.

Still absent: the K-sweep (`resultK_*`) and H-convention (`resultH_*`) runs,
and any mesh other than N=301. Those are open items C1/C2/C3; the checks that
need them report SKIP with the reason.

## Layout

    00_document/    PROOFS_v2.tex (the paper), EMPIRICAL_v2.tex, references.bib
    00_reader/      TODO.md, proof_registry.py, check_citations.py,
                    ledger_empirical.py, TERMINOLOGY.md
    01_theory/      the verifiers (SymPy) and the Lean files, including
                    lean_project/ — a buildable Lake project (SMF, QVI_Part1,
                    RateBased), pinned to Lean v4.32.0-rc1 + matching
                    mathlib. Compiler-verified against that toolchain; see
                    01_theory/lean_project/README.md to rebuild with
                    `lake build`.
    02_numerical/   solver, M-operator verifier, sweep drivers
    03_empirical/   design, results/ (your .mat files go here)
    04_reproduce/   run_all.sh — the full harness
    RUN_C3.sh       one-shot mesh-refinement test for D2 (smooth fit vs
                    kink at x_L). Solves FRESH, ignores any existing
                    .mat/.csv. HOURS to run at default settings (real
                    solves at N up to 2401). Writes c3_output.txt.
    05_summaries/   SHORT_SUMMARY, EXECUTIVE_SUMMARY, THEORY_PITCH
    06_empirical_starter/
                    EMPIRICAL_CLAIM + the KSW feasibility/design scripts,
                    the FDIC panel, and the two re-derivation scripts
                    (see HANDOVER_20261010.md)
    07_correspondence/
                    RESPONSE_csl_scg_open_issue — the reply on the
                    lambda_V identification question
    VERIFY_NOW.sh   one-shot: everything above, one report
    DIAGNOSE_OCTAVE.sh
                    only if the figure checks cannot read the .mat files

## Verification map

Every numbered result in PROOFS_v2 has exactly one canonical verifier;
`00_reader/proof_registry.py` enforces that and fails if any is unassigned.

    lean            5   proof-grade
    lean_partial    4   proof-grade  (algebraic core in Lean, analytic step in
                                      the document; scope per entry in the
                                      registry's LEAN_PARTIAL_SCOPE)
    symbolic       12   proof-grade  (SymPy)
    proof_in_text   5   proof-grade  (proved in the document, no machine check)
    numerical       9   evidence-grade

`01_theory/verify_document_figures.py` regenerates every numerical figure the
document quotes, from the .mat files, and asserts each against the printed
value. Figures whose solve outputs are absent report SKIP with the reason and
the open TODO item; a read that fails while the data IS present is a FAIL, not
a SKIP.

Lean is not compile-checked by the harness — run `lake build` separately.

## Bibliography

`references.bib` appears in four places — `00_document/`, `05_summaries/`,
`06_empirical_starter/`, `07_correspondence/` — because each must compile
standalone. They are kept
byte-identical; `00_document/references.bib` is canonical. Edit that one and
copy it to the other two in the same change. `VERIFY_NOW.sh` section 6 hashes
all three and reports if they drift.
