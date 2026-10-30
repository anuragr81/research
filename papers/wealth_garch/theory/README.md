# Theory paper

The theory paper of `papers/wealth_garch`. `wealth_garch_theory.tex` is built
under `papers/MANUSCRIPT_SKELETON.md` and carries the paper's theoretical
rows only: headlines K1 (identification) and K2 (partition), model rows M1
to M14, literature rows L1 to L47, conclusions C1 to C3 and C6, and C4, C5
pending. Section 3.1 traces the question the literature leaves open, and Section 3.2 draws the three approaches as ledgers against the paper's own (Figures 1 to 4), and Section 4 says what each theoretical result adds to them. `TODO.md` holds the plan and the open
decisions. `./verify.sh` runs the four suites and writes `VERIFICATION.md`.

| Path | Role |
|---|---|
| `wealth_garch_theory.tex` | The manuscript, its tables and appendices A to G (renamed from `MANUSCRIPT.tex` on 23 October 2026). |
| `checks/verify_manuscript.py` | The skeleton checker, rules MS-1 to MS-9 with controls. |
| `solve/` | The numerical solve of the model by policy iteration, an illustration that the hypotheses of M1 to M4 hold (Appendix B); `results_solve.md` is generated and pinned (TH-6). |
| `figures/` | The four ledger figures of the manuscript (TikZ) and their shared styles. |
| `MEASUREMENT_MAP.tex`, `map_tables.tex` | The measurement map, its three tables shared with Appendix C of the manuscript. |
| `checks/verify_measurement_map.py` | The map's checker, rules MM-1 to MM-11 with controls. |
| `checks/ledger_theory.py`, `checks/ledger_model.py` | The ledger of the paper and the spec it follows (a copy of the author's earlier `ledger_model.py`), proposed in `notes/ledger.md`. |
| `verify_theory.py` | The Lean suite (below). |
| `verify.sh` | Runs the four suites, writes `VERIFICATION.md` (generated). |
| `refs.bib` | Bibliography of the papers the L rows cite. |
| `citations.json`, `checks/fetch_citations.py` | Citation counts (OpenAlex), print dates (Crossref) and ABDC 2025 ratings (`papers/journal_rankings/`) of every cited paper, for the table of citations; MS-10 checks the table against them. |
| `solve/run_noborrow.py` | The same solve with no borrowing, pi <= 1 (Appendix B); `results_noborrow.md` is generated and pinned (TH-6). |
| `notes/writing_discipline.md` | The writing rules, copied from the author's earlier work. |
| `lit/` | The 20 literature records the paper cites, with `litcheck.py`, `ocr_pages.py` and `verify_lit.sh` (Suite 5); copied from the bundle's `lit/` at commit `740823e4` and the paper's own since (its `README.md`). |
| `lean/ORIGIN.md` | The sha256 pin of every Lean file (TH-1) and the commit the copies were taken at. |
| `TODO.md` | Plan, decisions, open items. |

## Lean

`lean/` is a Lake project holding thirty-three files, the author's earlier Lean
work that the model rows cite, `Comparison.lean`, which is the theory paper's
own, and the Lean companions of the literature records; the theory rows use them as follows.

| File | Theory rows |
|---|---|
| `Identification.lean` | M1, M2, M3 |
| `Envelope.lean` | M1, M2, M5 |
| `SecondOrder.lean` | M4; R1 |
| `SmoothFit.lean` | M5 |
| `ImpulseCount.lean` | M6 |
| `Targeting.lean` | M7 |
| `QVI_Part1.lean` | M8 |
| `CapGeometry.lean` | M9, M10, M11 |
| `Coordinate.lean` | M9 |
| `Bounds.lean` | M12 |
| `Comparison.lean` | M13, M14 (new for the theory paper, Section 3.3) |
| `RateBased.lean` | none; the author's earlier rows, kept |
| `BCVW.lean` | L1 to L4, C3 |

The other twenty are the Lean companions of the literature records the L
rows cite, named in each record's `LEAN` file under `lit/`.

The files were copied from `lean/mathlib/` of the bundle at commit `740823e4`
(28 October 2026) and are the theory paper's own since then. `lean/ORIGIN.md`
pins each by sha256 and TH-1 holds the files to the pins, so a change is made
here, with its pin, and later changes to `lean/mathlib/` do not reach the
paper. Toolchain and Mathlib pin are Lean v4.32.0-rc1, Mathlib v4.32.0-rc1.

### Check

    python3 theory/verify_theory.py

- TH-1 every file matches its sha256 pin in `lean/ORIGIN.md`, and every pin has a file.
- TH-4 every paper `theory/wealth_garch_theory.tex` cites has a `theory/lit/` record whose
  Lean file (its `LEAN`) is in `theory/lean/`.
- TH-5 every Lean name `theory/wealth_garch_theory.tex` cites (model rows and inline
  `\lean{}`) is declared here: 72 names.
- TH-6 the solve's results files are pinned to the scripts as they now are.
- TH-3 runs `checks/verify_mathlib.py` on `theory/lean/`: `lake build`, no
  `sorry`, and every theorem's axioms within propext, Classical.choice
  and Quot.sound.

### Building

    cd theory/lean && lake exe cache get && lake build

Where the Mathlib cache host (lakecache.blob.core.windows.net) is not
reachable, `lake build` alone compiles Mathlib from source, several hours
on four cores.
