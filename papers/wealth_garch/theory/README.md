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
| `checks/ledger_theory.py` | The ledger of the paper under the bundle's spec (`00_reader/ledger_model.py`), proposed in `notes/ledger.md`. |
| `verify_theory.py` | The Lean suite (below). |
| `verify.sh` | Runs the four suites, writes `VERIFICATION.md` (generated). |
| `refs.bib` | Bibliography of the papers the L rows cite. |
| `citations.json`, `checks/fetch_citations.py` | Citation counts (OpenAlex), print dates (Crossref) and ABDC 2025 ratings (`papers/journal_rankings/`) of every cited paper, for the table of citations; MS-10 checks the table against them. |
| `solve/run_noborrow.py` | The same solve with no borrowing, pi <= 1 (Appendix B); `results_noborrow.md` is generated and pinned (TH-6). |
| `notes/writing_discipline.md` | The writing rules, copied from the parent. |
| `TODO.md` | Plan, decisions, open items. |

## Lean

`lean/` is a Lake project holding twenty-three files. Twelve are those the parent
`MANUSCRIPT.tex`'s model rows cite, and `Comparison.lean` is the theory paper's own; the theory rows use them as follows.

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
| `RateBased.lean` | none; kept for TH-2, the parent's rows |
| `BCVW.lean` | L1 to L4, C3 |

The other ten are the Lean companions of the literature records the L
rows cite: `BlackPerold.lean` (L5, L6), `PeuraKeppo.lean` (L7, L8),
`DaiHuangKeppo.lean` (L9), `BoltonChenWang.lean` (L10), `LokkaZervos.lean`
(L11), `BarberisHuangSantos.lean` (L12), `EngleSiriwardane.lean` (L13),
`LiYuZhang.lean` (L14), `BergerEtAl.lean` (L15) and `MilneWhalley.lean` (L16).

The files are copies. `lean/mathlib/` stays the canonical project, which
`verify.sh` builds; TH-1 fails if a copy differs from its source, so a
change is made there and copied here. Toolchain and Mathlib pin are those
of `lean/mathlib/` (Lean v4.32.0-rc1, Mathlib v4.32.0-rc1).

### Check

    python3 theory/verify_theory.py

- TH-1 every file is identical to its source in `lean/mathlib/`.
- TH-2 every Lean name the parent MANUSCRIPT.tex cites (the last field of each model
  row, and each inline `\lean{}`) is declared here: 89 names.
- TH-4 every paper `theory/wealth_garch_theory.tex` cites has a `lit/` record whose
  Lean file (its `LEAN`) is in `theory/lean/`.
- TH-5 every Lean name `theory/wealth_garch_theory.tex` cites (model rows and inline
  `\lean{}`) is declared here: 72 names.
- TH-6 the solve's results file is pinned to the scripts as they now are.
- TH-3 runs `checks/verify_mathlib.py` on `theory/lean/`: `lake build`, no
  `sorry`, and every theorem's axioms within propext, Classical.choice
  and Quot.sound.

### Building

    cd theory/lean && lake exe cache get && lake build

Where the Mathlib cache host (lakecache.blob.core.windows.net) is not
reachable, `lake build` alone compiles Mathlib from source, several hours
on four cores.
