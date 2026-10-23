# Theory paper

The theory paper of `papers/wealth_garch`. `MANUSCRIPT.tex` is built
under `papers/MANUSCRIPT_SKELETON.md` and carries the paper's theoretical
rows only: headlines K1 (identification) and K2 (partition), model rows M1
to M12, literature rows L1 to L14, conclusions C1 to C3, and C4, C5 pending. `TODO.md` holds the plan and the open
decisions. `./verify.sh` runs the three suites and writes `VERIFICATION.md`.

| Path | Role |
|---|---|
| `MANUSCRIPT.tex` | The manuscript, its tables and appendices A to G. |
| `checks/verify_manuscript.py` | The skeleton checker, rules MS-1 to MS-9 with controls. |
| `checks/ledger_theory.py` | The ledger of the paper under the bundle's spec (`00_reader/ledger_model.py`), proposed in `notes/ledger.md`. |
| `verify_theory.py` | The Lean suite (below). |
| `verify.sh` | Runs the three suites, writes `VERIFICATION.md` (generated). |
| `refs.bib` | Bibliography, the nine papers the L rows cite. |
| `notes/writing_discipline.md` | The writing rules, copied from the parent. |
| `TODO.md` | Plan, decisions, open items. |

## Lean

`lean/` is a Lake project holding twenty files. Twelve are those the parent
`MANUSCRIPT.tex`'s model rows cite; the theory rows use them as follows.

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
| `RateBased.lean` | none; kept for TH-2, the parent's rows |
| `BCVW.lean` | L1 to L4, C3 |

The other eight are the Lean companions of the literature records the L
rows cite: `BlackPerold.lean` (L5, L6), `PeuraKeppo.lean` (L7, L8),
`DaiHuangKeppo.lean` (L9), `BoltonChenWang.lean` (L10), `LokkaZervos.lean`
(L11), `BarberisHuangSantos.lean` (L12), `EngleSiriwardane.lean` (L13) and
`LiYuZhang.lean` (L14).

The files are copies. `lean/mathlib/` stays the canonical project, which
`verify.sh` builds; TH-1 fails if a copy differs from its source, so a
change is made there and copied here. Toolchain and Mathlib pin are those
of `lean/mathlib/` (Lean v4.32.0-rc1, Mathlib v4.32.0-rc1).

### Check

    python3 theory/verify_theory.py

- TH-1 every file is identical to its source in `lean/mathlib/`.
- TH-2 every Lean name MANUSCRIPT.tex cites (the last field of each model
  row, and each inline `\lean{}`) is declared here: 89 names.
- TH-4 every paper `theory/MANUSCRIPT.tex` cites has a `lit/` record whose
  Lean file (its `LEAN`) is in `theory/lean/`.
- TH-5 every Lean name `theory/MANUSCRIPT.tex` cites (model rows and inline
  `\lean{}`) is declared here: 72 names.
- TH-3 runs `checks/verify_mathlib.py` on `theory/lean/`: `lake build`, no
  `sorry`, and every theorem's axioms within propext, Classical.choice
  and Quot.sound.

### Building

    cd theory/lean && lake exe cache get && lake build

Where the Mathlib cache host (lakecache.blob.core.windows.net) is not
reachable, `lake build` alone compiles Mathlib from source, several hours
on four cores.
