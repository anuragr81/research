# Theory paper

The theory paper's Lean, separated from the literature and measurement
files of `lean/mathlib/`. What the paper delivers is set by the author and
not yet recorded here.

## Contents

`lean/` is a Lake project holding the twelve files that MANUSCRIPT.tex's
model rows M1 to M16 cite, and the refuted conjectures R1 and R2:

| File | Rows |
|---|---|
| `RateBased.lean` | M1, M12, M13; R2 |
| `CapGeometry.lean` | M2, M3, M8 |
| `Identification.lean` | M4, M5, M6 |
| `Envelope.lean` | M4, M5, M10 |
| `SecondOrder.lean` | M7; R1 |
| `Coordinate.lean` | M8, M9 |
| `BCVW.lean` | M9 |
| `SmoothFit.lean` | M10 |
| `QVI_Part1.lean` | M11 |
| `ImpulseCount.lean` | M14 |
| `Bounds.lean` | M15 |
| `Targeting.lean` | M16 |

`BCVW.lean` is the literature record of Bayraktar et al. (2026); it is here
because M9 cites `BCVW.piBar_at_one` and `Coordinate.lean` imports it.

The files are copies. `lean/mathlib/` stays the canonical project, which
`verify.sh` builds; TH-1 fails if a copy differs from its source, so a
change is made there and copied here. Toolchain and Mathlib pin are those
of `lean/mathlib/` (Lean v4.32.0-rc1, Mathlib v4.32.0-rc1).

## Check

    python3 theory/verify_theory.py

- TH-1 every file is identical to its source in `lean/mathlib/`.
- TH-2 every Lean name MANUSCRIPT.tex cites (the last field of each model
  row, and each inline `\lean{}`) is declared here: 89 names.
- TH-3 runs `checks/verify_mathlib.py` on `theory/lean/`: `lake build`, no
  `sorry`, and every theorem's axioms within propext, Classical.choice
  and Quot.sound.

`VERIFICATION.md` is the output of the last run: 5 checks, 0 failures;
98 theorems built and audited.

## Building

    cd theory/lean && lake exe cache get && lake build

Where the Mathlib cache host (lakecache.blob.core.windows.net) is not
reachable, `lake build` alone compiles Mathlib from source, several hours
on four cores.
