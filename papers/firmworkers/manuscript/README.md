# Inequality and Social Contests — manuscript skeleton and measurement map

- `claims.yaml` — headline, model (M*), literature (L*), concluding claims and references
- `measurement_map.yaml` — micro-model inputs and outputs (X*) with real-world referents
- `check.py` — validates both files; `python3 check.py --lean` re-verifies every Lean claim via `#print axioms`
- `build_manuscript.py` → `manuscript.tex`; `build_map.py` → `measurement_map.tex`; compile each with `xelatex` (twice)
- `manuscript.pdf` — self-contained: claims tables, references, full Lean source (App. A), Lean build files (App. B), SymPy evidence (App. C)
- `measurement_map.pdf` — independent document
- `lean/` — Lean 4 project, toolchain v4.21.0, Mathlib pinned to tag v4.21.0 (commit 308445d7) in `lake-manifest.json`
- `sympy/micro_checks.py` — SymPy evidence referenced by claims
- `sources/` — plain text of supplied primary sources (`L<n>.txt`) for VERBATIM checks
- `TODO.md` — pending items
- `HANDOVER.md` — start here for a new session
- `inputs/` — source manuscript and earlier artefacts (see HANDOVER.md §3)

Lean build: `cd lean && lake exe cache get && lake build`.
