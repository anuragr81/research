# Inequality and Social Contests — manuscript skeleton and measurement map

- `claims.yaml` — headline, model (M*), literature (L*), concluding claims and references
- `measurement_map.yaml` — micro-model inputs and outputs (X*) with real-world referents
- `check.py` — validates both files; `python3 check.py --lean` audits every declared model theorem via `#print axioms`
- `verify.sh` — builds Lean, runs `check.py --lean` and `lit/verify_lit.sh`, writes `VERIFICATION.md` (generated)
- `refs.bib` — bibliography; every key of a literature row read in full must be here
- `lit/` — one record per paper read (`lit/README.md`); Lean for cited papers in `lean/Lit/`
- `build_manuscript.py` → `manuscript.tex`; `build_map.py` → `measurement_map.tex`; compile each with `xelatex` (twice)
- `manuscript.pdf` — self-contained: claims tables, references, full Lean source (App. A), Lean build files (App. B)
- `measurement_map.pdf` — independent document
- `lean/` — Lean 4 project, toolchain v4.21.0, Mathlib pinned to tag v4.21.0 (commit 308445d7) in `lake-manifest.json`
- Source PDFs and their text are cached outside the repository in `~/.cache/firmworkers/`, pinned by sha256 in each record's suite
- `TODO.md` — pending items
- `HANDOVER.md` — start here for a new session
- `inputs/` — source manuscript and earlier artefacts (see HANDOVER.md §3)

Lean build: `cd lean && lake exe cache get && lake build`.
