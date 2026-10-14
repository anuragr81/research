# Paper A, verified manuscript skeleton

Stereotype Rigidity in Social Inferences (the "Mimic Men" paper), rebuilt claim
by claim under `papers/MANUSCRIPT_SKELETON.md`. The predecessor document is
`../theory_and_decisions/manuscript.tex`. It stays in place until a later pass
retires it.

| Path | Role |
|---|---|
| `MANUSCRIPT.tex` | The skeleton, and the only canonical statement of the results. |
| `TODO.md` | The pass plan, the claim queue, the author's decisions and the novelty ledger. Read it first. |
| `lean/mathlib/` | Lake project pinned to Lean `v4.24.0-rc1` and Mathlib `14871d5`. |
| `checks/` | `verify_mathlib.py` (build and axiom audit), `verify_manuscript.py` (rules MS-1 to MS-6), `verify_docs.py` (stated counts against the audit). |
| `verify.sh` | Runs every suite and writes `VERIFICATION.md`, which is generated. |
| `lit/` | One record per paper read. None yet. |
| `notes/writing_discipline.md` | The writing rules, copied from the reference bundle. |
| `notes/from_archive/` | Three notes from the author's archive `paper-A-mimic-men.tar.gz` (snapshot of 3 July 2026), kept as background. They are not evidence for any row. |

## Setup

    cd lean/mathlib && lake exe cache get && lake build
    ./verify.sh
    lit/verify_lit.sh
    pdflatex MANUSCRIPT.tex && pdflatex MANUSCRIPT.tex

`lean/mathlib/.lake/` is not committed. On this machine it was copied, by
reflink, from the entry-contest bundle, which pins the same toolchain and
Mathlib commit.
