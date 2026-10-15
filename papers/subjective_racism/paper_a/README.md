# Paper A, verified manuscript skeleton

Closure Traps from Social Inference (titled Stereotype Rigidity in Social
Inferences until 15 October 2026), rebuilt claim by claim under `papers/MANUSCRIPT_SKELETON.md`. The predecessor document,
`../theory_and_decisions/manuscript.tex`, was retired at pass 6 on 15 October
2026. `RETIREMENT.md` maps every part of it to its home and says how to read
it from git.

| Path | Role |
|---|---|
| `MANUSCRIPT.tex` | The skeleton, and the only canonical statement of the results. |
| `MEASUREMENT_MAP.tex` | Companion note. Every object of the model with one name, its type, what it represents and how an observer obtains it. Its Name column is the terminology. |
| `RETIREMENT.md` | Where every environment, section, figure and number of the retired predecessor went. Checked by `checks/verify_retirement.py`. |
| `TODO.md` | The pass plan, the claim queue, the author's decisions and the novelty ledger. Read it first. |
| `lean/mathlib/` | Lake project pinned to Lean `v4.24.0-rc1` and Mathlib `14871d5`. |
| `checks/` | `verify_mathlib.py` (build and axiom audit), `verify_manuscript.py` (rules MS-1 to MS-7), `verify_docs.py` (stated counts against the audit), `verify_measurement_map.py` (rules MM-1 to MM-9), `verify_retirement.py` (the retired predecessor, R0 to R8), `lean_files_table.py` (the linked table of Lean files in Appendix B). |
| `verify.sh` | Runs every suite and writes `VERIFICATION.md`, which is generated. |
| `lit/` | One record per paper read. None yet. |
| `notes/writing_discipline.md` | The writing rules, copied from the reference bundle. |
| `notes/from_archive/` | Three notes from the author's archive of Paper A (snapshot of 3 July 2026), kept as background. They are not evidence for any row. |

## Setup

    cd lean/mathlib && lake exe cache get && lake build
    ./verify.sh
    lit/verify_lit.sh
    pdflatex MANUSCRIPT.tex && bibtex MANUSCRIPT && pdflatex MANUSCRIPT.tex && pdflatex MANUSCRIPT.tex
    pdflatex MEASUREMENT_MAP.tex

`lean/mathlib/.lake/` is not committed. On this machine it was copied, by
reflink, from the entry-contest bundle, which pins the same toolchain and
Mathlib commit.
