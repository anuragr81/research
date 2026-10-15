# Paper A2, the law of motion and what it carries

Snapshot of `paper_a` at commit `9f108a44` (15 October 2026), taken when the
author split Paper A in two. Paper A1 (`../paper_a`) keeps everything that
needs no law of motion, and a check there fails if one creeps in. Paper A2,
this bundle, starts from the complete snapshot and will hold the basis for
the law of motion and every row, assumption, conclusion and literature
parallel that rests on it (M4 to M13, M16, M17's figures, M15 (iii) to (v),
R1, A5 to A11, K1 to K6, C2 to C9, C12, C13, C17, C18, L1, L2, L3, L5).
Its restructuring has not started. The predecessor's retirement record,
`RETIREMENT.md`, and its suite live here. The text below is the snapshot's
README, unchanged.

---

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
| `checks/` | `verify_mathlib.py` (build and axiom audit), `verify_manuscript.py` (rules MS-1 to MS-8), `verify_docs.py` (stated counts against the audit), `verify_measurement_map.py` (rules MM-1 to MM-10), `verify_retirement.py` (the retired predecessor, R0 to R8), `lean_files_table.py` (the linked table of Lean files in Appendix B). |
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
