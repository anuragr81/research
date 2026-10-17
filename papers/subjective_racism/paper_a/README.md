# Paper A1, verified manuscript skeleton

Class Read from Group Markers. Until 15 October 2026 this bundle was Paper A,
Closure Traps from Social Inference, rebuilt claim by claim under
`papers/MANUSCRIPT_SKELETON.md` from a predecessor retired at pass 6. On
15 October 2026 the author split Paper A in two. Paper A1, this bundle, keeps
every row that needs no law of motion for a group's standing, and suite 5
fails if one creeps in. Paper A2, `../paper_a2`, starts from the snapshot of
Paper A at commit `9f108a44` and holds the law of motion, the basis for it,
and every row that rests on it. The predecessor's retirement record and its
suite live there. Row IDs are shared with the snapshot, so IDs absent here
(M4 to M13, M16, A5 to A11, L1 to L3, L5, C2 to C9, C12 to C14, C17, C18,
R1) are rows of Paper A2.

| Path | Role |
|---|---|
| `skeleton_ClassMarkers.tex` | The skeleton, and the only canonical statement of the results. |
| `ledger_picture.tex` | The picture of Figure 1, two entities and fifteen actions. The manuscript includes it, and `notes/ledger_diagram.tex` draws it alone for the planning notes. |
| `MEASUREMENT_MAP.tex` | Companion note. Every object of the model with one name, its type, what it represents, whether it is directly observed and how an observer obtains it. Its Name column is the terminology. |
| `TODO.md` | The pass plan, the claim queue, the author's decisions and the novelty ledger. Read it first. |
| `lean/mathlib/` | Lake project pinned to Lean `v4.24.0-rc1` and Mathlib `14871d5`. |
| `checks/` | `verify_mathlib.py` (build and axiom audit), `verify_manuscript.py` (rules MS-1 to MS-8), `verify_docs.py` (stated counts against the audit), `verify_measurement_map.py` (rules MM-1 to MM-10), `verify_no_law.py` (no law of motion, NL-1 to NL-5), `lean_files_table.py` (the linked table of Lean files in Appendix B). |
| `verify.sh` | Runs every suite and writes `VERIFICATION.md`, which is generated. |
| `lit/` | One record per paper read. Phelps (1972) and Jeffrey (1983, chapter 11). |
| `notes/writing_discipline.md` | The writing rules, copied from the reference bundle. |
| `notes/from_archive/` | Three notes from the author's archive of Paper A (snapshot of 3 July 2026), kept as background. They are not evidence for any row. |

## Setup

    cd lean/mathlib && lake exe cache get && lake build
    ./verify.sh
    lit/verify_lit.sh
    pdflatex skeleton_ClassMarkers.tex && bibtex skeleton_ClassMarkers && pdflatex skeleton_ClassMarkers.tex && pdflatex skeleton_ClassMarkers.tex
    pdflatex MEASUREMENT_MAP.tex

`lean/mathlib/.lake/` is not committed. On this machine it was copied, by
reflink, from the entry-contest bundle, which pins the same toolchain and
Mathlib commit.
