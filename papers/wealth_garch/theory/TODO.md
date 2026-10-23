# Theory paper, plan and decisions

## PLAN

The passes of `papers/MANUSCRIPT_SKELETON.md`, section 7.

| Pass | Work | Status |
|---|---|---|
| 1 | Skeleton, the empty tables, ID scheme, appendix stubs, checker with controls, `verify.sh` | Done 2026-10-23. MS-1 to MS-9 pass on the skeleton and on their good samples, and fail on their bad ones. The PDF builds with no warning. |
| 2 | Lean project, toolchain and Mathlib pinned, build roots, audit | Done 2026-10-23. `lean/` holds the twelve files the parent manuscript's rows M1 to M16 and R1, R2 cite. |
| 3 | Analytic steps into Lean | Open. Waits on what the paper delivers. |
| 4 | Model rows, their proofs and assumptions, the measurement map | Open. Waits on what the paper delivers. |
| 5 | Appendices B to G filled | Open. |
| 6 | Retire a predecessor | Not applicable unless the parent manuscript's theory rows are moved here rather than copied. |
| 7 | Novelty reading | Open. |
| 8 | Conclusions, then the introduction | Open. |
| 9 | Literature table | Open. |
| 10 | Readability rounds | Open. |
| 11 | Referee comments | Not applicable. |

## Decisions

- 2026-10-23. The theory paper is one of three papers from `papers/wealth_garch`,
  with its own directory `theory/` (author). The others are the testing
  thread and the enhancement thread, both in `comparison/` for now.
- 2026-10-23. The skeleton follows `papers/MANUSCRIPT_SKELETON.md`, with
  Paper A (`papers/subjective_racism/paper_a/`) as the reference (author).

- 2026-10-23. Novelty check of C2 and C4 (`notes/novelty_check_20261023.md`).
  Search only, every lead [A] or [C], since the paper hosts are denied by the
  network policy. Neither claim can be called new yet. The closest leads
  are Black and Perold (1992) for C2 and Angelis (2025) for C4. Peura and
  Keppo's Table 2 shows C4's mechanism with the issue delay in place of the
  asymmetry parameter.

## Open, for the author

- Novelty. Whether C2 and C4 move to the parent's pending table now, as
  section 4.4 of the skeleton guideline requires (recommended), and the
  papers to supply for the readings of section 5 of the novelty note.

- What the paper delivers. The question, and which rows enter. Candidates
  are in the parent `MANUSCRIPT.tex`, headlines K1 and K2, model rows M1 to
  M16, literature rows, conclusions C1 to C4, refuted conjectures R1 and R2.
- The title. The skeleton carries the parent's, Asymmetric Capital Buffer
  Control, as a working title.
- Literature records. The checker reads the parent's `lit/` records, shared
  by the three papers, so no record is duplicated (recommended). The
  alternative is a `theory/lit/` of its own.
- The measurement map. The parent's `MEASUREMENT_MAP.tex` holds the names the
  author agreed on 13 October 2026. Recommended: the theory paper takes its
  model objects from that map, so the three papers share one terminology.
- The Lean files are copies of `lean/mathlib/`, checked identical by TH-1.
  Moving them instead needs `verify.sh` and `checks/verify_mathlib.py` of
  the parent changed.

## Mechanics deferred

- The bibliography enters with the first literature row. Until a key is
  cited, bibtex stops with an error.
- `checks/verify_docs.py` enters with the first count the manuscript states.
- The measurement-map checker enters with the map, at pass 4.
- `lit/verify_lit.sh` joins `verify.sh` with the first literature row.
