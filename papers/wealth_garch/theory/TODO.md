# Theory paper, plan and decisions

## PLAN

The passes of `papers/MANUSCRIPT_SKELETON.md`, section 7.

| Pass | Work | Status |
|---|---|---|
| 1 | Skeleton, the empty tables, ID scheme, appendix stubs, checker with controls, `verify.sh` | Done 2026-10-23. MS-1 to MS-9 pass on the skeleton and on their good samples, and fail on their bad ones. The PDF builds with no warning. |
| 2 | Lean project, toolchain and Mathlib pinned, build roots, audit | Done 2026-10-23. `lean/` holds the twelve files the parent manuscript's rows M1 to M16 and R1, R2 cite, and the eight Lean companions of the literature records the theory rows cite. 169 theorems audited. |
| 3 | Analytic steps into Lean | Done 2026-10-23. No new step; the rows reuse the parent's audited theorems. `BCVW.ceiling_tighter_than_lcr` added for C3. |
| 4 | Model rows, their proofs and assumptions, the measurement map | Rows done 2026-10-23, M1 to M12 and A1 to A6 (map below). The measurement map is open. |
| 5 | Appendices B to G filled | Done 2026-10-23. B, C, D and E filled; F and G not applicable. |
| 6 | Retire a predecessor | Not applicable unless the parent manuscript's theory rows are moved here rather than copied. |
| 7 | Novelty reading | Passes 0 to 6 done; four papers remain (`notes/novelty_check_20261023.md`, section 5). C4 and C5 pending. |
| 8 | Conclusions, then the introduction | Done 2026-10-23. C1 to C3 and C6 verified, C4 and C5 pending; K1, K2. The question rewritten as the literature's. |
| 9 | Literature table | Done 2026-10-23. L1 to L21, eleven papers, each read in full with a record and a Lean companion; L15 to L21 trace the question the literature leaves open. |
| 10 | Readability rounds | Open, for the author. |
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
  Superseded by the full-text passes 1 to 6 of the same note, section 6.

- 2026-10-23. The theory paper is built on its theoretical novelty only
  (author). K1 is identification, leading; K2 is the partition of the
  asymmetry parameter into the thresholds and out of the variance limits
  (author, on the recommendation that the parent's K1 read as a negative
  claim). The empirical rows of the parent (M1, M9, M12, M13, the E rows)
  are not carried.
- 2026-10-23. Every paper a claim rests on is read in full and has a Lean
  companion in `theory/lean/` (author). TH-4 checks it.
- 2026-10-23. Bolton, Chen and Wang (2011) and Løkka and Zervos (2008) are
  read in full in their pre-publication versions (accepted manuscript;
  2005 preprint), tagged [F] for those versions. The published versions
  were not read.

- 2026-10-23. The question is the literature's, not the author's (author,
  "we need to be clear that there is a question that remains unanswered in
  the literature"). Section 4.1 of the manuscript traces it: buffers above
  the minimum (L15), guarding against intervention or market response (L16),
  the issuance-cost model underpredicting (L17), Peura and Keppo calling for
  a graduated penalty (L18), leaving the constituency open (L19) and setting
  parameters by intuition (L20), Dai, Huang and Keppo fitting values (L21).
  The chain is question, K1 and K2, M rows, C1 to C3 and C6, with C4 and C5
  pending. The constituency question (L19) is outside the paper; the
  thresholds identify the weight of the penalty, not whose cost it is.

### Row map, parent to theory

| Theory | M1 | M2 | M3 | M4 | M5 | M6 | M7 | M8 | M9 | M10 | M11 | M12 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Parent | M4 | M5 | M6 | M7 | M10 | M14 | M16 | M11 | M8 | M2 | M3 | M15 |

Parent C4 is theory C4 (pending, under K1); parent C2 is theory C5 (pending,
under K2). Refuted conjecture R1 is kept, R2 is not.

## Open, for the author

- The ledger (`notes/ledger.md`, section 5). Six entities and eight
  actions of the bundle's ledger are proposed, with the insurer left out.
  Decide the name of P2 (investors, recommended), the creditors, the
  insurer, the reading of the shortfall penalty, and a figure.

- The question. The skeleton's question is drafted from the headlines;
  the author may reword it.
- C4 and C5. The four papers of section 5 of the novelty note, each to be
  read in full with a record, and for C5 a proof that the optimal exposure
  binds the cap near both ends.
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

- `checks/verify_docs.py` enters with the first count the manuscript states.
- The measurement-map checker enters with the map, at pass 4.
- `lit/verify_lit.sh` is not yet in `verify.sh`. The nine cited suites are run by hand (all pass, 23 October 2026); they need the source PDFs in `~/.cache/wealth_garch/`.
