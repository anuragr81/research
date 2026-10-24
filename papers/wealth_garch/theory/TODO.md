# Theory paper, plan and decisions

## PLAN

The passes of `papers/MANUSCRIPT_SKELETON.md`, section 7.

| Pass | Work | Status |
|---|---|---|
| 1 | Skeleton, the empty tables, ID scheme, appendix stubs, checker with controls, `verify.sh` | Done 2026-10-23. MS-1 to MS-9 pass on the skeleton and on their good samples, and fail on their bad ones. The PDF builds with no warning. |
| 2 | Lean project, toolchain and Mathlib pinned, build roots, audit | Done 2026-10-23. `lean/` holds the twelve files the parent manuscript's rows M1 to M16 and R1, R2 cite, and the eight Lean companions of the literature records the theory rows cite. 169 theorems audited. |
| 3 | Analytic steps into Lean | Done 2026-10-23. No new step; the rows reuse the parent's audited theorems. `BCVW.ceiling_tighter_than_lcr` added for C3. |
| 4 | Model rows, their proofs and assumptions, the measurement map | Done 2026-10-23, M13 and M14 added 2026-10-24. M1 to M14, A1 to A6 (map below), and the measurement map as Appendix C with its checker. |
| 5 | Appendices B to G filled | Done 2026-10-23. B, C, D and E filled; F and G not applicable. |
| 6 | Retire a predecessor | Not applicable unless the parent manuscript's theory rows are moved here rather than copied. |
| 7 | Novelty reading | Passes 0 to 6 done; four papers remain (`notes/novelty_check_20261023.md`, section 5). C4 and C5 pending. |
| 8 | Conclusions, then the introduction | Done 2026-10-23. C1 to C3 and C6 verified, C4 and C5 pending; K1, K2. The question rewritten as the literature's. |
| 9 | Literature table | Done 2026-10-23. L1 to L34, eleven papers, each read in full with a record and a Lean companion; L15 to L21 trace the question the literature leaves open, L22 to L34 carry the literature ledgers of Figures 2 to 4. |
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
  the literature"). Section 3.1 of the manuscript traces it: buffers above
  the minimum (L15), guarding against intervention or market response (L16),
  the issuance-cost model underpredicting (L17), Peura and Keppo calling for
  a graduated penalty (L18), leaving the constituency open (L19) and setting
  parameters by intuition (L20), Dai, Huang and Keppo fitting values (L21).
  The chain is question, K1 and K2, M rows, C1 to C3 and C6, with C4 and C5
  pending. The constituency question (L19) is outside the paper; the
  thresholds identify the weight of the penalty, not whose cost it is.

- 2026-10-23. The question reassessed against a measurement map (author,
  "use measurement map, redefine, reassess the question"). Appendix C is now
  the map of `papers/MANUSCRIPT_SKELETON.md` section 4.5, three tables with
  the six status words, checked by `checks/verify_measurement_map.py`
  (Suite 4), with the parent's agreed names. What the map says. The
  asymmetry parameter and the fixed issuance cost are never observed. The
  two thresholds are recorded as the state at a dividend and at an issue,
  and on the FDIC panel the parent bundle read neither behaves as a
  threshold (parent map G14, G15 fail). The variance limits are computed
  from assumed volatilities and apply where the cap binds, which on that
  panel it does not (G2 fails). So K1 and K2 are results within the model,
  the question is one of identification, and the Question, the Answer and
  the scope notes now say so. The weight of the penalty matters because at
  one it returns the issuance-cost model (M8), so its identification is what
  would let the graduated-penalty specification be tested rather than
  imposed.
- 2026-10-23. Four names proposed to the author, not yet agreed. Solvency
  floor ($a_1$), run-off rate ($a_2$) and liquidity haircut ($a_3$), since
  the three parameters of the cap geometry have different statuses in the
  map (recorded, assumed, assumed); and value function ($V$). The parent
  map's Terminology table should take them if agreed.

- 2026-10-23. The question stays one of identification, "a theory of what
  the policy would reveal if a bank followed a threshold policy" (author).
  The redirection toward what the panel shows belongs to the testing thread.
- 2026-10-23. The four proposed names are accepted for now (author). The
  checker keeps them in its PROPOSED set until the parent map's Terminology
  table takes them.
- 2026-10-23. The map is its own document, `theory/MEASUREMENT_MAP.tex`,
  with the three tables in `theory/map_tables.tex`, which Appendix C of the
  manuscript includes (author, "the measurement for the theory paper should
  be in the theory paper's subfolder").
- 2026-10-23. The reference is defined, $R=1+q$, the state at which equity
  equals required capital and the capital buffer is zero (author, "if you
  need to define R go ahead"). The penalty is then charged exactly in
  deficit; `QVI_Part1.shortfall_zero_at_or_above_reference` and
  `QVI_Part1.shortfall_pos_below_reference`, with a control. Its counterpart
  is the requirement, where prompt corrective action's restrictions begin.
  M1's hypothesis that the penalty is flat at the barrier now says the
  barrier lies above the requirement.

- 2026-10-23. The four names are agreed in the parent map (solvency floor,
  run-off rate, liquidity haircut, value function), and the theory map
  checker's set of proposed names is empty.
- 2026-10-23. The gap is drawn (author, "visualise the gap in literature
  that the paper closes"). Three approaches, issuance-cost barrier models,
  regulatory-cap control, and preference asymmetry read in volatility, each
  as a ledger against the paper's own, in the manuscript as Figures 1 to 4
  (author, "the diagrams should be in the manuscript"), with Section 3.2 and
  Table `tab:approaches`. Thirteen literature rows, L22 to L34, carry the
  quotations the drawings rest on; four quotations were added to the
  Bayraktar et al. record (BCVW-Q19 to Q22). The planning note is
  `notes/ledger_literature.md`.
- 2026-10-23. The manuscript is `wealth_garch_theory.tex`, renamed from
  `MANUSCRIPT.tex` (author). Every checker and note follows the new name.

- 2026-10-24. Section 4, "What the paper adds to the three approaches" (author,
  "write the literature novelty ... what the paper's theoretical claims
  address that's not in the three chains"). One paragraph per result. K1
  adds a graduated shortfall penalty to the issuance-cost models and turns
  their tabulated responses into a derived local identification; K2 adds
  the variance limits under the regulatory cap that regulatory-cap control
  does not evaluate, free of the asymmetry parameter; K1 and K2 together
  read the asymmetry parameter off the thresholds rather than the variance;
  C3 replaces a solved range by a condition. A last paragraph lists what is
  shared and not claimed (M4 to M7, M9, the kinked form of M8). Each
  paragraph names its falsifier, and the section compares only with the
  papers read; novelty beyond them stays in C4 and C5. Two rows added,
  L35 (BCVW-Q17) and L36 (QJE-Q4). Written under
  `notes/writing_discipline.md` (author); the closing paragraph of 3.2 and
  a closing clause of its third paragraph moved here rather than staying
  in both places.

- 2026-10-24. Terminology audit (author, "has it been made sure that
  alternative names for concepts are not used"). No retired name of the
  parent table was in the paper's own prose; the seven uses of "loss
  aversion" are Barberis et al.'s and Li et al.'s own object in sentences
  citing their literature rows, which the writing discipline allows. Six
  alternative names were replaced by the map's names wherever they named the
  paper's own object, in the text, the figures, the map and the commentary of
  literature rows L11, L18, L19, L23 and L25: the weight of the penalty
  (asymmetry parameter), fixed cost (fixed issuance cost), the asset and
  liability shocks (asset volatility, liability volatility, return-liability
  correlation), the requirement (required capital), variance limits (limits
  of buffer variance), the issuance-cost model (the model with issuance costs
  alone). Short forms of an agreed name (the trigger, the cap, the state,
  exposure) are kept, as in the parent manuscript. MM-12 of
  `checks/verify_measurement_map.py`, with three controls, now holds the text,
  the figures and the literature-row commentary to this.
- 2026-10-24. "Thresholds", the dividend barrier and the recapitalisation
  trigger together, is defined at first use in the Answer and entered as
  Proposed in the parent terminology table.

- 2026-10-24. "Thresholds", the dividend barrier and the recapitalisation
  trigger together, is Agreed (author) and so marked in the parent
  terminology table.

- 2026-10-24. Section 3.3, the three approaches in one formulation (author,
  "a common mathematical treatment ... to compare the three approaches ...
  more quantitatively"). The paper's model A1 to A6 holds each paper read as
  a restriction of its parameters (Table `tab:common`), and two quantities
  are evaluated on each at its own parameters (Table `tab:common-values`).
  The determinant of the two thresholds in the two frictions is $-29/100$
  and $+1/25$ in Peura and Keppo's Table 2 (small and highest cost; one
  printed decimal), about $-0.0041$ in Dai, Huang and Keppo, not formable
  for Milne and Whalley (figures only), and negative by sign in the paper
  (M3). The limits of buffer variance at Bayraktar et al.'s baseline are
  about 3.140 and 0.0688 (new M13), and with no liability shock they are
  Black and Perold's log-cushion variances (new M14). Lean in the new
  `Comparison.lean` and in the Peura and Keppo and Dai, Huang and Keppo
  companions; quotations PK-Q32 to Q34 added; L41 to L47. The comparison is
  of signs and of which quantity exists, not of sizes across papers.

### Row map, parent to theory

| Theory | M1 | M2 | M3 | M4 | M5 | M6 | M7 | M8 | M9 | M10 | M11 | M12 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Parent | M4 | M5 | M6 | M7 | M10 | M14 | M16 | M11 | M8 | M2 | M3 | M15 |

Parent C4 is theory C4 (pending, under K1); parent C2 is theory C5 (pending,
under K2). Refuted conjecture R1 is kept, R2 is not.

## Open, for the author


- C6 and Section 4. C6 says what K1 supplies to the specification Peura and
  Keppo call for, which the K1 paragraph of Section 4 now says with its
  evidence. Under rule 5 of the writing discipline one should go.
  Recommended: drop C6 from Table 7 and keep Section 4.

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
- The Lean files are copies of `lean/mathlib/`, checked identical by TH-1.
  Moving them instead needs `verify.sh` and `checks/verify_mathlib.py` of
  the parent changed.

## Mechanics deferred

- `checks/verify_docs.py` enters with the first count the manuscript states.
- `lit/verify_lit.sh` is not yet in `verify.sh`. The nine cited suites are run by hand (all pass, 23 October 2026); they need the source PDFs in `~/.cache/wealth_garch/`.
