# Claims about Fu & Lu (2010)

**Paper.** Qiang Fu, Jingfeng Lu, "Contest design and optimal endogenous
entry". The source read is **MPRA Paper No. 945**, posted 28 Nov 2006, the working
version of the *Economic Inquiry* article (48(1), 80-88, per `refs.bib`).
Evidence level **[F, WP version]**. Title page confirmed from the PDF on
2026-10-06. Page locators are the paper's printed page numbers.

`LITERATURE.tex` flags that the title differs between the MPRA posting and
Fu–Jiao–Lu's citation of it, and lists a published-version check as
outstanding (`TODO.md` S1). The MPRA number and date in the survey ("MPRA
Paper 945, November 2006") are confirmed from the PDF cover.

## Claims as stated in `LITERATURE.tex` and `PROOFS.tex`

| ID | Claim | Where our documents make it | Source locator |
|---|---|---|---|
| FL-A | `M >= 3` identical contestants, sequential entry with full observation of current participants, organiser chooses prize and a per-entrant fee or subsidy. | `LITERATURE.tex` §sec:entry, Fu–Lu paragraph | p.5 |
| FL-B | **Equation (7)**: total effort equals budget minus `N` times the entry cost. | `LITERATURE.tex` §sec:entry; `PROOFS.tex` §P7 "What is and is not new" ("reach the same relation as an equality, their (7) giving total effort as budget minus `N` times the entry cost") | p.10, eq. (7) |
| FL-C | Eq. (7) "is again the P7 accounting identity"; the accounting bound behind P7 is "already present in" Fu–Lu eq. (7). | `LITERATURE.tex` §sec:entry and contribution list item 1; `PROOFS.tex` "What is not claimed" | p.10 |
| FL-D | Equation (7) is what drives **Theorem 1**: the optimal contest attracts exactly two entrants. | `LITERATURE.tex` §sec:entry | p.10-11, Theorem 1 and proof |
| FL-E | Their conclusion lists different types of contestants as future research, so the design literature on endogenous entry is homogeneous-agent by construction. | `LITERATURE.tex` §sec:entry | p.14 |
| FL-F | Footnote 6 uses sequential entry with observability to break the entrant-identity tie; identity is "resolved there [...] by sequential arrival". | `LITERATURE.tex` §sec:entry, identity paragraph; `PROOFS.tex` complete-information paragraph | p.5, fn 6; p.7, Lemma 2 |
| FL-G | Fu–Lu's `N` is the count induced by an optimally chosen contest, so "under fixed rules with no designer" separates P7 from Fu–Lu. | `LITERATURE.tex` contribution list item 1; `PROOFS.tex` §P7 "What is and is not new" | p.8-10 |
| FL-H | "Fu–Lu's is the sharper case, (7) being an equality rather than an inequality." | `LITERATURE.tex` contribution list item 1 | p.10 |

## What is checkable, and where

| ID | SymPy (`verify_fulu.py`) | Lean (`FuLu.lean`) |
|---|---|---|
| FL-A | none | `countAfter_succ_of_enters`, `countAfter_succ_of_not_enters`, `countAfter_le`, `countAfter_mono`, `countAfter_invariant`, `lemma2_count`, `lemma2_count_unique`, `lemma2_needs_decreasing` (control) |
| FL-B | FL-1, FL-4 | `eq7_from_lemmas`, `eq7_count_cap`, `eq7_rhs_strictly_decreasing`, `eq7_effort_bound`, `eq7_hypotheses_satisfiable` |
| FL-C | FL-L4 (cross-file) | `eq7_dissipation_exact`, and its use in `FuJiaoLu.accounting_bound` |
| FL-D | FL-2, FL-3 | `theorem1_exactly_two`, `theorem1_needs_positive_cost` (control), `zero_cost_effort_is_budget` |
| FL-E | none, textual | none |
| FL-F | none | `stay_out_persists`, `entrants_form_prefix`, `countAfter_of_all_enter`, `enters_iff_position`, `simultaneous_identity_not_pinned` (contrast) |
| FL-G | FL-5, FL-6 | `fixed_rules_cap`, `fixed_rules_count_cap`, `eq7_iff_lemma4_and_lemma5`, `eq7_fails_without_lemma4` (control), `eq7_fails_without_lemma5` (control) |
| FL-H | FL-4 | `eq7_count_cap` with `eq7_dissipation_exact` |

- **FL-C**'s shared content is proved once, in
  `../fu_jiao_lu_2015/Accounting.lean` (`accounting_bound`). `FuLu.lean`
  proves only that Fu–Lu's objects meet that lemma's dissipation hypothesis
  with equality (`eq7_dissipation_exact`), and the suite compiles the two
  files together to apply `accounting_bound` to them (FL-L4).
- **FL-E** is a textual reading, confirmed verbatim in `NOTES.md`.
- Findings against these claims are in `NOTES.md`. Three of them change how
  FL-B, FL-G and FL-H should be worded.
