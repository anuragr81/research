# Paper B: everything pending (as of 2026-10-01)

Status key: [ ] open, [~] blocked on the author, [x] done. The audit log
`notes/citation_audit.md` holds the item-level detail (M, P, D, L, S, J, R, X);
the master plan is `notes/manuscript_corrections.tex` / `.pdf`.

## 1. Author decisions, first priority

- [~] **Approve entries of Section C (C.1 to C.15) and Section E (E.1 to E.14),
  entry by entry.** Approved entries are applied to `PAPER_B_MANUSCRIPT.tex`,
  compiled and committed. Order constraints:
  - E.10 (Propositions ORD and ADJ) before any entry that cites ORD or ADJ.
  - E.14 (bibliography) before E.1, E.5, E.7 and C.15.
  - E.11 before E.12.
- [~] **Notation in E.10:** rename the one-cue beliefs $P^{A}$, $P^{B}$ to
  $P^{(A)}$, $P^{(B)}$ so they cannot be read as the benchmark $P^{\mathrm B}$
  (roman B). Notation only.
- [ ] After each batch of approvals: rebuild `PAPER_B_MANUSCRIPT.pdf` (`make paper`),
  check no undefined references, commit on `master`.
- [ ] **Push.** Local `master` is one commit ahead of origin (6afe9135, the built
  manuscript PDF). Rebuild the PDF right before pushing, since it goes stale
  when the tex changes.

## 2. Carry the old md plan over more fully (deferred by the author until some
## corrections are applied)

Gaps found when comparing `manuscript_change_plan_asof_2026-09-30.md` with the tex:

- [ ] **1.E:** an intro sentence stating Proposition ORD with its instrument
  (marginals and the share of decisions changed show the sequence effect at first
  order; the association and the surplus-weighted loss do not). No AFTER text
  exists. Must come after E.10 in the application order.
- [ ] **X2 / W.A:** Table 1 (manuscript line 894) says "share of *candidates*
  whose decision the reading sequence changed"; the text and Proposition SHR say
  *evaluators*. Add a correction entry.
- [ ] **W.A:** one clause in Section 6 naming heterogeneous impressions (the model
  holds $P$, $q$, $r$ common across evaluators) next to heterogeneous $c$.
- [ ] Record the parked Table 1 redesign in the tex so it is not lost.
- [ ] Section D of the tex: confirm that every surviving element of old 1.D
  (the two premises, $\omega$ named in the intro) is carried by C.1 to C.8, by a
  sentence-level diff against the author's new intro. Currently taken from the
  Section D status table only.

- [ ] **Identification overclaim (found 2026-10-01).** "A failure of identification,
  not of estimation, which no sample size repairs" is true only for the odds ratio
  each evaluator holds (Lemma SEP, exactly the prior's in either sequence). The
  cross-product association and the loss are second order, visible at a precision
  finer than $c^2$ (MS line 456). Withdrawn from C.1; still in C.15 (C.11 part 9),
  `positioning_economics.tex` :49-58 and `papers_dialectic.tex` :175-179.

## 3. X5: incorporate the exploration documents (after the current state is verified)

Method as set out under X5 in the audit: extract each document's points, map each
to a manuscript line or plan item (covered, partly covered, missing), apply the
audit's corrections before anything is carried over, draft new BEFORE/AFTER
entries for missing points under `notes/writing_discipline.md`, record points left
out and why. Output: a coverage matrix plus new entries.

- [ ] `papers_dialectic.tex`
- [ ] `interior_omega.tex` (largely in E.12; check the rest)
- [ ] `positioning_economics.tex` (X3: decide whether to retire it)
- [ ] `question_and_answer.tex`, `two_horn_motivation_body.tex`
- [ ] `empirical_analytics.tex`, `worked_example.tex`
- [ ] Substantive entries of `paper_review_log.md` (e.g. Entry 17, the zero-slope test)
- [ ] `the_discrimination_problem.tex` (never committed; read-only source)
- [ ] **X1:** check that the plan's literature edits (3.A to 3.D, now E.5 to E.7
  and C.15) carry what `papers_dialectic.tex` argues, after the corrections.

## 4. Corrections to the exploration notes themselves (not the manuscript)

- [ ] **D1/J5** `papers_dialectic.tex`: Döring's objection is normative, not
  psychological; "even Jeffrey took [the ...]" (J5).
- [ ] **D9, D10** `interior_omega.tex` and `literature/weisberg2009/README.md`:
  Weisberg mapping; "order dependence" wording.
- [ ] **D13** `the_discrimination_problem.tex`: BIR trichotomy
  (impartial type, three sources).
- [ ] **D2, D5, D7, D8, D11, D12, D14 to D16** (see audit).
- [ ] **L2 to L8, L10, L11** corrections to the `literature/*/README.md` records.
- [ ] **J2** the counterfactual-likelihood argument wherever it still appears
  (`two_horn_motivation_body.tex`, `papers_dialectic.tex`); replaced in the
  manuscript plan by C.6 and E.3.
- [ ] **D3** `verification_coverage.md` counts are out of date (file stays
  uncommitted).
- [ ] **D4** `check_reversal.py` excluded from `run_all.py` by design; document why.

## 5. Source and citation checks

- [ ] **M18 and publisher checks:** DOIs (BHW, Ortoleva 2012, Epstein 2006), page
  ranges, journal details against the publishers' pages.
- [ ] **X4:** Weisberg 2009 citation unconfirmed from the preprint.
- [ ] **L11 / Wilson:** the draft's Theorem 5(i) is false as stated; compare with
  the published version.
- [ ] **S1 to S4:** file identity notes (`goodmittal1987.pdf` is Good 1960; Domotor
  scan duplicates; items not in `temp`; unused PDFs).
- [ ] **R3 / R5:** Jeffrey 1988 has no Lean record (source unavailable). Every
  other cited paper has one.
- [ ] Two absence claims could not be checked without OCR and were narrowed to the
  rendered pages (Domotor pp. 384 to 403 for "likelihood"/"marginal"; Jeffrey 1983
  ch. 11 for "invariance"). Run OCR over the full scans to settle them.

## 6. Standing rules

- Work directly on `master` in the paper repo (author's decision, 2026-10-01).
- Every manuscript change is shown BEFORE/AFTER and verified by the author first.
- All new text follows `notes/writing_discipline.md` (gate: `discipline_check()`).
- Every cited paper has a Lean record; new Lean artifacts are committed and pushed.
- Never commit `the_discrimination_problem.tex`, `paper_review_log.md`,
  `verification_coverage.md`. Keep LaTeX build outputs, `__pycache__` and
  `lean/.lake` out of commits.
