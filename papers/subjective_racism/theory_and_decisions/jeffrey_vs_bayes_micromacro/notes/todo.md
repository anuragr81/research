# Paper B: everything pending (as of 2026-10-01)

Status key: [ ] open, [~] blocked on the author, [x] done. The audit log
`notes/citation_audit.md` holds the item-level detail (M, P, D, L, S, J, R, X);
the master plan is `notes/manuscript_corrections.tex` / `.pdf`.

## 1. Author decisions, first priority

- [x] **Applied at db3a2f41 (2026-10-02):** 2.1, 2.2, 2.3, 2.4, 2.5, 3.1, 3.4, 4.1, 5.1, 5.2,
  6.1, 6.2, 6.3, with the five bibliography entries they cite (Bohren2019, Doring1999,
  Garber1980, Heckman1998, Jeffrey2004). The manuscript builds with no undefined references
  or citations; its two overfull boxes (title block, summary table) predate the change.
- [x] **Applied at 6775f822 (2026-10-02):** 3.2 parts 3 to 5 (the Dietrich sentences deleted
  rather than rewritten, the author having judged the pooling literature out of scope; the
  Epstein/Ortoleva/Cripps and Pettigrew-Weisberg sentences corrected) and the new 5.3, the
  pooling sentence in Section 5 (`sympy/verify_pooling.py`, 10/10).
- [~] **Pending, the author's call entry by entry:** 0.1, 1.1 to 1.9, 2.6, 3.2 parts 1 and 2, 3.3,
  3.5, 4.2, A.1, B.1, B.2. The plan lists applied and pending entries above its table and keeps the
  numbering fixed. Among the pending, B.2 is now reduced to Benjamin et al. 2019 for 1.3's
  optional sentence, since the other five entries are in the .bib; B.1 goes last.
- [ ] **Follow-ups on applied 3.1 (flagged 2026-10-01, applied as approved):** its second
  paragraph repeats the intro's Hawthorne point and replies with the old defence (measure the
  degree of adoption) rather than the scope defence of 1.4 and Section 6; its third
  paragraph's first-order against second-order sentence needs "under full adoption".
- [ ] **Follow-up on applied 3.4:** the appended identification paragraph still carries "which
  no sample size repairs" (true for the odds ratio only; see the identification item below).
- [~] **Notation in 6.3 (Proposition ADJ):** rename the one-cue beliefs $P^{A}$, $P^{B}$ to
  $P^{(A)}$, $P^{(B)}$ so they cannot be read as the benchmark $P^{\mathrm B}$
  (roman B). Notation only.
- [ ] After each batch of approvals: rebuild `PAPER_B_MANUSCRIPT.pdf` (`make paper`),
  check no undefined references, commit on `master`.
- [ ] **Push** after each batch. Rebuild `PAPER_B_MANUSCRIPT.pdf` right before pushing,
  since it goes stale when the tex changes.
- [x] **Adoption weight (decided 2026-10-01).** Keep $\omega$, repurposed as the nesting
  of Hogarth-Einhorn's belief-adjustment rule. Verified: `sympy/verify_ladder.py` (37/37),
  `lean/JeffreyOrder/Ladder.lean`. Partial adoption moves every belief statistic except the odds
  ratio one order earlier with the ranking unchanged; the odds ratio is blind at every weight; the
  share-versus-loss separation holds only under full adoption.
- [~] **Approve the reworked entries:** 0.1 to 1.3 (rebased on c2ae782f) and 1.4 (rebased on
  d48dd872), residual corrections only; 2.3, 5.2 (Proposition ORD alone), 6.3 (the partial-adoption
  subsection of Section 6: two channels, ADJ, LAD, the new Proposition FAC on factor inputs,
  Table tab:robust, the rubric prediction and the settings table), with its heading 6.1 and
  roadmap clause 1.9.
- [ ] **Partial-adoption subsection (added 2026-10-01, reframed the same day).** Section 6
  keeps the author's title "Scope and robustness" and splits into Scope (assumptions
  paragraph, 6.2) and "Partial adoption and factor inputs" (6.3). Framing decided by the
  author: the second-order results require full adoption; Proposition ADJ tests for it from
  three marginals of one reading group; Propositions LAD and FAC say what partial adoption
  and a factor reading change. Not presented as robustness, since the association's
  second-order result fails for every $\omega<1$. Proposition FAC: under factor inputs, full
  adoption commutes (the benchmark) and a factor adopted in part, $a^\omega$, re-opens the
  position channel. Verified: `verify_ladder.py` section F (53/53 in all), Ladder.lean
  factor-input theorems (the Lean proves the damped-factor form for an arbitrary $a'$; the
  power form is sympy). The weighted factor is this paper's construction, not one of
  Hawthorne's variants; the text says so.
- [ ] The author may drop "and robustness" from the Section 6 title.
- [ ] Two tables in the partial-adoption subsection (tab:robust, tab:settings); the author
  may drop tab:settings.
- [ ] The decision rows of tab:robust and tab:settings and the paragraph after LAD's
  proof (share and loss of order zero under partial adoption) are the LOS integral read
  with an order-zero gap, not a sympy row. Add a check if the author wants it verified
  like the rest.
- [ ] `ZhaoOsherson2010` is no longer cited in the manuscript (author's c2ae782f); its
  bibliography entry is unused. Keep, or cite again, the author's call.

## 2. Carry the old md plan over more fully (deferred by the author until some
## corrections are applied)

Gaps found when comparing `manuscript_change_plan_asof_2026-09-30.md` with the tex:

- [ ] **1.E:** an intro sentence stating Proposition ORD with its instrument
  (marginals and the share of decisions changed show the sequence effect at first
  order; the association and the surplus-weighted loss do not). No AFTER text
  exists. Must come after 5.2 in the application order.
- [ ] **X2 / W.A:** Table 1 (manuscript line 894) says "share of *candidates*
  whose decision the reading sequence changed"; the text and Proposition SHR say
  *evaluators*. Add a correction entry.
- [ ] **W.A:** one clause in Section 6 naming heterogeneous impressions (the model
  holds $P$, $q$, $r$ common across evaluators) next to heterogeneous $c$.
- [ ] Record the parked Table 1 redesign in the tex so it is not lost.
- [ ] Section D of the tex: confirm that every surviving element of old 1.D
  (the two premises, $\omega$ named in the intro) is carried by 0.1 to 1.8, by a
  sentence-level diff against the author's new intro. Currently taken from the
  Section D status table only.

- [ ] **Identification overclaim (found 2026-10-01).** "A failure of identification,
  not of estimation, which no sample size repairs" is true only for the odds ratio
  each evaluator holds (Lemma SEP, exactly the prior's in either sequence). The
  cross-product association and the loss are second order, visible at a precision
  finer than $c^2$ (MS precision passage). Withdrawn from 0.1; the abstract's last
  sentence is fixed by 0.1 part 3 and the intro's "identification of sequence effects"
  sentence is cut by 1.1 part 4. Still in 3.4 (identification paragraph),
  `positioning_economics.tex` :49-58 and `papers_dialectic.tex` :175-179.

## 3. X5: incorporate the exploration documents (after the current state is verified)

Method as set out under X5 in the audit: extract each document's points, map each
to a manuscript line or plan item (covered, partly covered, missing), apply the
audit's corrections before anything is carried over, draft new BEFORE/AFTER
entries for missing points under `notes/writing_discipline.md`, record points left
out and why. Output: a coverage matrix plus new entries.

- [ ] `papers_dialectic.tex`
- [ ] `interior_omega.tex` (largely in 6.3; check the rest). The "moot" classification
  claims were corrected 2026-10-01 against `verify_ladder.py`.
- [ ] `positioning_economics.tex` (X3: decide whether to retire it)
- [ ] `question_and_answer.tex`, `two_horn_motivation_body.tex`
- [ ] `empirical_analytics.tex`, `worked_example.tex`
- [ ] Substantive entries of `paper_review_log.md` (e.g. Entry 17, the zero-slope test)
- [ ] `the_discrimination_problem.tex` (never committed; read-only source)
- [ ] **X1:** check that the plan's literature edits (3.A to 3.D, now 3.1 to 3.5
  and 3.4) carry what `papers_dialectic.tex` argues, after the corrections.

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
  manuscript plan by 1.6 and 2.5.
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
