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

- 2026-10-25. The solve (author, "can you proceed with the solve?").
  `solve/solve_qvi.py` solves the discrete QVI by policy iteration;
  `solve/run_illustration.py` writes `solve/results_solve.md`, pinned, and
  TH-6 checks the pins. At $(\lambda_S,K)=(1.2,0.05)$ every hypothesis of M1,
  M2 and M4 holds on 601 and 1201 nodes, the envelope identities hold to
  0.7 percent, and the threshold responses have the signs of M1 and M2
  (determinant $-0.30$ numerically, $-0.33$ by formula). Over 56 points the
  trigger is interior at 42, where every hypothesis holds, and at the
  distress boundary at 14 (large $K$ against $\lambda_S$), where the trigger
  cannot move; K1's scope note now says so. Appendix B carries it as an
  illustration; no row rests on it.
- 2026-10-25. Two things learned about the numerics, recorded for the
  parent bundle. The time-stepping scheme of `02_numerical/` gives the
  thresholds within one grid step of policy iteration (Octave run at 301
  nodes, $(1.2, 0.05)$: $y^*=1.750$, $x_L=1.030$, $y_{\text{post}}=1.550$
  against $1.745$, $1.025$, $1.555$), but its value differs from the value
  of its own policy by about $10^{-3}$, too coarse for finite differences in
  $\lambda_S$, and at the parent's settings it stops at 1500 iterations
  without reaching its tolerance. A trigger at the reference that the
  numpy port of the scheme produced at $(1.5, 0.02)$ is an artefact of the
  port; policy iteration gives an interior trigger there.
- 2026-10-25. Eight papers uploaded to Drive read in full, each with a
  record under `lit/` and a Lean companion: Constantinides and Richard
  (1978), Vo (2009), Behn, Daminato and Salleo (2019), Lang and Menno (2026),
  Glover, Johnson, Evatt and Cheng (2023), Décamps, Mariotti, Rochet and
  Villeneuve (2011), Lambertini and Uysal (2013, 2014). The first paragraph
  of Section 3.1 is replaced by two, the forms the cost of falling below
  takes in the bank papers, and where the two frictions are paired (L48 to
  L57). Glover et al. and Lambertini and Uysal are struck from C4's pending
  list. Constantinides and Richard is the nearest precedent to K1 and C4 now
  names it (L52). Vo (2009) and Lambertini and Uysal (2013) have records and
  are not cited.
- 2026-10-25. Milne (2004) and Alvarez, Le Bihan and Lippi (2014, NBER WP
  20155) read in full with records. Milne (2004) is not cited: on the pairing
  of audit and fixed cost it summarises Milne and Whalley. C4 now cites
  Alvarez et al. (L58), who identify from moments of the distribution of
  adjustments rather than thresholds and need a second moment where two
  parameters move one moment the same way. C4 stays pending on the author's
  decision to narrow it and on the Banque de France version.
- 2026-10-25. C5 stays in the table of pending claims, awaiting verification,
  and no prose is expanded on it (author). K2 is argued from M10 to M12 and
  C2, C3 only. The three existing pointers to C5 (the opening of Section 4,
  the K2 paragraph of Section 4, the opening of the Conclusions) stay as
  pointers; none argues from C5.
- 2026-10-25. C4 narrowed and concluded (author). It now reads: in a bank's
  objective, identification of the asymmetry parameter and the fixed
  issuance cost from the dividend barrier and the recapitalisation trigger
  by opposite signs on the trigger is new against the papers read in full,
  with Constantinides and Richard (L52) and Alvarez, Le Bihan and Lippi
  (L58) as the nearest precedents outside banking. It moved to Table 6 with
  its ID kept. The NBER version of Alvarez et al. is the one read and cited,
  so the Banque de France version is no longer needed. Section 5 of
  `notes/novelty_check_20261023.md` is now read in full for C4. Bonaparte and
  Cooper (2009) and Bonaparte, Cooper and Zhu (2011) appear only in that
  note's pass-0 list (section 4) and were not read when C4 was concluded;
  both were read on 28 October (see the entry below).
- 2026-10-28. Target journal: Annals of Finance (Springer, ABDC 2025 B), chosen
  by the author on median decision time. The 34-day median from submission to
  first decision was read from a search snippet of a mirror of its Springer
  page and is not verified on link.springer.com, which this environment
  cannot reach. The journal's author guidelines (length, format, appendices)
  are not yet read, for the same reason.
- 2026-10-28. Four more papers read in full with records and Lean:
  Federico, Modena and Regis (SAFE WP 487, 2026), Egami and Oryu (version of
  8 November 2014), Bonaparte and Cooper (NBER 15227, August 2009 version),
  Bonaparte, Cooper and Zhu (NBER 16957, 2011). None holds C4's result.
  Federico et al. is the closest bank architecture (dividend barrier,
  impulse issue at a fixed cost to an endogenous target) but its trigger is
  fixed at required capital, its dividend restriction is a macro-state tax,
  and every threshold response is numerical at one calibration; it is cited
  in Section 3.1 (L59, L60). Bonaparte, Cooper and Zhu jointly estimate
  preference parameters and a fixed adjustment cost with an explicit inaction
  band, but identify from moments and a table of elasticities, not from the
  thresholds; it is cited in C4 (L61). Egami and Oryu (regulator's problem,
  passive bank) and Bonaparte and Cooper (2009) have records and are not
  cited. C4 stands. Left unread from the first-pass list: Grossman and Vila
  (1992) and Hilscher, Raviv and Wiener (2024), for K2's positioning only.

- 2026-10-28. The skeleton carries a table of citations (author): every cited
  paper, the version read, where it is published, its citation count and its
  journal's ABDC 2025 rating; cited papers only and no impact factor (author).
  The ABDC list the author supplied is `papers/journal_rankings/`. Counts are
  OpenAlex's, retrieved 28 October 2026 by `checks/fetch_citations.py`, dates
  Crossref's; MS-10 checks the table. Four papers read as working papers or
  preprints are now published: Li, Yu and Zhang (SIAM J. Financial Math.
  2024), Glover et al. (European J. Finance 2023), Federico, Modena and Regis
  (Mathematical Finance, online 30 September 2026), Bonaparte, Cooper and Zhu
  (J. Monetary Econ. 2012). Alvarez, Le Bihan and Lippi have an AER 2016 paper
  under another title, not checked to be the same paper.
- 2026-10-29. The table of citations carries each paper's title and a link to
  each OpenAlex record it counts, and is sorted by citations (author). Counts
  are of the published version wherever one is found (author); Alvarez, Le
  Bihan and Lippi's NBER WP 20155 is the AER 2016 paper (same result in the
  abstract), so its row counts the AER record, 169 (+56), rated A*. Counts
  refetched 29 October 2026.
- 2026-10-31. M16, the optimal exposure binds the cap (author: work on the
  proof). lean/CapBinding.lean: the first term of the equation is quadratic
  in U = pi x, h(U) - h(u) factors, and the cap is optimal wherever V'' >= 0
  (given sigma >= 2 max(a1, a3) c sigma_L) and, where V'' < 0, exactly when
  G = V''(u sigma^2 - D) + V'(mu_s - r) >= 0. Applied with A6 and M4: binds
  below x_L and above y* (V affine there), and near both thresholds. This
  proves the hypothesis of M11 for the maximiser; those ends are states the
  bank leaves at once. solve/run_binding_check.py: the condition predicts the
  solver's exposure at every interior node of both baselines; binds
  everywhere at r = 0.02 (smallest slack 9 percent of the premium term), below
  the cap on [1.124, 1.231] at r = 0.025. K2's scope note updated.
- 2026-10-31. Submission-related concerns are queued here and not raised in
  the work (author: nowhere near submission). Queued: check the JEDC article
  of Milne and Robertson against the draft; read Baglioni and Cherubini if
  C4's novelty is challenged; text issues F4, F6, F8; the Annals of Finance
  assessment.
- 2026-10-31. Behn, Daminato and Salleo are not one of the three approaches:
  payoff term (a step charge, L53) and impulse term (no fixed part, L54), no
  cap term, estimation outside the equation. Offered: rows for them and Lang
  and Menno in Table 5; waits for the author.
- 2026-10-30. Milne and Robertson read in full from the February 1994 draft
  (author: quotations are internal), record lit/milne_robertson_1996/,
  MilneRobertson.lean, rows L81 to L83, sentences in Sections 3.5 and 4.1,
  cited as the JEDC article. Their footnote 5 offers a penalty rate on a
  shortfall in place of liquidation (L82). Their printed barrier 0.17872
  checks against the closed form. Theorem 2's proof misstates a sign (MR-D3).
  Open: check the JEDC article against the draft before submission.
- 2026-10-30. Engle and Siriwardane contrasted in Section 4.3 (author: yes),
  C6 conditions on no delay (author: yes), drift of the state agreed (author).
  Offered: the identity that the paper's multiplier is the reciprocal of the
  risk-weighted capital ratio, for Section 4.3 with a Lean fact; waits for the
  author.
- 2026-10-30. Calem and Rob, and Fries, Mella-Barral and Perraudin, read in
  full and cited (author: added to the Drive folder). Records
  lit/calem_rob_1999/ (read as FEDS 96-12, 1996; published in the JFI 8(4)
  1999, not read; cited as calem1999) and
  lit/fries_mellabarral_perraudin_1997/ (the JBF article), Lean files
  CalemRob.lean and FriesEtAl.lean, rows L71 to L78, sentences in Sections
  2.3, 3.5 and 4.1, and two citation rows; citations.json refetched on 30
  October 2026. Quotations are internal notes, not for publication (author).
  Open:
  - Milne and Robertson (1996): the Drive file is the February 1994 draft
    marked "Not for quotation"; the skeleton is internal, so its record may
    rest on the draft (author), and the submitted paper cites the JEDC
    article, whose results should be checked against the draft.
  - Engle and Siriwardane: not a fourth approach and not in the equation
    (author and Claude agree); a contrast paragraph for Section 4.3 is
    drafted and waits for the author.
- 2026-10-30. Baglioni and Cherubini (1994, RISEC 41, 893-904) dropped from the
  reading list (author). Not cited. C4 claims novelty against the papers read
  in full, which stays accurate; theirs is the (S, s) bank model with fixed
  recapitalisation costs and a capital requirement that a referee is most
  likely to raise against it.
- 2026-10-30. Comparative statics as Section 3 (author: main text, after the
  model; baseline of Bayraktar et al., argued for). On the lines of Milne and
  Whalley's section 4: what the rows prove, the baseline with rho as the
  parameter the results depend on most, five figures (thresholds against
  lambda_S, against K, against the solvency floor with the determinant; the
  attitude to risk; the safety-value frontier of Q7), the delay, Bayraktar et
  al.'s Table 1 and the branches of the cap, and what is not checked.
  Appendix B's in-progress log moved into it. Provenance table
  (tab:provenance) and MS-11, which redraws the figures from the saved outputs
  and checks every digest (author: be explicit where the data come from).
  Scripts: solve/run_cs.py, solve/run_q7_safety.py, solve/q7_paths.py,
  solve/make_cs_figures.py. Findings new in the text: required capital moves
  the dividend barrier by about three-eighths of its rise and the trigger by a
  seventh, against Milne and Whalley's one for one (row L69); lambda_S buys
  safety at 4 percent of cash value where the floor costs 20 percent, and at
  equal safety (floor 0.10) lambda_S leaves the cash value 0.082 higher; a
  floor of 0.06 gives more time in deficit than 0.045; the exposure is at the
  cap on the whole inaction region, so Koehn and Santomero's shift has no room.
- 2026-10-30. M15, buffer variance under time-varying asset volatility
  (author: do it), lean/TimeVarying.lean, pinned in lean/ORIGIN.md. K2 cites
  it; K1's scope note says the identification is given the asset volatility,
  which moves the thresholds as K does, and given no delay (author). Engle and
  Siriwardane positioned for K2 only, in Section 3.1 and the Question
  (author), with ES-Q8, ES-Q9 and row L70: they use the structural relation
  for returns and not levels, on financial firms. Open:
  - The model with time-varying asset volatility, state (x, h), thresholds as
    curves: deferred to scoping the enhancement paper (author).
  - That the optimal exposure binds the cap once asset volatility moves is
    not shown.
- 2026-10-30. The six missing sources restored from the author's Drive folder
  wealth_garch, each matching its pinned sha256: Black and Perold
  (28665427.pdf), Constantinides and Richard
  (Existence_of_Optimal_Simple_Policies_for_Discounte.pdf, OCR regenerated
  with rapidocr_onnxruntime installed for the user), Decamps et al. (JF.pdf),
  Federico et al. (1978961634.pdf), Glover et al. (Capital ideas ... .pdf),
  Lambertini and Uysal (bank_011014_2.pdf). New records note the original
  file name.
- 2026-10-30. Milne and Whalley corrections and the objective (author).
  Their Result 8 (MW-Q15, row L64) says the bank never issues by choice, so
  L38 and the Section 3.1 sentence no longer read their (s, S) phrase as an
  issue point and a target; the issue point is the audit. Their equation (3)
  (MW-Q16, MW-D4, row L65) is in the payoff term but depends on V, so not a
  choice of Lambda; with a fine f per unit of shortfall it is Lambda with
  lambda_S - 1 = q f, and the thresholds identify q f only. Section 3.4 and
  Table 5 no longer call the audit outside the equation. Section 3.1 opens
  with a paragraph positioning the dynamic models by what each makes
  endogenous, on the lines of their pp. 9 to 10. Appendix D, "The
  objective": the objectives of the papers read and why the paper's is
  linear in dividends with one kink (MW-Q17 to MW-Q20, rows L66 to L68).
  To do:
  - Read, PDFs to be found by the author: Milne and
    Robertson (1996, JEDC 20, 1427-1449), the base of Peura and Keppo and
    of Milne and Whalley; Calem and Rob (1996 FEDS 96-12, published version
    if any), dynamic capital and risk-taking; Fries, Mella-Barral and
    Perraudin (1997, JBF 21, 441-468), optimal closure. Not Merton (1978)
    (author).
  - Milne (2004) has a record in the parent's lit/ (milne_2004) and takes
    up the portfolio question of L67; a theory record needs only a copy.
  - Q7 to report the average risky exposure and its ratio to the cap along
    paths as the solvency floor rises, the Koehn and Santomero question of
    L67 in the paper's model.
  - O'Hara (1983), managers' cost of losing their jobs, is a further reading
    of whose cost the penalty is (Appendix D), if read.
- 2026-10-30. The bank's problem stated before the results (author): new
  Section 2.3 (who optimises, state equation with drift, admissible policies,
  objective, whose penalty, no liquidation, the QVI as eq:qvi). Section 3.4
  refers to it. A1 states the drift; the map gains D, I_n, r, mu_s, mu_L,
  gamma and mu. Open:
  - "drift of the state" is an In use name, not yet put to the author.
  - The symbol b is the retention ratio in the Names table; the drift is now
    mu(x, pi) everywhere, as in the proofs.
- 2026-10-30. Milne and Whalley's control of variance (author: exposition
  only, no change to the model). MW-Q13, MW-Q14, MW-D3; rows L62 (Result 6)
  and L63 (Result 5); a sentence in Section 3.4 that the cap term holds their
  rule; their cap-term cell now "sigma_1 or sigma_2, assumed".
- 2026-10-30. Q1 pass 3, a delay on issues (`solve/delay_model.py`,
  `solve/run_q1_delay.py`, `results_q1_delay.md`), remark in Appendix B,
  "A delay on issues". Checked: the delay raises all three thresholds (as
  PeuraKeppo.table2_signs); the model without delay fitted to (y*, x_L)
  overstates lambda_S - 1 by 9 to 78 percent at a delay of 0.1 years and K by
  3 to 10 percent; the fitted y_post matches to 1e-4 in all twelve fits. For
  the author:
  - K1's scope note and C6 assume no delay. Whether to say so in K1 and C6
    is the author's decision; the remark does not change them.
  - Candidate row: when y_post > R, y_post is a function of y* and kappa
    alone (the penalty is zero on [y_post, y*], V is fixed there by
    V'(y*) = 1, V''(y*) = 0 and the equation at y*). Needs a hypothesis-form
    Lean statement before it is a row. It also answers the open item of the
    Q3 pass 2 entry: y_post can identify kappa and nothing else.
  - Not checked: whether Peura and Keppo's issue moments (time between
    issues, cost per issue) separate the delay from lambda_S.
  - Not checked: a delay with liquidation during the wait, which is
    Peura and Keppo's channel; the paper's model has no liquidation.
- 2026-10-30. Peura and Keppo's two distinctive predictions, issuance at a
  positive buffer and default risk despite the option, are tested nowhere in
  their paper (author's observation, for later). Summary from the records of
  the papers read, not a search: later models put the trigger at the floor
  or at zero (Decamps et al., Lokka and Zervos, Bolton et al., Federico et
  al., Bayraktar et al.), a positive trigger returns only where a cost grows
  below the requirement, and no paper read tests issue timing or default
  against data. To do: a search beyond the papers read, and a decision on
  whether the manuscript says this.
- 2026-10-30. Q3 regimes (`solve/run_q3_regimes.py`, `results_q3_regimes.md`), in
  Appendix B (author asked for the idea, not Bayraktar et al.'s design): the
  solvency branch carries K1 (interior trigger, determinant -0.33 and -0.29)
  and K2's deficit limit; without it the trigger is at the distress boundary
  and the hypotheses fail. The surplus limit is not reached in the solves.
  Liquidity alone gives 0.209 of the 0.244 value loss against the ceiling
  alone, but the attribution depends on ordering and is not in the text.
- 2026-10-30. Citation table in lexicographic order (author), descending in
  claims, then in citations, then by bib key; it replaces the rank sums of
  earlier the same day. Bayraktar et al. (13 claims) first, Peura and Keppo
  (12) second.
- 2026-10-30. The table of citations gains Claims, the number of literature
  rows on each paper linked to the first, and If not relied on, one sentence
  in the map's names on what the manuscript loses without the paper (author).
  `\cirow` has nine arguments and `\cinot` the tenth; MS-10 checks the
  claims, the sentence and the order, with a control that swaps two rows.
- 2026-10-30. Q3 pass 2 and Q1 pass 1 (`solve/run_q3_pass2.py`,
  `solve/run_q1_frictions.py`), in Appendix B with table tab:q3-det, which
  replaces tab:q3-spread. Checked: the determinant of M3 by its formula
  shrinks from -0.269 to -0.037 as a1 rises from 0.045 to 0.12, hypotheses
  holding at all 17 points; K -> 0.001 brings the paper's model to within
  0.011 of Bayraktar et al.'s converged barrier; every interior point of Q1
  has the signs of M1 and M2. Open:
  - Done: `solve/run_q3_pass3.py` reruns the determinant on 1201 and 2401 nodes;
    it shrinks from -0.289 to -0.041, finite differences agree within 15 percent,
    and W(y*) falls from 0.0385 to 0.0007, the numerator of dy*/dlambda_S in M1.
  - Inferred, not checked: kappa lowers the injection target where K raises
    it, so the three thresholds might identify (lambda_S, K, kappa) together.
    Needs the 3x3 determinant and a row in Lean before any text.
  - Peura and Keppo's three angles are covered by `solve/run_q1_pk.py` and
    in Appendix B: issue moments, value of the option to issue, and the
    barrier against asset volatility and expected return. Checked: moments'
    determinant is non-zero by size, thresholds' by sign. Open: the time
    between issues is good to ten percent only; a finer grid and more paths
    would tighten it. No hump in the option value, as the model has no delay
    or liquidation.
  - Q7, safety against value, needs a path simulator.
- 2026-10-30. The comparative-statics exercise is in Appendix B, paragraph
  "Comparative statics, in progress" (author). The illustrations paragraph
  now states r = 0.025 in place of Bayraktar et al.'s 0.02. The numbers
  behind BCVW-D10 are regenerated by `solve/run_q3_concavity.py`; those of
  BCVW-D12 by `solve/run_q3_bcvw_regimes.py`.
- 2026-10-30. Comparative statics narrowed to Q1, Q3 and Q7 (author): the
  thresholds against each friction, the regulatory parameters, and safety
  against value. Q10, recovering the asymmetry from data, belongs to the
  enhancement paper. Q3 pass 1 is `solve/run_q3_regulatory.py` and
  `solve/results_q3.md`, with a port of Bayraktar et al.'s solver
  (`solve/bcvw_port.py`, MIT) and policy iteration on their model
  (`solve/bcvw_model.py`). Findings in BCVW-D9 to D11. Not in the manuscript.
- 2026-10-30. Section 3.4 rewritten around the one equation (author). The
  three approaches are the three terms of the quasi-variational inequality,
  the cap term, the impulse term and the payoff term, with a rule at the
  distress boundary in place of the impulse operator where a paper
  liquidates or forces an issue. Table tab:common's columns are renamed to
  those terms. A closing paragraph places K1 in the two free-boundary
  branches and K2 in the cap term, with the term separation of
  `solve/results_noborrow.md` section 4 as the illustration.
- 2026-10-30. Choulli, Taksar and Zhou (2004), Ann. Appl. Probab. 14(4),
  read in full from the author's upload, with the record
  `lit/choulli_taksar_zhou_2004/` and `lean/ChoulliTaksarZhou.lean` (10
  theorems, two controls; 49 checks pass). It does not pre-empt K2: constant
  exposure bounds, a deterministic liability, no asymmetry parameter
  (CTZ-D3 to D5). It is a precedent for the hypothesis of M11, since the
  minimum exposure binds near ruin and, with barrier dividends, the maximum
  binds before the dividend barrier (CTZ-Q11, CTZ-Q15), and its Proposition
  3.3 is a proof template. C5 still names the SIAM paper of 2003, reference
  [4] there, which is not read. Not yet cited in the manuscript.
- 2026-10-30. The theory paper is self-contained (author). It uses the Lean
  artifacts of the author's earlier work and nothing from the enhancement
  paper, and its text refers to no other manuscript. `lean/` is pinned by
  sha256 to the copies taken from `lean/mathlib/` at commit `740823e4`
  (`lean/ORIGIN.md`, TH-1); TH-2, which read the earlier `MANUSCRIPT.tex`,
  is dropped. The 20 literature records it cites are copied to `lit/` at the
  same commit with `litcheck.py` and `ocr_pages.py`, and the checkers read
  them there. The agreed names are the Names table of `MEASUREMENT_MAP.tex`,
  and MM-11 and MM-12 read it there. The ledger spec is `checks/ledger_model.py`.
  The earlier manuscript is not retired (author).
- 2026-10-28. K2 kept as written; a solve with no borrowing added (author).
  `solve/run_noborrow.py`, Appendix B "Without borrowing". The audit of
  28 October is `notes/audit_20261028.md`.

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
- C5. Choulli, Taksar and Zhou (2003), to be read in full with a record, and
  a proof that the optimal exposure binds the cap near both ends. The 2004
  companion is read (`lit/choulli_taksar_zhou_2004/`); the 2003 paper is not.
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
