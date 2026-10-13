# TODO

The skeleton follows `../MANUSCRIPT_SKELETON.md`, with
`../jite/endogenous_entry/entry_contest/` as the reference bundle.

## PLAN

| Pass | Work | Status |
|---|---|---|
| 1 | Scaffold: four empty tables, ID scheme, appendix stubs, `checks/verify_manuscript.py` with controls, `verify.sh` | done 2026-10-10 |
| 2 | Lean project `lean/mathlib/` (Lean v4.32.0-rc1, Mathlib v4.32.0-rc1), every file a build root, axiom audit in `verify.sh` | done 2026-10-10; the `sorry` in `RateBased.saturated_ratio_tendsto` closed by `RateBased.tendsto_tanh_atTop` |
| 2a | Candidate claims: every headline PROOFS_v2 makes is listed in the novelty ledger below, with the results it rests on, the Lean it waits on and the reading its novelty waits on. A candidate enters Table 5 as a C row once its first model row exists, since MS-3 refuses a C row with no row to point to. This list drives passes 3 and 7 | done 2026-10-10 |
| 3 | Port the analytic steps to Lean, one cluster per pass, with `control_*` theorems and counterexamples (inventory below) | in progress; H1 (`CapGeometry`), H3 (`Identification`) and SOC (`SecondOrder`, M7) done 2026-10-10 |
| 4 | Model rows and Appendix A proofs, one family per pass | |
| 5 | Appendices B to E; `MEASUREMENT_MAP.tex` | `MEASUREMENT_MAP.tex` (G1 to G17), `checks/verify_measurement.py` (suite 2), `lean/mathlib/MeasurementMap.lean` and `lit/bcbs_2011`, `lit/bcbs_2013`, `lit/ffiec_2026` done 2026-10-10; FDIC field names pending |
| 6 | Retire `00_document/PROOFS_v2.tex` with `RETIREMENT.md` and its check | done 2026-10-10; `proof_registry.py` and `check_citations.py` retired with it |
| 7 | Novelty reading, one `lit/` record per paper | in progress; `lit/bayraktar_2026`, `lit/barberis_huang_santos_1999`, `lit/barberis_huang_santos_2001`, `lit/engle_siriwardane_2018`, `lit/li_yu_zhang_2023` done 2026-10-10 |
| 8 | Conclusions, then introduction; headline overreach review | conclusions C1 to C4 and headlines K1, K2 done 2026-10-10 |
| 9 | Literature table | L1 to L4 entered 2026-10-10 |
| 10 | Readability rounds | |
| 11 | Referee comments | not applicable |

## Goal

A manuscript skeleton of definite claims. Each claim rests only on a Lean
proof of the model or a verbatim quote from a primary source that was read.
Each claim of novelty is established from the literature before it leaves
Table 5. The checks decide whether every conclusion stands on fully verified
grounds.

## Decisions

- 2026-10-10. Model rows rest on Lean only. The 12 SymPy results of
  PROOFS_v2 are ported to Lean at pass 3. The 5 results proved only in text
  (`lem:reg`, `lem:bdr`, `lem:itr`, `prop:lowerbound`, `prop:ver`) and the
  Danskin steps of the 4 Lean-partial results enter as hypotheses, listed in
  Appendix B under "What Lean does not carry" and in Appendix D. The 9
  numerical remarks are illustrations only and carry no row.
- 2026-10-10. The skeleton covers the theory only. `MEASUREMENT_MAP.tex`
  records how each model object is observed. The FDIC findings stay in a
  separate empirical document.
- 2026-10-10. The bundle lives at the `wealth_garch/` root.
  `00_document/PROOFS_v2.tex` is retired into `MANUSCRIPT.tex` at pass 6.
- 2026-10-10. The opening question of the introduction is drafted by Claude
  from the PROOFS_v2 abstract, for the author's review (below).

- 2026-10-10. The drafted opening question stays for now. It is reconsidered
  once it is clear how strongly the claims can be stated (pass 8).
- 2026-10-10. The fixed terms in `notes/writing_discipline.md` §6 are
  necessary and adopted.
- 2026-10-10. No referee reports are relevant. Appendices F and G and pass 11
  do not apply.
- 2026-10-10. Submission details (author line and the like) are out of scope
  for the skeleton.
- 2026-10-10. Source PDFs for `lit/` go in the Drive folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, cached locally under
  `~/.cache/wealth_garch/`.

- 2026-10-10. The journal version of Barberis, Huang and Santos was supplied
  and read (`lit/barberis_huang_santos_2001`). L2 cites it (key
  `barberis2001`, p. 47), replacing the working-paper citation of earlier the
  same day. Both records are kept.
- 2026-10-10. C4 moves to Table 4: novelty of the joint identification is
  established against the papers read and the recorded C4 search, whose
  leads were all excluded at abstract level (author's decision).
- 2026-10-10. H2 and H4 enter as model rows only, M8 and M9 (the log
  coordinate and the drift at the distress boundary) and M10 (smooth fit at
  the trigger), with no novelty claim (author's decision). Guo and Wu (2009)
  is therefore not needed.
- 2026-10-10. C2 is narrowed to the two-branch result, that the ratio
  $\nu_1^2/\nu_3^2$ under the Basel cap carries no information about
  $\lambda_S$, and moves to Table 4. The constancy of $\nu_1^2$ and the log
  coordinate are stated as not claimed new (author's decision). Black and
  Perold (1992) and Taksar (2000) are excluded for the narrowed claim at
  abstract level: CPPI has one multiplier and no preference parameter
  [A]/[C]; Taksar's model is risk neutral, with exposure capped by full
  retention rather than a two-branch cap [C, from the 1999 preprint read by
  the search agent]. Taksar remains useful for a scope note on the
  log-reserve mechanism, not needed.
- 2026-10-10. The saturation limits are renamed $\nu_1^2,\nu_3^2$ (from
  PROOFS_v2's $\kappa_1^2,\kappa_3^2$) so that $\kappa$ means only the
  proportional issuance cost. Lean `CapGeometry.kappa2` became
  `CapGeometry.nu2`.

## Measurement map, 10 October 2026

- Sources read, pages only as listed in each `RECONSTRUCTION.md`. Basel III
  capital framework (rev June 2011), Basel III LCR (January 2013), FFIEC
  031/041 Schedule RC-R Part I instructions (June 2026).
- $a_1=0.045$ is the CET1 minimum (B3-Q1), though BCVW names the Tier 1
  ratio (B3-D1). $a_2=0.05$ is the stable retail deposit run-off (LCR-Q9).
  $a_3=0.30$ is no haircut of the standard (LCR-D2), so G6 is a
  calibration.
- $q$ is read as a leverage floor, $q=\ell_0/(1-\ell_0)$ (G7). The
  $z$-volatility and so K1 are free of $q$ (`MeasurementMap.zvol2_free_of_q`).
- Pending. The FDIC field names of the empirical scripts (RWAJT, RBCT1J,
  RBCRWAJ, ASSET, LIAB, EQCDIV, EQCSTKRX) need the FDIC data dictionary.
  Schedule RC-R items 12 to 18 and 27 to 29 (the adjustments) were not
  read. Banks electing the community bank leverage ratio report no
  risk-based ratio (RCR-D1).
- `lit/litcheck.py` now refuses a stated page that maps before the first
  PDF page. Before, such a page made `pdftotext` read the whole document.

## Decisions, 12 October 2026

- 2026-10-12. Computation on pinned data is a third evidence class, beside
  verbatim quotations and Lean proofs (author's decision). A computed claim
  needs its input data pinned by sha256 and committed or fetched by a
  committed script, its fields defined through a `lit/` record
  (`lit/fdic_2026` for the FDIC panel), a results file holding the exact
  output, and a pre-registration where any choice was involved.
  `checks/verify_measurement.py` (MM-7) re-runs every cited results file on
  its pinned data.
- 2026-10-11. The empirical test asks the narrower question: do distress
  recapitalisations behave like the model's impulse (author's decision).
  Pre-registered in `06_empirical_starter/PREREG_distress_targeting.md`
  (`d92b237d`). Result: exact targeting fails for both valid proxies
  (`results_distress_targeting.md`).
- 2026-10-12. `MEASUREMENT_MAP.tex` records what the FDIC panel could
  measure (results table). Fails: the corner (G2, G4), dividends only at
  the barrier (G14), exact targeting (G15). Tracked: G1, G3. Not measured:
  the rest, including $\sigma$ and every level that needs it.
- 2026-10-12. M15 added: for every policy that complies with the cap,
  binding or not, $\zeta\le\max\{\sigma_L^2,\nu_1^2\}$ everywhere and
  $\le\max\{\sigma_L^2,\nu_3^2\}+\varepsilon$ deep in surplus
  (`Bounds.lean`). It is the bound form of K1 and needs no optimality.
  K1 itself is unchanged, pending the author.
- 2026-10-12. M12 tested under `PREREG_persistence.md` (`2a56b918`).
  T1 (deficit level) fails: persistence 0.88 against 1.01, because banks in
  deficit shrink risk-weighted assets, which the model takes as exogenous.
  T3 (surplus level) fails: 0.9959 against 0.9789. T2 (fall in $g$) is not
  measured. Map rows G18 to G22.
- 2026-10-12. Direction (author). No paper built on a negative empirical
  result. The paper's claim is that its enhanced model explains the data
  better than a simple standard baseline, shown by a backtest fixed in
  advance. The empirical backtest and the model enhancement run in
  parallel. The failures found so far become the baseline's scorecard.
  Open: the baseline. BCVW was proposed as standard on no literature
  evidence. It is an arXiv preprint (v2, 2026) and calls its own main
  theorem "a robustness and extension result" of classical threshold
  models (BCVW pp. 3-4, read 2026-10-12). The baseline is chosen only
  after reading in full, as primary sources, the candidates BCVW cites
  (descriptions so far [C], from BCVW's reference list):
  Moreno-Bromberg and Rochet (2018, Princeton UP) [23]; Peura and Keppo
  (2006, J. Business) [25]; Lokka and Zervos (2008, IME) [21]; Bolton,
  Chen and Wang (2011, JF) [4]. The reasons the chosen baseline is
  standard enter `MANUSCRIPT.tex` as literature rows with quotations.
- 2026-10-12. Baseline candidates read in full: `lit/peura_keppo_2006`,
  `lit/lokka_zervos_2008`, `lit/bolton_chen_wang_2011` (with BCVW).
  Findings. (1) What is standard is the class, not one paper: a dividend
  barrier with costly issuance at a lower barrier. BCW-Q1 (the double
  barrier "is not new", Miller and Orr 1966), BCVW pp. 3-4 and PK-Q2
  (builds on Milne and Robertson 1996) say so. (2) With a fixed issuance
  cost proportional to size (PK-Q5, BCW-Q4, our K), issues are lumps to a
  single target (PK-D2, BCW-D1); with proportional costs only, minimal
  top-ups at zero (LZ-D1, BCW-Q8). Every member predicts dividends only at
  the barrier. So the FDIC failures G14 and G15 are failures of the whole
  class. (3) Peura and Keppo is the bank form of the class, published,
  calibrated to 61 US banks' capital ratios, with sigma defined on
  accounting returns (PK-D3). Proposed baseline: Peura and Keppo. (4)
  Novelty: PK already ran an implied-volatility calibration (PK-Q10,
  PK-Q15), so a sigma backtest is not new by itself; PK-Q18 calls for a
  graduated penalty below the requirement, the role of lambda_S.
  Decided (author, 2026-10-12, by starting the replication stages, and
  confirmed 2026-10-13): the baseline is Peura and Keppo (2006).
- 2026-10-12. Rule for the empirical paper (author). Where a paper's
  authors gave no empirical test, our test of its theory is new ground.
  Where they did, we replicate their analysis first, to strengthen our
  understanding and claims. Slow, verified route. Applied: Lokka and
  Zervos (no test) and Bolton, Chen and Wang (calibration only, and for
  non-financial firms, BCW-Q5) are new ground; Peura and Keppo (61 US
  banks, 1983-2002) is replicated first. The same rule applies to any
  later model that enters the comparison (Mankart, Michaelides and
  Pagratis 2020; Corbae and D'Erasmo 2021, both [A], not yet read).
- 2026-10-12. Peura and Keppo replication plan. Stage 1, model side:
  reproduce the printed barriers (Fig. 2: u1 0.90%, u2 3.66%, u0 3.80%;
  Table 2, twelve rows) from formulas (14)-(18). Stage 2, data side, their
  period: Table 1, Fig. 5 and Table 3 on FDIC Call Reports (from 1984;
  risk-weighted assets from 1990, earlier years imputed as in their n. 14),
  pre-registered, with every difference from Compustat recorded. Stage 3:
  the same method on 2015-2024. Stage 4: our tests of what they did not
  test (dividend timing, targeting, persistence).
- 2026-10-12. Working papers are accepted as the version read for
  Mankart, Michaelides and Pagratis (2020), Corbae and D'Erasmo (2021),
  Begenau, Bigio, Majerovitz and Vieyra (2025), De Nicolò, Gamba and
  Lucchetta (2014), Berger et al. (2008) and Flannery and Rangan (2008)
  (author). Each record names its version and attributes only its pages.
  Later the same day the author accepted working papers in general,
  including Kisin and Manela (2014 draft), Gropp, Mosk, Ongena and Wix
  (SAFE WP 156), Jokipii and Milne (BoF RDP 17/2006), Lepetit,
  Saghi-Zedek and Tarazi (HAL 2013), Valencia (IMF WP 08/248, 13/241)
  and Nuño and Thomas (ECB WP 1524).
- 2026-10-12. Ownership split of E2 and E3, after reading Holod and Peek
  and Houston, James and Marcus (subsidiaries' dividends and capital
  receipts are transfers with the parent). Run 1
  (PREREG_ownership_split.md): `HCTMULT` is never 0 and is constant within
  banks, so the stand-alone group was empty; recorded unchanged. Run 2
  (PREREG_ownership_split_2.md, groups from `RSSDHCR` counts per quarter):
  stand-alone banks fail E3 (56.0% pay below y*) and E2 (b_post +1.38,
  +1.39). E2 and E3 stand and now say so for stand-alone banks. Reported
  only: banks with no holding company pay below y* in 26.8% of
  bank-quarters; 70.5% of stand-alone recapitalisation comes from the
  parent. MS-7 now pairs each results file with the pre-registration it
  names.
- 2026-10-12. Records added: Gordon and Hirtle (2026), Berger et al.
  (2008). Peura and Keppo re-pinned to the JSTOR copy with a text layer
  (all 21 quotations match). Next in the reading order: Mankart,
  Michaelides and Pagratis, Hirtle (SR 666), Jokipii and Milne (2008),
  Baron (2020).
- 2026-10-12. Records added: Mankart, Michaelides and Pagratis (NBER SI
  draft, May 2016; not yet in Drive), Hirtle (SR 666), Jokipii and Milne
  (2008), Baron (2020). litcheck reads two-column sources in reading order
  when a suite sets `layout` to false. Mechanisms for the enhancement
  discussion found in the records: concave dividend value (MMP-Q4), a
  signalling cost of cutting dividends (H-Q4), asymmetric adjustment costs
  named but untested (JM-Q1, BDF-Q15), and recapitalisation to the minimum
  at a proportional cost (BAR-Q2, BAR-Q3). Next: Peura and Keppo
  replication, stage 2.
- 2026-10-12. Peura and Keppo stage 2 (their section V on Call Reports,
  1984-2002). Run 1 (PREREG_stage2.md): charter consolidations dominate
  the return moments; recorded unchanged. Run 2 (PREREG_stage2_2.md, an
  asset-doubling screen, written after run 1 and saying so): the 61
  largest national banks reproduce F3 to F7, the model-side findings
  (barrier driven by σ; buffers track the barrier among high-provision
  banks with slope below one and positive intercept; the model
  underpredicts capital). F1, their negative μ-σ correlation, reproduces in
  no run. Next: stage 3, the same method on 2015-2024.
- 2026-10-12. Peura and Keppo stage 3 (PREREG_stage3.md; windows
  2015-2024 and 2005-2024). F3, F4 and F7 hold in every sample and window;
  F1 in none, including the window with the 2008-2010 crisis. F5 fails
  mostly on its slope and intercept conditions with few banks. In 2015-2024
  half the 61 largest banks have no issue barrier at K = 0.25% (μ/σ ≥ 2.6):
  the model's optimum for them is never to issue. Next: stage 4, our tests
  of what Peura and Keppo did not test.
- 2026-10-12. Peura and Keppo stage 4 (PREREG_stage4.md), the baseline's
  scorecard on 2015-2024 with bank-specific barriers: T1 (dividends below
  the barrier) holds only because capital sits far above the barrier;
  T2 (capital far above the barrier, 93-94% of bank-quarters), T3 (issues
  start near u1, 6-7%) and T4 (issues land near u2, 8-11%) fail, in both
  samples. The four shares are the numbers an enhanced model is scored
  against. Open, for the author: whether to add the scorecard as an
  empirical row of MANUSCRIPT.tex, and which enhancement to solve first.
- 2026-10-12. Plain-English summary of the model (306 words) at the head
  of the Model section, and a terminology table in MEASUREMENT_MAP.tex with
  one plain name per object (author's request). Checks MM-8 to MM-11 keep
  them consistent. Open, for the author: the Proposed names (assets-to-
  liabilities ratio, capital buffer, buffer variance, deficit limit,
  surplus limit, asset volatility, expected asset return, liability
  volatility, return-liability correlation). Once agreed, their replaced
  names (log coordinate, z-volatility, saturation limits, deep in deficit,
  capital buffer's volatility) leave K1, M2, M3, M8 and M15, the
  manuscript's Terminology section takes the map's names, and the
  retired-name check extends from the summary to the whole manuscript.
- 2026-10-13. The author agreed the nine Proposed names; all are now
  Agreed. The retired names left K1, M2, M3, M8, M15, C1, C2, the
  appendices and map row G7 and G16. The manuscript's Terminology section
  uses only the map's names (MM-13) and gained the assets-to-liabilities
  ratio, capital buffer, distress boundary, the two branches, the crossing
  point and the four volatility and return names. The map gained rows for
  W, N and the impulse operator, which the glossary already used. MM-12
  checks retired names across both documents, exempting literature rows and
  sentences that cite a paper (C2 keeps Barberis et al.'s own "loss
  aversion"). writing_discipline.md section 6 records the new names.
  Alternatives may be considered later (author).
- 2026-10-13. Comparison protocol, decisions (author). The two threads are
  named the testing thread (the backtest) and the enhancement thread (the
  enhanced models); both are recorded in the one manuscript. Scorecard:
  stage 4's T1 to T4 with their pre-registered tolerances. Verdict per
  test by a margin of 10 points on the share. Sample B primary, A
  reported. Enhancements' new parameters from regulation and a pre-fixed
  grid, never chosen on 2015-2024; same mu_i, sigma_i as the baseline.
  Enhancements nested, in several passes. A numerical HJB solve is
  accepted as computation. 2025 stays sealed. Open before
  PREREG_comparison.md: the verdict rule across the grid, the reference R
  (8% or 10.5%), and whether the enhancement keeps Peura and Keppo's issue
  delay (PK-Q3, PK-Q8) so that it nests the baseline.
- 2026-10-13. Direction (author). We find the empirical data of the
  background papers and implement those papers ourselves, as with Peura
  and Keppo. The testing thread then scores every implemented paper on one
  common dataset that we choose. No time limit. First step: a data
  inventory of the records with bank-level empirical work (Baron; Mankart,
  Michaelides and Pagratis; Bayraktar et al.; Berger et al.; Gordon and
  Hirtle; Hirtle; Jokipii and Milne; Holod and Peek; Houston, James and
  Marcus), each source quoted from its data section.
- 2026-10-13. Grid verdict rule and reference level are decided once the
  papers are implemented, as a discussion of findings (author). Setting new
  parameters on a 2005-2014 training window (b) is preferred, one value
  fixed from the literature (c) acceptable if it does the job. The
  reference level is open. The decisive comparison runs on the sealed
  hold-out, so exploration on 2015-2024 does not spend it.
- 2026-10-13. The enhancement thread keeps Peura and Keppo's issue delay
  (Delta = 0.5 years; PK-Q3, PK-Q8), as established literature (author).
  The first enhancement is their model with the shortfall penalty added,
  which must return their barriers at lambda_S = 1. Delta = 0 may be
  explored, and it is the bridge to the manuscript's instantaneous model.
- 2026-10-13. The common dataset is banks, Call Reports (author): the
  paper stays with banks. Papers built on holding companies (Berger et
  al., Gordon and Hirtle, Hirtle) are replicated on their own FR Y-9C
  data, then their findings are tested on the bank data. Data inventory in
  `lit/DATA_INVENTORY.md`. WRDS (Compustat, CRSP) is open: the author
  would obtain access if needed.
- 2026-10-13. FDIC API coverage, probed on the first 2,000 banks of each
  report date (indicative; the pre-registered fetch will state the full
  counts). No registration. Capital, income and dividends are quarterly
  from 1984; risk-weighted assets from 1990. The equity-change flows
  (`EQCSXQ`, `EQCBHCTR`, `EQCMRG`) are reported by every bank in December
  from 1984, but in mid-year quarters by only about a fifth to a third of
  banks until 2000 (small banks filed the schedule annually, as HP-A3
  says), and by every bank quarterly from 2001. FFIEC CDR bulk files
  answer (HTTP 200); the FFIEC NIC Y-9C download page refuses scripted
  access (HTTP 403).
- 2026-10-13. Common bank dataset fetched and pinned
  (`common_data/`, PREREG_common_data.md, results_common_data.md): 41
  yearly files, 1984-2024, 1,656,057 rows, 39 fields. All five coverage
  expectations hold; a same-day refetch of 2024 is byte-identical. From
  2020 about 35-38% of commercial banks report RWAJT = 0 (community bank
  leverage ratio), so tests treat RWAJT <= 0 as missing. RBCT2 starts in
  1990, EQCTRSTX in 2001. MM-14 reproduces the coverage on every run.
  Next: Gordon and Hirtle (needs FR Y-9C, possibly a manual download),
  or Mankart, Michaelides and Pagratis, which runs on this dataset.
- 2026-10-13. Gordon and Hirtle, stage 1, pre-registered
  (`replication/gordon_hirtle_2026/PREREG_stage1.md`, 799cfea6) with the
  MDRM dictionary pinned. Y-9C December files 1990-2020 and CPI fetched by
  fetch_stage1.py and pinned. A development run of stage1.py on 1990-2020
  was made and its output seen, to test the code before the 2021-2024
  files (manual FFIEC download by the author) arrive. Code changes after
  it: standard errors printed to four decimals, to confirm that two-way
  clustering differs from one-way (it does, 0.0316 against 0.0319; their
  columns 1 and 2 also share a standard error). No pre-registered choice
  changed. The results file is written only from the full 1990-2024 run.
- 2026-10-13. Gordon and Hirtle stage 1 run (results_stage1.md). The FFIEC
  files carry no holder fields, so top-tier for 2021-2024 comes from the
  NIC relationships file under PREREG_stage1_2.md (48a74626); it agrees
  with RSSD9348 on 99.0% of 2015-2020 bank-years. 20 of 22 targets
  reproduce: the typical-year components, median issue, Table 1 means and
  all of Table 2 (first-lag coefficients 0.224, 0.224, 0.229, 0.106 against
  0.201, 0.201, 0.208, 0.097). Differ: dividend-paying share 84.9% against
  "more than 85 percent", and 7,843 bank-years against 6,448 (+21.6%),
  source not identified. Open for the author: whether to investigate the
  sample difference under a new pre-registration, and stage 2 (their
  findings on the common bank dataset).
- 2026-10-13. Gordon and Hirtle sample-size difference
  (PREREG_stage1_3.md, PREREG_stage1_4.md; results_stage1_3.md,
  results_chart2.md). C1, bank holding companies only, drops 319 savings
  and loan holding companies and 16 intermediate holding companies (the
  2012 jump) and is adopted: primary sample 7,508 bank-years, and the
  dividend share now reproduces (85.4%). Their Chart 2, read by pixel,
  sits 11 to 48 a year below our C1 counts in every year, 30 on average,
  evenly over time; C2 (GDP deflator) and C3 (continuity) do not close it
  and are not adopted. The first draft states our rule, theirs as
  written, and the remaining gap; every result except the count reproduces
  in every candidate sample. Writing to the authors is optional, not a
  dependency (author).
- 2026-10-12. Later option, not the primary goal (author). A sigma
  backtest: solve the model on a grid of sigma and report, for each
  observable, the band of sigma consistent with it. Only the
  sigma-dependent claims enter (band location, the corner, volatility
  levels). Exact targeting and dividends below the barrier fail at every
  sigma and stay out. The test is whether the bands from different
  observables overlap. M15 turns book volatility into a lower bound on
  sigma, provided book volatility understates economic volatility. Cost:
  one solve per grid point, about two hours each.
- Open, for the author. Whether to reformulate the model claims as bounds
  that need no optimality (the cap bounds exposure and the $z$-volatility
  near each end), and whether to narrow theory and empirics to measurable,
  scale-free implications. An Empirical analysis section of
  `MANUSCRIPT.tex` (E rows, headlines allowed to cite them) waits on this.

## Carried from the retired documents, 11 October 2026

From the reader TODO and the starter pack, retired at pin `4f57aab1`
(`RETIREMENT.md`). None of these carries a row.

- B3. Angelis (JEEA 2025). Full text blocked, embargoed to 15 Oct 2026, so
  citable at abstract level only. Needed only if a row comes to rest on it.
- C1. A K-sweep recording both thresholds directly, six solves at
  K in {0, 0.001, 0.005, 0.01, 0.02, 0.05}. Illustration only.
- C2. Convergence certification of the H-operator sweep. Illustration only.
- F1. The KSW issuance question. The quarterly issuance rate (11.09%)
  exceeded the annual rate (6.6%). Candidate cause, from the FDIC
  definition of `EQCSTKRX`: banks under $100 million file it only in
  December, and the field is net of buybacks and conversions.
  `issuance_diagnostic.py` has not been run on real data.
- The response d(level)/dK is not recorded and needs the solve files.
- The retired summaries README left two bibliography items open (the page range
  of Kahneman and Tversky, the volume and pages of `cao2026`). Neither is
  cited by `MANUSCRIPT.tex`; they matter only if one is cited.

## Bundle mechanics

- The MS-5 control resolves its literature row against the real record
  `lit/bayraktar_2026` (switched from a fixture on 2026-10-10).
- Every literature suite uses `lit/litcheck.py`. It checks each quotation on
  its stated page of the sha256-pinned PDF, after NFKC normalisation and
  rejoining hyphens broken across lines, and builds and audits the paper's
  Lean file.
- C1 moved to Table 4 on 2026-10-10. It rests on M1 to M3 and on L1, read in
  full, and claims nothing new. The novelty claim is C2, in Table 5.
- PROOFS_v2 retired 2026-10-10 (pin `1d1a56f5`). `EMPIRICAL_v2.tex` now
  cites rows M1, M3 and the solver illustration, and
  `00_reader/ledger_empirical.py` resolves them against `MANUSCRIPT.tex`.
  `run_all.sh` and `VERIFY_NOW.sh` no longer compile PROOFS_v2.
- The older author documents that cited PROOFS_v2 numbering were retired
  on 2026-10-11 (author's decision, pin `4f57aab1`). `RETIREMENT.md` maps
  each to its new home. Their open items are under "Carried from the
  retired documents" below. `HANDOVER_20261010.md`, the live handover of
  the empirical companion, is kept.
- `EMPIRICAL_v2.tex` has issues that predate the retirement: an undefined
  reference `sec:attrition` and overfull lines at its lines 37-41, 122-133,
  215 and 275-287.

## Pass 3 inventory

From `00_reader/proof_registry.py`. Label, PROOFS_v2 code, current verifier.

SymPy, to port to Lean:
`thm:lambda4` FPV, `prop:robust` ROB, `prop:egarch` EGE, `prop:sojourn` SJT,
`prop:rrL` CCP, `prop:statemap` TSO, `cor:boundary` CDA, `prop:satlimits` SCG,
`prop:tcs` TCS, `prop:kcs` KCS, `cor:idn` IDN, `prop:soc` SOC.

- Done 2026-10-10: `thm:lambda4` and `prop:robust` are row M1;
  `prop:statemap` (ii) and `prop:satlimits` are rows M2 and M3
  (`CapGeometry.lean`). The identification of the discrete-time $\lambda_V^4$
  with $\nu_1^2/\nu_3^2$ is a definition, recorded in Appendix D, and no
  theorem links the discrete recursion to the diffusion.
- Done 2026-10-10: `prop:tcs`, `prop:kcs`, `cor:idn` are M4 to M6
  (`Identification.lean`). `prop:soc` (ii) is M7 (`SecondOrder.lean`).
  `prop:soc` (i), strict $V''(y_{\text{post}})<0$, is refuted as derived
  (Appendix E, R1, `SecondOrder.refuted_target_strict`): a sign change of
  $\varphi'$ allows $\varphi''=0$ at a zero of odd order. It stays a
  hypothesis of M5, and C3 does not use it.
- Done 2026-10-10: `prop:nesting` and `prop:concave-payoff` are M11
  (`QVI_Part1`); `prop:identity` and `prop:persistence` are M12, with
  `RateBased.balanced_growth_iff` the Lean statement of ACI; `prop:sojourn`
  is M13; `lem:fin` is M14 (`ImpulseCount`). `prop:persistence` as stated in
  PROOFS_v2 ("vanishes with g") is refuted (Appendix E, R2): the asymmetry
  tends to $\delta_p$. Every Lean file now has a control.
- Still without a row: `prop:egarch` (EGE, Gaussian moments), and the
  hypotheses `lem:reg`, `lem:bdr`, `lem:itr`, `prop:lowerbound`, `prop:ver`,
  which enter Appendix B. The numerical remarks stay illustrations.
- `prop:sojourn` is already proved in `RateBased.lean`
  (`sojourn_strictAnti`, `sojourn_tendsto_atTop`).
- `thm:lambda4` rests on `RateBased.saturated_ratio_tendsto`, now free of
  `sorry`. What remains is the link from the recursion `eq:zrec` to `Vresp`.
- `prop:egarch` rests on Gaussian moment identities. Check what Mathlib
  carries before scoping.

Already Lean: `prop:identity` ACI, `prop:persistence` PSA, `prop:nesting` NST,
`prop:concave-payoff` RPC, `prop:smf` SMF. The registry names no theorem for
`prop:identity`, so its Lean statement must be located or written.

Lean-partial, Danskin step to enter as a hypothesis: `lem:envelope` ENV,
`lem:envelopeK` ENVK, `rem:smfn` SMFN, `lem:fin` FIN.

Hypotheses: `lem:reg` REG, `lem:bdr` BDR, `lem:itr` ITR, `prop:lowerbound`
LQB, `prop:ver` VER.

Illustrations: `rem:solver` SLV, `rem:capbinds` CBI, `rem:compstat` CSL,
`rem:nonconcave` LNC, `rem:Ksens` KSW, `rem:tcsn` TCSN, `rem:kcsn` KCSN,
`rem:socn` SOCN, `rem:vern` VERN.

Every file under `lean/mathlib/` has at least one `control_*` theorem (2026-10-10).

## C2 search, 10 October 2026

Run by a web-search agent; every entry is a lead, not a finding. Triage at
exploration level ([A] abstract or search summary, [C] citing description),
by the author's rule of 2026-10-10 that primary sources are needed only for
verification and novelty checks.

- Survived triage for the first wording of C2: Black and Perold (1992).
  Excluded for the narrowed C2 (Decisions, 2026-10-10).
- Excluded [A]: Décamps, Mariotti, Rochet and Villeneuve (2011), stock-price
  volatility from issuance costs, no capped capital-ratio state. Hilscher,
  Raviv and Wiener (2024), a volatility schedule imposed by regulation, not
  derived from a cap. Dudziak and Schmidli (arXiv:2510.25494), no investment
  cap and no volatility analysis. Milne and Whalley (SSRN 303176), no
  computation of the state's volatility seen.

Leads as reported:

- Black and Perold (1992), "Theory of constant proportion portfolio
  insurance", *Journal of Economic Dynamics and Control* 16(3):403-426. The
  solvency branch $u=(x-1)/a_1$ is a CPPI rule with floor 1 and multiplier
  $1/a_1$, and under CPPI the log cushion has constant volatility. If the
  reading confirms it, the constancy of $\nu_1^2$ is CPPI geometry and C2's
  novelty rests on the two-branch limits, the deposit-shock terms and the
  silence of $\lambda_S$.
- Décamps, Mariotti, Rochet and Villeneuve (2011), "Free Cash Flow, Issuance
  Costs, and Stock Prices", *Journal of Finance* 66(5):1501-1544. Volatility
  asymmetry from financing frictions with risk-neutral shareholders.
- Hilscher, Raviv and Wiener (2024), "Dynamic volatility regulation of
  financial institutions", *Finance Research Letters* 61, doi
  10.1016/j.frl.2023.104968.
- Dudziak and Schmidli, "Stochastic Control of Dividends with a Drawdown
  Penalty", arXiv:2510.25494.
- Lower: Milne and Whalley, "Bank Capital and Incentives for Risk-Taking",
  SSRN 303176. Checked by the agent as likely not relevant: Peura and Keppo
  (2006), Bolton, Li, Wang and Yang (2025), Hugonnier and Morellec (2017),
  Klimenko, Pfeil, Rochet and De Nicolò, Chevalier, Ly Vath and Roch (2020).

## C4 search, 10 October 2026

Exploration level only ([A] abstract, publisher page or author slides read,
[C] citing description). The search agent warned that a page summariser
invented comparative statics for one source, so only raw text counted.

- Part (iii), joint local identification of a preference parameter and a
  fixed issuance cost from two thresholds: no lead found that could contain
  it. Excluded [A]: Peura and Keppo (2006, *Journal of Business*
  79(4):2163-2202), calibrated to bank data but no preference parameter;
  Hugonnier and Morellec (2017); Décamps, Gryglewicz, Morellec and Villeneuve
  (2017, RFS); Løkka and Zervos (2008), proportional costs only; Eisenberg
  and Schmidli (2011); Xu and Woo (2020); Xu (2023); Constantinides and
  Richard (1978); Lakner and Reed (arXiv:2206.04107); Attanasio (2000) and
  Aguirregabiria (1999), reduced-form or retail (S,s) estimation with no
  shortfall preference.
- Parts (i) and the target/gap-in-$K$ half of (ii), which C4 does not claim
  as new but a headline built on M4 and M5 would: possibly in Décamps,
  Mariotti, Rochet and Villeneuve (2011, *Journal of Finance*
  66(5):1501-1544), fixed and proportional issuance costs, issuance at zero
  cash; Bolton, Chen and Wang (2011, *Journal of Finance* 66:1545-1578),
  fixed cost reported from memory by the agent, unconfirmed; Zhou and Yuen
  (2015, *ASTIN Bulletin*), injection size rising in $K/l$, numerically.
  Lower: Zhu (2017, *ASTIN Bulletin* 47(1):239-268); Yao, Yang and Wang
  (2011, *EJOR* 211(3):568-576). None has a chosen recapitalisation trigger
  that moves with a preference parameter. Primary sources are needed only if
  a headline claims (i) or (ii) as new.
- The envelope identities $\partial_KV=-N$, $\partial_{\lambda_S}V=-W$: no
  source found stating them; likely standard, never claimed new.

## H2 and H4 search, 10 October 2026

Exploration level only.

- H4, smooth fit at the impulse trigger, is a standard result. Guo and Wu
  (2009), "Smooth Fit Principle for Impulse Control of Multidimensional
  Diffusion Processes", *SIAM Journal on Control and Optimization*
  48(2):594-617, doi 10.1137/080716001, prove $C^1$ regularity across the
  intervention boundary by viscosity methods [A]; Davis, Guo and Wu
  (arXiv:0912.3297) extend it [A]. In one dimension it goes back to Richard
  (1977) and Harrison, Sellke and Taylor (1983) [A]/[C]. What remains
  specific here is the local setting (a diffusion degenerate at $x=1$, a
  singular dividend control) and the proof route (no convex kink). H4 is
  not a novelty headline. A scope note saying it is known needs Guo and Wu
  as a primary source.
- H2, the degeneracy at $x=1$ as an artefact of the level coordinate. Taksar
  (2000), "Optimal risk and dividend distribution control models for an
  insurance company", *Mathematical Methods of Operations Research*
  51(1):1-42, doi 10.1007/s001860050001: under the optimal policy the reserve
  "behaves like a logarithmic Brownian motion" near 0 and bankruptcy time is
  infinite (the agent read the 1999 SFB 303 preprint). Same mechanism as H2,
  and as the constancy of $\nu_1^2$ in C2. Behind it, Højgaard and Taksar
  (1999), *Mathematical Finance* 9(2):153-182 [A]. The CPPI literature uses
  the log cushion as standard (Dupret and Hainaut 2021 [A]). With BCVW
  Remark 3.1, H2 is not a novelty headline.
- Taksar (2000) is therefore also a lead for C2, alongside Black and Perold
  (1992).
- Excluded [A]: He and Liang (2009), Kulenko and Schmidli (2008),
  Bolton-Chen-Wang (2011), Décamps et al. (2011), Peura and Keppo (2006),
  Junca, Moreno-Franco and Pérez (arXiv:1808.02182), Cadenillas, Sarkar and
  Zapatero (2007), all with recapitalisation at a fixed boundary or impulses
  on dividends; Hugonnier and Morellec (2017), Brigo and Vrins, Azcue and
  Muler (2010), De Angelis (arXiv:1805.12035), Huang (arXiv:2107.02242).

## Novelty ledger

No result is claimed new yet. Candidates, from the numbered results of
`00_document/PROOFS_v2.tex`. "Rests on" names PROOFS_v2 labels; "Lean" says
what exists or must be ported; "Reading" says what novelty waits on. A
candidate whose reading list says "search" has no identified paper yet, and
the search itself is the pending item.

| ID | Candidate claim | Rests on | Lean | Reading |
|---|---|---|---|---|
| H1 (C1 in Table 4, C2 in Table 5; M1 to M3, L1 to L3) | The variance ratio $\lambda_V^4=\nu_1^2/\nu_3^2$ is a function of $(\sigma,\sigma_L,a_1,a_3,c)$ only, so it carries no information about the asymmetry parameter $\lambda_S$. | `thm:lambda4`, `prop:robust`, `prop:statemap`, `prop:satlimits` | done, `RateBased`, `CapGeometry` | read: `bayraktar2026` (defines the cap, no limits), `barberis2001` (level of loss aversion silent in return volatility, cited in L2), `engle2018` (decomposition of a news asymmetry). `li2023` read (preference reaches the limits of an unconstrained control, L4). `barberis1999` (working paper) read too. Waiting: a search for papers that evaluate limits of state volatility under a regulatory cap on the risky position |
| H2 (M8, M9 from 2026-10-10, no novelty claim) | The degeneracy of the diffusion at the distress boundary $x=1$ is a coordinate artefact. In $z=\log((x-1)/q)$ the volatility is constant on the solvency regime. | `prop:rrL`, `prop:statemap`, `cor:boundary` | port CCP and TSO (i) to (iii); `BCVW.diffusion_vanishes_at_one`, `BCVW.drift_at_one` exist | `bayraktar2026` read: Remark 3.1 (p. 11) states the degeneracy at $y=1$ and the drift $r-r_L$. The paper never changes coordinate, so only the artefact reading can be new. Search for degenerate-diffusion bank capital models |
| H2b | $x=1$ is unreachable if and only if $r>r_L$. | `cor:boundary` | none possible as stated: boundary classification of a diffusion is not in Mathlib. Enters as a hypothesis unless the scale-function step is formalised | `bayraktar2026` Remark 3.1 already states "locally repelling when r > rL" (BCVW-Q3). Not new as a local statement |
| H3 (C3 in Table 4, C4 in Table 5; M4 to M6, L5, L6 from 2026-10-10) | $(\lambda_S,K)$ is locally identified from the pair of thresholds $(y^*,x_L)$, because the two parameters move the trigger in opposite directions. | `lem:reg`, `lem:envelope`, `lem:bdr`, `prop:tcs`, `lem:envelopeK`, `prop:kcs`, `cor:idn`, `prop:soc` | Envelope cores exist (`Envelope.*`); port TCS, KCS, IDN, SOC algebra; REG and BDR enter as hypotheses | `bayraktar2026` read: no fixed issuance cost, and it states one would generally break the one-dimensional reduction (BCVW-Q4). Appendix D must state that PROOFS_v2's $K$ is charged per unit of liabilities when $K$ enters a row. Search for comparative statics of impulse-control thresholds in a fixed issuance cost; `altinkilic2000`, `buhner2002` for the cost structure only |
| H3b (M5) | The fixed issuance cost widens the inaction region at both ends, and the trigger-to-target gap $y_{\text{post}}-x_L$ is strictly increasing in $K$. | `prop:kcs`, `lem:envelopeK` | as H3 | as H3 |
| H4 (M10 from 2026-10-10, no novelty claim) | Smooth fit holds at the recapitalisation trigger, $V'(x_L^-)=V'(x_L^+)=1+\kappa$. | `prop:smf`, `rem:smfn`, `lem:itr` | `SmoothFit.smooth_fit` exists; ITR and the viscosity supersolution property enter as hypotheses | Øksendal and Sulem, *Applied Stochastic Control of Jump Diffusions*, Thm 9.7 and Ch. 9, and Crandall, Ishii and Lions (1992), both cited in the header of `SmoothFit.lean` for the supersolution property and unread; search for smooth fit at impulse-control boundaries. High risk that this is known. `bayraktar2026` read: smooth fit only at the dividend barrier, and its recapitalisation threshold is exogenous (BCVW-Q6) |
| H5 | At $\lambda_S=1$ the problem is the risk-neutral benchmark exactly, and the running payoff $-\Lambda$ is concave for every $\lambda_S\ge1$, unlike an S-shaped penalty. | `prop:nesting`, `prop:concave-payoff` | `QVI_Part1.lambda_asymmetry_vanishes_at_one`, `QVI_Part1.neg_Lambda_concave` | `kahneman1979` unread. `barberis1999` and `barberis2001` read: the loss-aversion term is already kinked-linear (BHS-Q4, QJE-Q4), so the kinked-linear form is not new and the contrast is only with S-shaped curvature. `li2023` read: the S-shaped case that needs a concave envelope (LYZ-Q3). Positioning rather than novelty |
| H6 | The discrete-time recursion is observationally equivalent to an EGARCH-class specification conditioned on the state level, so $\lambda_V$ is estimable from returns without observing $Q_t$, given $\theta$. | `prop:egarch` | port; rests on Gaussian moment identities, Mathlib coverage unchecked | Nelson (1991); `engle2018` |
| H7 | Balanced growth forces $r_E=g/b$, and the persistence asymmetry $\phi^--\phi^+$ vanishes with $g$. | `prop:identity`, `prop:persistence` | `RateBased.persistence_asymmetry_*`, `phi_minus_*`; the theorem for ACI is unnamed | search; likely a known accounting identity (sustainable growth). Probably a lemma, not a headline |

Results outside every candidate: `prop:sojourn` (SJT), `prop:lowerbound`
(LQB), `prop:ver` (VER), `lem:fin` (FIN). They support the candidates as
lemmas or hypotheses.

Every headline that rests on `prop:ver` (H3, H3b, H4) holds for the value
function only under the verification hypothesis, which Lean does not carry.
The scope note of each such headline says so.

Bib entries in `00_document/references.bib` not yet tied to a candidate:
`armstrong2019`, `chen2026`, `cao2026`, `angelis2025`.
