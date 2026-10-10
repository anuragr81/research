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
| 5 | Appendices B to E; `MEASUREMENT_MAP.tex` | |
| 6 | Retire `00_document/PROOFS_v2.tex` with `RETIREMENT.md` and its check | |
| 7 | Novelty reading, one `lit/` record per paper | in progress; `lit/bayraktar_2026`, `lit/barberis_huang_santos_1999`, `lit/barberis_huang_santos_2001`, `lit/engle_siriwardane_2018`, `lit/li_yu_zhang_2023` done 2026-10-10 |
| 8 | Conclusions, then introduction; headline overreach review | |
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
- 2026-10-10. The saturation limits are renamed $\nu_1^2,\nu_3^2$ (from
  PROOFS_v2's $\kappa_1^2,\kappa_3^2$) so that $\kappa$ means only the
  proportional issuance cost. Lean `CapGeometry.kappa2` became
  `CapGeometry.nu2`.

## Bundle mechanics

- The MS-5 control resolves its literature row against the real record
  `lit/bayraktar_2026` (switched from a fixture on 2026-10-10).
- Every literature suite uses `lit/litcheck.py`. It checks each quotation on
  its stated page of the sha256-pinned PDF, after NFKC normalisation and
  rejoining hyphens broken across lines, and builds and audits the paper's
  Lean file.
- C1 moved to Table 4 on 2026-10-10. It rests on M1 to M3 and on L1, read in
  full, and claims nothing new. The novelty claim is C2, in Table 5.
- `00_reader/TODO.md` and `04_reproduce/run_all.sh` belong to the PROOFS_v2
  layer and retire with it at pass 6. The root `README.md` "Lean status"
  section still describes the `sorry` closed on 2026-10-10 and the old
  `01_theory/lean_project/` path. It is rewritten or retired at pass 6.

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

No file under `lean/mathlib/` has a `control_*` theorem yet. Every cluster
needs one.

## C2 search, 10 October 2026

Run by a web-search agent; every entry is a lead, not a finding. Triage at
exploration level ([A] abstract or search summary, [C] citing description),
by the author's rule of 2026-10-10 that primary sources are needed only for
verification and novelty checks.

- Survives, primary source needed for C2: Black and Perold (1992).
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
| H2 | The degeneracy of the diffusion at the distress boundary $x=1$ is a coordinate artefact. In $z=\log((x-1)/q)$ the volatility is constant on the solvency regime. | `prop:rrL`, `prop:statemap`, `cor:boundary` | port CCP and TSO (i) to (iii); `BCVW.diffusion_vanishes_at_one`, `BCVW.drift_at_one` exist | `bayraktar2026` read: Remark 3.1 (p. 11) states the degeneracy at $y=1$ and the drift $r-r_L$. The paper never changes coordinate, so only the artefact reading can be new. Search for degenerate-diffusion bank capital models |
| H2b | $x=1$ is unreachable if and only if $r>r_L$. | `cor:boundary` | none possible as stated: boundary classification of a diffusion is not in Mathlib. Enters as a hypothesis unless the scale-function step is formalised | `bayraktar2026` Remark 3.1 already states "locally repelling when r > rL" (BCVW-Q3). Not new as a local statement |
| H3 (C3 in Table 4, C4 in Table 5; M4 to M6, L5, L6 from 2026-10-10) | $(\lambda_S,K)$ is locally identified from the pair of thresholds $(y^*,x_L)$, because the two parameters move the trigger in opposite directions. | `lem:reg`, `lem:envelope`, `lem:bdr`, `prop:tcs`, `lem:envelopeK`, `prop:kcs`, `cor:idn`, `prop:soc` | Envelope cores exist (`Envelope.*`); port TCS, KCS, IDN, SOC algebra; REG and BDR enter as hypotheses | `bayraktar2026` read: no fixed issuance cost, and it states one would generally break the one-dimensional reduction (BCVW-Q4). Appendix D must state that PROOFS_v2's $K$ is charged per unit of liabilities when $K$ enters a row. Search for comparative statics of impulse-control thresholds in a fixed issuance cost; `altinkilic2000`, `buhner2002` for the cost structure only |
| H3b (M5) | The fixed issuance cost widens the inaction region at both ends, and the trigger-to-target gap $y_{\text{post}}-x_L$ is strictly increasing in $K$. | `prop:kcs`, `lem:envelopeK` | as H3 | as H3 |
| H4 | Smooth fit holds at the recapitalisation trigger, $V'(x_L^-)=V'(x_L^+)=1+\kappa$. | `prop:smf`, `rem:smfn`, `lem:itr` | `SmoothFit.smooth_fit` exists; ITR and the viscosity supersolution property enter as hypotheses | Øksendal and Sulem, *Applied Stochastic Control of Jump Diffusions*, Thm 9.7 and Ch. 9, and Crandall, Ishii and Lions (1992), both cited in the header of `SmoothFit.lean` for the supersolution property and unread; search for smooth fit at impulse-control boundaries. High risk that this is known. `bayraktar2026` read: smooth fit only at the dividend barrier, and its recapitalisation threshold is exogenous (BCVW-Q6) |
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
