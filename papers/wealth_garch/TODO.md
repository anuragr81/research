# TODO

The skeleton follows `../MANUSCRIPT_SKELETON.md`, with
`../jite/endogenous_entry/entry_contest/` as the reference bundle.

## PLAN

| Pass | Work | Status |
|---|---|---|
| 1 | Scaffold: four empty tables, ID scheme, appendix stubs, `checks/verify_manuscript.py` with controls, `verify.sh` | done 2026-10-10 |
| 2 | Lean project `lean/mathlib/` (Lean v4.32.0-rc1, Mathlib v4.32.0-rc1), every file a build root, axiom audit in `verify.sh` | done 2026-10-10; the `sorry` in `RateBased.saturated_ratio_tendsto` closed by `RateBased.tendsto_tanh_atTop` |
| 2a | Candidate claims: every headline PROOFS_v2 makes is listed in the novelty ledger below, with the results it rests on, the Lean it waits on and the reading its novelty waits on. A candidate enters Table 5 as a C row once its first model row exists, since MS-3 refuses a C row with no row to point to. This list drives passes 3 and 7 | done 2026-10-10 |
| 3 | Port the analytic steps to Lean, one cluster per pass, with `control_*` theorems and counterexamples (inventory below) | |
| 4 | Model rows and Appendix A proofs, one family per pass | |
| 5 | Appendices B to E; `MEASUREMENT_MAP.tex` | |
| 6 | Retire `00_document/PROOFS_v2.tex` with `RETIREMENT.md` and its check | |
| 7 | Novelty reading, one `lit/` record per paper | |
| 8 | Conclusions, then introduction; headline overreach review | |
| 9 | Literature table | |
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

## Waiting on the author

1. Symbol $\kappa$. The fixed terms reserve bare $\kappa$ for the proportional
   issuance cost, while PROOFS_v2 and the manuscript write the saturation
   limits as $\kappa_1^2,\kappa_3^2$. The subscript keeps them apart on the
   page, but they share a letter. Keep, or rename the saturation limits.
   Recommendation: rename them before the prose pass, since $\kappa$ and
   $\kappa_1$ will sit in the same sentences once the issuance cost enters
   the rows.

## Bundle mechanics

- The MS-5 control resolves its literature row against a fixture record
  (`Fixture2000`) because `lit/` has no record yet. Switch the control to a
  real record when the first one exists.
- `refs.bib` is empty and `MANUSCRIPT.tex` has no `\bibliography` until the
  first L row, since bibtex fails on a document with no citation.
- `lit/verify_lit.sh` fails with "no paper suites ran" until the first
  record exists.
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
  with $\kappa_1^2/\kappa_3^2$ is a definition, recorded in Appendix D, and no
  theorem links the discrete recursion to the diffusion.
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

## Novelty ledger

No result is claimed new yet. Candidates, from the numbered results of
`00_document/PROOFS_v2.tex`. "Rests on" names PROOFS_v2 labels; "Lean" says
what exists or must be ported; "Reading" says what novelty waits on. A
candidate whose reading list says "search" has no identified paper yet, and
the search itself is the pending item.

| ID | Candidate claim | Rests on | Lean | Reading |
|---|---|---|---|---|
| H1 (C1; M1, M2, M3 from 2026-10-10) | The variance ratio $\lambda_V^4=\kappa_1(c)^2/\kappa_3(c)^2$ is a function of $(\sigma,\sigma_L,a_1,a_3,c)$ only, so it carries no information about the asymmetry parameter $\lambda_S$. | `thm:lambda4`, `prop:robust`, `prop:statemap`, `prop:satlimits` | `RateBased.saturated_ratio_tendsto` (one `sorry`), `saturated_ratio_lambda4`; port TSO (ii) and SCG | `engle2018` (in `references.bib`); Nelson (1991) EGARCH, named in PROOFS_v2 text but not in the bib; search for state-dependent volatility of bank capital |
| H2 | The degeneracy of the diffusion at the distress boundary $x=1$ is a coordinate artefact. In $z=\log((x-1)/q)$ the volatility is constant on the solvency regime. | `prop:rrL`, `prop:statemap`, `cor:boundary` | port CCP and TSO (i) to (iii) | the paper behind the benchmark solver (`github.com/yuqiongwang/bank_capital_structure`), whose coordinate this is; search for degenerate-diffusion bank capital models |
| H2b | $x=1$ is unreachable if and only if $r>r_L$. | `cor:boundary` | none possible as stated: boundary classification of a diffusion is not in Mathlib. Enters as a hypothesis unless the scale-function step is formalised | as H2 |
| H3 | $(\lambda_S,K)$ is locally identified from the pair of thresholds $(y^*,x_L)$, because the two parameters move the trigger in opposite directions. | `lem:reg`, `lem:envelope`, `lem:bdr`, `prop:tcs`, `lem:envelopeK`, `prop:kcs`, `cor:idn`, `prop:soc` | Envelope cores exist (`Envelope.*`); port TCS, KCS, IDN, SOC algebra; REG and BDR enter as hypotheses | search for comparative statics of impulse-control thresholds in a fixed issuance cost; `altinkilic2000`, `buhner2002` for the cost structure only |
| H3b | The fixed issuance cost widens the inaction region at both ends, and the trigger-to-target gap $y_{\text{post}}-x_L$ is strictly increasing in $K$. | `prop:kcs`, `lem:envelopeK` | as H3 | as H3 |
| H4 | Smooth fit holds at the recapitalisation trigger, $V'(x_L^-)=V'(x_L^+)=1+\kappa$. | `prop:smf`, `rem:smfn`, `lem:itr` | `SmoothFit.smooth_fit` exists; ITR and the viscosity supersolution property enter as hypotheses | Øksendal and Sulem, *Applied Stochastic Control of Jump Diffusions*, Thm 9.7 and Ch. 9, and Crandall, Ishii and Lions (1992), both cited in the header of `SmoothFit.lean` for the supersolution property and unread; search for smooth fit at impulse-control boundaries. High risk that this is known |
| H5 | At $\lambda_S=1$ the problem is the risk-neutral benchmark exactly, and the running payoff $-\Lambda$ is concave for every $\lambda_S\ge1$, unlike an S-shaped penalty. | `prop:nesting`, `prop:concave-payoff` | `QVI_Part1.lambda_asymmetry_vanishes_at_one`, `QVI_Part1.neg_Lambda_concave` | `kahneman1979`, `barberis2001` for the S-shaped contrast. Positioning rather than novelty |
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
