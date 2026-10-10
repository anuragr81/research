# Retirement of PROOFS_v2.tex

`00_document/PROOFS_v2.tex` and its PDF were retired from the tree on
2026-10-10, at pass 6 of the plan in `TODO.md`. Its last version is at
commit `1d1a56f5`:

    git show 1d1a56f5:papers/wealth_garch/00_document/PROOFS_v2.tex

Its reader tools `00_reader/proof_registry.py` and `00_reader/check_citations.py`
retired with it. `checks/verify_retirement.py` reads the file from git and
fails unless every label, section, decimal number and Lean name in it has a
row below, and every home named here exists.

Homes: a row ID of `MANUSCRIPT.tex` (K, M, L, C); `App. A` to `App. E` of
`MANUSCRIPT.tex`; `illustration` (the numerical layer, `01_theory/` and
`02_numerical/`, which no row rests on); `TODO.md`; `VERIFICATION.md`; or a
file path.

Documents that still cite PROOFS_v2's numbering and were not changed:
`00_reader/PITCH_AND_SUMMARY.md`, `00_reader/THEORY_PAPER_STARTER.md`,
`00_reader/TODO.md`, `00_reader/TERMINOLOGY.md`, `05_summaries/`,
`06_empirical_starter/`, `HANDOVER_20261010.md`, the comments of
`02_numerical/` and `01_theory/verify_document_figures.py`, and the headers
of the Lean files. Their locators refer to commit `1d1a56f5`.

## Labels

| Label | Home | Note |
|---|---|---|
| `sec:discrete` | App. A, M1, M12, M13 | discrete-time notation and rows |
| `eq:z` | App. A | log buffer ratio $z$ |
| `eq:zrec` | M1 | the variance recursion |
| `prop:identity` | M12 | ACI; Lean statement `RateBased.balanced_growth_iff` |
| `eq:prop1-ratio` | M12 | proof of ACI |
| `eq:prop1-chain` | M12 | proof of ACI |
| `prop:persistence` | M12, App. E | PSA; its claim that the asymmetry vanishes with $g$ is refuted (R2) |
| `thm:lambda4` | M1 | FPV |
| `eq:lambda4` | M1 | FPV |
| `prop:robust` | M1 | ROB, the limit for every saturation scale |
| `prop:egarch` | TODO.md | EGE, not yet ported (pass 3 inventory) |
| `prop:sojourn` | M13, App. B | SJT; Wald's identity is a hypothesis |
| `sec:formulation` | App. A, App. D | continuous-time notation and primitives |
| `eq:Lambda` | M11, App. A | the shortfall penalty |
| `eq:rhoL` | App. A | liability-net discount rate |
| `eq:qvi` | M7, M10, App. B | the QVI; its inequalities enter as hypotheses |
| `eq:drift` | M9, App. A | drift along the cap $B$ |
| `eq:diff` | M2, App. A | the diffusion $\sigma^2(x,\pi)$ |
| `prop:nesting` | M11 | NST |
| `prop:concave-payoff` | M11 | RPC |
| `sec:capcoef` | M2, M9 | coefficients where the cap binds |
| `eq:capcoef` | M9, App. A | capped coefficients |
| `eq:kappa1c` | M2 | renamed $\nu_1^2$ |
| `prop:rrL` | M9 | CCP |
| `prop:lowerbound` | App. B | LQB; used by no row, recorded as not carried |
| `sec:statemap` | M2, M8, M9 | the state map |
| `prop:statemap` | M8, M2, M9 | TSO (i), (ii), (iii) |
| `eq:statemap` | M8 | the log coordinate |
| `cor:boundary` | M8, M9, App. B | CDA; unreachability is not proved |
| `prop:satlimits` | M3 | SCG |
| `eq:kappa3c` | M3 | renamed $\nu_3^2$ |
| `eq:lamV-geom` | App. D, C1 | the variance ratio in continuous time, a definition |
| `sec:compstat-thm` | M4, M5 | threshold comparative statics |
| `eq:Wdef` | App. A, App. C | $W$ |
| `lem:reg` | App. B | REG, a hypothesis |
| `lem:envelope` | M4, App. B | ENV; Danskin is a hypothesis |
| `eq:envelope` | App. B | envelope identity, a hypothesis |
| `eq:Wode` | M4 | the ODE for $W$ |
| `eq:Wneumann` | M4 | $F'(y^*)=0$ |
| `eq:Wnonlocal` | App. B | used for the sign of $W'(x_L)$, a hypothesis |
| `lem:bdr` | App. B | BDR, the implicit function theorem, a hypothesis |
| `prop:tcs` | M4, M5 | TCS |
| `eq:tcs` | M4, M5 | TCS slopes |
| `rem:tcsn` | App. B, illustration | sign conditions; figures are illustrations |
| `sec:kcs` | M4, M5, M6 | fixed cost and identification |
| `eq:Ndef` | App. A, App. C | $N$ |
| `lem:envelopeK` | M4, M5, App. B | ENVK; Danskin is a hypothesis |
| `eq:envelopeK` | App. B | envelope identity, a hypothesis |
| `eq:Node` | M4 | the ODE for $N$ |
| `prop:kcs` | M4, M5 | KCS |
| `eq:kcs` | M4, M5 | KCS slopes |
| `cor:idn` | M6 | IDN |
| `eq:jac` | M6 | the Jacobian |
| `rem:kcsn` | K2, illustration | the mechanism is K2; the K-sweep figures are illustrations |
| `sec:secondorder` | M7, App. E | second-order conditions |
| `eq:Maffine` | M10, App. A | $\mathcal MV$ affine |
| `eq:Psi` | M7 | $\Psi$ |
| `prop:soc` | M7, App. E | SOC (ii) is M7; SOC (i) as derived is refuted (R1) |
| `rem:socn` | M7, illustration | strictness condition; figures are illustrations |
| `prop:smf` | M10 | SMF |
| `rem:smfn` | M10 | SMFN, `Envelope.smf_permits` |
| `sec:verification` | App. B | regularity and verification, hypotheses |
| `lem:itr` | App. B | ITR, a hypothesis; positivity of $\sigma^2$ is `BCVW.sig2_nondegenerate` |
| `lem:fin` | M14 | FIN |
| `prop:ver` | App. B | VER, the verification theorem, a hypothesis |
| `rem:vern` | App. B, App. E | its use of strict $V''(y_{\text{post}})<0$ rests on R1 |
| `sec:numerical` | illustration | the numerical solve |
| `rem:solver` | illustration | SLV, `02_numerical/verify_M_operator.m` |
| `rem:capbinds` | App. B, illustration | CBI, the numerical support for the binding hypothesis |
| `rem:compstat` | illustration | CSL; directions agree with M4, M5 |
| `rem:nonconcave` | illustration | LNC |
| `rem:Ksens` | illustration | KSW |
| `sec:open` | M10, TODO.md | smooth fit is M10; the estimated $\lambda_V$ goes to the empirical companion |
| `app:status` | App. B, VERIFICATION.md | verification status |

## Sections

| Section | Home | Note |
|---|---|---|
| Index of result codes | TODO.md | codes kept in the pass 3 inventory and this file |
| The discrete-time model | M1, M12, M13 |  |
| Setup | App. A |  |
| Results | M1, M12, M13 |  |
| The continuous-time formulation | App. A, App. D |  |
| State, controls, objective | App. A, App. D |  |
| Two structural properties | M11 |  |
| Coefficients where the cap binds | M2, M9 |  |
| The state variable, and the boundary at \texorpdfstring{$x=1$}{x=1} | M2, M8, M9 |  |
| Threshold comparative statics | M4, M5 |  |
| The fixed cost, and joint identification | M4, M5, M6 |  |
| Second-order conditions at the impulse boundaries | M7, App. E |  |
| Regularity and verification | App. B, M14 |  |
| The numerical solve | illustration |  |
| What is not established | M10, TODO.md |  |
| Verification status | App. B, VERIFICATION.md |  |

## Numbers

Every decimal number in PROOFS_v2, with the line of its first occurrence.

| Number | Line | Section | Home | Note |
|---|---|---|---|---|
| 0.025 | 346 | Coefficients where the cap binds | illustration | a figure at the numerical baseline, not a row |
| 0.03 | 346 | Coefficients where the cap binds | illustration | a figure at the numerical baseline, not a row |
| 0.02 | 347 | Coefficients where the cap binds | illustration | a figure at the numerical baseline, not a row |
| 0.01 | 347 | Coefficients where the cap binds | illustration | a figure at the numerical baseline, not a row |
| 0.015 | 347 | Coefficients where the cap binds | illustration | a figure at the numerical baseline, not a row |
| 1.15 | 425 | The state variable, and the boundary at \texorpdfs | illustration | a figure at the numerical baseline, not a row |
| 0.15 | 426 | The state variable, and the boundary at \texorpdfs | illustration | a figure at the numerical baseline, not a row |
| 1.7720 | 459 | The state variable, and the boundary at \texorpdfs | illustration | a figure at the numerical baseline, not a row |
| 0.2623 | 460 | The state variable, and the boundary at \texorpdfs | illustration | a figure at the numerical baseline, not a row |
| 45.633 | 460 | The state variable, and the boundary at \texorpdfs | illustration | a figure at the numerical baseline, not a row |
| 2.599 | 460 | The state variable, and the boundary at \texorpdfs | illustration | a figure at the numerical baseline, not a row |
| 2.5 | 464 | The state variable, and the boundary at \texorpdfs | illustration | a figure at the numerical baseline, not a row |
| 0.045 | 721 | Threshold comparative statics | illustration | a figure at the numerical baseline, not a row |
| 0.05 | 864 | The fixed cost, and joint identification | illustration | a figure at the numerical baseline, not a row |
| 0.159 | 866 | The fixed cost, and joint identification | illustration | a figure at the numerical baseline, not a row |
| 0.685 | 866 | The fixed cost, and joint identification | illustration | a figure at the numerical baseline, not a row |
| 1.69 | 870 | The fixed cost, and joint identification | illustration | a figure at the numerical baseline, not a row |
| 1.5715 | 870 | The fixed cost, and joint identification | illustration | a figure at the numerical baseline, not a row |
| 0.012 | 967 | Second-order conditions at the impulse boundaries | illustration | a figure at the numerical baseline, not a row |
| 0.023 | 967 | Second-order conditions at the impulse boundaries | illustration | a figure at the numerical baseline, not a row |
| 0.035 | 968 | Second-order conditions at the impulse boundaries | illustration | a figure at the numerical baseline, not a row |
| 1.25 | 968 | Second-order conditions at the impulse boundaries | illustration | a figure at the numerical baseline, not a row |
| 6.4 | 969 | Second-order conditions at the impulse boundaries | illustration | a figure at the numerical baseline, not a row |
| 13.9 | 969 | Second-order conditions at the impulse boundaries | illustration | a figure at the numerical baseline, not a row |
| 18.7 | 969 | Second-order conditions at the impulse boundaries | illustration | a figure at the numerical baseline, not a row |
| 1.167 | 1096 | Regularity and verification | illustration | a figure at the numerical baseline, not a row |
| 1.57 | 1097 | Regularity and verification | illustration | a figure at the numerical baseline, not a row |
| 0.04 | 1187 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.12 | 1188 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.09 | 1188 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.08 | 1188 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.20 | 1189 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.30 | 1190 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.673866 | 1198 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.227682 | 1198 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.522875 | 1199 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.674 | 1199 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.228 | 1199 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.5229 | 1199 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.5 | 1205 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 5.6 | 1211 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 5.0 | 1219 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 4.1 | 1220 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 4.9 | 1220 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.00 | 1232 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.10 | 1232 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.50 | 1232 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.75 | 1232 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 2.00 | 1232 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.167647 | 1234 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.1 | 1253 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.3725 | 1254 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.4371 | 1254 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.145 | 1255 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.200 | 1255 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.5037 | 1255 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.4702 | 1255 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.6596 | 1257 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.088 | 1257 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.065 | 1258 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.030 | 1258 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.105 | 1258 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.3843 | 1259 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.3395 | 1259 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.0250 | 1283 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.0275 | 1283 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.0288 | 1283 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.0300 | 1284 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.365 | 1289 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.370 | 1289 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.040 | 1296 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.121 | 1297 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.133 | 1297 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.001 | 1309 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.005 | 1309 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.3845 | 1322 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.26 | 1323 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.35 | 1324 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 0.030 | 1330 | The numerical solve | illustration | a figure at the numerical baseline, not a row |
| 1.33 | 1359 | What is not established | TODO.md | estimated $\lambda_V$, for the empirical companion |
| 1.39 | 1359 | What is not established | TODO.md | estimated $\lambda_V$, for the empirical companion |
| 1.17 | 1360 | What is not established | TODO.md | estimated $\lambda_V$, for the empirical companion |
| 0.18 | 1362 | What is not established | TODO.md | estimated $\lambda_V$, for the empirical companion |
| 0.22 | 1362 | What is not established | TODO.md | estimated $\lambda_V$, for the empirical companion |
| 0.16 | 1364 | What is not established | TODO.md | estimated $\lambda_V$, for the empirical companion |
| 0.34 | 1408 | Verification status | App. B |  |
| 0.42 | 1408 | Verification status | App. B |  |

## Lean names

| Name in PROOFS_v2 | Home |
|---|---|
| `Envelope.lean` | lean/mathlib/Envelope.lean |
| `ImpulseCount.lean` | lean/mathlib/ImpulseCount.lean |
| `QVI_Part1.lean` | lean/mathlib/QVI_Part1.lean |
| `RateBased.lean` | lean/mathlib/RateBased.lean |
| `SmoothFit.lean` | lean/mathlib/SmoothFit.lean |
| `count_le` | ImpulseCount.count_le (M14) |
| `count_summable` | ImpulseCount.count_summable (M14) |
| `impulse_sum_summable` | ImpulseCount.impulse_sum_summable (M14) |
| `lambda_asymmetry_vanishes_at_one` | QVI_Part1.lambda_asymmetry_vanishes_at_one (M11) |
| `neg_Lambda_concave` | QVI_Part1.neg_Lambda_concave (M11) |
| `neumann_at_ystar` | Envelope.neumann_at_ystar (M4) |
| `node_of_envelope` | Envelope.node_of_envelope (M4) |
| `smf_permits` | Envelope.smf_permits (M10) |
| `smooth_fit` | SmoothFit.smooth_fit (M10) |
| `trigger_jump` | Envelope.trigger_jump (M5) |
| `wode_of_envelope` | Envelope.wode_of_envelope (M4) |
