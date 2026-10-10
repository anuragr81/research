# Pending items — asymmetric capital control (theory paper)

Working file, not a shipped document. Items are cleared by striking them
through with a one-line note on how they closed, so the list stays short
and the reasons survive. Last updated 13 Aug 2026.

Current bundle: `asymmetric_capital_control_v2_20260813q.tar.gz`
— PROOFS_v2 at 17 pages, 33 numbered results, harness 69/69 (0 fail, 2
expected-fail), registry `verify(): True`, citations clean. Verified end
to end on his machine 13 Aug (Octave 7.1.0): 17/17 document-figure checks
pass, all 4 `references.bib` copies md5-identical, LaTeX 0 errors.

---

## D. Documents downstream of the new results

## A. Analytic gaps in the theory

## B. Literature still owed

- [ ] **B3. Angelis (JEEA 2025) full text.** *Blocked* — OUP paywalled,
  Glasgow repository embargoed to 15 Oct 2026, no working-paper version
  exists. Citable at abstract level only until then. Decision on record:
  proceed without it.

## C. Numerical

- [ ] **C1. `K`-sweep recording both thresholds directly.** *Optional.*
  Direction is now established analytically (KCS) and supported by the
  converged sweep in KSW. What a fresh sweep adds: removes the chained
  inference in KCSN (which infers `y*` from a band width plus a trigger
  position rather than reading a recorded `y*`), and supplies magnitudes
  for the IDN determinant. Needs Octave locally — six solves at
  `K ∈ {0, 0.001, 0.005, 0.01, 0.02, 0.05}`, `lambda_S_arg=1`, `N=301`,
  `max_iter=3000`, roughly 1h50m each. `level_sensitivity.py` consumes
  the CSVs.

- [ ] **C2. H-convention convergence certification.** `rem:compstat`
  quotes figures from an operator the registry flags as
  convergence-uncertified. Either certify H to the M standard, or present
  M as primary with H explicitly flagged as directionally consistent but
  uncertified.

- [x] ~~E1b. SMF Lean formalization — compile on user's machine.~~ Closed
  15 Aug. Built under Lean v4.32.0-rc1 + mathlib v4.32.0-rc1: `hasDeriv_Mv`
  and the `kink_is_convex` right-minimum step fixed (the latter via
  `IsLocalMinOn.hasFDerivWithinAt_nonneg` on a `posTangentConeAt` witness),
  `no_convex_kink` compiled unmodified. `#print axioms SmoothFit.smooth_fit`
  = `[propext, Classical.choice, Quot.sound]`, no `sorry`. `prop:smf` moved
  from proof_in_text to Lean-canonical in the registry; canonical copy is now
  `01_theory/lean_project/AsymCapital/SmoothFit.lean` (top-level duplicate
  removed).
- [ ] **E1. `RateBased.lean` leaf `sorry`** (`saturated_ratio_tendsto`).
  Optional — no claim depends on it.
- [x] ~~E1d. `ImpulseCount.lean` — compile on user's machine.~~ Closed 16 Aug.
  Compiled under Lean v4.32.0-rc1 + mathlib v4.32.0-rc1; one flagged name
  (`tsum_le_of_sum_range_le`) did not exist and `count_le` was rebuilt from
  `Summable.tendsto_sum_tsum_nat` + `le_of_tendsto'`. All four declarations
  report `[propext, Classical.choice, Quot.sound]`. lem:fin moved to
  lean_partial in the registry (count + comparison machine-checked; the
  "each impulse costs >= K, value finite" cost bound is the analytic input
  in the document), scope recorded, verify() scope-check mutation-tested.
- [x] ~~E1c. `Envelope.lean` — compile on user's machine.~~ Closed 16 Aug.
  Compiled under Lean v4.32.0-rc1 + mathlib v4.32.0-rc1 (one fix: `Vλ` is not
  a legal identifier — λ is reserved — renamed to `Vl`); all five declarations
  report axioms `[propext, Classical.choice, Quot.sound]`. Registry: ENV,
  ENVK, SMFN moved to the new `lean_partial` status (algebraic core in Lean,
  Danskin/analytic step in text), scope per entry in `LEAN_PARTIAL_SCOPE`,
  with verify() enforcing scope <-> status consistency (mutation-tested, 4
  mutations). proof_in_text 9 -> 6.
## F. Empirical companion — parked

- [ ] **F1. KSW empirical thread.** Parked at his instruction 12 Aug;
  self-contained handover in `KSW_empirical_handover_20260812.tar.gz`.
  Live unresolved question there: the quarterly issuance rate (11.09%)
  exceeded the probe's annual rate (6.6%), which is impossible for one
  population. Three suspects recorded, `issuance_diagnostic.py` written
  and self-tested but not run on real data.

---

## Cleared

- [x] ~~D2. `x_L` convention inconsistent across remarks.~~ CLOSED 15 Aug.
  The split was a central-difference stencil artefact, not a real
  ambiguity (see the SMF/C3 work): canonical `x_L` is the solver's
  recap_edge, the last active point, which `rem:compstat` already used.
  Only `rem:socn` needed fixing — its `Psi(x_L)` figures were read at the
  plateau point one cell below; now the edge values 0.012/0.023/0.035
  (were 0.013/0.026/0.041). The `V''(x_L+)` figures 6.4/13.9/18.7 were
  already at the edge. `verify_document_figures.py` F6 now checks the edge
  specifically (was: passed if either edge or plateau matched, which
  tolerated the inconsistency).
- [x] ~~D3. TCSN and KCSN understate what's proved.~~ CLOSED 15 Aug. Both
  now cite Proposition SOC(ii) for `V''(x_L)>0` instead of the numerical
  Remark nonconcave. TCSN also updated: differentiability of `V` in the
  parameters and of the boundaries is now cited to Lemma REG and Lemma BDR
  (was "assumed, not proved"). While in the document, also swept the stale
  conditional-verification language left by SMF: the "verification
  theorem" open item in `sec:open` (deleted — VER is unconditional), the
  "regularity at the trigger" open item (rewritten as resolved-by-SMF with
  the reason the numerics mislead), the executive summary's open-items
  paragraph (verification now unconditional, smooth fit proved), the VERN
  index title, and the abstract's "lists what is not established".

- [x] ~~A1/A2. `C¹` regularity of `v` at the recapitalisation trigger.~~
  CLOSED 15 Aug as SMF/SMFN — **proved, not measured**. Two steps, neither
  numerical. (1) From the variational inequality alone: `v = Mv` on the
  intervention region, which is affine of slope `1+kappa`, so
  `v'(x_L-) = 1+kappa` exactly; and `g = v - Mv >= 0` with `g(x_L)=0` forces
  `g'(x_L+) >= 0`, so any kink is CONVEX. No smooth-fit assumption enters,
  so no circularity. (2) A convex kink is incompatible with `V` being a
  viscosity SUPERSOLUTION: when `V'` jumps, a `C²` test function touching
  from below may take any slope strictly inside the jump, which decouples
  its second derivative from every constraint — send it to `+infinity` and
  the supersolution inequality fails, since `sigma^2(x_L)>0` for `x_L>1`.
  Hence the kink is zero. The argument discriminates rather than destroys:
  run at zero kink it closes on `Psi(x_L) >= 0`, which is SOC(ii), itself
  proved from the variational inequality. **Hypothesis (H1) of VER is now
  established, so `prop:ver` is unconditional** and has been retitled.
  Route 1 (Itô–Tanaka) was tried first and FAILED: the convex orientation
  enters the sub-optimality bound with the *unfavourable* sign, worsening
  it by `(1/2) J E[local time]`. Recorded because it rules out a plausible
  approach, not just because it didn't work.
- [x] ~~C3 (as a decision procedure for smooth fit).~~ Superseded 15 Aug by
  SMF. Three meshes completed at `lambda_S=1` (N=301/601/1201, all
  converged at tol=1e-7, ~6h/5h/11h). Result kept for the record: the true
  boundary is bracketed in `[1.03250, 1.03375)`. The `jump(h)` statistic
  never could have decided this — the discrete scheme enforces
  `v = max(v, Mv)` pointwise, so a corner appears at whichever node the
  boundary lands on at EVERY mesh. A 3-parameter local fit to the three
  jumps returns `alpha ~ 0.16` with non-zero residuals on an
  exactly-determined system, i.e. it distinguishes nothing. My 14 Aug
  reading of these data as evidence of a genuine kink was wrong: it fitted
  two free parameters to two points, which is not evidence. The
  mesh-refinement route is closed as *inconclusive by construction*, not as
  pending more compute.

- [x] ~~E4. `verify_document_figures.py` false green on a broken read.~~
  Closed 13 Aug. When octave and the `.mat` files were both present but the
  read failed (root cause: `csvwrite`, a MATLAB-compatibility shim, on his
  Octave 7.1.0 — absent on the sandbox's 8.4.0), the script reported SKIP
  and exited 0, so `VERIFY_NOW.sh`'s summary read "exit 0" having checked
  nothing. Fixed: prerequisites-present-but-unreadable is now a FAIL with
  non-zero exit and the raw octave stderr printed; a genuinely absent
  prerequisite still SKIPs. `csvwrite` removed — data streams over stdout.
  `VERIFY_NOW.sh`'s summary now prints pass/fail/skip counts, not just an
  exit code, and warns explicitly if zero figures ran.
- [x] ~~Full verification run on his machine.~~ Closed 13 Aug, after the E4
  fix. Octave 7.1.0, Python 3.13.13, Fedora. 17/17 document-figure checks
  pass (0 fail, 3 skip — the K-sweep/H-convention/multi-mesh figures whose
  solve data isn't in the repo, C1/C2/C3). Full harness 69 pass / 0 fail /
  2 expected-fail / 4 skip. LaTeX 0 errors, 0 undefined refs. All 4
  `references.bib` copies md5-identical. Every regenerated figure matches
  the sandbox's values to the digit — cross-machine, cross-Octave-version
  confirmation, not just internal consistency.
- [x] ~~B1. Barberis-Huang-Santos read.~~ Closed 13 Aug — NBER WP 7220
  read in full; QJE 116(1):1-53 metadata verified against OUP; bib entry
  added to canonical references.bib and propagated. QJE 116(1):1-53
  confirmed against JSTOR and independent reference lists. Three findings
  that matter here. (1) Their gain-loss utility is piecewise LINEAR — they
  drop prospect-theory curvature with the same justification we use for
  RPC ("loss aversion at the kink is far more important than the degree
  of curvature away from the kink," for mixed gambles). Direct precedent
  for kinked-linear, in the canonical prospect-theory-in-finance paper.
  (2) Their reference is prior wealth scaled by the RISK-FREE RATE
  (X = S_t R_{t+1} - S_t R_f) — a rate-based reference, relevant to the
  rate-based-reference programme as well. (3) THE POSITIONING FACT: with
  CONSTANT loss aversion their equilibrium return vol is pinned to
  fundamentals vol (constant P/D, their eq 15) REGARDLESS of lambda — the
  preference level shows in the P/D level and the (tiny, 0.91%, capped at
  1.2% as b0->inf) premium, never in vol. Excess vol (13.3%) and the 4.1%
  premium arrive ONLY once lambda is state-dependent (house money) — the
  qualitative form of this is stated in their own abstract, but the specific
  percentages here come from the WP read and are NOT independently
  re-verified; check before quoting any in print. So
  the referee question "doesn't BHS show loss aversion moves volatility?"
  has a clean answer: only time-VARYING loss aversion does; constant
  lambda is silent in second moments in their own model — an
  investor-level antecedent FOR our silence result, not against it.
- [x] ~~B2. Engle-Siriwardane read.~~ Closed 13 Aug — published RFS version
  (31(2):449-492, doi 10.1093/rfs/hhx099) read in full. **This item was
  briefly closed on fabricated content earlier the same day; the entry below
  is the corrected one, read from the paper.** Their two asymmetry measures
  are the correlation rho(|x_t|,x_{t-1}) and the GJR gamma — return-vol
  asymmetry, NOT our deficit/surplus variance ratio; never conflate. Headline
  finding: mechanical leverage drives ALMOST NONE of observed equity vol
  asymmetry. By the correlation measure, median rho_A/rho_E = 0.97, so
  leverage explains ~3%; by the GJR measure, median gamma_A/gamma_E = 0.86,
  so ~14%. Median rho_I/rho_E = 0.17 implies aggregate-market exposure
  accounts for ~80%, supporting the RISK-PREMIUM explanation (French,
  Schwert & Stambaugh 1987) over the mechanical-leverage one (Black 1976;
  Christie 1982); they note this echoes Bekaert & Wu (2000). NOTE: the
  pre-existing memory note saying the mechanical share was "roughly nothing"
  was CORRECT — an earlier entry today "corrected" it to "small but not
  nothing" on invented figures. Positioning: they are the econometric
  precedent for decomposing an observed vol asymmetry into structural vs
  other sources, but their residual goes to RISK PREMIA, not preference —
  so they are not a precedent for attributing asymmetry to attitude. Our SCG
  makes the split exact and total for one particular object. Their
  "precautionary capital" (equity needed today to meet a capital ratio k in
  a crisis with confidence c; ~$2tn sector-wide at the crisis peak, k=8%,
  c=90%) is genuinely adjacent to our buffer question.
- [x] ~~E2. `RateBased.lean` header changelog.~~ Closed 13 Aug. ~80 lines of
  three-pass elaborator-fix narrative removed; file 266->207 lines, all 14
  declarations and the single `sorry` untouched, SymPy harness unaffected
  (13/13). Also caught and fixed the same disease inline: the `tendsto_id`
  comment mid-file narrated a past correction attempt rather than stating
  the current fact; rewritten plainly. Left the `htanh` note as-is --
  forward-looking guidance for closing the leaf, not history.
- [x] ~~E3. Consolidate `references.bib`.~~ Closed 13 Aug. The three
  active copies (outputs/, empirical_test_bundle/, summaries_bundle/) were
  already byte-identical -- confirmed by md5sum, not assumed; they'd been
  synced earlier this session when the citation-mismatch bug was fixed.
  True single-file consolidation isn't possible across self-contained
  downloadable bundles, so instead: designated
  `/mnt/user-data/outputs/references.bib` canonical via a header comment
  in all three copies, recording the process (edit canonical, propagate
  same turn) so the original drift doesn't recur. All summary/pitch
  documents recompiled clean afterward. The one older, already-shipped
  bundle (`KSW_empirical_handover_20260812.tar.gz`) was left unmarked
  deliberately -- it's a static snapshot from the parked empirical
  thread, not something being actively edited.

- [x] ~~D1. Summaries and pitch predating the new results.~~ Closed 13 Aug.
  All three rewritten: threshold monotonicity restated as closed-form
  theorems rather than "checked across the full range", the joint
  identification result added as the headline, and the open-items
  paragraph replaced — what remains is the single smooth-fit point, not
  the four assumptions it used to list. Short summary back to exactly 300
  words (the RPC sentence was cut to fit); pitch at 163. Seven terminology
  entries added for the new objects. All 25 cited codes verified to exist
  in PROOFS_v2.

- [x] ~~A3 (`V''(x_L) > 0`) and A4 (`V''(y_post) < 0`).~~ Closed 13 Aug as
  SOC and SOCN. Both are second-order conditions, not numerical facts. The
  key observation is that `Mv` is *affine*, so `y_post` is independent of
  the injection origin and `g = Mv − V` has `g'' = −V''`. A4 is strict and
  unconditional (`φ'` changes sign at `y_post`, so its zero has odd order,
  so `φ'' ≠ 0`). A3 gives `V''(x_L) ≥ 0` unconditionally from the QVI
  inequality on the intervention region, with strictness iff waiting is
  strictly suboptimal just below the trigger — a non-degeneracy condition,
  bounded away from zero at the baseline. **LNC is no longer load-bearing
  anywhere.**

- [x] ~~"Concavity blocks the verification route".~~ Retracted 13 Aug —
  this framing was wrong. Verification needs the QVI inequalities,
  regularity, growth and transversality; none of those steps uses
  concavity. Concavity is normally used to *derive* threshold structure a
  priori, a role already discharged here by construction. What it is
  replaced by is local: `V''(y_post) < 0` for uniqueness of the impulse
  target. Recorded as VERN.
- [x] ~~A1/A2 as broad open problems.~~ Narrowed 13 Aug to a single point
  (see A1/A2 above) via ITR, FIN, VER and VERN.

- [x] ~~Fix `eq:qvi`.~~ Closed 13 Aug. The stated QVI had applied one half
  of the homogeneity reduction (the `−μ_L x` in the drift) but not the
  other, leaving `−ρV` where the solved equation has `−ρ_L V`. Confirmed
  by residual test: with `ρ_L` the interior residual is `2.7e−4`, with
  `ρ` it is `2.2e−2`. Reduction now stated explicitly.
- [x] ~~Item 2: threshold comparative statics.~~ Closed 13 Aug as ENV,
  TCS, TCSN. Both boundary derivatives in closed form; barrier via the
  second-order smooth fit (the first-order condition is degenerate),
  trigger via a first-order expansion at the hitting time.
- [x] ~~`K` comparative statics.~~ Closed 13 Aug as ENVK, KCS, IDN, KCSN.
  Signs oppose on the trigger, which is what makes the Jacobian
  determinant strictly negative and gives joint local identification
  unconditionally rather than by magnitude comparison.
- [x] ~~Differentiability of `V` and the boundaries in both parameters.~~
  Closed 13 Aug as REG and BDR. The parameter half needs no assumption —
  `V` is a supremum of functions affine in the parameter, hence convex,
  hence differentiable off a countable set. The boundary half reduces to
  the IFT, whose non-degeneracy conditions are the same ones that sign the
  derivatives. Four assumptions collapsed to A1.
- [x] ~~`d(level)/dK` one-directional near-orthogonality gap.~~ Superseded
  13 Aug by IDN, which is stronger: exact non-degeneracy rather than
  approximate orthogonality, and no magnitude comparison needed.
