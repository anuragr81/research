# Theory paper — starter pack

Everything needed to write the theory paper, and nothing that belongs to the
empirical companion. Written 12 August 2026 against bundle
`asymmetric_capital_control_v2_complete_20260812a`.

The companion empirical paper is a separate project with a separate data
requirement; see the closing section. This paper does not depend on it and
must not be written as though it does.

---

## 1. The claim

A bank chooses risk exposure continuously, pays dividends at an upper
barrier, and recapitalises by a lump at a lower trigger, under a regulatory
cap that tightens as its buffer erodes. Its objective carries a kinked-linear
shortfall penalty: an asymmetry parameter `lambda_S >= 1` weighting outcomes
below a reference level. At `lambda_S = 1` the problem collapses exactly to
the risk-neutral benchmark, so `lambda_S` measures excess asymmetry against
that benchmark rather than against zero.

**The distinctive theoretical claim, stated narrowly** — this is the version
that survived a closed novelty check, and it should not be widened in
drafting:

> Firm-level singular control at payout *and* impulse control at
> recapitalisation, with an asymmetric **preference** — not a constraint —
> governing both.

Not "reference-dependence in control is new" (false). Not "loss aversion in
banking is new" (false). Not "asymmetric adjustment speeds in bank capital
are new" (false — occupied, see §6).

**The identification result is the paper's most useful export**, and it is
entirely model-internal — it needs no data, which is exactly why it belongs
here and not in the companion:

> The preference parameter is identified from the institution's committed
> thresholds, not from the volatility its policy induces.

Supporting numbers, all from recorded sweeps (§5): the barrier
comparative-static slopes run 85–116x the convergence noise floor, with
near-orthogonal separation from the fixed cost `K`; while `lambda_V`'s entire
signal across `lambda_S` in [1,2] is about the width of its own null's
calibration band.

---

## 2. What the paper is not

State these early and plainly. Each has cost the project time when left
implicit.

- **Not an empirical identification paper.** Ruling of 28 July 2026, never
  revoked. Any panel material is illustrative calibration, explicitly not
  estimation.
- **Not a psychological claim.** Binding language discipline, 27 July 2026:
  `lambda_S` is an *asymmetric risk-response parameter*, agnostic as to
  source (behavioural, institutional, regulatory). "Loss aversion" appears
  only as cited motivating background in the introduction, never as a claim
  the mathematics or the data support. Behavioural content requires elicited
  data under known payoffs — Haigh & List (2005), Larson–List–Metcalfe (2016)
  are the citations that establish the data-type gap. Nothing mathematical
  changes under this discipline.
- **Not a verification-theorem paper.** That the constructed solution is the
  value function is open (§8). Everything numerical is a property of a
  validated solver's fixed point.

---

## 3. Suggested structure

1. **Introduction.** Motivating puzzle: capital ratios sit well above
   regulatory minima nearly everywhere, and a jump-aware optimal policy
   predicts that gap where a model without jump risk has no reason to.
   Position against the three-way novelty table (§6). State the narrow claim.
2. **The discrete-time model.** Props 1–2, Thm 1, Props 3–5. The
   fourth-power variance ratio and its robustness. This section is fully
   verified and can be asserted without qualification.
3. **The continuous-time formulation.** State, three controls of distinct
   type, the QVI. Props 6–9: exact nesting at `lambda_S = 1`, concavity of
   the running payoff, parameter consistency, the liquidation lower bound.
4. **The state variable and the boundary.** Prop 10 (the two states are one
   state), Cor 1 (the degeneracy is a coordinate artefact). This is what
   makes the `lambda_V`/`lambda_S` question well-posed at all.
5. **The saturation limits.** Prop 11: the definitional evaluation of
   `lambda_V` is a constant of cap geometry, independent of `lambda_S`.
   This is a *negative* identification result and should be presented as
   one — it is half the argument for §6 below.
6. **Identification: why the thresholds and not the variance.** The
   comparative statics, the signal-to-noise comparison, the near-triangular
   Jacobian separating `lambda_S` from `K`. Armstrong–Brigo as the scope
   theorem (§6 of this brief).
7. **The numerical solve.** Rems 1–5. Labelled numerical throughout.
8. **What is not established.** Verbatim honesty; see §8 here.

---

## 4. Result inventory and what may be asserted

Eighteen numbered results. One canonical verifier each: Lean where one
exists, exact symbolic otherwise. Full table in `PROOFS_v2.tex` Appendix A.

**Tier 1 — Lean, compiler-verified, no `sorry`, no added axioms. Assert flat.**

| Result | Content | File |
|---|---|---|
| Props 1–2 | Accounting identity; persistence asymmetry | `RateBased.lean` |
| Prop 6 | Exact nesting of the risk-neutral benchmark at `lambda_S=1` | `QVI_Part1.lean`, `lambda_asymmetry_vanishes_at_one` |
| Prop 7 | The running payoff is concave | `QVI_Part1.lean`, `neg_Lambda_concave` |

Prop 7 is load-bearing for positioning: it is what distinguishes this from
the S-shaped alternative, where non-concavity is a certainty rather than a
risk.

**Tier 2 — exact symbolic (`simplify(...)==0` or a sign condition on
symbols; no floating point, no sampling). Assert flat, cite the verifier.**

| Result | Content | File |
|---|---|---|
| Thm 1 | Fourth-power variance ratio | `verify_rate_based.py` (S1a–S1b) |
| Props 3–5 | Robustness of the ratio; EGARCH-class equivalence; sojourn time | `verify_rate_based.py` |
| Prop 8 | Consistency condition on parameters | `verify_state_map.py` (M10) |
| Props 10, Cor 1 | The two states are one state; boundary a coordinate artefact | `verify_state_map.py` (M1–M9) |
| Prop 11 | The surplus limit is cap geometry too | `verify_saturation_limits.py` (L1–L3) |

**Tier 3 — proof in text.** Prop 9 (liquidation lower bound), via the
admissibility construction.

**Tier 4 — numerical. Evidence, not proof. Label every time.**
Rems 1–5: the solver and its validation; the cap binds throughout the
inaction region; comparative statics in `lambda_S`; local non-concavity
under a fixed issuance cost; the `K`-sweep.

**Two deliberate expected-failure checks.** `M8*` and `L5*` are designed to
fail; their *passing* would mean a claim in the corresponding `.tex` is
wrong. Do not "fix" them. Say so in a footnote if the verification appendix
is described.

---

## 5. The identification argument, with its numbers

From the recorded sweeps (`rem:compstat`, `rem:nonconcave`, `rem:Ksens`);
no `.mat` files needed to restate these.

- Barrier binding-function slopes: `d(y*)/d(lambda_S)` = +0.065 (H
  operator), +0.088 (M operator); `d(trigger)/d(lambda_S)` = +0.055 (H),
  +0.075 (M). Convergence noise floor 7.6e-4. **Signal-to-noise 85–116x.**
- `lambda_S` moves *levels* (0.075) with a near-constant trigger-to-target
  *gap* (0.365 to 0.370, slope 0.005). `K` moves the *gap* steeply
  (gap ~ K^0.3845, `d(gap)/dK` ~ +9.3 at K=0.01). **Gap identifies `K`,
  levels identify `lambda_S`** — near-triangular Jacobian.
- Against this, `lambda_V`: the `lambda_RN` band is 0.96–1.01, width 0.050,
  and that is pure calibration uncertainty at `lambda_S = 1` alone. If
  `lambda_V`'s binding function has barrier-comparable elasticity, its total
  movement across `lambda_S` in [1,2] is 0.047–0.073 — **signal-to-band
  roughly 0.9–1.5x. `lambda_V`'s entire signal is about the size of the
  uncertainty in its own null.**

**One gap in this argument, stated rather than glossed:** `d(level)/dK` is
not recorded anywhere and needs the `.mat` files. Until it is computed the
near-orthogonality claim is supported in one direction only. Either compute
it before drafting §6, or state the separation as partial.

**A deeper point worth making in the text**, because it is the mechanism
rather than the measurement: the model's own recapitalisation policy censors
the severe excursions needed to identify `theta`, so the structural model at
its optimum generates data from which its own `lambda_V` is weakly
recoverable. Better capital management means less identifiable asymmetry.
That is a property of the model, not of any dataset.

---

## 6. Literature, all confirmed read in full

**The novelty comparison — the three closest candidates, none of which has
all three pieces:**

| | Firm-level | Singular + impulse | Asymmetric preference |
|---|---|---|---|
| Li–Yu–Zhang (arXiv:2108.02648) | no (individual consumption) | no | yes (S-shaped) |
| Angoshtari–Bayraktar–Young (2019, SIAM JFM 10(2):547–577) | yes | singular only | no (CRRA + constraint) |
| Bayraktar–Chevalier–Ly Vath–Wang (arXiv:2603.14557) | yes | yes | no (risk-neutral) |
| **This paper** | yes | yes | yes (kinked-linear) |

ABY's own follow-ups checked and cleared: arXiv:2102.03414 (2022 SIAM JFM
13(1):321–352) and arXiv:2012.02277 (2023 SIAM JFM 14(2):557–597) — both
individual consumption, CRRA plus constraint. arXiv:2204.00530 (Liang, Luo &
Yuan) read in full, no overlap.

**The impulse-control lineage, all read in full, all risk-neutral** —
this is why "asymmetric preference" is the load-bearing word in the novelty
claim: Milne & Whalley (2001, SSRN 299319); Peura & Keppo (2006, *Journal of
Business* 79(4):2163–2201); Bertsch & Mariathasan (BIS WP 923, 2021);
Klimenko, Pfeil, Rochet & De Nicolo (SFI RP 16-42, 2016).

**Armstrong & Brigo — the scope theorem, and the paper's sharpest
positioning move.** (arXiv:1711.00443; *Journal of Banking & Finance* 101
(2019):122–135.) Their Theorem 4.1 shows VaR and ES constraints cannot
reduce expected utility for an agent "risk-seeking in the left tail" —
defined as disutility growing *sublinearly*, `u(x) > -c|x|^eta` with
`eta < 1`. The proof concentrates a catastrophic loss on a vanishing
probability slice whose utility cost `-c*alpha^(1-eta)*|loss|^eta` tends to
zero precisely because `eta < 1`. **A kinked-linear penalty has `eta = 1`
and violates the hypothesis**: the evasion cost does not vanish. Their
Section 6 positive result shows constraints built on concave criteria *do*
bind.

The argument to make from this: the same modelling choice that bought
tractability (concavity of the running payoff, Prop 7 — no concavification
needed, unlike the S-shaped route) also buys **identifiability**, because
limits bind on and therefore reveal a concave-criterion preference. One
decision, two payoffs.

Two caveats to state, not hide: (i) their economic argument for S-shaped
preferences is limited liability, which banks have — the reply is about
*where*, since convexification bites near default and the inaction band sits
above the distress trigger by construction; this is a scope condition.
(ii) Their setting is a regulator constraining an adversarial agent with
full payoff-design freedom in a complete market; ours is self-imposed
policy. The transfer is weakened in both directions.

**Cited as background only, under the language discipline:** Barberis, Huang
& Santos (2001, QJE); Haigh & List (2005, JF 60(1):523–534); Larson, List &
Metcalfe (NBER WP 22605, 2016); Kahneman & Tversky. Chen (arXiv:2606.00970)
— note the critical distinction: Chen's `lambda_bar` is a value-function
*curvature* ratio, ours is a conditional-*variance* ratio; different objects
despite the shared name, and Chen's banking values (2.4–9) are not our
benchmark.

---

## 7. Standing rules that shaped the artifacts

Carry these into drafting; they are why the bundle looks as it does.

- One canonical verifier per claim — Lean where it exists, symbolic
  otherwise. Never cite two verifiers for one result.
- Every number regenerated from the scripts, never carried by hand.
- No history or correction narrative in any shipped file.
- Never describe the conditional concavity chain as "machine-verified": it
  chains Tier-2 steps through pen-and-paper lemmas and is only as strong as
  its weakest link. Normal for a theory paper; must not be overstated.

---

## 8. What is not established — reproduce this honestly

Three items, already written into `PROOFS_v2.tex` §"What is not
established". Keep them.

1. **The verification theorem.** That the constructed solution is the value
   function is not shown. The usual concavity-based route is unavailable
   since `V` is not concave for `K > 0` (Rem 4). What is needed is a
   comparison or uniqueness argument for the QVI with the impulse operator
   and the boundary of Cor 1. The first-order conditions for the risk
   control, the injection target and the dividend condition are heuristic
   pending this, and are deliberately not stated as results.
2. **Concavity of `V`.** A *conditional* theorem: concave if every envelope
   gap endpoint has excess-slope integral `E(a) = K`. The hypothesis is
   unverified. Deliberately not formalised in Lean — formalising a
   conditional whose hypothesis was never checked would misrepresent it.
   Full detail in the concavity file, not repeated here.
3. **Regularity at the recapitalisation trigger.** For `K > 0` the one-sided
   derivatives differ in every solve, but the measurement cannot support the
   reading that `V` is `C^0` but not `C^1` there: the detected trigger is the
   last mesh point where the obstacle is active, so a one-sided difference
   mixes both branches in a proportion that moves essentially at random, and
   the mixed quantity varies by a factor of sixteen across `lambda_S`
   non-monotonically. Nothing else depends on it.

**One item to reconcile before drafting**, found while assembling this pack:
`PROOFS_v2.tex` §"What is not established" currently says that whether an
*estimated* `lambda_V` varies with `lambda_S`, and in which direction, "is
not known". That was true when written. It is now answered numerically —
both moving-threshold conventions decrease across the grid, and the
measured-scale computation (`results/lambda_RN_measured.py`) confirms the
same direction under the panel's own estimator. It remains *unproven*, so
the sentence should be rewritten to say the direction is known numerically
and not analytically, rather than left as an open question it no longer is.

---

## 9. Smaller open items

| Item | Status | Action |
|---|---|---|
| `sorry` in `RateBased.lean` | Leaf, `saturated_ratio_tendsto`, at tanh -> 1 | Optional. No claim depends on it — Thm 1 is symbolic-canonical |
| H-operator convergence certification | Uncertified sweep | Needed only if Rem 1/Rem 3 quote H figures as certified; currently flagged in the appendix |
| Prop 5 verifier row | Symbolic now; `RateBased.lean` proves it sorry-free | His one-word call under the Lean-where-it-exists rule. Flagged in README, never changed silently |
| `RateBased.lean` header | ~80-line changelog | Strip to scope note on next touch — last history-character artefact |
| `d(level)/dK` | Not recorded | Needs `.mat` files; gates the full near-orthogonality claim (§5) |

---

## 10. File map

| Need | File |
|---|---|
| The results and their proofs | `00_document/PROOFS_v2.tex` (+ `.pdf`) |
| Verification status table | same, Appendix A |
| Lean proofs | `01_theory/lean_project/AsymCapital/` (SmoothFit, QVI_Part1, RateBased) |
| Symbolic ledgers | `01_theory/verify_rate_based.py`, `verify_state_map.py`, `verify_saturation_limits.py`, `verify_egarch_recovery.py` |
| Numerical solve and sweeps | `02_numerical/` |
| Machine-checkable claim registry | `00_reader/proof_registry.py` |
| Citation checker | `00_reader/check_citations.py` |
| Full reproduction | `04_reproduce/run_all.sh` |

Current harness state: 35 pass / 0 fail / 2 expected-fail / 1 skip.

**Not for this paper:** `00_document/EMPIRICAL_v2.tex`,
`03_empirical/` in its entirety, `00_reader/ledger_empirical.py`. Those
belong to the companion. If any panel material is used here it is a short
illustrative calibration with the estimation claim explicitly disclaimed —
and the safer choice is to omit it and let the companion carry it.
