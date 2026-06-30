# PAPER B — FOUNDATION NOTES (multi-scale Jeffrey–Bayes)

*This bundle is a **foundation kit**, not a paper-in-progress: there is no manuscript yet.
It contains the verified result, its corollary, the dead ends already ruled out, and the
referee boundaries — everything needed to draft the manuscript from scratch in a fresh
session without re-deriving or re-walking anything.*

---

## The result (verified)

**Multi-scale Jeffrey–Bayes theorem.** An evaluator combines two soft cues about an
individual; each cue bears on one of two binary attributes (A, B) whose prior has covariance
`c`. A Jeffrey updater registers each cue as the **impression** it produces (a target
marginal, orthogonal conditionals held rigid); a Bayesian registers each as a **likelihood**
(matched single-cue likelihood `l_i = q_i / (prior marginal)_i`, so the rules are compared on
identical evidence). Then:

1. **Single cue** → Jeffrey = Bayes, **exactly**, for any `c` and any impression strength.
2. **Independent cues** (`c = 0`) → Jeffrey = Bayes, **exactly**, order-independent.
3. **Two correlated cues, individual belief** → the Jeffrey posterior diverges from Bayes,
   **order-dependently**, by **O(c)** — first order in cue correlation, vanishing at `c = 0`.
4. **Aggregate welfare** → the expected welfare loss vs the Bayes-optimal decision is **O(c²)**
   — second order. (Order-dependence flips only near-threshold decisions, where the welfare
   gradient is zero: an envelope/indifference argument.)

**Corollary — micro–macro decoupling.** Beliefs diverge at O(c); welfare at O(c²). That
order gap *is* the result: individual soft-cue evaluation is order-dependent and non-Bayesian,
while the aggregate it sums to matches Bayes to first order. A population of order-dependent
Jeffrey updaters is therefore statistically indistinguishable, in aggregate, from a population
of Bayesians. **Aggregate rationality-fit licenses no inference about individual-level
processing in the correlated-soft-cue regime.**

**The foundation to highlight (part 1).** A single cue, however uninformative, gives
Jeffrey = Bayes *exactly*. The entire effect is multi-cue. Low signal informativeness alone
produces nothing Jeffrey-specific — that is ordinary noisy statistical discrimination (Phelps,
Aigner–Cain), fully Bayesian. The Jeffrey content lives only in cue **correlation** `c`.

## What this is, and is not

- It is **positive / descriptive**: it predicts order-sensitivity in soft-cue evaluation that
  Bayes cannot produce (Bayes has exactly zero order-dependence), and it explains why that
  individual-level sensitivity is invisible in aggregates.
- It is **not normative**: there is no efficiency theorem and no robust discrimination theorem
  (see dead ends 2–4). The surviving stake is **horizontal equity** among marginal candidates
  (likes treated unalike by accident of sequence), not allocative efficiency.
- "Limit to tolerance" applications (migration acceptance, etc.) are a **weak illustration**,
  not the spine. Note the direction carefully: the washout makes the *aggregate robust* to the
  individual churn — so the contribution is "you cannot read individuals off the aggregate,"
  **not** "the aggregate limit is fragile." (Random order washes out → no aggregate shift;
  systematic order is sign-contingent → no robust shift. Either way the aggregate survives.)

## Dead ends already ruled out (do NOT re-walk)

1. **Persistence / closure framing.** The dynamic closure trap is a fixed-point property;
   Jeffrey's content is transient. At steady state Jeffrey collapses onto or inverts vs Bayes.
   Jeffrey does not belong in the persistence paper (that is Paper A, and its trap is
   belief-tracking, not rigidity). *Verified: frozen-Jeffrey map is quadratic / no trap.*
2. **Directional discrimination via a fixed favorable order.** There is no configuration-free
   "favorable order" a network could impose to systematically advantage a group. The favorable
   order depends jointly on relative base rates, cue valences, and the sign of `c`; it tracks
   `sign(β−α)` only ~86% with an offset boundary and **flips with the sign of correlation**.
   Same sign-contingency that sank the earlier "divergence" proposition. *Verified by grid.*
3. **Persistent-bias aggregation.** Order-averaging does not recover Bayes at the belief level
   (a symmetric O(c) bias survives), but its welfare effect is second-order. The directional
   bias is itself sign-contingent. *Verified.*
4. **Efficiency / deadweight-loss claim.** Order-instability causes **no** expected welfare
   loss — it flips only near-indifferent (marginal) candidates, where misallocation costs
   O(c²). What looked like a loss under a 0/1 accuracy metric at a knife-edge threshold was an
   artifact; under continuous welfare it washes out. *Verified by exact integration: slope ≈ 2,
   loss/c² → const.*

The common signal across 2–4: Jeffrey's one genuine property (order-dependence) sits in the
welfare-flat region of every decision problem posed, because the things it moves are
near-indifferent by construction. The positive/epistemic claim (the corollary) is what
survives; any normative upgrade reintroduces a ruled-out contingency.

## Two referee boundaries to state up front

1. **Positive, not normative.** Do not smuggle an efficiency or discrimination claim. The
   honest stake is horizontal equity for the affected (marginal) individuals.
2. **Order effects are well-trodden in psychology** (primacy/recency). Novelty rests on the
   **economics framing** and the **O(c) belief / O(c²) welfare decoupling** — the
   micro-rational/macro-Bayesian reconciliation — **not** on "order matters."

## Files in this bundle

- `verification/multiscale_theorem_verification.py` — the consolidated theorem, 7/7 checks:
  parts (1)–(2) exact symbolic, (3) O(c) symbolic, (4) O(c²) by exact integration.
- `verification/markers_jeffrey_vs_bayes_sympy.py` — the underlying order-dependence mechanics
  (Bayes commutes; Jeffrey does not; decision flips; effect requires correlated markers).
- `verification/SoftMarkers.lean` — Lean 4: `bayes_order_indep` (general) + `jeffrey_not_comm`
  (concrete 2×2 witness). Compiles.
- `verification/MultiScale.lean` — Lean 4: the two exact coincidences (single-cue; c=0).
  Compiles. Parts (3)/(4) stay in SymPy by design.

## Next step

Draft the manuscript in a fresh conversation from this kit. Suggested spine: the corollary
(epistemic micro–macro decoupling), built on the foundation (part 1, exact), with the
hard-vs-soft-profession contrast as the economic setting (one decisive hard signal →
Jeffrey-irrelevant; a bundle of correlated soft cues → high `c`). Keep the two boundaries
visible throughout.
