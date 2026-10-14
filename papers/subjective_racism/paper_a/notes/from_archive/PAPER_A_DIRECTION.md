# PAPER A — DIRECTION NOTE (read this first)

*Status: the manuscript's framing has NOT been edited. This note records a corrected
interpretive thesis established after the manuscript was last revised. The mathematics in
the manuscript is intact and correct; one interpretive claim inverts. Read this before
working on `manuscript.tex` so you don't rebuild on the wrong attribution.*

---

## The one-sentence correction

The self-confirming **closure trap requires belief-TRACKING** — the responsive, Bayesian
behaviour in which the observer's belief follows the group's deteriorating standing. It is
**not** produced by Jeffrey rigidity. Frozen-belief Jeffrey *prevents* the self-confirming
trap; tracking *creates* it. The manuscript currently says the opposite in its prose.

## Why (verified)

Two laws of motion, distinguished by how the observer's believed conditional `beta` updates:

- **Tracking (Bayesian / responsive):** `phi_t = S_G * F(c_bar_t)` with `c_bar_t` evolving.
  The composed law of motion is **cubic** → bistable → the self-confirming closure trap.
  This is the form `prop:class-closure` actually uses.
- **Frozen Jeffrey (rigid):** `phi = S_G * F(beta_0)` constant. The map is **quadratic**,
  with a unique interior fixed point for all `beta_0`, all `delta` → **no bistability, no
  trap.**

So the trap is a property of *responsiveness*, not *rigidity*. Evidence:
`verification/verify_bayes_jeffrey.py` (the two-armed contrast that produces this inversion),
plus `verification/proof_coincidence_sympy.py` and `verification/Coincidence.lean` (at exact
sufficiency neither rule tracks, so the rules *coincide on no-trap*, not on the trap).

## What carries over VERBATIM (do not touch)

- **All propositions are mathematically correct as stated.** `prop:class-closure` (the cubic,
  the bistability, the fixed points), and the surrounding dynamics results, hold exactly.
- **All verification stands** — the SymPy checks and the Lean topology file are unaffected.
  The cubic still has its trap; nothing computational changes.
- `prop:coincidence` (single-cue Bayes = prior under sufficiency, full analytical proof) is
  correct and is the anchor for the corrected reading.

What inverts is a **label**, not an equation: "Jeffrey rigidity sustains the trap" →
"belief-tracking sustains the trap; rigidity prevents it." The cubic map was always a
belief-tracking object; it was mislabeled as Jeffrey.

## What must REFRAME (prose only)

- The thesis sentence(s) attributing the trap to Jeffrey rigidity.
- **The abstract** still says the model "recovers the statistical-discrimination benchmark
  at the standard-Bayesian limit" — this now contradicts `prop:coincidence` and must be cut
  or reworded. (Flagged, deliberately NOT edited — high-stakes wording, author's call.)
- Any Discussion text that leans on rigidity-as-cause.

## The general diagnostic (the reusable lesson)

Persistence — a closure trap, a self-confirming steady state — is a **fixed-point** property.
Jeffrey's only distinctive content (order/path dependence) is **transient**. They live at
different scales. A steady state is *where the transients have died*, so it is exactly where
Jeffrey must collapse onto, or invert relative to, Bayes. Attaching a transient-formalism
*name* to a steady-state mechanism was structurally doomed: the formalism's one real bite is
gone by the time you reach the object being studied.

Process note: verification checks that *claims follow from assumptions*. It does **not** check
that the *label* on a structure is the right label. The false claim ("this cubic map *is*
Jeffrey") never existed as a checkable object — it lived in the prose, one level above
everything that was verified. When a formalism's *name* is load-bearing in a paper,
instantiate the alternative as its own dynamical object and compare them **at the scale where
the claimed effect lives**, before attributing the effect to one of them.

## Open items for the next session

1. Decide the fate of the abstract's "standard-Bayesian limit" sentence (cut vs reframe).
2. Align the thesis/Discussion prose with the corrected direction (tracking, not rigidity).
3. Pre-existing cosmetic defect untouched: a double `∎` in ~12 proofs (explicit `\qed` +
   amsthm auto-qed). Paper-wide fix available, not applied.

## Companion

The *static / decision-level* result that came out of the same investigation (the multi-scale
O(c)/O(c^2) decoupling) is a **separate paper** with its own self-contained bundle
(`paper-B-multiscale`). It does not belong here, and nothing from this dynamics paper should
be imported into it.
