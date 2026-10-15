# POST-MORTEM — the label-attribution failure (read if you value not repeating it)

*Scope: this documents a specific, nameable methodological failure that cost ~a month on the
"Stereotype Rigidity" paper, the exact mechanism by which it survived verification, and a
pre-flight checklist to prevent its recurrence. It is written to be read cold by a future
session. It is not a verdict on the research or the researcher — the mathematics survived
intact and the correction is itself a clean result. Encode the **mechanism**, not "papers go
badly."*

---

## TL;DR

A structure — a cubic law of motion with a bistable self-confirming "closure trap" — was
**labelled** as the product of *Jeffrey rigidity*. It was actually the product of *belief-
tracking* (the responsive, Bayesian behaviour). The model only ever contained **one** law of
motion, so the comparison that would have exposed the mislabel was never even posed as a
question. SymPy and Lean verified the **true** claim ("the cubic map has a trap") exhaustively,
while the **false** claim ("this map *is* Jeffrey") lived entirely in the prose, one level
above everything that was being checked. Rigidity, written down honestly as its own dynamical
object, gives a *quadratic* map with a unique equilibrium and **no trap**. So the thesis
inverted: tracking creates the trap; rigidity prevents it.

## What happened, precisely

1. The paper's contribution rested on an **attribution**: the closure trap was claimed to be a
   consequence of *Jeffrey conditioning* (frozen belief), distinguishing it from a Bayesian
   benchmark.
2. But only one map was ever instantiated — the cubic, `phi = S_G * F(c_bar_t)` with `c_bar_t`
   evolving. "Jeffrey" was a *name attached to it*, never a second, distinct dynamical object.
3. Therefore the falsifying question ("write Jeffrey down as its own map; does *it* have the
   trap?") was never asked. There was nothing to compare against.
4. The moment Jeffrey is instantiated as its own system — frozen belief `phi = S_G * F(beta_0)`
   — the map becomes **quadratic**, unique interior fixed point, no bistability, no trap.
   (Verified: `verification/verify_bayes_jeffrey.py`.)
5. Self-confirming dynamics **require the belief to track the deteriorating state** — that
   responsiveness is what closes the loop. Rigidity is precisely *non-response*. The verbal
   thesis asked a frozen belief to do the tracking only a moving belief can do — a contradiction
   invisible in a sentence and invisible in a single-map model verified only against itself.

## The four reasons it survived a month

1. **One map, not two.** The comparison was never a posed question because the alternative was
   never an object. A label cannot be falsified if its negation is never instantiated.
2. **Verification scope.** SymPy/Lean check *entailment* (claims follow from assumptions). They
   do **not** check that the *label/interpretation* on a structure is correct. The 52 passing
   checks were all about the true claim; the false claim was never a checkable object.
3. **The false claim lived in prose.** "This cubic map represents Jeffrey conditioning" sat in
   the interpretation layer, above the equations, where no tactic or assertion ever reached it.
4. **Structural inevitability that wasn't yet seen.** Persistence (a trap, a self-confirming
   steady state) is a **fixed-point** property. Jeffrey's only distinctive content
   (order/path-dependence) is **transient**. A steady state is *where transients have died* —
   exactly the regime where Jeffrey must collapse onto, or invert relative to, Bayes. Attaching
   a transient-formalism name to a steady-state mechanism was structurally doomed from the
   start; the diagnostic to see that only crystallised during the correction.

## What was NOT lost (keep this accurate)

- Every proposition is mathematically correct as stated; the cubic still has its trap.
- All verification stands. Nothing computational was wrong.
- The correction is publishable: "belief-tracking sustains the trap; rigidity prevents it" is a
  clean, correct result, and the single-cue coincidence anchors it.
- A reusable diagnostic was produced (the transient-vs-steady-state scale rule), which is worth
  more than the mislabel cost.

What was lost: ~a month spent *believing a label*. Not a month of wrong mathematics.

---

## PRE-FLIGHT CHECKLIST (the actionable part — run this before building on any new thesis)

**1. Label-check / two-object rule.**
If the contribution rests on attributing a structure to one formalism over another — any
sentence of the form *"this is X, not Y"* (Jeffrey not Bayes; rigidity not tracking; structural
not epistemic; etc.) — then **instantiate Y as its own object** and compare X and Y directly.
Do not let a formalism's *name* do load-bearing work while only one object exists.

**2. Compare at the scale where the claimed effect lives.**
Match the comparison to the phenomenon. If the claimed effect is a steady state / fixed point /
aggregate, compare X and Y *there*, not in a transient. If it is a transient (order, path,
primacy), compare *there*. A coincidence or inversion at the wrong scale tells you nothing.

**3. Scale diagnostic (the red flag).**
Classify the target phenomenon: **transient/path-dependent** vs **steady-state/fixed-point/
aggregate**. Order-dependent or otherwise transient formalisms (Jeffrey vs Bayes is the type
case) *collapse onto or invert each other* at steady state and *bite* only in transients. If the
target is a steady state and the proposed mechanism is a transient formalism → **STOP**, this is
the failure mode; expect collapse/inversion and check for it first.

**4. Interpretation audit (separate from verification).**
SymPy/Lean verify entailment, not interpretation. For each load-bearing *prose* claim, ask: *is
this a checkable object, or a label?* If a label, write down what observation or derivation would
**falsify** it, and run that. Verifying that the math is correct is not verifying that the math
is *about* what you say it is about.

**5. Falsifier-first on the headline.**
Before building the apparatus, state the single derivation/observation that would kill the
central thesis, and run it in Phase 1 — not after the paper exists. (In the *correct* phase of
this project, sign-stability checks were run before building and caught three dead ends cheaply;
the lesson is to apply that same discipline to the *headline attribution*, earliest.)

**6. Self-undermining check.**
Does the verbal mechanism secretly require two contradictory properties? (Here: "self-confirming"
needs responsiveness; "rigidity" is non-response.) If the thesis sentence contains a tension
that the equations quietly resolve in one direction, that direction may be the real result.

---

## How to use this document

When opening either paper bundle, read this before touching the central thesis. If you find
yourself defending an attribution ("but this *is* Jeffrey because…"), that is the signal to run
the checklist, not to proceed. The cost of one extra comparison is minutes; the cost of skipping
it was a month.
