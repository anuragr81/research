# Garber, D. (1980), "Field and Jeffrey Conditionalization" (Discussion)

*Philosophy of Science* 47(1), 142-145.

Contrary to the handover's warning ("badly garbled OCR in the middle pages"), this
particular Drive scan has a clean text layer throughout -- no image rendering was
needed. (The handover's caution may reflect a different, earlier scan attempt; this
one extracted cleanly with `pdftotext -layout`.)

## Claims formalized

Garber's central counterexample: repeated, phenomenally-identical, weakly-informative
glances at a ball in dim light, each processed via Field's (1978) $\alpha$-reparametrized
update, drive the belief in "the ball is blue" from a modest starting point to near
certainty in a handful of repetitions -- his argument that Field's portable, prior-free
input parameter $\alpha$ is not a coherent notion of "how much a stimulation affects
belief," because repeating an *unremarkable* experience should not compound this fast.

- **First example**: prior $p_0=P(E)=.3$; a single glance raises it to $q_0=.4$;
  Field's eq. (4) gives $\alpha=.2209$. Repeating the *same* $\alpha$ nine times drives
  $P(E)$ to $.3\to.4\to.5091\to.6173\to.7150\to.7961\to.8586\to.9043\to.9363\to.9581$.
- **Second example**: a "slightly richer" experience, $p_0=.3\to q_0=.5$, exceeds $.95$
  after only five repetitions.

## Result

Both sequences reproduced exactly in `sympy/check_repeated_glances.py` (matching
Garber's reported 4-decimal values at every step): $\alpha=.220916\approx.2209$; the
nine-step sequence matches to 4 decimal places throughout; the five-step sequence
reaches $.9674>.95$, confirming his "exceeds .95" claim.

No Lean formalization: this is a fixed numeric iteration (10 steps of a closed-form
recurrence), not a general identity -- SymPy's exact rational arithmetic already gives
a stronger check (exact fractions, not floating-point) than a machine-checked proof
would add.

## The philosophical point, not independently checkable

Garber's dilemma (not a computation, so not formalized): either (a) $P_1(E)$ is
independent of $P_0(E)$, in which case ordinary Jeffrey conditionalization (no
reparametrization) already applies and Field's fix is unneeded, or (b) $P_1(E)$ is not
independent of $P_0(E)$, in which case there's reason to doubt conditionalization is the
right mechanism at all, and reparametrizing it (rather than replacing it) misses the
point. He concludes Field's repair is "neither correct nor necessary," leaving the
deeper question -- what characterizes rational belief change when direct effects of
experience are *not* independent of prior belief -- explicitly open, a task he calls
"far more interesting and far more difficult" than reparametrization.

## Bearing on Paper B

This is the counterexample Hawthorne answers (per the handover): Garber's repeated
glances are a *within-basis* phenomenon (the same event $E$, glanced at repeatedly),
whereas Paper B's two cues are *distinct-basis* by construction (Assumption
`as:local`) -- exactly the configuration Hawthorne shows Field/Bayes-factor updating is
safest in. So Garber's objection, while numerically striking and now independently
verified, does not directly threaten Paper B's benchmark $\PB$: it is a caution about
reusing a *fixed* portable $\alpha$ under *repetition on the same basis*, not about
combining two cues on *different* attributes. Worth a citation only if the manuscript
ever needs to preempt a reader who has this specific counterexample in mind; it does
not, on its own, require any change to the manuscript's claims about $\PB$.
