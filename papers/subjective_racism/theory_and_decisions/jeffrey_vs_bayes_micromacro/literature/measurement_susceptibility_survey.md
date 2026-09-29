# Are existing measurements susceptible to the identification problem?

A targeted read of two empirical papers, against one criterion: **what does the
paper elicit, and does that statistic register the sequence effect (unprotected)
or hide it (protected)?** The point is not to formalize their models but to
decide whether the paper's *instrument* is one the manuscript's identification
result would caution against — i.e. an aggregate audit that reads a protected
statistic and infers sequence-freeness from a null.

Read directly from Drive: Hogarth & Einhorn (1992), full plain-LaTeX
transcription; Zhao & Osherson (2010), full PDF.

**Bottom line.** Neither paper is susceptible, but for opposite reasons, and
together they bracket exactly the two halves of the instrument the manuscript
recommends. Hogarth-Einhorn measure only the level (unprotected, visible, but
incomplete on its own); Zhao-Osherson measure the conditional directly,
per subject (the hard quantity, elicited head-on rather than inferred from an
aggregate). The design the identification result actually attacks -- an
aggregate, association-only, benchmark-compared audit -- is run by neither.
That *sharpens* rather than weakens the contribution: the susceptible design is
the observational/field audit, and the careful lab literature sidesteps it by
never aggregating the association against a benchmark.

---

## Hogarth & Einhorn (1992), *Order Effects in Belief Updating*

**What they elicit.** A single scalar. The belief-adjustment model is
`S_k = S_{k-1} + w_k [ s(x_k) - R ]`, where `S_k in [0,1]` is the "degree of
belief in some hypothesis, impression or attitude after evaluating k pieces of
evidence" (Eq. 1). It is one-dimensional: a likableness rating, an estimated
average, a strength of belief in one hypothesis. There is no joint distribution
over two attributes anywhere in the paper, and therefore no cross-attribute
association. Response mode (Step-by-Step vs End-of-Sequence) governs *when* the
single scalar is reported, not *what* is reported.

**Protected or unprotected.** Unprotected. A level/marginal-type quantity is
exactly the class the manuscript shows carries the sequence effect at first
order (Prop. DRF). Consistent with this, HE document order effects (primacy and
recency) abundantly across 76 data points -- the effect is plainly visible in
their instrument.

**Susceptible?** No -- but not because it is safe; because it measures the
visible half and can never form the protected statistic. A single rating shows
an order effect yet cannot separate an order-dependent (amnestic) population
from a sequence-free one, because that separation needs the association, which
HE never elicit. This is precisely the manuscript's line-240 observation: the
belief-adjustment tradition measures a single evaluative anchor and so "must
also factor in the cross-attribute association," requiring a full posterior
table rather than one rating.

**Positioning.** HE is the precedent for the *right kind but incomplete*
instrument. It confirms the effect lives in the level (supporting the
unprotected row of Table 1 empirically) and motivates why the association must
be elicited alongside it. It is already cited; this read confirms the citation
is used correctly and can be sharpened from "focus on a single anchor" to "the
single anchor is an unprotected statistic -- visible but non-identifying."

---

## Zhao & Osherson (2010), *Updating beliefs in light of uncertain evidence:
Descriptive assessment of Jeffrey's rule*

**What they elicit.** Conditional probabilities, directly and by name. In
Experiment 1 participants estimate `Pr(G)`, `Pr(B)`, `Pr(G|B)`, `Pr(G|~B)`,
`Pr(B|G)`, `Pr(B|~G)` -- twice, before and after a dim-flashlight glimpse of a
card's colour. The test of "invariance" (their term; they note Jeffrey's is
"rigidity") is whether `Pr_2(G|B) = Pr_1(G|B)` and `Pr_2(G|~B) = Pr_1(G|~B)`.
This is the rigidity assumption measured head-on, at the level of the individual
conditional, within subject.

**Protected or unprotected.** The elicited object is the conditional itself --
the quantity whose constancy *is* rigidity, and which sits on the
association/dependence axis, not the level axis. But the manuscript's
protected/unprotected dichotomy is a statement about an *aggregate* statistic
compared to a benchmark; ZO do not compute one. They measure each subject's
conditional directly and check its movement over time.

**Susceptible?** No, and instructively so. ZO sidesteps the identification
problem entirely by not aggregating: measuring `Pr(G|B)` directly per subject is
immune to the protection effect, which only arises when an observer infers the
association from a pooled belief and compares it to a sequence-free benchmark.
ZO is thus the existence proof that the "hard" quantity -- the conditional /
association -- *can* be elicited directly rather than inferred. Their question is
also different from the manuscript's: they test whether the rigidity *assumption*
holds descriptively (finding rough, selective conformity -- stability in the
conditionally-independent direction `Pr(G|B)`, larger movement in the
non-invariant converse `Pr(B|G)`), not whether a population is detectably
sequence-dependent.

**Two points worth carrying into the manuscript.**

1. *ZO is the complementary instrument to HE.* HE measures the level (visible,
   non-identifying); ZO measures the conditional (identifying, but by direct
   per-subject elicitation, not aggregate audit). The manuscript's recommended
   design -- elicit the full posterior table, i.e. levels *and* association -- is
   exactly the union of the two, and neither paper alone runs it. This is the
   cleanest way to state what an identifying measurement requires.

2. *ZO independently supports the two-horn "marginal reading."* Their discussion
   reaches for "the ineffable character of sensory impressions (Jeffrey 1983,
   x11.1)" to explain why invariance was violated *more* under the vivid
   flashlight than under the tangible lottery draw. That is close to the
   two-horn draft's claim that an impression fixes a level rather than a
   reportable conditional: the more purely sensory the cue, the less well
   subjects hold a numeric conditional fixed. ZO is not currently in the
   bibliography; if the two-horn material goes in, ZO is a natural citation for
   the marginal reading and for "rigidity is elicitable but noisy on genuinely
   sensory input."

---

## Does anyone frame this as an identification / observational-equivalence problem?

Not in these two. HE frame it as a descriptive taxonomy of order effects; ZO as
a descriptive test of the rigidity assumption. Neither states that a protected
aggregate statistic can make a sequence-dependent population look sequence-free,
which is the manuscript's claim. On the strength of the two most relevant
empirical papers, the identification framing appears novel; the honest phrasing
remains "has received little attention" rather than "none," since a negative
across the whole literature cannot be established from two reads.

## Zhao, Crupi, Tentori, Fitelson & Osherson (2012), *Updating: Learning versus
## supposing* (Cognition 124, 373-378)

**What they elicit.** A single directly-elicited probability judgment per
subject: `Pr(A)` after *learning* B versus `Pr(A|B)` when B is only *supposed*,
in a between-subjects yoked design. Bayesian orthodoxy (their Eq. 1) requires
`Pr_2(A) = Pr_1(A|B)`; they test whether ordinary judgment obeys it.

**Finding.** It does not. Supposing B has less impact on the credibility of A
than learning B (Exp. 3: learn `Pr(A|B)` = 0.64, suppose = 0.53, control `Pr(A)`
= 0.51; suppose is statistically indistinguishable from the control that never
saw B). Their footnote 1 frames the violation as a failure of the invariance of
`Pr(A|B)` for the *learned* event.

**Protected or unprotected / susceptible?** Same verdict as ZO 2010, and for the
same reason: the instrument is direct per-subject elicitation of a probability,
not an aggregate association-vs-benchmark audit, so the protection/identification
issue does not arise. It adds no third instrument type.

**Bearing on Paper B.** This is the *hard-learning* regime -- B is raised to
certainty -- which Paper B explicitly places out of scope (Assumption `as:soft`;
two hard cues already commute). The paper's own conclusion says the
learning/supposing debate "might be of limited relevance to the typical
transition from one probability distribution to another," where confidence in B
is revised without reaching certainty, and points to Jeffrey's rule and Zhao &
Osherson (2010) as the relevant direction -- i.e. it hands off to exactly the
soft-cue regime Paper B occupies. One incidental detail is on-point: in Exp. 3,
*learn* participants treated a win in one swing state as raising the chance of a
win in another (a cross-attribute association induced by the evidence), while
*suppose* participants did not -- but this too is read off direct per-subject
estimates, not an aggregate audit. Net: confirms, does not revise, the framing.

## Overall re-assessment (three papers)

The empirical literature on Jeffrey/rigidity elicits probabilities directly --
a level (Hogarth-Einhorn), a conditional (Zhao-Osherson), or a learned vs
supposed judgment (Zhao et al. 2012) -- always per subject, never as an
aggregate association compared to a sequence-free benchmark. None runs the
design the manuscript's identification result actually attacks. This is now
checked across all three most relevant papers, so "the identification framing
has received little attention" is well supported; only an exhaustive-search
claim ("no one") remains unavailable, as it must from any finite reading.

## Scope / not done

Targeted susceptibility reads only -- no formalization subdirectory, since none
of the three contributes an algebraic identity to reproduce. All three most
relevant empirical papers have now been read directly from Drive.
