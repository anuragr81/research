# BIAS–VARIANCE ANGLE — exploration note (Paper A)

*Status: EXPLORATORY. Nothing in this document is a thesis yet. Per POSTMORTEM.md, the
bias–variance framing may enter the manuscript only after (a) the estimator is instantiated
with the bias term and the variance term explicitly named, and (b) the directional claim is
SymPy-verified for sign-stability. Until then it is a candidate framing, not a result.*

---

## The candidate idea, stated precisely

Humans evaluate one another through two channels:

- **Individuating attributes** — person-specific soft cues (appearance, health indicators,
  manners, friendliness). High-dimensional, roughly idiosyncratic to the individual,
  informative about *this* person, each cue noisy.
- **Group tags** — shared category markers (cultural background, religious conviction,
  political inclination). Low-dimensional, shared across many people, functioning as a
  prior/pooling term: they let the evaluator borrow information across everyone bearing
  the tag.

The candidate formal reading: an evaluator's estimate of an individual's type trades off
these channels as a **bias–variance trade**:

- Weighting the **group tag** = committing toward the group mean: stable across encounters
  (**low variance**) but systematically wrong for any individual who departs from their
  group (**bias**).
- Weighting **individuating attributes** = tracking the actual person (**low bias**) but each
  soft cue is noisy, so the estimate is unstable (**high variance**).

On this reading, statistical discrimination is the acceptance of bias to buy variance
reduction. This is a *static* trade-off — no dynamics, no steady state — which keeps the
framing safely outside the persistence regime that produced the original inversion.

## THE TRAP (read before drafting anything)

"Group = bias, individual = variance" is clean only when the group tag is a **hard** marker
(fixed, certain). Paper A is about **soft** markers and Jeffrey updating — and a soft
group-tag *impression* is itself uncertain, so it carries variance too. Two distinct axes are
in play and must not be collapsed:

1. **The pooling axis** — using the group mean as a shrinkage target trades bias for
   variance. (This is the bias–variance trade proper.)
2. **The softness axis** — how much variance any given cue (group OR individual) carries as
   evidence. (This is about the evidence channel, not the pooling decision.)

They interact but they are not the same thing. Collapsing them is exactly the
two-things-under-one-label failure documented in POSTMORTEM.md. Every sentence using
"bias" or "variance" must be traceable to axis 1 or axis 2 explicitly.

## Qualification gate — what must exist before this enters the manuscript

1. **Instantiate the estimator.** Write down the evaluator's estimate of the individual's
   type as an explicit function of (i) the group-tag channel and (ii) the individuating
   channel, with a weighting between them. Derive MSE = bias² + variance and point to which
   term is which. No prose use of "bias" or "variance" that does not correspond to a named
   term in this decomposition.
2. **Verify the directional claim (SymPy).** The candidate claim "Jeffrey updating on a
   soft group-tag shifts the bias–variance trade in direction D" is a *directional* claim.
   Directional claims in this project have repeatedly proven sign-contingent (divergence;
   favorable-order; persistent-bias direction). Before any direction is asserted: run the
   sign-stability check across the parameter grid (base rates, cue valences, correlation
   sign). If the direction is configuration-dependent, the paper may state only the
   conditional or magnitude version.
3. **Position against the two admitted literatures** (below) so the contribution is the
   formal soft-marker piece, not a re-description of either.

## Admitted literatures (two, by design)

**Statistical discrimination — the Phelps line.** Phelps (1972), Arrow (1973),
Aigner–Cain (1977) — all already in the Paper A bibliography. This tradition is explicitly
about using group membership as an informative prior when individual signals are noisy:
the pooling-vs-individuating trade in economic form, though rarely named as bias–variance.
This is the economic home of axis 1. The candidate contribution relative to it: making the
bias–variance structure explicit *and* asking what changes when the group tag itself is a
soft (Jeffrey-updated) impression rather than an observed category — axis 2, which the
Phelps line does not have.

**Category-based vs individuating impression formation — the Fiske–Neuberg line.**
Fiske & Neuberg's continuum model (impression formation runs from category-based processing
toward individuating processing as motivation/attention increase) is the canonical
psychological statement of the SAME dichotomy, empirically grounded and process-oriented but
not formal. [CITATION UNVERIFIED — the canonical reference is believed to be Fiske &
Neuberg 1990, Advances in Experimental Social Psychology, vol. 23 — verify before citing;
do not cite from memory.] The candidate contribution relative to it: a formal
characterization of *when and why* the trade tips, under soft markers specifically. The
dichotomy itself is NOT novel and must not be presented as such.

**Excluded by decision: the machine-learning / statistics line** (shrinkage, James–Stein,
empirical Bayes, hierarchical models, algorithmic fairness). Reason: Paper A is a theory
paper, not an empirical one; the ML register would import an estimation-theoretic framing
and referee expectations (data, prediction, loss curves) foreign to the paper. The
bias–variance *decomposition itself* is standard probability and needs no ML citation.

## Relation to the existing Paper A machinery

- The two-channel structure is already implicitly present: the group prior (Beta–de Finetti
  belief over the group) is the pooling channel; the soft marker impression is the
  individuating channel. The candidate framing may therefore be largely a *re-description*
  of existing objects — which would be good (no new machinery) — but this must be checked
  against the actual model, not assumed.
- The framing is static. It does not touch the law of motion, the closure map, or any
  fixed-point object, and must not be allowed to drift there. Any sentence connecting
  bias–variance to *persistence* is out of scope by construction (see POSTMORTEM.md,
  scale diagnostic).

## Next steps (in order; do not skip ahead)

1. Search and verify the two admitted literatures: what exactly Phelps/Aigner–Cain already
   say in variance terms; the precise Fiske–Neuberg citation and claim. (Web search task.)
2. Instantiate the estimator inside the existing Paper A model (group prior + soft marker),
   derive the MSE decomposition symbolically.
3. SymPy sign-stability check on the directional claim before any direction is asserted.
4. Only then: decide whether the framing earns a section in the manuscript or remains a
   remark.
