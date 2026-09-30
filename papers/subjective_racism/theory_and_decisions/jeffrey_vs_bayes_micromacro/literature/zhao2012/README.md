# Zhao, J., Crupi, V., Tentori, K., Fitelson, B. & Osherson, D. (2012), "Updating: Learning versus supposing"

*Cognition* 124, 373-378. DOI 10.1016/j.cognition.2012.05.001.

**Source.** `zhao2012.pdf` is the journal PDF, pp. 373-378. Read in full on
2026-09-30. The text layer keeps the tables intact.

## Claims formalized

The paper is empirical. Its formal content is the identity it tests and the
gloss on it in footnote 1:

* **Eq. (1) UPDATING FOR LEARNED EVENTS** (p.373). "If `B ⊆ Ω` is learned
  between times 1 and 2 (and nothing else relevant is learned) then for all
  events `A ⊆ Ω`, `Pr₂(A) = Pr₁(A|B)` (provided that `Pr₁(B) > 0`)." They add
  that "`Pr₂` as defined by (1) is a genuine probability distribution and …
  `Pr₂(B) = 1`".
* **Footnote 1** (p.373). "Violation of (1) can be conceived as failure to
  respect the invariance of conditional probability for the learned event B.
  This is because failure to update via (1) yields
  `Pr₂(A|B) = Pr₂(A|Ω) = Pr₂(A) ≠ Pr₁(A|B)`."

The design and results that the project reports:

* **Design.** Between-subjects and yoked: each suppose participant is yoked to
  the preceding learn participant. There are five judgments per participant:
  five decks in Experiments 1-2, and in Experiment 3 "This procedure was
  performed five times per participant" (p.376). Fig. 1 shows "100 estimates
  (20 participants, each providing 5 estimates)".
* **Experiment 3** (Table 5, p.377). The learn and suppose means of the raw
  estimate are 0.64 and 0.53. The control group (N = 20), "in which just `Pr(A)`
  was estimated", has mean 0.51. This "is close to the 0.53 estimate … in the
  suppose group [t(19) = 0.51, p = .61] but reliably different from the 0.64
  estimate of the learn group [t(19) = 4.09, p < .001]".
* **The swing-state conclusion** (p.377). "learn participants interpreted a win
  [loss] of one swing state to increase the chance of a win [loss] of another.
  In contrast, suppose participants' estimates of `Pr(A|B)` were almost
  identical to the control group's `Pr(A)`."

## Result

`lean/Zhao2012.lean` is a symlink to `lean/Literature/Zhao2012.lean`. It is
standalone on Mathlib and checks with `lake env lean`, with no `sorry`. Its 22
theorems use only the axioms `[propext, Classical.choice, Quot.sound]`; 7 of
them use no axioms.

| Lean theorem | Content |
|---|---|
| `learn_isDist`, `learn_prB` | `Pr₁(· \| B)` is a distribution with `Pr₂(B) = 1` (p.373) |
| `eq1` | **eq. (1)**: `Pr₂(A) = Pr₁(A\|B)` |
| `learn_invariance` | updating by (1) keeps `Pr(A\|B)` invariant |
| `cond_eq_marg_of_certain` | fn 1, first step: `Pr₂(B) = 1 ⇒ Pr₂(A\|B) = Pr₂(A)` |
| `fn1` | **fn 1**: for any `Pr₂` with `Pr₂(B) = 1`, violating (1) ⟺ `Pr₂(A\|B) ≠ Pr₁(A\|B)` |
| `five_per_participant` | 100 estimates = 20 participants × **5** (Fig. 1) |
| `exp12_groups`, `exp3_groups_disjoint` | Exps 1-2: 20 + 20 = 40. Exp 3: a "new group of sixty" = 20 learn + 20 suppose + 20 control, so the three groups are disjoint; 60 × 5 = 300 judgments |
| `swingStates_length`, `swingStates_nodup` | the 20 swing states of fn 4 |
| `exp3_order`, `exp3_gaps` | control 0.51 < suppose 0.53 < learn 0.64. The gaps are learn − suppose 0.11, suppose − control 0.02 and learn − control 0.13 |
| `exp3_suppose_near_control` | the suppose-control gap is less than a sixth of the learn-control gap |
| `exp3_between_group` | the swing-state comparison is a difference of means between groups of different participants |
| `exp3_interaction` | consistent vs inconsistent pairs: (0.72 − 0.50) − (0.55 − 0.48) = 0.15 > 0 |
| `brier_half`, `exp3_penalties` | 0.5 "guarantees a penalty of 0.25"; learn 0.18 < suppose 0.25 < control 0.29 |
| `table1_decks`, `table3_decks` | every deck in Tables 1 and 3 has 20 cards |
| `exp2_more_extreme`, `extremeness_rows` | Exp. 2's objective conditionals are more extreme (mean `\|Pr(A\|B) − ½\|` 0.132 vs 0.302), as p.375 says; the (b) rows of Tables 2/4 flip accordingly |

`sympy/check_zhao2012.py` (20/20 PASS) re-derives eq. (1) and fn 1
symbolically. It also checks the design, deck and Experiment 3 facts, and the
binomial tests:

* 16 of 20, printed p = .01: exact two-sided p = .0118.
* 17 of 20, printed p = .01: exact p = .0026, which satisfies "≤ .01" but would
  round to .00.

It also prints a consistency note. If 60% of pairs are consistent for every
participant, then the Table 5 means 0.64 and 0.53 are compatible with the
consistent/inconsistent means (0.72/0.50 and 0.55/0.48), but only at the edge
of rounding.

## What formalizing revealed

1. **Footnote 1 is a two-line theorem, and it holds for every time-2
   distribution that makes `B` certain, whether or not it came from updating**
   (`fn1`). "Violating (1)" and "failing invariance of `Pr(A|B)`" are the same
   condition. The footnote does not say that (1) is equivalent to invariance
   for other conditionals.
2. **Five judgments per participant, and three disjoint groups in
   Experiment 3.** Both are arithmetic consequences of the reported counts
   (100 = 20 × 5; 60 = 3 × 20). The control comparison is between different
   participants, yoked in triples, so the tests are `t(19)`. It is not a
   within-subject change.
3. **Minor, about the paper.** The 17-of-20 binomial p is reported as ".01",
   whereas the exact two-sided value is .0026.

## Bearing on Paper B

The paper is not cited in the manuscript or the write-ups (review log Entry 15).
It remains in `measurement_susceptibility_survey.md` as research history. It
sits in the hard-learning regime (`B` revealed, `Pr₂(B) = 1`), which Paper B
places out of scope. Its own conclusion hands off to Jeffrey's rule and
Zhao & Osherson (2010) for the soft case (p.377).

## Audit findings (2026-09-30)

This section summarises `notes/citation_audit.md` L10 (c) and (d) and
`notes/citation_audit/verify_record_papers.md` §4, checked against the paper
and the formalization.

* **L10(c) / VRP 4.2 (WRONG-NUMBER), survey :126.** "A single
  directly-elicited probability judgment per subject" is wrong, and this is
  confirmed:
  * Exps 1-2 used five decks per participant (p.374).
  * Exp 3 says "This procedure was performed five times per participant"
    (p.376).
  * Fig. 1 shows 100 estimates from 20 participants (`five_per_participant`).
* **L10(d) / VRP 4.10 (CONTRADICTED), survey :152-153.** "This too is read off
  direct per-subject estimates, not an aggregate audit" is contradicted, and
  this is confirmed. The swing-state conclusion compares group means of three
  disjoint groups of 20:
  * the learn mean 0.64 and the suppose mean 0.53 against the control group's
    mean `Pr(A)` of 0.51 (`exp3_groups_disjoint`, `exp3_between_group`);
  * plus a two-way ANOVA on consistent vs inconsistent pairs.

  So it is an aggregate comparison against a no-evidence benchmark.
  Qualification to VRP 4.10: the groups are yoked in triples and the tests are
  paired (`t(19)`), but the participants are different people. So "between-group
  (yoked)" is exact, and "per-subject" is not.
* **VRP 4.5, 4.4, 4.3, 4.6, 4.9, 4.12 (VERIFIED)** are confirmed:
  * the Experiment 3 means 0.64, 0.53 and 0.51 (`exp3_order`, `exp3_gaps`);
  * eq. (1) (`eq1`);
  * the yoked between-subjects design;
  * fn 1 (`fn1`);
  * the swing-state description (which covers losses too);
  * the hard-learning regime.
* **VRP 4.7 (VWC), survey :144-146.** The quotation "might be of limited
  relevance to the typical transition from one probability distribution to
  another" is verbatim. Its subject is "the debate about (1)", that is, the
  normative status of Bayesian updating, not the learning/supposing debate.
* **VRP 4.11 (VWC), survey :157-161.** "Always per subject, never as an
  aggregate association compared to a sequence-free benchmark" survives only
  through its qualifiers. Zhao et al. compare group means against a control
  benchmark. The statistic is a level, not an association, and the benchmark is
  not "sequence-free".

### Proposed corrected wording (not applied)

* **Survey :126-128.** Replace "A single directly-elicited probability judgment
  per subject: `Pr(A)` after *learning* B versus `Pr(A|B)` when B is only
  *supposed*, in a between-subjects yoked design." with:
  > Five directly elicited probability judgments per participant (five decks,
  > or five trials in Experiment 3): `Pr(A)` after *learning* B versus `Pr(A|B)`
  > when B is only *supposed*, in a between-subjects yoked design (plus, in
  > Experiment 3, a control group estimating `Pr(A)` with no B evoked).
* **Survey :137-140** (the "Protected or unprotected / susceptible?"
  paragraph). Replace "the instrument is direct per-subject elicitation of a
  probability, not an aggregate association-vs-benchmark audit" with:
  > the instrument is direct elicitation of a probability, compared across
  > groups by their means (learn, suppose and, in Experiment 3, a no-evidence
  > control); the compared statistic is a level, not an association
* **Survey :144-146.** Replace "The paper's own conclusion says the
  learning/supposing debate "might be of limited relevance …"" with:
  > The paper's own conclusion says that the debate about the normative status
  > of (1) "might be of limited relevance to the typical transition from one
  > probability distribution to another"
* **Survey :149-153.** Replace from "One incidental detail is on-point" to "not
  an aggregate audit." with:
  > One incidental detail is on-point: in Exp. 3, the mean estimate of *learn*
  > participants (0.64) exceeded that of a separate control group asked for
  > `Pr(A)` with no conditioning event (0.51), so learning a win [loss] in one
  > swing state raised the judged chance of a win [loss] in another, while the
  > *suppose* group's mean (0.53) was indistinguishable from the control's.
  > This is a between-group comparison of mean levels against a no-evidence
  > benchmark -- an aggregate comparison, though of a level, not of an
  > association, and not against a sequence-free benchmark.
* **Survey :157-160.** Replace "always per subject, never as an aggregate
  association compared to a sequence-free benchmark" with:
  > per subject (Zhao-Osherson) or as group means of a level against a
  > no-evidence control (Zhao et al. 2012), never as an aggregate association
  > compared to a sequence-free benchmark

## Not formalized

* The t-tests and the ANOVA. The binomial tests are recomputed in the sympy
  script only.
* Figure 1's scatter.
* The quadratic penalties beyond the 0.25 benchmark.
