# Austen-Smith and Fryer (2003), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the text layer (LC-2).
- The model is implemented in Lean in the paper's notation. Lemmas 1, 2 and 3,
  Proposition 1, the comparative statics of eq. (11), the role of assumption
  (7) and Proposition 9 are formalised in the paper's generality. Lemma 4,
  Remark 1, Proposition 3, Corollary 1 and Proposition 8 are formalised in
  part. Propositions 2 and 4 to 7 are not formalised (coverage table in
  `CLAIMS.md`).
- Our reading is consistent with the working paper on these points.

## What could not be verified

- The printed threshold of Proposition 9 for `γ > 0` (AS-D2). The threshold
  derived from the paper's own payoffs is adopted.
- The equilibrium results that rest on the D1 refinement.
- Nothing is attributed to the published 2005 version, which is not read.

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1. In this model
the unseen type is ability, read from a chosen signal, education, and not from
a group marker. Firms ignore group membership and the group does not condition
on it (AS-Q2). Race enters only in the conclusion, as which individuals face a
peer audience (AS-Q13).

### The puzzle

The source offers a channel for bias in friendship across the group line that
differs from A1's. The behaviours the authors' epigraph lists as sanctioned
include "having white friends" (AS-Q10). In the model a signal sent to one
audience is read by the other (AS-Q1), so a cross-group tie can cost a member
acceptance in the member's own group. That cost comes from the member's own
group reading the signal, not from the partner inferring the member's class.
The model does not contain friendships, and the epigraph is quoted rather than
derived. A1 can cite the source as a rival channel that operates on the race
line, where A1's operates on the class line.

### Budget compatibility

The source's foundation for who is accepted is reliability under repeated
interaction, not budgets. Membership costs effort, "not money per se"
(AS-Q15), and the group has no preference over income (AS-Q2). The cost of a
contribution, `θk`, rises with ability, so the able are the unreliable members
(`AustenSmithFryer2003.lemma1_threshold`). Nothing bears on affordability or
shared consumption. A1's budget foundation is a different premise from the
source's, and the two do not conflict.

### Avoidance that depends on the observer's class

- **Both directions.** The source has one group, and the majority faces no
  peer audience (AS-Q13). It neither supports nor contradicts two-way
  avoidance.
- **The member who differs from the group.** The source has the mirror image
  of A1's direction (ii). In A1 a low-class member of the high group may prefer
  outsiders. In the source a member whose type is above the group's critical
  type leaves the group, either by separating at school or by refusing the
  first high contribution (AS-Q9, `AustenSmithFryer2003.corollary1_uniform`).
  The source thus supports the general point that a member who differs from
  the group's own type has weaker reasons to stay. It limits the parallel,
  because the difference runs upward and the exit is to the labour market,
  not to another group.
- The group itself has no type, so its acceptance cannot depend on an
  observer's class (`AustenSmithFryer2003.lemma2_accept_iff`).

### The penalty vanishing when class is shown

The source has the parallel property for ability, with a condition A1 does not
have. When the signal reveals the type, "there is no residual incomplete
information" and the group accepts exactly the reliable types (AS-Q8,
`AustenSmithFryer2003.prop3_3_core`, `AustenSmithFryer2003.control_separation_rejects`).
The wage is the conditional mean of output given the signal (AS-Q5), so a
pooled worker is paid the pool's mean and a revealed worker his own output.
The conclusion turns this into a group difference. At one education level a
minority worker pools and a majority worker separates, so the two signals
differ as Milgrom and Oster (1987) postulate (AS-Q14), and the authors read
this as support for Phelps's noisier perception of minority ability (AS-Q12).
The condition is that the type can be revealed at all. Under peer pressure no
equilibrium reveals every type (Proposition 2, not formalised), so in the
source the penalty cannot vanish for the whole group. In A1 the property is
stated for class shown exogenously, and the source shows what happens when
showing it is a costly choice.

### The shadow market

The source places group-based sanction outside the labour contract. Firms are
assumed to ignore group membership (AS-Q2), which matches a contracting
channel closed to group, and the sanction operates through the peer group's
acceptance, a contact channel the firm does not reach (AS-Q1). The group
screens on education because reliability is unseen (AS-Q6), a signal standing
in for a criterion that cannot be observed rather than one the law forbids.
When cheap talk is available and individuals are patient, the tolerant group
needs no screen (`AustenSmithFryer2003.prop8_honest_iff`). The source has no
law, no venues, no prices and no intermediaries of social information, so
none of the corrected account's comparative statics of the shadow market can
be traced to it.

### Premises of A1 the source does not share

- The unseen type is ability, which lowers the cost of education and raises
  the cost of contributing, not class.
- Group membership is not a marker that anyone reads (AS-Q2).
- The observer is one anthropomorphised group with no type of its own.
- The signal is chosen, and acceptance is a threshold on a posterior mass, not
  an expected squared gap (`AustenSmithFryer2003.lemma2_accept_iff`).
- The model is dynamic, with repeated contributions after school, and the
  equilibria are selected by the D1 refinement.
- The spread of accepted types is an outcome of patience (AS-Q11), not a term
  in anyone's payoff.

## Controls

- LC-2. A fabricated quotation, "firms are assumed to care about each
  employee's group membership", the opposite of AS-Q2, is not found. A true
  quotation is not found on a neighbouring page.
- Lean. Each main result has a control that drops one condition.
  - `control_lemma1_needs_7` drops `π v(1|1) < v(1|0)` from Lemma 1.
  - `control_no_assumption7_all_accepted` drops (7) from Lemma 2, as AS-Q7
    says.
  - `control_lemma3_needs_single_crossing` drops single crossing from Lemma 3.
  - `control_remark1` drops the strict gap in (3) from Remark 1.
  - `control_separation_rejects` drops pooling from Proposition 3(3) and
    Corollary 1.
  - `control_prop8_needs_3` drops `v(1|1) > v(1|0)` from Proposition 8.
  - `control_prop9_needs_pi_lt_one` drops `π < 1` from Proposition 9.
- The suite names `control_no_assumption7_all_accepted` and
  `control_separation_rejects`.
