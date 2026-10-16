# Mailath and Postlewaite (2006), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the text layer of
  WP 06-003, by exact normalised containment (LC-2).
- The model is formalised in the paper's notation (LC-4). Characteristics,
  payoffs, matchings, blocking pairs, stability over all pairs, the value
  equations and the stationary equilibrium of the balanced case are
  definitions. Propositions 1, 2 and 3, eqs. (3.1) to (3.8), the multiplicity
  of Section 3.3, the configuration of Section 3.4, footnote 9, the law of
  motion (5.1) and its limits, Proposition 5 on our reading, the steady state
  of Section 6, the ordering of purchases, the algebra of Example 2 and the
  white-shoe example are theorems. The coverage table in CLAIMS.md gives the
  status of every numbered result: 10 general, 9 special case, 2 not
  formalised.
- On 16 October 2026 the 54 theorems cited in CLAIMS.md were audited against
  a fresh compile of the module. Each uses only `propext`,
  `Classical.choice` and `Quot.sound`.
- Our reading is consistent with the source on these points, except where
  MP-D4 records that the printed rationed difference and threshold (5.2) do
  not follow from the paper's own value equations.

## What could not be verified

- Propositions 4 and 8 are not formalised. Proposition 4 is a limit of
  value functions under a regime switch. Proposition 8 rests on Brouwer's
  theorem, which Mathlib lacks.
- The continuity and unravelling steps of Propositions 6 and 7, the Pareto
  clause of Proposition 5, and parts of Examples 1 and 2 are read, not
  formalised.
- The published article in the International Economic Review is not read
  (MP-D1), so whether it corrects MP-D4 is unknown.

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1. The source
models a world A1 does not assume. Income is seen at matching (MP-Q6, MP-Q7),
and the authors state that there is "no asymmetric information, hence no role
for signalling" (MP-Q14). The judgements below compare the two worlds.

### The puzzle

The source says nothing about friending, contact across class lines or
racial diversity. What it does say runs against the direction of the puzzle.
When a visible attribute is valued, matching across income levels rises
rather than falls (MP-Q16). A rich agent without the attribute matches a poor
agent who has it (MP-Q9, `stable_mixed_iff`), and the variance of consumption
falls (MP-Q4). A valued marker is therefore a source of ties across class in
the source, not of friending bias. The source offers no mechanism by which a
marker lowers contact across class, which is the mechanism A1 needs.

### Budget compatibility

The source's origin of assortative matching on income is joint consumption,
which makes every agent prefer a richer partner (MP-Q5). Assortative matching
then follows from stability with no side payments (`prop1_stable`). The cost
of a match across income falls on the richer partner, as lost joint
consumption, and the poorer partner gains (`stable_mixed_iff`). In A1's
budget compatibility the cost of a gap is borne by both sides, because the
two budgets cannot sustain the same activities. The source's shared
consumption bears on cost in a second way. With concave utility a mixed
couple's consumption is worth more than the average of the two pure couples,
`u ≥ 1/2`, and this insurance is what makes matching across income valuable
(`control_risk_neutral`, `prop3`). Shared consumption in the source thus
pulls toward matching across class, where A1's budget compatibility pulls
away from it.

### Avoidance that depends on the observer's class (both directions)

In the source, whether an agent accepts a partner across income depends on
the agent's own income, as A1's avoidance depends on the observer's class.
The dependence is one-directional. A rich agent may refuse a poor one, which
is the `Hn` constraint (3.5) (`stable_mixed_iff`, `control_no_altruism`). A
poor agent never refuses a rich one: under rationing an `Ly` agent "always
wants to match with an Hn agent" (MP-Q12, `rationed_LyIC`). Direction (i) of
A1, avoidance running both ways between groups, has no counterpart, since
vertical preference (MP-Q5) rules out a poor agent avoiding a richer partner.
Direction (ii), a low-class member of the high group preferring outsiders,
has a counterpart in form. A poor holder of the valued attribute (`Ly`)
matches outside the attribute group with a rich agent who lacks it (MP-Q9,
`prop2_stable`). The reason differs from A1's. In the source the poor holder
seeks the outsider's income, and in A1 the low-class member seeks a closer
class.

### The penalty vanishing when class is shown

The source contains no such statement. Class is always shown in the source,
and the premium on the attribute is zero or positive depending on the
equilibrium. In the assortative equilibrium with an unproductive attribute
the two attributes have equal value (`valueEq_assort` at `k = 0`). In the
mixed equilibrium the attribute has a positive value
(`eq_3_4`, `multiple_equilibria_k0`). Both are equilibria under (3.8), and
which attribute is valued is itself a selection (MP-Q3). A premium on a
visible attribute that survives the display of class is what the source's
channel produces. A1's property, that the penalty vanishes when class is
shown, holds for A1's inference channel and fails for this one. The authors
separate their channel from the signalling channel of Fang (2001) in the same
terms (MP-Q13, MP-Q14), which is the link to the statistical-discrimination
literature that A1 can cite.

### The shadow market

The source has nothing on law, venues, platforms, group-neutral rules on
correlated variables, or signals that substitute for a banned criterion. Two
passages come near. Section 6 opens a market in which parents buy the
attribute for their children (`steady_state_assort`, `q_order`), which is a
market in the attribute itself rather than in information about where to go
or whom to meet. In the white-shoe example social skills act as "a record
device, similar to money as memory" (MP-Q18), and a young lawyer pays for
them by taking a wage below his output (`lawyer_accept`). That is a price on
a social credential with no productive value, not a market in social
information about other people's class.

### Premises of A1 the source does not share

- Class is seen, not inferred (MP-Q7, MP-Q14). A1's marker carries the
  group's class distribution; the source's attribute carries nothing.
- Preferences over a partner's income are vertical under joint consumption
  (MP-Q5). A1's engagement utility, `v − κ(c_i − c_j)²`, is homophilous.
- The source is a two-sided matching model with stability and no side
  payments (`Stable`). A1 is a Nash equilibrium of engagement.
- The source is dynamic, with a law of motion across generations
  (`law_5_1_high`, `rho_lt_half_tendsto`). A1 is static.
- The source's attribute is heritable, and its value comes from the matching
  prospects of children (MP-Q9). A1's marker has no intergenerational role.
- The source's equilibria are Markov in characteristics (MP-Q15), and
  multiplicity with equilibrium selection is central (MP-Q3,
  `multiple_equilibria_k0`). A1 does not rely on selection.

## Controls

- LC-2. A fabricated quotation, "agents cannot observe a partner’s income and
  infer it from the attribute", the opposite of MP-Q7 and MP-Q14, is found on
  no page. A true quotation is not found on a neighbouring page.
- Lean. Each main result has a control that drops one condition:
  `control_productive_blocks` (Proposition 1, `k = 0`), `control_no_altruism`
  (Proposition 2, `β > 0`), `control_risk_neutral` (Proposition 3 and (3.8),
  `u > 1/2`), `control_config_needs_small_k` (Section 3.4),
  `control_prop5_risk_neutral` (Proposition 5), `control_rho_half_stays`
  (Propositions 6 and 7, `ρ ≠ 1/2`), `control_prop7_no_rationing`
  (`prop7_core`) and `control_q_order_linear` (`q_order`, strict concavity).
- The suite's named controls are `control_no_altruism` and
  `control_risk_neutral`.

## Page images read

PDF pp. 7, 9, 11, 19, 20, 24, 26, 27, 28 and 35 of WP 06-003 and PDF p. 20 of
WP 04-025 were rendered at 110 dpi with `pdftoppm` from the pinned PDFs. They
are reproducible from them and not pinned separately.
