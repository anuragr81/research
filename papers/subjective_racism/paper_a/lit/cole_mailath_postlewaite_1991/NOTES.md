# Cole, Mailath and Postlewaite (1991), notes

## What was verified

- Seventeen quotations are found on the PDF pages they cite, by exact
  normalised containment in the cached text layer (LC-2).
- 32 Lean theorems implement the paper's model in its notation, with the
  primitives `u`, `lamStar`, `VoluntaryMatching` and the status rule `n` of
  eq. (12) as definitions. Of the 28 results in the coverage table of
  `CLAIMS.md`, 8 are formalised in general, 8 in a special case and 12 not
  at all (LC-4).
- Our reading is consistent with the working paper on these points.

## What could not be verified

- Propositions I to III, the Lemma, Properties 2, 4, 5 and 6, and eqs. (11),
  (14) and (15) are read, not formalised. The reasons are in the coverage
  table.
- The journal version (1992) is not read. A claim about it needs its own
  record.

## The bearing on Paper A1

Nothing in the paper contradicts the corrected account of A1. The paper is a
full-information model with vertical preferences, so it bears on A1 mostly
through the premises it does not share.

**The puzzle.** The paper says nothing about race, diversity or contact
across groups. It explains why one part of a society can close itself by
birth while the rest sorts by wealth (CM-Q12), and inside the closed group a
random match is worth the group mean (`upper_group_mean`), the same object A1
computes from a marker. The paper does not explain why closure would be
stronger where groups are more mixed, and it cannot, because no one infers
anything (CM-Q4).

**Budget compatibility.** The paper's assortative matching on wealth comes
from common vertical preferences (CM-Q2) and a protocol in which every woman
takes the wealthiest proposer (CM-Q6). The couple pools its budget into one
joint consumption, chosen "to avoid distributional issues" (CM-Q15), so a
richer partner is worth more to everyone. Under pooling a wealth gap produces
competition for the rich rather than avoidance of the poor. A1's budget
compatibility rests on the opposite premise, that the two budgets are not
pooled and each must sustain the shared activities. The paper is a reference
for what pooling implies, and the point at which A1 departs from it.

**Avoidance that depends on the observer's class.** (i) In the paper
avoidance runs one way. Every agent prefers a higher partner (CM-Q2), so only
the higher side refuses, as in condition (13) (`eq13`), and a low-status man
always wants to move up (`control_zero_status_deviates`). (ii) The hybrid
equilibrium has the opposite pattern to A1's second direction. Members below
the upper group's mean gain from pooling with it and members above lose
(CM-Q16, `hybrid_welfare`, `top_man_loss`). With a payoff linear in the
partner's endowment a low member of the high group never prefers outsiders,
and a group's spread does not enter (`spread_blind`). A quadratic loss makes
the spread count (`quadratic_spread`). Both of A1's directions therefore need
A1's homophily and are not available under the paper's preferences.

**The penalty vanishing when class is shown.** The paper has no such
statement. Matching is under complete information throughout (CM-Q4), so the
paper is itself the benchmark in which class is shown, and it offers nothing
to link to the statistical-discrimination literature.

**The shadow market.** The paper has no law, no banned criterion and no
intermediary. The authors chose matching precisely to avoid modelling an
allocator who could sell access (CM-Q13), and they name the alternative they
set aside, goods allocated by status rather than by markets (CM-Q17). That is
where a venue as an intermediary of social access would sit, and the paper
leaves it unmodelled. The closure of the hybrid group is by birth, enforced by
banishment (CM-Q8, CM-Q10), which is a formal group rule and not a shadow one.

**Premises of A1 the paper does not share.**
- Complete information rather than inference from a marker (CM-Q4).
- Common vertical preferences rather than homophily (CM-Q2).
- A payoff linear in the partner's endowment rather than a quadratic loss
  (`spread_blind`, `quadratic_spread`).
- Pooled joint consumption rather than separate budgets (CM-Q15).
- A law of motion across generations through bequests (`eq7`,
  `bequest_rises`) rather than a static equilibrium.
- Status as a rank assigned by a norm (CM-Q1, CM-Q8) rather than a group
  standing equal to the mean of the visible members.

## Controls

- LC-2. A fabricated quotation, "matching is voluntary and takes place based
  upon incomplete information", the opposite of CM-Q4, is not found. A true
  quotation is not found on a neighbouring page.
- Lean. Each main result has a control that drops a condition.
  `control_zero_status_deviates` for (13), `control_L_eq_k_at_zero` for
  `L_t(j)`, `control_printed_value_not_mean` for the hybrid match value,
  `control_euler_unique` for (10), `control_linear_allows_switch` for
  Property 1, `control_wis_rank_atom` for the wealth-is-status rank,
  `control_lambda_j_flat` for the two-period comparative static, and
  `quadratic_spread` for `spread_blind`.
