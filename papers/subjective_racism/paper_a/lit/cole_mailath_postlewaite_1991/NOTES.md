# Cole, Mailath and Postlewaite (1991), notes

## What was verified

- Fourteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the cached text layer (LC-2).
- Condition (13), the compensating capital `L_t(j)`, the value of a random
  match in the upper group of the hybrid equilibrium, the loss from
  banishment and the stationary ratio of eq. (6) are Lean theorems in the
  paper's notation (LC-4).
- Our reading is consistent with the working paper on these points.

## What could not be verified

- Propositions I to III and Properties 1 to 6 are read, not formalised. They
  are existence and limit results, and Paper A relies on none of them.
- The journal version (1992) is not read. A claim about it needs its own
  record.

## The bearing on Paper A1

Nothing in the paper contradicts the A1 model as described. The two models
differ in the respects below, and none of the four candidate contributions
is in the paper.

1. **Contribution 1 (avoidance from class homophily under inference).** Not
   in the paper. Matching takes place under complete information (CM-Q4), so
   nothing is inferred from a marker. Preferences over mates are common and
   vertical (CM-Q2), so the value of a partner does not depend on the
   chooser's own type, which is the dependence A1 derives from the squared
   gap. Within a group, the match payoff is linear in the partner's
   endowment, so a group's spread does not enter (`spread_blind`), where A1's
   quadratic loss adds the within-group variance.
2. **Contribution 2 (budget compatibility as the source of homophily).** Not
   in the paper. Matching is assortative on wealth (CM-Q6) because every
   woman prefers the wealthiest proposer, a vertical ranking, rather than
   because like matches like.
3. **Contribution 3 (venue exclusion, price screening).** Not in the paper.
   The authors chose matching precisely to avoid an allocator who might sell
   access (CM-Q13). The hybrid equilibrium has a closed upper group with
   random matching inside it and banishment for deviators (CM-Q10, CM-Q12),
   which is exclusion by birth and enforced by a norm, not by a price or by
   inference.
4. **Contribution 4 (the class channel vanishes when class is shown).** Not
   in the paper. Class is always shown.

**What A1 can cite.** The paper is a precedent for valuing a social position
only through the equilibrium matches it brings, rather than putting it in the
utility function (CM-Q3). A1 derives group avoidance from class homophily in
the same spirit, rather than from a taste over groups. The hybrid equilibrium
is a precedent for a group within which matching is random, so that a member's
expected match is the group mean (`upper_group_mean`). A1's group sufficiency
gives the same mean, and adds the variance because its loss is quadratic.

**Where the models part.** Status in the paper is assigned by a norm and
sustained across generations by bequests and punishment (CM-Q8). A1 has no
law of motion, and standing is the mean of the visible members, imposed as an
equilibrium consistency condition. The paper has no counterpart of that
condition, because status is a rank and not a belief.

## Controls

- LC-2. A fabricated quotation, "matching is voluntary and takes place based
  upon incomplete information", the opposite of CM-Q4, is not found. A true
  quotation is not found on a neighbouring page.
- Lean. `control_zero_status_deviates` shows that (13) fails at status 0, and
  `control_printed_value_not_mean` that the printed value of an upper-group
  match is not the group mean. `control_L_eq_k_at_zero` shows that the gap
  between `L_t(j)` and `k_t(j)` needs `j > 0`.
