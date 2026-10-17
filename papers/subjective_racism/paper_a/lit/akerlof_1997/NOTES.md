# Akerlof (1997), notes

## What was verified

Our reading of eqs. (1) to (5), the benefit of Section 3 and the intrinsic
case of eq. (6) is consistent with the source and proved in
`lean/mathlib/Akerlof1997.lean`. The three-person example is not formalised,
because its conditions carry no explicit bounds.

## What could not be verified

The three-person example, and the paper's verbal claims about rational
expectations and about how positions update.

## The bearing on Paper A1

1. **The engagement utility.** A1's loss `κ(c − y)²` in the class distance is
   the quadratic form of eq. (5), which Akerlof calls "a special case"
   (AK-Q9). His main form is a benefit that falls in distance by a gravity
   formula, taken from "the common practice" of trade models (AK-Q11) and not
   derived. A1 now derives the quadratic loss from separate budgets and one
   shared activity (M19). The predecessor took it as given. So the choice
   needs stating against Akerlof, and the derivation is what A1 adds to it.
2. **Where the choice changes A1's conclusions.** Two results of A1 belong to
   the quadratic form, and one does not.
   - **The spread.** Under the quadratic loss the expected distance is the
     squared gap plus the spread for every observer, so a wider spread always
     hurts (`contrast_quadratic_spread`). Under Akerlof's benefit a wider spread
     helps an observer far from the group, because some members come closer
     (`contrast_spread_helps_distant`), and hurts an observer at its mean
     (`contrast_spread_hurts_at_mean`). The spread term of M18, the risk
     premium of Aigner and Cain in a contact setting, and implication I1 rest
     on the quadratic form.
   - **The range of the own-class direction.** M18 says refusal does not rise
     in the standing among observers at or above the standing. That range
     belongs to the quadratic form. Under Akerlof's benefit an observer above
     the standing but below some members can lose when the group rises
     (`control_benefit_range_differs`).
   - **What survives.** For any benefit that does not rise in distance, a
     rising group helps observers above every member and does not help
     observers below every member (`contrast_shift_up_above`,
     `contrast_shift_up_below`). So the claim that the direction of a group's
     effect depends on the observer's own class survives the change of form,
     on narrower ranges.
3. **C22.** The value of contact depends on the chooser's own position in
   Akerlof (AK-Q16), but positions are known and nothing is read from a
   marker (AK-Q2). C22 claims novelty only within models of inference from a
   group marker, and Akerlof does not touch that. Loury (1998, 2002) is still
   pending.
4. **The marker.** Akerlof makes dialect a chosen variable that tracks the
   clustering of contact (AK-Q18, AK-Q19). A1 takes the marker as given (A4).
   Akerlof's account is a reason the marker carries group information, and
   also a reason it may not be exogenous.
5. **Passing.** Leaving one's origin costs lost contacts (AK-Q20, AK-Q21),
   the cost of passing in A12, which A1 does not derive.
6. **Policy.** Interventions that move a whole network at once, and judging
   programmes by their effect on group interaction (AK-Q22, AK-Q23, AK-Q25),
   bear on the implications table. They are the case against a lever applied
   one person at a time.
7. **Solution concept.** Each person chooses a position given static
   expectations of others' positions, and the paper discusses rational
   expectations (AK-Q15, AK-Q17). Nothing is hidden. In the families of
   `notes/solution_concepts.md` this is family 4, contact with nothing hidden,
   with a value of contact that depends on own position.

## Controls

- `control_conform_outside_zone`. A point outside the zone of eq. (4) is not
  a best reply to itself.
- `control_benefit_range_differs`. The range of M18 fails under Akerlof's
  benefit, while the quadratic loss keeps it.
