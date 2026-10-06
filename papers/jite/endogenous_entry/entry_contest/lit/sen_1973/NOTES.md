# Sen (1973), verification notes

## Source

JSTOR copy of the published article, *Economica* 40(159), pp. 241-259, read in
full on 6 Oct 2026 (Drive folder `1r7J3xJ2rotpmlRaBANCNhviy2d8a1bx_`). The
extracted text layer garbles some footnotes but every quoted sentence in
`CLAIMS.md` was read in a clean passage. Page numbers come from the running
headers.

## Lean (`Sen1973.lean`), theorem by theorem

| Theorem | Claim | What it proves |
|---|---|---|
| `weak_axiom_forces_transitive_pair` | SEN-2 | From strict choice of x over y in {x,y} and of y over z in {y,z}, the Weak Axiom and nonempty choice on {x,z} and {x,y,z} give x chosen and z rejected in {x,z} |
| `cyclic_choice_violates_weak_axiom` | SEN-3 | Adding strict choice of z over x in {x,z} contradicts the Weak Axiom |
| `cycle_on_pairs_satisfies_weak_axiom` | SEN-3c, control | A cyclic choice function on the three pair menus satisfies the Weak Axiom on that domain |
| `cycle_on_pairs_is_cyclic` | SEN-3c, control | The same function chooses x over y, y over z and z over x |
| `confess_strictly_dominant` | SEN-4 | Confessing gives a strictly higher payoff against either action |
| `mutual_nonconfession_better_for_each` | SEN-4 | -10 < -2 |
| `nonconfession_reveals_no_preferred_outcome` | SEN-5 | Neither outcome of non-confession beats the matching outcome of confession for the chooser |
| `other_regarding_makes_nonconfession_dominant` | SEN-6 | Under the objective of the other's sentence, non-confession is strictly dominant |
| `as_if_play_better_in_own_terms` | SEN-6 | That play leaves each better off in own terms, although confession remains dominant in own terms |
| `dilemma_from_orderings` | SEN-7 | Any payoffs ordered temptation > reward > punishment > sucker give dominance and the Pareto ranking |
| `dilemma_survives_asymmetric_sentences` | SEN-7 | One asymmetric instance |
| `control_dilemma_needs_reward_above_punishment` | SEN-7, control | With the reward below the punishment the Pareto ranking fails |

The Weak Axiom is stated in the form the §III argument uses, with revealed
strict preference meaning "choosing x and rejecting y" (p.245). The proof
needs no case split on whether z is chosen from the triple, because the Weak
Axiom applied to {y,z} already excludes it. A first draft split on that case
and so depended on `Classical.choice`; the suite's axiom parse caught it, and
the constructive proof replaced it.

## Non-vacuity

- SEN-2 and SEN-3 fail without the triple in the domain, which is what the
  pairs-only control shows.
- SEN-7 fails when the reward is not above the punishment, which is what
  `control_dilemma_needs_reward_above_punishment` shows.
- SymPy SEN-4c shows dominance fails when the mutual non-confession payoff is
  raised to 0.

## What is not formalised

- Sen's budget-set setting for SEN-3c (p.246). The control is an analogue with
  the same missing menu, not his construction.
- SEN-1 and SEN-8 to SEN-11 are claims about interpretation and are verified
  by quotation only.

## Findings

- None against our documents. The measurement map quotes pp.253, 254,
  257-258 and 259, and each quotation is verbatim.
- The form of the Weak Axiom matters. Sen's first statement on pp.241-242 reads
  "if he chooses x when y is available, then he will not choose y in a
  situation in which x is also obtainable", which taken literally forbids
  indifference. The §III argument works with "choosing x and rejecting y", and
  the Lean follows §III.
