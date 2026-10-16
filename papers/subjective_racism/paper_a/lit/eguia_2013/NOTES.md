# Eguia (2013), notes

## What was verified

- Twenty-one quotations are found on the pages they cite in the publisher's
  text layer, by exact normalised containment (LC-2).
- The model of Sections 1 and 2 is implemented in Lean in the paper's
  notation. Eqs. (1) to (9), the steps of the proof of Proposition 1, the
  screening argument of p. 27 and the arithmetic of Example 4 are theorems,
  general where the coverage table in `CLAIMS.md` says so and special cases
  where it says so (LC-4). Every main result has a control that drops one
  condition. Our reading is consistent with the source on these points,
  except for the value at `s = 1` recorded as EG-D1 and the implicit proviso
  recorded as EG-D7.

## What could not be verified

- The parts of Propositions 1 to 3 marked "not formalised" in the coverage
  table, chiefly the arguments by continuity in `λ`, the uniqueness of each
  incumbent's preferred cutoff, and the `𝒟` side of Proposition 3.
- The formal versions of the three generalizations, which are in an unread
  add-on file (EG-D6).
- The published 2017 article (EG-D5).

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1. Two of A1's
directions meet findings in the source that point the other way, and these are
recorded under the headings below as limits, not contradictions, because the
source's objects differ from A1's.

### The puzzle

The source does not study friending across class. The source reads the
evidence on acting white as stronger in integrated schools (EG-Q13), and
explains the pattern by the exit option that integration offers to the ablest
members of the disadvantaged group (EG-Q19). That is a second mechanism by
which racial diversity turns differences within a group into sorting across
groups. A1 explains the same kind of pattern by inference about class from the
marker, and the source explains it by selective exit with no inference about
persons (EG-Q10). The source leaves open whether its exit mechanism lowers
cross-class ties within the disadvantaged group.

### Budget compatibility

The source assumes that interaction pays when agents "follow the same norms
and take part in the same activities" (EG-Q17), which is close to A1's shared
activities. The source then models the payoff as vertical. A richer, more
skilled group gives every member more (EG-Q6, eq. 1), so a gap in endowment
is a gain to the poorer member rather than a cost to the relationship. The
source therefore has shared activity as the origin of the value of contact,
and has no cost of a gap in resources. Affordability enters only as the cost
of learning the advantaged group's norms, which falls with skill (EG-Q4,
`screen_exists`), not with wealth.

### Avoidance that depends on the observer's class

- **Direction (i), avoidance runs both ways.** The source has it only as a
  taste for the other group's marker, in words (EG-Q15, EG-Q20). When both
  groups dislike each other's attributes, fewer agents assimilate and the
  result is voluntary segregation (EG-Q20). The derived preferences of the
  model run one way, since only the advantaged group sets a barrier.
- **Direction (ii), a low-class member of the high group may prefer
  outsiders.** The source points the other way. The least skilled members of
  the advantaged group are the most exclusionary, because they lose most from
  the dilution of the group's endowment and gain least from its skill
  (EG-Q18, `prop1iii`, `maximiser_antitone`). The object differs from A1's.
  In the source the low-skilled incumbent ranks entrants by what they do to the
  group's mean, and in A1 the low-class member ranks partners by distance in
  class. The source supports the mirror image of (ii). The ablest members of
  the disadvantaged group turn toward the other group and are more popular
  there (EG-Q19, `cutoff_rule`).

### The penalty vanishing when class is shown

The source has no such statement. Nobody infers anyone's type, so there is no
group penalty on persons for revealed type to remove (EG-Q2, EG-Q10).
Screening works "without a need to observe" skill (EG-Q9), and the source
does not say what would change if skill were observed.

### The shadow market

The source has the substitution of a correlated criterion for the marker. It
argues that the barrier "must be based on malleable individual traits and
behaviors that correlate with ability, and not on immutable characteristics"
(EG-Q16), built as norms that the able acquire cheaply (EG-Q21), and a barrier
on an immutable trait cannot screen (`control_lump_sum_no_screen`, EG-Q14).
The motive differs from A1's shadow market. In the source the marker carries
no information about ability (EG-Q3), so the incumbents move to correlated
traits for efficiency. In A1 the marker is informative and the law bans
screening on it, so the correlated cues gain value from the ban. The source
has no law, no venues and no intermediaries of social information. Its one
formal channel is immigration policy, mentioned in passing on p. 15.

### Premises of A1 the source does not share

- Ability has the same distribution in both backgrounds (EG-Q3), whereas A1's
  group class distributions differ.
- Preferences over group composition are vertical (EG-Q6), whereas A1's are
  homophilous in class.
- The payoff depends on realised group means only (EG-Q11), so within-group
  spread has no counterpart of A1's σ² term.
- Nobody forms beliefs about persons (EG-Q10), whereas A1's engagement rests
  on the posterior given the marker.
- The game is sequential, with incumbents moving first and a pivotal member
  setting the barrier (`clamp_truthful`), whereas A1 is a simultaneous Nash
  equilibrium.
- The composition condition is the same in kind as A1's. Group means are the
  means of the members the cutoff admits (`eq3_sA`, `eq3_sD`), and the
  marginal entrant is indifferent at that composition (`dcut_indifferent`).
  Selective exit lowers the source group's mean (`sD_lt_half`), the effect A1
  records as known.

## Controls

- LC-2. A fabricated quotation, "Individual ability is publicly observed by
  every agent.", the opposite of EG-Q2, is not found. A true quotation is not
  found on a neighbouring page.
- Lean. Each main result has a control. `control_no_mobility` and
  `control_cutoff_zero` for the composition effects,
  `control_cutoff_needs_monotone_gain` for the cutoff rule,
  `control_dcut_no_gap` for the increase of `d(s)`, `control_foc_no_mobility`
  and `control_foc_large_endowment` for the first-order condition,
  `control_prop1ii_large_endowment`, `control_prop1iii_increasing_differences`,
  `control_clamp_not_single_peaked`, `control_prop2_large_gap`,
  `control_prop3_pressure_bites` and `control_lump_sum_no_screen`.
