# Kim and Loury (2012), notes

Pages here are PDF pages of the issue, as in `CLAIMS.md`. The journal page is
the PDF page minus 3.

## What was verified

- Eighteen quotations are found on the PDF pages they cite in the publisher's
  text layer, by exact normalised containment (LC-2).
- The model is implemented in `lean/mathlib/KimLoury2012.lean`. Eqs. (1) to
  (8), the concavity of `R` and the signs of `R′` at the ends are proved for a
  finite signal space. Eq. (15), Lemma 1 and the equivalence of `ΔU > 0` with
  `πA > πB` are proved in general. Propositions 1 and 2, Lemma 2, the trivial
  ASE and the two claims of Section IV that follow from Proposition 2 are
  proved for a finite cost distribution (LC-4). The coverage table in
  `CLAIMS.md` gives the status of every numbered result.
- Every cited theorem passes the axiom audit, using only `propext`,
  `Classical.choice` and `Quot.sound`.
- Our reading is consistent with the source on these points.

## What could not be verified

- The existence of a non-trivial ASE and the comparison of inequality between
  endogenous and exogenous groups are argued informally on p. 14 and not
  proved (KLI-Q12, KLI-Q13, KLI-D5). No row may cite them as results.
- The published version (*Journal of Public Economics*, 2019) is not read
  (KLI-D1).

## The bearing on Paper A1

Nothing in the article contradicts the corrected account of A1. One reading
qualifies how A1 states what is known about passing. The article's selection
runs toward the group with the greater return to skill, which is the
better-reputed group only where the return rises in the reputation (KLI-Q11,
KLI-D2, `control_reputation_without_return`).

### The puzzle

The article does not address contact, friendship or the diversity of a
setting, so it offers no account of friending bias. What it does supply is a
reason a group marker can carry more information about an unseen type than
the groups' fundamentals warrant. Where people choose their group, the able
sort into the group with the greater return (KLI-Q1, `prop2_strict`), and the
gap between the groups' entering members exceeds the gap between their
reputations (`cohort_gap`). The groups start identical (KLI-Q7), and the
correlation between group and type is produced by selection.

### Budget compatibility

The article has no matching or homophily between workers, so it assumes
nothing about the origin of assortative ties. Two points bear on cost. First,
the value of ties to one's own group enters only as the exogenous affect cost
`k`, "loss of ties to one's own kind" (KLI-Q16), drawn independently of
ability (KLI-Q7). Budget compatibility makes the cost of a tie rise with the
class gap, and the article's cost of leaving a group does not depend on type
at all. Second, spending appears as a marker rather than a constraint.
Conspicuous consumption and residence in affluent areas are listed as methods
of partial passing (KLI-Q15), and traits "expensive to obtain" screen by
differential gain (KLI-Q18). Neither point supports or contradicts budget
compatibility. The article simply has no shared consumption.

### Avoidance that depends on the observer's class

The evaluator in the article is an employer with no type, who pays the
posterior mean (KLI-Q9). Nothing depends on the observer's own standing, and
a group's spread carries no penalty. The dependence on type in the article
sits with the person who chooses a group. Every type gains from the group
whose wages are higher (`prop1_pointwise`, `dU_pos_iff`), and the type sets
how much it gains, never the sign (`dU_antitone`).

- **Direction (i), avoidance both ways.** Not in the article. Preference over
  groups runs one way, toward the higher reputation, for every type
  (`prop1_both`).
- **Direction (ii), a low-class member of the high group preferring
  outsiders.** The article's structure rules out the analogue. A worker with
  a high skill cost gains least from the favoured group but still gains
  (`prop1_pointwise`). Such a worker sits in the other group only because of
  his affect cost `k`. The article therefore neither supports nor
  contradicts (ii), because its payoff rises with the reputation for every
  type, while A1's engagement utility falls with the class gap.

### The penalty vanishing when class is shown

The article contains no such statement. The employer always sees an
individual signal as well as the group (KLI-Q8). At every signal that both
types send, the posterior depends on the group's reputation
(`control_group_matters_interior`), and the full support of the signal rules
out any other signal. As our extension, not a result of the article, the
wage of eq. (1) has the property at a signal that only a skilled worker
sends, where the posterior is 1 whatever the reputation (`rhoRaw_revealing`). The
article's own remark on markers runs the other way. A further group marker
improves screening (KLI-Q5). The link A1 can draw is to eq. (1) itself. The
group enters the wage only through the prior `π` in `ρ(π, t)`.

### The shadow market

The article has no law that bars screening on group. Employers use group
identity openly (KLI-Q8). Three of its elements bear on the shadow market.

- **A marker substituting for type.** Employers screen on a chosen cultural
  trait "not relevant for productivity" because it is correlated with skill
  (KLI-Q17, reported from Fang 2001), and the article's affect works the same
  way (`prop2_strict`).
- **Markers correlated with standing.** Speech, dress, conspicuous
  consumption and residence (KLI-Q15) overlap with the predecessor's list of
  shadow cues (dress codes, price, location).
- **Certificates that gain value by self-selection.** The ex-convict
  programme (KLI-Q14) and expensive traits (KLI-Q18).

What the article lacks is the shadow market's institutional side. Nothing
moves to a channel the law does not reach, no rule is group-neutral on a
correlated variable, and no venue or platform supplies the cues. In the
article the person who is screened supplies the marker. In the shadow market
a venue sets the cues and clients read them.

### Premises of A1 the source does not share

- The evaluator sees an individual signal (KLI-Q8). A1's group sufficiency
  rules this out for the marker.
- The type is a chosen skill, and ability is a low cost of acquiring it
  (KLI-Q6). A1's class is given.
- The evaluator values only the posterior mean (KLI-Q9). A1's engagement
  utility is quadratic in the class gap, so A1's evaluator also cares about
  the group's spread.
- The setting is a wage (KLI-Q3). A1's setting is contact.
- The groups are identical in fundamentals and differ only in equilibrium
  (KLI-Q7, `trivial_ase`). A1's groups have their own class distributions.
- One premise is shared. Each group's believed rate equals the actual rate of
  its chosen members (KLI-Q10, `IsASE`). A1's consistency condition has this
  form. A1 imposes it on the mean of an exogenous class, and the article on a
  rate of chosen skill.

## Controls

- LC-2. A fabricated quotation, "The disproportionately less talented
  workers, whose human capital investment costs (c) are relatively higher,
  choose the 'affect' that corresponds to the greater return", the opposite
  of KLI-Q11, is not found. A true quotation is not found on a neighbouring
  page.
- Lean, one control for each main result.
  - `control_no_selection_equal_return`. Equal returns give no selection
    even when the beliefs differ.
  - `control_strict_needs_mass_above`. Without a type above the threshold
    there is no strict selection.
  - `control_reputation_without_return`. A better reputation with a lower
    return draws the able away.
  - `control_prop1_needs_symmetry`. Proposition 1 fails without the symmetry
    of `H`.
  - `control_uninformative_flat`. The concavity results fail for an
    uninformative signal.
  - `control_group_matters_interior`. The extension `rhoRaw_revealing`
    fails at a signal both types send.
