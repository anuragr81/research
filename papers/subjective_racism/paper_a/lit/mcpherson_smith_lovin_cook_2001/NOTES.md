# McPherson, Smith-Lovin and Cook (2001), notes

## What was verified

- Twenty-one quotations are found on the journal pages they cite in the
  publisher's text layer, by exact normalised containment (LC-2).
- Every structural claim the review makes is formalised in Lean or listed as
  not formalised, in the coverage table of `CLAIMS.md` (LC-4). **The
  formalisation is of our reading of the structure.** The review states no
  model and proves none of these results.
- Our reading is consistent with the source on these points.

## What could not be verified

- Every empirical finding in the review is another study's. None of those
  studies is read, so Marsden (1987, 1988), Blau et al. (1982), Laumann (1973),
  Kalmijn (1998), Feld (1982), Hallinan and Smith (1985) and Popielarz and
  McPherson (1995) are [C] through this record.

## The bearing on Paper A1

Nothing in the review contradicts the corrected account outright. Two findings
it reports limit it. Race homophily is reported as same-different rather than
graded (MSC-Q9), and educational homophily is read as cultural rather than
economic (MSC-Q11). Both are [C] for the studies behind them.

### The puzzle

The review reports two facts that bear on why friending across class falls in
racially diverse settings, and neither is A1's mechanism.
- Where African Americans are a larger share of the population, the class
  differences between Black and Anglo residents are larger, and once those
  differences are controlled, group sizes again predict intermarriage
  (MSC-Q7, [C] for Blau et al. 1982). So racial diversity and the class gap
  between races move together. In A1 a larger gap `(c̄_g − c_i)²` raises the
  cost of the group, so the marker carries more class information where the
  population is more diverse. This link is inferred here and not checked.
- In a classroom, a smaller racial category makes cross-race friendships grow
  closer because same-race alternatives are few (MSC-Q16, [C]). Diversity
  supplies those alternatives, and at a given in-group share a larger category
  shows less inbreeding (`inbreeding_antitone_size`). The review explains
  this by opportunity, not by inference.

### Budget compatibility

The review does not assume budget compatibility, and it names no cost of
shared consumption. It gives four origins of homophily, in this order of
weight. Structure comes first, through foci, geography, family and roles
(MSC-Q14, MSC-Q19, MSC-Q21). Choice comes after it, through "ease of
communication, shared cultural tastes" and coordination (MSC-Q12). For
education it reads the evidence as cultural rather than economic (MSC-Q11). The
only cost the review names is the effort of reaching people who are far away
(MSC-Q18). MSC-Q11 is the reading A1 must answer, since it attributes class
homophily in education to culture and not to budgets.

### Avoidance that depends on the observer's class

- **Both directions between groups.** Racial inbreeding runs both ways, and
  African Americans show more of it than Anglos (MSC-Q20, [C]). That supports
  direction (i). The review does not split it by the observer's class.
- **Dependence on the observer's own position.** For education the effect is
  graded (MSC-Q10), and inbreeding is largest at the extremes (MSC-Q17). In
  A1's form the cost of a group rises in the observer's squared distance from
  its mean, which is largest at the extremes. The review is consistent with
  that dependence for education and silent on it for race.
- **A low-class member of a high group.** A member unlike the other members of
  a group is more likely to leave it (MSC-Q13, [C]). Read as mean squared
  distance, that is the gap to the group mean plus the group's variance
  (`niche_decomposition`), the same decomposition as A1's cost. So a member far
  below the group's mean has the strongest pull away from it, which supports
  direction (ii) for voluntary groups. The decomposition needs the spread as
  well as the mean (`control_mean_only_misses_spread`).
- **The limit.** For race the review reports same-different, not graded
  (MSC-Q9). With more than two groups whose means are ordered, A1 grades
  avoidance by the gap between means, and Marsden's result, [C] here, is
  evidence against that grading.

### The penalty vanishing when class is shown

The review has no such statement. Its nearest is the consolidation result,
that controlling class differences restores the size-based prediction for
intermarriage (MSC-Q7, [C]), with the warning that inbreeding is not "choice
or agency purified of structural factors" (MSC-Q5). In Lean, ties formed on
class that is seen, with race correlated with class, give race an in-group
share above its baseline (`consolidation_pos`, `foci_inbreeding_nonneg`) and
none when the racial mix is the same in every class (`control_no_consolidation`,
`control_foci_same_mix`). So the property that the group penalty vanishes when
class is shown is shared by a world without inference. What is A1's own is a
penalty between two people of given classes that exists only while class is
unseen. That is inferred here, not checked against A1's Lean.

### The shadow market

The review has no law, no banned criterion and no channel that discrimination
moves into. It does have rules on variables correlated with background that
sort people into foci. Tracking groups children "of similar backgrounds,
abilities, and achievement levels" into the same classes (MSC-Q19), and foci
produce ties about twice as homogeneous as chance (MSC-Q14). These are
group-neutral rules that produce homophily, which is the material A1's shadow
market works with. The review treats demographic similarity as a sign of
shared knowledge (MSC-Q12), so a visible trait carries social information. It
does not treat venues or platforms as intermediaries of that information.

### Premises of A1 the source does not share

- Structure outweighs choice in the literature the review reads (MSC-Q21). A1
  is an equilibrium of choices.
- Race homophily is same-different (MSC-Q9). A1's cost is graded in class.
- Class homophily is cultural (MSC-Q11). A1 grounds it in budgets.
- The review treats homophily as the outcome of opportunity and selection on
  traits that are seen. A1 has class unseen and inferred.

What A1 should cite the source for. The definition of homophily (MSC-Q1), the
ordering with race first (MSC-Q2), baseline against inbreeding homophily
(MSC-Q3), consolidation (MSC-Q5, MSC-Q7), and the graded pattern for education
against same-different for race (MSC-Q9, MSC-Q10).

## Controls

- LC-2. A fabricated quotation, "Marsden (1988) found a strong social distance
  effect for race, after taking baseline and inbreeding into account.", the
  opposite of MSC-Q9, is not found. A true quotation is not found on a
  neighbouring page.
- Lean. `control_no_consolidation` and `control_foci_same_mix` show the
  inbreeding of `consolidation_pos` and `foci_inbreeding_nonneg` needs race and
  class to be correlated. `control_equal_dissolution` shows the result of
  `dissolution_raises_homophily` needs unequal rates,
  `control_multiplex_neutral` that cumulation needs a relation above neutral,
  and `control_mean_only_misses_spread` that the niche decomposition needs the
  spread.
  `control_same_different_flat` shows a same-different rule has no distance
  effect, and `control_distance_needs_kappa` that the distance effect needs
  `κ > 0`.
