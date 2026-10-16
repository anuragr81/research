# McPherson, Smith-Lovin and Cook (2001), notes

## What was verified

- Fifteen quotations are found on the journal pages they cite in the
  publisher's text layer, by exact normalised containment (LC-2).
- Our reading of three structures the review describes in words is formalised
  in Lean (LC-4). These are baseline against inbreeding homophily,
  consolidation, and a graded social distance effect against a same-different
  rule. **The formalisation is of our reading of the structure.** The review
  states no model and proves none of these results.
- Our reading is consistent with the source on these points.

## What could not be verified

- Every empirical finding in the review is another study's. None of those
  studies is read, so Marsden (1987, 1988), Blau et al. (1982), Laumann (1973),
  Kalmijn (1998), Feld (1982) and Popielarz and McPherson (1995) are [C]
  through this record. A positioning claim in A1 must not rest on them until
  they are read.

## The bearing on Paper A1

Two findings the review reports sit uneasily with A1, and they come first.

- **Race is same-different, education is graded.** The review reports that
  Marsden (1988) finds "no significant social distance effect for race" and
  reads the distinction as "same-different" (MSC-Q9), while for education ties
  grow less likely as the difference grows (MSC-Q10). With two groups A1 is
  same-different by construction. With more than two groups whose class means
  differ, A1's cost `(c̄_g − c_i)² + σ_g²` grades avoidance by the gap between
  group means. Marsden's result, [C] here, is evidence against that grading for
  race. A1 should either stay with two groups or say why race need not show a
  distance effect when the group means are ordered.
- **Culture rather than economics.** The review reads the evidence as showing
  that it is "the cultural aspects of educational and occupational homophily,
  rather than the economic ones, that drive the structure" (MSC-Q11). That is a rival
  reading to budget compatibility.

The candidate contributions of A1 are taken in turn.

1. **Group avoidance from class homophily under inference.** Not in the
   source. The review's account of race homophily already joins the class
   differences between racial groups to "the personal prejudices that often
   result from the latter" (MSC-Q6). Its nearest statement to inference is that demographic
   similarity may "indicate shared knowledge" and so draw similar people
   together (MSC-Q12). Neither has an observer whose avoidance of a group
   depends on the observer's own class, and neither penalises a group's
   spread. A1 should cite MSC-Q6 and MSC-Q12 as the sociological statement of
   the idea and say that A1 derives what the review asserts.
2. **Budget compatibility.** Not in the source. MSC-Q11 is the reading A1 must
   answer.
3. **Venue exclusion.** Not in the source in A1's form. Foci, the review's
   venues, produce ties about twice as homogeneous as chance (MSC-Q14), and a
   member unlike the others is more likely to leave a group (MSC-Q13). Both
   work on traits that are seen. No client infers anything from a marker, and
   no price screens.
4. **A test of the class channel.** Not stated for contact. The review reports
   a related test on intermarriage. Once the class differences between Black
   and Anglo populations are controlled, group sizes again predict the
   homogeneity of ties (MSC-Q7, [C] for Blau et al. 1982). The review also
   names the identification problem for values, whether homophily is on the
   trait itself or on "other social characteristics that are correlated"
   with it (MSC-Q15).

**What consolidation implies for A1's test.** Inbreeding homophily includes
homophily "induced by other dimensions with which the focal dimension is
correlated" (MSC-Q5). In the Lean file, ties formed on class alone, seen, with
race correlated with class, give race an in-group share above its baseline
(`consolidation_pos`) and none when the mixes are equal
(`control_no_consolidation`). That world also passes A1's test, since showing
class removes the race penalty there too. So the test as stated separates A1
from a taste for race, but not from consolidation. What separates A1 from
consolidation is a race penalty at the level of the pair, given both classes,
that is present when the observer cannot see class and absent when the observer
can. This is inferred here and not yet checked against A1's Lean.

What A1 should cite the source for. The definition of homophily (MSC-Q1), the
ordering of dimensions with race first (MSC-Q2), baseline against inbreeding
homophily (MSC-Q3), status against value homophily (MSC-Q4), race having the
highest inbreeding (MSC-Q8), and the contrast between same-different and
graded patterns (MSC-Q9, MSC-Q10).

## Controls

- LC-2. A fabricated quotation, "Marsden (1988) found a strong social distance
  effect for race, after taking baseline and inbreeding into account.", the
  opposite of MSC-Q9, is not found. A true quotation is not found on a
  neighbouring page.
- Lean. `control_no_consolidation` shows the inbreeding of
  `consolidation_pos` needs race and class to be correlated.
  `control_same_different_flat` shows a same-different rule has no distance
  effect, and `control_distance_needs_kappa` that the distance effect needs
  `κ > 0`.
