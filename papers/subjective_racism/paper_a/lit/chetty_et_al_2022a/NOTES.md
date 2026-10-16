# Chetty et al. (2022a), Social Capital I, notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite, by exact normalised
  containment, in a reading-order extraction of the pinned PDF (LC-2, CA-D1).
- The measure of eqs. (1) to (3), the no-homophily benchmark under our reading
  of it, the printed shares and their arithmetic, and two rows of Extended
  Data Table 1 are Lean theorems (LC-4). Our reading is consistent with the
  source on these points.
- One printed figure, "30.2% fewer", follows from a rounded intermediate
  (CA-D2). Nothing else checked disagrees with the source.

## What could not be verified

- The correlations, regressions and causal estimates, which rest on data we do
  not hold. They are recorded as printed.
- The SES model and the Supplementary Information.

## The bearing on Paper A1

Nothing in this paper contradicts the A1 model. The paper measures who is
friends with whom by SES, and it says nothing about how people come to know
each other's SES, which is the step A1 models.

What A1 can take from it. Class homophily in friendship is strong (CA-Q2,
CA-Q3), and it is graded by distance (CA-Q5). In Extended Data Table 1 the
bottom decile's friend shares fall at every step away from it
(`row1_strictAnti`), and the fifth decile's shares peak at the sixth decile and
fall toward both ends (`row5_interior_peak`). A middle-SES person has fewer
top-decile friends than middle-decile ones. That is the shape A1's utility
`v − κ(c_i − c_j)²` gives, and it is not the shape a ranking shared by all
observers gives to the share of partners a person seeks. It is a pattern of
realised friendships, however, and realised friendships also reflect where
people meet and whether both sides agree. A stable matching under a common
vertical ranking also sends the middle to the middle. So the table is
consistent with contribution 1 and does not establish it (CA-D7).

The race results are at the level of areas. There are no individual race data
(CA-D4). Areas with larger Black populations have lower EC, and EC accounts for
their lower mobility (CA-Q7, CA-Q12). Within areas that are nearly all one race
EC still predicts that race's mobility (CA-Q8). None of this says whether
cross-class friendship is lower across racial lines because class is inferred
from race.

The candidate contributions are taken in turn.

1. **Group avoidance derived from class homophily under inference.** Not in
   the source. The source documents class homophily and its gradient in
   distance (CA-Q3, CA-Q5) and offers no model of it. A1 can cite the source
   for the premise that friendship falls with class distance.
2. **Budget compatibility.** Not in the source.
3. **Venue exclusion and price screening.** Not in the source. Exposure and
   friending bias are deferred to the companion paper (p. 3), whose record is
   `chetty_et_al_2022b/`.
4. **A test of the class channel.** Not in the source. Its restriction to
   areas of one race (CA-Q8) holds race fixed. It does not vary whether class
   is visible.

Two points of method A1 should note. The authors read class from visible
correlates such as phone price and ZIP code (CA-Q10), and they keep friends'
information out of the prediction so that errors are not shared across friends
(CA-Q9). That is the econometrician's inference of class from markers. A1's
observers do the same with a group marker. And EC averages people rather than
friendships (CA-Q13), so a community where low-SES people differ in friend
counts has an EC that differs from its pooled share
(`control_EC_mean_of_ratios`). An A1 prediction stated as EC must be stated per
person.

## Controls

- LC-2. A fabricated quotation that reverses CA-Q5, "Connectedness is higher
  between deciles that are further apart.", is not found on any page. A true
  quotation is not found on a neighbouring page.
- Lean. `control_equal_friends` shows that the 55.6% benchmark and the 30.2%
  shortfall rest on the 25.4% difference in friend counts, since equal counts
  give one half and 22.4%. `control_EC_mean_of_ratios` shows that eq. (3)
  differs from the pooled share once friend counts differ.
- The formalisation is of the paper's measure and printed numbers, and of our
  reading of the benchmark. It models no behaviour.
