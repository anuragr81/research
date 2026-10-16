# Chetty et al. (2022a), Social Capital I, notes

## What was verified

- Eighteen quotations are found on the PDF pages they cite, by exact normalised
  containment, in a reading-order extraction of the pinned PDF (LC-2, CA-D1).
- The measures of eqs. (1) to (4) and (6), the range of spectral homophily up
  to `|λ| ≤ 1`, the balance of cross-SES links under mutual friendship, the
  no-homophily benchmark under our reading of it, two rows of Extended Data
  Table 1, and every number in the text that follows from other printed
  numbers are Lean theorems (LC-4). The coverage table in CLAIMS.md lists each
  result with its status. Our reading is consistent with the source on these
  points.
- Three printed figures rest on rounding or on averaging. "30.2% fewer" uses a
  rounded benchmark (CA-D2), Indianapolis's EC of 0.65 uses an unrounded share
  (CA-D9), and the shares 38.8% and 70.6% are means over people, which do not
  satisfy the pooled balance of links (CA-D8).

## What could not be verified

- The correlations, regressions and causal estimates, which rest on data we do
  not hold. They are recorded as printed.
- The SES model and the Supplementary Information.

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1. The source
measures who is friends with whom by predicted SES and says nothing about how
people come to know each other's SES, which is the step A1 models.

### The puzzle

This paper defines the measure in which the puzzle is posed and does not state
the puzzle itself, which is in the companion record `chetty_et_al_2022b/`. It
supplies two facts the theory must respect. Low-SES people have 30.2% fewer
high-SES friends than a no-homophily benchmark that allows for high-SES people
having 25.4% more friends (CA-Q4, `fewer_p3`, `control_equal_friends`).
Friendship falls with SES distance (CA-Q5). The bottom decile's friend shares
fall at every step away from it (`row1_strictAnti`), and the fifth decile's
peak at the sixth and fall toward both ends (`row5_interior_peak`). A
middle-SES person has fewer top-decile friends than middle-decile ones. That is
the shape `v − κ(c_i − c_j)²` gives. The table is a pattern of realised
friendships, which also reflect where people meet, and a common vertical
ranking with mutual consent also sends the middle to the middle, so the table
is consistent with A1's utility without establishing it (CA-D7).

### Budget compatibility

The source assumes no origin for homophily and offers none. It records
homophily as an outcome (CA-Q2, CA-Q3) and cites Blau for the need for
opportunities of contact (CA-Q6). It says nothing on cost, affordability or
shared consumption. One point of method bears on the premise. The authors read
SES from correlates that are themselves consumption, phone model price and
mobile carrier (CA-Q10). Class is visible in what people can afford, which is
the observable side of budget compatibility. That reading is ours.

### Avoidance that depends on the observer's class

1. **Avoidance runs both ways.** Friendship needs both parties (CA-Q16), and
   the authors impose the same on Instagram (CA-Q17). Under mutual friendship,
   cross-SES links counted from the low-SES side equal those counted from the
   high-SES side (`cross_links_balance`, `balance_shares`). Low-SES people
   having few high-SES friends and high-SES people having few low-SES friends
   are one fact in pooled counts, and the data cannot say which side declines.
   A model in which avoidance runs both ways is therefore observationally
   equivalent, on these counts, to one in which only one side avoids. The
   balance fails for a directed relation (`control_balance_needs_symmetry`). By
   race the source has nothing, since it has no individual race data (CA-Q18).
2. **A low-class member of the high group may prefer outsiders.** Not
   observable in the source. The middle-decile peak of `row5_interior_peak` is
   the class-only version of an observer whose preferred partners depend on
   his own class, and it neither supports nor limits the racial claim.

### The penalty vanishing when class is shown

No statement in the source. Its correlations within areas of one race (CA-Q8)
hold race fixed and do not vary what is known about class.

### The shadow market

Nothing in the source. It does not discuss law, screening, venues or
intermediaries of social information. Its release of statistics by ZIP code
(p. 1) is information about places, intended for research and policy.

### Premises of A1 the source does not share

- **Class is unseen.** SES is predicted by the authors from markers (CA-Q9,
  CA-Q10), and the source does not ask what friends can see.
- **Race as a marker of class.** Race enters only through the composition of
  areas (CA-Q18). The source links larger Black populations with lower EC and
  lower mobility (CA-Q7, CA-Q12) without a mechanism.
- **A measure per relation.** EC averages people, not friendships (CA-Q13), so
  a prediction of A1 stated as EC must be stated per person
  (`control_EC_mean_of_ratios`, CA-D8).

## Controls

- LC-2. A fabricated quotation that reverses CA-Q5, "Connectedness is higher
  between deciles that are further apart.", is not found on any page. A true
  quotation is not found on a neighbouring page.
- Lean. `control_equal_friends` shows that the 55.6% benchmark and the 30.2%
  shortfall rest on the 25.4% difference in friend counts.
  `control_EC_mean_of_ratios` shows that eq. (3) differs from the pooled share
  once friend counts differ. `control_balance_needs_symmetry` shows that the
  balance of links needs mutual friendship. `control_eig_needs_nonneg` shows
  that the eigenvalue bound needs nonnegative entries.
- The formalisation is of the paper's measures, its printed numbers and the
  structure of mutual friendship, and of our reading of the benchmark. It
  models no behaviour.
