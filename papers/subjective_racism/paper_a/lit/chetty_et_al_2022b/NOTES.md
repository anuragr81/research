# Chetty et al. (2022b), Social Capital II, notes

## What was verified

- Twenty-one quotations are found on the PDF pages they cite, by exact
  normalised containment, in a reading-order extraction of the pinned PDF
  (LC-2, CB-D1).
- The measures of eqs. (1), (3) and (4), the decomposition (2), eqs. (6) and
  (7), the benchmark (5) under our reading of it, the specifications of Table 1,
  the cross-cohort and regression-discontinuity designs with their identifying
  conditions, the attenuation correction, and every number in the text that
  follows from other printed numbers are Lean theorems (LC-4). The coverage
  table in CLAIMS.md lists each result with its status. Our reading is
  consistent with the source on these points.
- Three printed statements do not hold as printed and are recorded with the
  reading adopted. The TCEC formula contradicts the text unless its first
  factor is the low-SES share (CB-D5). The 73% and 16% across ZIP codes are the
  means of the two orders of the decomposition, which the paper does not say
  (CB-D4). "About 20% more likely" is a difference in bias, while the ratio of
  befriending rates is 1.24 (CB-D17).

## What could not be verified

- The regressions, the reliabilities and the quasi-experimental estimates. They
  rest on data we do not hold and are recorded as printed.
- The size of the racial-diversity gradient. The paper reports no coefficient,
  and the readings in CB-D7 are from plots.
- The Supplementary Information of the Nature article. The NBER version's
  supplementary material is read instead, and its differences are CB-D8 to
  CB-D13.

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1. One finding
limits how A1 may state its marker. In the least racially diverse ventile of
high schools, friending bias by parental SES is about −1% (Extended Data
Fig. 1e, CB-D7), against a benchmark of −0.10 for high schools
(`benchmark_values`). Low-SES students under-befriend high-SES students by about
nine points even where nearly everyone is of one race. A1's marker must
therefore include more than race (speech, dress, neighbourhood, course track),
or A1 must leave part of the level to channels of exposure inside the group.
This is inferred from a figure reading and the printed benchmark.

### The puzzle

The source states three facts that a theory of friending bias must reproduce.

1. **The level.** Low-SES people befriend high-SES people at lower rates than
   their group's composition implies (CB-Q1, CB-Q2), and this accounts for
   between 34% and 46% of the gap in EC by SES, depending on the order of the
   decomposition (CB-Q11, `fig3a_shares`). The measure is set against a
   benchmark that is negative because high-SES people have more friends
   (CB-Q18, `eq5`). Against setting benchmarks computed from Extended Data
   Table 4, low-SES bias lies above the benchmark in all six settings, by about
   0.10 in religious groups and 0.13 to 0.17 in schools, colleges, workplaces
   and recreational groups, and by 0.30 in neighbourhoods (`excess_low`,
   CB-D18). High-SES people over-befriend high-SES people (CB-Q16), below the
   benchmark in five settings (`high_below_bench`) and at it in recreational
   groups (`control_rec_high_at_bench`).
2. **The gradient in racial diversity.** Bias is higher in more racially
   diverse high schools (CB-Q6), colleges and neighbourhoods (CB-Q8). In the
   plots it rises by about three points across high schools and by about
   nineteen across neighbourhoods, and it is an inverted U in the share of white
   students (CB-D7). The cross-school standard deviation of bias in high
   schools is 0.04 (CB-D8, NBER only). The authors explain it in one sentence,
   with students of different races less likely to be friends (CB-Q7). Modelled
   as friendship only within race, that sentence yields mean bias
   `πA πB (hA − hB)²/(w(1 − w))`, proportional to the two-race diversity index
   at a given SES gap between races (`raceOnlyBias_formula`), and zero when the
   races do not differ in SES (`control_raceOnly_no_gap`). So any account in
   which race organises friendship reproduces the gradient, provided race
   carries information about class. The gradient alone cannot tell why race
   organises friendship. A1 supplies a reason, inference of class from the
   marker.
3. **The settings and the costs.** Bias is lowest in religious groups and
   highest in neighbourhoods (CB-Q17, `religious_vs_neighbourhood`), and the
   same people show more bias outside their religious group, which the authors
   read as the setting mattering (CB-Q4). Bias is higher with academic
   tracking, in larger schools, and near an even SES mix (p. 9), and lower at an
   exam school (CB-Q20). The cost examples are fraternity dues (CB-Q10),
   lunchrooms split by "options for low-cost and free lunch" (CB-Q12) and, in
   the NBER version only, prom tickets (CB-D11). The authors say the
   determinants are largely unknown (CB-Q9).

### Budget compatibility

The source assumes no origin for homophily. Friending bias is defined as
biased sampling from the pool of peers and not as a preference (CB-Q3). The
explanations it offers are structural, namely tracking, group size,
architecture and segregation within ZIP codes (pp. 9, 12, CB-D10), set against
preference rather than derived from it (CB-Q4). Its three cost examples each
price a shared activity, dues for a fraternity, the price of lunch for a
lunchroom, the price of a ticket for a prom (CB-Q10, CB-Q12, CB-D11). Each is a
case of two budgets that cannot sustain the same activity, which is A1's
budget compatibility, offered by the authors as a suggestion from student
newspapers and an architect's account, not as evidence. That religious groups,
whose activities are not priced by attendance, show the least bias (CB-Q17)
fits budget compatibility, but the source does not give that reason and the
link is ours.

### Avoidance that depends on the observer's class

1. **Avoidance runs both ways.** By class, the source shows both sides. Low-SES
   people under-befriend high-SES people and high-SES people over-befriend
   high-SES people (CB-Q16, `excess_low`, `high_below_bench`). Because
   friendship is mutual, the two are one fact in pooled counts
   (`ChettyEtAl2022a.cross_links_balance` in the companion record), and the
   measure cannot say which side declines. By race, the source has nothing,
   because it uses no individual race (CB-D16).
2. **A low-class member of the high group may prefer outsiders.** Not
   observable in the source, for the same reason. Nothing supports or
   contradicts it.

### The penalty vanishing when class is shown

The source makes no such statement and has no condition under which class is
shown. The nearest structure is our model of the authors' sentence, in which
race-only friending produces no SES bias when race carries no information about
class (`control_raceOnly_no_gap`). That is our reading, not the source's claim.

### The shadow market

The source does not mention law, anti-discrimination rules or screening on
group. It does record rules that are neutral on group but act through a
correlate. Assignment to learning communities at Berkeley High separated
"white students and students of color" (CB-Q19). Lunch prices sorted students
into lunchrooms (CB-Q12). Dues price entry to Greek life (CB-Q10). An entrance
exam screens at Payton (CB-Q20). It proposes venues as intermediaries of
contact (CB-Q13), and its one example is a paid gym where market roles create
contact across class (CB-Q21). The public release of bias by school and ZIP
code (p. 1) is information about where contact happens, intended for policy
makers rather than clients. These are the instruments the predecessor's
contact channel names. The source does not treat them as discrimination and
does not ask whether they substitute for a banned criterion.

### Premises of A1 the source does not share

- **Class is unseen.** The source measures bias on the authors' predicted SES
  (CB-Q14) and does not ask what friends can see.
- **Bias is an equilibrium of inference.** The source treats bias as a
  statistical departure that institutions may produce (CB-Q3, CB-Q4).
- **The direction from class to race.** The source's sentence runs from race to
  SES (CB-Q7).
- **Exposure is chosen.** The source takes group composition as given and
  attributes it to segregation across groups (p. 3).

## Controls

- LC-2. A fabricated quotation that reverses CB-Q6, "Friending bias is lower in
  more racially diverse schools", is not found on any page. A true quotation is
  not found on a neighbouring page.
- Lean. Each main result has a control. `control_bias_zero_iff_equal_friends`
  shows that zero bias under random friending needs equal friend counts.
  `control_order_matters` shows that the share given to exposure depends on the
  order whenever both factors differ. `control_tcec_printed` shows that the
  printed TCEC, read literally, is not maximised at a 50% share.
  `control_fig3b_single_order` shows that neither order alone gives 73%.
  `control_spec_unequal`, `control_cohort_varying_bias` and
  `control_rd_other_jump` show what each estimating design needs.
  `control_rec_high_at_bench`, `control_diversity_needs_shares` and
  `control_raceOnly_no_gap` bound the remaining results.
- The formalisation is of the paper's definitions, specifications and stated
  identities, and of our reading of eq. (5), TCEC, the setting benchmarks and
  the race mechanism. It models no behaviour beyond that reading.
