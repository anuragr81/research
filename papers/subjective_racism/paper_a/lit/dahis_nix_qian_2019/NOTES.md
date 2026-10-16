# Dahis, Nix and Qian (2019), notes

## What was verified

- Eighteen quotations are found on the PDF pages they cite in the text layer,
  by exact normalised containment (LC-2).
- The 2SUP linking rule with its three conditions for a false pass and the
  example of Figure A.4, the population lower bound, the linear specifications
  of Tables 7 and 8, the post-stratification and probit weights of Appendix F,
  and every number in the text that follows from other printed numbers are
  Lean theorems (LC-4). The coverage table in `CLAIMS.md` lists each result.
  Our reading is consistent with the source on these points, except where a
  reading is recorded (DNQ-D3, DNQ-D13, DNQ-D14).
- The step from selection into passing to the composition of those who remain
  is a Lean theorem, and it is our reading, not a statement of the paper.

## What could not be verified

- The estimates of Tables 1 to 8 and of the appendix tables. The linked
  microdata are not available here, so the numbers are taken as printed.
- Whether those who pass have a higher mean class than those who remain. Table
  7 gives conditional correlations with an income proxy, not the two means.
- Fourteen readings are recorded as DNQ-D1 to DNQ-D14. No row relies on a
  passage that a typo affects.

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1. The account
below follows the six headings of the second reading.

### The puzzle

The source does not study friendship or contact across class. It bears on the
puzzle in one way. In the source, the group a person is recorded in is read
from the company the person keeps. Enumerators inferred race from appearance,
behaviour and association (DNQ-Q18), and in the legal cases the authors cite,
a person who associated with whites was taken to be white (DNQ-Q5). Our
reading is that contact across the group line then moves the group others
assign to a person. Whether that raises the cost of contact across class in a
diverse setting is not addressed by the source and is left open.

### Budget compatibility

The source gives a legal origin, not a budgetary one, for matching within
race. Intermarriage was illegal in many states, almost every black man who
becomes married to a white woman is recorded as having passed, and almost none
married to a black woman is (Tables 4 and 5, DNQ-Q17). It says nothing about
cost, affordability or shared consumption as a reason for homophily, and so
neither supports nor contradicts budget compatibility.

### Avoidance that depends on the observer's class

Direction (i), avoidance both ways. The source records movement across the line
in both directions only as reclassification. 0.7% of white men are recorded as
black ten years later. By the counts and rates of Table 4, columns 3 and 4,
about half of them are men who become married to a black woman
(`table4_white_to_black`), and the
authors note that some are earlier passers returning (PDF pp. 24 to 25). Their
class is not reported, so the source does not speak to direction (i).

Direction (ii), a low-class member of the high group preferring outsiders. Not
in the source. The white men recorded as black after marrying black women are
the only case, and nothing is reported about their class.

The mirror of (ii), a high-class member of the low group preferring the high
group's pool. Passing is this choice, and the source finds it selected on an
income proxy. Passing rises with the man's own occupational income score given
his county (DNQ-Q9), the authors read their correlates as middle-class men
passing more (DNQ-Q3), and the cost falls on ties to the group (DNQ-Q10). The
source's motive is escape from formal discrimination and the income gain that
follows (DNQ-Q1, DNQ-Q11), not contact with partners of similar class. It
supports the direction of selection that A1's mirror predicts and gives a
different reason for it.

### The penalty vanishing when class is shown

The source has no such statement. Its penalty is formal. Under the one-drop
rule a man of known black ancestry was black whatever his appearance or
standing (DNQ-Q16), and Table 8 finds a gain from passing with the base-year
occupational score held fixed (DNQ-Q11). A penalty set by law on ancestry would
not vanish when class is shown. This limits A1's property to penalties that
arise from inference, the statistical-discrimination case, and the source is
not one.

### The shadow market

The source's setting is the reverse of the predecessor's. The law mandated
discrimination on ancestry, which no one could see, so enforcement ran on
appearance, conduct, company and local knowledge (DNQ-Q18, DNQ-Q5, DNQ-Q16).
Passing went around the formal rule by changing what observers read, moving to
where one was not known (DNQ-Q16) and associating with whites (DNQ-Q6).
Information about who is who had value to both sides, which is the shadow
market's object seen from the side that evades the rule. Passing is higher
where the formal rule against intermarriage held, by 31.9 percentage points
(DNQ-Q17, `miscegenation_points`). That is the sign of the predecessor's
legal-strictness item, with the law on the other side. Venues carried social
information about who belonged. In the cases the authors cite, attendance at
white balls and assemblies was evidence of whiteness (DNQ-Q5, footnote 25). The
signals in the source substitute for an unseen criterion, not a banned one.

### Premises of A1 the source does not share

- The marker carries class information within the group. The enumerator
  recorded a better dressed or educated man as mulatto more often (DNQ-Q12),
  and the perception of blackness correlates with socio-economic status
  (DNQ-Q4). A1's group sufficiency rules this out.
- The marker is inferred from contact (DNQ-Q5, DNQ-Q18), where A1 infers class
  from the marker.
- The penalty is formal and on ancestry, not an inference about class.
- What A1 can take from the source. The size of passing in the United States
  between 1880 and 1940, at least 1.4% of black men per census interval and
  16.6% of those linked (DNQ-Q7, DNQ-Q8), and its selection on an income proxy
  (DNQ-Q9). The composition step that passing lowers the mean of those who
  remain is ours (`left_behind_lower`), and A1 already credits it to Kim and
  Loury (2012).

## Controls

- LC-2. A fabricated quotation that says the opposite of DNQ-Q9, "a man with a
  higher occupational income score is less likely to pass for white", is not
  found on any page. A true quotation is not found on a neighbouring page.
- Lean. `control_backward_needed` and `control_two_candidates` show that the
  2SUP conditions do the work of removing false passers. `control_bound_strict`
  shows that 1.4% is a bound and not an estimate. `control_doubling_not_unit`
  shows the reading DNQ-D3. `control_selection_unobservables` shows that the
  weighted rates need selection on observables. `control_no_selection` shows
  that the composition step needs selection.
