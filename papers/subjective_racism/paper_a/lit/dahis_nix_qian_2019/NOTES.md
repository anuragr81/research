# Dahis, Nix and Qian (2019), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the text layer,
  by exact normalised containment (LC-2).
- The 2SUP linking rule with the example of Figure A.4, the population lower
  bound and the adjusted weighted rate of Appendix F are Lean theorems in the
  paper's terms (LC-4). Our reading is consistent with the source on these
  points.
- The step from selection into passing to the composition of those who remain
  is a Lean theorem, and it is our reading, not a statement of the paper.

## What could not be verified

- The regression estimates of Tables 7 and 8 and the rates of Tables 1 to 6.
  The linked microdata are not available here, so the numbers are taken as
  printed.
- Whether those who pass have a higher mean class than those who remain. Table
  7 gives conditional correlations with an income proxy, not the two means.
- Twelve readings are recorded as DNQ-D1 to DNQ-D12. No row relies on a passage
  that a typo affects.

## The bearing on Paper A1

Nothing in the source contradicts the A1 model. One premise differs. In the
source the recorded marker responds to class cues and to association. The
enumerator records a better dressed or educated man as mulatto more often
(DNQ-Q12), the perception of blackness correlates with socio-economic status
(DNQ-Q4), and in the legal cases the authors cite, a person who associated
with whites was taken to be white (DNQ-Q5). Under A1's group sufficiency the
marker carries no information on class within a group. The source's marker
does carry such information, because what observers record mixes ancestry,
appearance, class cues and the company a person keeps.

Who passes. Conditional on county characteristics, a higher own occupational
income score goes with more passing (DNQ-Q9), and the authors read their
correlates as middle-class men passing more (DNQ-Q3). Marriage, children and a
distinctively black name go with less passing (DNQ-Q10). Own literacy is not a
significant correlate (DNQ-D8). Passing requires association with whites
(DNQ-Q6) and goes with moving to whiter counties (Table 6, Figure 1).

The group left behind. The paper does not state the composition effect. Its
statements about those left behind concern spouses and children who cannot
pass (Tables 4 and 5). The composition step is ours (`left_behind_lower`). It
needs those who pass to have a mean above the group's, which Table 7 supports
only as a conditional correlation. A1 already treats "passing lowers the
visible mean" as known from Kim and Loury (2012). This source gives that claim
its historical magnitude, at least 1.4% of black men per census interval
(DNQ-Q8) and 16.6% of those linked (DNQ-Q7), and a selection on an income
proxy that points the same way.

The candidate contributions of A1 are taken in turn.

1. **Group avoidance from class homophily under inference.** Not in the
   source. Discrimination in the source is by whites against everyone recorded
   as black, the same for every white observer. Nothing depends on the
   observer's own class, and no group's spread enters.
2. **Budget compatibility as the source of class homophily.** Not in the
   source.
3. **Venue exclusion from client inference, and price screening.** Not in the
   source. The nearest material runs the other way. In the legal cases the
   authors cite, attendance at white balls and assemblies was evidence of
   whiteness (DNQ-Q5, footnote 25). There the venue certifies the marker, and
   the observer infers the marker from contact. In A1 the observer infers
   class from the marker. The source has no prices and no venue that screens
   on class.
4. **A test of the class channel.** Not in the source. Table 8, column 1,
   reports a higher occupational score for passers given the base-year score
   (DNQ-Q11). That is a return to the marker in the labour market, holding an
   income proxy fixed, and the authors present it as a correlation. It is not
   a test of whether a penalty in contact vanishes when class is shown.

What A1 can cite the source for. The size of passing in the United States
between 1880 and 1940 (DNQ-Q7, DNQ-Q8), the positive association of passing
with the occupational income score (DNQ-Q9), the authors' reading that passing
is a choice made in response to incentives (DNQ-Q1), and the evidence that the
recorded marker responds to class cues (DNQ-Q4, DNQ-Q12). The last is a
qualification A1 should state alongside group sufficiency.

## Controls

- LC-2. A fabricated quotation that says the opposite of DNQ-Q9, "a man with a
  higher occupational income score is less likely to pass for white", is not
  found on any page. A true quotation is not found on a neighbouring page.
- Lean. `control_backward_needed` shows that the backward condition is what
  removes the false passer of Figure A.4. `control_bound_strict` shows that
  1.4% is a bound and not an estimate. `control_no_selection` shows that the
  composition step needs selection.
