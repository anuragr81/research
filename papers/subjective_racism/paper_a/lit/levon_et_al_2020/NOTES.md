# Levon, Sharma, Watt and Perry (2020), notes

## What was verified

- Eighteen quotations are found on the PDF pages they cite in the text layer
  (LC-2).
- The cached PDF and text match their pinned sha256 (LC-3).
- Nineteen Lean theorems, six of them controls, formalise the printed sample
  identities and our reading of every structural claim the report makes about
  listeners (LC-4). The report states no model. The formalisation is of our
  reading of the inferences it describes, not of a model the report states.
- On 16 October 2026 the second-pass file was compiled to a scratch `.olean`.
  The suite's own audit read the project's earlier build and audited 7 of 19
  names. The audit was then run by hand, `#print axioms` on all 19 cited names
  against the scratch `.olean`, and every one depends on no axiom beyond
  `propext`, `Classical.choice` and `Quot.sound`.
- Our reading is consistent with the report on these points.

## What could not be verified

- No estimate or test, since the report gives none (AB-D4).
- The sample sizes and the age threshold differ from those the Sutton Trust
  report gives for the same studies (AB-D2).
- Whether all five interventions help or only one (AB-D7).
- Whether social class is a protected characteristic, on which the two reports
  disagree (AB-D10).

## The bearing on Paper A1

Nothing in the report contradicts the corrected account of A1. The report
measures ratings of competence and likeability in a hiring task, never
contact, so its bearing is on A1's premises and on the shadow market.

**The puzzle.** The report says nothing about friending or about racially
diverse settings. It does show that a single marker carries class and
ethnicity together (AB-Q1, AB-Q4, AB-Q6), which is the condition under which
the puzzle arises in A1, since a marker read for class in a diverse setting is
also read for group.

**Budget compatibility.** The report has no account of why class matters in
contact, and nothing on cost, affordability or shared consumption. Its origin
of the penalty is a stereotype used as a cognitive shortcut (AB-Q2), read here
as a prior on competence (`identical_answer_penalty`). That is a vertical
motive, every listener preferring the more competent speaker, and not
homophily. A1 should not cite the report for the origin of class homophily.

**Avoidance that depends on the observer's class.**
- Direction (i), avoidance both ways. The report has one direction. The
  penalty on working-class accents is larger among listeners of higher class
  (AB-Q8), which fits A1's squared gap for a high-class observer facing a
  low-class group. The reverse direction appears only as the lawyers'
  marginally lower likeability of RP, which the authors read either as low
  solidarity or as a higher expectation for RP (AB-Q12). Only the second
  reading has a structure (`higher_expectation_lower_rating`), and it is not
  avoidance. The report neither supports nor rules out direction (i).
- Direction (ii), a low-class member of the high group preferring outsiders.
  Nothing in the report bears on it.

**The penalty vanishing when class is shown.** The report has no statement
about class shown. What it shows is competence. Identical answers are rated
lower in working-class accents (AB-Q5), expert content raises every accent
(AB-Q7, `expert_raises_all`) and does not remove the penalty for older
Southern listeners (AB-Q8). Professional raters rate by the answer alone
(AB-Q10). In our reading the competence penalty vanishes under either of two
conditions. One is that the answer reveals competence (`revealing_no_penalty`, failing
with `control_partial_signal`). The other is that the rater puts full weight
on content (`full_control_no_penalty`, failing with `control_partial_control`).
The first is the statistical-discrimination condition of Phelps and of Aigner
and Cain, where the group weight falls as the individual signal becomes
precise. The second is a condition on the rater, not on the information.
A1's property is the first kind, stated for class rather than competence. The
report is a measured case of the first kind for competence, and A1 can cite
it as such.

**The shadow market.** The report bears on it in three ways.
- A marker the law does not reach. Accent is not a protected characteristic,
  so employers "can legally discriminate" on it, and accent "becomes a proxy
  for other forms of discrimination that are prohibited" (AB-Q16, AB-Q4). This
  is a signal substituting for a banned criterion, the mechanism behind the
  predecessor's legal constraint on the admission policy. AB-D10 records that
  the report also lists social class as protected, which the Sutton Trust
  report contradicts.
- Formal screening cleaned, informal contact untouched. Professional raters
  set accent aside when rating content (AB-Q10, AB-Q13). The report says it
  does not cover informal interaction during the interview, post-hiring
  experience or progression (AB-Q17). That is the contracting channel with the
  contact channel left unmeasured, the split the predecessor takes from Loury.
- Competence rating against likeability. Among lawyers, MCPR moved likeability
  but not competence ratings (PDF pp. 22, 23). The rating closest to contact
  was the one still moved by the listener's own disposition.

**Premises of A1 the report does not share.**
- The report holds that accent carries no information about competence
  (AB-Q3) and treats its use as a stereotype. A1 holds that a marker carries
  information about class through the group's class distribution, so its use
  is inference, not error.
- The report's marker carries region, age and networks as well as class and
  ethnicity (AB-Q1). A1's marker is a group label read through one class
  distribution.
- The report reads the penalty's fall with expertise and awareness as
  cognitive control (`rating_penalty`), a change in the rater. In A1 the
  observer's inference is fixed by the marker and the group distribution.

## Controls

- LC-2. A fabricated quotation, "lawyers and law firm recruiters consistently
  rated responses in Received Pronunciation higher, regardless of quality",
  the opposite of AB-Q10, is not found. A true quotation is not found on a
  neighbouring page.
- Lean. `control_partial_signal` shows that without full revelation the
  penalty survives. `control_signal_can_widen_gap` shows that the smaller gap
  with real voices (AB-Q15) is an empirical finding and not a consequence of
  the reading. `control_additive_accent_effect` shows that an accent effect
  that does not run through inferred competence survives a good answer.
  `control_partial_control` shows the penalty survives any weight on content
  below 1. `control_flat_cohort` shows the life-stage inference needs cohorts
  that differ. `control_equal_expectation` shows the expectation reading needs
  a gap in expectations.
