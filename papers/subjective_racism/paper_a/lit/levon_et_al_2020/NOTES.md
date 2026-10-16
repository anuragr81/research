# Levon, Sharma, Watt and Perry (2020), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the text layer
  (LC-2).
- The cached PDF and text match their pinned sha256 (LC-3).
- Seven Lean theorems, three of them controls, formalise our reading of the
  structure the report attributes to listeners (LC-4). They are not a model
  the report states. The report states no model, and the Lean is our
  reading of the inference it describes, a prior attached to an accent and an
  answer read as a signal of competence.
- Our reading is consistent with the report on these points.

## What could not be verified

- No estimate or test, since the report gives none (AB-D4).
- The sample sizes and the age threshold differ from those the Sutton Trust
  report gives for the same studies (AB-D2).
- Whether all five interventions help or only one (AB-D7).

## The bearing on Paper A1

Nothing in the report contradicts A1 as described. The report measures
ratings of competence and likeability in a hiring task, never contact.

1. **Group avoidance derived from class homophily.** The report has a penalty
   on working-class accents that is larger among listeners of higher class
   (AB-Q8) and among older and Southern listeners. That is a penalty that
   depends on the observer's own class, which A1's squared gap predicts for a
   high-class observer facing a low-class group. The report does not have
   the other half of A1's prediction, a penalty that low-class listeners place
   on RP. Lawyers rate RP marginally lower on likeability (AB-Q12), but the
   authors offer two readings and test neither. The report has no variance
   term and nothing on a group's spread. Contribution 1 is not in the report.
   AB-Q8 is evidence A1 may cite for the dependence of the penalty on the
   observer's class, with the caveat that the outcome is a competence rating
   and not contact.
2. **Budget compatibility.** Not in the report.
3. **Venue exclusion and price screening.** Not in the report.
4. **A test of the class channel.** The report does not have A1's test.
   What it shows is competence, not class. Holding the answer fixed and
   varying the accent produces a penalty (AB-Q5). Expert content raises every
   rating (AB-Q7) and does not remove the penalty for older Southern listeners
   (AB-Q8). Professional raters rate by the answer alone (AB-Q10), although
   EE and MLE still get the lowest single ratings (AB-Q11). Under our reading
   (`revealing_no_penalty`) an answer that reveals the trait being rated
   removes the penalty. The report's competence penalty therefore runs
   through believed competence, a channel separate from A1's class channel. A1's test
   needs class itself shown in a contact setting. The report is the nearest
   empirical design to it, with accent as the marker and the answer as
   information about the individual, and A1 can cite it as the design to
   adapt. A1 should not cite it as evidence that the group penalty vanishes
   when class is shown.

What A1 can cite. Accent as a marker read as class (AB-Q1, AB-Q2, AB-Q6),
with the caveat that it also carries region, age and ethnicity (AB-Q1,
AB-Q4). The stability of the accent hierarchy over fifty years (PDF p. 17).
The observer's class shaping the penalty (AB-Q8).

## Controls

- LC-2. A fabricated quotation, "lawyers and law firm recruiters consistently
  rated responses in Received Pronunciation higher, regardless of quality",
  the opposite of AB-Q10, is not found. A true quotation is not found on a
  neighbouring page.
- Lean. `control_partial_signal` shows that without full revelation the
  penalty survives. `control_signal_can_widen_gap` shows that the smaller
  gap with real voices (AB-Q15) is an empirical finding and not a consequence
  of the reading. `control_additive_accent_effect` shows that an accent effect
  that does not run through inferred competence survives a good answer.
