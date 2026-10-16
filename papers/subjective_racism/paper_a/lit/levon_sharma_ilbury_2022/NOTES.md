# Levon, Sharma and Ilbury (2022), notes

## What was verified

- Twenty-six quotations are found on the PDF pages they cite in the text
  layer (LC-2).
- The cached PDF and text match their pinned sha256 (LC-3).
- Seventeen Lean theorems, four of them controls (LC-4). They check the
  identities behind the printed shares and the comparisons the text draws from
  them, and formalise our reading of three claims about modification. The
  report states no model. The formalisation of those claims is of our reading,
  not of a model the report states.
- On 16 October 2026 the second-pass file was compiled to a scratch `.olean`.
  The suite's own audit read the project's earlier build and audited 7 of 17
  names. The audit was then run by hand, `#print axioms` on all 17 cited names
  against the scratch `.olean`, and every one depends on no axiom beyond
  `propext`, `Classical.choice` and `Quot.sound`.
- Our reading is consistent with the report on these points.

## What could not be verified

- Any statistical result, since the report gives no estimates.
- The accent-strength finding (ST-Q3), cited from a paper not read.
- How common accent modification is, and who does it, in any representative
  sample (ST-D8).

## The bearing on Paper A1

Nothing in the report contradicts the corrected account. One finding lies
outside A1's marker. A milder accent within the same accent group is rated
better (ST-Q3, `steeper_slope_larger_return`), and the same accent is read as
middle class in one population and working class in another (ST-Q11). A1's
marker is a group label read through the group's class distribution. A graded
marker whose strength carries information about class within a group falls
outside group sufficiency, and a reading that changes with the population of
observers is, in A1, a difference in the group distribution those observers
hold.

**The puzzle.** The report places the peak of accent bias at university,
"a more heterogeneous social environment" (ST-Q26), where people "interact
with a substantially wider social range for the first time" (ST-Q18). The
same accent is read as a lower class at an elite university than at home
(ST-Q11), and an accent is read as race as well as class (ST-Q23). These are
the report's own explanations, not tested ones. They agree with the puzzle's
direction, more class reading from markers in mixed settings, and with A1's
premise that in a mixed setting the marker is read through a different group
distribution.

**Budget compatibility.** The report has no account of the origin of class
homophily. One testimonial puts distance across class in past opportunities,
"haven't had the same opportunities as them and can't relate to them"
(ST-Q24). That is closer to shared experience than to shared budgets, and it
is one voice. Nothing in the report bears on cost, affordability or shared
consumption. A1's budget compatibility is neither supported nor contradicted.

**Avoidance that depends on the observer's class.**
- Direction (i), avoidance both ways. The report has both directions, unequal
  in size. Speakers of working-class accents are excluded by students of
  higher class (ST-Q8, ST-Q10), and contact sorts by accent (ST-Q2). Speakers
  of RP are teased for sounding too posh (ST-Q22), and a Southern teacher in
  the North feels "perceived as posh or stuck up" (PDF p. 37). The authors
  report no cost to RP speakers from that bias (ST-Q21). The report supports
  direction (i) as existing in both directions and limits it, since the costs
  are reported for one direction only.
- Direction (ii), a low-class member of the high group preferring outsiders.
  Nothing in the report bears on it directly. The Lancashire student's only
  friends were working-class students met by chance (PDF p. 27), which is
  class homophily within whatever group they belonged to, not evidence on
  direction (ii).
- The observer's class. Senior managers of lower parental grade report more
  mockery, with a class gap of 21 points in social settings against 7 in the
  workplace (ST-Q13, `senior_gap_social_exceeds_workplace`). These are the
  targets' reports, not the observers' choices.

**The penalty vanishing when class is shown.** The report has no statement
about class shown. Expert knowledge narrows the accent penalty in ratings of
competence (ST-Q15), which is information about the individual's competence,
the statistical-discrimination condition, reported from a study not read.

**The shadow market.** This is the report's second use to A1, after passing.
- Channels the law does not reach. "accent, alongside social class, is not
  protected by the Equality Act 2010" (ST-Q17), and working-class candidates
  with the qualifications are kept out by "informal 'poshness tests', such as
  a candidate's style of speaking" (ST-Q16). Screening on a correlated marker
  is lawful and informal.
- Formal evaluation against social settings. For professionals, bias in
  high-stakes contexts such as recruiting is 8%, against 25% in the workplace
  overall and 46% in social settings (ST-Q19, Figures 5 and 6). The authors
  read the low formal rate as "some effort in workplaces" (ST-Q19) and put the
  bulk of accent bias in social settings (ST-Q20,
  `social_exceeds_institutional`). This is the contracting channel constrained
  and the contact channel not, measured on the target's side. The
  high-stakes rate is not lower for university students (ST-D12).
- Signals substituting for the criterion. Modification to RP keeps RP
  over-represented in authority (`modification_raises_heard_RP`), so passing
  makes the marker heard in elite settings a stronger signal of class.

**Passing.** Modification is reported on entry to settings of higher class
(ST-Q6). It is motivated by class inference (ST-Q7) and by competence
inference (ST-Q9). Its costs are a cognitive burden and the loss of standing in one's own group
(ST-Q4). Imperfect modification is read as lack of confidence (ST-Q5), and
pressure to modify falls on speakers of non-standard accents (ST-Q14). The
authors' claim that older professionals modified more (ST-Q12) is not
identified by their figure (`control_not_identified`).

**Premises of A1 the report does not share.**
- A graded, partly modifiable marker (ST-Q3) against A1's group label.
- A reading of the marker that depends on the observers' population (ST-Q11),
  where A1 fixes one group distribution per marker within an equilibrium.
- A dynamic "vicious circle" (ST-Q25), where A1 is a static equilibrium with
  no law of motion.

## Controls

- LC-2. A fabricated quotation, "standard accent speakers feel much more
  pressure to modify their accents", the opposite of ST-Q14, is not found. A
  true quotation is not found on a neighbouring page.
- Lean. `control_not_identified` shows two parameterisations, one with and one
  without modification, give the same Figure 17 rates.
  `control_unweighted_not_24` shows the stated 24% needs the sample weights.
  `control_no_modification_native_share` shows the RP share heard rises above
  the native share only through modification. `control_flat_strength` shows
  modification pays only where ratings fall in accent strength.
