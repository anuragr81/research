# Labov (1964), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the OCR text layer,
  by exact normalised containment (LC-2). Elisions are used where a line ends in
  a soft hyphen (LV-D2).
- The indexes the dissertation defines, the socio-economic index and its count of
  configurations, the index of linguistic insecurity, the department store
  ordering, the deviation measure and the cross-over are Lean theorems in the
  dissertation's notation (LC-4). The inference from speech to class is
  formalised as our reading, Bayes' rule with two classes, and not as a model the
  source states. Our reading is consistent with the source on these points.
- The axiom audit passes for all twenty cited theorems when the module is
  compiled into the reader's scratch directory. The build-root check fails until
  the coordinator adds `Labov1964` to the roots.

## What could not be verified

- Values shown only in figures with garbled OCR, and the class 9 (r) value in
  isolated words (LV-D9).
- The sample count on PDF p. 435 does not add up (LV-D5). No row relies on it.
- The 1966 book is not read. Wording and pages may differ from the dissertation
  (LV-D1).

## The bearing on Paper A1

Three points in the source sit uneasily with the A1 model as described, and come
first.

- **The marker in Labov is individual evidence of class, not a group marker.**
  The speech variables are stratified by class within one occupation (LV-Q1,
  LV-Q2) and within each ethnic group, Italians and Jews following the same class
  pattern at different levels (PDF p. 336). The author reads a speaker's class
  from his own speech frequencies (LV-Q4, LV-Q5). Under A1's group sufficiency the
  marker tells the observer the group and nothing more about the person. Labov's
  speech variables are closer to Phelps's test score. A1 can cite Labov for the
  premise that class is read from cues, but it must say that a speech cue is
  individual evidence, which A1 leaves out, while A1's marker is the group
  marker. The individual reading is noisy (LV-Q15), and in our reading it raises
  the posterior on the higher class only when the form is stratified
  (`posterior_gt_prior`, `control_posterior_eq_prior_unstratified`).
- **The evaluation measured is vertical and shared.** Listeners rate the highest
  job a speaker's speech allows, on one ordered scale (LV-Q6), and are told not to
  judge the job she holds (LV-Q7, LV-D7). All classes rank the speakers alike
  (LV-Q8), and every respondent aged 18 to 39 rates the prestige form up (LV-Q9).
  The test is not a test of whether listeners infer class, and it is not a test of
  homophily.
- **A marker penalty survives known class.** Nathan B., class 9 and a Ph.D., was
  refused an academic post unless he corrected his stops for (th) and (dh)
  (LV-Q14, PDF pp. 277 to 280). His class was known, so the penalty falls on the
  speech itself, valued for the role. This is the rival to A1's class channel in
  an occupational setting.

The candidate contributions of A1 are taken in turn.

1. **Group avoidance from class homophily under inference.** Not in the source.
   The listener's own class changes how hard the middle class penalises a
   speaker on the common scale (LV-Q8), and those who use a stigmatised form most
   are most sensitive to it (PDF p. 463). Neither is a preference for partners
   near one's own class. Labov names a pressure toward one's own ethnic,
   neighbourhood or occupational group beside the pressure toward the common
   ranking (LV-Q10), and says his test measures only the second (PDF p. 434). The
   first pressure is the nearest thing in the source to A1's observer-dependent
   preference, and it is unmeasured. Listeners penalise a speaker's own
   inconsistency in a variable (PDF pp. 451 to 455). That is variance within one
   speaker's marker, not the spread of class within a group, so A1's σ² term has
   no counterpart here.
2. **Budget compatibility as the source of class homophily.** Not in the source.
3. **Venue exclusion as the aggregate of client inference.** Not in the source.
   The stores are ranked by price, advertising and location (PDF pp. 90 to 92),
   and the staff's speech follows the store's rank (LV-Q2). The store selects
   staff by speech (LV-Q3), and receptionists on Saks's upper floors "screen out
   the casual spectators from the serious buyers" (PDF p. 105). These are a
   venue's screening of its staff and of browsers. No client infers other
   clients' class from a group marker.
4. **A test of the class channel.** Not in the source. The source has the rival
   case, a speech penalty that remains when class is known (LV-Q14). A test in
   A1's form has to separate the marker's value as a class signal from any value
   the marker has in itself.

**Passing.** Hypercorrection is the lower middle class outdoing the upper middle
class in prestige forms in formal styles (LV-Q12), with the highest index of
linguistic insecurity (LV-Q11). It can be read as attempted passing by marker, a
choice of variant that overshoots the class imitated. The source frames it as
insecurity and as a mechanism of change, not as misrepresentation, and records
that adoption fails under conscious effort (LV-Q13). The source has no effect of
such adoption on what observers impute to the group, so it neither contains nor
contradicts the Kim and Loury result that passing lowers the visible mean.
`notes/motivation_scan.md` cites "Labov (1966) [F]" for markers that "can be
used to misrepresent class". The dissertation supports attempted adoption of a
higher class's forms, in formal styles and imperfectly, and does not use the
word misrepresent.

**What A1 should cite the source for.** The ordering of a speech marker by the
social rank of groups, with the department store numbers (LV-Q1, LV-Q2). The
author's own inference of class from speech (LV-Q4, LV-Q5). Listeners' evaluation
of speech on an occupational scale, with the caveat that it is evaluation, not
inference (LV-Q6, LV-Q7). Linguistic insecurity and hypercorrection in the lower
middle class (LV-Q11, LV-Q12). The limits of the inference for individuals
(LV-Q15).

## Controls

- LC-2. A fabricated quotation, "In this measure of linguistic insecurity, the
  upper middle class outdistances all others.", the opposite of LV-Q11, is not
  found on any page. A true quotation is not found on a neighbouring page.
- Lean. `control_wage_order_not_stratified` shows the store ordering fails when
  the stores are ranked by wages, and `control_posterior_eq_prior_unstratified`
  that a speech form used equally by both classes carries no information about
  class. `control_sec_middle_not_uniform` and `control_regular_no_crossover` are
  further controls.
