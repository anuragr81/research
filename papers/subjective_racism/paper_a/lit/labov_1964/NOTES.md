# Labov (1964), notes

## What was verified

- Twenty-seven quotations are found on the PDF pages they cite in the OCR text
  layer, by exact normalised containment (LC-2). Elisions are used where a line
  ends in a soft hyphen (LV-D2).
- The Lean file formalises our reading of the structures the dissertation states.
  The dissertation states no model of choice or equilibrium, so the formalisation
  is of its measures, orderings, tables and stated inferences, not of a model.
  The measures are the (r), (th)-(dh), (eh) and (ay) indexes, the SEC matrix and
  the SC index, the index of linguistic insecurity and the tests of the subjective
  reaction test. The orderings are the department store result, Table 4 of
  Chapter VII as a regular structure, and the cases of change in apparent time.
  Every printed number that follows from other printed numbers in the tables
  formalised is recomputed. Figure 3's deviation of nine relations in thirty
  follows from the printed Table 4 (`table4_oh_nine_of_thirty`). The inference
  from speech to class is written as Bayes' rule with two classes, which is our
  reading and not the source's notation. Our reading is consistent with the
  source on these points, except for the printed discrepancies LV-D10 and LV-D11.
- 44 cited theorems, with no `sorry`, use only `propext`, `Classical.choice` and
  `Quot.sound`. This was checked against the edited module compiled into the
  reader's scratch directory. `CLAIMS.md` lists the coverage of each result.

## What could not be verified

- Values shown only in figures with garbled OCR, the class 9 (r) value in isolated
  words (LV-D9), and the tables listed as not formalised in `CLAIMS.md`.
- The sample counts on PDF p. 435 do not add up (LV-D5). The four-cell table on
  PDF p. 454 does not match Table 11 (LV-D10). Two percentages and one N of the
  /ʌy/ tables do not match their counts (LV-D11). No row relies on the printed
  figure in any of these cases.
- The 1966 book is not read (LV-D1).

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1. Three of its
findings limit how A1 can cite it. Labov's speech marker is evidence about the
individual's class, not a group marker. His listeners evaluate speech on one
shared scale. In one of his cases a speech penalty survives known class. Each
point is placed below under its heading.

### The puzzle

The source documents that contact between groups does not wear down the markers
that tell them apart. Labov finds "increased differentiation of language behavior
despite close contact" (LV-Q19), and sets this against dialect geography, where
mixing makes speech uniform (LV-Q20). The markers are also read alike by everyone
who hears them (LV-Q9). In a mixed setting a marker therefore stays legible and
keeps its meaning, which is what A1's inference needs. The source also reports
that the ethnic opposition in New York speech was moving from Jews, Irish and
Italians toward white against Negro and Puerto Rican, which Labov ties to rising
economic stratification (LV-Q23). At the bottom of the class scale, white and
Negro speech converge (LV-Q21). The source does not study friendship or contact
rates, so it bears on A1's premises and not on friending bias itself.

### Budget compatibility

Not in the source. The source explains shared speech within a group by the peer
group of childhood (LV-Q24) and by identification with one's immediate group
(LV-Q10, PDF p. 540), not by cost or affordability. Its class index deliberately
leaves out consumption and "how the person spends his money" (LV-Q25). The stores
are ranked partly by price (`store_stratified`, PDF pp. 90 to 91), but the source
says nothing about whether people of different budgets can share an activity.
Budget compatibility is A1's own foundation, and this source neither supplies nor
contradicts it.

### Avoidance that depends on the observer's class (both directions)

The source supports dependence on the observer's group in two respects and limits
it in a third.
- Supports. How hard a listener penalises a speaker depends on the listener's
  class. The middle class stigmatises all but the most cultured speaker (LV-Q8),
  and Negro respondents reverse white New Yorkers' evaluation of New York speech
  (LV-Q22). Labov names a pressure toward one's own ethnic, neighbourhood or
  occupational group (LV-Q10). Children sanction peers who take up middle class
  norms (LV-Q27), which is an in-group penalty for moving toward the other class.
- Limits. On the scale the source measures, every class ranks speakers the same
  way (LV-Q6, LV-Q9). The source's evidence for observer dependence is about
  intensity and loyalty, not about a symmetric preference for partners near one's
  own class. Direction (i), avoidance running both ways, is therefore not shown.
  The in-group pressure that would carry it is named and left unmeasured (LV-Q10).
- Direction (ii), a low-class member of the high group preferring outsiders. The
  nearest evidence is that lower class whites' speech resembles Negro speech
  (LV-Q21), which suggests contact across the race line at the bottom of the class
  scale. Labov attributes this to Negro influence on lower class speech and does
  not measure who befriends whom.

### The penalty vanishing when class is shown

The source states nothing of this kind. It has the opposite case in an
occupational setting. Nathan B., class 9 with a Ph.D., was refused an academic
post because of his speech (LV-Q14). His class was known, and the penalty fell on
the speech itself as a requirement of the role. The subjective reaction test is
built on that kind of requirement, since it rates the jobs a speaker's speech
allows (LV-Q6, LV-Q17). For the link to statistical discrimination, the source's
own reading of class from speech is a statistical inference from an individual
signal (LV-Q5). In our reading the signal informs only when it is stratified
(`posterior_gt_prior`, `control_posterior_eq_prior_unstratified`), as with
Phelps's test score, and the source predicts groups, not individuals (LV-Q15).

### The shadow market

The source does not discuss law. It does contain the instruments that A1's shadow
market works through.
- A criterion that is group-neutral in form and correlated with group. Speech is a
  hiring criterion ("you would take it into consideration", LV-Q17). A store
  selects its staff by speech (LV-Q3). Speech is stratified by class and,
  within class, by ethnicity and race (LV-Q2, `store_orderings`, PDF p. 336).
- Venues as carriers of social information. The stores are ranked by price,
  newspaper advertising, location and floor space (`store_stratified`, PDF pp. 90
  to 92). Staff speech is part of the store's "social imprint" on customers
  (LV-Q26). Saks's upper floors screen browsers at the door (LV-Q18). These match
  the predecessor's atmosphere vector of pricing, location, décor and staff
  manner.
- Gatekeepers. Teachers, personnel managers and employment agencies are named as
  channels of pressure on speech and left for further study (LV-Q16).

The source has nothing on discrimination moving to channels the law does not
reach, on the value of knowing where to go, or on a signal replacing a banned
criterion.

### Premises of A1 the source does not share

- The marker. A1's marker is a group marker under group sufficiency. Labov's
  speech variables are evidence about the individual's class, stratified within a
  single occupation and within each ethnic group (LV-Q1, LV-Q2, LV-Q5).
- Fixity. A1's marker is visible and fixed, unless chosen in passing. Labov's
  markers are frequencies that shift with the formality of the context
  (`table4_regular`). Every speaker moves toward the prestige form when attending
  to speech, though none reaches it (LV-Q13).
- Passing. The nearest object is hypercorrection, in which the lower middle class
  outdoes the upper middle class in prestige forms in formal styles (LV-Q12,
  `r_crossover`). That class also has the highest linguistic insecurity (LV-Q11,
  `table16_ili`). It is partial and context-bound, and the source reports no effect
  on what observers impute to a group.
- Statics. A1 is static. The source reads change across age groups
  (`ay_caseIB`, `r_caseIIB`). Its static content is the cross-section of class by
  style.
- Preferences. The source has no utility or choice of partners. Its evaluations
  are ratings of speech on a common occupational scale (LV-Q6, LV-Q7).

## Controls

- LC-2. A fabricated quotation, "In this measure of linguistic insecurity, the
  upper middle class outdistances all others.", the opposite of LV-Q11, is not
  found on any page. A true quotation is not found on a neighbouring page.
- Lean. Each main result has a control.
  - The store ordering fails under the wage ranking
    (`control_wage_order_not_stratified`).
  - The regularity count detects order (`control_r_wrong_direction`).
  - SEC uniformity holds only at the extremes (`control_sec_middle_not_uniform`).
  - An unstratified form carries no information about class
    (`control_posterior_eq_prior_unstratified`).
  - A regular order admits no cross-over (`control_regular_no_crossover`).
  - The cases of apparent time are distinct (`control_cases_distinct`).
  - The two-choice test does not imply the four-choice test
    (`control_two_not_four`).
  - A zero index of linguistic insecurity does not identify usage
    (`control_ili_zero_not_usage`).
