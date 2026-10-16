# Bertrand and Mullainathan (2004), notes

## What was verified

- Twenty quotations are found on the journal pages they cite in the cached
  layout text, by exact normalised containment, with elisions for the
  interleaved column and for hyphen-broken words (LC-2, BM-D1, BM-D10).
- Every number in Tables 1, 2 and 4 that follows from other printed numbers,
  the classification of Table 2, two of the four correlations of Table 8, the
  race-blind benchmark of Section IV.A and the four models of Section IV.C are
  Lean theorems (LC-4). Two printed values of Table 1 disagree with the rates
  (BM-D9).
- The logic of the Table 8 and Table 6 tests is formalised in Lean. **The
  formalisation of the two tests is of our reading.** The paper states the
  predictions in words, and the linear predictor of class from group and
  address is ours.
- Our reading is consistent with the source on these points.

## What could not be verified

- The probit estimates and their standard errors, the tests of proportions,
  and the averages of Table 8.
- The White female and White male correlations of Table 8 in Lean (BM-D2).
- Fryer and Levitt (2003) and Mullainathan (2003), which the paper cites.

## The bearing on Paper A1

Nothing in the source contradicts the corrected account. One finding limits the
link A1 draws to statistical discrimination, and it comes first. Better
credentials raise callbacks less for African-American names (BM-Q2,
`table4_all`). A model in which race stands in for unobservable skill predicts
the reverse (BM-Q13, BM-Q14, `phelps_larger_update`). A model in which the same
credential is noisier for one group predicts the finding
(`aigner_cain_smaller_update`), and so does lexicographic search (BM-Q15,
`lexicographic_zero_return`). A1 must place its property among these.

### The puzzle

The source is about hiring, not friending, and does not bear on friending bias
directly. Its one result on the racial make-up of a setting runs the other way
from the puzzle. Employers located in more African-American neighbourhoods
discriminate somewhat less (BM-Q20), significantly so only in Chicago (p. 1006).
That is the contracting channel, where the law applies, and A1's puzzle is in
the contact channel.

### Budget compatibility

Not in the source. Employers rank applicants vertically, and the paper says
nothing on cost, affordability or shared consumption. The only bearing is that
an applicant's neighbourhood raises callbacks for everyone (BM-Q18), so
employers value a correlate of class in its own right.

### Avoidance that depends on the observer's class

The source cannot speak to either direction. The employer's own class and race
are not observed. The only attribute of the observer measured is location
(BM-Q20). At the level of ads, 3.5 percent of employers favour African-American
names against 8.4 percent favouring White names (`table2_counts`), which shows
that treatment is not uniform across employers but does not say why.

### The penalty vanishing when class is shown

The source states two versions of the property, each with its condition, and
finds neither.
- If the names mainly signal background, the gap should be smaller at better
  addresses (BM-Q7). It is not (BM-Q4).
- If race stands in for unobservable skill, credentials should help
  African-Americans more (BM-Q13, BM-Q14). They help them less (BM-Q2).

Two things limit what these findings say about A1's property. First, the tests
concern class conveyed by the name beyond race (BM-Q6, BM-Q8). Class conveyed
by race itself, A1's channel under group sufficiency, is set aside, and under
group sufficiency the Table 8 test predicts no association rather than a
positive one (`control_group_sufficiency_zero`, against
`background_concordant`). Second, an address shows class only in part. In a
linear predictor with a common weight `w` on the address, the gap is
`(1 − w)(μ_W − μ_B)` at every address (`gap_indep_address`). It vanishes when
the address shows class fully (`gap_vanishes_class_shown`) and moves with the
address level only if the weight differs by group
(`control_unequal_weights_gap_moves`). A1's property is the limit `w = 1`.
The source varies the level of a partial signal, not its precision, so it does
not reach that limit. This reading is ours, not the authors'.

For the link to the statistical-discrimination literature, the source is where
the two classes of model are set against data. The Phelps and Arrow class
predicts larger returns to shown information for the group with the more
uncertain prior. The Aigner and Cain class predicts smaller returns for the
group whose signal is noisier (`phelps_larger_update`,
`aigner_cain_smaller_update`, `control_equal_precision`). A1's property should
state which of the two it shares.

### The shadow market

The source bears on four of the items in the corrected account.
- **A marker through which a banned criterion enters.** Race is not on the
  resume, the name carries it, and screening on it "matches the legal
  definition of discrimination" (BM-Q19, `race_blind_equal`,
  `control_race_used`). The authors chose names because affiliations would
  carry more than race (BM-Q3).
- **A group-neutral variable correlated with group.** The address raises
  callbacks, more for Whiter, more educated and richer zip codes (BM-Q18,
  `table6_college`), and screening on it is legal.
- **Legal strictness.** Equal Opportunity Employers and federal contractors do
  not show a smaller gap (BM-Q17), and each shows a larger one, marginally
  significant for federal contractors (p. 1005). The authors note these
  employers may receive more African-American applications (n. 41). In the
  contracting channel, tighter legal exposure does not shrink the gap.
- **The channel the experiment cannot reach.** Social networks are "another
  common means through which people find jobs" and cannot be studied here
  (BM-Q16). That is the contact channel, which the corrected account says the
  law does not reach.

### Premises of A1 the source does not share

- The observer is an employer who ranks applicants vertically. A1's observer
  values distance in class.
- The setting is the contracting channel, a callback, not contact.
- The class question is about the name beyond race (BM-Q8), not about the group.
- Credentials are skill, not class (BM-Q2).

What A1 should cite the source for. The size of the name penalty (BM-Q2,
`table1_gap`), the authors' statement of the class channel as an inference
from a name (BM-Q5), the scope of their class tests (BM-Q8), the contrast
between the two classes of statistical discrimination on returns to
credentials (BM-Q13, BM-Q14), and the shadow-market items above (BM-Q16 to
BM-Q19).

## Controls

- LC-2. A fabricated quotation, "We also find strong evidence that employers are
  inferring social class from the names.", the opposite of BM-Q1, is not found.
  A true quotation is not found on a neighbouring page.
- Lean. `control_group_sufficiency_zero` shows the positive concordance of
  `background_concordant` fails when the callback depends on the group only.
  `control_unequal_weights_gap_moves` shows the invariance of
  `gap_indep_address` fails when the weight on the address differs by group.
- Lean, added in the second pass. `control_race_used` shows equal treatment
  fails once the rule uses race, `control_equal_precision` that neither
  statistical model predicts different returns without a difference in
  precision, and `control_no_taste` that a taste-based gap needs `d > 0`.
