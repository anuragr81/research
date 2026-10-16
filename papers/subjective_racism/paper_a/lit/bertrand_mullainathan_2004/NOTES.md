# Bertrand and Mullainathan (2004), notes

## What was verified

- Fifteen quotations are found on the journal pages they cite in the cached
  layout text, by exact normalised containment, with elisions for the
  interleaved column and for hyphen-broken words (LC-2, BM-D1).
- The arithmetic of Tables 1 and 4 and two of the four correlations of Table 8
  are Lean theorems (LC-4).
- The logic of the Table 8 and Table 6 tests is formalised in Lean. **The
  formalisation of the two tests is of our reading.** The paper states the
  predictions in words and no model, and the linear predictor of class from
  group and address is ours.
- Our reading is consistent with the source on these points.

## What could not be verified

- The probit estimates and their standard errors.
- The White female and White male correlations of Table 8 in Lean (BM-D2).
- Fryer and Levitt (2003) and Mullainathan (2003), which the paper cites.

## The bearing on Paper A1

**What the source does not show, first.** The abstract reports "little
evidence that employers are inferring social class from the names" (BM-Q1). A1
must not read that as evidence against its own channel. The authors say that a
class effect is "of independent interest" only if the names they chose carry a
lower background than the average African-American, or a higher one than the
average White (BM-Q8). Their tests ask whether the name conveys class beyond
race (BM-Q6). A1's channel is class inferred from the group itself, under group
sufficiency, and the paper sets that aside. Under group sufficiency an
employer's inference is the same for every name in a group, so the Table 8
test predicts no association between callback and mother's education rather
than a positive one (`control_group_sufficiency_zero`, against
`background_concordant`). The negative correlations of Table 8 (BM-Q10,
BM-Q12) therefore reject class conveyed by the name, and they say nothing
about class conveyed by race.

**What does cut against an inference reading, in hiring.** Better credentials
raise callbacks less for African-American names (BM-Q2, `table4_returns`). The
authors read this as the opposite of what a model with race as a proxy for
unobservable skill predicts (BM-Q13, BM-Q14). If a credential were a shown
signal of class, A1's inference would predict the reverse. A1 concerns contact
rather than hiring, and a credential is skill rather than class, but a referee
will raise this result against any account of the race penalty as inference.

The candidate contributions of A1 are taken in turn.

1. **Group avoidance from class homophily under inference.** Not in the
   source. Employers rank applicants vertically. Nothing depends on the
   employer's own class, and no group's spread is penalised.
2. **Budget compatibility.** Not in the source.
3. **Venue exclusion.** Not in the source.
4. **A test of the class channel.** The source has the nearest tests in the
   labour market, of whether a name penalty is a class signal. They differ from
   A1's test in two ways.
   - Their tests vary the class conveyed within a group, by name (Table 8), and
     A1's test shows the individual's class. Neither of their tests shows class
     in A1's sense.
   - The address is the nearest thing to showing class, and a better address
     does not narrow the gap (BM-Q4, BM-Q7). In a linear predictor of class
     from group and address with a common weight on the address, the gap is the
     same at every address (`gap_indep_address`). It vanishes only when the
     address shows class fully (`gap_vanishes_class_shown`). It moves with the
     address only if the weight differs by group
     (`control_unequal_weights_gap_moves`). So the address result is consistent
     with a class channel unless the address reveals class, which a zip code
     average does not. This is our reading, not the authors'.
   - Lexicographic search (BM-Q15) would make any shown class unread. A1's test
     needs class not only shown but seen.

What A1 should cite the source for. The size of the name penalty in hiring
(BM-Q2, `table1_gap`), the authors' own statement of the class channel as an
inference from a name (BM-Q5), and the scope of their class tests (BM-Q8), as
the gap A1's test fills. A1 should also cite the design choice of names over
affiliations because affiliations "may especially convey more than race"
(BM-Q3), which bears on markers that are chosen.

## Controls

- LC-2. A fabricated quotation, "We also find strong evidence that employers are
  inferring social class from the names.", the opposite of BM-Q1, is not found.
  A true quotation is not found on a neighbouring page.
- Lean. `control_group_sufficiency_zero` shows the positive concordance of
  `background_concordant` fails when the callback depends on the group only.
  `control_unequal_weights_gap_moves` shows the invariance of
  `gap_indep_address` fails when the weight on the address differs by group.
