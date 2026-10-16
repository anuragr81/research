# Mill and Stein (2012), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the text layer,
  by exact normalised containment (LC-2).
- Eq. (5.2), the removal of the family effect by a sibling comparison, the
  percentages that the text reads off Table 10, and the share of the gap that
  the sibling comparison removes are Lean theorems in the paper's notation
  (LC-4). Our reading is consistent with the source on these points.
- The Roy argument of PDF p. 34 is formalised in our reading, with a steeper
  White slope in earning potential in place of a higher White variance.

## What could not be verified

- The regression estimates. The microdata are not available here, so the
  numbers are taken as printed.
- The linking procedure, whose appendix is absent from this version (MLS-D7).
- Eleven readings are recorded as MLS-D1 to MLS-D11. No row relies on a passage
  that a typo affects. Footnote 5's 11.4% (MLS-Q14) and footnote 4's match
  rates (MLS-Q15) are quoted only to record that they disagree with the tables.

## The bearing on Paper A1

Nothing in the source contradicts the A1 model. One premise differs. Within the
black group, the finer recorded marker, Mulatto rather than Black, goes with
household status. Mulatto households in the random sample own their homes more
often (0.380 against 0.244) and have literate heads more often (0.664 against
0.562) (Table 2, read from the image), and the authors suggest that enumerators
coded children in higher-status households as Mulatto (MLS-Q6). A1's group
sufficiency says the marker carries no information on class within a group.
In the source the finer marker does.

Who passes. Passers were positively selected on schooling (MLS-Q13), came from
families with higher earning potential (PDF pp. 35 to 36), and needed both
light skin and the skills to act White (MLS-Q7). The authors expect this from a
Roy argument (MLS-Q10), and the returns to schooling are higher for those coded
White (MLS-Q12).

The returns to passing. 31% to 42% across all matched sons, 20% to 31% with a
schooling control (MLS-Q2), and 14% to 21% between brothers (MLS-Q3). Between
brothers with the same schooling the difference is near zero (MLS-D5).

The group left behind. The source states, without measuring it, that sorting
by economic outcomes across the colour line mattered for the long-run
development of African-Americans (MLS-Q9). Through Bond (1931) it reports that
the clubs which admitted only the light-skinned disintegrated as their kind
passed (MLS-Q4, MLS-Q5). Of the three sources read in this wave, this is the
nearest to a statement that passing changes the composition of those who
remain. It is a statement, not a result, and A1 already treats the claim as
known from Kim and Loury (2012).

The candidate contributions of A1 are taken in turn.

1. **Group avoidance from class homophily under inference.** Not in the
   source. The nearest material is Myrdal's teacher, who declined to pass for
   reasons that include his status within the black community and his social
   life "among the higher ranks of African-Americans" (MLS-Q8). That is a
   preference over contact that depends on one's own standing, reported second
   hand [C], and not a model of avoidance or of a group's spread.
2. **Budget compatibility as the source of class homophily.** Not in the
   source.
3. **Venue exclusion from client inference, and price screening.** Not in the
   form A1 has it. The blue-vein societies are venues that admitted on a
   visible marker within the group (MLS-Q5), and in Bond's account their pool
   emptied through exit by passing (MLS-Q4). The screen was the marker itself,
   set by the club, not an aggregate of clients' inference about class, and no
   price enters. The source also records that White venues and markets refused
   African-Americans (PDF pp. 7, 10), which is exclusion on the marker.
4. **A test of the class channel.** Not for contact. The source has the same
   structure in wages within the black group. The population gaps by recorded
   colour in literacy, schooling and earnings largely vanish between brothers
   (Tables 4 to 6), and the earnings gap vanishes in the population once
   schooling is controlled (Table 6, −0.116 to −0.028). The authors read this
   as family background, not colour, driving the gaps. In A1's terms the finer
   marker's penalty is a class difference, and it vanishes when class is held
   fixed. This is an employer-side wage result identified from siblings, not a
   contact decision with class shown to an observer, so it supports the logic
   of contribution 4 without being the test.

What A1 can cite the source for. Passing as a change of contact with appearance
fixed (MLS-Q1), positive selection into passing on schooling and family
(MLS-Q13), the size of the return to passing (MLS-Q2, MLS-Q3), and the
within-group colour gap as a family-background gap (Tables 4 to 6). The
blue-vein societies are a historical venue that screened on the marker
(MLS-Q5), cited through Bond as [C].

## Controls

- LC-2. A fabricated quotation that says the opposite of MLS-Q13, "we see that
  passers were negatively selected on education", is not found on any page. A
  true quotation is not found on a neighbouring page.
- Lean. `control_family_effect` shows that a comparison across families misses
  the return when family effects differ. `control_equal_slopes` shows that the
  Roy selection needs the steeper White slope. `control_log_not_percent` shows
  that the log-point coefficients are not the percentages.
