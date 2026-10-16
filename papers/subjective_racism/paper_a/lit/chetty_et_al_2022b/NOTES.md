# Chetty et al. (2022b), Social Capital II, notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite, by exact normalised
  containment, in a reading-order extraction of the pinned PDF (LC-2, CB-D1).
- The measures of eqs. (1), (3) and (4), the decomposition (2), the log form
  (7), the benchmark (5) under our reading of it, and the printed numbers of
  Fig. 3a, Fig. 3b and Fig. 6 are Lean theorems (LC-4). Our reading is
  consistent with the source on these points.
- Two printed statements do not hold as printed and are recorded with the
  reading adopted. The TCEC formula contradicts the text unless its first
  factor is the low-SES share (CB-D5). The 73% and 16% across ZIP codes are the
  means of the two orders of the decomposition, which the paper does not say
  (CB-D4).

## What could not be verified

- The regressions, the reliabilities and the quasi-experimental estimates. They
  rest on data we do not hold and are recorded as printed.
- The size of the racial-diversity gradient. The paper reports no coefficient,
  and the readings in CB-D7 are from plots.
- The Supplementary Information of the Nature article. The NBER version's
  supplementary material is read instead, and its differences are CB-D8 to
  CB-D13.

## The bearing on Paper A1

One finding bears against the A1 model if its marker is read as race alone,
and is set out first. In the least racially diverse ventile of high schools,
friending bias by parental SES is about −1% (Extended Data Fig. 1e, CB-D7),
against a no-homophily benchmark of −10% for high schools (p. 16,
`benchmark_values`). In neighbourhoods the least diverse ventile shows about
2% against −11%. So low-SES people under-befriend high-SES people within
groups that are nearly uniform in race. Under group sufficiency with race as
the only marker, everyone in a one-race group looks alike, engagement cannot
depend on class within it, and friending bias would sit at the benchmark. The
data therefore need either a marker richer than race (dress, speech, tracking,
neighbourhood) or channels of exposure inside the group, such as tracking
(Extended Data Fig. 1a,b) and segregation within ZIP codes (CB-D10). This is
inferred from figure readings and the printed benchmark, and is not a result
of the paper.

How the hook reads once quoted exactly. Friending bias is one minus a ratio of
shares within a group (CB-Q2). It is a statistical measure that the authors
decline to read as preference (CB-Q3). It is higher in more racially diverse
high schools (CB-Q6), colleges and neighbourhoods (CB-Q8). The authors'
suggested explanation is that students of different racial backgrounds are less
likely to be friends (CB-Q7), and they say the determinants are largely unknown
(CB-Q9). Their explanation runs from race to SES. A race-based friending
pattern, together with the correlation of race and SES, shows up as SES
friending bias. A1's runs the other way. Class homophily together with
inference of class from the marker produces avoidance by marker, which also
shows up as SES friending bias. Both predict the sign of the gradient, so the
gradient does not separate them. Neither version observes individual race
(CB-D16), so neither can say whether the missing cross-SES friendships are
cross-race ones.

The candidate contributions are taken in turn.

1. **Group avoidance derived from class homophily under inference.** Not in
   the source. The source has no model of friending, and its one suggested
   mechanism (CB-Q7) is a lower rate of friendship across racial lines, stated
   and not derived. Nothing in it depends on the observer's own class, and
   nothing penalises a group's spread. The source measures the bias of low-SES
   people toward high-SES people and of high-SES people toward high-SES people
   (Fig. 2c). The first is positive and the second negative in every setting
   except religious groups, where both are negative (Extended Data Table 4). A1 can cite that pattern as a fact its model reproduces, but the
   same pattern arises when all observers rank partners alike and friendship
   needs both sides to agree, so it does not support contribution 1 over the
   vertical rival.
2. **Budget compatibility as the micro-foundation of class homophily.** Not in
   the source as a model. The source offers three cost examples as
   suggestions from student newspapers and an architect's account, namely
   fraternity dues (CB-Q10), lunchrooms split by "options for low-cost and
   free lunch" (CB-Q12) and, in the NBER version only, prom tickets (CB-D11).
   A1 may cite them as motivation. The motivation scan's statement that dues
   are the source's "only cost evidence" should be corrected to name the
   lunchroom passage as well.
3. **Venue exclusion as the aggregate of client inference.** Not in the
   source. Exposure is the share of high-SES members of the group a person is
   in (eq. 3), taken as given and attributed to segregation across groups
   (p. 3). The source shows that settings shape friending bias more than the
   people in them (CB-Q4, Fig. 2d), proposes new venues (CB-Q13), and finds
   that exposure and bias are complements (p. 16). A1's venue choice would
   make exposure an equilibrium outcome. The source does not.
4. **A test of the class channel.** Not in the source. The causal designs vary
   the SES composition of a cohort (Fig. 6, Extended Data Fig. 4), not the
   visibility of class, and the paper never conditions on the race of the
   friend. The test A1 proposes, that the group penalty in contact vanishes
   when class is shown, has no counterpart here and would be the way to
   separate A1's direction from the authors' (CB-Q7).

What A1 should cite or build on. The definition (CB-Q2) and decomposition
(`eq2`), so that A1's prediction can be stated in the source's measure. The
order dependence of "about half" (CB-Q11, CB-D3), so that the manuscript
does not write "half" without the order. The Nature wording of the hook
(CB-Q1, CB-D2). The fact that SES in the data is itself predicted from visible
correlates such as phone price and ZIP code (CB-Q14), which is the
econometrician's version of A1's observer inference.

## Controls

- LC-2. A fabricated quotation that reverses CB-Q6, "Friending bias is lower in
  more racially diverse schools", is not found on any page. A true quotation is
  not found on a neighbouring page.
- Lean. `control_bias_zero_iff_equal_friends` shows that zero bias under random
  friending needs equal friend counts. `control_order_matters` shows that the
  share given to exposure depends on the order whenever both factors differ.
  `control_tcec_printed` shows that the printed TCEC, read literally, is not
  maximised at a 50% share. `control_fig3b_single_order` shows that neither
  order alone gives the printed 73%.
- The formalisation is of the paper's definitions and stated identities, and
  of our reading of eq. (5) and of TCEC. It models no behaviour.
