# Currarini, Jackson and Pin (2008), notes

## What was verified

- Fourteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the working paper's text layer (LC-2).
- The homophily measures, `H_i = q_i` in equilibrium, Propositions 1 and 2,
  eq. (6), and the Section 4.5 example are Lean theorems in the paper's
  notation (LC-4). The white and black values of `IH` in Table 1 are
  reproduced. Our reading is consistent with the source on these points.

## What could not be verified

- The Hispanic value of `IH` in the example school (CJP-D2).
- The existence results, the fit of Section 7.2 and the welfare results.
- The Econometrica version (CJP-D1).

## The questions put to this source

**How the paper separates a taste for same-type friends from biased
meeting.** Within the model, by two impossibility results that each shut one
channel. Without a taste (type-neutral `U`), every type forms the same number
of friends whatever the matching (Proposition 1, CJP-Q8, `prop1`), so the
rise of friend counts with group size needs a taste. Without biased meeting,
one group's inbreeding homophily forces another's heterophily (Proposition 2,
CJP-Q9, `prop2`, `eq6`), so homophily in every group needs biased meeting.
The fit and Figure 7 then assign the inverted U to meeting bias and the
friend counts to taste (CJP-Q12). The separation rests on equal meeting
rates across types. With increasing returns in meeting, the matching process
alone could produce the friend counts (CJP-Q13).

**How homophily varies with group size.** Relative homophily `H_i` rises with
group share. Inbreeding homophily `IH_i` is an inverted U, near 0 for very
small and very large groups and near .8 for groups of middle size (CJP-Q2,
CJP-Q4). In the model without meeting bias, the smaller group is
heterophilous (eq. 6), so the fit needs the meeting bias to be largest for
small groups (CJP-Q11).

**The bearing on the Chetty finding.** With two large groups, a middle group
share is a racially mixed school, so the source's inverted U says that own-race
bias relative to the population share is largest in racially mixed schools.
That pattern runs in the same direction as higher friending bias in racially
diverse schools. It is a different measurement, though. The source measures
own-race friendship by race, and Chetty et al. measure cross-class friendship
against exposure. The source attributes its pattern to biased meeting through
friends, clubs and activities (CJP-Q10, CJP-Q14), and not to tastes and not
to inference about an unseen trait. It therefore supplies a rival account
for A1 to set against its own. A1 must say why its channel, class read from a
race marker, predicts the cross-class pattern in diverse schools where a
race-based meeting bias would not.

## The bearing on Paper A1

Nothing in the source contradicts the A1 model. Two premises differ. Agents
never reject a meeting (CJP-Q7), whereas A1's observer decides whether to
engage. And types are seen, so nothing is inferred.

1. **Group avoidance from class homophily under inference (contribution 1).**
   Not in the source. Preferences are over the seen type, same or different,
   with no distance between types (CJP-Q5), and they are the same for every
   type (CJP-Q6). So there is no dependence on the observer's own position,
   no inference of an unseen type from a marker, and no term in a group's
   spread. The source's same-type bias (Definition 7) is the taste for
   same-type friends that A1 replaces with class homophily under inference.
2. **Budget compatibility (contribution 2).** Not in the source. The only
   cost is the time cost `c` per unit of search.
3. **Venue exclusion and price screening (contribution 3).** Not in the
   source. Clubs and activities appear as a reason meetings are biased toward
   one's own type (CJP-Q10), stated in words and not derived, with no
   exclusion and no screening on an unseen type. A1's venue channel differs
   in that a venue selects its clients by what they infer from markers.
4. **A test of the class channel (contribution 4).** Not in the source.

What A1 should cite. CJP-Q2 and CJP-Q4 for the inverted U of own-group bias in
group share. CJP-Q12 and CJP-Q14 for the rival account by biased meeting.
CJP-Q5 and CJP-Q6 for a taste over the seen type that does not depend on the
agent's own type, the specification A1 departs from.

## Controls

- LC-2. A fabricated quotation, "If the matching process is unbiased, then for
  any preferences and any steady state equilibrium, every type can be
  homophilous at once", the opposite of CJP-Q9, is not found. A true
  quotation is not found on a neighbouring page.
- Lean. `control_type_sensitive_numbers_differ` shows that Proposition 1
  fails without type neutrality, since in the Section 4.5 example the two
  types form `√3/(8c²)` and `(3 − √3)/(8c²)` friendships.
  `control_biased_matching_all_homophilous` shows that Proposition 2 fails
  under the biased process (7). With `β = 2` and equal groups, both meet their
  own type with probability `√2/2 > 1/2`.

## Suite status

The coordinator adds `CurrariniEtAl2008` to the build roots. Until then LC-4
fails its build-root check. With the module compiled into the scratch
directory and that directory on `LEAN_PATH`, the axiom audit passes for all
twelve cited theorems, which depend only on `propext`, `Classical.choice` and
`Quot.sound`.
