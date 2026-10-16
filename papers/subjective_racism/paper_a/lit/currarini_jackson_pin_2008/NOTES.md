# Currarini, Jackson and Pin (2008), notes

## What was verified

- Seventeen quotations are found on the PDF pages they cite, by exact
  normalised containment in the working paper's text layer (LC-2).
- The model is implemented in `lean/mathlib/CurrariniEtAl2008.lean`. The
  homophily measures, footnote 10, Table 1, Lemma 1, the two-type steady
  state, eq. (4), Propositions 1, 2, 4, 5, 7, 8 and 10, Claim 1, Section 9
  and Appendix D are Lean theorems, in the generality the coverage table in
  `CLAIMS.md` gives. Propositions 3, 6 and 9 are covered in special cases.
  Our reading is consistent with the source on these points, except for
  CJP-D2 and CJP-D8.
- The last step of the proof of Proposition 6 fails for types below baseline
  (CJP-D8, `control_prop6_step_fails`), so the claim that larger groups show
  more inbreeding homophily is unproven for more than two types.

## What could not be verified

- The existence results that need Brouwer's theorem or random matching on a
  continuum, the fit of Section 7.2 and the welfare plots.
- The Hispanic value of `IH` in the example school (CJP-D2).
- The Econometrica version (CJP-D1).

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1.

**The puzzle.** Inbreeding homophily by race is an inverted U in group share,
near 0 for very small and very large groups and near .8 for groups of
middle size (CJP-Q2, CJP-Q4). With two large groups a middle share is a
racially mixed school, so own-race bias relative to the population share is
largest in mixed schools. This runs in the same direction as Chetty et al.'s
higher cross-class friending bias in racially diverse schools, but it
measures own-race friendship by race, not friendship across class. Within
the model the inverted U needs a bias in meeting, largest for small groups
(CJP-Q11, CJP-Q14), because without it one group's homophily is the other's
heterophily (`prop2`, `eq6`). The source therefore offers a rival
explanation of A1's puzzle, meeting bias through friends, clubs and
activities (CJP-Q10), and leaves open why the bias would cross from race to
class. A1's explanation is that class is read from the race marker.

**Budget compatibility.** The source assumes homophily rather than deriving
it. Preferences have a same-type bias (Definition 7), and meetings may be
biased toward one's own type (eq. 7). The only cost is a time cost `c` per
unit of search, the same for all types, so nothing depends on budgets. The
authors suggest origins of same-type bias in words, among them shared
interests (CJP-Q15) and the ease of sustaining a risk-sharing agreement with
those who are closer (CJP-Q16). Shared interests are the nearest the source
comes to shared consumption, and the risk-sharing remark is a cost of
sustaining a cross-type relationship. Neither is modelled, and neither is a
budget constraint. A1's budget compatibility is a different origin and does
not conflict with the source.

**Avoidance that depends on the observer's class.**
- Direction (i), avoidance running both ways. The source supports it in
  form. Preferences are the same for every type (CJP-Q6) and distinguish
  only same from different (CJP-Q5), so each group favours its own and the
  bias runs both ways. It is assumed for the seen type, not derived from an
  unseen one.
- Direction (ii), a low-class member of the high group preferring outsiders.
  The source cannot represent it. Types are the groups, there is no
  variation within a group, and preferences are not indexed by type, so no
  member of a group differs from another in whom she favours.
- The marginal return to friendships rising with the share of one's own type
  (`prop4`, `prop5_t_mono`) gives larger groups more friends. In A1 the
  observer's own class sets which group is closer, which the source has no
  counterpart for.

**The penalty vanishing when class is shown.** Not applicable. Types are
seen and nothing is inferred, so there is no group penalty that showing a
trait could remove.

**The shadow market.** The source has no law and no prices. Two points bear
on it.
- Directing one's search toward one's own type pays when preferences have a
  same-type bias (CJP-Q17), and the authors name meeting friends through
  friends and joining clubs and activities as the means (CJP-Q10). That is a
  return to information about where to go and whom to meet, which is the
  object of the predecessor's shadow market. The source takes the meeting
  bias as exogenous, with no market for it and no intermediary.
- The welfare analysis treats school tracking and other constraints on the
  meeting process as policy levers (PDF p. 30). Changing the meeting process
  changes homophily without changing preferences (`control_biased_matching_all_homophilous`).

**Premises of A1 the source does not share.**
- Types are seen. A1's class is inferred from a marker.
- No meeting is ever refused (CJP-Q7). A1's observer decides whether to
  engage.
- Utility is the same for every type and depends only on same against
  different (CJP-Q5, CJP-Q6). A1's engagement utility depends on the squared
  distance between the two classes.
- The model is a steady state of flows with a time cost, not a static Nash
  equilibrium of engagement.

## Controls

- LC-2. A fabricated quotation, "If the matching process is unbiased, then for
  any preferences and any steady state equilibrium, every type can be
  homophilous at once", the opposite of CJP-Q9, is not found. A true
  quotation is not found on a neighbouring page.
- Lean. Each main result has a control. Proposition 1,
  `control_type_sensitive_numbers_differ`. Proposition 2,
  `control_biased_matching_all_homophilous`. Proposition 3,
  `control_equal_inflows_half`. Claim 1, `control_claim1_needs_anti`.
  Proposition 4, `control_no_bias_equal_times`. Proposition 5,
  `control_prop5_flat`. Proposition 6, `control_prop6_step_fails`.
  Proposition 7, `control_log_not_proportional`. Proposition 9,
  `control_prop9_needs_anti`. Proposition 10, `control_prop10_alpha`.
  Proposition 8 has no separate control. It is an identity given the steady
  state and Proposition 7.

## Suite status

The suite ends with 11 checks and 0 failures when the edited module is
compiled into the scratch directory and the axiom audit is pointed at it,
with all 47 cited theorems depending only on `propext`, `Classical.choice`
and `Quot.sound`. Until the coordinator rebuilds the library, the audit run
through `lake env` finds the older build.
