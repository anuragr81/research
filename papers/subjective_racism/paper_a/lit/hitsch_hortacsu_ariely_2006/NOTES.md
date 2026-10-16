# Hitsch, Hortaçsu and Ariely (2006), notes

## What was verified

- Twenty-one quotations are found on the PDF pages they cite in the text layer,
  by exact normalised containment (LC-2).
- The estimating model is formalised in the paper's notation. That covers the
  threshold rule, the expected utility of a meeting, the Bellman equations,
  stability, the monotone count of contacts under assumptions 1 and 2, the
  Poisson ratio, eq. (5) with footnotes 20, 21 and 23, the logit, the two by two
  case of Section 6.3, and every number that follows from other printed numbers
  (LC-4). The coverage table in `CLAIMS.md` lists what is general, what is a
  special case and what is not formalised.
- Our reading is consistent with the source on these points.

## What could not be verified

- The estimates. They come from data that are not available. They are read as
  printed.
- Adachi's results and the stable-matching lattice, which the source cites.
- Table 5.6 exactly. It is reproduced only to within about $6,000 from the
  rounded coefficients (HHA-D3).
- Fisman et al. (2005), whose speed-dating result the paper reports (HHA-Q13),
  is not read and is [C].

## The bearing on Paper A1

Two findings limit the corrected account, and they come first.
- **Income enters vertically.** Own income "matters only slightly" in valuing a
  partner's earnings (HHA-Q5), and women weight a man's income about twice as
  much as men weight a woman's (PDF pp. 27–28). In mate choice the source finds
  no homophily in income, which is the dimension where budget compatibility
  would act most directly.
- **The penalty survives shown class proxies.** Income and education are in
  every profile (HHA-Q2), and the ethnic penalty in first contacts is measured
  with them held fixed (HHA-Q3, HHA-Q4). The paper does not claim to test
  anything about A1. Its finding is that, in dating, showing these proxies does
  not remove the group penalty.

### The puzzle

The source has no variation in racial diversity and does not bear on friending
bias directly. It does show that the setting changes sorting. Same-race
preferences estimated online generate less endogamy than offline marriages show
(HHA-Q14). Traditional meeting places limit choice sets (HHA-Q19), and a venue
fixes which attributes can be seen (HHA-Q20). So the structure of a setting,
and not preferences alone, governs how much sorting by group appears.

### Budget compatibility

The source names three origins of sorting. These are a preference for a similar
partner, a common vertical ranking with stable matching, and frictions that
limit choice sets (HHA-Q19). It shows that sorting alone cannot tell the first
two apart (`sorting_observationally_equivalent`, against
`control_opposite_tastes`), and it concludes that sorting is "largely driven by
preference heterogeneity" (HHA-Q18). That is the source's assumption about
origin. Preferences that depend on one's own attribute are taken as given and
are not derived from costs. On the dimensions, income is vertical (HHA-Q5) and
education has a homophilous part (HHA-Q6, HHA-Q7). Nothing in the source bears
on affordability or shared consumption. The only cost it models is the cost of
writing an e-mail (PDF p. 15), which it finds small. A1 should say that budget
compatibility predicts homophily in what a couple can do together, and that
the source finds homophily in education rather than in income.

### Avoidance that depends on the observer's class

- **Both directions between groups.** Members of all four ethnic groups
  contact the other groups less (HHA-Q16). That supports direction (i).
- **Dependence on the chooser's own level, for education.** Less educated men
  avoid more educated women, and men with college or graduate degrees show
  little preference for similar education (HHA-Q17). Women prefer men of
  equivalent education (HHA-Q7). The avoidance depends on the chooser's level,
  as in A1, but asymmetrically. A1's squared distance is symmetric
  (`symmetric_quadratic`) and predicts that the more educated avoid the less
  educated as much as the reverse. The source finds that for women and not for
  men.
- **A low-class member of a high group.** The source cannot speak to direction
  (ii). Its race term `ϑ^{kl}` depends on the chooser's ethnicity and not on the
  chooser's income or education (eq. 5, `UM`), so the specification rules out
  the interaction. The probit's positive race terms for black choosers are
  mostly insignificant and are not relied on (HHA-D7).

### The penalty vanishing when class is shown

The source states no such property. It models race as a taste term in utility
(eq. 5), not as a signal of an unseen attribute, so it has no channel through
which showing class could remove the penalty. Its finding is the empirical
fact above (HHA-Q3, HHA-Q4), with its size in income units (HHA-Q8) and its
absence of any offset for women (HHA-Q9, `control_flat_income_not_feasible`).
Two conditions limit how far the fact reaches. Income is self-reported and
often missing (PDF p. 22). The race term is not identified apart from any trait
that is unseen and correlated with race, and Fisman et al., as reported here,
find the race penalty in speed dating shrinks once shared interests are
measured (HHA-Q13, [C]). For the link A1 draws to statistical discrimination,
the source is a case where the group term behaves as a taste given the class
proxies shown.

### The shadow market

The source has no law and no banned criterion. In dating the contracting
channel is open. The site lets users declare a preference for their own
ethnicity (HHA-Q21), and users act on race whether or not they declare it
(HHA-Q15). On venues, the source names "upscale" and "dive" bars, clubs and
church socials as institutions that "may well have arisen endogenously to
facilitate sorting along certain traits" (HHA-Q12), and it notes that a noisy
bar makes looks the main attribute anyone can sort on (HHA-Q20). Both are
statements about venues as filters of what is seen, which is the material of
A1's shadow market, and neither is modelled. The platform itself is an
intermediary of social information, since it puts income and education on
display (HHA-Q2).

### Premises of A1 the source does not share

- Every attribute in a profile is seen, and preferences are over seen
  attributes (HHA-Q2, `UM`). A1 has class unseen.
- Race is a separate taste (eq. 5), not an inference.
- Preferences over income are vertical (HHA-Q5).
- The setting is mate choice, and stability is the solution concept
  (`Stable`). A1's engagement is a static Nash equilibrium of contact.

What A1 should cite the source for. The threshold rule for contact (HHA-Q1),
the estimated squared distance utility (HHA-Q11, `symmetric_quadratic`), the
observational equivalence of vertical and homophilous preferences in sorting
(`sorting_observationally_equivalent`), avoidance between groups in every
direction (HHA-Q16), and the survival of the racial penalty given income and
education in dating (HHA-Q3, HHA-Q4).

## Controls

- LC-2. A fabricated quotation, "Note that these results do not control for
  other observable user attributes, such as income and education.", the
  opposite of HHA-Q3, is not found. A true quotation is not found on a
  neighbouring page.
- Lean. `control_identified_quadratic` shows the footnote 21
  reparametrisation fails at `α = 2`, `control_distance_terms_break_assumption1`
  that assumption 1 fails once distance terms are present, and
  `control_flat_income_not_feasible` that a flat income valuation leaves a
  negative group term uncompensated.
- Lean, added in the second pass. `control_no_common_index` shows the count of
  contacts orders users only under a common index, and `control_opposite_tastes`
  that assortative sorting needs preferences that favour the same level or a
  common ranking.
