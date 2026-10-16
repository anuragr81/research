# Kraus, Park and Tan (2017), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the text layer
  of the accepted manuscript (LC-2).
- The quartile and half chance benchmark is the manuscript's own metric and is
  formalised in general (`blindHit_uniform`).
- Every other Lean theorem formalises our reading of a claim the manuscript
  makes in words. These cover sorting, own-class affiliation, accuracy and
  inequality, stereotyping, strategic sharing and visible gaps. The
  squared-distance form in the sorting theorems is A1's utility standing in
  for "similarity enhances liking", and is not attributed to the authors. The
  coverage table in `CLAIMS.md` lists every prediction and statistic and says
  which are not formalised.
- Our reading is consistent with the accepted manuscript on these points.

## What could not be verified

- The published version (KPT-D1), the supplementary materials for the
  seven-word study, and every cited study, which is known here only through
  this manuscript [C].
- The Figure 2 values, read from the bars to about a percentage point
  (KPT-D4), and the `χ²` and `p` values.

## The bearing on Paper A1

**The puzzle.** The manuscript offers a rival account of class homophily
without groups. Class signals perceived directly "create barriers for
relationship formation across class boundaries" (KPT-Q7), so friending bias
would follow from reading class off the person. It says nothing on racial
diversity. The interplay of class and race perception is named as future
research (KPT-Q10). The one cited study that combines them, Bergeron and
Zanna, uses an indigenous Peruvian writer as its lower-class condition, so it
cannot separate ethnicity from class (KPT-Q9). The manuscript conjectures
that class is harder to discern where inequality is lower (KPT-Q12). In our
signal-plus-noise reading, accuracy rises with the spread of class
(`accuracy_rises_with_spread`). If racially diverse settings have larger class
gaps between groups, the same logic makes a group marker more informative
there. That extension is ours.

**Budget compatibility.** The manuscript's origin of homophily is similarity,
"similarity enhances liking and relationship formation" (p. 14, KPT-Q7). The
cited Côté et al. signals are spending plans and budget constraints, a summer
in Europe against a summer job "to help with family bills" (KPT-Q8). These
are the activities budget compatibility is about, though the manuscript reads
them as similarity. Its motive for class-directed sharing is a vertical
return, few "direct economic benefits for the sharer" (KPT-Q16,
`visible_share_rich`), which is not budget compatibility.

**Avoidance that depends on the observer's class.**
- Direction (i), avoidance runs both ways. Supported by cited evidence.
  Upper-class participants affiliate more with upper-class signals and
  lower-class participants with lower-class signals (KPT-Q8,
  `own_class_ranking`). Under an accurate signal the sorting is symmetric
  (`sorting_symmetric`). A vertical preference would give a common ranking
  instead (`control_vertical_common_ranking`).
- Direction (ii), a low-class member of the high group prefers outsiders. Not
  stated. The nearest evidence is that first-generation students feel
  alienated in universities populated by highly educated families (KPT-Q8),
  which is a lower-class individual in a high-class setting. The group there
  is the class itself, not a racial group, so it neither supports nor
  contradicts direction (ii) as A1 states it.

**The penalty vanishing when class is shown.** No statement on a group
penalty. The cited Nishi et al. experiment has the converse property. With
resources hidden, participants shared "regardless of the resources of their
partners", and with resources visible they shared with the rich (KPT-Q11,
`invisible_share_indifferent`, `visible_share_rich`). There is no marker in
that design, so it says nothing about a group penalty.

**The shadow market.** Two passages bear on it, both about class signals
rather than group.
- Top firms "rely on social class information in extracurricular activities
  and club memberships" to find higher-class applicants (KPT-Q15). That is
  screening on correlated social information, which is the kind of variable
  the shadow market trades.
- School uniforms aim to conceal class, and the authors ask whether "the
  myriad and subtle ways in which behaviors signal social class" make the
  policy ineffective (KPT-Q14). That is the shadow-market question, whether a
  rule on one channel moves the information to others. The manuscript asks it
  and does not answer it.
- Nothing on law, on venues as intermediaries, or on information about where
  to go acquiring value.

**Premises of A1 the source does not share.**
- Class is "signaled and perceived accurately" (p. 6) from the person's own
  behaviour, speech and possessions (KPT-Q1), with accuracy that survives
  controls for race (KPT-Q2, KPT-Q4). In A1 class is unseen and read from the
  group. The accuracy is low, correlations near .2 to .28, which leaves room
  for A1's idealisation.
- Class signals can be changed by aspiring individuals (KPT-Q13,
  `signal_upgrade_raises_competence`). In A1 the marker is fixed and passing
  is a choice of marker.
- Preferences are horizontal for sorting (KPT-Q7, KPT-Q8) but vertical for
  stereotypes and sharing (KPT-Q16). A1 has only the horizontal form.

## Controls

- LC-2. A fabricated quotation, "these associations disappeared after
  accounting for the race and gender of participants", the opposite of
  KPT-Q2, is not found. A true quotation is not found on a neighbouring page.
- Lean. Each main result has a control. `control_nonuniform_categories` shows
  the 25% benchmark needs sample quantiles. `control_vertical_common_ranking`
  shows own-class ranking fails under a vertical utility.
  `control_noiseless_flat` shows the link of accuracy to inequality needs
  noise. `control_flat_stereotype` shows the gain from an upgraded signal
  needs competence to depend on class.
- Axiom audit by hand against the scratch olean, all 16 declared theorems
  checked, none beyond propext, Classical.choice and Quot.sound.
