# Kraus, Park and Tan (2017), notes

## What was verified

- Fourteen quotations are found on the PDF pages they cite in the text layer
  of the accepted manuscript (LC-2).
- The quartile and half chance benchmark, and our formal reading of the
  sorting argument, are Lean theorems (LC-4). The sorting argument is stated
  in words in the manuscript. The formalisation is of our reading of its
  structure, with A1's squared-distance utility standing in for "similarity
  enhances liking", and is not attributed to the authors.
- Our reading is consistent with the accepted manuscript on these points.

## What could not be verified

- The published version (KPT-D1), the supplementary materials for the
  seven-word study, and every cited study, which is known here only through
  this manuscript [C].
- The Figure 2 values, read from the bars to about a percentage point
  (KPT-D4).

## The bearing on Paper A1

**How accurately class is read.** Weakly, with correlations of .22 to .28
between perceived and actual class from 60 s of behaviour, 20 Facebook
photographs or seven words (KPT-Q2, KPT-Q4). By quartiles, observers are
right about 35% to 36.5% of the time against 25%, and by halves about 60% to
62% against 50% (Figure 2, KPT-Q6). The authors call this "preliminary yet
promising" (p. 12).

**A1's premise that class is unseen and inferred from group markers.** The
manuscript's premise is the opposite in emphasis. Class is "signaled and
perceived accurately" (p. 6) from the person's own behaviour, speech and
possessions (KPT-Q1). Two of its numbers bear directly on A1.
- The accuracy survives controls for the target's race (KPT-Q2, KPT-Q4). The
  class read here is read from an individual cue net of race, not inferred
  from the group.
- The accuracy is low. A correlation of about .2 leaves most of the variance
  in class unread, which is consistent with A1 treating class as unseen to a
  first approximation and the group as the remaining evidence. The manuscript
  does not draw that conclusion.

**Is a racial signal read as a class signal?** Not shown. The one cited study
that bundles them, Bergeron and Zanna, uses an indigenous Peruvian writer as
the lower-class condition (KPT-Q9), so its result cannot separate ethnicity
from class. The interplay of class and race perception is named as future
research (KPT-Q10).

**Contributions 1 to 4.**
1. Group avoidance from class homophily. The micro-step is here in words, the
   group step is not. Accurate class signals create "barriers for
   relationship formation across class boundaries" through similarity
   (KPT-Q7), and the cited Côté et al. result has affiliation following the
   observer's own class (KPT-Q8), which is the observer-type dependence of
   contribution 1. Both are about class read from the person, not inferred
   from a group, and neither has a variance term. A1 can cite KPT-Q8 [C] as
   evidence for the homophily it assumes. It cannot cite the manuscript for
   group avoidance.
2. Budget compatibility. Not present. The reason given for homophily is
   similarity and liking (KPT-Q7), and for strategic sharing it is the
   expected return from a partner with resources (p. 21), a vertical motive.
3. Venue exclusion. Not present. Rivera's elite firms screening on club
   memberships (p. 15) and school uniforms concealing class (KPT-Q14) are the
   nearest points, both cited, neither modelled.
4. A test of the class channel. Not present as a test of the group penalty.
   The cited Nishi et al. experiment varies whether resources are visible and
   finds sharing directed to the rich only when they are (KPT-Q11). That is a
   test of what showing type does to behaviour, not of whether a group
   penalty vanishes when class is shown.

**Premises that differ.** In A1 the marker is fixed and passing is a choice
of marker. Here the class signal itself can be changed by aspiring
individuals (KPT-Q13), so the manipulable object is the class cue, not the
group.

## Controls

- LC-2. A fabricated quotation, "these associations disappeared after
  accounting for the race and gender of participants", the opposite of
  KPT-Q2, is not found. A true quotation is not found on a neighbouring page.
- Lean. `control_nonuniform_categories` shows the 25% benchmark needs
  sample-quantile categories. `control_vertical_common_ranking` shows the
  observer-dependent ranking fails under a vertical utility.
- The suite's LC-4 build-root check fails until the coordinator adds
  `KrausEtAl2017` to `lakefile.toml`. The axiom audit passed against an
  olean built in the reader's scratch directory.
