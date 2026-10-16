# Kraus, Torrez, Park and Ghayebi (2019), notes

## What was verified

- Fifteen quotations are found by exact normalised containment in the cached
  PubMed Central text, which is one page (LC-2). Each row gives its section
  heading because the text has no page numbers (KT-D1).
- The estimating model is formalised in Lean (LC-4). This covers the
  proportion correct and its chance line, the z-score composite, the profile
  correlation, and every printed number that follows from other printed
  numbers (effect sizes from `t`, `t` from correlations, the study 5
  difference in differences). The coverage table in `CLAIMS.md` lists every
  result.
- Two pieces are our reading and are not attributed to the authors. These
  are the race-only perceiver (`race_judge_gain`) and hypothesis 3 as a
  monotone proxy (`proxy_bias`).
- Our reading is consistent with the main text on these points, with four
  disagreements between printed numbers (KT-D8 to KT-D11).

## What could not be verified

- The SI Appendix is not cached, so the perceiver-education correlation
  (KT-Q11), the self-referencing correlations (KT-Q12) and the stimulus
  details are known only from the main text's description (KT-D2).
- The base rate of college graduates among the study 1 stimuli is not
  reported, so the validity of the 50% chance line is not checked (KT-D3).
- `P` values and estimates from data (see the coverage table).

## The bearing on Paper A1

**The puzzle.** The paper does not address friending bias or diversity. Two
of its findings bear on the puzzle. From the same seven words, race is read
more accurately than class, 64.05% against 55.49% (KT-Q4), and the authors
attribute the gap to the concealability of class (KT-Q5). In a first
encounter the racial marker is therefore available before class is. Our own
arithmetic on the paper's metric adds one point (`race_judge_gain`). With a
balanced set of speakers, a perceiver who reads class from race alone gains
`2 w (1 − w)(p₁ − p₂)` over chance, which is largest at equal group shares.
Race is then the most useful class cue in the most mixed population. That is
our reading, not the paper's, and it holds only under the balance condition.

**Budget compatibility.** Not in the source. The origins it offers for
preferring a higher-class or similar person are cultural matching and
similarity (KT-Q16, KT-Q9) and class taken as a proxy for merit
(hypothesis 3, `proxy_bias`). Nothing in the paper bears on cost,
affordability or shared consumption.

**Avoidance that depends on the observer's class.** Partial and one-sided.
Perceivers' class judgements correlate with their own class (KT-Q12), more
educated perceivers read college-educated speakers better (KT-Q11), and
perceivers with hiring experience, who "tend to be higher in social class",
judge higher-class candidates more similar to themselves (KT-Q9). Both
results on the perceiver are from the unread SI Appendix. Direction (i),
avoidance running both ways, is not testable here because lower-class
perceivers are not studied apart. Direction (ii), a low-class member of the
high group preferring outsiders, has nothing in the source.

**The penalty vanishing when class is shown.** No statement. No study shows
class and then measures a group penalty. Study 4's speech against transcript
contrast varies how much of the class cue is shown, not the group marker
(KT-Q7).

**The shadow market.** Present, and in the contracting channel rather than
the contact channel. The authors write that hiring managers "may
intentionally seek out these cues in applicants in ways that would be illegal
if racial or gender cues were utilized" (KT-Q15). That is a signal
substituting for a banned criterion. They add that class cues "may be used as
a means to perpetuate racial inequality, given the overlap between social
class and race" (KT-Q10), which is a group-neutral criterion correlated with
the group. And they note that standardising interviews "would not eliminate
the ways that bias enters into hiring practices at other stages of the
process" (KT-Q17), so a rule on the formal stage leaves the informal stages
unreached. The setting is preinterview small talk inside hiring. A1 can cite
the mechanism. It cannot cite the paper for venues or contact.

**Premises of A1 the source does not share.**
- Class is partly seen. It is read from the individual's own pronunciation,
  net of race (KT-Q6), weakly but above chance (KT-Q2, KT-Q3), and the
  study 1 effect is about half the size its printed `D` suggests (KT-D8).
  A1's "unseen" is an idealisation of this.
- The cue is evidence about the individual, not a group marker under group
  sufficiency. Class read from the group is not studied (KT-Q10).
- Preferences in study 5 are vertical in the sense that higher class is
  judged better on every dimension (KT-Q9). With a perceiver pool skewed to
  high class, the data cannot separate a vertical preference from A1's
  squared distance.

## Controls

- LC-2. A fabricated quotation, "social class judgments were significantly
  more accurate relative to speaker race", the opposite of KT-Q4, is not
  found. A true quotation is not found on a neighbouring page (an empty page,
  since the text is one page).
- Lean. Each main result has a control: `control_majority_beats_half` (the 50%
  line needs balance), `control_no_gap_chance` (the race-only perceiver needs
  a gap), `control_raw_average_not_centered` (the composite needs z scores),
  `control_corr_neg_scale` (scale invariance needs an increasing recoding),
  `control_no_proxy` (the hiring bias needs judgements that depend on class).
- Axiom audit by hand against the scratch olean, all 34 declared theorems
  checked, none beyond propext, Classical.choice and Quot.sound.
