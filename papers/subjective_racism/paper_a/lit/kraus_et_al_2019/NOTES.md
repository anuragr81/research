# Kraus, Torrez, Park and Ghayebi (2019), notes

## What was verified

- Fourteen quotations are found by exact normalised containment in the cached
  PubMed Central text, which is one page (LC-2). Each row gives its section
  heading because the text has no page numbers (KT-D1).
- The accuracy metric of study 1 and two facts about it are Lean theorems
  (LC-4). The theorems formalise our reading of the metric. The paper states
  the metric and the 50% benchmark and does not state the theorems.
- Our reading is consistent with the main text on these points.

## What could not be verified

- The SI Appendix is not cached, so the perceiver-education correlation
  (KT-Q11), the self-referencing correlations (KT-Q12) and the stimulus
  details are known only from the main text's description (KT-D2).
- The base rate of college graduates among the study 1 stimuli is not
  reported, so the validity of the 50% chance line is not checked (KT-D3).
- The sample counts of studies 1 and 4 differ between the results and the
  methods (KT-D4, KT-D5).

## The bearing on Paper A1

**How accurately class is read.** Weakly. From seven isolated words class is
judged correctly 55.49% of the time against a 50% line (KT-Q3). From 90 s of
speech the correlation between perceived and actual class is 0.21 (KT-Q7). In
study 5 the high and low candidates differ by under one rung of a ten-rung
ladder (KT-Q8). The authors define accuracy as any tendency above chance and
say it "falls well short of error-free" (KT-Q2).

**A1's premise that class is unseen and inferred from group markers.** The
source supports half of it and does not test the other half.
- Supported. Class is read less accurately than race from the same seven
  words, 55.49% against 64.05% (KT-Q4), and the authors attribute the gap to
  the concealability of class (KT-Q5). That is the asymmetry A1 assumes, a
  visible marker and a concealed class.
- Not tested. The cue studied is pronunciation, which is evidence about the
  individual speaker in the sense of Phelps's test score, not a group marker
  under group sufficiency. With speaker race as a control, pronunciation
  still predicts perceived class, and race has no significant effect
  (KT-Q6). The paper therefore shows class read from an individual cue net of
  race. It does not show class read from the group.
- A caution for any use of the accuracy numbers. A perceiver who answers from
  race alone scores above 50% whenever graduation rates differ by race
  (`race_judge_above_chance`, `race_judge_gain`). Above-chance accuracy alone
  does not separate a class cue from group inference. Only the controls of
  study 2 do, and study 1 has no such control.

**Is a racial signal read as a class signal?** Not in this paper. Race enters
only as a control (KT-Q6) and as a judgement made in parallel with class
(KT-Q4). The link from race to class is named as future research in both
directions (KT-Q10).

**Contributions 1 to 4.**
1. Group avoidance from class homophily. Not present. The one hint of
   observer-type dependence is that accuracy and perceived class rise with
   the perceiver's own class (KT-Q11, KT-Q12), both from the unread SI
   Appendix, and study 5's perceivers rate higher-class candidates as "more
   similar" to themselves (KT-Q9). None of this is a preference over groups,
   and there is no variance term.
2. Budget compatibility. Not present. Study 5 is hiring, where the authors
   read class as a proxy for merit and fit, not contact.
3. Venue exclusion. Not present.
4. A test of the class channel. Not present. No study shows class and then
   measures a group penalty. Study 4's speech against transcript contrast
   varies how much of the class cue is shown, not the group marker.

**What A1 should cite it for.** The accuracy numbers of KT-Q3, KT-Q4 and KT-Q7
as evidence that class is read only weakly from brief cues and less
accurately than race, with the caution above.

## Controls

- LC-2. A fabricated quotation, "social class judgments were significantly
  more accurate relative to speaker race", the opposite of KT-Q4, is not
  found. A true quotation is not found on a neighbouring page (an empty page,
  since the text is one page).
- Lean. `control_majority_beats_half` shows the 50% line is chance only under
  balance. `control_no_gap_chance` shows the race-only perceiver needs a gap
  in graduation rates to beat chance.
- The suite's LC-4 build-root check fails until the coordinator adds
  `KrausEtAl2019` to `lakefile.toml`. The axiom audit passed against an
  olean built in the reader's scratch directory.
