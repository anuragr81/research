# Edelman, Luca and Svirsky (2017), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the text layer of
  the accepted version, by exact normalised containment (LC-2).
- The race gap in the linear probability model with an interaction, the totals,
  rates and shares that follow from Appendix Table 4, the percent reading of
  footnote 11, and the cost of discrimination with its two numbers are Lean
  theorems in the paper's notation (LC-4). Our reading is consistent with the
  source on these points, with the readings ELS-D1 and ELS-D2.

## What could not be verified

- The regressions and the coding of responses. The record checks the reported
  arithmetic, not the data.
- The published version's pagination (ELS-D6).
- Hanson and Hawley (2011), which the source describes in footnote 15 (ELS-Q6).
  Its result bears on contribution 4 and is known here only through this
  citation, so the evidence tag for it is [C] and it cannot support a
  positioning claim until it is read.

## The bearing on Paper A1

Nothing in the source contradicts the A1 model. The design is the closest
field analogue so far of A1's engagement decision. A host decides whether to
engage with a stranger whose only distinguishing information is a group marker
(ELS-Q3), and the marker is penalised by 8 points (ELS-Q1).

How the inference works, as far as the source can say. The authors do not model
the host. They state that the names carry socioeconomic status as well as race
(ELS-Q4) and that the experiment cannot separate the two (ELS-Q5). That is the
opening A1 occupies. A penalty on a name marker is what A1 predicts when the
marker carries information about class and hosts care about class distance.

The candidate contributions of A1, in turn.

1. **Group avoidance from class homophily under inference.** Not in the source,
   and the evidence is mixed.
   - The homophily the source tests is homophily in race, read from the host's
     own race (ELS-Q7), and it finds little of it (ELS-Q8). African-American
     male hosts also accept African-American guests less. That pattern fits a
     penalty driven by inference about the guest that is common to hosts of
     both races, and it does not fit in-group taste in race. It is consistent
     with A1, where the posterior is common and the avoidance depends on the
     host's own class rather than race.
   - A1 predicts that avoidance depends on the observer's own class. The
     source does not observe host class. If the listing's price is read as a
     proxy for it, the penalty does not vary with price (ELS-Q9, interaction
     0.01, standard error 0.03). That reading of price as host class is ours,
     inferred and not checked, and the test has low power. A1 should not cite
     this as support, and it should not ignore it either.
   - No within-group variance enters anywhere in the source.
2. **Budget compatibility as the source of class homophily.** Not in the
   source.
3. **Venue exclusion from clients' inference.** Not in the source. The
   excluding party is the host, one person deciding on one guest. The source
   contrasts Airbnb with platforms where sellers "pre-commit to accept all
   buyers" (PDF p. 4) and proposes Instant Book (PDF p. 18), which are design
   rules that remove the host's screen. No price screens on anything.
4. **A test of the class channel.** Not in the source, which names the
   confound and leaves it (ELS-Q5). The source does not test whether
   information removes the penalty (ELS-Q12, ELS-Q13). The review-history
   result (ELS-Q2, ELS-Q11) is about the host's experience, not about
   information on the guest. The nearest precedent it cites is Hanson and
   Hawley (2011), where the penalty on African-American names falls when the
   message signals higher class (ELS-Q6). If that paper says what the source
   reports, contribution 4's test has already been run in rental housing for
   one class signal, and A1 must cite it and position against it. It should be
   read before any novelty claim on contribution 4.

Other findings A1 should know. Hosts discriminate at a cost of $65 to $100 per
rejection (ELS-Q14), so the penalty survives a material price. The gap is the
same for shared and entire properties (ELS-Q10), while Laouénan and Rathelot
(2022) find the price penalty on minority hosts larger for shared flats
(LR-Q12). The two papers measure different sides of the market, and A1 should
not set them against each other without saying so.

What A1 should cite the source for. The penalty on a pure name marker in host
choice of guests (ELS-Q1, ELS-Q3), the authors' own statement that the marker
carries class (ELS-Q4, ELS-Q5), and the weak homophily in race (ELS-Q8).

## Controls

- LC-2. A fabricated quotation, "When we restrict our analysis to hosts who have
  had an African-American guest in the recent past, discrimination doubles",
  the opposite of ELS-Q2, is not found. A true quotation is not found on a
  neighbouring page.
- Lean. `control_no_interaction` shows the gap cannot vanish among hosts with a
  trait without an offsetting interaction. `control_simple_yes_not_fifty` shows
  the reported rates need the pooled "Yes". `control_sure_replacement_costless`
  shows the cost of discrimination needs a chance of an empty night.
