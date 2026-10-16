# Bohren, Hull and Imas (2024), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the working paper's text layer (LC-2).
- Proposition 1 with eqs. (4) to (6), footnote 26, the vanishing of systemic
  discrimination when the signal distributions coincide, and the two-node
  structure of eqs. (7) and (8) are Lean theorems in the paper's notation, on
  a finite signal space (LC-4). Our reading is consistent with the source on
  these points.

## What could not be verified

- The estimates of the two field experiments and of Appendix B. No data were
  read, Figure 5 is an image, and the text gives three statements of the
  systemic share at the median market (BHI-D2).
- Any published version of the paper (BHI-D1).

## The question put to this source

Is a penalty that runs through an unseen correlated variable, such as class
behind a group marker, classed as systemic? In this framework it is not. A
penalty that arises because the decision maker infers an unseen,
payoff-relevant trait from the group is statistical discrimination, and
statistical discrimination is direct (BHI-Q3, BHI-Q9). It is measured at a
fixed signal, and when the signal carries nothing about the individual
beyond the group, both systemic terms are zero and the whole gap is direct
(`same_signal_all_direct`). A1's group avoidance is of this kind. The
observer reads class from the marker, so the penalty is direct
discrimination at the contact decision.

Systemic discrimination is the other case. The rule is the same for both
groups, and the gap comes from group differences in what the rule is
applied to, at a fixed reference qualification (BHI-Q7, BHI-Q8,
`neutral_rule_all_systemic`). The zip-code rule is the paper's example
(BHI-Q14). So a penalty through a correlated variable is systemic when the
variable is shown and the rule ignores the group, and direct when the
variable is unseen and inferred from the group. Two conditions qualify the
systemic label. The variable's group difference must arise from
discrimination at other nodes within the chosen subsystem, and otherwise the
residual is left unclassified and may be non-discriminatory (BHI-Q5). And
inaccurate beliefs act through the rule, so they are direct (BHI-Q10).

## The bearing on Paper A1

Nothing in the source contradicts the A1 model. The framework is a
classification of gaps and takes action rules in reduced form, so it neither
derives nor rules out any of A1's mechanisms.

1. **Group avoidance from class homophily under inference (contribution 1).**
   Not in the source. There are no preferences over partners, no observer
   types and no variance term. The source gives A1 its classification. The
   avoidance is direct statistical discrimination at the contact node
   (BHI-Q3, BHI-Q9).
2. **Budget compatibility (contribution 2).** Not in the source.
3. **Venue exclusion and price screening (contribution 3).** Not in the
   source. The classification bears on how A1 describes venue exclusion,
   and the following is our inference, not checked in Lean. Exclusion keyed
   to the marker would be direct discrimination at the venue node. Exclusion
   through a price that is the same for both groups would leave a group gap
   at the venue that is systemic in the paper's sense (BHI-Q7) only if the
   class difference behind it is attributed to discrimination elsewhere in
   the chosen subsystem.
4. **A test of the class channel (contribution 4).** Not in the source for
   contact. The source qualifies what A1's test can show. Showing class makes
   class part of the signal, and with class as the reference qualification
   the test measures direct discrimination at a fixed class, as a
   correspondence study does (PDF p. 21, `same_signal_all_direct`). A
   vanishing direct penalty need not mean a vanishing total gap (BHI-Q15),
   because groups with different class distributions still meet different
   rates of contact under a group-blind rule (`control_audit_misses_systemic`).
   Whether that remaining gap counts as discrimination depends on whether
   the class gap itself is attributed to discrimination, which is the
   bad-control concern of BHI-Q2. A1 should say that its test separates the
   class channel from a taste at a fixed class, and that it is silent on the
   gap that remains through class differences.

What A1 should cite. BHI-Q3 and BHI-Q9 for classing marker-based inference as
direct statistical discrimination. BHI-Q7 and BHI-Q14 for the systemic case
of a group-neutral rule on a correlated variable. BHI-Q2 and BHI-Q15 for the
limit of conditioning on shown class.

## Controls

- LC-2. A fabricated quotation, "any total discrimination that arises from a
  race-neutral action rule is direct, as the systemic component is zero by
  definition.", the opposite of BHI-Q7, is not found. A true quotation is not
  found on a neighbouring page.
- Lean. `control_pairing_matters` shows that eq. (4) needs its particular
  pairing of the direct and systemic terms. `control_audit_misses_systemic`
  shows that total discrimination equals direct discrimination only when the
  signal distributions coincide.

## Suite status

The coordinator adds `BohrenEtAl2024` to the build roots. Until then LC-4
fails its build-root check. With the module compiled into the scratch
directory and that directory on `LEAN_PATH`, the axiom audit passes for all
eight cited theorems, which depend only on `propext`, `Classical.choice` and
`Quot.sound`.
