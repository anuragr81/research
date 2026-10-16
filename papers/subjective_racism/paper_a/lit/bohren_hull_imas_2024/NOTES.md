# Bohren, Hull and Imas (2024), notes

## What was verified

- Twenty-one quotations are found on the PDF pages they cite, by exact
  normalised containment in the working paper's text layer (LC-2). Two use
  an elision where the text layer breaks a hyphenated word across a line
  (BHI-Q17, BHI-Q19).
- The framework of Sections 3 and 4 is implemented in
  `lean/mathlib/BohrenEtAl2024.lean`. Definitions 1 to 3, Proposition 1 and
  eqs. (3) to (6) hold on a finite signal space at one qualification level.
  The two-node subsystem of eqs. (7) and (8), the experimental Iterated
  Audit, footnote 44, the printed arithmetic of Table 1 and Table 3, and the
  screening example of Appendix B.1.1 are Lean theorems. The coverage table
  in `CLAIMS.md` gives the status of every numbered result. Our reading is
  consistent with the source on these points, except for the readings
  recorded as BHI-D2 to BHI-D9.

## What could not be verified

- The estimates of the two field experiments and of Appendix B. No data were
  read, Figure 5 is an image, and the text gives three statements of the
  systemic share at the median market (BHI-D2).
- The measure-theoretic general case and the averages of Appendix B.1.1.
- Any published version of the paper (BHI-D1).

## How the source classes a penalty through an unseen correlated variable

A penalty that arises because the decision maker infers an unseen,
payoff-relevant trait from the group is statistical discrimination, and
statistical discrimination is direct (BHI-Q3, BHI-Q9). Appendix B states the
sharper version. With productivity as the reference qualification, a group
difference in the prior distribution of productivity gives only direct
discrimination, while a group difference in the precision of the signal
gives both direct and systemic discrimination (BHI-Q21). A1's group penalty
comes from group differences in the class distribution read from the
marker, so in this framework it is direct discrimination at the contact
decision (`same_signal_all_direct`).

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1. One claim of
the predecessor is qualified below under the shadow market.

**The puzzle.** The source is silent on friendship and on diversity. It
supplies a vocabulary for the puzzle that A1 may use, and the following
mapping is our inference, not checked in Lean. Friending bias is measured
conditional on exposure, so it plays the role of discrimination at the
friending decision given who is met, while exposure is set at an earlier
node, where people go. Which part counts as systemic depends on the chosen
subsystem and reference qualification (BHI-Q5).

**Budget compatibility.** Nothing in the source bears on cost, affordability
or shared consumption, or on the origin of homophily. Action rules are
reduced form (PDF p. 12).

**Avoidance that depends on the observer's class.** Not in the source. There
are two groups and one action rule per group and node, with no heterogeneity
among decision makers by their own group or class, so the source neither
supports nor limits directions (i) and (ii).

**The penalty vanishing when class is shown.** The source has no statement
for contact. Its framework places the property. Showing class puts class in
the signal, and with class as the reference qualification the measure at a
fixed class is direct discrimination, as in a correspondence study (PDF
p. 21, `same_signal_all_direct`). A1's property is that this direct
discrimination is zero once class is shown. Two qualifications follow from
the source.
- A vanishing direct penalty need not mean a vanishing total gap (BHI-Q15).
  Groups with different class distributions still differ in contact under a
  rule that ignores the group (`control_audit_misses_systemic`), and whether
  that gap counts as discrimination depends on whether the class gap is
  itself attributed to discrimination, the bad-control concern (BHI-Q2).
- Inaccurate beliefs work through the action rule and so through direct
  discrimination (BHI-Q10). The property does not depend on beliefs being
  accurate.

**The shadow market.** The source bears on it directly.
- When the law forbids a rule that uses the group, a rule that uses
  correlated markers has zero direct discrimination by definition, and any
  group gap it produces is systemic (BHI-Q7, `neutral_rule_all_systemic`).
  The zip-code rule is the paper's example (BHI-Q14). In the source's terms
  the predecessor's shadow market converts direct discrimination into
  systemic discrimination.
- The classification turns on the reference qualification. If the venue's
  instruments (price, dress code, atmosphere) select on signals of class that
  are distributed alike in the two groups at the same class, then with class
  as the reference qualification there is no discrimination at all
  (`neutral_rule_same_signal_no_gap`). This is the predecessor's remark that
  "racial correlation is a consequence rather than an input" (main
  manuscript, line 1636). If the instruments read markers that differ by
  group at the same class, such as an accent or a dress code correlated with
  race beyond class, there is systemic discrimination at a fixed class
  (`control_audit_misses_systemic`). With no reference qualification, the
  whole group gap is systemic.
- Choosing which signals are read is itself a node of the system (BHI-Q16),
  and anticipated treatment at one node shapes choices at another (BHI-Q17).
  A venue's choice of atmosphere and clients' choice of where to go fit
  these two node types. The source does not model a market for that
  information or its value.
- The source records the expectation that discrimination survives a
  colour-blind mandate through neutral channels (BHI-Q19). That is the
  mechanism the predecessor attributes to legal strictness, stated as a
  historical argument about organisations, not as a result.
- A ban on using the group also forbids the "reverse" direct discrimination
  that would offset signal inflation (BHI-Q18, `signal_inflation_offset`).
  Under a strict ban a gap from inflated markers cannot be corrected inside
  the rule. This bears on why the return to shadow information could rise
  with legal strictness, and the step is our inference.
- The source limits one claim of the predecessor. The disparate impact
  doctrine reaches some facially neutral practices (BHI-Q20, the Griggs case
  on PDF pp. 21 and 39). The law therefore reaches part of the channel the
  predecessor calls unreached, at least in employment. A1 should say which
  settings of contact the doctrine does not cover, rather than say that the
  law does not reach group-neutral rules.

**Premises of A1 the source does not share.**
- Action rules are reduced form. Nothing is derived from preferences or
  equilibrium, so the source cannot speak to why a rule takes the form A1
  derives.
- Group membership is treated as separately manipulable from the signal
  (footnote 14, PDF p. 13). In A1 the group is known only through the
  marker, which is part of what is observed.
- Discrimination is defined relative to a chosen subsystem and reference
  qualification, so the same gap can be direct, systemic or unclassified.
  A1 has one decision and does not make this choice.

## Controls

- LC-2. A fabricated quotation, "any total discrimination that arises from a
  race-neutral action rule is direct, as the systemic component is zero by
  definition.", the opposite of BHI-Q7, is not found. A true quotation is not
  found on a neighbouring page.
- Lean. `control_pairing_matters` (eq. (4) needs its pairing),
  `control_audit_misses_systemic` (total equals direct only when signal
  distributions coincide), `control_fn44_no_direct` (the experience gap needs
  first-round direct discrimination) and `control_equal_precision_no_systemic`
  (Appendix B.1.1 needs unequal precisions).

## Suite status

The suite ends with 11 checks and 0 failures when the edited module is
compiled into the scratch directory and the axiom audit is pointed at it,
with all 20 cited theorems depending only on `propext`, `Classical.choice`
and `Quot.sound`. Until the coordinator rebuilds the library, the audit run
through `lake env` finds the older build.
