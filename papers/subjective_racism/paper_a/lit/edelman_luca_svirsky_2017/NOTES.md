# Edelman, Luca and Svirsky (2017), notes

## What was verified

- Twenty-two quotations are found on the PDF pages they cite in the text layer of
  the accepted version, by exact normalised containment (LC-2).
- The linear probability model with its implied coefficients, every number in
  the text and footnotes that follows from other printed numbers, and our
  reading of the remedy of PDF p. 18 are Lean theorems in the paper's notation
  (LC-4). The coverage table in `CLAIMS.md` lists what is general and what is a
  special case. Our reading is consistent with the source on these points, with
  the readings ELS-D1 and ELS-D2. Two printed numbers do not follow from the
  others (ELS-D8, ELS-D9).

## What could not be verified

- The regressions and the coding of responses. The record checks the reported
  arithmetic, not the data.
- The published version's pagination (ELS-D6).
- Hanson and Hawley (2011), which the source describes in footnote 15 (ELS-Q6).
  Its result bears on contribution 4 and is known here only through this
  citation, so the evidence tag for it is [C] and it cannot support a
  positioning claim until it is read.

## The bearing on Paper A1

Revised on 16 October 2026 against the corrected account of A1. Nothing in the
source contradicts that account. The source supports the shadow-market premise
more directly than any other read so far, and it does not share A1's premise
of a posterior common to all observers.

### The puzzle

The penalty does not vary with the share of African-American residents in the
listing's census tract (ELS-Q22, interaction 0.02, standard error 0.08), nor
with listings per tract, and it is present in every city (PDF p. 11). The
estimate is imprecise and the outcome is acceptance of a stranger, not
friending, so it neither supports nor rules out a penalty that rises with
diversity. The finding nearest to contact is that the penalty disappears among
hosts who have had an African-American guest (ELS-Q2), which the authors read
as hosts "using different statistical models" (ELS-Q11). Experience of the
other group goes with no penalty here. Whether that is exposure or selection
of hosts the design cannot say.

### Budget compatibility

The source has no account of the origin of homophily. The homophily it tests is
in race, read from the host's own race (ELS-Q7), and it finds little (ELS-Q8).
It names class through the marker. The names carry socioeconomic status
(ELS-Q4), and the design cannot separate race from class (ELS-Q5). On shared
consumption, the penalty is the same whether host and guest share the property
or not (ELS-Q10). If sharing a property is the kind of shared activity budget
compatibility concerns, A1 cannot cite this source for a cost that rises with
it.

### Avoidance that depends on the observer's class

Host class is not observed, so neither direction can be tested. Three findings
bear on them.

- Avoidance runs both ways only in part. White hosts accept White guests more,
  and African-American female hosts accept African-American female guests
  most, but African-American male hosts accept White guests more (Table 4,
  ELS-Q8). A penalty that hosts of both races apply is what a common inference
  about the guest predicts.
- If the listing's price is read as a proxy for the host's class, the penalty
  does not vary with it (ELS-Q9). That reading of price is ours and the test
  is weak.
- Nothing in the source bears on direction (ii).

### The penalty vanishing when class is shown

The source has no such statement. It names the confound of race and class and
leaves it (ELS-Q4, ELS-Q5), says that reviews "may reduce" a statistical penalty
without testing it (ELS-Q12, ELS-Q13), and reports Hanson and Hawley (2011),
in which the penalty on African-American names falls when the message signals
higher class (ELS-Q6). That paper is known here only through this citation
([C]) and is the nearest statement found of the property A1 wants to link. It
should be read.

### The shadow market

The source documents the setting the predecessor describes, and the remedies it
proposes are the design rules that decide whether the contact channel can use a
marker.

- The law reaches the contracting channel and not the platform. Anti-discrimination
  law binds the large landlord and likely not the small Airbnb host (ELS-Q16).
  A hotel that commits to serve all comers cannot use a name (ELS-Q17), and
  platforms whose sellers pre-commit offer "little scope" for discrimination
  (ELS-Q19).
- The platform decides which markers flow. The remedies are to conceal names
  or to expand Instant Book (ELS-Q18), and the authors conclude that platforms
  "may be providing too much information" (ELS-Q20), the opposite emphasis to
  Laouénan and Rathelot's call for more information (LR-Q17). Airbnb kept names
  and photos visible (ELS-Q21).
- Our reading in Lean. Concealing the name removes the gap under every
  acceptance rule exactly when nothing else displayed differs between the two
  accounts (`gap_zero_all_rules_iff`). Any displayed difference, a photo or a
  writing style, leaves room for a gap (`control_displayed_marker`). This is
  the condition under which a signal can substitute for the hidden marker. The
  source does not observe what happens after concealment.
- The marker itself is a correlated variable. A name signals class as well as
  race (ELS-Q4), so a host who screens on names screens on a variable
  correlated with both.

### Premises of A1 the source does not share

- A posterior common to all observers. Hosts with an African-American guest in
  their history do not discriminate (ELS-Q2), which the authors read as
  different hosts using different statistical models (ELS-Q11). A1's group
  sufficiency gives every observer the same posterior from the marker.
- Discrimination at a cost. Hosts forgo $65 to $100 per rejection (ELS-Q14).
  A1's engagement threshold allows a cost, but A1 should say that its avoidance
  survives a material price.
- The source treats the host's decision as one-sided screening of a guest. A1
  should say whether its engagement is one-sided or needs both parties.

## Controls

- LC-2. A fabricated quotation, "When we restrict our analysis to hosts who have
  had an African-American guest in the recent past, discrimination doubles",
  the opposite of ELS-Q2, is not found. A true quotation is not found on a
  neighbouring page.
- Lean. `control_no_interaction` shows the gap cannot vanish among hosts with a
  trait without an offsetting interaction. `control_simple_yes_not_fifty` shows
  the reported rates need the pooled "Yes". `control_sure_replacement_costless`
  shows the cost of discrimination needs a chance of an empty night.
  `control_displayed_marker` shows that concealing the name fails to remove the
  gap when any other displayed variable differs.
