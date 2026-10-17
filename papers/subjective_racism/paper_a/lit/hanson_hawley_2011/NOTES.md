# Hanson and Hawley (2011), notes

## What was verified

- Twenty-four quotations are found on the PDF pages they cite in the cached
  text, by exact normalised containment (LC-2).
- Every number of Tables 5 and 6 that follows from other printed numbers, three
  rows of Table 9, the 0.222 of Table 7 and the tipping band of note 26 are Lean
  theorems (LC-4). Two rows of Table 6 and the counts of Table 8 are
  inconsistent (HH-D2 to HH-D5).
- The identity behind the landlord-level measure is a Lean theorem
  (`net_eq_rate_diff`). It explains why Table 6 can show a significant gap in
  pooled reply rates beside an insignificant net share (HH-D1).
- **The predictor of quality and the threshold for a reply are our reading.**
  The paper reads its results as statistical discrimination in words (HH-Q10)
  and states no model.

## What could not be verified

- The probits, the tests of proportions and every p-value.
- The published article, which may differ from the working paper (HH-D7).
- The studies the paper cites, among them Ahmed, Andersson and Hammarstedt
  (2010), Bosch, Carnero and Farre (2010), Ewens, Tomlin and Wang (2009) and
  Card, Mas and Rothstein (2008). HH-Q20 and HH-Q21 are [C] for those studies.

## The bearing on Paper A1

Nothing in the source contradicts the corrected account. One finding limits the
link A1 draws, and it comes first. The penalty vanishes in one measure only. The
net share of discriminating landlords between two high-class e-mails is 1.77
percent and not significant (HH-Q7). Over all e-mails the race gap between
high-class e-mails is 4.01 points and significant, and the probits give a
significant race effect on a high-class e-mail (HH-Q8, HH-Q9, HH-D1). A
high-class African American name gets fewer replies than a low-class White name
(`table6_gaps`). The authors' "non-existent" (HH-Q1) is their reading of the
first measure.

### The puzzle

The source bears on the puzzle weakly. Discrimination is largest where a
neighbourhood is 80 to 95 percent White and smaller where it is over 95 percent
White (HH-Q15, `table8_band_largest`). That runs in the direction of Chetty et
al.'s gradient, with more discrimination where there is some diversity than
where there is none. Three things limit the parallel. The numbers of Table 8 are
inconsistent (HH-D4, HH-D5). The interaction with the percentage White is zero
(Table 10). The authors read the band as landlords keeping White tenants from
leaving (HH-Q14), not as class inference.

### Budget compatibility

Not in the source. The class the e-mails signal includes good references and a
recent credit report (HH-Q5), and the authors mean a better or worse tenant
(HH-Q6). That is the landlord's vertical question of whether a tenant can pay,
not compatibility of budgets between peers. A1 should not cite the source for
budget compatibility.

### Avoidance that depends on the observer's own class

The source cannot speak to either direction. The landlord's own race and class
are not observed. What the source does show is avoidance by proxy. Landlords of
apartments and condominiums discriminate, and landlords of single-family homes
do not (`table9_nets`). The authors read this as landlords bearing the cost of
other tenants' displeasure (HH-Q12, HH-Q13). The avoidance is that of the
tenants who will live in contact, carried out by the landlord.

### The penalty vanishing when class is shown

The source is the nearest empirical statement of the property in a setting
close to contact, and A1 should cite it with its scope.

- **The statement.** A positive class signal removes the net share of
  discriminating landlords (HH-Q1, HH-Q7, HH-Q11, HH-Q24). The authors say no
  earlier study found this (HH-Q2), and report two in which positive
  information did not close the gap (HH-Q20, HH-Q21).
- **The signal is partial.** The e-mail is an attempt to move an inference
  (HH-Q22), not shown class. In a linear predictor the group gap is
  `(1 − k)(μW − μA)` and vanishes only at `k = 1` (`gap_common_weight`,
  `gap_vanishes_full_signal`, `control_partial_signal_gap`).
- **A threshold reconciles the two measures.** A reply is a threshold on the
  predicted quality. A high enough signal puts both names over it although the
  predictions still differ (`threshold_equal_above`, `high_signal_clears`),
  and a low signal leaves the threshold between them
  (`control_threshold_between`). This reading is ours. It fits a small net share
  among landlords who see two high-class e-mails beside a pooled gap that
  persists. A1's engagement rule is also a threshold, `E[d] ≤ Δ*`, so A1 can
  state its property at the margin of engagement as well as in the limit of
  shown class.
- **The signal moves one group.** The high-class e-mail raises the reply rate
  for African American names by 3.62 points and not for White names
  (`table6_class_moves_one_group`). In normal updating the weight on a signal
  is larger for the group with the wider prior, the Phelps class of model
  (`BertrandMullainathan2004.phelps_larger_update`). Bertrand and Mullainathan
  found the reverse for credentials in hiring (BM-Q2). Under A1's σ² term the
  group with the wider class distribution gains more from a shown class. That
  link is our inference. A ceiling on White replies would give the same
  pattern.

### The shadow market

The source bears on three of the items in the corrected account.

- **The venue operator screens for its clients' contact.** Landlords who rent
  units in one building discriminate, and the authors give the reason that
  these landlords bear the cost if other tenants are displeased (HH-Q12,
  HH-Q13, `table9_nets`). The landlord is bound by the law (HH-Q19) and the
  tenants' contact preferences reach the decision through the landlord. This
  is the predecessor's venue that aggregates its clients' preferences, in a
  housing market.
- **Discrimination sits where it leaves no record.** The venue warns that
  stating a discriminatory preference is illegal (HH-Q18), and both kinds of
  discrimination are illegal (HH-Q19). The difference lies in whether a
  landlord replies. Once a landlord replies, the reply says the unit is
  available and invites a viewing at the same rate for both names (HH-Q16).
  This reading is ours. The unanswered e-mail is the shadow channel and the
  written reply is the formal one.
- **A legal signal stands in for a banned one.** Writing style is legal to
  screen on and moves the reply rate for African American names (HH-Q3,
  `table6_class_moves_one_group`). That is the substitution of KT-Q15 seen
  from the applicant's side.

One finding limits the account. Landlords behind Craigslist's anonymous
address do not discriminate more (HH-Q17). Less exposure does not raise
discrimination here, although the authors note that landlords may select into
anonymity (note 36).

### Premises of A1 the source does not share

- The observer is a landlord ranking tenants vertically (HH-Q6). A1's
  observer values distance in class.
- The outcome is a reply in a rental, the contracting channel at its informal
  margin, not contact.
- Class is moved by the e-mail within a group, the information A1's group
  sufficiency sets aside. The names may carry class as well (HH-Q3).
- Equal treatment counts landlords who did not read the name as a marker at
  all (HH-Q23).

What A1 should cite the source for. The property's empirical statement with its
scope (HH-Q1, HH-Q7, HH-Q8, HH-D1), the channel of class inferred from a name
(HH-Q3), class as not race by definition (HH-Q4), the landlord who screens for
tenants' contact (HH-Q12, HH-Q13), and discrimination at the unrecorded margin
(HH-Q16, HH-Q18, HH-Q19).

## Controls

- LC-2. A fabricated quotation, the abstract with "discrimination is unchanged"
  for "discrimination is non-existent", is not found. A true quotation is not
  found on a neighbouring page.
- Lean. `control_pooled_rates_differ` shows the identity of `net_eq_rate_diff`
  fails across Table 6's two samples. `control_partial_signal_gap` shows the
  gap of `gap_vanishes_full_signal` returns with a partial signal.
  `control_threshold_between` shows equal treatment fails when the threshold
  lies between the predictions. `control_zero_weight` shows a signal with no
  weight moves nothing.
