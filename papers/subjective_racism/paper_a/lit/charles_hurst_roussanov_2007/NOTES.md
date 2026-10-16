# Charles, Hurst and Roussanov (2007), notes

## What was verified

- Twenty-five quotations are found on the PDF pages they cite, by exact
  normalised containment in the text layer (LC-2). CH-Q13 carries an elision
  where the text layer places the parenthesis of `g(c^i)` inside the sentence.
- The general model, the Theory Appendix, the estimating model and every
  number that follows from printed numbers are in Lean in the paper's
  notation (LC-4). The coverage table in `CLAIMS.md` lists each result.
- Our reading is consistent with the working paper on these points.

## What could not be verified

- The QJE (2009) version is not read (CH-D1). A row that cites the 2009
  article for anything in this record carries an unverified mark until the
  article is read.
- The Theory Appendix's change of variables is not the chain rule (CH-D2).
  Only the stated solution is formalised.
- The definition of the dispersion regressor is inconsistent between the
  text and the note to Table 6 (CH-D3), so only its sign is used.

## The bearing on Paper A1

Judged against the corrected account of A1 (16 October 2026).

### The puzzle

The paper is about display, not contact, and does not address friending
bias. It does show that at equal permanent income Black and Hispanic
households display about 30 percent more visible goods than White households
(Table 2, `numbers_visible_dollars`), and that the gap goes with the mean
income of the own-race group (CH-Q10). An observer who reads class from
display in a racially mixed setting therefore reads a display shifted by
group. That this bears on cross-class friending in diverse settings is our
inference, and the paper does not draw it.

### Budget compatibility

This is the paper's most direct bearing. At equal total expenditure,
households of different races allocate their budgets differently, with more
on visible goods and less on education, entertainment and health (CH-Q19,
CH-Q20, Table 5). About 60 percent of the extra visible spending comes out of
education, health, food and housing (`numbers_table9`,
`numbers_diversion`). So the same budget does not mean the same activities,
and the visible part of a budget, which is what a stranger sees, overstates
the resources left for other shared activities in groups with a lower mean.
The paper does not derive any grouping. It assumes race-by-state reference
groups from cultural separation and segregation (CH-Q16, CH-Q7), and leaves
open whether the relevant group is local or racial (CH-Q17) and whether people
choose neighbours for status (CH-Q18). No matching cost appears.

### Avoidance that depends on the observer's class

Observers are homogeneous strangers and nobody chooses contact (CH-Q12), so
direction (i) gets no support or contradiction. Direction (ii) has a parallel
on the side of status, not engagement. With status as income relative to the
group mean, a member's status falls as the group gets richer
(`known_income_status`, `s_c_mu`), so a low-income member of a rich group
would gain status from a poorer reference group. The paper only raises the
choice of neighbours (CH-Q18), and the gain from switching is our inference.
Ireland's alternative, where a richer group confers status (CH-Q5), reverses
it (`control_status_gain_rising`). Direction (ii) in this literature therefore
depends on how status is formed.

### The penalty vanishing when class is shown

The paper's exact statement is in the Theory Appendix. With income known,
status would be relative income, `y/μ_G` (CH-Q21). Showing income ends the
signalling, but the group mean still moves status at fixed income
(`known_income_status`). This is a limit on A1's property, which holds when
the group enters only as a prior about class. Where the group is also the
yardstick of comparison, showing class does not remove it. The empirical
counterpart, Table 8, removes the race coefficient by controlling the group
mean, not individual income (CH-Q10, `ovb`, `table8_implied`). The paper does
not cite the statistical-discrimination literature.

### The shadow market

The paper sets aside discrimination in the housing and mortgage markets, a
contracting channel, as a reason to exclude housing (CH-Q22). It notes that
an observable correlated with poverty, participation in means-tested
programs, becomes a signal of class and carries a stigma (CH-Q23). Visible
goods are the signals of anonymous encounters (CH-Q12). It has no venue,
platform or law reaching the contact channel.

### Premises of A1 the source does not share

- Display separates. Consumption reveals income (CH-Q3), so the group is not
  a prior about class.
- Status is income relative to the own group, and the group is the yardstick
  (CH-Q6, CH-Q13).
- Race fixes the reference group exogenously (CH-Q7, CH-Q16).
- There is no engagement payoff, and observers are homogeneous strangers.

## Controls

- LC-2. A fabricated quotation, "outsiders infer a person's income from the
  person's race alone, using the variance of group income", the opposite of
  CH-Q2 and CH-Q4, is not found. A true quotation is not found on a
  neighbouring page.
- Lean. `control_status_gain_rising` shows the decline in the group mean needs
  `s_cμ < 0`, `control_soc_needed` that it needs the second order condition,
  `control_status_negative` and `control_concavity_needed` that the two
  implications of eq. (3) need `s_c > 0` and `u'' < 0`, and
  `control_fn12_no_bias` and `control_ovb_no_channel` that the bias and the
  decomposition vanish without their channel.
