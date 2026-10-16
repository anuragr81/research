# Moav and Neeman (2012), notes

## What was verified

- Nineteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the text layer (LC-2).
- The model is implemented in Lean in the paper's notation (LC-4). The
  coverage table in `CLAIMS.md` lists every numbered result with its status.
- Our reading is consistent with the source on these points, with the
  readings MN-D1 to MN-D4.

## What could not be verified

- Proposition 2 as stated. Its proof needs `y̲(h)/h` non-decreasing, which
  non-decreasing and convex `y̲` does not give (MN-D1). With `y̲(0) ≤ 0`, as
  under eq. (11), the conclusion holds (`prop2_corrected`).
- Proposition A1 beyond its steps (i) and (ii), and that Figure 2 has exactly
  two crossings.
- The authors' description of Charles et al. (2009) (MN-Q3) refers to the QJE
  version, which is not read (MN-D5).

## The bearing on Paper A1

Judged against the corrected account of A1 (16 October 2026).

### The puzzle

The paper says nothing about contact, friendship or racial diversity, so it
does not explain friending bias. What it explains is display. At a given
income, a person whose income is less certified by an observed trait spends
more on display (`x_anti_floor`, MN-Q16). An observer who reads class from
display therefore reads people without credentials as richer than they are.

### Budget compatibility

The paper takes the budget as split three ways, into hidden consumption,
hidden bequest and visible spending, with visible spending drawn from the
other two (`eq45_split`, `eq6_reduced`, MN-Q19). Two people with the same
income show different visible budgets when their credentials differ
(`x_anti_floor`), and the same display comes from different incomes at
different credentials. So display is not a reading of the budget that would
sustain shared activities. The paper assumes no matching and gives no origin
for assortative grouping. It names sorting into neighbourhoods only as a
rival mechanism (MN-Q18). The shared expenses it discusses, festivals and
weddings, are displays whose cost is the signal (MN-Q15).

### Avoidance that depends on the observer's class

Nothing in the paper supports, contradicts or limits either direction.
Status is the belief of "society" about a person's income (MN-Q1, MN-Q5), the
same for every observer, and nobody chooses whom to meet.

### The penalty vanishing when class is shown

The paper's exact statement is on the sender's side. "Providing an external
sign on income should reduce conspicuous spending" (MN-Q4), and revealing
incomes should lower charitable giving that is about signalling (MN-Q11). In
the model, a credential that pins income lowers display (`x_anti_floor`), and
without an observed credential the share is the constant `λ`
(`remark_constant_share`). The condition is a fully separating equilibrium in
which status is the belief about income. Under separation there is no group
penalty to vanish, since the belief about each person is a point (MN-Q2).
Footnote 15 says the distribution of a pooled set would enter the belief
under partial separation (MN-Q9), which is the situation of A1's marker. The
paper cites none of the statistical-discrimination literature.

### The shadow market

The paper reports a government closing channels of display, gold teeth,
phones at universities and large parties (MN-Q14), and capping guests and
food at weddings (MN-Q15). These are group-neutral rules on visible expenses.
The paper does not model where display goes after a ban. It does model two
signals of income as substitutes (MN-Q17): where a credential speaks, spending
falls (`x_anti_floor`). That a ban on one channel raises the value of the
others follows from that substitution only by our inference, and the paper
does not say it. No venue or platform appears.

### Premises of A1 the source does not share

- Status is a direct argument of utility, the belief about one's own income,
  not a payoff from engaging with someone (eq. 3).
- The equilibrium is fully separating, so beliefs are points (MN-Q2), while
  A1's marker pools.
- The observed trait is an individual credential, not a group.
- The model has a law of motion across generations (`phi_case_i`,
  `phi_case_ii`, `phi_two_crossings`). A1 is static.

## Controls

- LC-2. A fabricated quotation, "Thus, the variance of income given human
  capital determines the equilibrium expenditure on conspicuous
  consumption.", the opposite of MN-Q9, is not found. A true quotation is not
  found on a neighbouring page.
- Lean. `control_prop2_constant_floor` shows Proposition 2's conclusion fails
  for a non-decreasing, convex lowest income with a positive intercept, and
  `control_unobserved_constant` that the declining share needs observed
  human capital.
