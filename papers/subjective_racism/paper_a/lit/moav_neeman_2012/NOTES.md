# Moav and Neeman (2012), notes

## What was verified

- Thirteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the text layer (LC-2).
- Eq. (9) solving eq. (7), property (i), the first step of the proof of
  Proposition 2, the Remark and case (i) of the dynamics are Lean theorems in
  the paper's notation (LC-4).
- Our reading is consistent with the source on these points, with the
  readings MN-D1 to MN-D4.

## What could not be verified

- Proposition 2 as stated. Its proof needs `y̲(h)/h` non-decreasing, which
  non-decreasing and convex `y̲` does not give (MN-D1). Under eq. (11), the case
  the paper uses in Section 4, the ratio is non-decreasing and the conclusion
  holds.
- The authors' description of Charles et al. (2009) (MN-Q3) refers to the QJE
  version, which is not read (MN-D5).

## The bearing on Paper A1

1. **Inference from an observed trait.** Beliefs condition on an observed
   trait correlated with income, human capital, and on a costly signal
   (MN-Q5). The equilibrium separates, so the belief about each person is a
   point (MN-Q2). The authors say that only the lowest income given `h`
   matters under separation, and that under partial separation the
   distribution of the pooled set would enter the belief (MN-Q9). A1's
   observer sees the marker only, which is the pooled case, and that is why
   the group's within variance appears in A1's expected squared gap. MN-Q9 is
   the one-line statement of that contrast in this literature.
2. **Contribution 1, avoidance from homophily under inference.** Not in the
   paper. The observer is "society", there is no contact choice, and nothing
   depends on the observer's own income. The gloss on precision (MN-Q7)
   speaks of variance, but the result uses only the lower bound of income.
3. **Contribution 2, budget compatibility.** Not in the paper.
4. **Contribution 3, venues and price screening.** Not in the paper. An
   observed credential, human capital, reduces the need to signal by spending
   (MN-Q10), which is certification by a credential and not by a price or a
   venue.
5. **Contribution 4, the class-channel test.** Not in A1's form. The paper
   proposes a revelation test, that showing income should lower conspicuous
   spending (MN-Q4), applied to charity (MN-Q11). That test measures the
   sender's spending when income is revealed. A1's test measures the
   observer's group penalty in contact when class is shown. A1 can cite
   MN-Q4 as precedent for testing a signalling channel by revealing the
   hidden variable.
6. **A citation hazard.** The paper reads Charles et al. (2009) as showing
   that race is a signal of income (MN-Q3). The working paper of Charles et
   al. read here does not model race as a signal (MN-D5). A1 should not
   repeat the description without reading the QJE version.

## Controls

- LC-2. A fabricated quotation, "Thus, the variance of income given human
  capital determines the equilibrium expenditure on conspicuous
  consumption.", the opposite of MN-Q9, is not found. A true quotation is not
  found on a neighbouring page.
- Lean. `control_prop2_constant_floor` shows Proposition 2's conclusion fails
  for a non-decreasing, convex lowest income with a positive intercept, and
  `control_unobserved_constant` that the declining share needs observed
  human capital.
