# Pesendorfer (1995), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the cached text (LC-2). Because the cached text
  interleaves the two columns, each quotation is a contiguous run, and the
  rest of a sentence is given in the last column of `CLAIMS.md` from the page
  read (PF-D1).
- Eqs. (2) to (4), the shape of the inverse demand, sorting by
  complementarity and the static separating price are Lean theorems in the
  paper's notation (LC-4).
- Our reading is consistent with the source on these points.

## What could not be verified

- Theorem 1 and Propositions 1 to 3 are read, not formalised. The separating
  price in Lean is the static counterpart of Proposition 3's price floor, not
  the proposition.
- Footnote 9's continuum case is read in words only.

## The bearing on Paper A1

Nothing in the paper contradicts the A1 model as described. One difference in
preferences matters for contribution 3, and it comes first. Pesendorfer's
types both prefer a high partner (PF-Q6). A1's `v − κ(c_i − c_j)²` makes
the low class prefer a low partner (`control_quadratic_low_prefers_low`),
though it satisfies his complementarity (`quadratic_complementarity`).

1. **Contribution 1 (avoidance from class homophily under inference).** In
   part. An unseen type is inferred from a visible choice in a matching game
   (PF-Q1, PF-Q2), and the value of joining a pool depends on the chooser's
   own type (PF-Q7, `single_crossing`). The visible item is a design the
   person buys, not a group marker she is born with, and preferences are
   vertical rather than homophilous. The paper has no penalty on a group's
   spread. With two types the spread of a pool is fixed by its mean
   (`two_type_gap`), and footnote 9 notes only that with a continuum of types
   a larger in-group lowers both groups' averages (PF-Q10). A1's claim that
   everyone avoids the group with more within-group variance is not in the
   paper.
2. **Contribution 2 (budget compatibility as the source of homophily).** Not
   in the paper. Pesendorfer grounds complementarity in complementary human
   capital (PF-Q8) and says it is less justified when the type is wealth that
   couples pool (PF-Q14). If A1 grounds homophily in budgets, it has to say
   why budget compatibility is not the wealth pooling he sets aside.
3. **Contribution 3 (venue exclusion, price screening as certification).**
   Price screening as certification of an unseen type in social matching is
   in the paper, and is not new. A price high enough keeps the low types out
   and lets the high types separate (PF-Q2, PF-Q3, PF-Q5,
   `separating_price_iff`). Under competition only high types buy and
   matching is fully separated (PF-Q12). A seller's value is the average type
   of its clients (PF-Q15), and the share of high types among the actual
   users of a design is the chance of meeting one, which is a consistency
   condition between a pool's imputed and actual composition, the analogue
   of A1's condition on the visible members' mean. What the paper does not
   have is exclusion driven by inference from a group marker, since the
   design is the only signal, and exclusion under homophily. Under A1's
   utility a low-class person does not gain from a high-class pool, so the
   reason the price is needed in Pesendorfer, that low types want to meet
   high types, is absent, and A1 must give its own reason for a price.
4. **Contribution 4 (the class channel vanishes when class is shown).** Only
   as an analogue. The screening device has value only when types are
   unknown (PF-Q13, PF-Q11), and sumptuary laws are read as a substitute
   where the lawmaker can see who should meet whom. The paper states no group
   penalty and no test of one.

**What A1 should cite.** Pesendorfer for price screening of an unseen type in
matching, for the dependence of a pool's value on its actual composition,
and for complementarity as the condition under which a price sorts. A1 can
then state its difference from him as a group marker in place of a purchased
signal and homophily in place of vertical preferences.

## Controls

- LC-2. A fabricated quotation, "low types prefer to be matched with low
  types", the opposite of PF-Q6, is not found. A true quotation is not found
  on a neighbouring page.
- Lean. `control_no_separation_without_complementarity` shows that the
  separating price needs (1), `control_no_bandwagon_without_gain` that the
  bandwagon range needs `v^h > 0`, and `control_quadratic_low_prefers_low`
  that A1's utility breaks the paper's assumption that both types prefer a
  high partner.
