# Pesendorfer (1995), notes

## What was verified

- Eighteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the cached text (LC-2). Because the cached text
  interleaves the two columns, each quotation is a contiguous run, and the
  rest of a sentence is given in the last column of `CLAIMS.md` from the page
  read (PF-D1).
- 34 Lean theorems implement the paper's model in its notation, with eq. (4)
  `f`, matching technology (i) `meetHigh`, eq. (5) and eqs. (6) and (7) as
  definitions. Of the 24 results in the coverage table of `CLAIMS.md`, 10 are
  formalised in general, 7 in a special case and 7 not at all (LC-4).
- Our reading is consistent with the source on these points.

## What could not be verified

- Theorem 1, Proposition 1 (iv), Proposition 2 and the appendix claims are
  read, not formalised. The reasons are in the coverage table.
- The example prices are only partly reproduced from eq. (9) (PF-D5).
- `observed_type_no_value` formalises our reading of PF-Q13, not a result the
  paper states formally.

## The bearing on Paper A1

Nothing in the paper contradicts the corrected account of A1. The paper bears
on A1's shadow market more than on any other part of A1, and its premises
differ visibly from A1's, so both are set out.

**The puzzle.** The paper does not address race or diversity. It does say
that a signal of type matters more where people meet others whose type they do
not know (PF-Q18, PF-Q13), which runs in the same direction as A1's account,
in which mixing raises the weight on what a marker conveys. It also predicts
sharper separation when the gap between types is large (Proposition 1 (iii),
`prop1_iii_step`). Neither statement concerns a group marker, so the paper
offers a parallel to A1's explanation, not the explanation.

**Budget compatibility.** Sorting in the paper comes from complementarity,
grounded in complementary human capital (PF-Q7, PF-Q8), and the author says
it is less justified when the type is wealth that couples pool (PF-Q14).
Utility is quasi-linear in money, so a price screens by willingness to pay,
which complementarity makes higher for high types (`single_crossing`,
`separating_price_iff`), and never by ability to pay. Affordability plays no
role. If A1 grounds homophily in budget compatibility, a venue's price in A1
can screen by what a budget sustains, which is a different mechanism from
Pesendorfer's, and A1 should say so.

**Avoidance that depends on the observer's class.** (i) In the paper
avoidance runs one way. Both types prefer a high partner (PF-Q6), so the value
of a pool rises with its share of high types for both (`pool_value_mono`), and
high types avoid low types while low types seek high ones. Under A1's utility
the low class prefers a low partner (`control_quadratic_low_prefers_low`), so
avoidance runs both ways (`quadratic_avoidance_both_ways`), while A1's utility
still satisfies the paper's complementarity (`quadratic_complementarity`).
(ii) With two types the paper has no low member of a high group, and a pool's
spread is fixed by its mean (`two_type_gap`). Low types who join the in-group
in the egalitarian phase want to be there. The closest the paper comes to a
composition effect within a group is footnote 9, where a larger in-group
lowers the average of both groups (PF-Q10). The paper neither supports nor
contradicts direction (ii). Its preferences cannot produce it.

**The penalty vanishing when class is shown.** The exact statement is that a
necessary condition for demand for otherwise useless designs is that
individuals do not know each other's characteristics and histories (PF-Q13,
with PF-Q11). In our reading, for two types and matching that can condition
on type once it is observed, the design is worth nothing to a high type
(`observed_type_no_value`), against a positive value when type is unobserved
(`control_unobserved_positive`). This is a statement about the value of a
signal, not about a group penalty, but it has the same shape as A1's
property, and footnote 8 places it against pure signalling (PF-Q9), which is a
point of contact with the signalling side of the statistical-discrimination
literature.

**The shadow market.** The paper is close to the idea and differs from it in
specific ways.
- *What is close.* A seller provides information about whom one will meet.
  The design is sold by an intermediary whose value is the average type of its
  clients (PF-Q15), the chance of a good match is computed from the actual
  users (`in_pool_high`, `out_pool_high`), a price screens types that cannot
  be seen (PF-Q2, PF-Q3, PF-Q5), and under competition matching separates
  fully (PF-Q12, `prop3_price`). The paper also has the substitution between
  a formal class rule and a market signal. Sumptuary law is read as a way to
  avoid wasteful fashion where the lawmaker can identify efficient matches
  (PF-Q16, PF-Q17).
- *Where it differs.* The substitution runs in the mirror direction. A1's
  predecessor says that banning the formal criterion raises the value of
  shadow information, and Pesendorfer says that allowing a formal class rule
  removes the need for the signal. He models no ban on screening by group and
  no legal strictness. The signal is about the buyer's own type, chosen and
  paid for, not a marker correlated with a group, and there are no groups and
  no group standing. Admission is by self-selection at a price, not by a
  venue's policy on markers. The value of separation rises with the gap
  between types (`single_crossing`, `prop1_iii_step`), a parallel to the
  predecessor's intensity rising as a group's standing falls, but not the
  same statement.
A1 can cite the paper for a market that sells information about whom one will
meet and for the substitution between a formal class rule and a purchased
signal. The claim that legal strictness and marker salience raise the value
of such information is not in the paper.

**Premises of A1 the paper does not share.**
- Vertical preferences rather than homophily (PF-Q6, `pool_value_mono`).
- Two types rather than a continuum, so spread is fixed by the mean
  (`two_type_gap`).
- A purchased individual signal rather than a group marker (PF-Q1).
- Random matching within a pool rather than a chosen engagement
  (`in_pool_high`).
- Quasi-linear money rather than budgets (`separating_price_iff`).
- Dynamic durable-goods pricing rather than a static equilibrium
  (`prop3_price`).
- No law constraining screening (PF-Q16).

## Controls

- LC-2. A fabricated quotation, "low types prefer to be matched with low
  types", the opposite of PF-Q6, is not found. A true quotation is not found
  on a neighbouring page.
- Lean. Each main result has a control that drops a condition.
  `control_no_separation_without_complementarity` for the separating price,
  `control_no_bandwagon_without_gain` for the shape of eq. (4),
  `control_no_T_when_costly` for the cycle length of Proposition 3,
  `control_price_floor_one_designer` for the price floor,
  `control_unobserved_positive` for `observed_type_no_value`, and
  `control_quadratic_low_prefers_low` for the paper's assumption that both
  types prefer a high partner.
