# Craig and Fryer (2017), notes

## What was verified

- Twenty-one quotations are found on the PDF pages they cite in the
  publisher's text layer, by exact normalised containment (LC-2).
- The general model of Section 3 is implemented in Lean in the paper's
  notation, with the returns of eqs. (1) and (2) and the equilibrium of eqs.
  (3) and (4) as definitions. Bayes' rule and the thresholds, the zero
  equilibrium, both parts of Proposition 1 that the paper proves, Propositions
  2 and 3, the final steps of Propositions 4, 6, 7 and 8, the algebra of
  Proposition 10, the Section 3.1 example with its three equilibria and their
  stability, and the appendix's normalisation, uniform returns and eq. (11)
  are theorems (LC-4). The coverage table in `CLAIMS.md` gives the status of
  each. Every main result has a control that drops one condition. Our reading
  is consistent with the source on these points.

## What could not be verified

- The rows marked "not formalised" in the coverage table, chiefly the
  monotonicity step of Proposition 1, the stability condition, Propositions 5
  and 9, the Lemma and Note 1.
- Typos in the appendix are recorded as CF-D3 to CF-D6, and no row relies on
  the passages they affect.

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1. The source
supports A1's two-sided structure and its consistency condition, and it does
not share A1's preference, A1's group sufficiency or A1's fixed types.

### The puzzle

The source does not study friendship or class. Its authors extend the
two-sided reading beyond labour, to "universities or applicants; police or
civilians" (CF-Q13), which brings it closer to A1's contact. What the source
offers the puzzle is a mechanism of persistence. Under mutual consent either
side's pessimism can sustain a gap (CF-Q6, `control_one_sided_zero_fails`),
and the gap survives a change on one side alone (Proposition 7,
`prop7_st_fails`).

### Budget compatibility

The source has no assortative matching. A match is worth more to each side
when the other side invested, whatever the first side's own investment
(CF-Q10, `match_gain_indep_own`), so preferences are vertical. Investments are
complements across the sides (CF-Q21, `g_mono`), which is the source's
substitute for a reason to match with one's own kind. On cost, the firm's
investment is a group-specific environment such as flexible hours or affinity
groups (CF-Q16), paid as a fixed cost. The fixed cost makes accommodating a
small group unprofitable (CF-Q7, `control_minority_breaks_U`), and the
disadvantage vanishes when the cost is paid per hire (CF-Q15,
`eq11_indep_lambda`). That is a cost of providing a shared environment, close
in spirit to a venue's cost of an atmosphere. It is not a cost of a gap
between two budgets.

### Avoidance that depends on the observer's class

- **Direction (i), avoidance runs both ways.** The source has the structure.
  Workers withhold applications from employers believed not to have invested
  for their group, and employers withhold offers from groups believed not to
  have invested (CF-Q1, `offer_iff`), and each side's withdrawal makes the
  other's exclusion costless (CF-Q14). The two sides are workers and firms,
  not two groups of peers, and the withdrawal does not depend on the
  withdrawing party's own type.
- **Direction (ii), a low-class member of the high group may prefer
  outsiders.** The source has nothing on it. Every agent ranks partners the
  same way (CF-Q3, CF-Q10), so no observer's own type can reverse the ranking.

### The penalty vanishing when class is shown

The source has the property in two places, neither stated as a result about
the group penalty. In the matching model the posterior is the group prior when
the signal is uninformative (`post_uninformative`) and is 1 whatever the
positive prior when the signal reveals investment (`post_revealing`), so the
group drops out of the decision when the type is shown. In Section 6 the wage
puts the weight `σ²_ε/(σ²_ε + σ²_a)` on the group mean and `β_j` on ability
(CF-Q11, CF-Q17). The weight on the group mean is zero when the signal is
noiseless (`betaj_perfect_signal`) and positive otherwise
(`control_betaj_noise`). The conditions are a log-normal prior, an unbiased
log-normal signal and wages equal to marginal product. This is the Phelps and
Aigner–Cain form of the property, and A1 can cite it for the link to the
statistical-discrimination literature.

### The shadow market

The source has three pieces of it.
- The law cannot reach the information used. Equal treatment of equal signals
  "can only be enforced if – in every instance – a regulator can observe and
  verify all information upon which employers rely" (CF-Q18), so the authors
  turn to rules on outcomes.
- A rule on one channel moves the gap to a channel the rule does not reach. An
  equality-in-offers rule is met when B workers do not apply (CF-Q14,
  `prop2_nonbinding`), and a wage subsidy that makes every worker apply removes
  firms' return to investing (`subsidy_kills_firm_investment`).
- Intermediaries of information have value. The authors name the platform
  reading of their market (CF-Q19), and the programmes they favour carry
  signals to firms and show workers that firms invested (CF-Q20, Proposition
  8, `prop8_payment`).

The source does not have clients who pay for social information, venues, or a
value of such information that rises with legal strictness. Its intermediary
removes the information problem rather than trading on it.

### Premises of A1 the source does not share

- Types are binary investments chosen in equilibrium, whereas A1's class is
  given.
- Each side sees an individual signal (`offer_iff`), whereas A1 assumes group
  sufficiency. A1 is the source's case of an uninformative signal
  (`post_uninformative`).
- Preferences are vertical (CF-Q10), whereas A1's are homophilous in class.
- Group membership is fixed (CF-Q2), whereas A1 lets people pass.
- The consistency condition is the same in kind. Each side's prior about a
  group equals the rate the other side's best responses produce (CF-Q4,
  CF-Q5), as A1's imputed standing equals the actual mean of the visible
  members. In the source the rate moves through investment, in A1 through
  composition.
- The source studies dynamics by a backward-looking adjustment process
  (`example_Z_stable`, `example_U_unstable`), which A1 does not have.

## Controls

- LC-2. A fabricated quotation, "the benefit to workers from being matched to
  a good firm depends on their own investment decision", the opposite of
  CF-Q10, is not found. A true quotation is not found on a neighbouring page.
- Lean. Each main result has a control. `control_post_not_revealing`,
  `control_prop1_below_diagonal`, `control_prop2_applicants`,
  `control_prop3_equal_shares`, `control_prop4_positive_return`,
  `control_prop8_uninformative`, `control_betaj_noise`,
  `control_prop10_overstate`, `control_eq11_fixed_cost`,
  `control_one_sided_zero_fails` and `control_minority_breaks_U`.
