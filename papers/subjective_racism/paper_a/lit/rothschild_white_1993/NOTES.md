# Rothschild and White (1993), notes

## What was verified

- Eighteen quotations are found on the PDF pages they cite in the text layer of
  the NBER chapter, by exact normalised containment (LC-2).
- The stand-alone test, the general problem of section 1.3.1, eq. (1), the
  Spence monotonicity argument, the homogeneity and diversity examples, the
  ranking lemma for anonymous prices, the tuition and scholarship results of
  section 1.4, and the numbers that follow from other printed numbers are Lean
  theorems in the chapter's notation (LC-4). The chapter is verbal apart from a
  few displays, so these are formalisations of our reading of its structure.
  The coverage table in `CLAIMS.md` lists what is general and what is a special
  case. Our reading is consistent with the source on these points.

## What could not be verified

- The 1995 *Journal of Political Economy* article, which is the formal
  treatment of customers as inputs, is not read (RW-D1). Nothing about its
  model is attributed here.
- The claims the chapter cites rather than derives, the Spence-type segregating
  price (RW-Q8) and possible non-existence (RW-Q9).

## The bearing on Paper A1

Revised on 16 October 2026 against the corrected account of A1. Nothing in the
chapter contradicts that account. The chapter is the economics of a venue that
prices and admits on the composition of its clients, which is the supply side
of the shadow market. It has no group markers and no inference.

### The puzzle

The chapter does not address contact across groups. Two of its points bear on
diverse settings. A diverse student body is chosen by the venue, by admission
and selective scholarships (RW-Q4), so a diverse setting is a selected setting.
And when students care who their classmates are, stable allocations may not
exist (RW-Q12, RW-Q15), so the composition of a setting may not be pinned down.

### Budget compatibility

The chapter's origin of grouping by type is a cost in production. "It is hard
to teach a class when the students differ greatly in ability and background"
(RW-Q18), and a benefit falling with the variance of ability makes grouping
efficient (RW-Q7, `sorted_pairing_efficient`). The grouping is a planner's
result, not a preference of the students, and it disappears without the
synergy (RW-Q3, `control_no_synergy`, `all_efficient_without_synergy`).
Affordability enters in two places. Liquidity or wealth limits a price-based
allocation (RW-Q14), and a norm rules out buying access to the best venues
(RW-Q16). Neither is a cost of a relationship across a class gap. A1's budget
compatibility is a different origin of homophily from the chapter's, and the
two can be set side by side.

### Avoidance that depends on the observer's class

Neither direction is in the chapter. In section 1.3.2 every student gains from
a higher mean ability, so students rank colleges alike whatever their own
ability. In section 1.3.3 risk-averse students gain from classmates whose
inputs are unlike their own (`mixed_better`), a motive for mixing that does not
depend on class and fails when inputs are perfectly correlated
(`control_perfect_correlation`).

### The penalty vanishing when class is shown

Not in the chapter. The college observes the characteristics it prices on, so
there is nothing to infer (RW-Q10).

### The shadow market

The chapter supplies the venue's side of the shadow market, read with the
predecessor's platform model.

- Exclusion is a price. A denial of admission is an infinite price (RW-Q5,
  `pA_finite_iff`), and any allocation can be implemented by personal prices
  (eq. (1)).
- Prices may condition on characteristics and not on identity (RW-Q10), and net
  prices depend on "a great many personal characteristics" through
  scholarships (RW-Q17). A rule that removes one characteristic from the price
  leaves the others.
- Anonymous prices cannot separate clients who look alike
  (`anonymous_same_ranking`), and personal prices can
  (`control_personalised_separates`). Our reading is that a venue that wants a
  composition and is barred from pricing on group needs some other observable
  characteristic to condition on, so a variable correlated with group acquires
  value. That is the mechanism by which a signal substitutes for a banned
  criterion.
- A group-neutral price menu sorts clients by type through self-selection when
  the benefit has increasing differences (RW-Q8, `choice_monotone`), and not
  otherwise (`control_choice_not_monotone`). This is the
  predecessor's client self-selection `h(c_j, a)` in one formal version, with price or atmosphere as
  `g`.
- Rules on prices constrain the venue. A norm rules out selling access (RW-Q16),
  and the Justice Department pursued coordination of scholarship levels (PDF
  p. 14). The chapter does not ask whether such rules move selection to other
  channels.

### Premises of A1 the source does not share

- Types are observed by the venue. There is no marker and no inference.
- Preferences over pools are vertical in section 1.3.2.
- The results are about efficient allocations chosen by a planner or
  implemented by prices, not about an equilibrium of engagement.
- Colleges are non-profits that answer to non-tuition funders (PDF pp. 12, 17),
  an objective unlike a venue maximising profit.

## Controls

- LC-2. A fabricated quotation, "Allocations differ in efficiency even when
  there is no synergy.", the opposite of RW-Q3, is not found. A true quotation
  is not found on a neighbouring page.
- Lean. `control_no_synergy` shows that without a variance penalty all
  allocations tie. `control_perfect_correlation` shows that the gain from
  mixing needs inputs that are not perfectly correlated.
  `control_personalised_separates` shows that personalised prices do what
  anonymous ones cannot. `control_choice_not_monotone` shows that choices need
  not rise with type without increasing differences.
  `control_singleton_tests_insufficient` shows that the test against
  single-product rivals does not imply the full stand-alone test.
