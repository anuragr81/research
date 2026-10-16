# Aigner and Cain (1977), notes

## What was verified

- Nineteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the cached text (LC-2). Because the cached text
  interleaves the two columns, each quotation is a contiguous run within one
  column line, and the rest of the sentence is given in the last column of
  `CLAIMS.md` from the page read (AC-D1).
- Eqs. (2) to (8), footnotes 15, 17 and 18, the mean–variance criterion and
  the results drawn from Figs. 1A to 5 are Lean theorems in the paper's
  notation (LC-4). Eq. (8) is proved from the moment generating function of
  the normal law, not assumed. The coverage table in `CLAIMS.md` has 22
  rows, 17 general, 3 special cases and 2 not formalised.
- Our reading is consistent with the source on these points.

## What could not be verified

- The selection–truncation model (p. 11) has no equation in the paper and is
  formalised only at the extreme of Fig. 4. Eq. (2)'s normal conditional
  expectation is formalised as a least-squares slope. Spence's model is not
  formalised.
- The authors' estimate that the black–white differential is "perhaps 10 to
  30 percent for the same number of years of schooling" (p. 9) carries no
  source and is not used.

## The bearing on Paper A1

Nothing in the paper contradicts the corrected account of A1. One framing
used in the first pass does not survive the reading, and it comes first. A1's
`σ_g²` term is not an alternative to risk aversion. A1's `v − κ(c_i − x)²` is a
concave utility over the partner's class, and its expectation is the utility
at the mean less `κσ²` (`contrast_quadratic_ce`), so `σ_g²` is the risk premium
of A1's utility, as `k Var(q|y)` is the risk premium of eq. (7) (`eq8`,
`eq8_order`). What differs is narrower. A1's premium is exact for any
distribution, where the paper's needs a normal belief and exponential utility.
A1's curvature `κ` is the strength of homophily, so A1 has no counterpart of
the paper's risk-neutral employer (AC-Q4, `control_risk_neutral`).

### The puzzle

The paper is about pay in labour markets and says nothing about contact,
friendship or the diversity of a setting. It does not bear on why friending
bias across class is higher in racially diverse settings.

### Budget compatibility

The paper assumes no homophily and no assortative matching. Its employer's
payoff rises in the worker's productivity alone (eq. 7). The nearest thing to
a cost of mismatch is the authors' reading of the curvature of eq. (7) as "the
costs of mistakes in assigning workers to heterogeneous job slots" (AC-Q18).
That gives a concave payoff an origin in mismatch between a worker and a
slot, as budget compatibility gives A1's quadratic loss an origin in
mismatch between two budgets. Nothing in the paper concerns affordability or
shared consumption.

### Avoidance that depends on the observer's class

The paper has no observer type. Its employers differ only in the
coefficient `k` (p. 10), which changes the weight on the variance and never
the ranking of groups of equal variance by their means
(`contrast_cara_common_ranking`). So the paper neither supports nor
contradicts direction (i) or direction (ii). It limits both in one respect.
The variance penalty is common to every observer (`fig2_black_paid_less`,
AC-Q6, AC-Q9), so in A1 the `σ_g²` term pushes every observer away from a
heterogeneous group, whatever the observer's class. For direction (ii), a
low-class member of a high group prefers outsiders only when
`(c̄_out − c_i)² + σ_out² < (c̄_own − c_i)² + σ_own²`; the paper's mechanism is
the second term, which can work against (ii) when the outside group is the
more heterogeneous. Under A1's utility the ranking of equal-variance groups
does turn on the observer's own class
(`contrast_quadratic_ranking_depends_on_own_class`); that is our contrast,
not the paper's. The paper also shows a penalty that reverses with the
worker's own position, not the observer's. Below `y₀` the noisier group is
paid more (`fig2_crossing`), and in Figs. 1A and 1B the steeper group is
favoured above the mean and penalised below it (`fig1_crossing`).

### The penalty vanishing when class is shown

The paper states it for its unequal-means model. "In the presence of perfect
information (γ = 1), the systematic difference in E(q|y) or E(q|q) for the
two racial groups disappears" (AC-Q13, `perfect_information`). The conditions
are equal reliability across groups and a fully reliable signal. Short of
full revelation the gap is `(1 − γ)(α^W − α^B)`, so it shrinks in proportion
to the unreliability (`fig5_parallel`, `control_fig5_perfect_information`).
In the risk model the discount `k Var(q)(1 − γ)` also vanishes at `γ = 1`. A1's
property, that the group penalty vanishes when class is shown, is the case
`γ = 1` of this statement with A1's quadratic loss in place of pay. With
group sufficiency, `γ = 0`, the paper's discount is `k` times the group's
own variance (`zero_reliability`), the counterpart of A1's `σ_g²`.

### The shadow market

Three passages bear on discrimination that survives a rule on the formal
criterion.
- A legal rule on the observed criterion does not reach the group outcome.
  "Even a legal requirement that payments be equal for equal y scores would
  contribute nothing to the overall improvement of the status of blacks"
  (AC-Q15).
- A group-neutral rule on a correlated variable yields a group gap. In the
  truncation model employers "are not race-biased in their hiring process"
  (AC-Q17) and hire on `E(q|y)` alone, yet the group with the less reliable
  test is hired less (AC-Q11, AC-Q12, `fig4_truncation`). The correlated
  variable is the reliability of the signal, not a marker.
- Information about the unseen type acquires market value as the penalty
  grows. "A large R factor should activate a market for "test instruments"
  that are tailored to the separate groups" (AC-Q16), and location carries
  "extra search and informational costs" (AC-Q19). In the paper this market
  reduces the penalty by improving the signal; in A1's shadow market social
  information is used to sort. The paper has no venues and no platforms.

### Premises of A1 the paper does not share

- A payoff monotone in the other's type (eq. 7), where A1's has an ideal point
  at the observer's own class.
- A normal belief and exponential utility (footnote 17), where A1's premium
  needs neither.
- An individual signal `y` about the worker, where A1's marker under group
  sufficiency carries information about the group only.
- Consistency of beliefs with group means as an assumption backed by
  competition (AC-Q2, AC-Q3), where A1 imposes it as an equilibrium condition.
- Equal group means in the risk model, where A1's groups differ in mean.

## Controls

- LC-2. A fabricated quotation, "a risk-neutral employer will attempt to hire
  from the group with the smaller conditional variance", the opposite of
  AC-Q4 with AC-Q6, is not found. A true quotation is not found on another
  page.
- Lean. `control_risk_neutral` shows the Fig. 2 penalty needs `k > 0`, and
  `control_no_spread` shows it needs `Var(q) > 0`.

## Page images read

The 110 dpi images of PDF pages 3 and 8 were rendered from the pinned PDF
with `pdftoppm`. They are reproducible from it and not pinned separately.
