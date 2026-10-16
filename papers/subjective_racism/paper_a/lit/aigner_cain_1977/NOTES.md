# Aigner and Cain (1977), notes

## What was verified

- Fourteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the cached text (LC-2). Because the cached text
  interleaves the two columns, each quotation is a contiguous run within one
  column line, and the rest of the sentence is given in the last column of
  `CLAIMS.md` from the page read (AC-D1).
- Eqs. (3) and (8), footnotes 15, 17 and 18, the mean–variance criterion and
  the Fig. 2 result are Lean theorems in the paper's notation (LC-4). Eq. (8)
  is proved from the moment generating function of the normal law, not
  assumed.
- Our reading is consistent with the source on these points.

## What could not be verified

- The selection–truncation model (p. 11) has no equation in the paper and is
  read, not formalised.
- The authors' estimate that the black–white differential is "perhaps 10 to
  30 percent for the same number of years of schooling" (p. 9) carries no
  source and is not used.

## The bearing on Paper A1

One framing in the brief does not survive the reading, and it comes first.
The brief contrasts A1's `σ_g²` term, entering "through a quadratic loss in
class distance", with the paper's variance, entering "through risk aversion
over productivity". These are one mechanism. A1's `v − κ(c_i − x)²` is a
concave utility over the partner's class `x`, and its expectation is the
utility at the mean less `κσ²` (`contrast_quadratic_ce`). The `σ_g²` term is
therefore the risk premium of that utility, as `k Var(q|y)` is the risk
premium of eq. (7) (`eq8`, `eq8_order`). What differs is narrower and
checkable.

- A1's penalty is exact for any distribution of class. The paper's needs a
  normal belief and exponential utility (footnote 17).
- A1's curvature `κ` is the strength of homophily itself, so A1 has no
  counterpart of the paper's risk-neutral employer (AC-Q4,
  `control_risk_neutral`). Setting `κ = 0` removes the mean term and the
  variance term together.
- A1's mean term is the squared gap to the observer's own class. The paper's
  employers rank groups of equal conditional variance by their means alike,
  whatever their risk aversion (`contrast_cara_common_ranking`). Under A1,
  observers of different classes rank them oppositely
  (`contrast_quadratic_ranking_depends_on_own_class`).

A1 should present `σ_g²` as the risk premium implied by homophily and cite
the paper for the penalty on a group's variance, rather than as an
alternative to risk aversion.

1. **Contribution 1 (group avoidance from class homophily under inference).**
   The variance half is in the paper, for hiring. A risk-averse employer
   "will attempt to hire from the group of workers that maximizes expected
   productivity, q, discounted for risk" (AC-Q6), and the discount is on the
   group's conditional variance (AC-Q7, AC-Q8, AC-Q9). With a signal that says
   nothing about the individual, which is A1's group sufficiency, the
   discount is `k` times the group's own variance (`zero_reliability`). Every
   risk-averse employer applies it. The motivation scan's statement that "no
   source read has a group's spread penalised in matching" must be qualified,
   since hiring from one group or the other is a choice of partner group. The
   observer-dependent half is not in the paper. Its employers differ only in
   the coefficient `k` (p. 10), which changes the weight on the variance but
   not the ranking by mean.
2. **Contribution 2 (budget compatibility).** Not in the paper. Its payoff is
   the employer's profit from `q`, monotone in `q`.
3. **Contribution 3 (venue exclusion).** Not in the paper. The truncation
   model has a hiring floor on `E(q|y)` (AC-Q12), which screens on predicted
   type, but no venue chosen by clients.
4. **Contribution 4 (the test of the class channel).** The labour-market form
   is in the paper. With perfect information "the systematic difference in
   E(q|y) or E(q|q) for the two racial groups disappears" (AC-Q13), and with
   `γ = 1` the risk discount is zero, since `Var(q|y) = Var(q)(1 − γ)`. A1's
   test applies the same logic to contact.

The paper also states the consistency of beliefs that A1 imposes on imputed
group standing (AC-Q2, AC-Q3), as an assumption backed by competition rather
than an equilibrium fixed point.

The brief's phrase "penalised only under risk aversion" holds for the
paper's risk model and its Phelps model, where mean wages are equal without
risk aversion, but not for its truncation model, which penalises the noisier
group "without recourse to risk aversion" (AC-Q11, AC-D5).

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
