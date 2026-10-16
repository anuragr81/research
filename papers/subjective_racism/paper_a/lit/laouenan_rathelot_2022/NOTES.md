# Laouénan and Rathelot (2022), notes

## What was verified

- Twenty-one quotations are found on the PDF pages they cite in the publisher's
  text layer, by exact normalised containment (LC-2).
- The posterior mean as a precision-weighted average, the concavity and
  identification of `f(K) = K/(K + ρ)`, the identifying condition of Section
  III.A, Predictions 0 to 2 as statements about the conditional mean, the
  decomposition of Section III.B, the gap between two listings of equal quality
  and its limit with and without a taste term, the host's first-order
  condition behind eq. (1), the new-review rating of Section IV.E, and every
  number in the text that follows from other printed numbers are Lean theorems
  in the paper's notation (LC-4). The coverage table in `CLAIMS.md` lists what
  is general and what is a special case. Our reading is consistent with the
  source on these points, with the readings LR-D1, LR-D2 and LR-D8.
- The WRAP accepted manuscript agrees with the article at the level of results
  (LR-D5).

## What could not be verified

- The proofs in the online appendix, Section F, and the appendix tables. They
  are not in the cached file.
- The estimates themselves. The record checks our reading of the reported
  numbers, not the data.

## The bearing on Paper A1

Revised on 16 October 2026 against the corrected account of A1. Nothing in the
source contradicts that account. One premise of A1, that imputed and actual
group means agree, is not shared, and it is taken last.

### The puzzle

The source says nothing about friending across class lines or about racially
diverse settings. The result nearest to contact is that the price penalty is
larger for shared flats than for entire flats, 0.082 against 0.013, with
slower learning, ρ of 22 against 12 (LR-Q12, `shared_flat_rho`). The authors
read it as a larger set of unobservables in shared space. Guests are barely
sorted by the host's group, 0.81 points (LR-Q13).

### Budget compatibility

The source has no account of homophily or assortative matching. Demand
`D = Q^β/(P^κ Γ^m)` rises in quality for every guest, and the guest's budget
and own type are not modelled (PDF p. 11). The small ethnic matching of guests
to hosts (LR-Q13) is reported and not explained. Nothing in the source bears on
the cost of a relationship across a class gap.

### Avoidance that depends on the observer's class

Neither direction can be read from the source. Guests are one type, "primarily
from the majority group" (LR-Q21), and the model has no term in the guest's own
type. Two findings limit what A1 can borrow. Minority guests do not rate
minority hosts differently (Table 8, PDF p. 21), so the guest's group does not
change the evaluation once the stay has happened. And minority hosts receive
slightly more minority guests (LR-Q13), a weak sorting that fits avoidance
running both ways but is too small and too unexplained to support it.

### The penalty vanishing when class is shown

This is the source's main statement and the link A1 wants to the
statistical-discrimination literature. The exact statement is that "the price
gap should decrease with the number of reviews and tend to zero, conditional on
observables and the measure of quality provided by the reviews" (LR-Q3), and
the finding is that the whole residual gap is statistical (LR-Q2). Its
conditions, each formalised, are these.

- No taste term. With `γ = 0` the gap between two listings of equal quality
  tends to zero (`gap_tendsto_zero`); with `λγ ≠ 0` it tends to `−λγ`
  (`control_taste_gap_survives`, LR-Q4).
- Equal type. The vanishing is for pairs of equal quality. Group averages keep
  the true difference at every `K` (LR-Q5, `group_average_gap_constant`).
- No other change within the listing. The slope in reviews isolates the
  statistical term only if `ζ` and `w` are fixed (`within_listing_difference`,
  `control_zeta_varies`).
- Information about the individual. The weight on the individual evidence is
  `K/(K + ρ)` (`postMean_eq`, `precision_weighted`), and `ρ` is learned only
  from listings with reviews (`rho_identified`, `control_rho_needs_reviews`).

In A1 the same property holds when class is shown. The marker then carries no
information the observer needs, and the group term `(c̄_g − c_i)² + σ_g²`
gives way to the individual's class. The source is the empirical counterpart
for listing quality, built on Altonji and Pierret (2001).

### The shadow market

The source is about a platform's information design, and three of its points
bear on the shadow market.

- The authors weigh hiding identity against disclosing more about the
  individual, and prefer disclosure (LR-Q16, LR-Q17). In their model hiding
  the marker removes the statistical penalty only if nothing visible predicts
  the group.
- Location is the group-neutral variable that carries the group. Location
  effects take the raw gap from 17 to 7 percent (LR-Q18), and because minority
  hosts live in less valued neighbourhoods the true penalty is larger than the
  3.2 percent measured within location (LR-Q19). Footnote 24 gives 5.5 percent
  lower prices per standard deviation of the local minority share (PDF p.
  24). The paper's design nets out exactly the channel the predecessor calls
  the shadow market.
- Our extension inside the source's posterior (LR-D10, not the authors'). With
  the marker hidden, a visible proxy carries the penalty in proportion to how
  well it separates the groups (`proxy_gap`). A perfect proxy restores the
  whole statistical penalty (`perfect_proxy_restores_gap`), and an
  uninformative one removes it (`control_uninformative_proxy`). This is the
  formal form of a correlated signal substituting for a banned criterion. It
  says nothing about legal strictness or social quality, the other comparative
  statics of the predecessor's prop:shadow.

### Premises of A1 the source does not share

- Consistent beliefs. A1 requires the imputed standing of a group to equal the
  actual mean of its visible members. Here guests' imputed mean for minority
  listings is below the platform's actual mean, and that error carries 2.5 of
  the 3.4 points (LR-Q6, LR-Q10, `decomposition`). The authors' explanation is
  selection onto the platform (LR-Q20), the gap between a group's mean and its
  visible members' mean that A1's condition rules out. A1 should say whether
  its condition is a long-run state or a premise.
- Vertical preference. Every guest ranks listings by quality alike (PDF p. 11).
- The unseen trait is a listing's quality, not the person's class (LR-D7).
- Learning over time. The penalty falls as reviews accumulate, a dynamic the
  static A1 does not have.

The sources it names and that are not read here are Cui, Li and Zhang (2020), on reviews reducing the
acceptance gap faced by guests with African-American names (PDF p. 4, [C]);
Altonji and Pierret (2001); Bordalo et al. (2016).

## Controls

- LC-2. A fabricated quotation, "If the price gap is due to taste-based
  discrimination, the price gap should decrease with the number of reviews and
  tend to zero", the opposite of LR-Q4, is not found. A true quotation is not
  found on a neighbouring page.
- Lean. `control_taste_gap_survives` shows the vanishing fails with a taste
  term. `control_equal_means_needed` shows `βm = β̃m` fails without equal true
  means. `control_printed_p0` shows the printed constant of eq. (1) is not the
  one the first-order condition gives. `control_rho_needs_reviews` shows `ρ` is
  not identified without reviews. `control_zeta_varies` shows the within-listing
  slope fails to isolate the statistical term when `ζ` moves.
  `control_uninformative_proxy` shows a hidden marker leaves no penalty when no
  visible variable predicts the group.
