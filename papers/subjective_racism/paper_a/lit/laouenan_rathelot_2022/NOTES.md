# Laouénan and Rathelot (2022), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the publisher's
  text layer, by exact normalised containment (LC-2).
- The posterior mean and its rewriting, the half-information property of `ρ`,
  the gap between two listings of the same quality and its limit with and
  without a taste term, the identification of `βm` and `β̃m` in eqs. (3) and
  (4), the decomposition of Section III.B, and the host's first-order
  condition behind eq. (1) are Lean theorems in the paper's notation (LC-4).
  Our reading is consistent with the source on these points, with the sign
  readings LR-D1 and LR-D2.
- The WRAP accepted manuscript agrees with the article at the level of results
  (LR-D5).

## What could not be verified

- The proofs in the online appendix, Section F, and the appendix tables. They
  are not in the cached file.
- The estimates themselves. The record checks our reading of the reported
  numbers, not the data.

## The bearing on Paper A1

**What sits against the A1 model, stated first.** A1's equilibrium requires
the imputed standing of a group to equal the actual mean of its visible
members. In this market the guests' imputed mean for minority listings is
below the actual mean of minority listings on the platform, and that gap
carries 2.5 of the 3.4 points of the statistical penalty (LR-Q6, LR-Q10,
`decomposition`). The authors' explanation is differential selection into the
platform (PDF p. 14). Minority listings are more selected, so a belief formed
about the group at large understates the mean of the members who are visible
on Airbnb. That is the gap between a population mean and a visible-members
mean that A1's condition rules out. The unseen trait here is listing quality
rather than class (LR-D7), and the panel covers the platform's early years, so
the finding does not refute A1's equilibrium. It does show that the
consistency condition can fail where a marker penalty is measured, and A1
should say whether its condition describes a long-run state or a premise.

How the inference works. The guest sees the host's ethnicity and the listing's
observable part `ζ`. With no reviews the guest's expectation of the unseen
part is the group's prior mean (`postMean_no_reviews`, LR-Q8), which is A1's
group sufficiency in a market for a good. Reviews are evidence about the
individual listing, and they move the expectation toward the listing's own
quality at rate `K/(K + ρ)`.

The candidate contributions of A1, in turn.

1. **Group avoidance from class homophily under inference.** Not in the
   source. Demand rises in quality for every guest (`D = Q^β/(P^κ Γ^m)`, PDF
   p. 11), so every guest ranks listings alike and the penalty does not depend
   on the guest's own type. The within-group variance `σ_ν²` is common to both
   groups and does not enter the price, which is linear in the posterior mean.
   It enters only through `ρ`, the speed of learning. So A1's σ² term has no
   counterpart. The one result that touches contact is that the penalty is
   larger for shared flats than for entire flats, 0.082 against 0.013 (LR-Q12),
   which the authors attribute to a larger set of unobservables.
2. **Budget compatibility as the source of class homophily.** Not in the
   source.
3. **Venue exclusion from clients' inference.** Not in the source. The marked
   party sets the price, so the price is a response to inference rather than a
   screen. Guests are barely sorted by host group, 0.81 points (LR-Q13).
4. **A test of the class channel.** The form of the test is here, for a market
   rather than for contact. The penalty on the marked group falls toward zero
   as information about the individual accumulates, conditional on a proxy for
   true quality (LR-Q2, LR-Q3, `gap_tendsto_zero`), and the stated rival is a
   taste penalty that stays (LR-Q4, LR-Q7, `control_taste_gap_survives`). The
   design is Altonji and Pierret (2001) adapted (PDF p. 2). So a test in which
   a group penalty vanishes once the unseen type is shown is an established
   empirical design. A1's contribution 4 is new only as a test applied to
   contact with class as the unseen type. Two features of the source should
   carry over to A1's statement of the test. The vanishing holds for pairs of
   equal type and not for group averages (LR-Q5, `group_average_gap_constant`),
   so A1 should state its test conditional on shown class. And the design
   identifies only the part of the penalty that moves with information. A
   level penalty, taste or an unobserved differential, is absorbed and cannot
   be ruled out (LR-Q9, LR-Q14).

What A1 should cite the source for. Evidence that a penalty on a group marker
on a platform is informational and erodes with individual information (LR-Q2),
the split between inaccurate beliefs and a true difference (LR-Q10), and the
larger penalty where host and guest share space (LR-Q12). Sources it names that
bear on A1 and are not read here are Cui, Li and Zhang (2020), on reviews
reducing the acceptance gap faced by guests with African-American names (PDF
p. 4, known only through this citation, [C]); Altonji and Pierret (2001);
Bordalo et al. (2016).

## Controls

- LC-2. A fabricated quotation, "If the price gap is due to taste-based
  discrimination, the price gap should decrease with the number of reviews and
  tend to zero", the opposite of LR-Q4, is not found. A true quotation is not
  found on a neighbouring page.
- Lean. `control_taste_gap_survives` shows the vanishing fails with a taste
  term. `control_equal_means_needed` shows `βm = β̃m` fails without equal true
  means. `control_printed_p0` shows the printed constant of eq. (1) is not the
  one the first-order condition gives.
