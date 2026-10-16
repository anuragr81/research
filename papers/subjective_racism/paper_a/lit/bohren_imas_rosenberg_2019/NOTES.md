# Bohren, Imas and Rosenberg (2019), notes

## What was verified

- Eighteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the working paper's text layer (LC-2).
- The model of Section 2 is implemented in `lean/mathlib/BohrenEtAl2019.lean`
  in the paper's notation. Definitions 1 to 4, eqs. (1), (2), (4) to (7),
  Proposition 1, footnote 9, Lemmas 1 to 3, Proposition 2, the coarse model
  of Appendix A.3.1 at the first period, eq. (27), eq. (30) and the internal
  arithmetic of Tables 1 to 6 are Lean theorems. Proposition 3, Lemma 4 and
  the binary model are covered in special cases. The coverage table in
  `CLAIMS.md` gives the status of every numbered result. Our reading is
  consistent with the source on these points, with two exceptions. The claim
  after Definition 6 fails as printed (BIR-D8), and four table constants
  differ by rounding (BIR-D9).
- That discrimination falls in Proposition 2, which the paper leaves at a
  display, is proved (`prop2_D_decreases`). The closed form of the factor in
  eq. (12) is ours (BIR-D6).

## What could not be verified

- Proposition 3 in general, Proposition 4, Lemma 5 and the numerical
  robustness of Appendix A.2, which relies on Matlab code in an unread
  supplement.
- The experimental estimates themselves. No data were read.
- The published AER version (BIR-D1).

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1.

**The puzzle.** The source is silent on friendship, class and diversity. It
documents a belief-based group penalty on an online platform, about 0.4
standard deviations for novice questions, reversing to about 0.6 standard
deviations in favour of women at advanced accounts (BIR-Q14, BIR-Q15). It
supplies a model of how a group penalty depends on what is shown about the
individual, which is the half of A1's puzzle that concerns inference, and
nothing on why the penalty should rise with diversity.

**Budget compatibility.** Nothing in the source bears on cost, affordability
or shared consumption. The source does not model homophily or assortative
matching at all. Differential treatment has two assumed origins, a taste
`c_F` and a belief gap `μ̂_M − μ̂_F` (BIR-Q4), and neither is derived.

**Avoidance that depends on the observer's class.**
- The evaluator's own type enters only through her taste and her subjective
  means (BIR-Q3). Her payoff is accuracy about the worker, and nothing
  depends on the distance between her and the worker, so there is no
  analogue of A1's dependence on the observer's own class.
- Direction (i), avoidance running both ways. The source allows partiality in
  either direction across evaluators (BIR-Q16), but the direction is an
  individual error around the true mean, not tied to the evaluator's own
  group. It neither supports nor limits (i).
- Direction (ii), a low-class member of the high group preferring outsiders.
  Nothing in the source bears on it.
- The action is a continuous report equal to the posterior mean minus the
  taste (`eq2_optimal`), so the variance of the evaluator's belief changes
  her payoff and not her action (`eq1_expected_loss`). In A1 the action is
  binary against a threshold, so the group's variance `σ_g²` changes the
  action. A gender difference in the variance of ability appears only as an
  alternative model in words (BIR-Q13).

**The penalty vanishing when class is shown.** The exact statement is
Proposition 1 (BIR-Q7). As the signal becomes perfectly objective,
`τη → ∞`, there is initial discrimination if and only if the evaluator has
preference-based partiality (`prop1_limit`). The conditions are these.
- It concerns the first evaluation, with no history, in the normal model,
  and discrimination is measured between a man and a woman with the same
  signal (Definition 3, `eq5`).
- What becomes observable is the payoff-relevant object, the quality of the
  current task (BIR-Q8). Ability, the unseen trait the group predicts,
  stays unseen. The analogue in A1 is that class, the object of the
  engagement utility, is shown.
- The vanishing holds for biased beliefs as for correct ones, since
  `prop1_limit` does not need `μ̂_g = μ_g`. A taste survives
  (`control_taste_persists`, BIR-Q9).
- At a single precision a belief gap and a taste are observationally
  equivalent (BIR-Q6, `fn9_equivalence`). The paper separates them by
  varying precision (answers against questions) and separates correct from
  biased beliefs by reversals along histories (BIR-Q10, BIR-Q11, `prop2`).
A1 can cite Proposition 1 as the statistical-discrimination statement of its
property, with the difference that A1's observer chooses whether to engage
rather than reporting a value.

**The shadow market.** The source has no legal constraint and no venue. Two
points bear on the shadow market.
- Belief-based discrimination concentrates where judgement is subjective,
  and target groups respond by moving to output judged on objective criteria
  (BIR-Q17). Objective measures remove the belief-based penalty (BIR-Q18,
  `prop1_limit`). The predecessor's social quality, read from markers, is a
  subjective channel in this sense, which is where the source predicts
  belief-based discrimination to persist.
- Reputation is social information produced by earlier evaluators and read
  by later ones, and its meaning depends on what readers believe about
  those evaluators (`invert`, `control_reversal_misspecified`). The platform
  is an intermediary of social information, but the source does not model
  its design or any market for that information.

**Premises of A1 the source does not share.**
- Group beliefs may be wrong, `μ̂_g ≠ μ_g` (BIR-Q3). A1's imputed group
  standing equals the actual mean of the visible members.
- Group means are exogenous. Nothing in the source makes them equilibrium
  objects.
- The source is dynamic, with learning along a history (`lemma1`, `prop2`).
  A1 is static.
- The decision is a continuous evaluation, not a choice of partner.

## Controls

- LC-2. A fabricated quotation, "Then fixing an evaluation history,
  discrimination decreases across periods and eventually reverses.", the
  opposite of BIR-Q10, is not found. A true quotation is not found on a
  neighbouring page.
- Lean. `control_taste_persists` (Proposition 1 needs `c_F = 0` for the
  vanishing), `control_reversal_misspecified` (Proposition 2 needs a single
  type with common knowledge, outside the paper's `p ∈ (0, 1)` as a
  degenerate case), `control_D_rises_without_shrinking` (the decrease of
  discrimination needs Lemma 2), `control_coarse_taste` (the coarse model's
  limit needs `c_F = 0`), `control_stereo_distorts` (eq. (30) moves the
  belief only for `θ > 0`) and `control_shifting_difference_depends_on_v`
  (BIR-D8).

## Suite status

The suite ends with 11 checks and 0 failures when the edited module is
compiled into the scratch directory and the axiom audit is pointed at it,
with all 41 cited theorems depending only on `propext`, `Classical.choice`
and `Quot.sound`. Until the coordinator rebuilds the library, the audit run
through `lake env` finds the older build and audits only the theorems it
contains.
