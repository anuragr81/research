# Bohren, Imas and Rosenberg (2019), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the working paper's text layer (LC-2).
- Eq. (5), Proposition 1 with its limit, footnote 9, eq. (12), eq. (13),
  Lemma 2 and Proposition 2 are Lean theorems in the paper's notation for the
  normal model (LC-4). Our reading is consistent with the source on these
  points. The closed form of the factor in eq. (12) is ours (BIR-D6).

## What could not be verified

- Proposition 3 in general, Proposition 4 and the numerical robustness of
  Appendix A.2, which relies on Matlab code in an unread supplement.
- The experimental estimates themselves. Tables 1 and 2 are transcribed from
  the text layer, and nothing is recomputed. No data were read.
- The published AER version (BIR-D1).

## The bearing on Paper A1

Nothing in the source contradicts the A1 model. One modelling difference
matters for contribution 1 and is stated first. The evaluator's action is a
continuous report equal to the posterior mean of quality minus the taste
(eq. 2), so the variance of the evaluator's belief changes the level of her
payoff and not her action. In A1 the action is binary, engage or not, and the
expected squared gap including the group's variance `σ_g²` is compared with
a threshold, so the variance changes the action. The two models differ in
the action, not in the loss.

1. **Group avoidance from class homophily under inference (contribution 1).**
   Not in the source. The evaluator's own type enters only through her taste
   `c_{ig}` and her subjective mean `μ̂_{ig}` (BIR-Q3). The payoff
   `−(v − (q − c))²` is a loss on accuracy about the worker, and nothing in it
   depends on the distance between the evaluator's and the worker's
   attributes. There is no partner choice. A gender difference in the
   variance of ability appears only as an alternative model, where it makes
   discrimination non-monotonic in the signal (BIR-Q13), and the authors
   find no such difference in the forum data. It is not a penalty on a
   group's spread.
2. **Budget compatibility (contribution 2).** Not in the source.
3. **Venue exclusion and price screening (contribution 3).** Not in the
   source. Reputation is a public history of evaluations that is read by
   later evaluators, not a venue or a price.
4. **A test of the class channel (contribution 4).** The form of the test is
   in the source. Belief-based discrimination falls as the signal about the
   individual becomes more precise and vanishes when quality is perfectly
   observable, while preference-based discrimination does not (BIR-Q7 to
   BIR-Q9, `prop1_limit`, `control_taste_persists`). The authors take it to
   the field by comparing answers, judged on correctness, with questions,
   judged subjectively (Table 1). A1's test applies the same logic with class
   as the unseen trait and contact as the decision. What A1 adds is the
   setting, contact with an unseen class behind a group marker, rather than
   the logic. Two further points bear on how A1 states its test.
   - The vanishing holds for biased beliefs as well as for correct ones
     (`prop1_limit` does not need `μ̂_g = μ_g`). Showing class therefore
     separates a belief channel from a taste and does not separate accurate
     from inaccurate class beliefs. The source separates those by dynamics
     (BIR-Q10, BIR-Q11).
   - The source notes that distorted stereotypes have often been classed as
     tastes (BIR-Q5). A1 has to say that its class channel is a belief and
     that the test treats biased and unbiased class beliefs alike.

What A1 should cite. BIR-Q7 to BIR-Q9 as the precedent for a test that
removes a belief-based group penalty by showing the trait. BIR-Q6 and
`fn9_equivalence` for why a single cross-section cannot separate a belief
from a taste. BIR-Q14 and BIR-Q15 if A1 needs a field magnitude for a
belief-based penalty and its reversal.

## Controls

- LC-2. A fabricated quotation, "Then fixing an evaluation history,
  discrimination decreases across periods and eventually reverses.", the
  opposite of BIR-Q10, is not found. A true quotation is not found on a
  neighbouring page.
- Lean. `control_taste_persists` shows that the vanishing of discrimination
  as the signal becomes objective fails once a taste is present.
  `control_reversal_misspecified` shows that the no-reversal of Proposition
  2 fails once an evaluator attributes an evaluation to an evaluator with a
  different belief, which is the single-type assumption dropped. This control
  is the degenerate case in which the impartial evaluator is sure the first
  evaluator was heuristic, outside the paper's `p ∈ (0, 1)`.

## Suite status

The coordinator adds `BohrenEtAl2019` to the build roots. Until then LC-4
fails its build-root check. The module was compiled into the scratch
directory with `lake env lean -o`, and with that directory on `LEAN_PATH`
the suite's axiom audit passes for all twelve cited theorems, which depend
only on `propext`, `Classical.choice` and `Quot.sound`.
