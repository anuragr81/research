# Austen-Smith and Fryer (2003), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the text layer (LC-2).
- The acceptance condition of Lemma 2 and eq. (11), the role of assumption
  (7), the dilution of a pool under a uniform prior, the honesty condition of
  Proposition 8 and the threshold of Proposition 9 are Lean theorems (LC-4).
- Our reading is consistent with the working paper on these points.

## What could not be verified

- The printed threshold of Proposition 9 for `γ > 0` (AS-D2). The threshold
  derived from the paper's own payoffs is adopted.
- The equilibrium results that rest on the D1 refinement.
- Nothing is attributed to the published 2005 version, which is not read.

## The bearing on Paper A1

Nothing in the source contradicts the A1 model. In this model the unseen type
is read from a chosen signal, education, and not from a group marker. Group
membership is not a marker in the model. Firms ignore it and the group does
not condition on it (AS-Q2). The peer group is an audience that accepts or
rejects, and race enters only in the conclusion, as which individuals face
that audience (AS-Q13).

1. **Group avoidance from class homophily under inference (contribution 1).**
   Not in the source. The group's acceptance depends on the posterior mass
   of reliable types, `F(θ̂|s) ≥ F₀` (AS-Q6, `lemma2_accept_iff`), and the
   group has no type of its own, so there is no observer-dependent gap. The
   criterion is linear in that posterior mass, so the spread of types in a
   pool does not enter except through the mass below `θ̂`. The spread of
   accepted types is an outcome that rises with patience (AS-Q11), not a
   penalty in the group's choice. The nearest point to A1's σ² term is the
   authors' remark that pooling gives some justification for Phelps's
   assumption that minority ability is perceived more noisily (AS-Q12). That
   remark makes the noise an outcome of pooling, stated without derivation,
   and not an avoidance of noisy groups.
2. **Budget compatibility (contribution 2).** Not in the source. Group
   membership costs effort, not money, and the group has no interest in
   income (AS-Q2).
3. **Venue exclusion and screening (contribution 3).** The peer group is a
   club that screens on an unobserved type (AS-Q3), with an acceptance
   threshold on a posterior (AS-Q6). This is screening on unobserved type, a
   form contribution 3 must cite. It differs from A1's venue in three ways.
   The screen is a single group's decision, not the aggregate of clients'
   inferences. The signal is education chosen by the individual, not a price
   charged by a venue. The club screens on reliability, which the paper makes
   unrelated to income, rather than on class. The pool's inclusion of
   unreliable types (AS-Q9, `pool_admits_unreliable`) is the analogue of
   A1's venue admitting members whose class the venue cannot see.
4. **The test of the class channel (contribution 4).** Not in the source.
5. **The equilibrium consistency condition on group means.** The source has
   one. The pooled wage is the conditional mean of output over the pooled
   types, eq. (8) with the same belief held by firms and group (AS-Q5), and
   the proof of Proposition 5 writes it as the average `ω(s; θ′)` over the
   pool (PDF p. 32). This is the counterpart of A1's requirement that the
   imputed standing equal the actual mean of the visible members, with the
   pool defined by a chosen signal rather than by a marker.
6. **Passing.** The high types who separate and leave the pool (AS-Q10) are
   the counterpart of passing, and the conclusion notes that a wage rise for
   separators makes the marginal member break away (PDF p. 25), which lowers
   the pool's mean. This is the known result A1 does not claim (Kim and Loury
   2012).

What A1 should cite. AS-Q1 and AS-Q5 for a type read from one signal by two
audiences with one posterior. AS-Q3 and AS-Q6 for a club that screens on an
unseen type by a threshold on a posterior. AS-Q14 for the same signal carrying
a different meaning by group, which the authors tie to Milgrom and Oster
(1987).

## Controls

- LC-2. A fabricated quotation, "firms are assumed to care about each
  employee's group membership", the opposite of AS-Q2, is not found. A true
  quotation is not found on a neighbouring page.
- Lean. `control_no_assumption7_all_accepted` shows that without (7) every
  belief leads to acceptance, as AS-Q7 says.
  `control_separation_rejects` shows that without pooling a type above `θ̂`
  is rejected, so the acceptance of unreliable types needs the pool.

## Suite status

The coordinator adds `AustenSmithFryer2003` to the build roots. Until then
LC-4 fails its build-root check, and the axiom audit, which imports the
module, cannot run. The audit was run by hand on a scratch copy of the file
with `#print axioms` appended for every cited theorem, and every theorem
depends only on `propext`, `Classical.choice` and `Quot.sound`.
