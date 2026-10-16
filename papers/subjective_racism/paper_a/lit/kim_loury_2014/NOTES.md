# Kim and Loury (2014), notes

## What was verified

- Fourteen quotations are found on the PDF pages they cite, by exact
  normalised containment in the text layer (LC-2).
- Eq. (2) derived from the test structure, the threshold `Π*` and the
  employer's rule, Lemma 1, and the equivalences of eqs. (7) and (19) are Lean
  theorems in the paper's notation (`lean/mathlib/KimLoury2014.lean`).
- The integrals behind eqs. (10), (17) and (18), the quota standard
  `θ̃* = θq(1 − Π_h)/(1 − Π_l)`, eq. (23) and the condition
  `Pq < Pu ⟺ θq + θu > θ̄` were checked by hand.
- Our reading is consistent with the source on these points.

## What could not be verified

- The dynamics beyond the closed forms, as listed at the end of
  `RECONSTRUCTION.md`.
- The published version (*IER* 2018) is not read (KLB-D1).
- The suite's build-root check fails because `KimLoury2014` is not yet a root
  in `lakefile.toml`, which this record does not edit. The suite's axiom audit
  fails for the same reason, since it imports the module and finds no compiled
  module. The audit was run on a copy of the file with `#print axioms`
  appended. Every one of the twelve cited theorems depends only on `propext`,
  `Classical.choice` and `Quot.sound`.

## The bearing on Paper A1

Nothing in the paper contradicts A1's model. The paper and A1 differ in
setting and in the observer's payoff, and the differences are listed below.

1. **Group avoidance from class homophily (candidate 1). Not in the paper.**
   The employer's criterion is linear in the posterior probability that the
   worker is qualified, and it compares that probability with one threshold
   `Π*` that is the same for every employer (KLB-Q10, KLB-Q11, `bod_iff`). The
   type is binary, the employer has no type of his own, and the group enters
   only through its share of qualified workers. A1's observer weighs the
   squared class gap `(c̄_g − c_i)² + σ_g²`, so the observer's own class and
   the group's spread both matter. The paper has neither.
2. **Budget compatibility (candidate 2). Not in the paper.** Wages are
   exogenous, and the paper says inter-group interactions are not examined
   (KLB-Q14).
3. **Venue exclusion and price screening (candidate 3). Not in the paper.**
   The paper's screen is an employer's test with a group-specific hiring
   standard (Appendix A, eq. 26, and the policy of Proposition 5). No venue,
   club or price sorts people by an unseen type.
4. **The penalty vanishes when class is shown (candidate 4). Present as an
   assumption in a labour market.** In the permanent market the true type is
   known and assignment follows it alone (KLB-Q5). The group's reputation
   matters only while type and age are unseen (KLB-Q4, KLB-Q6). A1's
   candidate 4 is the contact analogue, offered as a test. The paper builds
   the vanishing into its two markets and does not offer it as a test or
   derive it. A1 should cite the paper for the structure and claim novelty only
   for the contact setting and for the test.
5. **The consistency condition. Present, and A1 should cite it.** Employers'
   belief about a group equals the group's true share of qualified workers
   (KLB-Q13), and a self-confirming reputation is a fixed point of the map from
   reputation to rule to investment (`lemma1`). A1's condition that the
   imputed group standing equal the mean of the visible members is the same
   kind of condition on a different statistic and in a static game.
6. **The group as a proxy (KLB-Q1).** The paper states the premise for
   decision makers who interact with individuals in general, and models it for
   employers only. Its employer also sees a test signal about the individual
   (KLB-Q3), which A1's group sufficiency rules out for the marker.
7. **Where inequality comes from.** In the paper the groups have identical
   fundamentals and differ by equilibrium (KLB-Q2, KLB-Q12). In A1 the groups'
   class distributions differ exogenously. A1 cannot borrow the paper's
   multiplicity argument without that change being stated.
8. **Dynamics.** The paper's contribution is the law of motion, the overlap
   and the reputation trap (Theorem 2). A1 has no law of motion, so these
   results are outside A1 and not contrasted with it.

## Controls

- LC-2. A fabricated quotation, "a worker receives the benefit of the doubt
  whatever the employer believes about the rate of human capital
  acquisition", the opposite of KLB-Q11, is not found. A true quotation is not
  found on a neighbouring page.
- Lean. `control_beta_strictAnti` shows that the return rises with the benefit
  of the doubt only under `Pq < Pu` (KLB-Q9).
  `control_low_not_equilibrium_without_threshold` and
  `control_low_not_equilibrium_cheap_cost` show that each condition of
  Lemma 1 is needed for `Π_l` to reproduce itself.
