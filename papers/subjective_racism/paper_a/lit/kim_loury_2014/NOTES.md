# Kim and Loury (2014), notes

## What was verified

- Seventeen quotations are found on the PDF pages they cite, by exact
  normalised containment in the text layer (LC-2).
- Every numbered result has a row in the coverage table of `CLAIMS.md`. Of
  the 40 rows, 28 are formalised in the paper's generality, 9 in a special
  case and 3 not at all (`lean/mathlib/KimLoury2014.lean`).
- One claim of the paper fails. On PDF p. 16 the paper says `πᵖ` falls with
  `δ`. That holds only with `v*` held fixed. Since `v*` rises with `δ`, `πᵖ`
  rises with `δ` whenever `ωPq > 0` and Lemma 1 holds (KLB-D10,
  `piP_strictMono_delta`, `piP_rises_example`). The claim on `π°` holds
  (`piO_strictMono_delta`). Nothing else in the paper depends on the `πᵖ`
  claim.
- Our reading is consistent with the source on every other formalised
  point.

## What could not be verified

- The results short of general, as listed at the end of `RECONSTRUCTION.md`.
- The published version (*IER* 2018) is not read (KLB-D1).
- The suite's axiom audit imports the compiled module. The project's own
  build directory holds an older compiled module, so the audit is run with
  the olean compiled into a scratch directory and a `lake` shim that runs
  `lean` with that path first. The coordinator's rebuild makes the shim
  unnecessary.

## The bearing on Paper A1

Nothing in the paper contradicts the corrected account of A1. The paper is a
labour-market model of statistical discrimination with dynamics. A1 is a
static model of contact. The record places the paper relative to A1 under
six headings.

1. **The puzzle.** The paper says nothing about friending across class lines
   or about racial diversity. It has no contact between members of different
   groups (KLB-Q14). It explains why ex-ante identical groups are treated
   differently (KLB-Q2, KLB-Q12, `lemma1`), which is a different question
   from why class-crossing friendship is rarer where groups are mixed. The
   general premise it states, that decision makers use group characteristics
   as a proxy for an unseen trait of the people they interact with (KLB-Q1),
   is A1's premise too, and the paper models it for employers only.
2. **Budget compatibility.** The paper assumes nothing about the origin of
   assortative matching. Employers are identical and meet workers at random
   (KLB-Q15). The only costs are the worker's investment cost and the
   employer's loss from an unqualified worker in Task One. Wages are
   exogenous (KLB-Q14). The paper says nothing about affordability or shared
   consumption, so it neither supports nor competes with budget
   compatibility as the source of class homophily.
3. **Avoidance that depends on the observer's class.** Not in the paper.
   The employer has no type of his own (KLB-Q15) and compares the posterior
   that a worker is qualified with one threshold `Π*` that is the same for
   every employer (KLB-Q10, `bod_iff`). Treatment runs one way, from employer
   to worker, so the paper can neither support nor limit direction (i), that
   avoidance runs both ways. On direction (ii), the paper shows only that
   pooling within a group moves value from its qualified to its unqualified
   members. An unqualified worker gains from his group's high reputation
   (`cor3_workers`), and a qualified one shares the penalty of a low one
   (KLB-Q4). Pooling of this kind is what would make the low-class member of
   a high group gain from being read by the group. The paper does not model
   that member's own choices.
4. **The penalty vanishing when class is shown.** Present, as an assumption
   in a labour market, with two exact statements. Once a worker's true
   qualification is revealed, at Poisson rate `η`, he is assigned by
   qualification alone (KLB-Q5), and the permanent-market term of eq. (3)
   contains no group variable (`eq3`). If age were observable, a cohort's
   investment would not depend on the group's reputation (KLB-Q6). In the
   temporary market the group enters only through the unclear test result.
   Pass and fail are treated alike across groups (KLB-Q10, `beta_eq_gap`).
   So the group penalty applies exactly where the individual signal is
   uninformative. A1's property, that the contact penalty vanishes when class
   is shown, is the same structure in contact, derived rather than assumed,
   and the paper is a citation for linking it to statistical discrimination.
5. **The shadow market.** Not in the paper. Its policies act on the formal
   screening rule, a training subsidy (eq. 20) and a hiring standard for the
   group (`lemma4`, `prop5`, `quota`), and employers comply by assumption
   (KLB-Q16). Nothing moves to a channel the policy does not reach, no
   group-neutral rule acts on a correlated variable, and no venue or
   intermediary trades social information. The paper's "patronizing"
   outcome (`prop5`) is a contracting-channel effect, in which a standard set too low
   lowers the return to investment. It is not a shadow-market effect, and
   A1 should not cite it as one.
6. **Premises of A1 the paper does not share.**
   - Identical fundamentals. The paper's groups differ only by equilibrium
     (KLB-Q2, KLB-Q12). A1's class distributions differ exogenously.
   - An individual signal. The employer sees a test about the worker
     (KLB-Q3), which A1's group sufficiency rules out for the marker.
   - A binary type and a payoff linear in the posterior (`bod_iff`). A1's
     observer weighs the squared class gap, so the group's spread `σ_g²`
     matters, and the paper has no counterpart to it.
   - Dynamics. The paper's results rest on a law of motion for the
     reputation (eq. 9) and expectations across cohorts (Theorem 2). A1 is
     static.

   A1 does share one premise, the consistency condition. The employers'
   belief about a group equals its true share of qualified workers (KLB-Q13),
   and a reputation is self-confirming when it reproduces itself (`lemma1`,
   `steady_states`). A1 imposes the same kind of condition on the mean class
   of a group's visible members, in a static game.

## Controls

- LC-2. A fabricated quotation, "a worker receives the benefit of the doubt
  whatever the employer believes about the rate of human capital
  acquisition", the opposite of KLB-Q11, is not found. A true quotation is
  not found on a neighbouring page.
- Lean. Each main result has a control that drops one condition.
  `control_beta_strictAnti` (eq. 2 without `Pq < Pu`),
  `control_low_not_equilibrium_without_threshold` and
  `control_low_not_equilibrium_cheap_cost` (Lemma 1),
  `control_steady_needs_threshold` (Theorem 1), `control_lemma2_cheap`
  (Lemma 2), `control_no_trap` (Proposition 1 and Theorem 2),
  `control_cor3_needs_threshold` (Corollary 3), `control_lemma4_T_zero`
  (Lemma 4) and `control_eq23_rises` (eq. 23 and Proposition 5). The second
  part of `prop5` is the paper's own failure case, the patronizing outcome.
