# Kim and Loury (2009), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the text layer,
  by exact normalised containment (LC-2).
- The cached PDF and text match their pinned sha256 (LC-3).
- The one-period structure of the simple model (Assumption 1, the BOD
  threshold `Π*` in both forms, the two values of `β`, the monotonicity of
  `β` and of individual reputation, the two steady states, the employer half
  of Proposition 3), the trace and determinant of Lemma 4, the discriminant
  of Lemma 5 and the market-learning threshold of Appendix A are Lean
  theorems (LC-4).
- Our reading is consistent with the source on these points, after the
  readings KLA-D1 to KLA-D5.

## What could not be verified

- Lemmas 1 and 2, the overlap formula (12) and the signs of Proposition 4 rest
  on phase-diagram arguments and are not formalised.
- Lemma 3 as printed does not follow from the printed `π^o` and `π^p`
  (KLA-D2).
- The profit integrals (13) and (14) of Proposition 5 are not checked.
- The comparison with the 2014 version (KLA-D9) is a term search of its text
  layer, not a reading. The 2014 version has its own record.

## The bearing on Paper A1

Nothing in the source contradicts the A1 model. The source differs from A1 in
its setting (wages, not contact), in its evidence (a signal about the
individual alongside the group, KLA-Q2, where A1 has group sufficiency) and
in having a law of motion, which A1 does not.

1. **Group avoidance from class homophily under inference.** Absent.
   Employers are identical (KLA-Q3), so no preference over groups can depend
   on the observer's own type. Group reputation is a mean, the proportion
   qualified (KLA-Q1), with a binary type, so no within-group variance enters
   any payoff. The group penalty works through the employer's threshold
   `Π*` (`bod_iff`), which compares the group mean with a cost ratio, the
   same for every employer.
2. **Budget compatibility as the micro-foundation of class homophily.**
   Absent. Nothing in the paper concerns budgets or the cost of contact.
3. **Venue exclusion and price screening.** Absent as a model. Section 5's
   coordinated employers change the standard for a whole group to raise its
   investment (KLA-Q11), which is a policy and not exclusion by a venue or a
   price. The paper proposes, without a model, a housing application in which
   residents use the group's quality to predict prices (KLA-Q12), and a
   college application in which reputation affects entrants and their
   willingness to pay tuition (KLA-Q13). Both are pointers A1 could cite as
   the nearest the paper comes to contribution 3.
4. **The group penalty vanishes when class is shown.** Present in a labour
   setting. The paper states that if investment can be confirmed, the group's
   reputation no longer matters (KLA-Q6, `revealed_type_no_group_effect`),
   and Appendix A derives that fast revelation removes the low state and the
   disparity (KLA-Q14, `market_learning_threshold`). A1's test in contact is
   the analogue of this, so A1 should cite it as prior form, with the
   difference that A1's penalty comes from class homophily and not from an
   employer's return on qualified workers.

Two further points. The equilibrium consistency on group means that A1
imposes has a counterpart here, where employers read the group's actual
proportion qualified from the aggregate signals (KLA-Q5, eq. 2). In the
source this is an informational assumption, and the steady states coincide
with Coate and Loury's (footnote 2). Passing does not appear, since group
identity is fixed. Group disparity arises with identical primitives
(KLA-Q10).

## Controls

- LC-2. A fabricated quotation, "if the market can learn the true
  characteristics of workers very quickly, the group disparity caused by the
  difference of initial group reputations will persist", the opposite of
  KLA-Q14, is not found. A true quotation is not found on a neighbouring page.
- Lean. `control_no_low_state_if_cutoff_low` shows the low state needs
  `(δ + λ)cm > ωPq`. `control_equal_informativeness` shows that without
  `Pq < Pu` reputation does not move the incentive to invest.
  `control_employer_ranking_needs_threshold` shows the employer ranking of
  `Qh` over `Ql` needs `Πh > Π*`.

## Page images read

The 110 dpi images of PDF pp. 6, 7, 11, 12, 16, 17, 20 and 21 were rendered
from the pinned PDF with `pdftoppm`. They are reproducible from it and not
pinned separately.
