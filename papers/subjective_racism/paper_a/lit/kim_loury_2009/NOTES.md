# Kim and Loury (2009), notes

## What was verified

- Twenty quotations are found on the PDF pages they cite in the text layer,
  by exact normalised containment (LC-2).
- The cached PDF and text match their pinned sha256 (LC-3).
- The model is implemented in Lean, the static stage of section 2, the
  dynamic system of section 3.2, the simple model and its phase analysis in
  section 3.3, the linearisation of section 3.4, the strategies of section 5
  and Appendix A (LC-4). The coverage table in `CLAIMS.md` gives 34 results
  in the paper's generality, 7 in a special case and 6 not formalised.
- Our reading is consistent with the source on these points, after the
  readings KLA-D1 to KLA-D5 and KLA-D10.

## What could not be verified

- Theorem 2 and the rise of `Πh`, `Πl` with `η` in the general model rest
  on figures and on the stable manifold theorem.
- Lemma 3 as printed does not follow from the printed `π^o` and `π^p`
  (KLA-D2). The form that does follow is proved.
- Lemma 4 takes the sign of `β′φ′ − 1` at each steady state from Figure 5.
- The comparison with the 2014 version (KLA-D9) is a term search of its text
  layer, not a reading. The 2014 version has its own record.

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1. The source is
a model of the contracting channel, in which employers assign workers to
jobs, and it says nothing about contact.

1. **The puzzle.** The source does not explain why friending bias across
   class lines is higher in racially diverse settings, and its mechanism
   cannot. Each group's reputation evolves on its own (KLA-Q20,
   `two_steady_states` holds group by group), so the mix of groups in a
   setting plays no role. What the source does explain is persistent group
   disparity with identical primitives (KLA-Q10), through self-fulfilling
   investment rather than through anyone's preference over contacts.
2. **Budget compatibility.** The source neither supports nor contradicts it.
   Workers meet employers at random (KLA-Q19), so no assortative matching or
   homophily is assumed or derived. The only costs are costs of investing in
   qualification (`Invests`), not costs of a shared activity.
3. **Avoidance that depends on the observer's class.** The source has no
   observer type, since employers are identical (KLA-Q3), so it bears on
   neither direction (i) nor direction (ii). One finding sits next to
   direction (ii) without touching it. A low type in a group at the high
   state gains from the group's reputation, because the benefit of the
   doubt reaches it too (KLA-Q18, `prop3_workers`). That gain is in the
   contracting channel. Direction (ii) concerns whom a low-class member of
   the high group prefers to meet, which the source does not model.
4. **The penalty vanishing when class is shown.** Present, in the contracting
   channel, in two forms. In words, if investment "can be easily confirmed",
   the group's reputation no longer affects a worker's chance of the good job
   (KLA-Q6). As a result, fast revelation of type removes the low state, and
   with it the group disparity, once `η > η*` with `(δ + λ)cm < ω` (KLA-Q14,
   `market_learning_threshold`). In the one-period model a signal that
   reveals type, `Pq = Pu = 1`, makes the benefit of investing independent
   of reputation (`revealed_type_no_group_effect`). Equal but imperfect
   informativeness, `Pq = Pu < 1`, does not suffice. It removes reputation
   from the incentive to invest (`control_equal_informativeness`), but
   employers still give the benefit of the doubt by group (`bod_iff`). A1
   can link its property to this source as the statistical-discrimination
   form of the same statement, in which the group penalty runs through an
   employer's return on qualified workers rather than through class
   homophily.
5. **The shadow market.** The source does not model it. It names
   "colorblind hiring enforcement" among remedies (KLA-Q16) and leaves its
   effects for further research (KLA-Q17), so nothing in it says whether a
   group-neutral rule moves screening to signals correlated with group. The
   coordinated employers of section 5 change the group-specific threshold
   in the contracting channel (KLA-Q11, `prop5_threshold_path`), which is
   preferential treatment and not a shadow market. Two remarks come close to
   it without a model. In housing, residents use the incoming group's
   quality to forecast prices (KLA-Q12). In colleges, the alumni's quality
   sets the institution's reputation, which then shapes who enters and what
   they will pay (KLA-Q13). Both place group reputation in a location or
   venue choice that works through price, which is the ground of A1's
   shadow market. Neither connects the choice to anti-discrimination law,
   so A1 can cite them as pointers to that ground, not as support for the
   claim that law displaces discrimination into it.
6. **Premises of A1 the source does not share.**
   - The employer sees a signal about the individual as well as the group
     (KLA-Q2, eq. 3), where A1 has group sufficiency.
   - Type is binary and group reputation is a mean, the proportion
     qualified (KLA-Q1), where A1 has class with a within-group variance.
   - Employers are identical (KLA-Q3), where A1's observers differ by class.
   - Reputation is read from aggregate signal data (KLA-Q5, `eq2`). A1
     instead imposes the equality of imputed and actual group standing as
     an equilibrium condition.
   - The model has a law of motion with forward-looking cohorts
     (`theorem1_loci`, `prop2_Qh`), where A1 is static.
   - Group membership is fixed, so there is no passing.

## Controls

- LC-2. A fabricated quotation, "if the market can learn the true
  characteristics of workers very quickly, the group disparity caused by the
  difference of initial group reputations will persist", the opposite of
  KLA-Q14, is not found. A true quotation is not found on a neighbouring page.
- Lean. Each main result has a control that drops one condition.
  `control_no_low_state_if_cutoff_low` (the steady states),
  `control_equal_informativeness` (Assumption 1),
  `control_employer_ranking_needs_threshold` and `control_prop3_medium`
  (Proposition 3), `control_prop2_needs_vStar` (Proposition 2),
  `control_lemma1_vortex` (Lemma 1, at `δ = 0`), `control_lemma2_sign`
  (Lemma 2), `control_prop4_degenerate` (Proposition 4), `control_lemma4`
  (Lemma 4) and `control_lemma5` (Lemma 5). Proposition 1, Lemma 3 and the
  section 5 timings are characterisations or computations, and have no
  control.

## Page images read

The 110 dpi images of PDF pp. 6, 7, 11, 12, 16, 17, 20 and 21 were rendered
from the pinned PDF with `pdftoppm`. They are reproducible from it and not
pinned separately.
