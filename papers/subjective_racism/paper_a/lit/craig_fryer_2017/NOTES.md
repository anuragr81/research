# Craig and Fryer (2017), notes

## What was verified

- Fifteen quotations are found on the PDF pages they cite in the publisher's
  text layer, by exact normalised containment (LC-2).
- The posterior and the offer and application rule, the independence of the
  match gain from one's own investment, and the Section 3.1 example with its
  three equilibria are Lean theorems in the paper's notation (LC-4). Our
  reading is consistent with the source on these points.

## What could not be verified

- Propositions 1 to 10 in general, the stability condition of Section 3.F and
  the wage-regression algebra of Section 6. They are read and not formalised.
- Typos in the appendix are recorded as CF-D3 to CF-D6 and no row relies on
  the passages they affect.

## The bearing on Paper A1

Nothing in the source contradicts the A1 model. The two models differ in the
preference and in the source of the group's mean, as set out below.

How the two-sided inference works. Each side holds a prior about the other
side's group, `π_j` for the employer and `δ_j` for the worker, and updates it
on an individual signal by Bayes' rule (CF-Q1, CF-Q3, `offer_iff`). A match
needs the worker to apply and the employer to offer. So engagement needs both
sides to agree, as in A1, and each side's acceptance is a threshold in its
posterior. The signal is individual evidence, which A1's group sufficiency
rules out. With an uninformative signal the posterior is the group prior
(`post_uninformative`), which is the A1 case.

Whether each side's preference depends on its own type. It does not. A
worker's gain from a job is `ω_q` or `−ω_u` whatever her own investment
(`match_gain_indep_own`), the employer's is `χ_q` or `−χ_u` whatever its own,
and the appendix states this as an assumption needed to replicate the
baseline (CF-Q10). Every agent ranks partners the same way, by whether the
partner invested. A1's utility `v − κ(c_i − c_j)²` makes the ranking depend on
the observer's own class.

What the equilibrium is. A pair of beliefs such that each side's prior about a
group equals the investment rate the other side's best responses produce
(eqs. 3 and 4, CF-Q4, CF-Q5). This is a consistency condition on a group mean,
of the same kind as A1's condition that the imputed group standing equal the
actual mean of the visible members. In Craig and Fryer the group's mean moves
through investment, and group membership is fixed (CF-Q2). In A1 it moves
through who passes. The equilibria are multiple. In the uniform example they
are exactly `Z`, `U` and `S` (`three_equilibria`, CF-Q9), and the zero
equilibrium needs both sides' pessimism (`control_one_sided_zero_fails`,
CF-Q6).

The candidate contributions of A1 are taken in turn.
1. **Group avoidance from class homophily under inference.** Not in the
   source. The group penalty comes from a lower prior that a member invested,
   and it is the same for every observer, because preferences are vertical
   (CF-Q3, CF-Q10). The source's within-group variance sets the weight on the
   individual signal and the slope of wages in ability (CF-Q11, CF-Q12), as in
   Phelps (1972). It does not make a group with more spread less likely to be
   matched, so A1's σ² term has no counterpart here.
2. **Budget compatibility as the source of class homophily.** Not in the
   source.
3. **Venue exclusion and price screening.** Not in the form A1 has it. The
   nearest object is the firm's group-specific investment, which a firm
   withholds when it believes the group's workers did not invest and few of
   them apply (eq. 2, CF-Q14). That is one firm's response to its own
   inference about workers, not an aggregate of clients' inference about each
   other, and no price screens anyone. The minority disadvantage of CF-Q7
   comes from a fixed cost and disappears with costs per hire (CF-Q15).
4. **A test of the class channel.** Not in the source. The proposed test asks
   whether employers' shrinkage toward the group mean lowers the return to
   ability more for black workers (Section 6, CF-Q11). It does not ask whether
   the group penalty vanishes when the type is shown.

What A1 should cite the source for. The two-sided structure of inference with
mutual consent (CF-Q1), the self-confirming equilibrium on group rates (CF-Q4,
CF-Q5), and the authors' own extension of the two-sided reading to settings
outside labour (CF-Q13). The contrast A1 can state is that Craig and Fryer's
partners are ranked the same way by everyone, while A1's are ranked by
distance in class.

## Controls

- LC-2. A fabricated quotation, "the benefit to workers from being matched to
  a good firm depends on their own investment decision", the opposite of
  CF-Q10, is not found. A true quotation is not found on a neighbouring page.
- Lean. `control_one_sided_zero_fails` shows the zero equilibrium fails when
  firm investment is held positive, and `control_minority_breaks_U` that the
  equilibrium `U` fails when the group share falls from 1/2 to 1/4.
