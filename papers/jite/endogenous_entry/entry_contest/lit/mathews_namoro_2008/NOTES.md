# Notes — Mathews and Namoro (2008), pass 1 (working paper, read in full)

Run `python3 lit/mathews_namoro_2008/verify_mathews_namoro.py` from the
repository root. It checks the quotations verbatim, on the page named, against
the cached text of the working paper (pinned by sha256), runs a reversed-quote
control, and measures `lean/mathlib/MathewsNamoro.lean` (see `LEAN`).

## Verdict for C5, C6 and C13

It is an entry paper only in a loose sense. The entry decision is which of two
events to join, there is no entry cost, the field of each event is fixed at
two, and wealth does not appear. So:

- **C5** (the sign rule of C1 is new): nothing in the paper signs a spread of
  wealth or of entry costs, or says how many enter.
- **C6** (count invariance with heterogeneous costs): the count of strategic
  entrants is fixed at two, so there is no count to be invariant.
- **C13** (the share of the prize taken by those who enter): there is no
  group of non-entrants who still compete. Every place is filled, by a
  strategic agent who chose its event or by a non-strategic agent N placed
  there by assumption. N agents can win but make no choice, and the paper
  does not analyse a share. C13's claim that those who do not enter cannot win
  therefore needs the exception that here N agents win without having chosen.

## What it is useful for

The result is a reversal in the sign of a prize effect. A larger prize in an
event does not always attract the stronger field, because it also attracts the
rival the stronger agent wants to avoid (allocation c). In the model here a
larger prize never lowers the number who pay: `k*` is non-decreasing in `V`
(M11), because `V` raises the gain from entering at every count. The
difference is that choosing between two events is not choosing whether to pay
a fee, and that their agents choose effort after entering.

## Findings from the formalisation

1. Equal effort is verified as a Nash equilibrium of the single tournament by
   a best-reply argument, not only as a first-order point, for the ratio
   success probability with `0 < z <= 1` (`single_nash`); uniqueness is not
   proved.
2. The printed form of Condition 1 at `z = 1` is not its value (MN-D1); the
   conclusion stands (`cond1_pos`).
3. A plus sign is missing on p.11 (MN-D2) and the range on p.11 reads
   `0 < alpha < 0` (MN-D3).
4. Under the intuitive restrictions, c never arises iff `eta'(1) <= 0`
   (`c_never_iff`); for every `z` this is Condition 2 and for some `z` it is
   Condition 3, and Condition 2 implies Condition 3 (`condition2_iff`,
   `condition3_iff`, `condition2_imp_condition3`).
5. The first-order example of Section 6.1 is checked as integrals of the
   printed distribution functions (`fosd_omega`, `fosd_lambda`, `fosd_rho`),
   not only as the three numbers.
6. In Appendix B the two minima and their order are proved (`b_min_cost`,
   `c_min_cost`, `c_cheaper_than_b`).
