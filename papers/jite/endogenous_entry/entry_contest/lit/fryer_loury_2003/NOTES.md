# Notes — Fryer and Loury (2003), pass 1 (working paper, read in full)

Run `python3 lit/fryer_loury_2003/verify_fryer_loury_2003.py` from the
repository root.

## Verdict for C12, C13 and C10

- **C12** (the sign of M28 on the entry margin is new): narrowed. The paper's
  blind rule creates an extensive margin inside the contest, a pool of
  contestants who exert zero effort and still compete, and the pool grows as
  the rule rewards effort less (FT-10, `pool_grows`). That is M28's direction.
  What it does not have:
  - a weight on talent;
  - a fee;
  - a direction that holds for every law of talent and of the bought
    component.

  Its rule is a redistribution target enforced through signalling. C12 must
  cite it and claim the sign for a weight on talent with a fixed fee.
- **C13** (the payers' share as a function of how many pay): unaffected. The
  paper computes the shares of the two categories, not those of payers and
  non-payers.
- **C10**: under laissez-faire the group with higher costs wins fewer
  contests (FT-5, `share_shortfall_iff`), a further instance of the group gap
  already cited from Fryer and Loury (2010).

## Findings from the formalisation

1. Proposition 1's incentive compatibility is proved over the whole cost
   support: the payoff from mimicking another cost is single-peaked at one's
   own cost (`prop1_best_reply`).
2. The claim that the pool "increases with the aggressiveness of the
   redistributive goal" is asserted in the paper and not derived. It holds
   under the hazard-rate order implied by Definition 1, because the
   representation constraint rises in the cut-off at the rate
   `(f_1(1-F_2) - f_2(1-F_1))/2` (`Gamma_hasDerivAt`, `Gamma_deriv_pos`,
   `pool_grows`).
3. Three slips: the direction of dominance in Definition 1's gloss (FT-D1), the
   function in the first-order condition on p.6 (FT-D2), and "non-decreasing"
   in footnote 10 (FT-D3).
