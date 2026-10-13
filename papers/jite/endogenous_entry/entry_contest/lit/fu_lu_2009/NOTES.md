# Notes — Fu and Lu (2009), pass 1 (manuscript, read in full)

Run `python3 lit/fu_lu_2009/verify_fu_lu.py` from the repository root.

## Verdict for C7

Not a precedent. The investment is continuous, every contestant competes,
and contestants are identical, so there is no binary investment and no group
that does not pay. Their object is the size of the investment as `K` grows,
and ours (M17, M18) is the gain from paying as the number of challengers
grows. C7 waits on no other paper found.

## Findings from the formalisation

1. The algebra of eqs. (5)-(14) is right (`effort_foc_k`, `effort_foc_other`,
   `stage_payoff`, `ratio_eq9`, `hasDerivAt_pay`, `sym_foc_iff`, `payoff14`,
   `effort12`). So is the sign used for Proposition 1(iii) (`hasDerivAt_pay'`,
   `soc_at_symmetric`).
2. The derivative of `Φ` on p.6 is misprinted, and the conclusion drawn from
   it is false for `K ≥ 4` (`hasDerivAt_Phi`).
3. For `K ≥ 4`, zero investment is an equilibrium exactly when
   `λ ≥ (K-1)(K+1)²/(4K³)`. The proof is a perfect square,
   `(K+1)²(K+t)² - 4K(K-1)(2K + (K+1)t) = ((K+1)t - K(K-3))²`
   (`zero_inv_key`). The bound is above the printed one and falls to 1/4 as
   `K` grows.
4. For `K = 3` the positive-investment candidate never survives a deviation to
   zero: the gain is `4(8 - 27λ)³/(19683(2λ + 8/27)²) > 0`
   (`K3_zero_beats_candidate`).
5. Taken together, among symmetric pure-strategy equilibria:
   - positive investment occurs only at `K = 2` (for `λ < 1/4`; that it is a
     best reply there rests on the paper's bell-shape argument);
   - for `λ < 1/4` no `K ≥ 3` has a symmetric pure equilibrium at all;
   - for `λ ≥ 1/4` every equilibrium has zero investment and effort
     `(K-1)/K²`.

   So investment and effort never rise with `K`. The pieces for `K = 3` and
   `K ≥ 4` are proved. The statement as a whole also uses the `K = 2` step and
   Proposition 1(iii)'s passage to a profitable deviation, which are not
   formalised.
