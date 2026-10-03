# Barberis, N. (2012), "A Model of Casino Gambling"

*Management Science* 58(1), 35-51. DOI 10.1287/mnsc.1110.1435.

**Source.** The journal version (Articles in Advance file, 17 pp., journal
pagination), downloaded 2026-10-03 from `nicholasbarberis.github.io/gb_final.pdf`.
Read in full on 2026-10-03. Page references are journal pages. Not in the Drive
folder.

## Claims formalized

* **The model** (pp.39-42). A casino offers a 50:50 bet to win or lose `$h` at
  each date up to `T`; node `(t, j)` of the binomial tree carries accumulated
  winnings `h(t + 2 - 2j)` (eq. 14, p.46). "At each moment of time, the agent in
  our model decides what to do by maximizing the cumulative prospect theory
  value of his accumulated winnings or losses at the moment he leaves the
  casino" (p.40), with Tversky and Kahneman's value and weighting functions
  (eqs. 5-6, p.38).
* **Path independence by assumption.** "We only allow the agent to consider
  path-independent plans of action: his planned action at time t depends only
  on his accumulated winnings at that time and not on the path by which he
  accumulated those winnings" (fn. 13, p.42).
* **Probability weighting is not a belief.** "The transformed probabilities in
  (3) and (4) do not represent erroneous beliefs: in Tversky and Kahneman's
  (1992) framework, an agent evaluating the lottery-like ($5,000, 0.001) gamble
  knows that the probability of receiving the $5,000 is exactly 0.001. Rather,
  the transformed probabilities are decision weights that capture the
  experimental evidence on risk attitudes" (p.39).
* **Figure 2's exit strategy** and its distribution (p.42).
* **The time inconsistency at node (4,1)** (p.41): conditions (7) and (8), and
  "it is straightforward to check that condition (8) holds for all α, δ ∈
  (0, 1)".
* **Proposition 1 and Corollary 1** (p.45, appendix p.50): condition (12), and
  "for Tversky and Kahneman's (1992) estimates, namely, (α, δ, λ) = (0.88, 0.65,
  2.25), the lowest value of T for which condition (12) holds is T = 26".

## Result

`lean/Barberis.lean` is a symlink to `lean/Literature/Barberis.lean`; standalone on
Mathlib, no `sorry`, standard axioms only. It proves the winnings at a node,
path independence of any plan (two paths with the same numbers of wins and
losses, in any sequence, reach the same node), Figure 2's distribution over all
32 paths, `(7) ⟺ (8)`, and condition (8) for all `α ∈ (0,1)`, `δ ∈ (0,1)`,
from `w(1/2) ≤ 1/2` and the concavity of `x^α`.

`sympy/check_barberis2012.py` checks the appendix's reflection-principle exit
probabilities against brute-force enumeration for `T = 3, …, 12`, and
reproduces Corollary 1 exactly: condition (12) fails for every `T` from 2 to 25
and holds at `T = 26`. All checks pass.

## Bearing on Paper B

Not cited. Read and formalised for a proposed Section 3 sentence on cumulative
prospect theory; the author dropped the dynamic half on 2026-10-03, since a
casino's sequence of bets on known 50:50 odds is not the panel's setting. Kept as
the record of what the paper says: in its model the gambler's choice depends on
the winnings accumulated so far and, by the assumption of fn. 13, not on the
sequence of wins and losses that produced them.

## Not formalized

The numerical solutions behind Figures 3-5 (problems 9, 13, 16); the
supply-side equilibrium in the online supplement.
