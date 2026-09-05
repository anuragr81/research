# Field (1978), "A Note on Jeffrey Conditionalization"

*Philosophy of Science* 45(3), 361-367.

## Claims formalized

- eq. (4)/(5): the log-odds reparametrization $\alpha = \tfrac12\log\big((q/p)/((1-q)/(1-p))\big)$
  is the inverse of $q = pe^\alpha/(pe^\alpha+(1-p)e^{-\alpha})$.
- eq. (7): for two cues on binary partitions $E$, $E'$, the exponential-tilt update
  applied $E$-then-$E'$ equals $E'$-then-$E$, and both equal the closed form
  $P''(F_k)\propto F_k\, e^{\pm\alpha\pm\alpha'}$ (sign matching whether $F_k\subseteq E,\neg E$
  and $E',\neg E'$).
- His unformalized assertion that the same two updates, expressed in the raw delivered
  credences $q,q'$ instead of $\alpha,\alpha'$, do **not** commute.

## Result

All verified in `sympy/check_commutativity.py`. The inversion is exact; the tilt update
commutes exactly (both directions match the closed form identically); the credence-input
version has a nonzero order-gap in general (numeric witness $651/28120$ at one test point).

No Lean formalization: the commutativity of eq. (7) is immediate from exponentials of
sums commuting, not a claim that benefits from machine-checking.

## Bearing on Paper B

Field's exponential-tilt update on a $2\times2$ cell structure is the same object as
Paper B's Bayes-factor benchmark $\PB$ applied sequentially rather than jointly -- eq. (7)
is, in different notation, the content of Propositions IMM/DIV (Jeffrey conditioning
doesn't commute; Bayes-factor reweighting does). This is independent corroboration in
Field's own formalism, not new mathematical content for the paper.

One precision needed if this is cited: Field's $\alpha$ is a **log-odds shift**
(symmetric around 0, combines additively in the exponent), not literally the same
normalization as Paper B's $\ell^A_i \propto q_i/P(A{=}i)$ (his $e^{2\alpha}$ is a squared
likelihood ratio). Same underlying object, different scaling convention.
