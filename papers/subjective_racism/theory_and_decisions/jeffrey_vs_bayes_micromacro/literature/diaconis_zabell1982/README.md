# Diaconis, P. and Zabell, S.L. (1982), "Updating Subjective Probability"

*Journal of the American Statistical Association* 77(380), 822-830.

This is the source of the "minimal change consistent with the delivered marginal"
characterization of Jeffrey's rule that the manuscript cites (footnote at line ~81),
and of the commutation condition Hawthorne (2004) restates as his Diaconis-Zabell 3.2
condition.

## Claims formalized

- **Theorem 3.2**: for two Jeffrey-updated partitions $\mathcal{E}=\{E_i\}$,
  $\mathcal{F}=\{F_j\}$, the two update orders coincide ($P_{\mathcal{E}\mathcal{F}} =
  P_{\mathcal{F}\mathcal{E}}$) if and only if $\mathcal{E}$ and $\mathcal{F}$ are
  *Jeffrey independent* with respect to the prior and the two target vectors --
  a condition strictly weaker than ordinary probabilistic independence of the two
  partitions (their Example 3.3 shows J-independence can hold at specific $p,q$ without
  $P$-independence).
- **Example 3.4**: an explicit $2\times2$ table (their own binary-attribute case) where
  updating $E$ then $F$ exactly reproduces the $E$-target $p_1$, but updating $F$ then
  $E$ does *not* reproduce the $F$-target $q_1$ -- i.e. the order of updating decides
  which marginal is held exactly and which one drifts.

## Result

Both verified in `sympy/check_theorem32.py`, using Paper B's own $2\times2$ cell
machinery (attribute-local Jeffrey steps, prior parametrized by $\alpha,\beta,c$).

- Example 3.4 reproduced exactly on their numbers: prior $P(E,F)=\tfrac18$,
  $P(E,\bar F)=\tfrac14$, $P(\bar E,F)=\tfrac38$, $P(\bar E,\bar F)=\tfrac14$; targets
  $p_1=\tfrac12$, $q_1=\tfrac{7}{15}$. Route $E$-then-$F$ gives margin$(E)=\tfrac12$
  exactly. Route $F$-then-$E$ gives margin$(F)=\tfrac{371}{851}\ne\tfrac{7}{15}$.
- Theorem 3.2, on Paper B's own parametrization: at $c=0$ (independent prior) the two
  routes coincide identically in $p,q$ -- consistent with $c{=}0$ being exactly the
  Jeffrey-independence condition for an attribute-local two-cue prior. At $c\ne0$ the
  gap in cell $(1,1)$ is not identically zero (numeric witness $-1032/369935$ at
  $(\alpha,\beta,c,p,q)=(.3,.55,.05,.4,.6)$).

No Lean formalization attempted: Theorem 3.2 itself is a biconditional over general
partitions (arbitrary cardinality) proved via Csiszar's I-projection result, which is
out of scope to reprove; what is checked here is its restriction to Paper B's own
$2\times2$, two-cue setting, which is already covered by the existing Lean development
(`JeffreyOrder/PropIMM.lean`'s $c=0$ case).

## Bearing on Paper B

Example 3.4 is the 1982 precedent for exactly what the manuscript calls "pinning": the
*last*-updated marginal is held exactly at its target; the earlier one is not
guaranteed to survive the second update untouched. Diaconis-Zabell demonstrate this
with a concrete counterexample forty-plus years before Paper B; Paper B's Proposition
DRF is the general (all $\alpha,\beta,c,q,r$) form of the same phenomenon their Example
3.4 witnesses at one point. Worth citing directly for "pinning" and for the $c=0$
commutation condition, rather than only via Hawthorne's secondhand restatement.
