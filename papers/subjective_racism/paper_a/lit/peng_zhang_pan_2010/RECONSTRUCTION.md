# Peng, Zhang and Pan (2010), reconstruction

Yun Peng, Shenyong Zhang and Rong Pan, "Bayesian Network Reasoning with
Uncertain Evidences", *International Journal of Uncertainty, Fuzziness and
Knowledge-Based Systems* 18(5), 2010, pp. 539–564, DOI
10.1142/S0218488510006696.

## Source read

- The published article, 26 PDF pages with a text layer. Cached at
  `~/.cache/paper_a/sources/peng_2010.pdf` (390,031 bytes), sha256
  `ecda7d4b…49b0`, and its extracted text `peng_2010.txt`, sha256
  `319cdf2d…a2a5`.
- The text of every page, PDF pages 1 to 26, was read on 16 October 2026, so
  the evidence tag is [F]. PDF pages 3, 4 and 7, which carry eqs. (1), (2),
  (6) and (9) and the definition of virtual evidence, were also rendered at
  110 dpi and read from the image.
- Pages in this record are PDF pages. PDF page `n` is journal page `538 + n`.

## The paper, from its own primitives

1. **Two kinds of uncertain evidence** (pp. 1–2). Virtual evidence is
   uncertainty about a reported observation, given as a likelihood ratio.
   Soft evidence is the uncertainty of an observed event, given as a
   distribution `R(Y)` (PZ-Q1).
2. **Jeffrey's rule and I-projection** (pp. 3–5). Bayes' rule cannot take
   `R(y)` as a condition because it is not an event (PZ-Q5). Jeffrey's rule
   asks that `Q(Y) = R(Y)` and that `Q(X∖Y | Y) = P(X∖Y | Y)` (PZ-Q6), which
   gives eq. (2), `Q(x) = P(x) R(y)/P(y)` if `P(y) ≠ 0` and `0` otherwise.
   The I-projection of `P` on the set of distributions with marginal `R(Y)` is
   eq. (6), the same expression (Theorem 1, PZ-Q7). Theorem 2 says that the
   I-projection also minimises the total variation, eq. (4).
3. **IPFP** (pp. 5–6). Cycling I-projections over several constraints
   converges, when the constraints are consistent, to the I-projection on
   their intersection (Theorem 3, cited). Theorem 4 says that IPFP on the joint equals
   one I-projection on the marginal of all evidence variables. Theorem 5 says that IPFP
   minimises the total variation.
4. **Virtual evidence** (pp. 6–7). Pearl's method adds a binary child `U` of
   `X_i` with `P(u|X_i = a) : P(u|X_i ≠ a) = L(X_i)` and instantiates `U = u`
   (PZ-Q8). This is Bayes' rule with likelihood `L`. Chan and Darwiche extend
   it to any partition `Y`.
5. **Soft evidence** (p. 7). `R(Y)` is a true observation of a distribution
   and must be preserved (PZ-Q9). Hard evidence is the degenerate case
   (PZ-Q10). Eq. (9) converts `R(Y)` into the likelihood ratio
   `L(y) = R(y)/P(y)`.
6. **Single and multiple evidence** (pp. 8–9). Applying eq. (9)'s virtual
   evidence gives the distribution of Jeffrey's rule (PZ-Q11, cited as Theorem
   5 of Chan and Darwiche). With two soft evidences, the two converted virtual
   evidences commute but satisfy neither marginal (PZ-Q12, PZ-Q14, Table 1),
   and recomputing the second likelihood after the first update satisfies
   the second marginal but moves the first. Jeffrey's rule applied in
   sequence is not commutative (PZ-Q13).
7. **BN-IPFP** (pp. 9–14). Two algorithms that run IPFP through virtual
   evidence on a Bayesian network, with Theorem 6 (convergence to the
   I-projection on the intersection) and timing experiments.
8. **Inconsistent soft evidence** (pp. 14–21). SR-IPFP, GEMA and the
   authors' SMOOTH, with Theorem 7 (convergence of SMOOTH for two
   constraints) and experiments.
9. **Conclusions and appendix** (pp. 21–25). Proofs of Theorems 2, 4, 6 and 7.

## Derivation chain of what Paper A1 uses

- A sum over joint states regroups by `y` (`sum_by_cell`, `sum_cell`), which
  gives eq. (1) from eq. (2) (`eq1`).
- Eq. (2) gives `Q(Y) = R(Y)` where `P(y) ≠ 0` (`jeffrey_marginal`). Lean's
  division by zero returns `0`, which is eq. (2)'s "otherwise" branch.
- Theorem 1 follows from the pointwise bound `a − b ≤ a log(a/b)`
  (`kl_term_ge`): for any `Q̃ ∈ P_{R(Y)}`, `I(Q̃‖P) ≥ Σ Q̃ log(R(y)/P(y)) =
  I(Q‖P)`, with `Q` Jeffrey's rule (`theorem1`). Without dominance the finite
  I-divergence breaks it (`control_theorem1_dominance`).
- Theorem 2, as stated and as proved in the appendix, (A-1) and (A-2)
  (`theorem2`).
- Theorem 4. Each IPFP step on `Y^j ⊆ Y` multiplies `Q` by a function of `y`
  (`step_factor`), so every finite iterate is `Q_0(x) K(y)`
  (`theorem4_finite`, `theorem4_rigid`).
- Theorem 5 does not follow. Theorem 2 gives minimality only over the
  distributions with marginal `Q*(Y)`, and a counterexample shows the
  statement false (`t5_two_steps`, `t5_ipfp_limit`, `theorem5_fails`).
- Eq. (9) and the cited Theorem 5 of Chan and Darwiche. With `L(y) =
  R(y)/P(y)`, the normaliser of Bayes' rule is `Σ_y R(y) = 1`, so the virtual
  update is eq. (2) (`eq9_soft_eq_virtual`). The converse, which the paper
  asserts as an equivalence (PZ-Q3) but does not write out
  (`virtual_eq_jeffrey`, `virtual_soft_sums_to_one`, `virtual_scale`).
- Hard evidence is the degenerate soft evidence (`hard_evidence`), and as a
  virtual likelihood it conditions and forgets the prior's weights across
  instantiations (`hard_evidence_virtual`, `hard_evidence_forgets_prior`).
- Two virtual evidences commute (`virtual_commute`) but, as Example 1 shows,
  do not preserve two soft marginals (`control_two_virtual_not_soft`).
- Theorem 7. Eq. (13-1) (`eq13_1`) and (A-5) (`eqA5`) with Theorem 2 give a
  contraction by `α` for one step (`theorem7_step`).
- Eq. (14) at `k = 0` and Table 4 (`eq14_initial`, `table4_consistent`,
  `table4_inconsistent`).

## What could not be reconstructed

The coverage table in `CLAIMS.md` has a row for every numbered result. Short
of general are these.

- Definition 1 and Theorem 1 are formalised without eq. (3)'s `+∞` branch,
  so Theorem 1 is proved for a strictly positive prior.
- Theorem 3 (cited), the limit in Theorems 4 and 6, and the iteration and
  limit in Theorem 7 need convergence arguments not formalised; Theorems 4,
  6 and 7 are formalised at every finite step.
- BN-IPFP's junction-tree computations, GEMA, SR-IPFP's convergence, Fig. 2's
  converged likelihoods, the conjecture for `m > 2`, and the timing and
  divergence tables are not formalised.
- Chan and Darwiche (2005), the source of the cited Theorem 5, is not read.
